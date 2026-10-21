namespace YAMLicious

open System.Collections.Generic

/// The exception that the parser raises inside its routines. `Parser.parseCursor` catches it and returns
/// the error. No other code needs it.
exception internal ParseFailure of YamlError

/// A lexeme with the range of its text and the tab flag of the blanks in front of it.
type internal Tok =
    { Lexeme: Lexeme
      Span: SourceSpan
      /// True when a tab stands among the blanks in front of the lexeme. For the first lexeme of a line these
      /// are the blanks of the indentation.
      Tab: bool }

/// A tag or an anchor that the parser has read and not yet attached to a node.
[<AllowNullLiteral>]
type internal Prop(name: PropertyName, tag: YamlTag option, anchor: string option, span: SourceSpan, tab: bool) =
    member _.Name = name
    member _.Tag = tag
    member _.Anchor = anchor
    member _.Span = span
    member _.Tab = tab
    /// True when a line break stands between this property and the next token.
    member val NewLine = false with get, set
    /// The comment on the line of this property, when a line break follows.
    member val Comment: YamlComment option = None with get, set
    /// The trivia lines between this property and the next property.
    member val Trivia: Trivia list = [] with get, set

/// What an open block collection waits for. A sequence is `AwaitingEntry` or `AfterDash`. A mapping is
/// `AwaitingEntry`, `AfterQuestion`, `AwaitingColon` (an explicit key without its value indicator yet) or
/// `AfterColon`.
[<RequireQualifiedAccess>]
type internal Phase =
    | AwaitingEntry
    | AfterDash
    | AfterQuestion
    | AwaitingColon
    | AfterColon

/// The positions at which the parser asks for a node. `Continue` stands for the first token of an entry
/// of a collection that is open already.
[<RequireQualifiedAccess>]
type internal RequestKind =
    | Root
    | Item
    | Value
    | ExplicitKey
    | ExplicitValue
    | Continue

/// An open block collection. The columns of the frames on the stack never decrease from the bottom to the
/// top. An indentless sequence has the column of the mapping below it.
[<AllowNullLiteral>]
type internal Frame(isMap: bool, col: int, start: SourceMark) =
    member _.IsMap = isMap
    /// The column of the first token of the collection, counted from 0: the dash of a sequence or the first
    /// token of the first key of a mapping.
    member _.Col = col
    /// The start of the collection node, which is its first property or its first entry.
    member _.Start = start
    member val LastEnd = start with get, set
    member val Phase = Phase.AwaitingEntry with get, set
    /// True when the frame holds an explicit key that no `:` has followed yet.
    member this.AwaitsColon =
        match this.Phase with
        | Phase.AwaitingColon -> true
        | _ -> false
    member val Indentless = false with get, set
    member val Tag: YamlTag option = None with get, set
    member val Anchor: string option = None with get, set
    member val Layout: PropertyPlacement list = [] with get, set
    member val Items = ResizeArray<SequenceItem>()
    member val Entries = ResizeArray<MappingEntry>()
    /// The trivia behind the last entry, collected at the end of the document (rule R4).
    member val End = ResizeArray<Trivia>()
    // The entry or item in progress.
    member val EntryStart = start with get, set
    member val Before: Trivia list = [] with get, set
    member val Dash: Separation option = None with get, set
    member val Question: Separation option = None with get, set
    member val KeyNode = YamlNode.empty with get, set
    member val BeforeColon: Trivia list = [] with get, set
    member val Colon: Separation option = None with get, set
    /// The span of the first entry or item, or of the entry in progress when the collection has none yet.
    member this.FirstSpan: SourceSpan =
        let first =
            if isMap then
                if this.Entries.Count > 0 then this.Entries.[0].Span else None
            elif this.Items.Count > 0 then
                this.Items.[0].Span
            else
                None
        match first with
        | Some span -> span
        | None -> { Start = this.EntryStart; Finish = this.LastEnd }
    /// The span of the first key of a mapping, or of the key in progress when the mapping has no entry yet.
    member this.FirstKeySpan: SourceSpan option =
        if this.Entries.Count > 0 then this.Entries.[0].Key.Span else this.KeyNode.Span

module internal ParserSupport =

    /// The longest implicit key, in characters.
    [<Literal>]
    let MaxKeyLength = 1024

    let zeroMark: SourceMark = { Line = 1; Column = 1 }

    let spanBetween (first: SourceMark) (finish: SourceMark) : SourceSpan = { Start = first; Finish = finish }

    let spanAt (mark: SourceMark) : SourceSpan = { Start = mark; Finish = mark }

    let noProperties: NodeProperties = { Tag = None; Anchor = None }

    /// A missing value: an empty plain scalar with a span of no width.
    let missingNode (at: SourceMark) : YamlNode = { YamlNode.empty with Span = Some (spanAt at) }

    let endOf (node: YamlNode) : SourceMark =
        match node.Span with
        | Some span -> span.Finish
        | None -> zeroMark

    let failWith (code: YamlErrorCode) (message: string) (span: SourceSpan) (related: SourceSpan list) : 'a =
        raise (ParseFailure { Code = code; Message = message; Span = span; Related = related })

    let isBlockScalar (scalar: YamlScalar) =
        match scalar.Style with
        | ScalarStyle.Block _ -> true
        | _ -> false

    let separation (newLine: bool) (comment: YamlComment option) (following: Trivia list) : Separation =
        if newLine || comment.IsSome then
            Separation.NewLine { Comment = comment; Following = following }
        else
            Separation.Inline

open ParserSupport

/// The parser of the block grammar. It owns the structure: indentation, collection membership, the roles of
/// keys and values, the properties of a node, the documents and the slot of every comment and blank line.
/// It pulls one lexeme at a time from `Lexer.next` and tells the lexer the context of the position.
///
/// The control flow is a loop over explicit state. `frames` holds the open block collections, and one node
/// request at a time says what the next node has to be and where it stands. A request ends when its node is
/// complete, and the node goes to the frame on top, or becomes the root. No call recurses with the depth of
/// the input.
///
/// Each character goes through the lexer once, each frame is pushed and popped once and each trivia line
/// moves to its slot once, so the time is linear in the length of the input.
type internal BlockParser(cursor: Cursor) =
    let stack = ResizeArray<Frame>()
    let pending = ResizeArray<Trivia>()
    let parts = ResizeArray<StreamPart>()

    // One lexeme can be put back.
    let mutable hasBuffer = false
    let mutable buffer: Tok =
        { Lexeme = Lexeme.StreamStart
          Span = spanAt zeroMark
          Tab = false }

    // The document in progress.
    let docPrelude = ResizeArray<PreludeItem>()
    let docBefore = ResizeArray<Trivia>()
    let docAfter = ResizeArray<Trivia>()
    // The prefix of each declared handle and the span of its directive.
    let tagHandles = Dictionary<string, string * SourceSpan>()
    let anchors = HashSet<string>()
    let mutable docOpen = false
    let mutable docMarker: DocumentMarker option = None
    let mutable docStart = zeroMark
    let mutable docRoot = YamlNode.empty
    let mutable yamlDirective: SourceSpan option = None
    let mutable hasDirective = false
    // The start of the first directive of the document that is about to start.
    let mutable directiveStart = zeroMark
    let mutable finished = false

    // The node request in progress.
    let props = ResizeArray<Prop>()
    let lead = ResizeArray<Trivia>()
    let mutable requested = false
    let mutable kind = RequestKind.Root
    let mutable owner = -1
    let mutable compact = false
    let mutable indentless = false
    let mutable sawNewLine = false
    let mutable indicatorEnd = zeroMark
    // The line break and the comment behind the element that was read last: the indicator or a property.
    let mutable elementNewLine = false
    let mutable elementComment: YamlComment option = None
    // The same for the indicator, once a property or the content has followed.
    let mutable indicatorNewLine = false
    let mutable indicatorComment: YamlComment option = None

    let isRoot () =
        match kind with
        | RequestKind.Root -> true
        | _ -> false

    let isContinue () =
        match kind with
        | RequestKind.Continue -> true
        | _ -> false

    // ---------------------------------------------------------------------------------------------
    // Lexemes

    member private _.Lex(ctx: LexContext) : Tok =
        let first = cursor.FirstOnLine
        match Lexer.next ctx cursor with
        | Error error -> raise (ParseFailure error)
        | Ok located ->
            { Lexeme = located.Value
              Span = located.Span
              Tab = if first then cursor.LeadingTab else cursor.SeparationHadTab }

    member private this.Next(ctx: LexContext) : Tok =
        if hasBuffer then
            hasBuffer <- false
            buffer
        else
            this.Lex ctx

    member private _.Unread(tok: Tok) =
        buffer <- tok
        hasBuffer <- true

    member private _.RootContext() : LexContext =
        { Grammar = GrammarContext.BlockIn
          ParentIndent = -1
          SimpleKey = false }

    member private _.NodeContext() : LexContext =
        if isContinue () then
            let top = stack.[stack.Count - 1]
            { Grammar = GrammarContext.BlockIn
              ParentIndent = top.Col
              SimpleKey = top.IsMap }
        else
            { Grammar = GrammarContext.BlockIn
              ParentIndent = owner
              SimpleKey = false }

    member private _.LineContext() : LexContext =
        { Grammar = GrammarContext.BlockIn
          ParentIndent = (if stack.Count > 0 then stack.[stack.Count - 1].Col else -1)
          SimpleKey = false }

    member private this.EolComment() : YamlComment option =
        let next = this.Next(this.RootContext())
        match next.Lexeme with
        | Lexeme.Comment (comment, false) -> Some comment
        | _ ->
            this.Unread next
            None

    member private _.TakePending() : Trivia list =
        if pending.Count = 0 then
            []
        else
            let taken = List.ofSeq pending
            pending.Clear()
            taken

    // ---------------------------------------------------------------------------------------------
    // Tags, anchors and properties

    member private _.ResolveTag(tok: Tok, token: TagToken) : YamlTag =
        match token with
        | TagToken.Verbatim uri ->
            if uri = "!" then
                failWith YamlErrorCode.InvalidTag "The verbatim tag !<!> is invalid, because a local tag needs a name behind the `!`." tok.Span []
            else
                YamlTag.Named { Prefix = ""; Suffix = uri }
        | TagToken.Shorthand (handle, suffix) ->
            if handle = "!" && suffix.Length = 0 then
                YamlTag.NonSpecific
            else
                let decoded =
                    match Lexer.decodeTagEscapes suffix with
                    | Some text -> text
                    | None -> failWith YamlErrorCode.InvalidTag "The %-escapes of a tag have to spell UTF-8 text." tok.Span []
                match tagHandles.TryGetValue handle with
                | true, (prefix, _) -> YamlTag.Named { Prefix = prefix; Suffix = decoded }
                | _ ->
                    if handle = "!!" then
                        YamlTag.Named { Prefix = YamlTag.coreNamespace; Suffix = decoded }
                    elif handle = "!" then
                        YamlTag.Named { Prefix = "!"; Suffix = decoded }
                    else
                        failWith YamlErrorCode.UndeclaredTagHandle (sprintf "The tag handle %s is not declared by a %%TAG directive." handle) tok.Span []

    /// Stores a property and ends the element in front of it.
    member private this.AddProp(tok: Tok) =
        if props.Count = 0 then
            indicatorNewLine <- elementNewLine
            indicatorComment <- elementComment
            lead.AddRange pending
            pending.Clear()
        else
            let previous = props.[props.Count - 1]
            previous.NewLine <- elementNewLine
            previous.Comment <- elementComment
            previous.Trivia <- this.TakePending()
        elementNewLine <- false
        elementComment <- None
        match tok.Lexeme with
        | Lexeme.Tag token -> props.Add(Prop(PropertyName.Tag, Some (this.ResolveTag(tok, token)), None, tok.Span, tok.Tab))
        | Lexeme.Anchor name ->
            anchors.Add name |> ignore
            props.Add(Prop(PropertyName.Anchor, None, Some name, tok.Span, tok.Tab))
        | _ -> failWith YamlErrorCode.UnexpectedContent "A property was expected." tok.Span []

    /// Ends the element that was read last, once the next token or the end of the node is known.
    member private _.CloseElement() =
        if props.Count = 0 then
            indicatorNewLine <- elementNewLine
            indicatorComment <- elementComment
        else
            let last = props.[props.Count - 1]
            last.NewLine <- elementNewLine
            last.Comment <- elementComment
        elementNewLine <- false
        elementComment <- None

    /// The number of properties up to and including the last one that a line break follows. These stand on
    /// lines of their own in front of the line of the content.
    member private _.OwnLineCount() : int =
        let mutable count = 0
        for i in 0 .. props.Count - 1 do
            if props.[i].NewLine then count <- i + 1
        count

    /// The tag and the anchor of the properties from `first` up to but excluding `until`.
    member private _.BuildProps(first: int, until: int) : NodeProperties =
        let mutable tag: YamlTag option = None
        let mutable anchor: string option = None
        let mutable tagSpan = spanAt zeroMark
        let mutable anchorSpan = spanAt zeroMark
        for i in first .. until - 1 do
            let prop = props.[i]
            if prop.Name = PropertyName.Tag then
                if tag.IsSome then
                    failWith YamlErrorCode.DuplicateTag "A node has at most one tag." prop.Span [ tagSpan ]
                tag <- prop.Tag
                tagSpan <- prop.Span
            else
                if anchor.IsSome then
                    failWith YamlErrorCode.DuplicateAnchor "A node has at most one anchor." prop.Span [ anchorSpan ]
                anchor <- prop.Anchor
                anchorSpan <- prop.Span
        { Tag = tag; Anchor = anchor }

    /// The placements of the properties from `first` up to but excluding `until`. `lastFollowing` holds the
    /// trivia between the last property and the content, and `contentFollows` is false for a node without content.
    member private _.BuildLayout(first: int, until: int, lastFollowing: Trivia list, contentFollows: bool) : PropertyPlacement list =
        let layout = ResizeArray<PropertyPlacement>()
        for i in first .. until - 1 do
            let prop = props.[i]
            let after =
                if i < until - 1 then
                    separation prop.NewLine prop.Comment prop.Trivia
                elif contentFollows then
                    separation prop.NewLine prop.Comment lastFollowing
                else
                    match prop.Comment with
                    | Some comment -> Separation.NewLine { Comment = Some comment; Following = [] }
                    | None -> Separation.Inline
            layout.Add
                { Property = prop.Name
                  After = after
                  Span = Some prop.Span }
        List.ofSeq layout

    // ---------------------------------------------------------------------------------------------
    // Delivering nodes

    member private _.StartRequest(newKind: RequestKind, newOwner: int, newCompact: bool, newIndentless: bool, afterIndicator: SourceMark, startsOnNewLine: bool) =
        requested <- true
        kind <- newKind
        owner <- newOwner
        compact <- newCompact
        indentless <- newIndentless
        props.Clear()
        lead.Clear()
        sawNewLine <- startsOnNewLine
        indicatorEnd <- afterIndicator
        elementNewLine <- false
        elementComment <- None
        indicatorNewLine <- false
        indicatorComment <- None

    /// Stores the separation behind the indicator that the request follows.
    member private _.SetIndicator(sep: Separation) =
        match kind with
        | RequestKind.Root -> ()
        | RequestKind.Continue -> ()
        | RequestKind.Item -> stack.[stack.Count - 1].Dash <- Some sep
        | RequestKind.ExplicitKey -> stack.[stack.Count - 1].Question <- Some sep
        | RequestKind.Value -> stack.[stack.Count - 1].Colon <- Some sep
        | RequestKind.ExplicitValue -> stack.[stack.Count - 1].Colon <- Some sep

    /// Hands a complete node to the frame on top, or makes it the root.
    member private _.Deliver(node: YamlNode) =
        if stack.Count = 0 then
            docRoot <- node
        else
            let top = stack.[stack.Count - 1]
            let finish = endOf node
            top.LastEnd <- finish
            match top.Phase with
            | Phase.AfterDash ->
                top.Items.Add
                    { Before = top.Before
                      Dash = top.Dash
                      Node = node
                      Span = Some (spanBetween top.EntryStart finish) }
                top.Phase <- Phase.AwaitingEntry
            | Phase.AfterQuestion ->
                top.KeyNode <- node
                top.Phase <- Phase.AwaitingColon
            | Phase.AfterColon ->
                top.Entries.Add
                    { Before = top.Before
                      Question = top.Question
                      Key = top.KeyNode
                      BeforeColon = top.BeforeColon
                      Colon = top.Colon
                      Value = node
                      Span = Some (spanBetween top.EntryStart finish) }
                top.Phase <- Phase.AwaitingEntry
            | Phase.AwaitingEntry -> failWith YamlErrorCode.UnexpectedContent "A node cannot follow here, because the collection waits for an entry." (spanAt finish) []
            | Phase.AwaitingColon -> failWith YamlErrorCode.UnexpectedContent "A node cannot follow here, because the entry waits for its colon." (spanAt finish) []

    /// An explicit key that no `:` follows ends its entry with a missing value.
    member private _.FinishWithoutValue(frame: Frame) =
        let keyEnd = endOf frame.KeyNode
        frame.Entries.Add
            { Before = frame.Before
              Question = frame.Question
              Key = frame.KeyNode
              BeforeColon = []
              Colon = None
              Value = missingNode keyEnd
              Span = Some (spanBetween frame.EntryStart keyEnd) }
        frame.Phase <- Phase.AwaitingEntry

    member private this.CloseTop() =
        let frame = stack.[stack.Count - 1]
        stack.RemoveAt(stack.Count - 1)
        if frame.IsMap && frame.AwaitsColon then
            this.FinishWithoutValue frame
        let nodeKind =
            if frame.IsMap then
                YamlNodeKind.Mapping
                    { Style = CollectionStyle.Block
                      OpeningComment = None
                      Items = List.ofSeq frame.Entries
                      End = List.ofSeq frame.End }
            else
                YamlNodeKind.Sequence
                    { Style = CollectionStyle.Block
                      OpeningComment = None
                      Items = List.ofSeq frame.Items
                      End = List.ofSeq frame.End }
        this.Deliver
            { Kind = nodeKind
              Properties = { Tag = frame.Tag; Anchor = frame.Anchor }
              PropertyLayout = frame.Layout
              TrailingComment = None
              Span = Some (spanBetween frame.Start frame.LastEnd) }

    // ---------------------------------------------------------------------------------------------
    // The content of a node request

    /// The request ends without content. The node is an empty plain scalar that holds the properties read so far.
    member private this.EndMissing() =
        this.CloseElement()
        let count = props.Count
        if isContinue () then
            failWith YamlErrorCode.MisplacedProperties "Properties in front of an entry need a key behind them on the same line." (if count > 0 then props.[0].Span else spanAt indicatorEnd) []
        let nodeProps = this.BuildProps(0, count)
        let layout = this.BuildLayout(0, count, [], false)
        let leadTrivia = List.ofSeq lead
        let at = if count > 0 then props.[count - 1].Span.Finish else indicatorEnd
        let first = if count > 0 then props.[0].Span.Start else indicatorEnd
        if isRoot () then
            docBefore.AddRange lead
        else
            let sep =
                if count > 0 then separation indicatorNewLine indicatorComment leadTrivia
                else separation false indicatorComment []
            this.SetIndicator sep
        requested <- false
        let node = missingNode at
        this.Deliver
            { node with
                Properties = nodeProps
                PropertyLayout = layout
                Span = Some (spanBetween first at) }

    member private this.CheckAlias(tok: Tok, name: string) =
        if not (anchors.Contains name) then
            failWith YamlErrorCode.UndefinedAlias (sprintf "The alias *%s names no anchor that stands earlier in the document." name) tok.Span []

    /// A scalar or an alias that no `:` follows is a node of its own.
    member private this.CompleteLeaf(tok: Tok, trailing: YamlComment option) =
        this.CloseElement()
        if isContinue () then
            let top = stack.[stack.Count - 1]
            if top.IsMap then
                failWith YamlErrorCode.MissingColon "An implicit key needs a `:` behind it on its line." tok.Span (Option.toList top.FirstKeySpan)
            else
                failWith YamlErrorCode.MixedCollections "A sequence takes entries that start with a dash only." tok.Span []
        let count = props.Count
        let nodeKind =
            match tok.Lexeme with
            | Lexeme.Alias name ->
                if count > 0 then
                    failWith YamlErrorCode.AliasWithProperties "An alias takes no tag and no anchor." props.[0].Span [ tok.Span ]
                this.CheckAlias(tok, name)
                YamlNodeKind.Alias name
            | Lexeme.Scalar scalar -> YamlNodeKind.Scalar scalar
            | _ -> failWith YamlErrorCode.UnexpectedContent "A scalar or an alias was expected." tok.Span []
        let nodeProps = this.BuildProps(0, count)
        let following = this.TakePending()
        let layout = this.BuildLayout(0, count, following, true)
        let before = if count > 0 then List.ofSeq lead else following
        if isRoot () then
            docBefore.AddRange before
        else
            this.SetIndicator(separation indicatorNewLine indicatorComment before)
        requested <- false
        let first = if count > 0 then props.[0].Span.Start else tok.Span.Start
        this.Deliver
            { Kind = nodeKind
              Properties = nodeProps
              PropertyLayout = layout
              TrailingComment = trailing
              Span = Some (spanBetween first tok.Span.Finish) }

    /// A scalar, an alias or an empty key followed by a `:` starts an entry of a mapping. The mapping is new,
    /// or it is the open mapping on top of the stack when the request continues it.
    member private this.AddKeyEntry(leaf: Tok, colon: Tok) =
        let isAlias =
            match leaf.Lexeme with
            | Lexeme.Alias _ -> true
            | _ -> false
        if leaf.Span.Start.Line <> leaf.Span.Finish.Line then
            failWith YamlErrorCode.ImplicitKeyMultiline "An implicit key has to stay on one line." leaf.Span []
        this.CloseElement()
        let count = props.Count
        let own = this.OwnLineCount()
        if isAlias then
            if count > own then
                failWith YamlErrorCode.AliasWithProperties "An alias takes no tag and no anchor." props.[own].Span [ leaf.Span ]
            match leaf.Lexeme with
            | Lexeme.Alias name -> this.CheckAlias(leaf, name)
            | _ -> ()
        let keyStart = if count > own then props.[own].Span.Start else leaf.Span.Start
        let keyTab = if count > own then props.[own].Tab else leaf.Tab
        if keyTab then
            failWith YamlErrorCode.TabIndentation "A tab cannot be part of the indentation in front of a key." (spanBetween keyStart leaf.Span.Finish) []
        // YAML 1.2.2 (ns-s-implicit-yaml-key) limits the key to 1024 characters from its first character, its properties
        // included, up to the colon, so the blanks in front of the colon count too.
        if colon.Span.Start.Column - keyStart.Column > MaxKeyLength then
            failWith YamlErrorCode.ImplicitKeyLength "An implicit key can have at most 1024 characters, from its properties to its colon." (spanBetween keyStart colon.Span.Start) []
        let column = keyStart.Column - 1
        let mutable frame: Frame = null
        if isContinue () then
            frame <- stack.[stack.Count - 1]
            // The key starts the line that AfterLineStart matched to this frame, so it stands at the column of the mapping.
            // A key with properties has the trivia in front of its line in `lead`, and they belong to the entry.
            pending.InsertRange(0, lead)
        else
            if not sawNewLine && not compact then
                let code = if isRoot () then YamlErrorCode.BlockCollectionOnMarkerLine else YamlErrorCode.BlockCollectionOnKeyLine
                failWith code "A block mapping cannot start on the line of the indicator in front of it." (spanBetween keyStart colon.Span.Finish) []
            let ownProps = this.BuildProps(0, own)
            let ownLayout = this.BuildLayout(0, own, [], true)
            if own > 0 then
                if isRoot () then
                    docBefore.AddRange lead
                else
                    this.SetIndicator(separation indicatorNewLine indicatorComment (List.ofSeq lead))
                // The trivia between the last property of the mapping and the first key go to the first entry.
                pending.InsertRange(0, props.[own - 1].Trivia)
            else
                if not (isRoot ()) then
                    this.SetIndicator(separation indicatorNewLine indicatorComment [])
                // The mapping has no token of its own, so the trivia in front of the key belong to the first entry.
                // A key with properties has moved them to `lead`, and a key without properties has left them in `pending`.
                pending.InsertRange(0, lead)
            let start = if own > 0 then props.[0].Span.Start else keyStart
            frame <- Frame(true, column, start)
            frame.Tag <- ownProps.Tag
            frame.Anchor <- ownProps.Anchor
            frame.Layout <- ownLayout
            stack.Add frame
        let keyProps = this.BuildProps(own, count)
        let keyLayout = this.BuildLayout(own, count, [], true)
        let keyKind =
            match leaf.Lexeme with
            | Lexeme.Alias name -> YamlNodeKind.Alias name
            | Lexeme.Scalar scalar -> YamlNodeKind.Scalar scalar
            | _ -> failWith YamlErrorCode.UnexpectedContent "A key was expected." leaf.Span []
        frame.Before <- this.TakePending()
        frame.EntryStart <- keyStart
        frame.Question <- None
        frame.BeforeColon <- []
        frame.KeyNode <-
            { Kind = keyKind
              Properties = keyProps
              PropertyLayout = keyLayout
              TrailingComment = None
              Span = Some (spanBetween keyStart leaf.Span.Finish) }
        frame.Phase <- Phase.AfterColon
        frame.LastEnd <- leaf.Span.Finish
        this.StartRequest(RequestKind.Value, frame.Col, false, true, colon.Span.Finish, false)

    member private this.StartLeaf(tok: Tok) =
        if props.Count > 0 && props.[props.Count - 1].Span.Finish = tok.Span.Start then
            failWith YamlErrorCode.MissingSeparation "A tag or an anchor needs a blank behind it." tok.Span [ props.[props.Count - 1].Span ]
        let mutable isKey = false
        let mutable colon = tok
        let mutable trailing: YamlComment option = None
        let isBlock =
            match tok.Lexeme with
            | Lexeme.Scalar scalar -> isBlockScalar scalar
            | _ -> false
        if not isBlock then
            let next = this.Next(this.RootContext())
            match next.Lexeme with
            | Lexeme.Value _ ->
                isKey <- true
                colon <- next
            | Lexeme.Comment (comment, false) -> trailing <- Some comment
            | _ -> this.Unread next
        if isKey then this.AddKeyEntry(tok, colon) else this.CompleteLeaf(tok, trailing)

    /// A `:` at the start of a node is an entry with an empty key.
    member private this.StartEmptyKey(colon: Tok) =
        let key =
            { Lexeme = Lexeme.Scalar { Value = ""; Style = ScalarStyle.Plain; HeaderComment = None }
              Span = spanAt colon.Span.Start
              Tab = false }
        this.AddKeyEntry(key, colon)

    /// A `-` starts a sequence, or it is the next entry of the open sequence when the request continues it.
    member private this.StartSequence(dash: Tok) =
        this.CloseElement()
        let count = props.Count
        let column = dash.Span.Start.Column - 1
        let mutable frame: Frame = null
        if isContinue () then
            frame <- stack.[stack.Count - 1]
            // Continue calls this method itself for a dash at the column of an open sequence. A dash that arrives
            // through NodeStep stands behind properties on the line of a mapping entry. [187] and [188] let every
            // entry of a block mapping start with a key or a `?`, and [183], [184] and [186] put the dash of a
            // sequence entry right behind the indentation, so the dash mixes the collections.
            if frame.IsMap then
                failWith YamlErrorCode.MixedCollections "A mapping takes entries that start with a key, and a dash starts a sequence entry." dash.Span [ frame.FirstSpan ]
        else
            if not sawNewLine && not compact then
                let code = if isRoot () then YamlErrorCode.BlockCollectionOnMarkerLine else YamlErrorCode.BlockCollectionOnKeyLine
                failWith code "A block sequence cannot start on the line of the indicator in front of it." dash.Span []
            if count > 0 && not props.[count - 1].NewLine then
                failWith YamlErrorCode.BlockCollectionOnKeyLine "The properties of a block sequence stand on a line of their own." props.[count - 1].Span []
            let nodeProps = this.BuildProps(0, count)
            let layout = this.BuildLayout(0, count, [], true)
            if isRoot () then
                docBefore.AddRange lead
            else
                this.SetIndicator(separation indicatorNewLine indicatorComment (if count > 0 then List.ofSeq lead else []))
            let start = if count > 0 then props.[0].Span.Start else dash.Span.Start
            frame <- Frame(false, column, start)
            frame.Tag <- nodeProps.Tag
            frame.Anchor <- nodeProps.Anchor
            frame.Layout <- layout
            frame.Indentless <- indentless && sawNewLine && column = owner
            stack.Add frame
        frame.Before <- this.TakePending()
        frame.EntryStart <- dash.Span.Start
        frame.Dash <- None
        frame.Phase <- Phase.AfterDash
        frame.LastEnd <- dash.Span.Finish
        this.StartRequest(RequestKind.Item, frame.Col, true, false, dash.Span.Finish, false)

    /// A `?` starts a mapping with an explicit entry, or it is the next entry of the open mapping.
    member private this.StartExplicitKey(question: Tok) =
        this.CloseElement()
        let count = props.Count
        let column = question.Span.Start.Column - 1
        let mutable frame: Frame = null
        if isContinue () then
            frame <- stack.[stack.Count - 1]
            // A `?` starts an entry of a block mapping right behind the indentation ([187] to [190]), so properties
            // in front of it on the line of the entry belong to no node. The properties of the key stand behind the `?`.
            if count > 0 then
                failWith YamlErrorCode.MisplacedProperties "Properties cannot stand in front of a `?` on the line of an entry." question.Span [ props.[0].Span ]
        else
            if not sawNewLine && not compact then
                let code = if isRoot () then YamlErrorCode.BlockCollectionOnMarkerLine else YamlErrorCode.BlockCollectionOnKeyLine
                failWith code "A block mapping cannot start on the line of the indicator in front of it." question.Span []
            if count > 0 && not props.[count - 1].NewLine then
                failWith YamlErrorCode.BlockCollectionOnKeyLine "The properties of a block mapping stand on a line of their own." props.[count - 1].Span []
            let nodeProps = this.BuildProps(0, count)
            let layout = this.BuildLayout(0, count, [], true)
            if isRoot () then
                docBefore.AddRange lead
            else
                this.SetIndicator(separation indicatorNewLine indicatorComment (if count > 0 then List.ofSeq lead else []))
            let start = if count > 0 then props.[0].Span.Start else question.Span.Start
            frame <- Frame(true, column, start)
            frame.Tag <- nodeProps.Tag
            frame.Anchor <- nodeProps.Anchor
            frame.Layout <- layout
            stack.Add frame
        frame.Before <- this.TakePending()
        frame.EntryStart <- question.Span.Start
        frame.Question <- None
        frame.BeforeColon <- []
        frame.Colon <- None
        frame.Phase <- Phase.AfterQuestion
        frame.LastEnd <- question.Span.Finish
        this.StartRequest(RequestKind.ExplicitKey, frame.Col, true, true, question.Span.Finish, false)

    member private this.NodeStep() =
        let tok = this.Next(this.NodeContext())
        match tok.Lexeme with
        | Lexeme.Comment (comment, ownLine) ->
            if ownLine then pending.Add(Trivia.FullLineComment comment) else elementComment <- Some comment
            elementNewLine <- true
            sawNewLine <- true
        | Lexeme.BlankLine ->
            pending.Add(Trivia.BlankLine(Some tok.Span))
            elementNewLine <- true
            sawNewLine <- true
        | Lexeme.LineStart (indent, _) ->
            elementNewLine <- true
            sawNewLine <- true
            if isContinue () then
                failWith YamlErrorCode.MisplacedProperties "Properties in front of an entry need a key behind them on the same line." tok.Span [ props.[0].Span ]
            elif indent > owner then
                ()
            elif indent = owner && indentless && Lexer.nextIsBlockEntry cursor then
                ()
            else
                this.Unread tok
                this.EndMissing()
        | Lexeme.Tag _ -> this.AddProp tok
        | Lexeme.Anchor _ -> this.AddProp tok
        | Lexeme.Scalar _ -> this.StartLeaf tok
        | Lexeme.Alias _ -> this.StartLeaf tok
        | Lexeme.BlockEntry -> this.StartSequence tok
        | Lexeme.ExplicitKey -> this.StartExplicitKey tok
        | Lexeme.Value _ -> this.StartEmptyKey tok
        | Lexeme.FlowSequenceStart -> failWith YamlErrorCode.FlowNotSupportedYet "Flow collections are not supported yet." tok.Span []
        | Lexeme.FlowMappingStart -> failWith YamlErrorCode.FlowNotSupportedYet "Flow collections are not supported yet." tok.Span []
        | Lexeme.DocumentStart ->
            this.Unread tok
            this.EndMissing()
        | Lexeme.DocumentEnd ->
            this.Unread tok
            this.EndMissing()
        | Lexeme.StreamEnd ->
            this.Unread tok
            this.EndMissing()
        | Lexeme.Directive _ ->
            this.Unread tok
            this.EndMissing()
        | _ -> failWith YamlErrorCode.UnexpectedContent "This token cannot start a node." tok.Span []

    // ---------------------------------------------------------------------------------------------
    // Lines

    /// Moves the trivia behind the last node of a document to the `End` of the block collections (rule R4),
    /// or to the `AfterRoot` or `BeforeRoot` of the document when the root is no block collection.
    member private _.PlaceEndTrivia() =
        if stack.Count > 0 then
            let count = pending.Count
            if count > 0 then
                // The deepest frame whose column is at most the column of the comment. The columns of the
                // stack never decrease, so a binary search finds it.
                let frameFor (column: int) : int =
                    let mutable low = 0
                    let mutable high = stack.Count - 1
                    let mutable found = 0
                    while low <= high do
                        let middle = (low + high) / 2
                        if stack.[middle].Col <= column then
                            found <- middle
                            low <- middle + 1
                        else
                            high <- middle - 1
                    found
                let columnOf (trivia: Trivia) : int =
                    match trivia with
                    | Trivia.FullLineComment comment ->
                        match comment.Span with
                        | Some span -> span.Start.Column - 1
                        | None -> 0
                    | Trivia.BlankLine _ -> -1
                // The comments go first, in the order of the text. A comment goes to no deeper End than the comment
                // before it, so the comments keep their order when the Ends are written from the inner to the outer one.
                let targets = Array.zeroCreate<int> count
                let mutable deepest = stack.Count - 1
                let mutable lastCommentTarget = 0
                for i in 0 .. count - 1 do
                    let column = columnOf pending.[i]
                    if column >= 0 then
                        deepest <- min deepest (frameFor column)
                        targets.[i] <- deepest
                        lastCommentTarget <- deepest
                // A blank line goes with the comment that follows it, and the blank lines behind the last comment go with it.
                // Without any comment they go to the root collection.
                let mutable carry = lastCommentTarget
                for i in count - 1 .. -1 .. 0 do
                    if columnOf pending.[i] >= 0 then carry <- targets.[i] else targets.[i] <- carry
                for i in 0 .. count - 1 do
                    stack.[targets.[i]].End.Add pending.[i]
                pending.Clear()
        elif docOpen && pending.Count > 0 then
            if YamlNode.isMissingValue docRoot then docBefore.AddRange pending else docAfter.AddRange pending
            pending.Clear()

    member private this.Terminate(tok: Tok) =
        match tok.Lexeme with
        | Lexeme.StreamEnd -> if docOpen then this.PlaceEndTrivia()
        | Lexeme.DocumentEnd -> if docOpen then this.PlaceEndTrivia()
        | _ -> ()
        while stack.Count > 0 do
            this.CloseTop()
        this.DocumentToken tok

    /// The first token of a line that no open collection owns: the start of a bare document, or text behind a
    /// root that has ended.
    member private this.DocumentContent(tok: Tok) =
        if docOpen then
            failWith YamlErrorCode.UnexpectedContent "Text cannot follow the root node of a document." tok.Span (Option.toList docRoot.Span)
        if hasDirective then
            failWith YamlErrorCode.DirectiveWithoutDocumentStart "A directive needs a `---` behind it." tok.Span []
        this.BeginDocument(None, tok.Span.Start)
        this.Unread tok
        this.StartRequest(RequestKind.Root, -1, false, false, tok.Span.Start, true)

    member private _.BeginDocument(marker: DocumentMarker option, start: SourceMark) =
        docOpen <- true
        docMarker <- marker
        docStart <- start
        docRoot <- YamlNode.empty
        docBefore.Clear()
        docAfter.Clear()

    member private _.FinishDocument(endMarker: DocumentMarker option) =
        let finish =
            match endMarker with
            | Some marker ->
                match marker.Span with
                | Some span -> span.Finish
                | None -> endOf docRoot
            | None -> endOf docRoot
        parts.Add(
            StreamPart.Document
                { Prelude = List.ofSeq docPrelude
                  StartMarker = docMarker
                  BeforeRoot = List.ofSeq docBefore
                  Root = docRoot
                  AfterRoot = List.ofSeq docAfter
                  EndMarker = endMarker
                  Span = Some (spanBetween docStart finish) }
        )
        docPrelude.Clear()
        docBefore.Clear()
        docAfter.Clear()
        tagHandles.Clear()
        anchors.Clear()
        yamlDirective <- None
        hasDirective <- false
        docOpen <- false

    /// The text of a document end marker is a comment at most.
    member private this.EndMarkerTail() : YamlComment option =
        let next = this.Next(this.RootContext())
        match next.Lexeme with
        | Lexeme.Comment (comment, false) -> Some comment
        | Lexeme.Scalar _ | Lexeme.Alias _ | Lexeme.Tag _ | Lexeme.Anchor _ | Lexeme.BlockEntry | Lexeme.ExplicitKey | Lexeme.Value _ ->
            failWith YamlErrorCode.UnexpectedContent "Only a comment can follow a `...` on its line." next.Span []
        | _ ->
            this.Unread next
            None

    member private this.FlushPendingToPrelude() =
        for trivia in pending do
            docPrelude.Add(PreludeItem.PreludeTrivia trivia)
        pending.Clear()

    member private this.DocumentToken(tok: Tok) =
        match tok.Lexeme with
        | Lexeme.Directive directive ->
            if docOpen then
                failWith YamlErrorCode.UnexpectedContent "A directive cannot stand inside a document." tok.Span []
            match directive with
            | DirectiveKind.YamlVersion (major, _) ->
                if yamlDirective.IsSome then
                    failWith YamlErrorCode.DuplicateYamlDirective "A document has at most one %YAML directive." tok.Span (Option.toList yamlDirective)
                if major <> 1 then
                    failWith YamlErrorCode.UnsupportedYamlVersion "Only YAML 1.x documents are supported." tok.Span []
                yamlDirective <- Some tok.Span
            | DirectiveKind.TagDirective (handle, prefix) ->
                match tagHandles.TryGetValue handle with
                | true, (_, first) ->
                    failWith YamlErrorCode.DuplicateTagDirective (sprintf "The tag handle %s is declared twice." handle) tok.Span [ first ]
                | _ -> tagHandles.[handle] <- (prefix, tok.Span)
            | DirectiveKind.ReservedDirective _ -> ()
            this.FlushPendingToPrelude()
            let trailing = this.EolComment()
            docPrelude.Add(
                PreludeItem.Directive
                    { Kind = directive
                      TrailingComment = trailing
                      Span = Some tok.Span }
            )
            if not hasDirective then directiveStart <- tok.Span.Start
            hasDirective <- true
        | Lexeme.DocumentStart ->
            if docOpen then this.FinishDocument None
            this.FlushPendingToPrelude()
            let trailing = this.EolComment()
            // [209] l-directive-document makes the directives part of the document, and the comments in front of the
            // first one stay outside. Without a directive the document starts at its marker.
            this.BeginDocument(
                Some
                    { TrailingComment = trailing
                      Span = Some tok.Span },
                (if hasDirective then directiveStart else tok.Span.Start)
            )
            this.StartRequest(RequestKind.Root, -1, false, false, tok.Span.Finish, false)
        | Lexeme.DocumentEnd ->
            let trailing = this.EndMarkerTail()
            let marker: DocumentMarker =
                { TrailingComment = trailing
                  Span = Some tok.Span }
            if docOpen then
                this.FinishDocument(Some marker)
            else
                if hasDirective then
                    failWith YamlErrorCode.DirectiveWithoutDocumentStart "A directive needs a `---` behind it." tok.Span []
                for trivia in pending do
                    parts.Add(StreamPart.Trivia trivia)
                pending.Clear()
                parts.Add(StreamPart.ExtraEndMarker marker)
        | Lexeme.StreamEnd ->
            if docOpen then
                this.FinishDocument None
            else
                if hasDirective then
                    failWith YamlErrorCode.DirectiveWithoutDocumentStart "A directive needs a `---` behind it." tok.Span []
                for trivia in pending do
                    parts.Add(StreamPart.Trivia trivia)
                pending.Clear()
            finished <- true
        | _ -> failWith YamlErrorCode.UnexpectedContent "A document marker or the end of the text was expected." tok.Span []

    /// The first token of a line, after the collections that are deeper than the line have been closed.
    member private this.AfterLineStart(indent: int) =
        // The frames stay open until the first token is known. A document marker or the end of the text closes
        // them itself, and the trivia in front of it are placed by the columns of the open collections.
        let tok = this.Next(this.LineContext())
        match tok.Lexeme with
        | Lexeme.DocumentStart -> this.Terminate tok
        | Lexeme.DocumentEnd -> this.Terminate tok
        | Lexeme.StreamEnd -> this.Terminate tok
        | Lexeme.Directive _ -> this.Terminate tok
        | _ ->
            // The collection that the line closes last, which has the smallest indentation above the line.
            let mutable closed: Frame = null
            while stack.Count > 0 && stack.[stack.Count - 1].Col > indent do
                closed <- stack.[stack.Count - 1]
                this.CloseTop()
            if stack.Count = 0 then
                this.DocumentContent tok
            else
                let top = stack.[stack.Count - 1]
                if top.Col < indent then
                    if isNull closed then
                        failWith YamlErrorCode.BadIndentation "This line is indented deeper than the node in front of it allows." tok.Span [ spanBetween top.EntryStart top.LastEnd ]
                    else
                        failWith YamlErrorCode.BadIndentation "This line is indented less than the collection it ends and matches the indentation of no open collection." tok.Span [ spanBetween closed.Start closed.LastEnd ]
                this.Continue tok

    /// The first token of a line that stands at the column of the open collection on top.
    member private this.Continue(tok: Tok) =
        let mutable again = true
        while again do
            again <- false
            let top = stack.[stack.Count - 1]
            if top.IsMap then
                match tok.Lexeme with
                | Lexeme.BlockEntry ->
                    failWith YamlErrorCode.MixedCollections "A mapping takes entries that start with a key, and a dash starts a sequence entry." tok.Span [ top.FirstSpan ]
                | Lexeme.FlowSequenceStart -> failWith YamlErrorCode.FlowNotSupportedYet "Flow collections are not supported yet." tok.Span []
                | Lexeme.FlowMappingStart -> failWith YamlErrorCode.FlowNotSupportedYet "Flow collections are not supported yet." tok.Span []
                | Lexeme.Value _ ->
                    if top.AwaitsColon then
                        top.BeforeColon <- this.TakePending()
                        top.Phase <- Phase.AfterColon
                        this.StartRequest(RequestKind.ExplicitValue, top.Col, true, true, tok.Span.Finish, false)
                    else
                        this.StartRequest(RequestKind.Continue, top.Col, false, false, tok.Span.Start, true)
                        this.Unread tok
                | _ ->
                    if top.AwaitsColon then this.FinishWithoutValue top
                    this.StartRequest(RequestKind.Continue, top.Col, false, false, tok.Span.Start, true)
                    this.Unread tok
            else
                match tok.Lexeme with
                | Lexeme.BlockEntry ->
                    this.StartRequest(RequestKind.Continue, top.Col, false, false, tok.Span.Start, true)
                    this.StartSequence tok
                | _ ->
                    if top.Indentless then
                        this.CloseTop()
                        again <- true
                    else
                        failWith YamlErrorCode.MixedCollections "A sequence takes entries that start with a dash only." tok.Span [ top.FirstSpan ]

    member private this.LineStep() =
        let tok = this.Next(this.LineContext())
        match tok.Lexeme with
        | Lexeme.StreamStart -> ()
        | Lexeme.Comment (comment, true) -> pending.Add(Trivia.FullLineComment comment)
        | Lexeme.BlankLine -> pending.Add(Trivia.BlankLine(Some tok.Span))
        | Lexeme.LineStart (indent, _) -> this.AfterLineStart indent
        | Lexeme.DocumentStart -> this.Terminate tok
        | Lexeme.DocumentEnd -> this.Terminate tok
        | Lexeme.StreamEnd -> this.Terminate tok
        | Lexeme.Directive _ -> this.Terminate tok
        | _ -> failWith YamlErrorCode.UnexpectedContent "This text cannot follow the node in front of it." tok.Span []

    member this.Run() : YamlStream =
        while not finished do
            if requested then this.NodeStep() else this.LineStep()
        { Parts = List.ofSeq parts }

/// The parser of the candidate. This milestone reads block collections, scalars, aliases, properties and
/// documents. A flow collection stops the parser with `FlowNotSupportedYet`.
module Parser =

    /// Parses the text from the cursor on. A cursor that has been used does not start at the beginning.
    let internal parseCursor (cursor: Cursor) : Result<YamlStream, YamlError> =
        try
            Ok (BlockParser(cursor).Run())
        with ParseFailure error ->
            Error { error with Message = sprintf "Line %d: %s" error.Span.Start.Line error.Message }

    /// Parses a whole text into a stream. The text may start with a byte order mark.
    let parseStream (text: string) : Result<YamlStream, YamlError> = parseCursor (Cursor(text))
