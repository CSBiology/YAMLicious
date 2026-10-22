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

/// What an open collection waits for. A block sequence is `AwaitingEntry` or `AfterDash`. A block mapping is
/// `AwaitingEntry`, `AfterQuestion`, `AwaitingColon` (an explicit key without its value indicator yet) or
/// `AfterColon`.
///
/// A flow sequence cycles through `FlowItem` (an item is next, which holds right behind the `[` and behind
/// every `,`) and `FlowAfterItem` (an item is complete and a `,`, a `]` or the `:` of a flow pair follows). A
/// flow mapping and a flow pair, the one-entry mapping inside a flow sequence, cycle through `FlowEntry` (an
/// entry is next, flow mapping only), `FlowQuestion` (the explicit key is next), `FlowAfterKey` (the key is
/// complete), `FlowColon` (the value is next) and `FlowAfterValue` (the value is complete).
[<RequireQualifiedAccess>]
type internal Phase =
    | AwaitingEntry
    | AfterDash
    | AfterQuestion
    | AwaitingColon
    | AfterColon
    | FlowItem
    | FlowAfterItem
    | FlowEntry
    | FlowQuestion
    | FlowAfterKey
    | FlowColon
    | FlowAfterValue

/// The kind of collection that a frame stands for. Block and flow frames share one stack, because a flow
/// collection can only stand on top of the block collections that hold it.
[<RequireQualifiedAccess>]
type internal FrameKind =
    | Block
    | FlowSequence
    | FlowMapping
    /// The one-entry mapping of an entry such as `a: b` or `? a : b` inside a flow sequence.
    | FlowPair

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

/// An open collection. The columns of the block frames on the stack never decrease from the bottom to the
/// top. An indentless sequence has the column of the mapping below it. The flow frames stand on top of the
/// block frames and have no column, because the document ends only behind the last flow frame.
[<AllowNullLiteral>]
type internal Frame(isMap: bool, col: int, start: SourceMark) =
    member _.IsMap = isMap
    /// The column of the first token of the collection, counted from 0: the dash of a sequence or the first
    /// token of the first key of a mapping.
    member _.Col = col
    member val Kind = FrameKind.Block with get, set
    /// Flow frames. The span of the `[` or `{` that opened the collection, for the errors about its end. A
    /// flow pair holds the opener of the sequence around it.
    member val Opener: SourceSpan = { Start = start; Finish = start } with get, set
    /// Flow frames. True for the outermost collection, whose node goes to the block structure.
    member val Outermost = false with get, set
    /// Flow frames. True when a tab stands among the blanks in front of the opener.
    member val OpenerTab = false with get, set
    /// Flow frames. The comment behind the opener.
    member val OpeningComment: YamlComment option = None with get, set
    /// The outermost flow frame. The trivia between the last property of the collection and the opener.
    member val Following: Trivia list = [] with get, set
    /// Flow frames. The node that is complete and waits for the token behind it. This is an item of a
    /// sequence, a key or a value, and the token decides which comment slot holds the comment behind it.
    member val Pend = YamlNode.empty with get, set
    /// Flow frames. The end of `Pend`, or the end of the indicator that the node behind it follows.
    member val PendEnd = start with get, set
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

/// The parser. It owns the structure: indentation, collection membership, the roles of keys and values, the
/// properties of a node, the documents and the slot of every comment and blank line. It pulls one lexeme at
/// a time from `Lexer.next` and tells the lexer the context of the position.
///
/// The control flow is a loop over explicit state. `stack` holds the open collections, the block ones and
/// the flow ones on top of them. In the block structure one node request at a time says what the next node
/// has to be and where it stands. A request ends when its node is complete, and the node goes to the frame
/// on top, or becomes the root. A `[` or `{` in a request pushes a flow frame and suspends the request. The
/// flow frames read their own nodes, and when the outermost one closes, its node completes the request as a
/// scalar would. No call recurses with the depth of the input.
///
/// Each character goes through the lexer once, each frame is pushed and popped once and each trivia line
/// moves to its slot once, so the time is linear in the length of the input.
type internal YamlParser(cursor: Cursor) =
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
    // [211] l-yaml-stream lets a byte order mark follow any document, so the mark ends the document that is
    // open. Only comment lines, blank lines, more marks, `---` and `...` may follow it. `bomSpan` is the
    // first mark of the run.
    let mutable bomSeen = false
    let mutable bomSpan = spanAt zeroMark

    // The flow collections. `flowCount` is the number of flow frames on the stack, and `flowOwner` the
    // parent indentation that the lexer needs inside them.
    let mutable flowCount = 0
    let mutable flowOwner = -1
    // The node kind of the outermost flow collection while it completes the request that opened it.
    let mutable flowKind = YamlNodeKind.Alias ""
    // The gap behind a token of a flow collection: the comment on the line of the token, the question whether
    // a line break stands between the token and the next one, and whether the next token is read already.
    // The trivia lines of the gap go to `pending`.
    let mutable gapComment: YamlComment option = None
    let mutable gapNewLine = false
    let mutable gapReady = false
    // The node in progress inside a flow collection: its properties, the trivia in front of the first one, the
    // line break and the comment behind the last one, and the same for the indicator in front of the node.
    let flowProps = ResizeArray<Prop>()
    let mutable flowLead: Trivia list = []
    let mutable flowNewLine = false
    let mutable flowComment: YamlComment option = None
    let mutable flowIndicatorNewLine = false
    let mutable flowIndicatorComment: YamlComment option = None

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

    /// The property that a tag or an anchor lexeme spells. An anchor is defined from here on, so an alias
    /// inside the node that carries it may name it.
    member private this.MakeProp(tok: Tok) : Prop =
        match tok.Lexeme with
        | Lexeme.Tag token -> Prop(PropertyName.Tag, Some (this.ResolveTag(tok, token)), None, tok.Span, tok.Tab)
        | Lexeme.Anchor name ->
            anchors.Add name |> ignore
            Prop(PropertyName.Anchor, None, Some name, tok.Span, tok.Tab)
        | _ -> failWith YamlErrorCode.UnexpectedContent "A property was expected." tok.Span []

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
        props.Add(this.MakeProp tok)

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

    /// The tag and the anchor of the properties of `list` from `first` up to but excluding `until`.
    member private _.BuildPropsOf(list: ResizeArray<Prop>, first: int, until: int) : NodeProperties =
        let mutable tag: YamlTag option = None
        let mutable anchor: string option = None
        let mutable tagSpan = spanAt zeroMark
        let mutable anchorSpan = spanAt zeroMark
        for i in first .. until - 1 do
            let prop = list.[i]
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

    /// The tag and the anchor of the properties of the request from `first` up to but excluding `until`.
    member private this.BuildProps(first: int, until: int) : NodeProperties = this.BuildPropsOf(props, first, until)

    /// The placements of the properties of `list` from `first` up to but excluding `until`. `lastFollowing`
    /// holds the trivia between the last property and the content, and `contentFollows` is false for a node
    /// without content.
    member private _.BuildLayoutOf(list: ResizeArray<Prop>, first: int, until: int, lastFollowing: Trivia list, contentFollows: bool) : PropertyPlacement list =
        let layout = ResizeArray<PropertyPlacement>()
        for i in first .. until - 1 do
            let prop = list.[i]
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

    /// The placements of the properties of the request. See `BuildLayoutOf`.
    member private this.BuildLayout(first: int, until: int, lastFollowing: Trivia list, contentFollows: bool) : PropertyPlacement list =
        this.BuildLayoutOf(props, first, until, lastFollowing, contentFollows)

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
            | _ -> failwith "The parser delivered a block node to a flow frame."

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
            // The closer of the outermost flow collection stands for the whole collection.
            | Lexeme.FlowSequenceEnd -> flowKind
            | Lexeme.FlowMappingEnd -> flowKind
            | _ -> failWith YamlErrorCode.UnexpectedContent "A scalar, an alias or a flow collection was expected." tok.Span []
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
            | Lexeme.FlowSequenceEnd -> flowKind
            | Lexeme.FlowMappingEnd -> flowKind
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

    /// Ends the request with the scalar, the alias or the flow collection that `tok` stands for. When `peek`
    /// is true the next lexeme decides: a `:` makes the node the key of an entry, and a comment on the same
    /// line is the trailing comment of the node.
    member private this.FinishLeaf(tok: Tok, peek: bool) =
        let mutable isKey = false
        let mutable colon = tok
        let mutable trailing: YamlComment option = None
        if peek then
            let next = this.Next(this.RootContext())
            match next.Lexeme with
            | Lexeme.Value ->
                isKey <- true
                colon <- next
            | Lexeme.Comment (comment, false) -> trailing <- Some comment
            | _ -> this.Unread next
        if isKey then this.AddKeyEntry(tok, colon) else this.CompleteLeaf(tok, trailing)

    member private this.StartLeaf(tok: Tok) =
        if props.Count > 0 && props.[props.Count - 1].Span.Finish = tok.Span.Start then
            failWith YamlErrorCode.MissingSeparation "A tag or an anchor needs a blank behind it." tok.Span [ props.[props.Count - 1].Span ]
        let isBlock =
            match tok.Lexeme with
            | Lexeme.Scalar scalar -> isBlockScalar scalar
            | _ -> false
        this.FinishLeaf(tok, not isBlock)

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
        | Lexeme.Value -> this.StartEmptyKey tok
        | Lexeme.FlowSequenceStart -> this.OpenOutermost tok
        | Lexeme.FlowMappingStart -> this.OpenOutermost tok
        | Lexeme.ByteOrderMark ->
            // The mark stands at the start of a line, so it ends the node request like a line at the column 0.
            elementNewLine <- true
            sawNewLine <- true
            this.Unread tok
            this.EndMissing()
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
    // Flow collections
    //
    // A flow collection is a frame on the same stack as the block collections. A flow sequence cycles through
    // FlowItem and FlowAfterItem. A flow mapping, and the flow pair that stands for an entry such as `a: b`
    // inside a flow sequence, cycle through FlowEntry, FlowQuestion, FlowAfterKey, FlowColon and FlowAfterValue.
    // Every step reads at most one node or one punctuation token, so the depth of the collections costs frames
    // and no calls.
    //
    // The gap. Flow trivia carry no LineStart, so the parser reads the gap behind every token it consumes: the
    // comment on the line of the token, then the comment lines and the blank lines up to the next token, which
    // it puts back. Each handler works on that gap. The comment lands in `gapComment`, the lines in `pending`.
    // A flow node is complete only when the token behind it is known, because that token decides the slot of the
    // comment behind the node (rules R6, R7 and R8): the trailing comment of the node, the gap in front of the
    // colon, or the trivia in front of the next entry.

    /// The lexer context inside a flow collection. [69] s-flow-line-prefix(n) and [80] s-separate(n,c) ask for
    /// the lines of the collection to stand deeper than the block node that owns it, with n one above the
    /// column of the owner, and the lexer applies that rule to every line. It lets a line that starts with the
    /// closer, `]` or `}`, stand at the column of the owner, as in `expressionLib: [` followed by `]` at the
    /// column 0. That deviation is deliberate. ruamel (cwltool) and yaml@2.6.1 read that layout, and real CWL
    /// files use it. yaml@2.6.1 reads it for the outermost collection only, so the context is FlowIn when the
    /// next closer would end the outermost collection and FlowInNested otherwise. A flow pair has no closer of its
    /// own, so a closer behind a pair ends the sequence below it. The key of a flow pair is not known to be a
    /// key before the colon follows, so the parser checks its line and length afterwards, as it does for a block
    /// key. FlowKey and FlowOut stay unused.
    member private _.FlowContext() : LexContext =
        let top = stack.[stack.Count - 1]
        let ended = if top.Kind = FrameKind.FlowPair then stack.[stack.Count - 2] else top
        { Grammar = (if ended.Outermost then GrammarContext.FlowIn else GrammarContext.FlowInNested)
          ParentIndent = flowOwner
          SimpleKey = false }

    /// Reads the gap behind a token of a flow collection that ends at `previousEnd`. The next token goes back
    /// into the buffer, and `gapReady` says so.
    member private this.FlowGap(previousEnd: SourceMark) =
        let context = this.FlowContext()
        let held = pending.Count
        gapComment <- None
        let mutable tok = this.Next context
        match tok.Lexeme with
        | Lexeme.Comment (comment, false) ->
            gapComment <- Some comment
            tok <- this.Next context
        | _ -> ()
        let mutable reading = true
        while reading do
            match tok.Lexeme with
            | Lexeme.Comment (comment, _) ->
                pending.Add(Trivia.FullLineComment comment)
                tok <- this.Next context
            | Lexeme.BlankLine ->
                pending.Add(Trivia.BlankLine(Some tok.Span))
                tok <- this.Next context
            | _ -> reading <- false
        gapNewLine <- gapComment.IsSome || pending.Count > held || tok.Span.Start.Line > previousEnd.Line
        this.Unread tok
        gapReady <- true

    /// The token behind the gap that follows a token ending at `previousEnd`. A gap that is read already is
    /// not read again.
    member private this.FlowTake(previousEnd: SourceMark) : Tok =
        if not gapReady then this.FlowGap previousEnd
        gapReady <- false
        this.Next(this.FlowContext())

    /// Puts a token back together with the gap in front of it.
    member private this.FlowUnread(tok: Tok) =
        this.Unread tok
        gapReady <- true

    member private _.FlowName(top: Frame) : string =
        if top.Kind = FrameKind.FlowMapping then "mapping" else "sequence"

    member private this.FailUnclosed(top: Frame, at: SourceSpan) : unit =
        failWith
            YamlErrorCode.UnclosedFlow
            (sprintf "The flow %s that starts on line %d is not closed." (this.FlowName top) top.Opener.Start.Line)
            at
            [ top.Opener ]

    member private this.FailMismatch(top: Frame, closer: Tok) : unit =
        let text =
            match closer.Lexeme with
            | Lexeme.FlowSequenceEnd -> "]"
            | _ -> "}"
        failWith
            YamlErrorCode.MismatchedFlowCloser
            (sprintf "A `%s` cannot close the flow %s that starts on line %d." text (this.FlowName top) top.Opener.Start.Line)
            closer.Span
            [ top.Opener ]

    /// True when the closer ends the collection of the frame. A flow pair has no closer of its own: the
    /// closer of the sequence around it ends it.
    member private _.CloserMatches(top: Frame, closer: Tok) : bool =
        match closer.Lexeme with
        | Lexeme.FlowSequenceEnd -> top.Kind <> FrameKind.FlowMapping
        | Lexeme.FlowMappingEnd -> top.Kind = FrameKind.FlowMapping
        | _ -> false

    /// A `[` or `{` in the block structure opens the outermost flow collection. The request that met it stays
    /// as it is, with its properties and the element state, and waits for the node of the collection.
    member private this.OpenOutermost(opener: Tok) =
        if props.Count > 0 && props.[props.Count - 1].Span.Finish = opener.Span.Start then
            failWith YamlErrorCode.MissingSeparation "A tag or an anchor needs a blank behind it." opener.Span [ props.[props.Count - 1].Span ]
        flowOwner <- this.NodeContext().ParentIndent
        let following = this.TakePending()
        this.PushFlow(opener, following, true)

    /// Pushes the frame of the collection that `opener` opens. The properties of a nested collection are in
    /// `flowProps`. The outermost collection leaves them to the request in the block structure, which also
    /// decides whether they belong to the collection or to a mapping around it.
    member private this.PushFlow(opener: Tok, following: Trivia list, outermost: bool) =
        let isMap =
            match opener.Lexeme with
            | Lexeme.FlowMappingStart -> true
            | _ -> false
        let count = if outermost then 0 else flowProps.Count
        let start = if count > 0 then flowProps.[0].Span.Start else opener.Span.Start
        let frame = Frame(isMap, -1, start)
        frame.Kind <- (if isMap then FrameKind.FlowMapping else FrameKind.FlowSequence)
        frame.Phase <- (if isMap then Phase.FlowEntry else Phase.FlowItem)
        frame.Opener <- opener.Span
        frame.OpenerTab <- opener.Tab
        frame.Outermost <- outermost
        frame.PendEnd <- opener.Span.Finish
        frame.LastEnd <- opener.Span.Finish
        if outermost then
            frame.Following <- following
        elif count > 0 then
            let nodeProps = this.BuildPropsOf(flowProps, 0, count)
            frame.Tag <- nodeProps.Tag
            frame.Anchor <- nodeProps.Anchor
            frame.Layout <- this.BuildLayoutOf(flowProps, 0, count, following, true)
        stack.Add frame
        flowCount <- flowCount + 1
        this.FlowGap opener.Span.Finish
        frame.OpeningComment <- gapComment
        gapComment <- None

    member private this.FlowStep() =
        let top = stack.[stack.Count - 1]
        match top.Phase with
        | Phase.FlowItem -> this.FlowItemStep top
        | Phase.FlowAfterItem -> this.FlowAfterItemStep top
        | Phase.FlowEntry -> this.FlowEntryStep top
        | Phase.FlowQuestion -> this.FlowNode(top, this.FlowTake top.LastEnd)
        | Phase.FlowColon -> this.FlowNode(top, this.FlowTake top.LastEnd)
        | Phase.FlowAfterKey -> this.FlowAfterKeyStep top
        | Phase.FlowAfterValue -> this.FlowAfterValueStep top
        | _ -> failwith "The parser reached a flow step on a block frame."

    /// Stores the separation behind the `?` or the `:` that the node in progress follows.
    member private _.SetFlowIndicator(top: Frame, sep: Separation) =
        match top.Phase with
        | Phase.FlowQuestion -> top.Question <- Some sep
        | Phase.FlowColon -> top.Colon <- Some sep
        | _ -> ()

    /// Hands a complete node to the frame on top. The node waits there for the token behind it.
    member private _.FlowDeliver(top: Frame, node: YamlNode) =
        top.Pend <- node
        top.PendEnd <- endOf node
        match top.Phase with
        | Phase.FlowItem -> top.Phase <- Phase.FlowAfterItem
        | Phase.FlowEntry -> top.Phase <- Phase.FlowAfterKey
        | Phase.FlowQuestion -> top.Phase <- Phase.FlowAfterKey
        | Phase.FlowColon -> top.Phase <- Phase.FlowAfterValue
        | _ -> failwith "The parser delivered a flow node to a frame that waits for no node."

    /// An item of a flow sequence is next.
    member private this.FlowItemStep(top: Frame) =
        let tok = this.FlowTake top.LastEnd
        match tok.Lexeme with
        | Lexeme.FlowSequenceEnd -> this.CloseFlow(top, tok)
        | Lexeme.FlowMappingEnd -> this.FailMismatch(top, tok)
        | Lexeme.FlowEntry ->
            failWith YamlErrorCode.EmptyFlowEntry "A `,` needs an entry in front of it." tok.Span [ top.Opener ]
        | Lexeme.ExplicitKey -> this.StartPair(top, tok)
        | Lexeme.Value -> this.StartPair(top, tok)
        | Lexeme.StreamEnd -> this.FailUnclosed(top, tok.Span)
        | _ ->
            top.Before <- this.TakePending()
            top.EntryStart <- tok.Span.Start
            this.FlowNode(top, tok)

    /// A `?` or a `:` at the start of an item of a flow sequence starts a flow pair with an explicit or an
    /// empty key.
    member private this.StartPair(sequence: Frame, indicator: Tok) =
        sequence.Before <- this.TakePending()
        let pair = Frame(true, -1, indicator.Span.Start)
        pair.Kind <- FrameKind.FlowPair
        pair.Opener <- sequence.Opener
        pair.EntryStart <- indicator.Span.Start
        pair.LastEnd <- indicator.Span.Finish
        match indicator.Lexeme with
        | Lexeme.ExplicitKey -> pair.Phase <- Phase.FlowQuestion
        | _ ->
            pair.KeyNode <- missingNode indicator.Span.Start
            pair.Phase <- Phase.FlowColon
        stack.Add pair
        flowCount <- flowCount + 1
        this.FlowGap indicator.Span.Finish

    /// The item is complete. A `,` ends it, a closer ends it and the collection, and a `:` makes it the key of
    /// a flow pair.
    member private this.FlowAfterItemStep(top: Frame) =
        let tok = this.FlowTake top.PendEnd
        let comment = gapComment
        gapComment <- None
        match tok.Lexeme with
        | Lexeme.FlowEntry ->
            let trailing = this.FlowAfterComma(tok, top.PendEnd, comment)
            this.AddItem(top, trailing)
            top.Phase <- Phase.FlowItem
        | Lexeme.FlowSequenceEnd ->
            this.AddItem(top, comment)
            this.CloseFlow(top, tok)
        | Lexeme.FlowMappingEnd -> this.FailMismatch(top, tok)
        | Lexeme.Value -> this.ConvertToPair(top, tok, comment)
        | Lexeme.StreamEnd -> this.FailUnclosed(top, tok.Span)
        | _ -> failWith YamlErrorCode.MissingFlowComma "Two entries of a flow collection need a `,` between them." tok.Span [ top.Opener ]

    member private _.AddItem(top: Frame, trailing: YamlComment option) =
        let node = { top.Pend with TrailingComment = trailing }
        top.Items.Add
            { Before = top.Before
              Dash = None
              Node = node
              Span = Some (spanBetween top.EntryStart top.PendEnd) }

    /// The item before a `:` is the implicit key of a flow pair. [154] ns-s-implicit-yaml-key keeps such a key
    /// on one line with its colon and at 1024 characters at most.
    member private this.ConvertToPair(sequence: Frame, colon: Tok, comment: YamlComment option) =
        let key = sequence.Pend
        let keySpan =
            match key.Span with
            | Some span -> span
            | None -> spanAt sequence.PendEnd
        if comment.IsSome || gapNewLine || keySpan.Start.Line <> keySpan.Finish.Line then
            failWith YamlErrorCode.ImplicitKeyMultiline "The key of a flow pair has to stand on one line with its colon." keySpan []
        if colon.Span.Start.Column - keySpan.Start.Column > MaxKeyLength then
            failWith YamlErrorCode.ImplicitKeyLength "An implicit key can have at most 1024 characters, from its properties to its colon." (spanBetween keySpan.Start colon.Span.Start) []
        let pair = Frame(true, -1, keySpan.Start)
        pair.Kind <- FrameKind.FlowPair
        pair.Opener <- sequence.Opener
        pair.EntryStart <- keySpan.Start
        pair.KeyNode <- key
        pair.LastEnd <- colon.Span.Finish
        pair.Phase <- Phase.FlowColon
        stack.Add pair
        flowCount <- flowCount + 1
        this.FlowGap colon.Span.Finish

    /// Consumes the `,` behind a node that ends at `nodeEnd` and reads the gap behind the comma. Returns the
    /// trailing comment of the node. That is the comment on the line of the node, or else the comment behind
    /// the comma when the comma stands on the line where the node ends (rule R6). Any other comment is a
    /// comment line in front of the next entry.
    member private this.FlowAfterComma(comma: Tok, nodeEnd: SourceMark, comment: YamlComment option) : YamlComment option =
        let held = pending.Count
        this.FlowGap comma.Span.Finish
        let behind = gapComment
        gapComment <- None
        match comment with
        | Some _ ->
            match behind with
            | Some extra -> pending.Insert(held, Trivia.FullLineComment extra)
            | None -> ()
            comment
        | None ->
            match behind with
            | Some extra ->
                if comma.Span.Start.Line = nodeEnd.Line then
                    behind
                else
                    pending.Insert(held, Trivia.FullLineComment extra)
                    None
            | None -> None

    /// An entry of a flow mapping is next.
    member private this.FlowEntryStep(top: Frame) =
        let tok = this.FlowTake top.LastEnd
        match tok.Lexeme with
        | Lexeme.FlowMappingEnd -> this.CloseFlow(top, tok)
        | Lexeme.FlowSequenceEnd -> this.FailMismatch(top, tok)
        | Lexeme.FlowEntry ->
            failWith YamlErrorCode.EmptyFlowEntry "A `,` needs an entry in front of it." tok.Span [ top.Opener ]
        | Lexeme.StreamEnd -> this.FailUnclosed(top, tok.Span)
        | _ ->
            top.Before <- this.TakePending()
            top.EntryStart <- tok.Span.Start
            top.Question <- None
            top.Colon <- None
            top.BeforeColon <- []
            match tok.Lexeme with
            | Lexeme.ExplicitKey ->
                top.LastEnd <- tok.Span.Finish
                this.FlowGap tok.Span.Finish
                top.Phase <- Phase.FlowQuestion
            | Lexeme.Value ->
                top.KeyNode <- missingNode tok.Span.Start
                top.LastEnd <- tok.Span.Finish
                this.FlowGap tok.Span.Finish
                top.Phase <- Phase.FlowColon
            | _ -> this.FlowNode(top, tok)

    /// Reads one node of a flow collection from its first token: properties, then a scalar, an alias or a
    /// flow collection, or no content at all. The frame on top says which slot the node fills. The gap behind
    /// the `?` or the `:` in front of it is in `gapComment` and `pending`.
    member private this.FlowNode(top: Frame, first: Tok) =
        let hasIndicator =
            match top.Phase with
            | Phase.FlowQuestion -> true
            | Phase.FlowColon -> true
            | _ -> false
        flowIndicatorNewLine <- hasIndicator && gapNewLine
        flowIndicatorComment <- if hasIndicator then gapComment else None
        gapComment <- None
        flowLead <- []
        flowNewLine <- false
        flowComment <- None
        flowProps.Clear()
        let mutable tok = first
        let mutable reading = true
        while reading do
            match tok.Lexeme with
            | Lexeme.Tag _
            | Lexeme.Anchor _ ->
                if flowProps.Count = 0 then
                    flowLead <- this.TakePending()
                else
                    let previous = flowProps.[flowProps.Count - 1]
                    previous.NewLine <- flowNewLine
                    previous.Comment <- flowComment
                    previous.Trivia <- this.TakePending()
                let prop = this.MakeProp tok
                flowProps.Add prop
                this.FlowGap prop.Span.Finish
                flowNewLine <- gapNewLine
                flowComment <- gapComment
                gapComment <- None
                tok <- this.FlowTake prop.Span.Finish
            | Lexeme.Scalar _ ->
                this.FlowLeaf(top, tok)
                reading <- false
            | Lexeme.Alias _ ->
                this.FlowLeaf(top, tok)
                reading <- false
            | Lexeme.FlowSequenceStart ->
                this.FlowOpen(top, tok)
                reading <- false
            | Lexeme.FlowMappingStart ->
                this.FlowOpen(top, tok)
                reading <- false
            | Lexeme.FlowEntry ->
                this.FlowEmpty(top, tok)
                reading <- false
            | Lexeme.FlowSequenceEnd ->
                this.FlowEmpty(top, tok)
                reading <- false
            | Lexeme.FlowMappingEnd ->
                this.FlowEmpty(top, tok)
                reading <- false
            | Lexeme.Value ->
                this.FlowEmpty(top, tok)
                reading <- false
            | Lexeme.StreamEnd ->
                this.FailUnclosed(top, tok.Span)
                reading <- false
            | _ -> failWith YamlErrorCode.UnexpectedContent "This token cannot start a node of a flow collection." tok.Span [ top.Opener ]

    /// Gives the last property of the node in progress the line break and the comment behind it.
    member private _.FinishFlowProps() =
        if flowProps.Count > 0 then
            let last = flowProps.[flowProps.Count - 1]
            last.NewLine <- flowNewLine
            last.Comment <- flowComment

    member private _.CheckFlowSeparation(tok: Tok) =
        if flowProps.Count > 0 && flowProps.[flowProps.Count - 1].Span.Finish = tok.Span.Start then
            failWith YamlErrorCode.MissingSeparation "A tag or an anchor needs a blank behind it." tok.Span [ flowProps.[flowProps.Count - 1].Span ]

    /// A scalar or an alias is the content of the node.
    member private this.FlowLeaf(top: Frame, tok: Tok) =
        this.CheckFlowSeparation tok
        this.FinishFlowProps()
        let count = flowProps.Count
        let nodeKind =
            match tok.Lexeme with
            | Lexeme.Alias name ->
                if count > 0 then
                    failWith YamlErrorCode.AliasWithProperties "An alias takes no tag and no anchor." flowProps.[0].Span [ tok.Span ]
                this.CheckAlias(tok, name)
                YamlNodeKind.Alias name
            | Lexeme.Scalar scalar -> YamlNodeKind.Scalar scalar
            | _ -> failWith YamlErrorCode.UnexpectedContent "A scalar or an alias was expected." tok.Span []
        let nodeProps = this.BuildPropsOf(flowProps, 0, count)
        let following = this.TakePending()
        let layout = this.BuildLayoutOf(flowProps, 0, count, following, true)
        this.SetFlowIndicator(top, separation flowIndicatorNewLine flowIndicatorComment (if count > 0 then flowLead else following))
        let first = if count > 0 then flowProps.[0].Span.Start else tok.Span.Start
        this.FlowDeliver(
            top,
            { Kind = nodeKind
              Properties = nodeProps
              PropertyLayout = layout
              TrailingComment = None
              Span = Some (spanBetween first tok.Span.Finish) }
        )

    /// A `[` or `{` is the content of the node. The properties in front of it belong to the new collection.
    member private this.FlowOpen(top: Frame, opener: Tok) =
        this.CheckFlowSeparation opener
        this.FinishFlowProps()
        let count = flowProps.Count
        let following = this.TakePending()
        this.SetFlowIndicator(top, separation flowIndicatorNewLine flowIndicatorComment (if count > 0 then flowLead else following))
        this.PushFlow(opener, following, false)

    /// The node has no content: a `,`, a closer or a `:` follows its properties, or follows the indicator in
    /// front of it. The node is an empty plain scalar that holds the properties, or a missing value.
    member private this.FlowEmpty(top: Frame, delimiter: Tok) =
        let count = flowProps.Count
        if count = 0 then
            this.SetFlowIndicator(top, separation false flowIndicatorComment [])
            this.FlowUnread delimiter
            this.FlowDeliver(top, missingNode top.LastEnd)
        else
            // A comment on the line of the last property is the `After` of that property (rule R1), also when
            // a `:` on a later line follows. Only the trivia lines up to the colon go to BeforeColon (rule R7).
            let last = flowProps.[count - 1]
            last.NewLine <- flowNewLine
            last.Comment <- flowComment
            let nodeProps = this.BuildPropsOf(flowProps, 0, count)
            let layout = this.BuildLayoutOf(flowProps, 0, count, [], false)
            this.SetFlowIndicator(top, separation flowIndicatorNewLine flowIndicatorComment flowLead)
            let first = flowProps.[0].Span.Start
            this.FlowUnread delimiter
            this.FlowDeliver(
                top,
                { Kind = YamlNodeKind.Scalar { Value = ""; Style = ScalarStyle.Plain; HeaderComment = None }
                  Properties = nodeProps
                  PropertyLayout = layout
                  TrailingComment = None
                  Span = Some (spanBetween first last.Span.Finish) }
            )

    /// The key of a flow mapping entry or of a flow pair is complete.
    member private this.FlowAfterKeyStep(top: Frame) =
        let tok = this.FlowTake top.PendEnd
        let comment = gapComment
        gapComment <- None
        match tok.Lexeme with
        | Lexeme.Value ->
            // A comment on the line where the key ends is the trailing comment of the key (rule R8), as behind
            // the key of `? "a" # c` and `: v` in a block mapping. A key that holds properties only has taken
            // that comment into the `After` of its last property already (rule R1). The comment lines and the
            // blank lines up to the colon are the gap of rule R7.
            top.BeforeColon <- this.TakePending()
            top.KeyNode <- { top.Pend with TrailingComment = comment }
            top.LastEnd <- tok.Span.Finish
            this.FlowGap tok.Span.Finish
            top.Phase <- Phase.FlowColon
        | Lexeme.FlowEntry ->
            let trailing = this.FlowAfterComma(tok, top.PendEnd, comment)
            this.AfterEntryComma(top, this.KeyOnlyEntry(top, trailing))
        | Lexeme.FlowSequenceEnd ->
            if this.CloserMatches(top, tok) then
                this.AfterEntryCloser(top, this.KeyOnlyEntry(top, comment), tok)
            else
                this.FailMismatch(top, tok)
        | Lexeme.FlowMappingEnd ->
            if this.CloserMatches(top, tok) then
                this.AfterEntryCloser(top, this.KeyOnlyEntry(top, comment), tok)
            else
                this.FailMismatch(top, tok)
        | Lexeme.StreamEnd -> this.FailUnclosed(top, tok.Span)
        | _ -> failWith YamlErrorCode.MissingFlowComma "Two entries of a flow collection need a `,` between them." tok.Span [ top.Opener ]

    /// An entry that is a key alone. The value is missing and the value indicator is omitted. The comment
    /// behind a missing key goes to the separation behind its `?` (rule R1).
    member private _.KeyOnlyEntry(top: Frame, trailing: YamlComment option) : MappingEntry =
        let mutable key = top.Pend
        match trailing with
        | Some text ->
            if YamlNode.isMissingValue key then
                top.Question <- Some (Separation.NewLine { Comment = Some text; Following = [] })
            else
                key <- { key with TrailingComment = trailing }
        | None -> ()
        { Before = top.Before
          Question = top.Question
          Key = key
          BeforeColon = []
          Colon = None
          Value = missingNode (endOf key)
          Span = Some (spanBetween top.EntryStart (endOf key)) }

    /// The value is complete.
    member private this.FlowAfterValueStep(top: Frame) =
        let tok = this.FlowTake top.PendEnd
        let comment = gapComment
        gapComment <- None
        match tok.Lexeme with
        | Lexeme.FlowEntry ->
            let trailing = this.FlowAfterComma(tok, top.PendEnd, comment)
            this.AfterEntryComma(top, this.ValueEntry(top, trailing))
        | Lexeme.FlowSequenceEnd ->
            if this.CloserMatches(top, tok) then
                this.AfterEntryCloser(top, this.ValueEntry(top, comment), tok)
            else
                this.FailMismatch(top, tok)
        | Lexeme.FlowMappingEnd ->
            if this.CloserMatches(top, tok) then
                this.AfterEntryCloser(top, this.ValueEntry(top, comment), tok)
            else
                this.FailMismatch(top, tok)
        | Lexeme.StreamEnd -> this.FailUnclosed(top, tok.Span)
        | _ -> failWith YamlErrorCode.MissingFlowComma "Two entries of a flow collection need a `,` between them." tok.Span [ top.Opener ]

    /// An entry with a value indicator. The comment behind a missing value goes to the separation behind the
    /// colon (rule R1), and the comment behind another value is its trailing comment (rules R6 and R8).
    member private _.ValueEntry(top: Frame, trailing: YamlComment option) : MappingEntry =
        let mutable value = top.Pend
        match trailing with
        | Some text ->
            if YamlNode.isMissingValue value then
                top.Colon <- Some (Separation.NewLine { Comment = Some text; Following = [] })
            else
                value <- { value with TrailingComment = trailing }
        | None -> ()
        { Before = top.Before
          Question = top.Question
          Key = top.KeyNode
          BeforeColon = top.BeforeColon
          Colon = top.Colon
          Value = value
          Span = Some (spanBetween top.EntryStart (endOf value)) }

    /// A `,` ended the entry. A flow mapping goes on with the next entry, and a flow pair is complete.
    member private this.AfterEntryComma(top: Frame, entry: MappingEntry) =
        top.Entries.Add entry
        if top.Kind = FrameKind.FlowPair then this.ClosePair top else top.Phase <- Phase.FlowEntry

    /// A closer ended the entry. It ends the flow mapping, or the flow sequence around a flow pair.
    member private this.AfterEntryCloser(top: Frame, entry: MappingEntry, closer: Tok) =
        top.Entries.Add entry
        if top.Kind = FrameKind.FlowPair then
            this.FlowUnread closer
            this.ClosePair top
        else
            this.CloseFlow(top, closer)

    /// The flow pair becomes a one-entry flow mapping, the item of the sequence below it. The gap behind the
    /// pair is read already, and the sequence goes on with the next item or its closer.
    member private _.ClosePair(pair: Frame) =
        stack.RemoveAt(stack.Count - 1)
        flowCount <- flowCount - 1
        let entry = pair.Entries.[0]
        let node: YamlNode =
            { Kind =
                YamlNodeKind.Mapping
                    { Style = CollectionStyle.Flow
                      OpeningComment = None
                      Items = [ entry ]
                      End = [] }
              Properties = noProperties
              PropertyLayout = []
              TrailingComment = None
              Span = entry.Span }
        let sequence = stack.[stack.Count - 1]
        sequence.Items.Add
            { Before = sequence.Before
              Dash = None
              Node = node
              Span = entry.Span }
        sequence.Phase <- Phase.FlowItem

    /// The closer ends the collection on top. The trivia in front of it go to the end of the collection
    /// (rule R6).
    member private this.CloseFlow(top: Frame, closer: Tok) =
        if not (this.CloserMatches(top, closer)) then this.FailMismatch(top, closer)
        stack.RemoveAt(stack.Count - 1)
        flowCount <- flowCount - 1
        let ending = this.TakePending()
        let nodeKind =
            if top.Kind = FrameKind.FlowMapping then
                YamlNodeKind.Mapping
                    { Style = CollectionStyle.Flow
                      OpeningComment = top.OpeningComment
                      Items = List.ofSeq top.Entries
                      End = ending }
            else
                YamlNodeKind.Sequence
                    { Style = CollectionStyle.Flow
                      OpeningComment = top.OpeningComment
                      Items = List.ofSeq top.Items
                      End = ending }
        let node: YamlNode =
            { Kind = nodeKind
              Properties = { Tag = top.Tag; Anchor = top.Anchor }
              PropertyLayout = top.Layout
              TrailingComment = None
              Span = Some (spanBetween top.Start closer.Span.Finish) }
        if top.Outermost then
            this.FinishOutermost(top, node, closer)
        else
            this.FlowDeliver(stack.[stack.Count - 1], node)

    /// The outermost collection is complete. It ends the request of the block structure like a scalar, and a
    /// `:` or a comment behind its closer is read as it is behind a quoted scalar.
    member private this.FinishOutermost(top: Frame, node: YamlNode, closer: Tok) =
        pending.AddRange top.Following
        flowKind <- node.Kind
        this.FinishLeaf(
            { Lexeme = closer.Lexeme
              Span = spanBetween top.Start closer.Span.Finish
              Tab = top.OpenerTab },
            true
        )

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
        bomSeen <- false

    /// The text of a document end marker is a comment at most.
    member private this.EndMarkerTail() : YamlComment option =
        let next = this.Next(this.RootContext())
        match next.Lexeme with
        | Lexeme.Comment (comment, false) -> Some comment
        | Lexeme.Scalar _ | Lexeme.Alias _ | Lexeme.Tag _ | Lexeme.Anchor _ | Lexeme.BlockEntry | Lexeme.ExplicitKey | Lexeme.Value ->
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
            // [211] l-yaml-stream lets marks, comment lines and explicit documents follow a document without a `...`.
            // A directive belongs to an l-directive-document, which needs the `...`, so a directive behind a mark
            // that ended a document is content behind the mark.
            if docOpen && bomSeen then
                failWith YamlErrorCode.ContentAfterByteOrderMark "A directive cannot follow a byte order mark that ended a document without a `...` in between." tok.Span [ bomSpan ]
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

    /// A run of byte order marks at the start of a line. In front of a document, behind a `...` and at the start
    /// of the stream it belongs to the document prefix ([202]) and the parser ignores it. Behind the content of
    /// a document, [211] l-yaml-stream lets it end the document. The document stays open for the trivia and the
    /// marker that follow, because the model gives the trivia in front of a `---` to the next document and splits
    /// the trivia in front of a `...` or the end of the text among the block collections (rules R3 and R4), and
    /// the mark has no slot in the tree. Content behind the mark is an error.
    member private this.ByteOrderMarkStep(tok: Tok) =
        if hasDirective && not docOpen then
            failWith YamlErrorCode.InvalidCharacter "A byte order mark cannot stand between a directive and the `---` behind it." tok.Span []
        if docOpen && not bomSeen then
            bomSeen <- true
            bomSpan <- tok.Span

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
            if bomSeen then
                failWith YamlErrorCode.ContentAfterByteOrderMark "Only comments, blank lines, byte order marks, `---` and `...` can follow a byte order mark that ended a document." tok.Span [ bomSpan ]
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
                | Lexeme.Value ->
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
        | Lexeme.ByteOrderMark -> this.ByteOrderMarkStep tok
        | Lexeme.DocumentStart -> this.Terminate tok
        | Lexeme.DocumentEnd -> this.Terminate tok
        | Lexeme.StreamEnd -> this.Terminate tok
        | Lexeme.Directive _ -> this.Terminate tok
        | _ -> failWith YamlErrorCode.UnexpectedContent "This text cannot follow the node in front of it." tok.Span []

    member this.Run() : YamlStream =
        while not finished do
            if flowCount > 0 then this.FlowStep()
            elif requested then this.NodeStep()
            else this.LineStep()
        { Parts = List.ofSeq parts }

/// The parser. It reads the whole syntax: block and flow collections, scalars, aliases,
/// properties and the documents of a stream.
module internal Parser =

    /// Parses the text from the cursor on. A cursor that has been used does not start at the beginning.
    let parseCursor (cursor: Cursor) : Result<YamlStream, YamlError> =
        try
            Ok (YamlParser(cursor).Run())
        with ParseFailure error ->
            Error { error with Message = sprintf "Line %d: %s" error.Span.Start.Line error.Message }

    /// Parses a whole text into a stream. The text may start with a byte order mark. `Reader.tryReadDocuments`
    /// is the public entry and calls this.
    let parseStream (text: string) : Result<YamlStream, YamlError> = parseCursor (Cursor(text))
