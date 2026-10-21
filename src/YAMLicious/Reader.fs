module YAMLicious.Reader

open YAMLicious.Regex
open Syntax
open System.Text.RegularExpressions
open YAMLicious.RegexActivePatterns
open YAMLicious.YAMLiciousTypes
open System.Collections.Generic

let private restoreScalarPlaceholderValue (entry: StringMapEntry) =
    match entry.Kind with
    | QuotedStringKind.SingleQuotedString -> entry.Value
    // A separate case, because Fable Python loses the first case of a merged pattern over three cases.
    | QuotedStringKind.PlainScalar -> entry.Value
    | QuotedStringKind.DoubleQuotedString ->
        // Double-quoted scalars need escape processing for the decoded value.
        Escapes.unescapeDoubleQuoted entry.Value

let private tryParseExactPlaceholderIndex (v: string) =
    let m = Regex.Match(v.Trim(), $"^{StringReplacementPattern}$")
    if m.Success then Some (int m.Groups.["index"].Value) else None

let private restoreStringReplace (stringDict: Dictionary<int, StringMapEntry>) (v: string)  =
    System.Text.RegularExpressions.Regex.Replace(v, StringReplacementPattern, fun m ->
        let index = m.Groups.["index"].Value |> int
        restoreScalarPlaceholderValue stringDict.[index]
    )


let private restoreCommentReplace (commentDict: Dictionary<int, string>) (commentId: int option) =
    commentId |> Option.map (fun id -> commentDict.[id])

let private isBlankLineElement = function
    | PreprocessorElement.Line line when line.Trim() = "" -> true
    | _ -> false

/// Puts the value written after a sequence dash in front of the block that follows it. A block
/// scalar header keeps that block as one Intendation because the body is a single unit.
/// Flattening it would turn each body line into a node of its own.
let private joinSequenceItemValue (value: string option) (block: PreprocessorElement list) =
    match value with
    | Some header when BlockScalar.isHeaderValue header ->
        [PreprocessorElement.Line header; PreprocessorElement.Intendation block]
    | Some v ->
        PreprocessorElement.Line v :: block
    | None ->
        block

let rec collectSequenceElements (eles: PreprocessorElement list) : PreprocessorElement list list =
    match eles with
    | line::rest when isBlankLineElement line ->
        collectSequenceElements rest
    | SequenceMinusOpener v::Intendation yamlAstList::rest ->
        [
            joinSequenceItemValue v.Value yamlAstList
            yield! collectSequenceElements rest            
        ]
    | SequenceMinusOpener v::rest ->
        [
            match v.Value with
            | Some comment when Placeholder.isCommentOnlyLine comment ->
                // A dash with nothing but a comment is an empty item, and the comment stays behind it.
                yield []
                yield [PreprocessorElement.Line comment]
            | Some value ->
                yield [PreprocessorElement.Line value]
            | None ->
                yield []
            yield! collectSequenceElements rest
        ]
    | YamlComment _ as v::rest ->
        [
            [v]
            yield! collectSequenceElements rest
        ]
    | [] ->
        []
    | anyElse -> failwithf "Unknown pattern for sequence elements: %A" anyElse
    
let isSequenceElement = fun e -> match e with | Intendation _ | SequenceMinusOpener _ | YamlComment _ -> true | _ when isBlankLineElement e -> true | _ -> false

let private isCommentOrBlankElement e =
    match e with
    | YamlComment _ -> true
    | _ -> isBlankLineElement e

/// Splits the items of a block sequence from the elements that follow it. Comments after the
/// last item go to the next node in the same block. At the end of a block they stay in the
/// sequence as trailing entries.
let private splitSequenceItems (eles: PreprocessorElement list) =
    let run = eles |> List.takeWhile isSequenceElement
    let following = eles |> List.skip run.Length
    if following.IsEmpty then
        collectSequenceElements run, []
    else
        let trailing = run |> List.rev |> List.takeWhile isCommentOrBlankElement |> List.rev
        let items = run |> List.take (run.Length - trailing.Length)
        collectSequenceElements items, trailing @ following

let private tokenize (yamlList: PreprocessorElement list) (stringDict: Dictionary<int, StringMapEntry>) (commentDict: Dictionary<int, string>) (handles: Map<string, string>) =
    let rec flattenFlowContent (elements: PreprocessorElement list) : string list =
        elements
        |> List.collect (function
            | PreprocessorElement.Line s -> [s.Trim()]
            | PreprocessorElement.Intendation children -> flattenFlowContent children
            | _ -> [])
    
    /// The lines of a block in order, nested blocks included. Each line keeps the text that
    /// Preprocessing delivered.
    let rec collectContentLines (eles: PreprocessorElement list) : string list =
        eles
        |> List.collect (function
            | Line s -> [s]
            | Intendation children -> collectContentLines children
            | _ -> [])

    /// The lines of a block scalar body. Preprocessing delivers the body as one flat block, with only
    /// the indentation of the node that owns the scalar removed and no placeholder restored, because
    /// the scanner copies a body as it is. A text such as `<c f=0/>` in a body is content, so the lines
    /// stay verbatim here too.
    let blockScalarLines (body: PreprocessorElement list) : string list =
        body
        |> List.choose (function
            | Line s -> Some s
            | _ -> None)

    let restoreScalarWithStyle (raw: string) =
        match tryParseExactPlaceholderIndex raw with
        | Some idx ->
            let entry = stringDict.[idx]
            let value = restoreScalarPlaceholderValue entry
            let style =
                match entry.Kind with
                | QuotedStringKind.SingleQuotedString -> Some ScalarStyle.SingleQuoted
                | QuotedStringKind.DoubleQuotedString -> Some ScalarStyle.DoubleQuoted
                | QuotedStringKind.PlainScalar -> if entry.Value.Contains("\n") then Some ScalarStyle.Plain else None
            value, style
        | None ->
            let value = restoreStringReplace stringDict raw
            // Keep plain scalars style-neutral for backward compatibility.
            value, None

    let resolveTagShorthand (handles: Map<string, string>) (shorthand: string) : string =
        if shorthand = "!" then "!"
        else
            let m = Regex.Match(shorthand, "^(![\w-]+!|!!|!)(.*)")
            if m.Success then
                let handle = m.Groups.[1].Value
                let suffix = m.Groups.[2].Value
                match Map.tryFind handle handles with
                | Some prefix -> prefix + suffix
                | None -> shorthand
            else shorthand

    /// A node has at most one tag and one anchor, whether they stand on one line or on the lines in
    /// front of the node.
    let singleProperty (own: string option) (other: string option) (name: string) =
        match own, other with
        | Some _, Some _ -> failwithf "A node cannot have two %ss." name
        | Some _, None -> own
        | None, _ -> other

    let extractProperties (handles: Map<string, string>) (v: string) =
        let mutable current = v.Trim()
        let mutable tag = None
        let mutable anchor = None
        let mutable changed = true
        
        while changed do
            changed <- false
            let mTag = Regex.Match(current, VerbatimTagPattern)
            if mTag.Success then
                tag <- singleProperty tag (Some mTag.Groups.["tag"].Value) "tag"
                current <- current.Substring(mTag.Length).Trim()
                changed <- true
            else
                let mShort = Regex.Match(current, "^(!\S*)")
                if mShort.Success then
                    let raw = mShort.Groups.[1].Value
                    tag <- singleProperty tag (Some (resolveTagShorthand handles raw)) "tag"
                    current <- current.Substring(mShort.Length).Trim()
                    changed <- true
            
            let mAnchor = Regex.Match(current, AnchorPattern)
            if mAnchor.Success && current.StartsWith("&") then
                anchor <- singleProperty anchor (Some mAnchor.Groups.["anchor"].Value) "anchor"
                current <- current.Substring(mAnchor.Length).Trim()
                changed <- true

        {| Value = current; Tag = tag; Anchor = anchor |}

    let createScalarContent (raw: string) (comment: string option) =
        let props = extractProperties handles raw
        let finalValue, finalStyle = restoreScalarWithStyle props.Value
        YAMLContent.create(finalValue, ?comment = comment, ?anchor = props.Anchor, ?tag = props.Tag, ?style = finalStyle)

    let tryReadBlockScalar (rawHeader: string) (baseCommentId: int option) (block: PreprocessorElement list) =
        let headerWithoutComment, headerCommentId = Placeholder.splitTrailingComment rawHeader
        let props = extractProperties handles headerWithoutComment
        let commentId =
            match baseCommentId with
            | Some _ -> baseCommentId
            | None -> headerCommentId

        match Syntax.BlockScalar.parseHeader props.Value with
        | Some header ->
            let lines = blockScalarLines block
            // Preprocessing removed the indentation of the owning node from the body already, so the
            // indentation that the body is measured from is 0 here.
            let value = Syntax.BlockScalar.buildContent header.Style header.Chomp 0 header.Indent lines
            let comment = restoreCommentReplace commentDict commentId
            Some
                {| Props = props
                   Comment = comment
                   Style = header.Style
                   Chomp = header.Chomp
                   Indent = header.Indent
                   Value = value |}
        | None ->
            None

    /// The content of a block scalar, wherever its header stands: after a key, a sequence dash, an
    /// explicit key mark or at the root.
    let readBlockScalarContent (rawHeader: string) (baseCommentId: int option) (block: PreprocessorElement list) =
        match tryReadBlockScalar rawHeader baseCommentId block with
        | Some blockScalar ->
            YAMLContent.create(
                blockScalar.Value,
                ?comment = blockScalar.Comment,
                ?anchor = blockScalar.Props.Anchor,
                ?tag = blockScalar.Props.Tag,
                style = ScalarStyle.Block(blockScalar.Style, blockScalar.Chomp, blockScalar.Indent)
            )
        | None ->
            failwithf "Invalid block scalar header: %s" rawHeader

    let rec takeLeadingComments (elements: PreprocessorElement list) =
        match elements with
        | line :: rest when isBlankLineElement line ->
            takeLeadingComments rest
        | (YamlComment _ as commentElement) :: rest ->
            let comments, tail = takeLeadingComments rest
            commentElement :: comments, tail
        | _ ->
            [], elements

    let commentTokensToYaml (comments: PreprocessorElement list) =
        comments
        |> List.map (function
            | YamlComment comment -> YAMLElement.Comment(commentDict.[comment.Comment])
            | anyElse -> failwithf "Expected leading comment token, got: %A" anyElse
        )

    let rec parseFlowNode (tokens: FlowTokens.Token list) : YAMLElement * FlowTokens.Token list =
        match tokens with
        | FlowTokens.Token.OpenBrace :: rest -> parseFlowObject rest
        | FlowTokens.Token.OpenBracket :: rest -> parseFlowArray rest
        | FlowTokens.Token.String _ :: _ -> parseFlowScalar tokens []
        | FlowTokens.Token.Colon :: _ -> parseFlowScalar tokens []
        | FlowTokens.Token.EOF :: _ -> YAMLElement.Nil, tokens
        | _ -> failwithf "Unexpected flow token: %A" tokens

    and tryCommentElementFromFlowString (value: string) =
        Placeholder.tryParseComment value
        |> Option.map (fun id -> YAMLElement.Comment(commentDict.[id]))

    and parseFlowScalar (tokens: FlowTokens.Token list) (acc: string list) : YAMLElement * FlowTokens.Token list =
        match tokens with
        | FlowTokens.Token.String value :: rest when tryCommentElementFromFlowString value |> Option.isSome ->
            match List.rev acc with
            | [] ->
                (tryCommentElementFromFlowString value |> Option.get), rest
            | parts ->
                YAMLElement.Value(createScalarContent (String.concat "" parts) None), tokens
        | FlowTokens.Token.String value :: rest ->
            parseFlowScalar rest (value :: acc)
        | FlowTokens.Token.Colon :: rest ->
            parseFlowScalar rest (":" :: acc)
        | _ ->
            match List.rev acc with
            | [] -> YAMLElement.Nil, tokens
            | parts -> YAMLElement.Value(createScalarContent (String.concat "" parts) None), tokens

    and parseFlowObject (tokens: FlowTokens.Token list) : YAMLElement * FlowTokens.Token list =
        let rec loop remaining acc =
            match remaining with
            | FlowTokens.Token.CloseBrace :: rest ->
                YAMLElement.Object(List.rev acc), rest
            | FlowTokens.Token.Comma :: rest ->
                loop rest acc
            | FlowTokens.Token.EOF :: _ ->
                YAMLElement.Object(List.rev acc), remaining
            | FlowTokens.Token.String value :: rest when tryCommentElementFromFlowString value |> Option.isSome ->
                loop rest ((tryCommentElementFromFlowString value |> Option.get) :: acc)
            | FlowTokens.Token.String key :: FlowTokens.Token.Colon :: rest ->
                let keyContent = createScalarContent key None
                let value, afterValue = parseFlowNode rest
                let valueElement =
                    match value with
                    | YAMLElement.Object _ -> value
                    | YAMLElement.Sequence _
                    | YAMLElement.Value _ -> YAMLElement.Object [value]
                    | _ -> value
                loop afterValue (YAMLElement.Mapping(keyContent, valueElement) :: acc)
            | _ ->
                failwithf "Expected flow mapping key or close brace, got: %A" remaining

        loop tokens []

    and parseFlowArray (tokens: FlowTokens.Token list) : YAMLElement * FlowTokens.Token list =
        let rec loop remaining acc =
            match remaining with
            | FlowTokens.Token.CloseBracket :: rest ->
                YAMLElement.Sequence(List.rev acc), rest
            | FlowTokens.Token.Comma :: rest ->
                loop rest acc
            | FlowTokens.Token.EOF :: _ ->
                YAMLElement.Sequence(List.rev acc), remaining
            | FlowTokens.Token.String value :: rest when tryCommentElementFromFlowString value |> Option.isSome ->
                let comment = tryCommentElementFromFlowString value |> Option.get
                loop rest (comment :: acc)
            | _ ->
                let item, afterItem = parseFlowNode remaining
                let sequenceItem =
                    match item with
                    | YAMLElement.Value _
                    | YAMLElement.Sequence _ -> YAMLElement.Object [item]
                    | _ -> item
                loop afterItem (sequenceItem :: acc)

        loop tokens []

    let parseFlowSource (source: string) =
        let tokens = Syntax.FlowTokens.tokenize source
        let node, _ = parseFlowNode tokens
        node

    let tryFlowOpeningValue (raw: string) =
        let withoutComment, commentId = Placeholder.splitTrailingComment raw
        match withoutComment.Trim() with
        | "[" -> Some ("[", "]", commentId)
        | "{" -> Some ("{", "}", commentId)
        | _ -> None

    let tryNormalizeClosingLine (closing: string) (lines: string list) =
        let rec loop (suffix: string list) (remainingReversed: string list) =
            match remainingReversed with
            | [] -> None
            | line :: rest when line.Trim() = "" ->
                loop (line :: suffix) rest
            | line :: rest ->
                let withoutComment, commentId = Placeholder.splitTrailingComment line
                if withoutComment.Trim() = closing then
                    Some (List.rev rest @ [closing] @ suffix, commentId)
                else
                    None

        loop [] (List.rev lines)

    let tryParseMultilineFlowFromSequenceItem (rawOpener: string) (children: PreprocessorElement list) =
        match tryFlowOpeningValue rawOpener with
        | Some (opening, closing, openerCommentId) ->
            let flattened = flattenFlowContent children
            match tryNormalizeClosingLine closing flattened with
            | Some (flowLines, closerCommentId) ->
                let source = String.concat "\n" (opening :: flowLines)
                Some (parseFlowSource source, openerCommentId, closerCommentId)
            | None ->
                None
        | None ->
            None

    let wrapFlowSequenceItem openerCommentId closerCommentId node =
        let commentsBefore =
            match openerCommentId |> restoreCommentReplace commentDict with
            | Some c -> [YAMLElement.Comment c]
            | None -> []

        let commentsAfter =
            match closerCommentId |> restoreCommentReplace commentDict with
            | Some c -> [YAMLElement.Comment c]
            | None -> []

        match node with
        | YAMLElement.Object items -> YAMLElement.Object (commentsBefore @ items @ commentsAfter)
        | YAMLElement.Nil -> YAMLElement.Object (commentsBefore @ commentsAfter)
        | other -> YAMLElement.Object (commentsBefore @ [other] @ commentsAfter)

    let prependRootFlow (comment: string option) (node: YAMLElement) (acc: YAMLElement list) =
        let elements =
            [
                match comment with
                | Some c -> yield YAMLElement.Comment c
                | None -> ()

                match node with
                | YAMLElement.Object items -> yield! items
                | YAMLElement.Nil -> ()
                | other -> yield other
            ]

        (List.rev elements) @ acc

    let mapTag = "tag:yaml.org,2002:map"
    let seqTag = "tag:yaml.org,2002:seq"

    /// A mapping or a sequence has no field for properties. The core tag of the collection (`!!map` on a
    /// mapping, `!!seq` on a sequence) only says what the collection is, so dropping it loses nothing. Any
    /// other tag and every anchor would be lost, so they raise an error.
    let rejectCollectionProperties (props: {| Value: string; Tag: string option; Anchor: string option |}) (coreTag: string option) =
        let keptTag = props.Tag |> Option.filter (fun tag -> Some tag <> coreTag)
        if keptTag.IsSome || props.Anchor.IsSome then
            let lost = (Option.toList keptTag @ (props.Anchor |> Option.map (fun anchor -> "&" + anchor) |> Option.toList)) |> String.concat " "
            failwithf "YAMLicious does not keep properties on collections, so %s on a mapping or a sequence would be lost." lost

    /// The elements that a node gets when a line of properties stands in front of it. A scalar takes the
    /// tag and the anchor, and the comment of the line goes in front of it. Nothing behind the line gives an
    /// empty scalar that carries them. A block mapping or sequence keeps only the comment, and
    /// `rejectCollectionProperties` decides which properties it may drop.
    let attachProperties (props: {| Value: string; Tag: string option; Anchor: string option |}) (comment: string option) (node: YAMLElement) =
        let items = match node with YAMLElement.Object items -> items | other -> [other]
        let leading = comment |> Option.map YAMLElement.Comment |> Option.toList
        let isValue = function YAMLElement.Value _ -> true | _ -> false
        let isComment = function YAMLElement.Comment _ -> true | _ -> false
        match items |> List.filter (isComment >> not) with
        | [] ->
            items @ [ YAMLElement.Value(YAMLContent.create("", ?comment = comment, ?anchor = props.Anchor, ?tag = props.Tag)) ]
        | [ item ] when isValue item ->
            leading @ (items |> List.map (function
                | YAMLElement.Value content ->
                    YAMLElement.Value { content with Tag = singleProperty content.Tag props.Tag "tag"; Anchor = singleProperty content.Anchor props.Anchor "anchor" }
                | other -> other))
        | collection ->
            let coreTag =
                collection |> List.tryPick (function
                    | YAMLElement.Mapping _ -> Some mapTag
                    | YAMLElement.Sequence _ -> Some seqTag
                    | _ -> None)
            rejectCollectionProperties props coreTag
            leading @ items

    /// The source that the flow parser reads and the core tag for a flow collection that fits on one line.
    let tryOneLineFlowCollection (value: string) =
        match PreprocessorElement.Line value with
        | InlineSequence flow -> Some ("[" + flow.Value + "]", seqTag)
        | InlineJSON flow -> Some ("{" + flow.Value + "}", mapTag)
        | _ -> None

    /// The properties and the comment id of a value that holds properties and nothing else, as `!!seq` does
    /// in `k: !!seq`. Every property starts with `!` or `&`, which spares the other values the property scan.
    let tryPropertiesOnly (value: string) =
        if value.StartsWith("!") || value.StartsWith("&") then
            let text, commentId = Placeholder.splitTrailingComment value
            let props = extractProperties handles text
            if props.Value = "" then Some (props, commentId) else None
        else
            None

    /// A plain scalar cannot start with `[`, `]`, `{`, `}` or `,`, so a value that does belongs to a flow
    /// collection. The layouts of a flow collection that the Reader supports are matched before a value gets
    /// here. What is left spans several lines in a layout without a pattern, and reading its lines one by one
    /// would give a wrong value. A collection that fits on one line behind properties is not checked here.
    let failOnFlowFragment (value: string) =
        if value <> "" then
            match value.[0] with
            | '[' | ']' | '{' | '}' | ',' ->
                match PreprocessorElement.Line value with
                | InlineSequence _
                | InlineJSON _ -> ()
                | _ ->
                    failwithf "A flow collection over several lines in this layout is not supported yet: %s" (restoreStringReplace stringDict value)
            | _ -> ()

    /// A comment between the items goes into the sequence list as a Comment element. An empty item is Nil.
    /// `read` reads the lines of any other item. It is a parameter so that `loopRead` stays the one function
    /// of its own recursion, which Fable turns into a loop for the long runs of comments and keys.
    let readSequenceItem (read: PreprocessorElement list -> YAMLElement) (element: PreprocessorElement list) =
        match element with
        | [] -> YAMLElement.Nil
        | [YamlComment comment] -> YAMLElement.Comment commentDict.[comment.Comment]
        | _ -> read element

    /// Reads the block sequence that starts at the first element and returns it with the elements behind it.
    let readBlockSequence (read: PreprocessorElement list -> YAMLElement) (elements: PreprocessorElement list) =
        let items, rest = splitSequenceItems elements
        YAMLElement.Sequence (items |> List.map (readSequenceItem read)), rest

    let rec loopRead (handles: Map<string, string>) (restlist: PreprocessorElement list) (acc: YAMLElement list) : YAMLElement =
        match restlist with
        | AliasNode alias::rest ->
            loopRead handles rest (YAMLElement.Alias alias::acc)
        // Example1: 
        // - My Value 1 <c f=1/>
        //   My Value 2
        // - My Value 3
        // Example2:
        // -
        //   My Key1: My Value1
        //   My Key2: My Value2
        //   My Key3: My Value3
        | SequenceMinusOpener v::Intendation yamlAstList::rest0 -> //create/appendSequenceElement
            let firstItem =
                match v.Value |> Option.bind (fun value -> tryParseMultilineFlowFromSequenceItem value yamlAstList) with
                | Some (node, openerCommentId, closerCommentId) ->
                    wrapFlowSequenceItem openerCommentId closerCommentId node
                | None ->
                    loopRead handles (joinSequenceItemValue v.Value yamlAstList) []
            let sequenceElements, rest = splitSequenceItems rest0
            let current =
                YAMLElement.Sequence [
                    firstItem
                    for i in sequenceElements do
                        readSequenceItem (fun item -> loopRead handles item []) i
                ]
            loopRead handles rest (current::acc)
        | SequenceMinusOpener _::_ -> //create/appendSequenceElement
            let current, rest = readBlockSequence (fun item -> loopRead handles item []) restlist
            loopRead handles rest (current::acc)
        // [test1, test2, test] <c f=1/>
        | InlineSequence v::rest -> // create sequence
            let c = restoreCommentReplace commentDict v.Comment
            let current = parseFlowSource ("[" + v.Value + "]")
            loopRead handles rest (prependRootFlow c current acc)
        // [ #c1
        //   v1,
        //   v2,
        //   v3
        // ] #c2
        | SequenceSquareOpener opener::Intendation iList::SequenceSquareCloser closer::rest ->
            let c1 = opener.Comment |> restoreCommentReplace commentDict
            let c2 = closer.Comment |> restoreCommentReplace commentDict
            let items = flattenFlowContent iList |> String.concat "\n"
            let current = parseFlowSource ("[" + items + "]")
            let nextAcc =
                match c1, c2 with
                | Some c1, Some c2 -> 
                    YAMLElement.Comment c2::current::YAMLElement.Comment c1::acc
                | Some c1, None ->
                    current::YAMLElement.Comment c1::acc
                | None, Some c2 ->
                    YAMLElement.Comment c2::current::acc
                | None, None ->
                    current::acc
            loopRead handles rest nextAcc
        | InlineJSON v::rest -> // create object
            let c = restoreCommentReplace commentDict v.Comment
            let current = parseFlowSource ("{" + v.Value + "}")
            loopRead handles rest (prependRootFlow c current acc)
        | JSONKeyOpener opener::Intendation iList::JSONCloser closer::rest ->
            let keyComment = opener.Comment |> restoreCommentReplace commentDict
            let closerComment = closer.Comment |> restoreCommentReplace commentDict
            let keyContent = createScalarContent opener.Key keyComment
            let jsonContent = flattenFlowContent iList |> String.concat "\n"
            let fullJson = "{" + jsonContent + "}"
            let valueElement = parseFlowSource fullJson
            let current = YAMLElement.Mapping(keyContent, valueElement)
            let nextAcc =
                match closerComment with
                | Some c -> YAMLElement.Comment c::current::acc
                | None -> current::acc
            loopRead handles rest nextAcc
        | YamlValue opener::Intendation iList::JSONCloser closer::rest when opener.Value = "{" ->
            let c1 = opener.Comment |> restoreCommentReplace commentDict
            let c2 = closer.Comment |> restoreCommentReplace commentDict
            let items = flattenFlowContent iList |> String.concat "\n"
            let current = parseFlowSource ("{" + items + "}")
            let elements =
                [
                    match c1 with
                    | Some c -> yield YAMLElement.Comment c
                    | None -> ()

                    match current with
                    | YAMLElement.Object items -> yield! items
                    | YAMLElement.Nil -> ()
                    | other -> yield other

                    match c2 with
                    | Some c -> yield YAMLElement.Comment c
                    | None -> ()
                ]
            loopRead handles rest ((List.rev elements) @ acc)
        // ? key
        // : value
        | ExplicitKey k::rest ->
            // The comment behind the `?` belongs to the key and is not part of its text.
            let keyText, markCommentId =
                match k with
                | Some s -> Placeholder.splitTrailingComment s
                | None -> "", None

            // A key is one scalar, so the lines of a key that spans several lines become one text. A
            // complex key such as a sequence is flattened to its text the same way. A comment behind the
            // last line of such a key goes to the key, and a comment behind the `?` then stands in front
            // of the entry.
            let keyContent, commentsBefore, afterKey =
                match rest with
                | Intendation body::afterBody when BlockScalar.isHeaderValue keyText ->
                    readBlockScalarContent keyText markCommentId body, [], afterBody
                | Intendation keyBody::afterBody ->
                    let lines = (if keyText = "" then [] else [keyText]) @ collectContentLines keyBody
                    let text, lastLineCommentId = Placeholder.splitTrailingComment (String.concat "\n" lines)
                    let keyCommentId, beforeCommentId =
                        match lastLineCommentId with
                        | Some _ -> lastLineCommentId, markCommentId
                        | None -> markCommentId, None
                    let before = restoreCommentReplace commentDict beforeCommentId |> Option.map YAMLElement.Comment |> Option.toList
                    createScalarContent text (restoreCommentReplace commentDict keyCommentId), before, afterBody
                | _ ->
                    createScalarContent keyText (restoreCommentReplace commentDict markCommentId), [], rest
            let acc = commentsBefore @ acc

            // The lines behind the colon read as the lines behind a key do. A flow collection that
            // continues on the indented lines joins them into one line first. A colon with nothing behind
            // it, or with a comment alone, gives an empty value, and the comment stays behind the entry.
            let readExplicitValue (value: string) (block: PreprocessorElement list) =
                if not block.IsEmpty && (value.StartsWith("[") || value.StartsWith("{")) then
                    loopRead handles [PreprocessorElement.Line (String.concat " " (value :: flattenFlowContent block))] [], []
                else
                    let inlineValue = if value = "" then None else Some value
                    match joinSequenceItemValue inlineValue block with
                    | [] -> YAMLElement.Nil, []
                    | [YamlComment comment] -> YAMLElement.Nil, [YAMLElement.Comment commentDict.[comment.Comment]]
                    | lines -> loopRead handles lines [], []

            match afterKey with
            | ExplicitValue v::Intendation iList::tail ->
                let value, commentsAfter = readExplicitValue v iList
                loopRead handles tail (commentsAfter @ YAMLElement.Mapping(keyContent, value)::acc)
            | ExplicitValue v::tail ->
                let value, commentsAfter = readExplicitValue v []
                loopRead handles tail (commentsAfter @ YAMLElement.Mapping(keyContent, value)::acc)
            | _ ->
                // Without a value the key maps to nothing.
                loopRead handles afterKey (YAMLElement.Mapping(keyContent, YAMLElement.Nil)::acc)
        | Key v::Intendation yamlAstList::rest -> //createObject
            let c = restoreCommentReplace commentDict v.Comment
            let keyContent = createScalarContent v.Key c
            let current = YAMLElement.Mapping (keyContent, loopRead handles yamlAstList [])
            loopRead handles rest (current::acc)
        | Key v::rest0 ->
            let c = restoreCommentReplace commentDict v.Comment
            let keyContent = createScalarContent v.Key c
            let leadingComments, afterComments = takeLeadingComments rest0
            match afterComments with
            // The items of a sequence stand at the indentation of the key that owns it.
            | SequenceMinusOpener _::_ ->
                let sequence, rest = readBlockSequence (fun item -> loopRead handles item []) afterComments
                let current =
                    YAMLElement.Mapping(
                        keyContent,
                        YAMLElement.Object ((commentTokensToYaml leadingComments) @ [sequence])
                    )
                loopRead handles rest (current::acc)
            | _ ->
                // A key with no value and no indented block maps to nothing, like an explicit key without `:`.
                let current = YAMLElement.Mapping(keyContent, YAMLElement.Nil)
                loopRead handles rest0 (current::acc)
        // doc: |2\n  <block>
        | KeyValue v::Intendation block::rest when BlockScalar.isHeaderValue v.Value ->
            let keyContent = createScalarContent v.Key None
            let current = YAMLElement.Mapping(keyContent, YAMLElement.Value(readBlockScalarContent v.Value None block))
            loopRead handles rest (current::acc)
        | KeyValue v::Intendation block::rest ->
            let keyContent = createScalarContent v.Key None
            let current = YAMLElement.Mapping(keyContent, loopRead handles (PreprocessorElement.Line v.Value :: block) [])
            loopRead handles rest (current::acc)
        // My Key: [My Value, Test2]
        | KeyValue v::rest -> // createKeyValue
            let keyContent = createScalarContent v.Key None
            // Properties alone behind the key leave the node to the lines that follow. When a sequence follows
            // at the indentation of the key, as in `k: !!seq` and then `- entry`, the key owns that sequence.
            let ownedSequence =
                match tryPropertiesOnly v.Value with
                | Some (props, commentId) ->
                    match takeLeadingComments rest with
                    | leadingComments, (SequenceMinusOpener _::_ as sequenceSource) -> Some (props, commentId, leadingComments, sequenceSource)
                    | _ -> None
                | None -> None
            match ownedSequence with
            | Some (props, commentId, leadingComments, sequenceSource) ->
                let sequence, afterSequence = readBlockSequence (fun item -> loopRead handles item []) sequenceSource
                let node = YAMLElement.Object ((commentTokensToYaml leadingComments) @ [sequence])
                let value = YAMLElement.Object (attachProperties props (restoreCommentReplace commentDict commentId) node)
                loopRead handles afterSequence (YAMLElement.Mapping(keyContent, value)::acc)
            | None ->
                let current =
                    YAMLElement.Mapping (
                        keyContent,
                        //reuse default parsing into SequenceElements
                        loopRead handles [PreprocessorElement.Line v.Value] []
                    )
                loopRead handles rest (current::acc)
        // <c f=1/>
        | YamlComment v::rest -> // createComment
            let c = commentDict.[v.Comment]
            let current = 
                YAMLElement.Comment (c)
            loopRead handles rest (current::acc)
        // Root-level block scalar
        | YamlValue v::Intendation block::rest when BlockScalar.isHeaderValue v.Value ->
            let current = YAMLElement.Value(readBlockScalarContent v.Value v.Comment block)
            loopRead handles rest (current::acc)
        | YamlValue v::rest when v.Value = "" && v.Comment.IsNone ->
            // Ignore structural blank lines outside explicit scalar contexts.
            loopRead handles rest acc
        // My Value <c f=1/>
        | YamlValue v::rest -> // createValue
            let c = restoreCommentReplace commentDict v.Comment
            let props = extractProperties handles v.Value
            if props.Value = "" && (props.Tag.IsSome || props.Anchor.IsSome) then
                // The line holds only properties, so the node behind it owns them. That node is the block
                // nested behind the line or the rest of the block that the line belongs to.
                let node, afterNode =
                    match rest with
                    | Intendation block::tail -> loopRead handles block [], tail
                    | _ -> loopRead handles rest [], []
                loopRead handles afterNode (List.rev (attachProperties props c node) @ acc)
            else
                match tryOneLineFlowCollection props.Value with
                | Some (source, coreTag) ->
                    // A flow collection behind properties reads as a collection.
                    rejectCollectionProperties props (Some coreTag)
                    loopRead handles rest (prependRootFlow c (parseFlowSource source) acc)
                | None ->
                    failOnFlowFragment props.Value
                    let finalValue, finalStyle = restoreScalarWithStyle props.Value
                    let current =
                        YAMLElement.Value (
                            YAMLContent.create(finalValue, ?comment=c, ?anchor=props.Anchor, ?tag=props.Tag, ?style=finalStyle)
                        )
                    loopRead handles rest (current::acc)
        | [] ->
            acc
            |> List.rev
            |> YAMLElement.Object
        | anyElse -> failwithf "Unknown pattern: %A" anyElse
    loopRead handles yamlList []

let private readAst (ast: Preprocessor) =
    match ast.AST with
    | Level lvl ->
        tokenize lvl ast.StringMap ast.CommentMap ast.TagHandles
    | _ -> failwith "Not a root!"

let read (yaml: string) =
    readAst (Preprocessing.read yaml)

let readDocuments (yaml: string) : YAMLElement list =
    Persil.tokenize yaml
    |> Persil.prepareDocuments
    |> List.map (Preprocessing.readPrepared >> readAst)
