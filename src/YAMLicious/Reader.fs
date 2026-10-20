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
    | QuotedStringKind.EscapedPlainString -> entry.Value
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

let rec private skipBlankLineElements (eles: PreprocessorElement list) =
    match eles with
    | line :: rest when isBlankLineElement line -> skipBlankLineElements rest
    | _ -> eles

/// `- value` followed by blank lines and then an indented block continues the same item.
let private splitLeadingSequenceItemContinuation (eles: PreprocessorElement list) =
    match eles with
    | line :: _ when isBlankLineElement line ->
        match skipBlankLineElements eles with
        | Intendation yamlAstList :: tail -> Some (yamlAstList, tail)
        | _ -> None
    | _ ->
        None

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
        match splitLeadingSequenceItemContinuation rest with
        | Some (continuation, tail) ->
            [
                [
                    if v.Value.IsSome then
                        PreprocessorElement.Line v.Value.Value
                    yield! continuation
                ]
                yield! collectSequenceElements tail
            ]
        | _ ->
            [
                if v.Value.IsSome then
                    [PreprocessorElement.Line v.Value.Value]
                else
                    []
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
    /// Preprocessing delivered. `tryCollapsePlainScalarContent` matches the nonblank ones against the
    /// segments of a plain scalar.
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
                | QuotedStringKind.EscapedPlainString -> None
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

    let extractProperties (handles: Map<string, string>) (v: string) =
        let mutable current = v.Trim()
        let mutable tag = None
        let mutable anchor = None
        let mutable changed = true
        
        while changed do
            changed <- false
            let mTag = Regex.Match(current, VerbatimTagPattern)
            if mTag.Success then
                tag <- Some mTag.Groups.["tag"].Value
                current <- current.Substring(mTag.Length).Trim()
                changed <- true
            else
                let mShort = Regex.Match(current, "^(!\S*)")
                if mShort.Success then
                    let raw = mShort.Groups.[1].Value
                    tag <- Some (resolveTagShorthand handles raw)
                    current <- current.Substring(mShort.Length).Trim()
                    changed <- true
            
            let mAnchor = Regex.Match(current, AnchorPattern)
            if mAnchor.Success && current.StartsWith("&") then
                anchor <- Some mAnchor.Groups.["anchor"].Value
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

    let tryGetPlainScalarSegments (element: YAMLElement) =
        let isAllowedSegment (allowMetadata: bool) (content: YAMLContent) =
            let hasPlainCompatibleStyle =
                match content.Style with
                | None
                | Some ScalarStyle.Plain -> true
                | _ -> false

            let hasOnlyContinuationContent =
                content.Comment.IsNone
                && content.Anchor.IsNone
                && content.Tag.IsNone

            hasPlainCompatibleStyle
            && (allowMetadata || hasOnlyContinuationContent)

        let rec loop (allowMetadata: bool) (items: YAMLElement list) (acc: YAMLContent list) =
            match items with
            | [] ->
                match List.rev acc with
                | [] -> None
                | segments -> Some segments
            | YAMLElement.Value content :: rest when isAllowedSegment allowMetadata content ->
                loop false rest (content :: acc)
            | _ ->
                None

        match element with
        | YAMLElement.Object items -> loop true items []
        | _ -> None

    let tryCollapsePlainScalarContent (hasInlineFirstLine: bool) (block: PreprocessorElement list) (parsed: YAMLElement) =
        match tryGetPlainScalarSegments parsed with
        | Some (firstSegment :: _ as segments) ->
            let rawBlockLines = collectContentLines block
            let expectedSegmentCount =
                if hasInlineFirstLine then List.length segments - 1 else List.length segments

            let actualNonEmptyBlockLineCount =
                rawBlockLines
                |> List.filter (fun line -> line.Trim() <> "")
                |> List.length

            let rawLines =
                if expectedSegmentCount = actualNonEmptyBlockLineCount then
                    let values =
                        if hasInlineFirstLine then segments |> List.tail else segments
                        |> Queue

                    let renderedBlockLines =
                        rawBlockLines
                        |> List.map (fun rawLine ->
                            if rawLine.Trim() = "" then
                                ""
                            else
                                values.Dequeue().Value
                        )

                    if hasInlineFirstLine then
                        firstSegment.Value :: renderedBlockLines
                    else
                        renderedBlockLines
                else
                    segments |> List.map (fun segment -> segment.Value)

            let rawValue = System.String.Join("\n", rawLines)
            let style =
                if rawValue.Contains("\n") then Some ScalarStyle.Plain else firstSegment.Style

            Some { firstSegment with Value = rawValue; Style = style }
        | _ ->
            None

    let rec takePlainScalarContinuationContents (elements: PreprocessorElement list) (acc: YAMLContent list) =
        match elements with
        | Key _ :: _
        | KeyValue _ :: _
        | SequenceMinusOpener _ :: _
        | YamlComment _ :: _
        | [] ->
            List.rev acc, elements
        | YamlValue v :: rest when v.Value <> "" && v.Comment.IsNone ->
            let content = createScalarContent v.Value None
            match content.Style, content.Comment, content.Anchor, content.Tag with
            | (None | Some ScalarStyle.Plain), None, None, None ->
                takePlainScalarContinuationContents rest (content :: acc)
            | _ ->
                List.rev acc, elements
        | _ ->
            List.rev acc, elements

    let appendPlainScalarContinuations (content: YAMLContent) (continuations: YAMLContent list) =
        match continuations with
        | [] -> content
        | _ ->
            let rawValue =
                content :: continuations
                |> List.map (fun segment -> segment.Value)
                |> String.concat "\n"

            { content with Value = rawValue; Style = Some ScalarStyle.Plain }

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

    let rec loopRead (handles: Map<string, string>) (restlist: PreprocessorElement list) (acc: YAMLElement list) : YAMLElement =
        // Comments between items go into the sequence list as Comment elements.
        let readSequenceItem (element: PreprocessorElement list) =
            match element with
            | [YamlComment comment] -> YAMLElement.Comment commentDict.[comment.Comment]
            | _ -> loopRead handles element []

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
                    let objectList = joinSequenceItemValue v.Value yamlAstList
                    let parsedFirstItem = loopRead handles objectList []
                    match tryCollapsePlainScalarContent v.Value.IsSome yamlAstList parsedFirstItem with
                    | Some content -> YAMLElement.Object [YAMLElement.Value content]
                    | None -> parsedFirstItem
            let sequenceElements, rest = splitSequenceItems rest0
            let current =
                YAMLElement.Sequence [
                    firstItem
                    for i in sequenceElements do
                        readSequenceItem i
                ]
            loopRead handles rest (current::acc)
        | SequenceMinusOpener v::rest0 -> //create/appendSequenceElement
            let initialObjectList =
                if v.Value.IsSome then
                    [PreprocessorElement.Line v.Value.Value]
                else
                    []
            let objectList, sequenceSource =
                match splitLeadingSequenceItemContinuation rest0 with
                | Some (continuation, tail) ->
                    initialObjectList @ continuation, tail
                | None ->
                    initialObjectList, rest0
            let sequenceElements, rest = splitSequenceItems sequenceSource
            let current =
                YAMLElement.Sequence [
                    loopRead handles objectList []
                    for i in sequenceElements do
                        readSequenceItem i
                ]
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
            // A key is one scalar, so the lines of a key that spans several lines become one text. A
            // complex key such as a sequence is flattened to its text the same way.
            let keyContent, afterKey =
                match k, rest with
                | Some header, Intendation body::afterBody when BlockScalar.isHeaderValue header ->
                    readBlockScalarContent header None body, afterBody
                | _, Intendation keyBody::afterBody ->
                    createScalarContent (String.concat "\n" (Option.toList k @ collectContentLines keyBody)) None, afterBody
                | Some s, _ ->
                    createScalarContent s None, rest
                | None, _ ->
                    YAMLContent.create(""), rest

            // The lines behind the colon read as the lines behind a key do. A flow collection that
            // continues on the indented lines joins them into one line first.
            let readExplicitValue (value: string) (block: PreprocessorElement list) =
                if not block.IsEmpty && (value.StartsWith("[") || value.StartsWith("{")) then
                    loopRead handles [PreprocessorElement.Line (String.concat " " (value :: flattenFlowContent block))] []
                else
                    let inlineValue = if value = "" then None else Some value
                    let parsedValue = loopRead handles (joinSequenceItemValue inlineValue block) []
                    match tryCollapsePlainScalarContent inlineValue.IsSome block parsedValue with
                    | Some content -> YAMLElement.Object [YAMLElement.Value content]
                    | None -> parsedValue

            match afterKey with
            | ExplicitValue v::Intendation iList::tail ->
                loopRead handles tail (YAMLElement.Mapping(keyContent, readExplicitValue v iList)::acc)
            | ExplicitValue v::tail ->
                loopRead handles tail (YAMLElement.Mapping(keyContent, readExplicitValue v [])::acc)
            | _ ->
                // Without a value the key maps to nothing.
                loopRead handles afterKey (YAMLElement.Mapping(keyContent, YAMLElement.Nil)::acc)
        | Key v::Intendation yamlAstList::rest -> //createObject
            let c = restoreCommentReplace commentDict v.Comment
            let keyContent = createScalarContent v.Key c
            let parsedValue = loopRead handles yamlAstList []
            let valueElement =
                match tryCollapsePlainScalarContent false yamlAstList parsedValue with
                | Some content -> YAMLElement.Object [YAMLElement.Value content]
                | None -> parsedValue
            let current = 
                YAMLElement.Mapping (
                    keyContent,
                    valueElement
                )
            loopRead handles rest (current::acc)
        | Key v::rest0 ->
            let leadingComments, afterComments = takeLeadingComments rest0
            match afterComments with
            | SequenceMinusOpener w::Intendation yamlAstList::tail ->
                let c = restoreCommentReplace commentDict v.Comment
                let keyContent = createScalarContent v.Key c
                let objectList = joinSequenceItemValue w.Value yamlAstList
                let sequenceElements, rest = splitSequenceItems tail
                let seq =
                    YAMLElement.Sequence [
                        loopRead handles objectList []
                        for element in sequenceElements do
                            readSequenceItem element
                    ]
                let current =
                    YAMLElement.Mapping(
                        keyContent,
                        YAMLElement.Object ((commentTokensToYaml leadingComments) @ [seq])
                    )
                loopRead handles rest (current::acc)
            | SequenceMinusOpener w::tail ->
                let c = restoreCommentReplace commentDict v.Comment
                let keyContent = createScalarContent v.Key c
                let initialObjectList =
                    match w.Value with
                    | Some value -> [PreprocessorElement.Line value]
                    | None -> []
                let objectList, sequenceSource =
                    match splitLeadingSequenceItemContinuation tail with
                    | Some (continuation, remaining) ->
                        initialObjectList @ continuation, remaining
                    | None ->
                        initialObjectList, tail
                let sequenceElements, rest = splitSequenceItems sequenceSource
                let seq =
                    YAMLElement.Sequence [
                        loopRead handles objectList []
                        for element in sequenceElements do
                            readSequenceItem element
                    ]
                let current =
                    YAMLElement.Mapping(
                        keyContent,
                        YAMLElement.Object ((commentTokensToYaml leadingComments) @ [seq])
                    )
                loopRead handles rest (current::acc)
            | _ ->
                let c = restoreCommentReplace commentDict v.Comment
                let current = YAMLElement.Value(YAMLContent.create(v.Key + ":", ?comment = c))
                loopRead handles rest0 (current::acc)
        // doc: |2\n  <block>
        | KeyValue v::Intendation block::rest when BlockScalar.isHeaderValue v.Value ->
            let keyContent = createScalarContent v.Key None
            let current = YAMLElement.Mapping(keyContent, YAMLElement.Value(readBlockScalarContent v.Value None block))
            loopRead handles rest (current::acc)
        | KeyValue v::Intendation block::rest ->
            let keyContent = createScalarContent v.Key None
            let parsedValue = loopRead handles (PreprocessorElement.Line v.Value :: block) []
            let valueElement, restAfterValue =
                match tryCollapsePlainScalarContent true block parsedValue with
                | Some content ->
                    let continuations, restAfterContinuations = takePlainScalarContinuationContents rest []
                    let content = appendPlainScalarContinuations content continuations
                    YAMLElement.Object [YAMLElement.Value content], restAfterContinuations
                | None -> parsedValue, rest
            let current =
                YAMLElement.Mapping(
                    keyContent,
                    valueElement
                )
            loopRead handles restAfterValue (current::acc)
        // My Key: [My Value, Test2]
        | KeyValue v::rest -> // createKeyValue
            let keyContent = createScalarContent v.Key None
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
        | YamlValue v::Intendation block::rest ->
            let parsedValue = loopRead handles (PreprocessorElement.Line v.Value :: block) []
            let current =
                match tryCollapsePlainScalarContent true block parsedValue with
                | Some content ->
                    YAMLElement.Value content
                | None ->
                    match parsedValue with
                    | YAMLElement.Object [single] -> single
                    | _ -> failwithf "Unknown pattern: %A" (PreprocessorElement.Line v.Value :: block)
            loopRead handles rest (current::acc)
        | YamlValue v::rest when v.Value = "" && v.Comment.IsNone ->
            // Ignore structural blank lines outside explicit scalar contexts.
            loopRead handles rest acc
        // My Value <c f=1/>
        | YamlValue v::rest -> // createValue
            let c = restoreCommentReplace commentDict v.Comment
            let props = extractProperties handles v.Value
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
