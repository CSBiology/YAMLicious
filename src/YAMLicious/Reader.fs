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
    | QuotedStringKind.DoubleQuotedString ->
        // Double-quoted scalars need escape processing for the decoded value.
        Escapes.unescapeDoubleQuoted entry.Value

let private restoreBlockScalarPlaceholderValue (entry: StringMapEntry) =
    match entry.Kind with
    | QuotedStringKind.SingleQuotedString ->
        // Inside a block scalar, quote delimiters are literal content and must be preserved.
        "'" + entry.Value + "'"
    | QuotedStringKind.DoubleQuotedString ->
        "\"" + entry.Value + "\""

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

/// Returns the comment id of the first comment line in the elements. A block scalar body is one
/// flat block, so there are no nested elements to search.
let private tryFindCommentLine (eles: PreprocessorElement list) : int option =
    eles
    |> List.tryPick (function
        | YamlComment comment -> Some comment.Comment
        | _ -> None)

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
    
    let restoreInlinePlaceholders (line: string) =
        let withStrings =
            Regex.Replace(
                line,
                StringReplacementPattern,
                (fun (m: Match) ->
                    let idx = m.Groups.["index"].Value |> int
                    restoreBlockScalarPlaceholderValue stringDict.[idx]
                )
            )
        Regex.Replace(
            withStrings,
            CommentPattern,
            (fun (m: Match) ->
                let idx = m.Groups.["comment"].Value |> int
                "#" + commentDict.[idx]
            )
        )

    let rec flattenBlockScalarWithDepth (depth: int) (eles: PreprocessorElement list) : string list =
        eles
        |> List.collect (function
            | Line s ->
                let prefix =
                    if s = "" then ""
                    else System.String(' ', depth * 2)
                [prefix + restoreInlinePlaceholders s]
            | Intendation children ->
                flattenBlockScalarWithDepth (depth + 1) children
            | _ -> [])

    let flattenBlockScalar (eles: PreprocessorElement list) : string list =
        flattenBlockScalarWithDepth 0 eles

    /// Collects the lines of a block without adding indentation. A block scalar body arrives as one
    /// flat block with only the header's indentation removed, so its lines keep their content spaces.
    /// The scanner copies a body as it is and puts no placeholder into it, so a text such as `<s f=0/>`
    /// there is content. The lines stay verbatim and nothing restores a placeholder in them.
    let rec collectContentLines (eles: PreprocessorElement list) : string list =
        eles
        |> List.collect (function
            | Line s -> [s]
            | Intendation children -> collectContentLines children
            | _ -> [])

    let restoreScalarWithStyle (raw: string) =
        match tryParseExactPlaceholderIndex raw with
        | Some idx ->
            let entry = stringDict.[idx]
            let value = restoreScalarPlaceholderValue entry
            let style =
                match entry.Kind with
                | QuotedStringKind.SingleQuotedString -> ScalarStyle.SingleQuoted
                | QuotedStringKind.DoubleQuotedString -> ScalarStyle.DoubleQuoted
            value, Some style
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
            match tryFindCommentLine block with
            | Some lineCommentId ->
                failwithf "A comment line (#%s) ends a block scalar, so the more indented content after it is invalid." commentDict.[lineCommentId]
            | None -> ()
            let lines = collectContentLines block
            // The body is already relative to the header, so the header indentation is 0 here.
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
        | DocumentEnd _ :: _
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
        | DocumentEnd::_ ->
            acc
            |> List.rev
            |> YAMLElement.Object
        | SchemaNamespace v::Intendation yamlAstList::rest0 -> //create/appendSequenceElement
            let objectList = 
                PreprocessorElement.Line v.Key::yamlAstList
            let sequenceElements, rest = splitSequenceItems rest0
            let current =
                YAMLElement.Sequence [
                    loopRead handles objectList []
                    for i in sequenceElements do
                        readSequenceItem i
                ]
            loopRead handles rest (current::acc)
        | SchemaNamespace v::rest0 -> //create/appendSequenceElement
            let sequenceElements, rest = splitSequenceItems rest0
            let current =
                YAMLElement.Sequence [
                    loopRead handles [PreprocessorElement.Line v.Key] []
                    for i in sequenceElements do
                        readSequenceItem i
                ]
            loopRead handles rest (current::acc)
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
        // Explicit key with indented content (complex key), mapped to string for AST compatibility
        | ExplicitKey k::rest -> 
             let parseValue (vStr: string) =
                 let subPrep = Preprocessing.read vStr
                 let subLvl = match subPrep.AST with Level l -> l | _ -> []
                 let result = loopRead handles subLvl []
                 result

             match rest with
             | Intendation keyBody::ExplicitValue v::Intendation iList::tail ->
                let simplifiedKey = flattenBlockScalar keyBody |> String.concat "\n"
                let fullKey = match k with Some s -> s + (if s <> "" then "\n" else "") + simplifiedKey | None -> simplifiedKey
                let keyContent = createScalarContent fullKey None
                
                let separator = if v.TrimStart().StartsWith("[") || v.TrimStart().StartsWith("{") then " " else "\n"
                let fullValue = v + separator + (flattenBlockScalar iList |> String.concat separator)
                let valueElement = parseValue fullValue

                let current =
                    YAMLElement.Mapping (
                        keyContent,
                        valueElement
                    )
                loopRead handles tail (current::acc)
             | Intendation keyBody::ExplicitValue v::tail ->
                let simplifiedKey = flattenBlockScalar keyBody |> String.concat "\n"
                let fullKey = match k with Some s -> s + (if s <> "" then "\n" else "") + simplifiedKey | None -> simplifiedKey
                let keyContent = createScalarContent fullKey None
                
                let valueElement = parseValue v

                let current =
                    YAMLElement.Mapping (
                        keyContent,
                        valueElement
                    )
                loopRead handles tail (current::acc)
             | ExplicitValue v::Intendation iList::tail ->
                let keyContent =
                    match k with
                    | Some s -> createScalarContent s None
                    | None -> YAMLContent.create("")
                
                let fullValue = v + "\n" + (flattenBlockScalar iList |> String.concat "\n")
                let valueElement = parseValue fullValue

                let current =
                    YAMLElement.Mapping (
                        keyContent,
                        valueElement
                    )
                loopRead handles tail (current::acc)
             | ExplicitValue v::tail ->
                let keyContent =
                    match k with
                    | Some s -> createScalarContent s None
                    | None -> YAMLContent.create("")
                
                let valueElement = parseValue v

                let current =
                    YAMLElement.Mapping (
                        keyContent,
                        valueElement
                    )
                loopRead handles tail (current::acc)
             | _ ->
                // Orphan explicit key or unexpected sequence
                let keyContent =
                    match k with
                    | Some s -> createScalarContent s None
                    | None -> YAMLContent.create("")
                let current = 
                    YAMLElement.Mapping (
                        keyContent,
                        YAMLElement.Nil
                    )
                loopRead handles rest (current::acc)
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
            match tryReadBlockScalar v.Value None block with
            | Some blockScalar ->
                let keyContent = createScalarContent v.Key None
                let current =
                    YAMLElement.Mapping(
                        keyContent,
                        YAMLElement.Value(
                            YAMLContent.create(
                                blockScalar.Value,
                                ?comment = blockScalar.Comment,
                                ?anchor = blockScalar.Props.Anchor,
                                ?tag = blockScalar.Props.Tag,
                                style = ScalarStyle.Block(blockScalar.Style, blockScalar.Chomp, blockScalar.Indent)
                            )
                        )
                    )
                loopRead handles rest (current::acc)
            | None ->
                failwithf "Invalid block scalar header: %s" v.Value
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
            match tryReadBlockScalar v.Value v.Comment block with
            | Some blockScalar ->
                let current =
                    YAMLElement.Value(
                        YAMLContent.create(
                            blockScalar.Value,
                            ?comment = blockScalar.Comment,
                            ?anchor = blockScalar.Props.Anchor,
                            ?tag = blockScalar.Props.Tag,
                            style = ScalarStyle.Block(blockScalar.Style, blockScalar.Chomp, blockScalar.Indent)
                        )
                    )
                loopRead handles rest (current::acc)
            | None ->
                failwithf "Invalid block scalar header: %s" v.Value
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

let private flushDocument (documents: ResizeArray<Persil.TokenLine[]>) (current: ResizeArray<Persil.TokenLine>) =
    if current.Count > 0 then
        documents.Add(current.ToArray())
        current.Clear()

/// The comments and blank lines before the `---` of a document are not part of its content. The
/// directives stay, because they apply to the document.
let private keepDirectives (current: ResizeArray<Persil.TokenLine>) =
    let directives = ResizeArray<Persil.TokenLine>()
    for line in current do
        if line.Kind = Persil.LineKind.Directive then
            directives.Add line
    current.Clear()
    current.AddRange directives

/// Splits the lines of one tokenized input at the document markers that the scanner classified.
/// The scanner recognizes a marker only at column 0 and keeps quoted scalars from crossing it,
/// so a marker-like line in a scalar or a block scalar body never shows up as a marker here.
///
/// A `---` ends the current document when that document has content or has already started with its
/// own `---`. Otherwise it starts the document that the lines before it introduce, which then
/// drops its comments. A `---` that only follows directives keeps them for the next document, and
/// a `...` always ends the document.
let private splitDocuments (lines: Persil.TokenLine[]) =
    let documents = ResizeArray<Persil.TokenLine[]>()
    let current = ResizeArray<Persil.TokenLine>()
    let mutable started = false
    let mutable hasContent = false
    let mutable hasDirective = false

    for line in lines do
        match line.Kind with
        | Persil.LineKind.DocumentEnd ->
            flushDocument documents current
            started <- false
            hasContent <- false
            hasDirective <- false
        | Persil.LineKind.DocumentStart ->
            if hasContent || (started && not hasDirective) then
                flushDocument documents current
                hasDirective <- false
            else
                keepDirectives current
            started <- true
            hasContent <- false
            match Persil.tryInlineContent line with
            | Some inlineLine ->
                if inlineLine.Kind <> Persil.LineKind.CommentOnly then
                    current.Add inlineLine
                    hasContent <- true
            | None -> ()
        | Persil.LineKind.Directive ->
            current.Add(Persil.withoutTrailingComment line)
            hasDirective <- true
        | Persil.LineKind.Code
        | Persil.LineKind.BlockHeader ->
            current.Add line
            hasContent <- true
        | Persil.LineKind.BlockBody
        | Persil.LineKind.CommentOnly
        | Persil.LineKind.Blank ->
            current.Add line

    flushDocument documents current
    documents

let readDocuments (yaml: string) : YAMLElement list =
    let tokenized = Persil.tokenize yaml
    splitDocuments tokenized.Lines
    |> Seq.filter (fun document ->
        document |> Array.exists (fun line -> line.Kind <> Persil.LineKind.Blank)
    )
    |> Seq.map (fun document ->
        Persil.prepare tokenized document
        |> Preprocessing.readPrepared
        |> readAst
    )
    |> List.ofSeq
