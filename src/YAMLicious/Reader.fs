module YAMLicious.Reader

open YAMLicious.Regex
open YAMLicious.Preprocessing
open YAMLicious.FlowToBlock
open YAMLicious.Escapes
open System.Text.RegularExpressions
open YAMLicious.RegexActivePatterns
open YAMLicious.YAMLiciousTypes
open System.Collections.Generic

let private restoreScalarPlaceholderValue (entry: StringMapEntry) =
    match entry.Kind with
    | QuotedStringKind.SingleQuotedString -> entry.Value
    | QuotedStringKind.DoubleQuotedString ->
        // Double-quoted scalars need escape processing for the decoded value.
        YAMLicious.Escapes.unescapeDoubleQuoted entry.Value

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

let private parseBlockScalarHeader (header: string) =
    let h = header.Trim()
    if System.String.IsNullOrWhiteSpace h then None
    else
        let styleOpt =
            match h.[0] with
            | '|' -> Some BlockScalarStyle.Literal
            | '>' -> Some BlockScalarStyle.Folded
            | _ -> None

        match styleOpt with
        | None -> None
        | Some style ->
            let mutable indent: int option = None
            let mutable chomp = ChompingMode.Clip
            let mutable isValid = true

            for i in 1 .. h.Length - 1 do
                match h.[i] with
                | c when c >= '1' && c <= '9' ->
                    if indent.IsSome then isValid <- false
                    else indent <- Some (int (string c))
                | '-' ->
                    if chomp <> ChompingMode.Clip then isValid <- false
                    else chomp <- ChompingMode.Strip
                | '+' ->
                    if chomp <> ChompingMode.Clip then isValid <- false
                    else chomp <- ChompingMode.Keep
                | _ ->
                    isValid <- false

            if isValid then Some (style, indent, chomp) else None

let private applyChomping (chomp: ChompingMode) (content: string) =
    match chomp with
    | ChompingMode.Strip -> content.TrimEnd([|'\r'; '\n'|])
    | ChompingMode.Keep -> content
    | ChompingMode.Clip ->
        // Clip: keep one newline at end if present
        let trimmed = content.TrimEnd([|'\r'; '\n'|])
        if content.Length > trimmed.Length then trimmed + "\n" else trimmed

let private countLeadingSpaces (line: string) =
    line |> Seq.takeWhile (fun c -> c = ' ') |> Seq.length

let private splitTrailingCommentPlaceholder (s: string) =
    let m = Regex.Match(s, $"^(?<header>.*?)\s*(?:{CommentPattern})\s*$")
    if m.Success then
        let header = m.Groups.["header"].Value.TrimEnd()
        let commentGroup = m.Groups.["comment"]
        let commentId =
            if commentGroup.Success && commentGroup.Value <> "" then
                Some (int commentGroup.Value)
            else
                None
        header, commentId
    else
        s.TrimEnd(), None

let private stripBlockIndent (indent: int) (line: string) =
    if line.Trim() = "" then
        ""
    else
        let available = countLeadingSpaces line
        let toStrip = min indent available
        line.Substring(toStrip)

let private foldLines (lines: string list) : string =
    let isMoreIndented (line: string) =
        line.StartsWith(" ") || line.StartsWith("\t")

    let arr = lines |> List.toArray
    let sb = System.Text.StringBuilder()

    for i in 0 .. arr.Length - 1 do
        let line = arr.[i]
        let isEmpty = line.Trim() = ""

        if isEmpty then
            sb.Append('\n') |> ignore
        else
            sb.Append(line) |> ignore
            if i < arr.Length - 1 then
                let next = arr.[i + 1]
                let nextIsEmpty = next.Trim() = ""
                if nextIsEmpty then
                    sb.Append('\n') |> ignore
                elif isMoreIndented line || isMoreIndented next then
                    sb.Append('\n') |> ignore
                else
                    sb.Append(' ') |> ignore

    sb.ToString()

let private deindentBlockLines (headerIndent: int) (explicitIndent: int option) (lines: string list) =
    let contentIndent =
        match explicitIndent with
        | Some i -> headerIndent + i
        | None ->
            lines
            |> List.filter (fun l -> l.Trim() <> "")
            |> List.map countLeadingSpaces
            |> function
                | [] -> headerIndent
                | indents -> indents |> List.min

    lines |> List.map (stripBlockIndent contentIndent)

let private buildBlockScalarContent (style: BlockScalarStyle) (chomp: ChompingMode) (headerIndent: int) (explicitIndent: int option) (lines: string list) =
    let deindentedLines = deindentBlockLines headerIndent explicitIndent lines
    let content =
        match style with
        | BlockScalarStyle.Literal ->
            if List.isEmpty deindentedLines then "" else System.String.Join("\n", deindentedLines) + "\n"
        | BlockScalarStyle.Folded ->
            let folded = foldLines deindentedLines
            if folded.EndsWith("\n") then folded else folded + "\n"
    applyChomping chomp content

let private restoreCommentReplace (commentDict: Dictionary<int, string>) (commentId: int option) =
    commentId |> Option.map (fun id -> commentDict.[id])

let rec collectSequenceElements (eles: PreprocessorElement list) : PreprocessorElement list list =
    match eles with
    | SequenceMinusOpener v::Intendation yamlAstList::rest ->
        [
            if v.Value.IsSome then
                PreprocessorElement.Line v.Value.Value::yamlAstList
            else
                yamlAstList
            yield! collectSequenceElements rest            
        ]
    | SequenceMinusOpener v::rest ->
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
    
let isSequenceElement = fun e -> match e with | Intendation _ | SequenceMinusOpener _ | YamlComment _ -> true | _ -> false

let private tokenize (yamlList: PreprocessorElement list) (stringDict: Dictionary<int, StringMapEntry>) (commentDict: Dictionary<int, string>) (handles: Map<string, string>) =
    // First pass: transform any flow-style elements to block-style
    let ctx = defaultContext stringDict
    let blockStyleList = transformElements ctx yamlList
    
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

    let rec flattenBlockScalarContentWithDepth (depth: int) (eles: PreprocessorElement list) : string list =
        eles
        |> List.collect (function
            | Line s ->
                let prefix =
                    if s = "" then ""
                    // Preprocessing strips one structural indent level when building
                    // Intendation children. Rehydrate that baseline so explicit-indicator
                    // deindent math can preserve leading content spaces.
                    else System.String(' ', (depth + 1) * 2)
                [prefix + restoreInlinePlaceholders s]
            | Intendation children ->
                flattenBlockScalarContentWithDepth (depth + 1) children
            | _ -> [])

    let flattenBlockScalarContent (eles: PreprocessorElement list) : string list =
        flattenBlockScalarContentWithDepth 0 eles

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

    let isBlockScalarHeaderCandidate (rawHeader: string) =
        let headerWithoutComment, _ = splitTrailingCommentPlaceholder rawHeader
        let props = extractProperties handles headerWithoutComment
        props.Value.StartsWith("|") || props.Value.StartsWith(">")

    let tryReadBlockScalar (rawHeader: string) (headerIndent: int) (baseCommentId: int option) (block: PreprocessorElement list) =
        let headerWithoutComment, headerCommentId = splitTrailingCommentPlaceholder rawHeader
        let props = extractProperties handles headerWithoutComment
        let commentId =
            match baseCommentId with
            | Some _ -> baseCommentId
            | None -> headerCommentId

        match parseBlockScalarHeader props.Value with
        | Some (style, indent, chomp) ->
            let lines = flattenBlockScalarContent block
            let value = buildBlockScalarContent style chomp headerIndent indent lines
            let comment = restoreCommentReplace commentDict commentId
            Some
                {| Props = props
                   Comment = comment
                   Style = style
                   Chomp = chomp
                   Indent = indent
                   Value = value |}
        | None ->
            None

    let rec loopRead (handles: Map<string, string>) (restlist: PreprocessorElement list) (acc: YAMLElement list) : YAMLElement =
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
            let sequenceElements = rest0 |> Seq.takeWhile isSequenceElement |> Seq.toList |> collectSequenceElements
            let rest = rest0 |> Seq.skipWhile isSequenceElement |> Seq.toList
            let current =
                YAMLElement.Sequence [
                    loopRead handles objectList []
                    for i in sequenceElements do
                        loopRead handles i []
                ]
            loopRead handles rest (current::acc)
        | SchemaNamespace v::rest0 -> //create/appendSequenceElement
            let sequenceElements = rest0 |> Seq.takeWhile isSequenceElement |> Seq.toList |> collectSequenceElements
            let rest = rest0 |> Seq.skipWhile isSequenceElement |> Seq.toList
            let current =
                YAMLElement.Sequence [
                    loopRead handles [PreprocessorElement.Line v.Key] []
                    for i in sequenceElements do
                        loopRead handles i []
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
        | SequenceMinusOpener v::Intendation yamlAstList::rest0 when v.Value.IsSome && isBlockScalarHeaderCandidate v.Value.Value ->
            match tryReadBlockScalar v.Value.Value v.Indent None yamlAstList with
            | Some blockScalar ->
                let sequenceElements = rest0 |> Seq.takeWhile isSequenceElement |> Seq.toList |> collectSequenceElements
                let rest = rest0 |> Seq.skipWhile isSequenceElement |> Seq.toList
                let firstItem =
                    YAMLElement.Object [
                        YAMLElement.Value(
                            YAMLContent.create(
                                blockScalar.Value,
                                ?comment = blockScalar.Comment,
                                ?anchor = blockScalar.Props.Anchor,
                                ?tag = blockScalar.Props.Tag,
                                style = ScalarStyle.Block(blockScalar.Style, blockScalar.Chomp, blockScalar.Indent)
                            )
                        )
                    ]
                let current =
                    YAMLElement.Sequence [
                        firstItem
                        for i in sequenceElements do
                            loopRead handles i []
                    ]
                loopRead handles rest (current::acc)
            | None ->
                failwithf "Invalid sequence block scalar header: %s" v.Value.Value
        | SequenceMinusOpener v::Intendation yamlAstList::rest0 -> //create/appendSequenceElement
            let objectList = 
                if v.Value.IsSome then
                    PreprocessorElement.Line v.Value.Value::yamlAstList
                else
                    yamlAstList
            let sequenceElements = rest0 |> Seq.takeWhile isSequenceElement |> Seq.toList |> collectSequenceElements
            let rest = rest0 |> Seq.skipWhile isSequenceElement |> Seq.toList
            let current =
                YAMLElement.Sequence [
                    loopRead handles objectList []
                    for i in sequenceElements do
                        loopRead handles i []
                ]
            loopRead handles rest (current::acc)
        | SequenceMinusOpener v::rest0 -> //create/appendSequenceElement
            let sequenceElements = rest0 |> Seq.takeWhile isSequenceElement |> Seq.toList |> collectSequenceElements
            let rest = rest0 |> Seq.skipWhile isSequenceElement |> Seq.toList
            let objectList =
                if v.Value.IsSome then
                    [PreprocessorElement.Line v.Value.Value]
                else
                    []
            let current =
                YAMLElement.Sequence [
                    loopRead handles objectList []
                    for i in sequenceElements do
                        loopRead handles i []
                ]
            loopRead handles rest (current::acc)
        // [test1, test2, test] <c f=1/>
        // NOTE: This handler processes flow-style arrays within block-style context (e.g., "- [v1, v2, v3]").
        // FlowToBlock.transformElements only processes top-level flow patterns, not those embedded in Line elements.
        // This handler splits simple comma-delimited sequences; nested structures are transformed by FlowToBlock.
        | InlineSequence v::rest -> // create sequence
            // ensure inline comment is added on top of the sequence
            let c = restoreCommentReplace commentDict v.Comment
            
            // Simple case: split by delimiter (nested structures already transformed)
            let split = v.Value.Split([|SequenceSquareDelimiter|], System.StringSplitOptions.RemoveEmptyEntries)
            let current =
                YAMLElement.Sequence [
                    for value in split do
                        loopRead handles [PreprocessorElement.Line (value.Trim())] []
                ]
            
            let nextAcc =
                if c.IsSome then 
                    current::YAMLElement.Comment c.Value::acc
                else 
                    current::acc
            loopRead handles rest nextAcc
        // [ #c1
        //   v1,
        //   v2,
        //   v3
        // ] #c2
        | SequenceSquareOpener opener::Intendation iList::SequenceSquareCloser closer::rest ->
            let c1 = opener.Comment |> restoreCommentReplace commentDict
            let c2 = closer.Comment |> restoreCommentReplace commentDict
            let current = 
                YAMLElement.Sequence [
                    for i in iList do
                        let i' =
                            match i with
                            | Line s -> s.TrimEnd(',') |> Line
                            | anyElse -> failwithf "Unexpected element in MultiLineSquareBrackets: %A" anyElse
                        loopRead handles [i'] []
                ]
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
        // These patterns should no longer be reached after FlowToBlock transformation
        // Keeping them for backward compatibility with simple cases
        | InlineJSON v::rest when v.Value.Trim() = "" -> // create empty object
            let c = restoreCommentReplace commentDict v.Comment
            let current = []
            let nextAcc =
                if c.IsSome then 
                    current@(YAMLElement.Comment c.Value::acc)
                else 
                    current@acc
            loopRead handles rest nextAcc
        // Defensive check: Flow-style patterns should have been transformed by FlowToBlock.
        // If we reach here, it indicates a bug in the transformation logic or an edge case.
        | JSONKeyOpener opener::Intendation _::JSONCloser _::_ ->
            failwithf "Untransformed flow-style object detected. This is a bug in FlowToBlock transformation. Pattern: %A" opener
        | InlineJSON v::_ when v.Value.Trim() <> "" ->
            failwithf "Untransformed non-empty flow-style object detected: {%s}. This is a bug in FlowToBlock transformation." v.Value
        // Explicit key with indented content (complex key), mapped to string for AST compatibility
        | ExplicitKey k::rest -> 
             let parseValue (vStr: string) =
                 let subPrep = Preprocessing.read vStr
                 let subLvl = match subPrep.AST with Level l -> l | _ -> []
                 // Reuse context but reset base indent to 0 for the sub-parse
                 let subCtx = { ctx with BaseIndent = 0 }
                 let transformed = transformElements subCtx subLvl
                 let result = loopRead handles transformed []
                 result

             match rest with
             | Intendation keyBody::ExplicitValue v::Intendation iList::tail ->
                let simplifiedKey = flattenBlockScalar keyBody |> String.concat "\n"
                let fullKey = match k with Some s -> s + (if s <> "" then "\n" else "") + simplifiedKey | None -> simplifiedKey
                let kp = extractProperties handles fullKey
                
                let separator = if v.TrimStart().StartsWith("[") || v.TrimStart().StartsWith("{") then " " else "\n"
                let fullValue = v + separator + (flattenBlockScalar iList |> String.concat separator)
                let valueElement = parseValue fullValue

                let current =
                    YAMLElement.Mapping (
                        YAMLContent.create(kp.Value, ?anchor=kp.Anchor, ?tag=kp.Tag),
                        valueElement
                    )
                loopRead handles tail (current::acc)
             | Intendation keyBody::ExplicitValue v::tail ->
                let simplifiedKey = flattenBlockScalar keyBody |> String.concat "\n"
                let fullKey = match k with Some s -> s + (if s <> "" then "\n" else "") + simplifiedKey | None -> simplifiedKey
                let kp = extractProperties handles fullKey
                
                let valueElement = parseValue v

                let current =
                    YAMLElement.Mapping (
                        YAMLContent.create(kp.Value, ?anchor=kp.Anchor, ?tag=kp.Tag),
                        valueElement
                    )
                loopRead handles tail (current::acc)
             | ExplicitValue v::Intendation iList::tail ->
                let kp = match k with Some s -> extractProperties handles s | None -> {| Value = ""; Anchor = None; Tag = None |}
                
                let fullValue = v + "\n" + (flattenBlockScalar iList |> String.concat "\n")
                let valueElement = parseValue fullValue

                let current =
                    YAMLElement.Mapping (
                        YAMLContent.create(kp.Value, ?anchor=kp.Anchor, ?tag=kp.Tag),
                        valueElement
                    )
                loopRead handles tail (current::acc)
             | ExplicitValue v::tail ->
                let kp = match k with Some s -> extractProperties handles s | None -> {| Value = ""; Anchor = None; Tag = None |}
                
                let valueElement = parseValue v

                let current =
                    YAMLElement.Mapping (
                        YAMLContent.create(kp.Value, ?anchor=kp.Anchor, ?tag=kp.Tag),
                        valueElement
                    )
                loopRead handles tail (current::acc)
             | _ ->
                // Orphan explicit key or unexpected sequence
                let kp = match k with Some s -> extractProperties handles s | None -> {| Value = ""; Anchor = None; Tag = None |}
                let current = 
                    YAMLElement.Mapping (
                        YAMLContent.create(kp.Value, ?anchor=kp.Anchor, ?tag=kp.Tag),
                        YAMLElement.Nil
                    )
                loopRead handles rest (current::acc)
        | Key v::Intendation yamlAstList::rest -> //createObject
            let c = restoreCommentReplace commentDict v.Comment
            let props = extractProperties handles v.Key
            let current = 
                YAMLElement.Mapping (
                    YAMLContent.create(props.Value, ?comment=c, ?anchor=props.Anchor, ?tag=props.Tag),
                    loopRead handles yamlAstList []
                )
            loopRead handles rest (current::acc)
        | Key v::SequenceMinusOpener w::Intendation yamlAstList::rest0 -> //create/appendSequenceElement
            let c = restoreCommentReplace commentDict v.Comment
            let props = extractProperties handles v.Key
            let objectList = 
                if w.Value.IsSome then
                    PreprocessorElement.Line w.Value.Value::yamlAstList
                else
                    yamlAstList
            let sequenceElements = rest0 |> Seq.takeWhile isSequenceElement |> Seq.toList |> collectSequenceElements
            let rest = rest0 |> Seq.skipWhile isSequenceElement |> Seq.toList
            let seq =
                YAMLElement.Sequence [
                    loopRead handles objectList []
                    for i in sequenceElements do
                        loopRead handles i []
                ]
            let current = 
                YAMLElement.Mapping (
                    YAMLContent.create(props.Value, ?comment=c, ?anchor=props.Anchor, ?tag=props.Tag),
                    YAMLElement.Object [seq]
                )
            loopRead handles rest (current::acc)
        | Key v::SequenceMinusOpener w::rest0 -> //createObject
            let c = restoreCommentReplace commentDict v.Comment
            let props = extractProperties handles v.Key
            let sequenceElements = rest0 |> Seq.takeWhile isSequenceElement |> Seq.toList |> collectSequenceElements
            let rest = rest0 |> Seq.skipWhile isSequenceElement |> Seq.toList
            let seq =
                YAMLElement.Sequence [
                    loopRead handles [PreprocessorElement.Line w.Value.Value] []
                    for i in sequenceElements do
                        loopRead handles i []
                ]
            let current = 
                YAMLElement.Mapping (
                    YAMLContent.create(props.Value, ?comment=c, ?anchor=props.Anchor, ?tag=props.Tag),
                    YAMLElement.Object [seq]
                )
            loopRead handles rest (current::acc)
        // doc: |2\n  <block>
        | KeyValue v::Intendation block::rest when isBlockScalarHeaderCandidate v.Value ->
            match tryReadBlockScalar v.Value v.Indent None block with
            | Some blockScalar ->
                let keyProps = extractProperties handles v.Key
                let current =
                    YAMLElement.Mapping(
                        YAMLContent.create(keyProps.Value, ?anchor=keyProps.Anchor, ?tag=keyProps.Tag),
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
        // My Key: [My Value, Test2]
        | KeyValue v::rest -> // createKeyValue
            let props = extractProperties handles v.Key
            let current = 
                YAMLElement.Mapping (
                    YAMLContent.create(props.Value, ?anchor=props.Anchor, ?tag=props.Tag),
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
        | YamlValue v::Intendation block::rest when isBlockScalarHeaderCandidate v.Value ->
            match tryReadBlockScalar v.Value v.Indent v.Comment block with
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
    loopRead handles blockStyleList []

let read (yaml: string) =
    let ast = Preprocessing.read yaml
    match ast.AST with
    | Level lvl ->
        tokenize lvl ast.StringMap ast.CommentMap ast.TagHandles
    | _ -> failwith "Not a root!"

let private isStreamDocumentMarker (markerCheck: string -> bool) (line: string) =
    countLeadingSpaces line = 0 && markerCheck line

let private tryInlineContentAfterStartMarker (line: string) =
    let trimmed = line.TrimStart()
    if not (trimmed.StartsWith("---")) then
        None
    else
        let rest = trimmed.Substring(3).TrimStart()
        if System.String.IsNullOrWhiteSpace(rest) || rest.StartsWith("#") then
            None
        else
            Some rest

let private isDirectivePreludeLine (line: string) =
    let t = line.TrimStart()
    t = "" || t.StartsWith("%") || t.StartsWith("#")

let private isDirectivePreludeOnly (lines: string list) =
    let hasDirective =
        lines
        |> List.exists (fun line -> line.TrimStart().StartsWith("%"))
    hasDirective && (lines |> List.forall isDirectivePreludeLine)

let private tryDetectBlockScalarHeaderIndent (line: string) =
    let stripProperties (s: string) =
        let rec loop (current: string) =
            let c = current.TrimStart()
            if c.StartsWith("&") then
                let idx = c.IndexOf(' ')
                if idx < 0 then "" else loop (c.Substring(idx + 1))
            elif c.StartsWith("!<") then
                let idx = c.IndexOf('>')
                if idx < 0 then c else loop (c.Substring(idx + 1))
            elif c.StartsWith("!") && not (c.StartsWith("|")) && not (c.StartsWith(">")) then
                let idx = c.IndexOf(' ')
                if idx < 0 then "" else loop (c.Substring(idx + 1))
            else
                c
        loop s

    let trimmed = line.TrimStart()
    let afterDash =
        if trimmed.StartsWith("- ") then
            trimmed.Substring(2).TrimStart()
        else
            trimmed
    let candidate =
        let idx = afterDash.IndexOf(':')
        if idx >= 0 then afterDash.Substring(idx + 1).TrimStart()
        else afterDash
    let withoutProps = stripProperties candidate
    let headerToken =
        withoutProps.Split([|' '; '\t'|], System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.tryHead

    match headerToken with
    | Some token when parseBlockScalarHeader token |> Option.isSome ->
        Some (countLeadingSpaces line)
    | _ ->
        None

let readDocuments (yaml: string) : YAMLElement list =
    let normalized = yaml.Replace("\r\n", "\n").Replace("\r", "\n")
    let lines = normalized.Split([|'\n'|], System.StringSplitOptions.None) |> Array.toList

    let appendCurrentDocument (currentDoc: string list) (docs: string list list) =
        if List.isEmpty currentDoc then docs else (List.rev currentDoc)::docs

    let rec splitDocuments (remaining: string list) (currentDoc: string list) (docs: string list list) (blockHeaderIndent: int option) =
        match remaining with
        | [] ->
            appendCurrentDocument currentDoc docs |> List.rev
        | line::rest ->
            match blockHeaderIndent with
            | Some headerIndent ->
                if line.Trim() = "" || countLeadingSpaces line > headerIndent then
                    splitDocuments rest (line::currentDoc) docs blockHeaderIndent
                else
                    // A non-empty line dedented to the header level ends the block scalar.
                    splitDocuments remaining currentDoc docs None
            | None when isStreamDocumentMarker isDocumentStart line ->
                let inlineContent = tryInlineContentAfterStartMarker line
                if List.isEmpty currentDoc || isDirectivePreludeOnly currentDoc then
                    let preludeDirectives =
                        currentDoc
                        |> List.filter (fun l -> l.TrimStart().StartsWith("%"))
                    let nextDoc =
                        match inlineContent with
                        | Some content -> content::preludeDirectives
                        | None -> preludeDirectives
                    splitDocuments rest nextDoc docs None
                else
                    let docs' = appendCurrentDocument currentDoc docs
                    let nextDoc =
                        match inlineContent with
                        | Some content -> [content]
                        | None -> []
                    splitDocuments rest nextDoc docs' None
            | None when isStreamDocumentMarker isDocumentEnd line ->
                let docs' = appendCurrentDocument currentDoc docs
                splitDocuments rest [] docs' None
            | None ->
                let nextBlock =
                    match tryDetectBlockScalarHeaderIndent line with
                    | Some indent -> Some indent
                    | None -> None
                splitDocuments rest (line::currentDoc) docs nextBlock

    let documentTexts =
        splitDocuments lines [] [] None
        |> List.filter (fun doc ->
            doc |> List.exists (fun l -> l.Trim() <> "")
        )

    documentTexts
    |> List.map (fun docLines ->
        let docText = String.concat "\n" docLines
        read docText
    )
