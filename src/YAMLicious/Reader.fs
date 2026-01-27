module YAMLicious.Reader

open Regex
open Preprocessing
open FlowToBlock
open Escapes
open System.Text.RegularExpressions
open RegexActivePatterns
open YAMLiciousTypes
open System.Collections.Generic

let private restoreStringReplace (stringDict: Dictionary<int, string>) (v: string)  =
    let m = Regex.Match(v, StringReplacementPattern)
    if m.Success then
        let index = m.Groups.["index"].Value |> int
        let rawString = stringDict.[index]
        // Apply escape sequence processing to double-quoted strings
        unescapeDoubleQuoted rawString
    else
        v

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
            [PreprocessorElement.Line v.Value.Value]
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

let private tokenize (yamlList: PreprocessorElement list) (stringDict: Dictionary<int, string>) (commentDict: Dictionary<int, string>) =
    // First pass: transform any flow-style elements to block-style
    let ctx = defaultContext stringDict
    let blockStyleList = transformElements ctx yamlList
    
    // restore string and comment placeholders inside block scalar content
    let restoreInlinePlaceholders (line: string) =
        let withStrings =
            Regex.Replace(
                line,
                StringReplacementPattern,
                (fun (m: Match) ->
                    let idx = m.Groups.["index"].Value |> int
                    stringDict.[idx]
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

    let rec flattenBlockScalar (eles: PreprocessorElement list) : string list =
        eles
        |> List.collect (function
            | Line s -> [restoreInlinePlaceholders s]
            | Intendation children -> flattenBlockScalar children
            | _ -> [])

    let rec loopRead (restlist: PreprocessorElement list) (acc: YAMLElement list) : YAMLElement =
        match restlist with
        | SchemaNamespace v::Intendation yamlAstList::rest0 -> //create/appendSequenceElement
            let objectList = 
                PreprocessorElement.Line v.Key::yamlAstList
            let sequenceElements = rest0 |> Seq.takeWhile isSequenceElement |> Seq.toList |> collectSequenceElements
            let rest = rest0 |> Seq.skipWhile isSequenceElement |> Seq.toList
            let current =
                YAMLElement.Sequence [
                    loopRead objectList []
                    for i in sequenceElements do
                        loopRead i []
                ]
            loopRead rest (current::acc)
        | SchemaNamespace v::rest0 -> //create/appendSequenceElement
            let sequenceElements = rest0 |> Seq.takeWhile isSequenceElement |> Seq.toList |> collectSequenceElements
            let rest = rest0 |> Seq.skipWhile isSequenceElement |> Seq.toList
            let current =
                YAMLElement.Sequence [
                    loopRead [PreprocessorElement.Line v.Key] []
                    for i in sequenceElements do
                        loopRead i []
                ]
            loopRead rest (current::acc)
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
            let objectList = 
                if v.Value.IsSome then
                    PreprocessorElement.Line v.Value.Value::yamlAstList
                else
                    yamlAstList
            let sequenceElements = rest0 |> Seq.takeWhile isSequenceElement |> Seq.toList |> collectSequenceElements
            let rest = rest0 |> Seq.skipWhile isSequenceElement |> Seq.toList
            let current =
                YAMLElement.Sequence [
                    loopRead objectList []
                    for i in sequenceElements do
                        loopRead i []
                ]
            loopRead rest (current::acc)
        | SequenceMinusOpener v::rest0 -> //create/appendSequenceElement
            let sequenceElements = rest0 |> Seq.takeWhile isSequenceElement |> Seq.toList |> collectSequenceElements
            let rest = rest0 |> Seq.skipWhile isSequenceElement |> Seq.toList
            let current =
                YAMLElement.Sequence [
                    loopRead [PreprocessorElement.Line v.Value.Value] []
                    for i in sequenceElements do
                        loopRead i []
                ]
            loopRead rest (current::acc)
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
                        loopRead [PreprocessorElement.Line (value.Trim())] []
                ]
            
            let nextAcc =
                if c.IsSome then 
                    current::YAMLElement.Comment c.Value::acc
                else 
                    current::acc
            loopRead rest nextAcc
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
                        loopRead [i'] []
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
            loopRead rest nextAcc
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
            loopRead rest nextAcc
        // Defensive check: Flow-style patterns should have been transformed by FlowToBlock.
        // If we reach here, it indicates a bug in the transformation logic or an edge case.
        | JSONKeyOpener opener::Intendation _::JSONCloser _::_ ->
            failwithf "Untransformed flow-style object detected. This is a bug in FlowToBlock transformation. Pattern: %A" opener
        | InlineJSON v::_ when v.Value.Trim() <> "" ->
            failwithf "Untransformed non-empty flow-style object detected: {%s}. This is a bug in FlowToBlock transformation." v.Value
        | Key v::Intendation yamlAstList::rest -> //createObject
            let c = restoreCommentReplace commentDict v.Comment
            let current = 
                YAMLElement.Mapping (
                    YAMLContent.create(v.Key, ?comment=c),
                    loopRead yamlAstList []
                )
            loopRead rest (current::acc)
        | Key v::SequenceMinusOpener w::Intendation yamlAstList::rest0 -> //create/appendSequenceElement
            let c = restoreCommentReplace commentDict v.Comment
            let objectList = 
                if w.Value.IsSome then
                    PreprocessorElement.Line w.Value.Value::yamlAstList
                else
                    yamlAstList
            let sequenceElements = rest0 |> Seq.takeWhile isSequenceElement |> Seq.toList |> collectSequenceElements
            let rest = rest0 |> Seq.skipWhile isSequenceElement |> Seq.toList
            let seq =
                YAMLElement.Sequence [
                    loopRead objectList []
                    for i in sequenceElements do
                        loopRead i []
                ]
            let current = 
                YAMLElement.Mapping (
                    YAMLContent.create(v.Key, ?comment=c),
                    YAMLElement.Object [seq]
                )
            loopRead rest (current::acc)
        | Key v::SequenceMinusOpener w::rest0 -> //createObject
            let c = restoreCommentReplace commentDict v.Comment
            let sequenceElements = rest0 |> Seq.takeWhile isSequenceElement |> Seq.toList |> collectSequenceElements
            let rest = rest0 |> Seq.skipWhile isSequenceElement |> Seq.toList
            let seq =
                YAMLElement.Sequence [
                    loopRead [PreprocessorElement.Line w.Value.Value] []
                    for i in sequenceElements do
                        loopRead i []
                ]
            let current = 
                YAMLElement.Mapping (
                    YAMLContent.create(v.Key, ?comment=c),
                    YAMLElement.Object [seq]
                )
            loopRead rest (current::acc)
        // doc: |\n  <block>
        | KeyValue v::Intendation block::rest when v.Value = "|" || v.Value = ">" ->
            let lines = flattenBlockScalar block
            // '|' keeps new lines, '>' folds them. Here we keep simple behavior: preserve new lines for both.
            let blockValue = System.String.Join((string NewLineChar), lines)
            let current =
                YAMLElement.Mapping (
                    YAMLContent.create(v.Key),
                    YAMLElement.Value (YAMLContent.create(blockValue))
                )
            loopRead rest (current::acc)
        // My Key: [My Value, Test2]
        | KeyValue v::rest -> // createKeyValue
            let current = 
                YAMLElement.Mapping (
                    YAMLContent.create(v.Key),
                    //reuse default parsing into SequenceElements
                    loopRead [PreprocessorElement.Line v.Value] []
                )
            loopRead rest (current::acc)
        // <c f=1/>
        | YamlComment v::rest -> // createComment
            let c = commentDict.[v.Comment]
            let current = 
                YAMLElement.Comment (c)
            loopRead rest (current::acc)
        // My Value <c f=1/>
        | YamlValue v::rest -> // createValue
            let raw = restoreStringReplace stringDict v.Value
            let c = restoreCommentReplace commentDict v.Comment
            let current = 
                YAMLElement.Value (
                    YAMLContent.create(raw, ?comment=c)
                )
            loopRead rest (current::acc)
        | [] ->
            acc
            |> List.rev
            |> YAMLElement.Object
        | anyElse -> failwithf "Unknown pattern: %A" anyElse
    loopRead blockStyleList []

let read (yaml: string) =
    let ast = read yaml
    match ast.AST with
    | Level lvl ->
        tokenize lvl ast.StringMap ast.CommentMap
    | _ -> failwith "Not a root!"

let readDocuments (yaml: string) : YAMLElement list =
    // Split the input YAML by document start markers (---)
    let lines = yaml.Split([|"\r\n"; "\n"|], System.StringSplitOptions.None) |> Array.toList
    
    // Reconstruct documents by grouping lines
    let rec splitDocuments (lines: string list) (currentDoc: string list) (docs: string list list) =
        match lines with
        | [] ->
            if List.isEmpty currentDoc then
                List.rev docs
            else
                List.rev (List.rev currentDoc :: docs)
        | line::rest when isDocumentStart line ->
            // Start a new document (skip the --- line)
            if List.isEmpty currentDoc then
                splitDocuments rest [] docs
            else
                splitDocuments rest [] (List.rev currentDoc :: docs)
        | line::rest when line.TrimStart().StartsWith("...") ->
            // End current document (don't include the ... marker) - using DocumentEnd pattern logic
            if List.isEmpty currentDoc then
                splitDocuments rest [] docs
            else
                splitDocuments rest [] (List.rev currentDoc :: docs)
        | line::rest ->
            splitDocuments rest (line::currentDoc) docs
    
    // Split documents by markers
    let documentTexts = splitDocuments lines [] []
    
    // Parse each document
    documentTexts
    |> List.filter (fun doc -> not (List.isEmpty doc))
    |> List.map (fun docLines ->
        let docText = String.concat "\n" docLines
        read docText
    )