module YAMLicious.Reader

open Regex
open Preprocessing
open System.Text.RegularExpressions
open RegexActivePatterns
open YAMLiciousTypes
open System.Collections.Generic

module FlowStyleParser =
    type Token =
        | OpenBrace
        | CloseBrace
        | OpenBracket
        | CloseBracket
        | Colon
        | Comma
        | String of string
        | EOF

    let tokenize (input: string) : Token list =
        let rec parseString (chars: char list) (acc: char list) : char list * char list =
            match chars with
            | [] -> (List.rev acc, [])
            | '"'::rest -> (List.rev acc, rest)
            | '\\'::c::rest -> parseString rest (c::'\\'::acc)
            | c::rest -> parseString rest (c::acc)

        let rec tokenizeChars (chars: char list) (acc: Token list) : Token list =
            match chars with
            | [] -> List.rev (Token.EOF::acc)
            | ' '::rest | '\n'::rest | '\r'::rest | '\t'::rest -> tokenizeChars rest acc
            | '{'::rest -> tokenizeChars rest (Token.OpenBrace::acc)
            | '}'::rest -> tokenizeChars rest (Token.CloseBrace::acc)
            | '['::rest -> tokenizeChars rest (Token.OpenBracket::acc)
            | ']'::rest -> tokenizeChars rest (Token.CloseBracket::acc)
            | ':'::rest -> tokenizeChars rest (Token.Colon::acc)
            | ','::rest -> tokenizeChars rest (Token.Comma::acc)
            | '"'::rest ->
                let (str, remaining) = parseString rest []
                let strValue = System.String(Array.ofList str)
                tokenizeChars remaining (Token.String strValue::acc)
            | chars ->
                // Parse unquoted string until delimiter
                let rec parseUnquoted (cs: char list) (acc: char list) =
                    match cs with
                    | [] -> (List.rev acc, [])
                    | c::rest when c = '{' || c = '}' || c = '[' || c = ']' || c = ':' || c = ',' || c = ' ' || c = '\n' || c = '\r' || c = '\t' ->
                        (List.rev acc, cs)
                    | c::rest -> parseUnquoted rest (c::acc)
                
                let (str, remaining) = parseUnquoted chars []
                if str.IsEmpty then
                    tokenizeChars remaining acc
                else
                    let strValue = System.String(Array.ofList str)
                    tokenizeChars remaining (Token.String strValue::acc)

        tokenizeChars (List.ofSeq input) []

    let rec parseValue (tokens: Token list) : YAMLElement * Token list =
        match tokens with
        | Token.OpenBrace::rest -> parseObject rest
        | Token.OpenBracket::rest -> parseArray rest
        | Token.String s::rest -> (YAMLElement.Value(YAMLContent.create(s)), rest)
        | _ -> failwith $"Unexpected token in parseValue: {tokens}"

    and parseObject (tokens: Token list) : YAMLElement * Token list =
        let rec parseKeyValues (tokens: Token list) (acc: YAMLElement list) : YAMLElement list * Token list =
            match tokens with
            | Token.CloseBrace::rest -> (acc, rest)  // Don't reverse - maintain order
            | Token.Comma::rest -> parseKeyValues rest acc
            | Token.EOF::_ -> (acc, tokens)
            | Token.String key::Token.Colon::rest ->
                let (value, remaining) = parseValue rest
                // Only wrap Values, not Objects (which are already wrapped)
                // But DO wrap Sequences - block-style wraps them
                let wrappedValue =
                    match value with
                    | YAMLElement.Object _ -> value  // Object - already wrapped, don't double-wrap
                    | _ -> YAMLElement.Object [value]  // Value or Sequence - needs wrapping
                let mapping = YAMLElement.Mapping(YAMLContent.create(key), wrappedValue)
                // Use cons and reverse at the end for efficiency
                parseKeyValues remaining (mapping::acc)
            | _ -> failwith $"Unexpected token in parseObject: {tokens}"

        let (kvPairs, remaining) = parseKeyValues tokens []
        (YAMLElement.Object (List.rev kvPairs), remaining)

    and parseArray (tokens: Token list) : YAMLElement * Token list =
        let rec parseElements (tokens: Token list) (acc: YAMLElement list) : YAMLElement list * Token list =
            match tokens with
            | Token.CloseBracket::rest -> (acc, rest)  // Don't reverse yet
            | Token.Comma::rest -> parseElements rest acc
            | Token.EOF::_ -> (acc, tokens)
            | _ ->
                let (value, remaining) = parseValue tokens
                // Don't double-wrap Objects - array elements can already be Objects
                let wrappedValue =
                    match value with
                    | YAMLElement.Object _ -> value  // Already an Object
                    | _ -> YAMLElement.Object [value]  // Value or Sequence - wrap it
                parseElements remaining (wrappedValue::acc)

        let (elements, remaining) = parseElements tokens []
        (YAMLElement.Sequence (List.rev elements), remaining)

    let parseFlowStyle (input: string) : YAMLElement =
        let tokens = tokenize input
        let (result, _) = parseValue tokens
        result

let private restoreStringReplace (stringDict: Dictionary<int, string>) (v: string)  =
    let m = Regex.Match(v, StringReplacementPattern)
    if m.Success then
        let index = m.Groups.["index"].Value |> int
        stringDict.[index]
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
    // Helper to restore string placeholders in flow-style content (JSON-like syntax)
    let restoreFlowStyleStrings (content: string) =
        Regex.Replace(
            content,
            StringReplacementPattern,
            (fun (m: Match) ->
                let idx = m.Groups.["index"].Value |> int
                "\"" + stringDict.[idx] + "\""
            )
        )
    
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
            //printfn "[tokenize] Case1"
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
            //printfn "[tokenize] Case2"
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
            //printfn "[tokenize] Case1"
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
            //printfn "[tokenize] Case2"
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
        | InlineSequence v::rest -> // create sequence
            //printfn "[tokenize] Case3"
            // ensure inline comment is added on top of the sequence
            let c = restoreCommentReplace commentDict v.Comment
            
            // Check if this is a simple inline sequence or contains nested structures
            let hasNestedStructures = v.Value.Contains("{") || v.Value.Contains("[")
            
            let current =
                if hasNestedStructures then
                    // Restore string placeholders and use flow-style parser
                    let restored = restoreFlowStyleStrings ("[" + v.Value + "]")
                    match FlowStyleParser.parseFlowStyle restored with
                    | YAMLElement.Sequence s -> YAMLElement.Sequence s
                    | _ -> failwith "Expected Sequence from InlineSequence parsing"
                else
                    // Simple case: split by delimiter
                    let split = v.Value.Split([|SequenceSquareDelimiter|], System.StringSplitOptions.RemoveEmptyEntries)
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
            //printfn "[tokenize] Case3.5"
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
        | InlineJSON v::rest -> // create object from inline JSON
            //printfn "[tokenize] Case3.1"
            // ensure inline comment is added on top of the object
            let c = restoreCommentReplace commentDict v.Comment
            // Handle empty objects
            if v.Value.Trim() = "" then
                // Empty object - return empty Object list
                let current = []
                let nextAcc =
                    if c.IsSome then 
                        current@(YAMLElement.Comment c.Value::acc)
                    else 
                        current@acc
                loopRead rest nextAcc
            else
                // Restore string placeholders before parsing
                let restored = restoreFlowStyleStrings ("{" + v.Value + "}")
                // Use flow-style parser for nested structures
                let parsed = FlowStyleParser.parseFlowStyle restored
                let current =
                    match parsed with
                    | YAMLElement.Object o -> List.rev o  // Unwrap to get mappings - reverse because acc will be reversed later
                    | _ -> failwith "Expected Object from InlineJSON parsing"
                let nextAcc =
                    if c.IsSome then 
                        current@(YAMLElement.Comment c.Value::acc)
                    else 
                        current@acc
                loopRead rest nextAcc
        | JSONKeyOpener opener::Intendation iList::JSONCloser closer::rest ->
            //printfn "[tokenize] Case3.6"
            let c1 = opener.Comment |> restoreCommentReplace commentDict
            let c2 = closer.Comment |> restoreCommentReplace commentDict
            
            // Recursively flatten all lines from the indentation
            let rec flattenLines (eles: PreprocessorElement list) : string list =
                eles
                |> List.collect (function
                    | Line s -> [s.TrimEnd(',')]
                    | Intendation children -> flattenLines children
                    | anyElse -> failwithf "Unexpected element in JSON flattening: %A" anyElse)
            
            // Build the complete JSON string and parse it with flow-style parser
            let jsonContent = flattenLines iList |> String.concat "\n"
            
            // Restore string placeholders before parsing
            let restored = restoreFlowStyleStrings ("{" + jsonContent + "}")
            
            let parsed = FlowStyleParser.parseFlowStyle restored
            
            let current = 
                YAMLElement.Mapping (
                    YAMLContent.create(opener.Key, ?comment=c1),
                    parsed  // Use FlowStyleParser result directly (it now wraps nested objects correctly)
                )
            let nextAcc =
                match c2 with
                | Some c2 -> 
                    YAMLElement.Comment c2::current::acc
                | None ->
                    current::acc
            loopRead rest nextAcc
        | Key v::Intendation yamlAstList::rest -> //createObject
            //printfn "[tokenize] Case4"
            let c = restoreCommentReplace commentDict v.Comment
            let current = 
                YAMLElement.Mapping (
                    YAMLContent.create(v.Key, ?comment=c),
                    loopRead yamlAstList []
                )
            loopRead rest (current::acc)
        | Key v::SequenceMinusOpener w::Intendation yamlAstList::rest0 -> //create/appendSequenceElement
            //printfn "[tokenize] Case4.1"
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
            //printfn "[tokenize] Case4.2"
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
            //printfn "[tokenize] Case4.9 Block scalar"
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
            //printfn "[tokenize] Case5"
            let current = 
                YAMLElement.Mapping (
                    YAMLContent.create(v.Key),
                    //reuse default parsing into SequenceElements
                    loopRead [PreprocessorElement.Line v.Value] []
                )
            loopRead rest (current::acc)
        // <c f=1/>
        | YamlComment v::rest -> // createComment
            //printfn "[tokenize] Case5.5"
            let c = commentDict.[v.Comment]
            let current = 
                YAMLElement.Comment (c)
            loopRead rest (current::acc)
        // My Value <c f=1/>
        | YamlValue v::rest -> // createValue
            //printfn "[tokenize] Case6"
            let raw = restoreStringReplace stringDict v.Value
            let c = restoreCommentReplace commentDict v.Comment
            let current = 
                YAMLElement.Value (
                    YAMLContent.create(raw, ?comment=c)
                )
            loopRead rest (current::acc)
        | [] ->
            //printfn "[tokenize] Exit Multiple: Object"
            acc
            |> List.rev
            |> YAMLElement.Object
        | anyElse -> failwithf "Unknown pattern: %A" anyElse
    loopRead yamlList []

let read (yaml: string) =
    let ast = read yaml
    match ast.AST with
    | Level lvl ->
        tokenize lvl ast.StringMap ast.CommentMap
    | _ -> failwith "Not a root!"