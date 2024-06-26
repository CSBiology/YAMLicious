module YAMLicious.Reader

open Regex
open Preprocessing
open System.Text.RegularExpressions
open RegexActivePatterns
open YAMLiciousTypes
open System.Collections.Generic

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
            // split inline sequence by delimiter, then reuse default parsing into SequenceElements
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
        | InlineJSON v::rest -> // create sequence
            //printfn "[tokenize] Case3.1"
            // ensure inline comment is added on top of the sequence
            let c = restoreCommentReplace commentDict v.Comment
            // split inline sequence by delimiter, then reuse default parsing into SequenceElements
            let split = v.Value.Split([|SequenceSquareDelimiter|], System.StringSplitOptions.RemoveEmptyEntries)
            let current =
                [
                    for value in split do
                        match loopRead [PreprocessorElement.Line (value.Trim())] [] with
                        | YAMLElement.Object o -> yield! o
                        |_ -> failwith "Unexpected element in InlineJSON"
                    
                ]
                |> List.rev
            let nextAcc =
                if c.IsSome then 
                    current@(YAMLElement.Comment c.Value::acc)
                else 
                    current@acc
            loopRead rest nextAcc
        //Sammy Sosa: { #lel
        //    hr: 63,
        //    avg: 0.288,
        //}
        | JSONKeyOpener opener::Intendation iList::JSONCloser closer::rest ->
            //printfn "[tokenize] Case3.6"
            let c1 = opener.Comment |> restoreCommentReplace commentDict
            let c2 = closer.Comment |> restoreCommentReplace commentDict
            let current = 
                YAMLElement.Mapping (
                    YAMLContent.create(opener.Key, ?comment=c1),
                    YAMLElement.Object [
                        for i in iList do
                            let i' =
                                match i with
                                | Line s -> s.TrimEnd(',') |> Line
                                | anyElse -> failwithf "Unexpected element in MultiLineSquareBrackets: %A" anyElse
                            match loopRead [i'] [] with
                            | YAMLElement.Object o -> yield! o
                            |_ -> failwith "Unexpected element in MultilineJSON"
                    ]
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