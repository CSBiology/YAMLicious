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

/// Minus opener Sequence elements are difficult to collect with our logic. So whenever we return a list of YAMLElements we
/// should check for SequenceElements and collect them into a single Sequence
let private collectSequenceElements (eles: YAMLElement list) =
    let rec loop (eles: YAMLElement list) (latestSeqItems: YAMLElement list) (acc: YAMLElement list) =
        match eles with
        | YAMLElement.SequenceElement i::rest -> loop rest (YAMLElement.SequenceElement i::latestSeqItems) acc
        | anyElse::rest -> 
            if latestSeqItems.Length > 0 then
                let seq = YAMLElement.Sequence (List.rev latestSeqItems)
                loop rest [] (anyElse::seq::acc)
            else
                loop rest [] (anyElse::acc)
        | [] -> 
            if latestSeqItems.Length > 0 then
                let seq = YAMLElement.Sequence (latestSeqItems)
                seq::acc
            else
                acc
    loop eles [] []

let private tokenize (yamlList: PreprocessorElement list) (stringDict: Dictionary<int, string>) (commentDict: Dictionary<int, string>) =
    let rec loopRead (restlist: PreprocessorElement list) (acc: YAMLElement list) =
        match restlist with
        // Example1: 
        // - My Value 1 <c f=1/>
        //   My Value 2
        // - My Value 3
        // Example2:
        // -
        //   My Key1: My Value1
        //   My Key2: My Value2
        //   My Key3: My Value3
        | SequenceMinusOpener v::Intendation yamlAstList::rest -> //create/appendSequenceElement
            printfn "[readList] Case1"
            let objectList = 
                if v.Value.IsSome then
                    PreprocessorElement.Line v.Value.Value::yamlAstList
                else
                    yamlAstList
            let current =
                YAMLElement.SequenceElement (
                    loopRead objectList []
                )
            loopRead rest (current::acc)
        | SequenceMinusOpener v::rest -> //create/appendSequenceElement
            printfn "[readList] Case2"
            let current =
                YAMLElement.SequenceElement (
                    loopRead [PreprocessorElement.Line v.Value.Value] []
                )
            loopRead rest (current::acc)
        // [test1, test2, test] <c f=1/>
        | InlineSequence v::rest -> // create sequence
            printfn "[readList] Case3"
            // ensure inline comment is added on top of the sequence
            let container =
                let c = restoreCommentReplace commentDict v.Comment
                if c.IsSome then 
                    fun seq -> YAMLElement.Object [
                        YAMLElement.Comment c.Value
                        YAMLElement.Sequence seq
                    ]
                else
                    fun seq -> YAMLElement.Sequence seq
            // split inline sequence by delimiter, then reuse default parsing into SequenceElements
            let split = v.Value.Split([|SequenceSquareDelimiter|], System.StringSplitOptions.RemoveEmptyEntries)
            let current =
                container [
                    for value in split do
                        loopRead [PreprocessorElement.Line value] []
                        |> YAMLElement.SequenceElement
                ]
            loopRead rest (current::acc)
        // [ #c1
        //   v1,
        //   v2,
        //   v3
        // ] #c2
        | SequenceSquareOpener opener::Intendation iList::SequenceSquareCloser closer::rest ->
            let c1 = opener.Comment |> restoreCommentReplace commentDict
            let c2 = closer.Comment |> restoreCommentReplace commentDict
            let current = 
                YAMLElement.Object [
                    if c1.IsSome then YAMLElement.Comment c1.Value
                    YAMLElement.Sequence [
                        for i in iList do
                            let i' =
                                match i with
                                | Line s -> s.TrimEnd(',') |> Line
                                | anyElse -> failwithf "Unexpected element in MultiLineSquareBrackets: %A" anyElse
                            loopRead [i'] []
                            |> YAMLElement.SequenceElement
                    ]
                    if c2.IsSome then YAMLElement.Comment c2.Value
                ]
            loopRead rest (current::acc)
        | Key v::Intendation yamlAstList::rest -> //createObject
            printfn "[readList] Case4"
            let c = restoreCommentReplace commentDict v.Comment
            let current = 
                YAMLElement.Mapping (
                    YAMLContent.create(v.Key, ?comment=c),
                    loopRead yamlAstList []
                )
            loopRead rest (current::acc)
        // My Key: [My Value, Test2]
        | KeyValue v::rest -> // createKeyValue
            printfn "[readList] Case5"
            let current = 
                YAMLElement.Mapping (
                    YAMLContent.create(v.Key),
                    //reuse default parsing into SequenceElements
                    loopRead [PreprocessorElement.Line v.Value] []
                )
            loopRead rest (current::acc)
        // <c f=1/>
        | YamlComment v::rest -> // createComment
            printfn "[readList] Case5.5"
            let c = commentDict.[v.Comment]
            let current = 
                YAMLElement.Comment (c)
            loopRead rest (current::acc)
        // My Value <c f=1/>
        | YamlValue v::rest -> // createValue
            printfn "[readList] Case6"
            let raw = restoreStringReplace stringDict v.Value
            let c = restoreCommentReplace commentDict v.Comment
            let current = 
                YAMLElement.Value (
                    YAMLContent.create(raw, ?comment=c)
                )
            loopRead rest (current::acc)
        | [] when acc.Length = 1 -> acc.Head
        | [] ->
            acc
            |> collectSequenceElements // collect sequence elements into a list
            |> YAMLElement.Object
        | anyElse -> failwithf "Unknown pattern: %A" anyElse
    match loopRead yamlList [] with
    | YAMLElement.Object _ as o -> o
    | anyElse -> YAMLElement.Object [anyElse]

    

let read (yaml: string) =
    let ast = read yaml
    match ast.AST with
    | Level lvl ->
        tokenize lvl ast.StringMap ast.CommentMap
    | _ -> failwith "Not a root!"