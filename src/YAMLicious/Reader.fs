module YAMLicious.Reader

open PatternMatcher
open AST
open System.Text.RegularExpressions

module RegexActivePatterns =
    open System.Text.RegularExpressions

    // Define the active pattern
    let (|Key|_|) (input: YAMLASTElement) =
        match input with
        | Line s ->
            let m = Regex.Match(s, KeyPattern)
            if m.Success then 
                let comment: int option = 
                    let v = m.Groups.["comment"].Value
                    if v = "" then None else Some (int v)
                Some {| Comment = comment; Key = m.Groups.["key"].Value |}
            else
                None
        | _ -> None

    // Define the active pattern
    let (|KeyValue|_|) (input: YAMLASTElement) =
        match input with
        | Line s ->
            let m = Regex.Match(s, KeyValuePattern)
            if m.Success then 
                let v: string =
                    m.Groups.["value"].Value 
                Some {| Value = v; Key = m.Groups.["key"].Value |}
            else
                None
        | _ -> None

    // Define the active pattern
    let (|YamlValue|_|) (input: YAMLASTElement) =
        match input with
        | Line s ->
            let m = Regex.Match(s, ValuePattern)
            if m.Success then 
                let comment: int option = 
                    let v = m.Groups.["comment"].Value
                    if v = "" then None else Some (int v)
                let v: string =
                    m.Groups.["value"].Value 
                Some {| Comment = comment; Value = v |}
            else
                None
        | _ -> None

    // Define the active pattern
    let (|SequenceMinusOpener|_|) (input: YAMLASTElement) =
        match input with
        | Line s -> 
            let m = Regex.Match(s, SequenceMinusPattern) 
            if m.Success then 
                let v: string option =
                    let v = m.Groups.["value"].Value 
                    if v = "" then None else Some v
                Some {| Value = v |}
            else
                None
        | _ -> None

    let (|InlineSequence|_|) (input: YAMLASTElement) =
        match input with
        | Line s -> 
            let m = Regex.Match(s, InlineSequencePattern) 
            if m.Success then 
                let comment: int option = 
                    let v = m.Groups.["comment"].Value
                    if v = "" then None else Some (int v)
                let v: string =
                    m.Groups.["inlineSequence"].Value 
                Some {| Comment = comment; Value = v|}
            else
                None
        | _ -> None

    let (|SequenceSquareOpener|_|) (input: YAMLASTElement) =
        match input with
        | Line s -> 
            let m = Regex.Match(s, SequenceOpenerPattern) 
            if m.Success then 
                let comment: int option = 
                    let v = m.Groups.["comment"].Value
                    if v = "" then None else Some (int v)
                Some {| Comment = comment|}
            else
                None
        | _ -> None

    let (|SequenceSquareCloser|_|) (input: YAMLASTElement) =
        match input with
        | Line s -> 
            let m = Regex.Match(s, SequenceCloserPattern) 
            if m.Success then 
                let comment: int option = 
                    let v = m.Groups.["comment"].Value
                    if v = "" then None else Some (int v)
                Some {| Comment = comment|}
            else
                None
        | _ -> None

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
        | YAMLElement.SequenceElement i::rest -> loop rest (i::latestSeqItems) acc
        | anyElse::rest -> 
            if latestSeqItems.Length > 0 then
                let seq = YAMLElement.Sequence (List.rev latestSeqItems)
                loop rest [] (anyElse::seq::acc)
            else
                loop rest [] (anyElse::acc)
        | [] -> 
            if latestSeqItems.Length > 0 then
                let seq = YAMLElement.Sequence (List.rev latestSeqItems)
                seq::acc
            else
                acc
    loop eles [] []
    |> List.rev

let private tokenize (yamlList: YAMLASTElement list) (stringDict: Dictionary<int, string>) (commentDict: Dictionary<int, string>) =
    let rec loopRead (restlist: YAMLASTElement list) (acc: YAMLElement list) =
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
                    YAMLASTElement.Line v.Value.Value::yamlAstList
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
                    loopRead [YAMLASTElement.Line v.Value.Value] []
                )
            loopRead rest (current::acc)
        // [test1, test2, test] <c f=1/>
        | InlineSequence v::rest -> // create sequence
            printfn "[readList] Case3"
            // ensure inline comment is added on top of the sequence
            let container =
                let c = restoreCommentReplace commentDict v.Comment
                if c.IsSome then 
                    fun seq -> YAMLElement.Level [
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
                        loopRead [YAMLASTElement.Line value] []
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
                YAMLElement.Level [
                    if c1.IsSome then YAMLElement.Comment c1.Value
                    YAMLElement.Sequence [
                        for i in iList do
                            loopRead [i] []
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
                    YAMLContent(v.Key, ?comment=c),
                    loopRead yamlAstList []
                )
            loopRead rest (current::acc)
        // My Key: [My Value, Test2]
        | KeyValue v::rest -> // createKeyValue
            printfn "[readList] Case5"
            let current = 
                YAMLElement.Mapping (
                    YAMLContent(v.Key),
                    //reuse default parsing into SequenceElements
                    loopRead [YAMLASTElement.Line v.Value] []
                )
            loopRead rest (current::acc)
        // My Value <c f=1/>
        | YamlValue v::rest -> // createValue
            printfn "[readList] Case6"
            let raw = restoreStringReplace stringDict v.Value
            let c = restoreCommentReplace commentDict v.Comment
            let current = 
                YAMLElement.Value (
                    YAMLContent(raw, ?comment=c)
                )
            loopRead rest (current::acc)
        | [] -> 
            acc
            |> collectSequenceElements // collect sequence elements into a list
            |> YAMLElement.Level
        | anyElse -> failwithf "Unknown pattern: %A" anyElse
    loopRead yamlList []

let read (ast: YAMLAST) =
    match ast.AST with
    | Level lvl ->
        tokenize lvl ast.StringMap ast.CommentMap
    | _ -> failwith "Not a root!"