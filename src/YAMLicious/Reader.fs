module YAMLicious.Reader

open PatternMatcher
open AST
open System.Text.RegularExpressions

module RegexActivePatterns =
    open System.Text.RegularExpressions

    // Define the active pattern
    let (|Key|_|) (input: YAMLAST) =
        match input with
        | Line s ->
            let m = Regex.Match(s, KeyPattern)
            if m.Success then 
                let comment: string option = 
                    let v = m.Groups.["comment"].Value
                    if v = "" then None else Some v
                Some {| Comment = comment; Key = m.Groups.["key"].Value |}
            else
                None
        | _ -> None

    // Define the active pattern
    let (|KeyValue|_|) (input: YAMLAST) =
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
    let (|YamlValue|_|) (input: YAMLAST) =
        match input with
        | Line s ->
            let m = Regex.Match(s, ValuePattern)
            if m.Success then 
                let comment: string option = 
                    let v = m.Groups.["comment"].Value
                    if v = "" then None else Some v
                let v: string =
                    m.Groups.["value"].Value 
                Some {| Comment = comment; Value = v |}
            else
                None
        | _ -> None

    // Define the active pattern
    let (|SequenceMinusOpener|_|) (input: YAMLAST) =
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

    let (|InlineSequence|_|) (input: YAMLAST) =
        match input with
        | Line s -> 
            let m = Regex.Match(s, InlineSequencePattern) 
            if m.Success then 
                let comment: string option = 
                    let v = m.Groups.["comment"].Value
                    if v = "" then None else Some v
                let v: string =
                    m.Groups.["inlineSequence"].Value 
                Some {| Comment = comment; Value = v|}
            else
                None
        | _ -> None

    let (|SequenceSquareOpener|_|) (input: YAMLAST) =
        match input with
        | Line s -> 
            let m = Regex.Match(s, SequenceOpenerPattern) 
            if m.Success then 
                let comment: string option = 
                    let v = m.Groups.["comment"].Value
                    if v = "" then None else Some v
                Some {| Comment = comment|}
            else
                None
        | _ -> None

    let (|SequenceSquareCloser|_|) (input: YAMLAST) =
        match input with
        | Line s -> 
            let m = Regex.Match(s, SequenceCloserPattern) 
            if m.Success then 
                let comment: string option = 
                    let v = m.Groups.["comment"].Value
                    if v = "" then None else Some v
                Some {| Comment = comment|}
            else
                None
        | _ -> None

open RegexActivePatterns
open YAMLiciousTypes

/// Minus opener Sequence elements are difficult to collect with our logic. So whenever we return a list of YAMLElements we
/// should check for SequenceElements and collect them into a single Sequence
let private collectSequenceElements (eles: YAMLElement list) =
    let rec loop (latestSeqItems: YAMLElement list) (acc: YAMLElement list) =
        match acc with
        | YAMLElement.SequenceElement i::rest -> loop (i::latestSeqItems) rest
        | anyElse::rest -> 
            if latestSeqItems.Length > 0 then
                let seq = YAMLElement.Sequence (List.rev latestSeqItems)
                loop [] (anyElse::seq::rest)
            else
                loop [] (anyElse::rest)
        | [] -> 
            acc
    loop eles []

let private readList (yamlList: YAMLAST list) =
    let rec loopRead (restlist: YAMLAST list) (acc: YAMLElement list) =
        match restlist with
        // Example1: 
        // - My Value 1
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
                    YAMLAST.Line v.Value.Value::yamlAstList
                else
                    yamlAstList
            let current =
                YAMLElement.SequenceElement (
                    YAMLElement.List [
                        loopRead objectList []
                    ]
                )
            loopRead rest (current::acc)
        | SequenceMinusOpener v::rest -> //create/appendSequenceElement
            printfn "[readList] Case2"
            let current =
                YAMLElement.SequenceElement (
                    loopRead [YAMLAST.Line v.Value.Value] []
                )
            loopRead rest (current::acc)
        | InlineSequence v::rest -> // create sequence
            printfn "[readList] Case3"
            // ensure inline comment is added on top of the sequence
            let container =
                if v.Comment.IsSome then 
                    fun seq -> YAMLElement.List [
                        YAMLElement.Comment v.Comment.Value
                        YAMLElement.Sequence seq
                    ]
                else
                    fun seq -> YAMLElement.Sequence seq
            // split inline sequence by delimiter, then reuse default parsing into SequenceElements
            let split = v.Value.Split([|SequenceSquareDelimiter|], System.StringSplitOptions.RemoveEmptyEntries)
            let current =
                container [
                    for value in split do
                        loopRead [YAMLAST.Line value] []
                        |> YAMLElement.SequenceElement
                ]
            loopRead rest (current::acc)
        | Key v::Intendation yamlAstList::rest -> //createObject
            printfn "[readList] Case4"
            let current = 
                YAMLElement.Mapping (
                    YAMLContent(v.Key, ?comment=v.Comment),
                    loopRead yamlAstList []
                )
            loopRead rest (current::acc)
        | KeyValue v::rest -> // createKeyValue
            printfn "[readList] Case5"
            let current = 
                YAMLElement.Mapping (
                    YAMLContent(v.Key),
                    //reuse default parsing into SequenceElements
                    loopRead [YAMLAST.Line v.Value] []
                )
            loopRead rest (current::acc)
        | YamlValue v::rest -> // createValue
            printfn "[readList] Case6"
            let current = 
                YAMLElement.Value (
                    YAMLContent(v.Value, ?comment=v.Comment)
                )
            loopRead rest (current::acc)
        | [] -> 
            acc
            |> collectSequenceElements // collect sequence elements into a list
            |> YAMLElement.List
        | anyElse -> failwithf "Unknown pattern: %A" anyElse
    loopRead yamlList []

let read (ast: YAMLAST) =
    match ast with
    | Level lvl ->
        readList lvl
    | _ -> failwith "Not a root!"
    // At this point we should propably insert comment- and string-replacements.
    |> fun x -> x