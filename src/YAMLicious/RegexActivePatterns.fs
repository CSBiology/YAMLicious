module YAMLicious.RegexActivePatterns

open System.Text.RegularExpressions
open AST
open PatternMatcher

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
let (|YamlComment|_|) (input: YAMLASTElement) =
    match input with
    | Line s ->
        let m = Regex.Match(s, CommentPattern)
        if m.Success then 
            Some {| Comment = m.Groups.["comment"].Value |> int|}
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