module YAMLicious.RegexActivePatterns

open System.Text.RegularExpressions
open YAMLiciousTypes
open Preprocessing
open Regex

// Define the active pattern
let (|Key|_|) (input: PreprocessorElement) =
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
let (|KeyValue|_|) (input: PreprocessorElement) =
    match input with
    | Line s ->
        let m = Regex.Match(s, KeyValuePattern)
        if m.Success then 
            let v: string =
                m.Groups.["value"].Value.Trim() 
            Some {| Value = v; Key = m.Groups.["key"].Value |}
        else
            None
    | _ -> None

// Define the active pattern
let (|YamlValue|_|) (input: PreprocessorElement) =
    match input with
    | Line s ->
        let m = Regex.Match(s, ValuePattern)
        if m.Success then 
            let comment: int option = 
                let v = m.Groups.["comment"].Value
                if v = "" then None else Some (int v)
            let v: string =
                m.Groups.["value"].Value.Trim()
            Some {| Comment = comment; Value = v |}
        else
            None
    | _ -> None

// Define the active pattern
let (|YamlComment|_|) (input: PreprocessorElement) =
    match input with
    | Line s ->
        let m = Regex.Match(s, LineCommentPattern)
        if m.Success then 
            Some {| Comment = m.Groups.["comment"].Value |> int|}
        else
            None
    | _ -> None

// Define the active pattern
let (|SequenceMinusOpener|_|) (input: PreprocessorElement) =
    match input with
    | Line s -> 
        let m = Regex.Match(s, SequenceMinusPattern) 
        if m.Success then 
            let v: string option =
                let v = m.Groups.["value"].Value.Trim()
                if v = "" then None else Some v
            Some {| Value = v |}
        else
            None
    | _ -> None

let (|InlineSequence|_|) (input: PreprocessorElement) =
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

let (|SequenceSquareOpener|_|) (input: PreprocessorElement) =
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

let (|SequenceSquareCloser|_|) (input: PreprocessorElement) =
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

let (|SchemaNamespace|_|) (input: PreprocessorElement) =
    match input with
    | Line s -> 
        let m = Regex.Match(s, SchemaNamespacePattern) 
        if m.Success then 
            Some {| Key = m.Groups.["key"].Value|}
        else
            None
    | _ -> None