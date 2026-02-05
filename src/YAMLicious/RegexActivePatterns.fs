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

let (|FlowStyleArray|_|) (input: PreprocessorElement) =
    match input with
    | Line s -> 
        let m = Regex.Match(s, FlowStyleArrayPattern) 
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

// Backward compatibility alias
let (|InlineSequence|_|) = (|FlowStyleArray|_|)

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

let (|FlowStyleObject|_|) (input: PreprocessorElement) =
    match input with
    | Line s -> 
        let m = Regex.Match(s, FlowStyleObjectPattern) 
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

// Backward compatibility alias
let (|InlineJSON|_|) = (|FlowStyleObject|_|)

let (|FlowStyleObjectOpener|_|) (input: PreprocessorElement) =
    match input with
    | Line s -> 
        let m = Regex.Match(s, FlowStyleObjectOpenerPattern) 
        if m.Success then 
            let comment: int option = 
                let v = m.Groups.["comment"].Value
                if v = "" then None else Some (int v)
            Some {| Key = m.Groups.["key"].Value; Comment = comment|}
        else
            None
    | _ -> None

// Backward compatibility alias
let (|JSONKeyOpener|_|) = (|FlowStyleObjectOpener|_|)

let (|FlowStyleObjectCloser|_|) (input: PreprocessorElement) =
    match input with
    | Line s -> 
        let m = Regex.Match(s, FlowStyleObjectCloserPattern) 
        if m.Success then 
            let comment: int option = 
                let v = m.Groups.["comment"].Value
                if v = "" then None else Some (int v)
            Some {| Comment = comment|}
        else
            None
    | _ -> None

// Backward compatibility alias
let (|JSONCloser|_|) = (|FlowStyleObjectCloser|_|)

let (|SchemaNamespace|_|) (input: PreprocessorElement) =
    match input with
    | Line s -> 
        let m = Regex.Match(s, SchemaNamespacePattern) 
        if m.Success then 
            Some {| Key = m.Groups.["key"].Value|}
        else
            None
    | _ -> None

let (|DocumentEnd|_|) (input: PreprocessorElement) =
    match input with
    | Line s when s.TrimStart().StartsWith("...") -> Some ()
    | _ -> None

let (|WithAnchor|_|) (input: string) =
    let m = Regex.Match(input, AnchorPattern)
    if m.Success then Some m.Groups.["anchor"].Value
    else None

let (|AliasNode|_|) (input: PreprocessorElement) =
    match input with
    | Line s ->
        let m = Regex.Match(s.Trim(), AliasPattern)
        if m.Success then Some m.Groups.["alias"].Value
        else None
    | _ -> None

let (|VerbatimTag|_|) (input: string) =
    let m = Regex.Match(input, VerbatimTagPattern)
    if m.Success then Some m.Groups.["tag"].Value
    else None

let (|ExplicitKey|_|) (input: PreprocessorElement) =
    match input with
    | Line s when s.TrimStart().StartsWith("?") -> 
        let value = s.TrimStart().Substring(1).Trim()
        Some (if value = "" then None else Some value)
    | _ -> None

let (|ExplicitValue|_|) (input: PreprocessorElement) =
    match input with
    | Line s when s.TrimStart().StartsWith(":") ->
        let v = s.TrimStart().Substring(1).Trim()
        Some v
    | _ -> None
