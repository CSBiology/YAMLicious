module YAMLicious.RegexActivePatterns

open System.Text.RegularExpressions
open YAMLiciousTypes
open Preprocessing
open Regex

let private trimForPattern (s: string) = s.TrimStart()

let private leadingIndent (s: string) =
    s.Length - (s.TrimStart().Length)

// Define the active pattern
let (|Key|_|) (input: PreprocessorElement) =
    match input with
    | Line s ->
        let line = trimForPattern s
        let m = Regex.Match(line, KeyPattern)
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
        let line = trimForPattern s
        let m = Regex.Match(line, KeyValuePattern)
        if m.Success then 
            let v: string =
                m.Groups.["value"].Value.Trim() 
            Some {| Value = v; Key = m.Groups.["key"].Value; Indent = leadingIndent s |}
        else
            None
    | _ -> None

// Define the active pattern
let (|YamlValue|_|) (input: PreprocessorElement) =
    match input with
    | Line s ->
        let line = trimForPattern s
        let m = Regex.Match(line, ValuePattern)
        if m.Success then 
            let comment: int option = 
                let v = m.Groups.["comment"].Value
                if v = "" then None else Some (int v)
            let v: string =
                m.Groups.["value"].Value.Trim()
            Some {| Comment = comment; Value = v; Indent = leadingIndent s |}
        else
            None
    | _ -> None

// Define the active pattern
let (|YamlComment|_|) (input: PreprocessorElement) =
    match input with
    | Line s ->
        let line = trimForPattern s
        let m = Regex.Match(line, LineCommentPattern)
        if m.Success then 
            Some {| Comment = m.Groups.["comment"].Value |> int|}
        else
            None
    | _ -> None

// Define the active pattern
let (|SequenceMinusOpener|_|) (input: PreprocessorElement) =
    match input with
    | Line s -> 
        let line = trimForPattern s
        let m = Regex.Match(line, SequenceMinusPattern) 
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
        let line = trimForPattern s
        let m = Regex.Match(line, FlowStyleArrayPattern) 
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
        let line = trimForPattern s
        let m = Regex.Match(line, SequenceOpenerPattern) 
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
        let line = trimForPattern s
        let m = Regex.Match(line, SequenceCloserPattern) 
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
        let line = trimForPattern s
        let m = Regex.Match(line, FlowStyleObjectPattern) 
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
        let line = trimForPattern s
        let m = Regex.Match(line, FlowStyleObjectOpenerPattern) 
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
        let line = trimForPattern s
        let m = Regex.Match(line, FlowStyleObjectCloserPattern) 
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
        let line = trimForPattern s
        let m = Regex.Match(line, SchemaNamespacePattern) 
        if m.Success then 
            Some {| Key = m.Groups.["key"].Value|}
        else
            None
    | _ -> None

let (|DocumentEnd|_|) (input: PreprocessorElement) =
    match input with
    | Line s when isDocumentEnd s -> Some ()
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
    | Line s ->
        let line = trimForPattern s
        if line.StartsWith("?") then
            let value = line.Substring(1).Trim()
            Some (if value = "" then None else Some value)
        else
            None
    | _ -> None

let (|ExplicitValue|_|) (input: PreprocessorElement) =
    match input with
    | Line s ->
        let line = trimForPattern s
        if line.StartsWith(":") then
            let v = line.Substring(1).Trim()
            Some v
        else
            None
    | _ -> None
