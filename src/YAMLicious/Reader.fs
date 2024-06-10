module YAMLicious.Reader

open PatternMatcher
open AST
open System.Text.RegularExpressions

module RegexActivePatterns =
    open System.Text.RegularExpressions

    // Define the active pattern
    let (|KeyValue|_|) (input: YAMLAST) =
        match input with
        | Line s ->
            let m = Regex.Match(s, KeyValuePattern)
            if m.Success then 
                let comment: string option = 
                    let v = m.Groups.["comment"].Value
                    if v = "" then None else Some v
                let v: string option =
                    let v = m.Groups.["value"].Value 
                    if v = "" then None else Some v
                Some {| Comment = comment; Value = v; Key = m.Groups.["key"].Value |}
            else
                None
        | _ -> None

    // Define the active pattern
    let (|SequenceMinusOpener|_|) (input: YAMLAST) =
        match input with
        | Line s -> 
            let m = Regex.Match(s, SequenceValuePattern) 
            if m.Success then 
                let comment: string option = 
                    let v = m.Groups.["comment"].Value
                    if v = "" then None else Some v
                let v: string option =
                    let v = m.Groups.["value"].Value 
                    if v = "" then None else Some v
                Some {| Comment = comment; Value = v|}
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
                let v: string option =
                    let v = m.Groups.["inlineSequence"].Value 
                    if v = "" then None else Some v
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

let rec readList (restlist: YAMLAST list) =
  match restlist with
  | SequenceMinusOpener v::Intendation yamlAstList::rest -> () //createSequenceElement
  | SequenceMinusOpener v::rest -> () //createSequenceElement
  | InlineSequence v::rest -> () //createSequence
  | KeyValue v::Intendation yamlAstList::rest -> () //createObject
  | KeyValue v::rest -> () // createKeyValue
  | x::xs -> readNode x :: readList xs

let read (ast: YAMLAST) =
    let rec readNode (node: YAMLAST) =
      match node with
      | Level lList ->
        ()
      | Intendation iList ->
        ()
      | Line str ->
        ()

    readNode ast.Root