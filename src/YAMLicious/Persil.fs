module YAMLicious.Persil

open System
open System.Collections.Generic
open System.Text.RegularExpressions
open YAMLiciousTypes

// Patterns:
// https://regex101.com/r/V0AkIH/1

[<Literal>]
let StringMatchPattern = 
    // (?<iscomment>#.*?)? checks if inside comment. if group is found do not parse as string
    #if FABLE_COMPILER_PYTHON
    "(?P<all>(?P<iscomment>#.*?)?\"(?P<stringValue>.+?)\\\")"
    #endif
    #if FABLE_COMPILER_JAVASCRIPT || FABLE_COMPILER_TYPESCRIPT
    "(?<all>(?<iscomment>#.*?)?\"(?<stringValue>.+?)\")" // fable sets /gu for js regex (unicode, not required to escape ")
    #endif
    #if !FABLE_COMPILER
    "(?<all>(?<iscomment>#.*?)?\\\"(?<stringValue>.+?)\\\")" // \\\" --> escaped \"
    #endif

[<Literal>]
let SingleQuotedStringPattern = 
    // Single-quoted strings: '' represents an escaped single quote
    #if FABLE_COMPILER_PYTHON
    "(?P<all>(?P<iscomment>#.*?)?'(?P<stringValue>(?:[^']|'')*)')"
    #endif
    #if FABLE_COMPILER_JAVASCRIPT || FABLE_COMPILER_TYPESCRIPT
    "(?<all>(?<iscomment>#.*?)?'(?<stringValue>(?:[^']|'')*)')" 
    #endif
    #if !FABLE_COMPILER
    "(?<all>(?<iscomment>#.*?)?'(?<stringValue>(?:[^']|'')*)')" 
    #endif

[<Literal>]
let CommentMatchPattern = 
    #if FABLE_COMPILER_PYTHON
    "#(?P<comment>.*)"
    #else
    "#(?<comment>.*)"
    #endif

[<Literal>]
let NewLineChar = '\n'

let private nextStringIndex (dict: Dictionary<int, StringMapEntry>) =
    if dict.Count = 0 then 0
    else (dict.Keys |> Seq.max) + 1

let private foldSingleQuoted (s: string) =
    let lines = s.Split([|NewLineChar|])
    let sb = new System.Text.StringBuilder()
    for i in 0 .. lines.Length - 1 do
        let mutable line = lines.[i]
        if i > 0 then line <- line.TrimStart()
        if i < lines.Length - 1 then line <- line.TrimEnd()

        if line.Length = 0 then
            sb.Append(NewLineChar) |> ignore
        else
            if sb.Length > 0 && sb.[sb.Length - 1] <> NewLineChar then
                    sb.Append(" ") |> ignore
            sb.Append(line) |> ignore
    sb.ToString().Replace("''", "'")

let private replaceQuotedStrings (target: QuotedStringKind) (dict: Dictionary<int, StringMapEntry>) (s: string) =
    let sb = new System.Text.StringBuilder(s.Length)
    let mutable i = 0
    let mutable n = nextStringIndex dict
    let mutable inComment = false
    let length = s.Length

    let appendPlaceholder (entry: StringMapEntry) =
        let currentN = n
        n <- n + 1
        dict.Add(currentN, entry)
        sb.Append(sprintf "<s f=%i/>" currentN) |> ignore

    while i < length do
        let c = s.[i]
        if inComment then
            sb.Append(c) |> ignore
            i <- i + 1
            if c = NewLineChar then
                inComment <- false
        else
            match c with
            | '#' ->
                inComment <- true
                sb.Append(c) |> ignore
                i <- i + 1
            | '\'' when target = QuotedStringKind.SingleQuotedString ->
                i <- i + 1
                let content = new System.Text.StringBuilder()
                let mutable closed = false
                while i < length && not closed do
                    let ch = s.[i]
                    if ch = '\'' then
                        if i + 1 < length && s.[i + 1] = '\'' then
                            content.Append("''") |> ignore
                            i <- i + 2
                        else
                            closed <- true
                            i <- i + 1
                    else
                        content.Append(ch) |> ignore
                        i <- i + 1

                if closed then
                    let folded = foldSingleQuoted (content.ToString())
                    appendPlaceholder { Value = folded; Kind = QuotedStringKind.SingleQuotedString }
                else
                    sb.Append('\'').Append(content.ToString()) |> ignore
            | '"' when target = QuotedStringKind.DoubleQuotedString ->
                i <- i + 1
                let content = new System.Text.StringBuilder()
                let mutable closed = false
                let mutable escaped = false
                while i < length && not closed do
                    let ch = s.[i]
                    if escaped then
                        content.Append(ch) |> ignore
                        escaped <- false
                        i <- i + 1
                    else
                        match ch with
                        | '\\' ->
                            content.Append(ch) |> ignore
                            escaped <- true
                            i <- i + 1
                        | '"' ->
                            closed <- true
                            i <- i + 1
                        | _ ->
                            content.Append(ch) |> ignore
                            i <- i + 1

                if closed then
                    appendPlaceholder { Value = content.ToString(); Kind = QuotedStringKind.DoubleQuotedString }
                else
                    sb.Append('"').Append(content.ToString()) |> ignore
            | _ ->
                sb.Append(c) |> ignore
                i <- i + 1

    sb.ToString()

let encodingCleanUp (s: string) =
    //let newLineChars = "\f\u0085\u2028\u2029" |> Array.ofSeq
    let s1 = s.Replace("\r\n", string NewLineChar)
    //let mutable index = s1.IndexOfAny(newLineChars)
    //if index = -1 then s1
    //else
    //    let sb = new System.Text.StringBuilder(s1)
    //    while index <> -1 do
    //        sb.Chars(index) <- replacementChar
    //        index <- s1.IndexOfAny(newLineChars, index + 1)
    //    sb.ToString()
    s1

let stringCleanUp (dict: Dictionary<int, StringMapEntry>) (s: string) =
    replaceQuotedStrings QuotedStringKind.DoubleQuotedString dict s

let singleQuotedStringCleanUp (dict: Dictionary<int, StringMapEntry>) (s: string) =
    replaceQuotedStrings QuotedStringKind.SingleQuotedString dict s

let commentCleanUp (dict: Dictionary<int, string>) (s: string) =
    let mutable n = 0
    let regex = Regex(CommentMatchPattern)
    let matcheval = new MatchEvaluator(fun m ->
        let v = m.Groups.["comment"].Value
        let currentN = n
        n <- n + 1
        dict.Add(currentN, v)
        sprintf "<c f=%i/>" currentN
    )
    regex.Replace(s, matcheval)

let cut(yamlString: string) =
    let lines =
        yamlString.Split([|NewLineChar|], StringSplitOptions.None)
        |> Array.toList
    let withoutLeading =
        match lines with
        | ""::rest -> rest
        | _ -> lines
    let withoutTrailing =
        match List.rev withoutLeading with
        | ""::rest -> List.rev rest
        | _ -> withoutLeading
    withoutTrailing |> List.toArray

let parseYAMLDirective (line: string) : YAMLiciousTypes.YAMLDirective option =
    let m = Regex.Match(line.Trim(), "^%YAML\s+(\d+)\.(\d+)")
    if m.Success then
        Some { Major = int m.Groups.[1].Value; Minor = int m.Groups.[2].Value }
    else None

let parseTagDirective (line: string) : (string * string) option =
    // Handle %TAG !handle! prefix
    let m = Regex.Match(line.Trim(), "^%TAG\s+(\S+)\s+(\S+)")
    if m.Success then
        Some (m.Groups.[1].Value, m.Groups.[2].Value)
    else None

let private isDocumentMarker (marker: string) (line: string) =
    let trimmed = line.TrimStart()
    if not (trimmed.StartsWith(marker)) then
        false
    elif trimmed.Length = marker.Length then
        true
    else
        let next = trimmed.[marker.Length]
        Char.IsWhiteSpace(next)

let pipeline (yamlString: string) =
    let stringMap = new Dictionary<int, StringMapEntry>()
    let commentMap = new Dictionary<int, string>()
    let lines =
        encodingCleanUp yamlString
        |> singleQuotedStringCleanUp stringMap  // Handle single-quoted strings first
        |> stringCleanUp stringMap              // Then handle double-quoted strings
        |> commentCleanUp commentMap
        |> cut
    let directiveLines = lines |> Array.takeWhile (fun l -> l.TrimStart().StartsWith("%"))
    let contentLines = lines.[directiveLines.Length..]
    let mutable yamlVersion = None
    let mutable tagHandles = 
        Map.empty 
        |> Map.add "!" "!" 
        |> Map.add "!!" "tag:yaml.org,2002:"
    for line in directiveLines do
        match parseYAMLDirective line with
        | Some v -> 
            if yamlVersion.IsSome then failwith "Duplicate YAML directive"
            yamlVersion <- Some v
        | None ->
            match parseTagDirective line with
            | Some (h, t) -> tagHandles <- Map.add h t tagHandles
            | None -> ()
            
    let finalContentLines =
        if contentLines.Length > 0 && isDocumentMarker "---" contentLines.[0] then
            contentLines.[1..]
        else
            contentLines
    {|StringMap = stringMap; CommentMap = commentMap; Lines = finalContentLines; YAMLVersion = yamlVersion; TagHandles = tagHandles|}
