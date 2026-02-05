module YAMLicious.Persil

open System
open System.Collections.Generic
open System.Text.RegularExpressions

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

let stringCleanUp (dict: Dictionary<int, string>) (s: string) =
    let mutable n = 0
    let regex = Regex(StringMatchPattern)
    let matcheval = new MatchEvaluator(fun m ->
        match m.Groups.["iscomment"].Success with
        | true ->
            m.Groups.["all"].Value
        | false ->
            let v = m.Groups.["stringValue"].Value
            let currentN = n
            n <- n + 1
            dict.Add(currentN, v)
            sprintf "<s f=%i/>" currentN
    )
    regex.Replace(s, matcheval)

let foldSingleQuoted (s: string) =
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

let singleQuotedStringCleanUp (dict: Dictionary<int, string>) (s: string) =
    let mutable n = 0
    let regex = Regex(SingleQuotedStringPattern)
    let matcheval = new MatchEvaluator(fun m ->
        match m.Groups.["iscomment"].Success with
        | true ->
            m.Groups.["all"].Value
        | false ->
            // Handle '' escape sequence (represents single quote) and folding
            let v = foldSingleQuoted m.Groups.["stringValue"].Value
            let currentN = n
            n <- n + 1
            dict.Add(currentN, v)
            sprintf "<s f=%i/>" currentN
    )
    regex.Replace(s, matcheval)

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
    yamlString.Split([|NewLineChar|], StringSplitOptions.RemoveEmptyEntries)

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

let pipeline (yamlString: string) =
    let stringMap = new Dictionary<int, string>()
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
        if contentLines.Length > 0 && contentLines.[0].Trim() = "---" then
            contentLines.[1..]
        else
            contentLines
    {|StringMap = stringMap; CommentMap = commentMap; Lines = finalContentLines; YAMLVersion = yamlVersion; TagHandles = tagHandles|}