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

let private parseSingleQuotedSegment (s: string) (startIndex: int) =
    let content = new System.Text.StringBuilder()
    let mutable i = startIndex + 1
    let mutable closed = false
    while i < s.Length && not closed do
        let ch = s.[i]
        if ch = '\'' then
            if i + 1 < s.Length && s.[i + 1] = '\'' then
                content.Append("''") |> ignore
                i <- i + 2
            else
                closed <- true
                i <- i + 1
        else
            content.Append(ch) |> ignore
            i <- i + 1
    closed, content.ToString(), i

let private parseDoubleQuotedSegment (s: string) (startIndex: int) =
    let content = new System.Text.StringBuilder()
    let mutable i = startIndex + 1
    let mutable closed = false
    let mutable escaped = false
    while i < s.Length && not closed do
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
    closed, content.ToString(), i

let private isTokenBoundaryStart (s: string) (quoteIndex: int) =
    let isInlineWhitespace c = c = ' ' || c = '\t'
    let isTokenSeparator c =
        c = ' '
        || c = '\t'
        || c = '\n'
        || c = '\r'
        || c = ':'
        || c = '-'
        || c = ','
        || c = '['
        || c = '{'
        || c = '?'
    let mutable i = quoteIndex - 1
    while i >= 0 && isInlineWhitespace s.[i] do
        i <- i - 1

    if i < 0 then
        true
    else
        match s.[i] with
        | '\n'
        | '\r'
        | ':'
        | '-'
        | ','
        | '['
        | '{'
        | '?' -> true
        | _ ->
            // Allow quoted scalars immediately following a tag/anchor token
            // (e.g. `!foo "bar"` or `&a "bar"`), while still rejecting plain
            // scalar content like `rock 'n' roll`.
            let mutable start = i
            while start >= 0 && not (isTokenSeparator s.[start]) do
                start <- start - 1
            let token = s.Substring(start + 1, i - start)
            token.StartsWith("!") || token.StartsWith("&")

let private isTokenBoundaryEnd (s: string) (nextIndex: int) =
    let isInlineWhitespace c = c = ' ' || c = '\t'
    let mutable i = nextIndex
    while i < s.Length && isInlineWhitespace s.[i] do
        i <- i + 1

    if i >= s.Length then
        true
    else
        match s.[i] with
        | '\n'
        | '\r'
        | '#'
        | ','
        | ']'
        | '}'
        | ':' -> true
        | _ -> false

let private replaceQuotedStrings (target: QuotedStringKind) (dict: Dictionary<int, StringMapEntry>) (protectedBlockScalarLines: Set<int>) (s: string) =
    let sb = new System.Text.StringBuilder(s.Length)
    let mutable i = 0
    let mutable n = nextStringIndex dict
    let mutable inComment = false
    let length = s.Length
    let mutable lineIndex = 0

    let appendPlaceholder (entry: StringMapEntry) =
        let currentN = n
        n <- n + 1
        dict.Add(currentN, entry)
        sb.Append(sprintf "<s f=%i/>" currentN) |> ignore

    let appendChar (c: char) =
        sb.Append(c) |> ignore
        if c = NewLineChar then
            lineIndex <- lineIndex + 1

    let appendText (text: string) =
        for c in text do
            appendChar c

    let advanceLineIndex (startIndex: int) (endIndex: int) =
        for j in startIndex .. endIndex - 1 do
            if s.[j] = NewLineChar then
                lineIndex <- lineIndex + 1

    while i < length do
        let c = s.[i]
        if protectedBlockScalarLines.Contains lineIndex then
            appendChar c
            i <- i + 1
            if c = NewLineChar then
                inComment <- false
        elif inComment then
            appendChar c
            i <- i + 1
            if c = NewLineChar then
                inComment <- false
        else
            match c with
            | '#' ->
                inComment <- true
                appendChar c
                i <- i + 1
            | '\'' when target = QuotedStringKind.SingleQuotedString ->
                if isTokenBoundaryStart s i then
                    let closed, rawContent, nextIndex = parseSingleQuotedSegment s i
                    if closed && isTokenBoundaryEnd s nextIndex then
                        let folded = foldSingleQuoted rawContent
                        appendPlaceholder { Value = folded; Kind = QuotedStringKind.SingleQuotedString }
                        advanceLineIndex i nextIndex
                    else
                        appendText (s.Substring(i, nextIndex - i))
                    i <- nextIndex
                else
                    appendChar c
                    i <- i + 1
            | '\'' ->
                // Preserve single-quoted segments untouched during double-quote pass.
                let _, _, nextIndex = parseSingleQuotedSegment s i
                appendText (s.Substring(i, nextIndex - i))
                i <- nextIndex
            | '"' when target = QuotedStringKind.DoubleQuotedString ->
                if isTokenBoundaryStart s i then
                    let closed, rawContent, nextIndex = parseDoubleQuotedSegment s i
                    if closed && isTokenBoundaryEnd s nextIndex then
                        appendPlaceholder { Value = rawContent; Kind = QuotedStringKind.DoubleQuotedString }
                        advanceLineIndex i nextIndex
                    else
                        appendText (s.Substring(i, nextIndex - i))
                    i <- nextIndex
                else
                    appendChar c
                    i <- i + 1
            | '"' ->
                // Preserve double-quoted segments untouched during single-quote pass.
                let _, _, nextIndex = parseDoubleQuotedSegment s i
                appendText (s.Substring(i, nextIndex - i))
                i <- nextIndex
            | _ ->
                appendChar c
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

let private countLeadingSpaces (line: string) =
    line |> Seq.takeWhile (fun c -> c = ' ') |> Seq.length

let private parseBlockScalarHeaderToken (token: string) =
    if String.IsNullOrWhiteSpace token then
        false
    else
        let t = token.Trim()
        if t.Length = 0 then
            false
        else
            let first = t.[0]
            if first <> '|' && first <> '>' then
                false
            else
                let mutable valid = true
                let mutable seenIndent = false
                let mutable seenChomp = false
                let mutable idx = 1
                while idx < t.Length && valid do
                    match t.[idx] with
                    | c when c >= '1' && c <= '9' ->
                        if seenIndent then valid <- false
                        else seenIndent <- true
                    | '-'
                    | '+' ->
                        if seenChomp then valid <- false
                        else seenChomp <- true
                    | _ ->
                        valid <- false
                    idx <- idx + 1
                valid

let private tryDetectBlockScalarHeaderIndent (line: string) =
    let stripProperties (s: string) =
        let rec loop (current: string) =
            let c = current.TrimStart()
            if c.StartsWith("&") then
                let idx = c.IndexOf(' ')
                if idx < 0 then "" else loop (c.Substring(idx + 1))
            elif c.StartsWith("!<") then
                let idx = c.IndexOf('>')
                if idx < 0 then c else loop (c.Substring(idx + 1))
            elif c.StartsWith("!") && not (c.StartsWith("|")) && not (c.StartsWith(">")) then
                let idx = c.IndexOf(' ')
                if idx < 0 then "" else loop (c.Substring(idx + 1))
            else
                c
        loop s

    let trimmed = line.TrimStart()
    let afterDash =
        if trimmed.StartsWith("- ") then
            trimmed.Substring(2).TrimStart()
        else
            trimmed
    let candidate =
        let idx = afterDash.IndexOf(':')
        if idx >= 0 then afterDash.Substring(idx + 1).TrimStart()
        else afterDash
    let withoutProps = stripProperties candidate
    let token =
        withoutProps.Split([|' '; '\t'|], StringSplitOptions.RemoveEmptyEntries)
        |> Array.tryHead

    match token with
    | Some t when parseBlockScalarHeaderToken t ->
        Some (countLeadingSpaces line)
    | _ ->
        None

let private detectBlockScalarContentLines (yamlString: string) =
    let normalized = yamlString.Replace("\r\n", "\n").Replace("\r", "\n")
    let lines = normalized.Split([|'\n'|], StringSplitOptions.None)
    let protectedLines = HashSet<int>()
    let mutable i = 0
    let mutable openBlockIndent: int option = None

    while i < lines.Length do
        match openBlockIndent with
        | Some headerIndent ->
            let line = lines.[i]
            if line.Trim() = "" || countLeadingSpaces line > headerIndent then
                protectedLines.Add(i) |> ignore
                i <- i + 1
            else
                openBlockIndent <- None
        | None ->
            match tryDetectBlockScalarHeaderIndent lines.[i] with
            | Some indent ->
                openBlockIndent <- Some indent
                i <- i + 1
            | None ->
                i <- i + 1

    protectedLines |> Seq.toList |> Set.ofList

let private stringCleanUpWithProtected (dict: Dictionary<int, StringMapEntry>) (protectedBlockScalarLines: Set<int>) (s: string) =
    replaceQuotedStrings QuotedStringKind.DoubleQuotedString dict protectedBlockScalarLines s

let private singleQuotedStringCleanUpWithProtected (dict: Dictionary<int, StringMapEntry>) (protectedBlockScalarLines: Set<int>) (s: string) =
    replaceQuotedStrings QuotedStringKind.SingleQuotedString dict protectedBlockScalarLines s

let stringCleanUp (dict: Dictionary<int, StringMapEntry>) (s: string) =
    stringCleanUpWithProtected dict Set.empty s

let singleQuotedStringCleanUp (dict: Dictionary<int, StringMapEntry>) (s: string) =
    singleQuotedStringCleanUpWithProtected dict Set.empty s

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

let private tryInlineContentAfterStartMarker (line: string) =
    let trimmed = line.TrimStart()
    if not (trimmed.StartsWith("---")) then
        None
    else
        let rest = trimmed.Substring(3).TrimStart()
        if String.IsNullOrWhiteSpace(rest) || rest.StartsWith("#") then
            None
        else
            Some rest

let pipeline (yamlString: string) =
    let stringMap = new Dictionary<int, StringMapEntry>()
    let commentMap = new Dictionary<int, string>()
    let normalizedContent = encodingCleanUp yamlString
    let protectedBlockScalarLines = detectBlockScalarContentLines normalizedContent
    let lines =
        normalizedContent
        |> singleQuotedStringCleanUpWithProtected stringMap protectedBlockScalarLines  // Handle single-quoted strings first
        |> stringCleanUpWithProtected stringMap protectedBlockScalarLines              // Then handle double-quoted strings
        |> commentCleanUp commentMap
        |> cut
    let hasDirectivePrelude =
        lines
        |> Array.tryFind (fun l ->
            let t = l.Trim()
            t <> "" && not (t.StartsWith("#"))
        )
        |> Option.map (fun line -> line.TrimStart().StartsWith("%"))
        |> Option.defaultValue false

    let directivePreludeLength =
        if hasDirectivePrelude then
            lines
            |> Array.takeWhile (fun l ->
                let t = l.TrimStart()
                t.StartsWith("%") || t.StartsWith("#") || t.Trim() = ""
            )
            |> Array.length
        else
            0

    let directiveLines = lines |> Array.take directivePreludeLength
    let contentLines = lines.[directivePreludeLength..]
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
            match tryInlineContentAfterStartMarker contentLines.[0] with
            | Some inlineContent ->
                Array.append [| inlineContent |] contentLines.[1..]
            | None ->
                contentLines.[1..]
        else
            contentLines
    {|StringMap = stringMap; CommentMap = commentMap; Lines = finalContentLines; YAMLVersion = yamlVersion; TagHandles = tagHandles|}
