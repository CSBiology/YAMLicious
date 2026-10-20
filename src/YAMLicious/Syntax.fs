module YAMLicious.Syntax

open System
open System.Text
open YAMLicious.YAMLiciousTypes

module Line =

    let normalizeNewlines (s: string) =
        s.Replace("\r\n", "\n").Replace("\r", "\n")

    let countLeadingSpaces (line: string) =
        line |> Seq.takeWhile (fun c -> c = ' ') |> Seq.length

    let isBlank (line: string) =
        line.Trim() = ""

module CommentMarker =

    /// YAML 1.2.2 [s-b-comment]: `#` starts a comment only at the start of a line or after
    /// whitespace, so `tool.cwl#main` and `C#` stay part of the scalar.
    let startsAt (s: string) (index: int) =
        index = 0
        ||
        match s.[index - 1] with
        | ' ' | '\t' | '\n' -> true
        | _ -> false

module Placeholder =

    let private tryParseInt (s: string) =
        match Int32.TryParse s with
        | true, value -> Some value
        | false, _ -> None

    let tryParseComment (line: string) =
        let t = line.Trim()
        if t.StartsWith("<c f=") && t.EndsWith("/>") then
            let value = t.Substring(5, t.Length - 7)
            if value |> Seq.forall Char.IsDigit then
                tryParseInt value
            else
                None
        else
            None

    let isCommentOnlyLine (line: string) =
        tryParseComment line |> Option.isSome

    let splitTrailingComment (s: string) =
        let trimmed = s.TrimEnd()
        let start = trimmed.LastIndexOf("<c f=")
        if start >= 0 && trimmed.EndsWith("/>") then
            let valueStart = start + 5
            let valueLength = trimmed.Length - valueStart - 2
            if valueLength >= 1 then
                let value = trimmed.Substring(valueStart, valueLength)
                if value |> Seq.forall Char.IsDigit then
                    trimmed.Substring(0, start).TrimEnd(), tryParseInt value
                else
                    trimmed, None
            else
                trimmed, None
        else
            trimmed, None

module Document =

    let isMarker (marker: string) (line: string) =
        let trimmed = line.TrimStart()
        if not (trimmed.StartsWith(marker)) then
            false
        elif trimmed.Length = marker.Length then
            true
        else
            Char.IsWhiteSpace(trimmed.[marker.Length])

    let isStart line = isMarker "---" line

    let isEnd line = isMarker "..." line

    let isTopLevelMarker (markerCheck: string -> bool) (line: string) =
        Line.countLeadingSpaces line = 0 && markerCheck line

    let tryInlineContentAfterStartMarker (line: string) =
        let trimmed = line.TrimStart()
        if not (trimmed.StartsWith("---")) then
            None
        else
            let rest = trimmed.Substring(3).TrimStart()
            if String.IsNullOrWhiteSpace(rest) || rest.StartsWith("#") then
                None
            else
                Some rest

    let isDirectivePreludeLine (line: string) =
        let t = line.TrimStart()
        t = "" || t.StartsWith("%") || t.StartsWith("#")

    let isDirectivePreludeOnly (lines: string list) =
        let hasDirective =
            lines |> List.exists (fun line -> line.TrimStart().StartsWith("%"))

        hasDirective && (lines |> List.forall isDirectivePreludeLine)

module BlockScalar =

    type Header =
        { Style: BlockScalarStyle
          Indent: int option
          Chomp: ChompingMode }

    let parseHeader (header: string) =
        let h = header.Trim()
        if String.IsNullOrWhiteSpace h then
            None
        else
            let style =
                match h.[0] with
                | '|' -> Some BlockScalarStyle.Literal
                | '>' -> Some BlockScalarStyle.Folded
                | _ -> None

            match style with
            | None -> None
            | Some style ->
                let mutable indent: int option = None
                let mutable chomp = ChompingMode.Clip
                let mutable valid = true

                for i in 1 .. h.Length - 1 do
                    match h.[i] with
                    | c when c >= '1' && c <= '9' ->
                        if indent.IsSome then
                            valid <- false
                        else
                            indent <- Some (int (string c))
                    | '-' ->
                        if chomp <> ChompingMode.Clip then
                            valid <- false
                        else
                            chomp <- ChompingMode.Strip
                    | '+' ->
                        if chomp <> ChompingMode.Clip then
                            valid <- false
                        else
                            chomp <- ChompingMode.Keep
                    | _ ->
                        valid <- false

                if valid then
                    Some { Style = style; Indent = indent; Chomp = chomp }
                else
                    None

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

    let headerTokenFromLine (line: string) =
        let trimmed = line.TrimStart()
        let afterDash =
            if trimmed.StartsWith("- ") then
                trimmed.Substring(2).TrimStart()
            else
                trimmed

        let candidate =
            let idx = afterDash.IndexOf(':')
            if idx >= 0 then afterDash.Substring(idx + 1).TrimStart() else afterDash

        stripProperties candidate
        |> fun s -> s.Split([|' '; '\t'|], StringSplitOptions.RemoveEmptyEntries)
        |> Array.tryHead

    /// The header a line opens, such as `|2-` in `key: |2-`. The line must not contain comment text,
    /// because a `: |` inside a comment would look like a header. A comment placeholder is fine.
    let tryParseHeaderLine (line: string) =
        headerTokenFromLine line |> Option.bind parseHeader

    /// True when a value, as written after a key or a sequence dash, starts a block scalar.
    /// Properties and a trailing comment placeholder do not count.
    let isHeaderValue (value: string) =
        let withoutComment, _ = Placeholder.splitTrailingComment value
        let indicator = stripProperties withoutComment
        indicator.StartsWith("|") || indicator.StartsWith(">")

    /// YAML 1.2.2 8.1.1.1: the indentation of the content lines of a block scalar. An indentation
    /// indicator counts from the header's indentation. Without one, the first nonblank line sets it.
    /// The content is always indented deeper than the header, so a first nonblank line at or below
    /// the header means the scalar has no content.
    let contentIndent (headerIndent: int) (indicator: int option) (followingLines: string seq) =
        match indicator with
        | Some spaces -> headerIndent + spaces
        | None ->
            match followingLines |> Seq.tryFind (Line.isBlank >> not) with
            | Some first -> max (headerIndent + 1) (Line.countLeadingSpaces first)
            | None -> headerIndent + 1

    /// Blank lines and lines indented at least as deep as the content indentation form the body.
    let isBodyLine (indent: int) (line: string) =
        Line.isBlank line || Line.countLeadingSpaces line >= indent

    let applyChomping (chomp: ChompingMode) (content: string) =
        match chomp with
        | ChompingMode.Strip -> content.TrimEnd([|'\r'; '\n'|])
        | ChompingMode.Keep -> content
        | ChompingMode.Clip ->
            let trimmed = content.TrimEnd([|'\r'; '\n'|])
            if content.Length > trimmed.Length then trimmed + "\n" else trimmed

    let stripIndent (indent: int) (line: string) =
        if line.Trim() = "" then
            ""
        else
            let available = Line.countLeadingSpaces line
            line.Substring(min indent available)

    let foldLines (lines: string list) =
        let isMoreIndented (line: string) =
            line.StartsWith(" ") || line.StartsWith("\t")

        let arr = lines |> List.toArray
        let sb = StringBuilder()

        for i in 0 .. arr.Length - 1 do
            let line = arr.[i]
            if line.Trim() = "" then
                sb.Append('\n') |> ignore
            else
                sb.Append(line) |> ignore
                if i < arr.Length - 1 then
                    let next = arr.[i + 1]
                    if next.Trim() = "" then
                        sb.Append('\n') |> ignore
                    elif isMoreIndented line || isMoreIndented next then
                        sb.Append('\n') |> ignore
                    else
                        sb.Append(' ') |> ignore

        sb.ToString()

    let deindentLines (headerIndent: int) (explicitIndent: int option) (lines: string list) =
        let indent = contentIndent headerIndent explicitIndent lines
        lines |> List.map (stripIndent indent)

    let buildContent (style: BlockScalarStyle) (chomp: ChompingMode) (headerIndent: int) (explicitIndent: int option) (lines: string list) =
        let deindentedLines = deindentLines headerIndent explicitIndent lines
        let content =
            match style with
            | BlockScalarStyle.Literal ->
                if List.isEmpty deindentedLines then "" else String.Join("\n", deindentedLines) + "\n"
            | BlockScalarStyle.Folded ->
                let folded = foldLines deindentedLines
                if folded.EndsWith("\n") then folded else folded + "\n"

        applyChomping chomp content

module FlowTokens =

    type Token =
        | OpenBrace
        | CloseBrace
        | OpenBracket
        | CloseBracket
        | Colon
        | Comma
        | String of string
        | EOF

    let tokenize (input: string) : Token list =
        let rec parseString (chars: char list) (acc: char list) : char list * char list =
            match chars with
            | [] -> (List.rev acc, [])
            | '"'::rest -> (List.rev acc, rest)
            | '\\'::c::rest -> parseString rest (c::'\\'::acc)
            | c::rest -> parseString rest (c::acc)

        let consumePlaceholder (cs: char list) (acc: char list) =
            let rec loop (cs: char list) (acc: char list) =
                match cs with
                | '/'::'>'::rest -> (List.rev ('>'::'/'::acc), rest)
                | c::rest -> loop rest (c::acc)
                | [] -> (List.rev acc, [])
            loop cs acc

        let rec tokenizeChars (chars: char list) (acc: Token list) : Token list =
            match chars with
            | [] -> List.rev (Token.EOF::acc)
            | ' '::rest | '\n'::rest | '\r'::rest | '\t'::rest -> tokenizeChars rest acc
            | '{'::rest -> tokenizeChars rest (Token.OpenBrace::acc)
            | '}'::rest -> tokenizeChars rest (Token.CloseBrace::acc)
            | '['::rest -> tokenizeChars rest (Token.OpenBracket::acc)
            | ']'::rest -> tokenizeChars rest (Token.CloseBracket::acc)
            | ':'::rest -> tokenizeChars rest (Token.Colon::acc)
            | ','::rest -> tokenizeChars rest (Token.Comma::acc)
            | '"'::rest ->
                let (str, remaining) = parseString rest []
                let strValue = System.String(Array.ofList str)
                tokenizeChars remaining (Token.String strValue::acc)
            | chars ->
                let rec parseUnquoted (cs: char list) (acc: char list) =
                    match cs with
                    | [] -> (List.rev acc, [])
                    | '<'::'s'::' '::'f'::'='::rest ->
                        let (placeholder, remaining) = consumePlaceholder rest ['=';'f';' ';'s';'<']
                        let (restStr, final) = parseUnquoted remaining []
                        (placeholder @ restStr, final)
                    | '<'::'c'::' '::'f'::'='::rest ->
                        let (placeholder, remaining) = consumePlaceholder rest ['=';'f';' ';'c';'<']
                        let (restStr, final) = parseUnquoted remaining []
                        (placeholder @ restStr, final)
                    | c::rest when c = '{' || c = '}' || c = '[' || c = ']' || c = ':' || c = ',' || c = '\n' || c = '\r' || c = '\t' ->
                        (List.rev acc, cs)
                    | ' '::rest when acc.IsEmpty ->
                        parseUnquoted rest acc
                    | c::rest -> parseUnquoted rest (c::acc)

                let (str, remaining) = parseUnquoted chars []
                if str.IsEmpty then
                    tokenizeChars remaining acc
                else
                    let strValue = System.String(Array.ofList str)
                    tokenizeChars remaining (Token.String strValue::acc)

        tokenizeChars (List.ofSeq input) []
