module YAMLicious.Syntax

open System
open System.Text
open YAMLicious.YAMLiciousTypes

module Line =

    let normalizeNewlines (s: string) =
        s.Replace("\r\n", "\n").Replace("\r", "\n")

    let countLeadingSpaces (line: string) =
        let mutable count = 0
        while count < line.Length && line.[count] = ' ' do
            count <- count + 1
        count

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

module Property =

    /// The index after the property token that starts at `start`, a `!` tag or a `&` anchor. A token
    /// ends at whitespace. A shorthand tag or an anchor also ends at a flow indicator or a quote,
    /// because neither can be part of a tag handle or an anchor name. A verbatim tag `!<...>` runs to
    /// its closing `>` and may contain `:` and `,`, as in `!<tag:yaml.org,2002:str>`.
    let endIndex (s: string) (start: int) =
        let isSpace (c: char) =
            c = ' ' || c = '\t' || c = '\n' || c = '\r'

        let endsShorthand (c: char) =
            isSpace c || c = ',' || c = '[' || c = ']' || c = '{' || c = '}' || c = '\'' || c = '"'

        let mutable i = start + 1
        if s.[start] = '!' && i < s.Length && s.[i] = '<' then
            while i < s.Length && s.[i] <> '>' && not (isSpace s.[i]) do
                i <- i + 1
            if i < s.Length && s.[i] = '>' then i + 1 else i
        else
            while i < s.Length && not (endsShorthand s.[i]) do
                i <- i + 1
            i

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

    /// A block scalar header on a line, together with the indentation its content counts from.
    type HeaderLine =
        { Header: Header
          /// The structural indentation of the line, not the column of the `|` or `>`. A compact
          /// `- key: |` reports the indentation of the dash. The deeper indentation of the mapping
          /// that `key` opens is not calculated yet.
          Baseline: int }

    let private isBlankChar (c: char) = c = ' ' || c = '\t'

    let private skipBlanks (s: string) (start: int) =
        let mutable i = start
        while i < s.Length && isBlankChar s.[i] do
            i <- i + 1
        i

    /// Skips every tag and anchor from `start`, together with the blanks after each one.
    let private skipProperties (s: string) (start: int) =
        let mutable i = start
        while i < s.Length && (s.[i] = '!' || s.[i] = '&') do
            i <- skipBlanks s (Property.endIndex s i)
        i

    let stripProperties (s: string) =
        s.Substring(skipProperties s (skipBlanks s 0))

    /// True for the `-`, `?` and `:` that start a sequence entry, an explicit key or an explicit value.
    let private isEntryIndicator (s: string) (i: int) =
        (s.[i] = '-' || s.[i] = '?' || s.[i] = ':') && (i + 1 >= s.Length || isBlankChar s.[i + 1])

    let private isHeaderStart (s: string) (i: int) =
        i < s.Length && (s.[i] = '|' || s.[i] = '>')

    /// The block scalar header that ends the code of a line. Quoted scalars must already be
    /// placeholders and the line must not contain comment text, so this function never searches for
    /// quotes or hashes. It accepts these shapes, each after any number of properties:
    ///
    /// - a standalone header, such as `|2`
    /// - a header after `---`, `-`, `?` or `:`, such as `--- |` or `- >-`
    /// - a header as the value of an implicit key, such as `key: |` or `- key: |`
    ///
    /// The header must end the code, so `a: | extra` has none. A flow collection never holds one.
    let tryParseHeaderCode (code: string) =
        let line = code.TrimEnd()
        // A header ends with `|`, `>`, a digit, `-` or `+`, and most lines fail this cheap test.
        let mutable last = line.Length - 1
        while last >= 0 && ((line.[last] >= '1' && line.[last] <= '9') || line.[last] = '-' || line.[last] = '+') do
            last <- last - 1

        if last < 0 || (line.[last] <> '|' && line.[last] <> '>') then
            None
        else
            let mutable p = skipBlanks line 0
            if line.StartsWith("---") && (line.Length = 3 || isBlankChar line.[3]) then
                p <- skipBlanks line 3
            while p < line.Length && isEntryIndicator line p do
                p <- skipBlanks line (p + 1)
            p <- skipProperties line p

            let mutable start = -1
            if isHeaderStart line p then
                start <- p
            elif p < line.Length && line.[p] <> '[' && line.[p] <> '{' then
                // The first `:` followed by a blank or the end of the line separates a key from its
                // value. A colon inside a verbatim tag or a plain word such as `a:b` does not count.
                let mutable q = p
                while q < line.Length && not (line.[q] = ':' && (q + 1 >= line.Length || isBlankChar line.[q + 1])) do
                    q <- q + 1
                if q < line.Length then
                    let value = skipProperties line (skipBlanks line (q + 1))
                    if isHeaderStart line value then
                        start <- value

            if start < 0 then
                None
            else
                match parseHeader (line.Substring start) with
                | Some header -> Some { Header = header; Baseline = Line.countLeadingSpaces line }
                | None -> None

    /// The header that a line of the Preprocessing input opens. Such a line holds the placeholders of
    /// quoted scalars and may end in a comment placeholder, which is not part of the code.
    let tryParseHeaderLine (line: string) =
        let code, _ = Placeholder.splitTrailingComment line
        tryParseHeaderCode code

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
