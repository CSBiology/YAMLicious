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

    /// True for an empty line and for a line of spaces only. A tab is no space, so a line with a tab
    /// holds content.
    let isSpacesOnly (line: string) =
        countLeadingSpaces line = line.Length

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

    /// A digit of the number in a placeholder. The scanner, the patterns in Regex and these helpers agree
    /// on the ASCII digits, so text with another kind of digit stays plain text.
    let internal isDigit (c: char) = c >= '0' && c <= '9'

    let private tryParseInt (s: string) =
        match Int32.TryParse s with
        | true, value -> Some value
        | false, _ -> None

    let tryParseComment (line: string) =
        let t = line.Trim()
        if t.StartsWith("<c f=") && t.EndsWith("/>") then
            let value = t.Substring(5, t.Length - 7)
            if value |> Seq.forall isDigit then
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
                if value |> Seq.forall isDigit then
                    trimmed.Substring(0, start).TrimEnd(), tryParseInt value
                else
                    trimmed, None
            else
                trimmed, None
        else
            trimmed, None

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

module LineValue =

    let internal isBlankChar (c: char) = c = ' ' || c = '\t'

    let internal skipBlanks (s: string) (start: int) =
        let mutable i = start
        while i < s.Length && isBlankChar s.[i] do
            i <- i + 1
        i

    /// Skips every tag and anchor from `start`, together with the blanks after each one.
    let internal skipProperties (s: string) (start: int) =
        let mutable i = start
        while i < s.Length && (s.[i] = '!' || s.[i] = '&') do
            i <- skipBlanks s (Property.endIndex s i)
        i

    /// The columns of the `-` indicators that stand in a row at the start of a line. `- - a` has two, at the
    /// columns 0 and 2. A dash is an indicator when a blank or the end of the line follows it.
    let dashColumns (line: string) : int list =
        let rec loop (position: int) (columns: int list) =
            if position < line.Length && line.[position] = '-' && (position + 1 >= line.Length || isBlankChar line.[position + 1]) then
                loop (skipBlanks line (position + 1)) (position :: columns)
            else
                List.rev columns

        loop (skipBlanks line 0) []

    /// True for the `-`, `?` and `:` that start a sequence entry, an explicit key or an explicit value.
    let private isEntryIndicator (s: string) (i: int) =
        (s.[i] = '-' || s.[i] = '?' || s.[i] = ':') && (i + 1 >= s.Length || isBlankChar s.[i + 1])

    /// Where the value of a line starts and who owns it. `Start` is the index behind the entry
    /// indicators, the key and the properties, or the length of the line when nothing follows them.
    type Found =
        { Start: int
          /// The indentation of the node that owns the value. In `- key: value` it is the column of the
          /// key. For a value behind `-`, `?` or `:`, as in `- value` or `? value`, it is the column of
          /// the last of those indicators. A line without any of these has its own indentation. Properties
          /// in front of the value do not move it.
          Owner: int
          /// True when no entry indicator and no implicit key stands in front of the value. The parent node
          /// owns such a value, which only the caller knows. `Owner` is the indentation of the line then.
          Bare: bool }

    /// Walks the line from its start. The code must hold quoted scalars as placeholders and no comment,
    /// so the walk never searches for quotes or hashes. It follows an optional `---`, any number of
    /// entry indicators and properties, and then the first `:` followed by a blank or the end of the
    /// line, which separates an implicit key from its value. A flow collection or a block scalar header
    /// right behind the indicators has no key.
    let find (line: string) : Found =
        let mutable p = skipBlanks line 0
        let mutable owner = Line.countLeadingSpaces line
        if line.StartsWith("---") && (line.Length = 3 || isBlankChar line.[3]) then
            p <- skipBlanks line 3
        let mutable hasIndicator = false
        while p < line.Length && isEntryIndicator line p do
            owner <- p
            hasIndicator <- true
            p <- skipBlanks line (p + 1)
        // A value that follows an indicator belongs to the node of the last indicator. A key that
        // follows it starts a mapping of its own, which owns the value of the key.
        let keyColumn = if hasIndicator then p else owner
        p <- skipProperties line p

        let startsNode = p < line.Length && (line.[p] = '[' || line.[p] = '{' || line.[p] = '|' || line.[p] = '>')
        if p < line.Length && not startsNode then
            // The first `:` followed by a blank or the end of the line separates a key from its value.
            // A colon inside a verbatim tag or a plain word such as `a:b` does not count.
            let mutable q = p
            while q < line.Length && not (line.[q] = ':' && (q + 1 >= line.Length || isBlankChar line.[q + 1])) do
                q <- q + 1
            if q < line.Length then
                { Start = skipProperties line (skipBlanks line (q + 1)); Owner = keyColumn; Bare = false }
            else
                { Start = p; Owner = owner; Bare = not hasIndicator }
        else
            { Start = p; Owner = owner; Bare = not hasIndicator }

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

    /// A block scalar header on a line, together with the indentation of the node that owns the scalar.
    type HeaderLine =
        { Header: Header
          /// The indentation of the node that owns the scalar. An explicit indentation indicator counts
          /// from it, and the content behind the scalar is never indented deeper than it unless it
          /// belongs to the scalar. In `- key: |` it is the column of the key. For a header behind `-`,
          /// `?` or `:`, as in `- - |` or `? |`, it is the column of the last of those indicators.
          /// Properties in front of the header do not move it.
          Baseline: int }

    let stripProperties (s: string) =
        s.Substring(LineValue.skipProperties s (LineValue.skipBlanks s 0))

    let private isHeaderStart (s: string) (i: int) =
        i < s.Length && (s.[i] = '|' || s.[i] = '>')

    /// The block scalar header that ends the code of a line. Quoted scalars must already be
    /// placeholders and the line must not contain comment text, so this function never searches for
    /// quotes or hashes. Any number of properties may precede the header. The header can stand alone, as
    /// in `|2`, or follow `---`, `-`, `?` or `:`, as in `--- |` or `- >-`. It can also be the value of an
    /// implicit key, as in `key: |` or `- key: |`.
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
            let value = LineValue.find line
            if isHeaderStart line value.Start then
                match parseHeader (line.Substring value.Start) with
                | Some header -> Some { Header = header; Baseline = value.Owner }
                | None -> None
            else
                None

    /// True when a value, as written after a key or a sequence dash, starts a block scalar.
    /// Properties and a trailing comment placeholder do not count.
    let isHeaderValue (value: string) =
        let withoutComment, _ = Placeholder.splitTrailingComment value
        let indicator = stripProperties withoutComment
        indicator.StartsWith("|") || indicator.StartsWith(">")

    /// YAML 1.2.2 8.1.1.1: the indentation of the content lines of a block scalar. An indentation
    /// indicator counts from `headerIndent`, the indentation of the node that owns the scalar. Without
    /// an indicator, the first line that holds more than spaces sets it, and the lines of spaces in
    /// front of it must not be longer. The content is always indented deeper than that node, so a
    /// first such line at or below it means the scalar has no content. A body of lines of spaces only
    /// takes the length of its longest line, so that all of them stay empty.
    let contentIndent (headerIndent: int) (indicator: int option) (followingLines: string seq) =
        match indicator with
        | Some spaces -> headerIndent + spaces
        | None ->
            let longestEmpty =
                followingLines
                |> Seq.takeWhile Line.isSpacesOnly
                |> Seq.fold (fun longest line -> max longest line.Length) 0
            match followingLines |> Seq.tryFind (Line.isSpacesOnly >> not) with
            | Some first ->
                let indent = max (headerIndent + 1) (Line.countLeadingSpaces first)
                if longestEmpty > indent then
                    failwithf "A line of spaces in front of the first content line of a block scalar is indented with %d spaces, more than the %d spaces of the content." longestEmpty indent
                indent
            | None -> max (headerIndent + 1) longestEmpty

    /// Lines of spaces and lines indented at least as deep as the content indentation form the body.
    /// A line of spaces is empty when it is no longer than the indentation and content otherwise.
    let isBodyLine (indent: int) (line: string) =
        Line.isSpacesOnly line || Line.countLeadingSpaces line >= indent

    let applyChomping (chomp: ChompingMode) (content: string) =
        match chomp with
        | ChompingMode.Strip -> content.TrimEnd([|'\r'; '\n'|])
        | ChompingMode.Keep -> content
        | ChompingMode.Clip ->
            // Clipping keeps the final line break of content. A scalar of blank lines has no content.
            let trimmed = content.TrimEnd([|'\r'; '\n'|])
            if trimmed.Length > 0 && content.Length > trimmed.Length then trimmed + "\n" else trimmed

    /// Removes up to `indent` leading spaces from a line. A line of spaces that is no longer than
    /// `indent` becomes the empty string, which marks an empty line from here on. A longer line of
    /// spaces keeps the spaces beyond `indent` as content.
    let stripIndent (indent: int) (line: string) =
        line.Substring(min indent (Line.countLeadingSpaces line))

    /// YAML 1.2.2 8.1.3: folds lines that `stripIndent` has processed. The empty string is an empty
    /// line and every other line is content, also a line of spaces beyond the indentation. A line
    /// break between two folded lines becomes a space. Empty lines between them drop that break and
    /// add one line break each. Next to a line that starts with a space or a tab (a more indented
    /// line), the line break stays and the empty lines add theirs. Empty lines at the end come back
    /// as line breaks, so that the chomping can count them. Decode folds the lines of a multiline
    /// plain scalar the same way.
    let foldLines (lines: string list) =
        let isMoreIndented (line: string) =
            line.StartsWith(" ") || line.StartsWith("\t")

        let sb = StringBuilder()
        let mutable hasContent = false
        let mutable previous = ""
        let mutable empties = 0

        for line in lines do
            if line = "" then
                empties <- empties + 1
            else
                if not hasContent then
                    sb.Append(System.String('\n', empties)) |> ignore
                elif isMoreIndented previous || isMoreIndented line then
                    sb.Append(System.String('\n', empties + 1)) |> ignore
                elif empties = 0 then
                    sb.Append(' ') |> ignore
                else
                    sb.Append(System.String('\n', empties)) |> ignore
                sb.Append(line) |> ignore
                hasContent <- true
                previous <- line
                empties <- 0

        if empties > 0 then
            // The line break that ends the last content line comes first, then one for each empty line.
            sb.Append(System.String('\n', if hasContent then empties + 1 else empties)) |> ignore

        sb.ToString()

    let deindentLines (headerIndent: int) (explicitIndent: int option) (lines: string list) =
        let indent = contentIndent headerIndent explicitIndent lines
        lines |> List.map (stripIndent indent)

    let buildContent (style: BlockScalarStyle) (chomp: ChompingMode) (headerIndent: int) (explicitIndent: int option) (lines: string list) =
        let deindentedLines = deindentLines headerIndent explicitIndent lines
        let content =
            if List.isEmpty deindentedLines then
                ""
            else
                match style with
                | BlockScalarStyle.Literal -> String.Join("\n", deindentedLines) + "\n"
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
