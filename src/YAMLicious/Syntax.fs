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

    /// True when a tab follows the leading spaces, so the indentation of the line holds a tab.
    let hasTabIndent (line: string) =
        let spaces = countLeadingSpaces line
        spaces < line.Length && line.[spaces] = '\t'

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
    /// ends at whitespace. A shorthand tag or an anchor also ends at a flow indicator. A double quote
    /// ends a tag, because the characters of a tag are those of a URI. An anchor may hold both quote
    /// characters, as in `&a"b`, and a tag may hold a single quote. A verbatim tag `!<...>` runs to
    /// its closing `>` and may contain `:` and `,`, as in `!<tag:yaml.org,2002:str>`.
    let endIndex (s: string) (start: int) =
        let isSpace (c: char) =
            c = ' ' || c = '\t' || c = '\n' || c = '\r'

        let isTag = s.[start] = '!'

        let endsShorthand (c: char) =
            isSpace c || c = ',' || c = '[' || c = ']' || c = '{' || c = '}' || (isTag && c = '"')

        let mutable i = start + 1
        if isTag && i < s.Length && s.[i] = '<' then
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

    /// The index of the first `:` from `start` on that a blank or the end of the line follows, or the length
    /// of the line. That colon separates an implicit key from its value. A colon inside a verbatim tag or a
    /// plain word such as `a:b` does not count.
    let private keyColon (line: string) (start: int) =
        let mutable q = start
        while q < line.Length && not (line.[q] = ':' && (q + 1 >= line.Length || isBlankChar line.[q + 1])) do
            q <- q + 1
        q

    /// True when a flow collection or a block scalar header starts at `p`. `find` reads no implicit key for it.
    let private startsKeylessNode (line: string) (p: int) =
        p < line.Length && (line.[p] = '[' || line.[p] = '{' || line.[p] = '|' || line.[p] = '>')

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

        if p < line.Length && not (startsKeylessNode line p) then
            let q = keyColon line p
            if q < line.Length then
                { Start = skipProperties line (skipBlanks line (q + 1)); Owner = keyColumn; Bare = false }
            else
                { Start = p; Owner = owner; Bare = not hasIndicator }
        else
            { Start = p; Owner = owner; Bare = not hasIndicator }

    /// True when a tab stands between an entry indicator or a document start marker and a block collection
    /// that starts behind it on the same line, as in `-\tk: v`, `- \t- a` or `---\tk: v`. YAML 1.2.2 allows
    /// only spaces there. A tab in front of a flow node, as in `-\tv`, is separation and stays valid. Like
    /// `find`, it expects quoted scalars as placeholders and no comment.
    let hasTabBeforeCollection (line: string) : bool =
        let startsMarker = line.StartsWith("---") && (line.Length = 3 || isBlankChar line.[3])
        let mutable p = if startsMarker then 0 else skipBlanks line 0
        // The marker takes three characters and an indicator one.
        let mutable width = if startsMarker then 3 else 1
        let mutable found = false
        while not found && p < line.Length && (width = 3 || isEntryIndicator line p) do
            let next = skipBlanks line (p + width)
            let mutable tabbed = false
            for k in p + width .. next - 1 do
                if line.[k] = '\t' then tabbed <- true
            let node = skipProperties line next
            let startsCollection =
                node < line.Length
                && (isEntryIndicator line node || (not (startsKeylessNode line node) && keyColon line node < line.Length))
            found <- tabbed && startsCollection
            p <- next
            width <- 1
        found

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
        /// A tag or an anchor in front of a node, as written, such as `!!str` or `&a`.
        | Property of string
        /// The name of an alias, as in `*a`.
        | Alias of string
        /// The `?` that starts an explicit key.
        | Explicit
        | EOF

    /// Cuts the text of a flow collection into tokens. The scanner has replaced the quoted scalars and the
    /// comments with placeholders, so a quote never starts a token. A plain scalar is one token, with the
    /// blanks inside it and behind it. A placeholder inside it stays in the text. A comment placeholder
    /// ends the scalar in front of it and is a token of its own.
    let tokenize (input: string) : Token list =
        let n = input.Length
        let tokens = ResizeArray<Token>()

        let isFlowIndicator (c: char) =
            c = ',' || c = '[' || c = ']' || c = '{' || c = '}'

        let isSpace (c: char) =
            c = ' ' || c = '\n' || c = '\r' || c = '\t'

        // A colon separates a key from its value when a blank, a flow indicator or the end of the text
        // follows it. Otherwise it is part of a plain scalar, as in `12:45` or `http://example.com`. The `?`
        // of an explicit key needs the same.
        let isIndicator (k: int) =
            k >= n || isSpace input.[k] || isFlowIndicator input.[k]

        let startsWithAt (k: int) (prefix: string) =
            let mutable matches = k + prefix.Length <= n
            let mutable j = 0
            while matches && j < prefix.Length do
                if input.[k + j] <> prefix.[j] then matches <- false
                j <- j + 1
            matches

        // Copies the text from `start` up to but excluding `stop`, character by character. Substring on a large
        // text costs time proportional to its start offset in Fable Python, and a flow collection over many
        // lines is one long text.
        let copyRange (start: int) (stop: int) =
            let sb = StringBuilder()
            for k in start .. stop - 1 do
                sb.Append(input.[k]) |> ignore
            sb.ToString()

        // The offset behind the `/>` that ends the placeholder at `k`, or the end of the text.
        let placeholderEnd (k: int) =
            let mutable e = k
            while e < n && not (input.[e] = '/' && e + 1 < n && input.[e + 1] = '>') do
                e <- e + 1
            if e < n then e + 2 else n

        // A colon right behind a quoted scalar or a flow collection separates a key without a blank, as
        // in `{"a":1}`.
        let followsJsonLikeNode () =
            if tokens.Count = 0 then
                false
            else
                match tokens.[tokens.Count - 1] with
                | Token.String value -> value.StartsWith("<s f=") && value.EndsWith("/>")
                | Token.CloseBrace
                | Token.CloseBracket -> true
                | _ -> false

        let mutable p = 0
        while p < n do
            let c = input.[p]
            if isSpace c then
                p <- p + 1
            elif c = '{' then
                tokens.Add Token.OpenBrace
                p <- p + 1
            elif c = '}' then
                tokens.Add Token.CloseBrace
                p <- p + 1
            elif c = '[' then
                tokens.Add Token.OpenBracket
                p <- p + 1
            elif c = ']' then
                tokens.Add Token.CloseBracket
                p <- p + 1
            elif c = ',' then
                tokens.Add Token.Comma
                p <- p + 1
            elif c = ':' && (isIndicator (p + 1) || followsJsonLikeNode ()) then
                tokens.Add Token.Colon
                p <- p + 1
            elif c = '?' && isIndicator (p + 1) then
                tokens.Add Token.Explicit
                p <- p + 1
            elif c = '"' then
                // The scanner leaves no quoted scalar in the text, so this only reads text that a caller built by hand.
                let sb = StringBuilder()
                let mutable j = p + 1
                let mutable closed = false
                while not closed && j < n do
                    if input.[j] = '"' then
                        closed <- true
                        j <- j + 1
                    elif input.[j] = '\\' && j + 1 < n then
                        sb.Append(input.[j]).Append(input.[j + 1]) |> ignore
                        j <- j + 2
                    else
                        sb.Append(input.[j]) |> ignore
                        j <- j + 1
                tokens.Add(Token.String(sb.ToString()))
                p <- j
            elif c = '!' || c = '&' then
                let e = Property.endIndex input p
                tokens.Add(Token.Property(copyRange p e))
                p <- e
            elif c = '*' && p + 1 < n && not (isSpace input.[p + 1]) && not (isFlowIndicator input.[p + 1]) then
                let mutable e = p + 1
                while e < n && not (isSpace input.[e]) && not (isFlowIndicator input.[e]) && not (input.[e] = ':' && isIndicator (e + 1)) do
                    e <- e + 1
                tokens.Add(Token.Alias(copyRange (p + 1) e))
                p <- e
            elif startsWithAt p "<c f=" then
                let e = placeholderEnd p
                tokens.Add(Token.String(copyRange p e))
                p <- e
            else
                let start = p
                let mutable e = p
                let mutable stop = false
                while not stop && e < n do
                    let ch = input.[e]
                    if ch = '{' || ch = '}' || ch = '[' || ch = ']' || ch = ',' || ch = '\n' || ch = '\r' || ch = '\t' then
                        stop <- true
                    elif ch = ':' && isIndicator (e + 1) then
                        stop <- true
                    elif ch = '<' && startsWithAt e "<c f=" then
                        stop <- true
                    elif ch = '<' && startsWithAt e "<s f=" then
                        let behind = placeholderEnd e
                        if e = start then
                            // A placeholder at the start of the token stands alone when a colon, a comment or the end follows it.
                            let mutable q = behind
                            while q < n && input.[q] = ' ' do
                                q <- q + 1
                            if q >= n || input.[q] = ':' || isFlowIndicator input.[q] || isSpace input.[q] || startsWithAt q "<c f=" then
                                stop <- true
                        e <- behind
                    else
                        e <- e + 1
                if e = start then e <- start + 1
                tokens.Add(Token.String(copyRange start e))
                p <- e

        tokens.Add Token.EOF
        List.ofSeq tokens
