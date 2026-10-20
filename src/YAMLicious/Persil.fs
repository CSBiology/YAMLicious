module YAMLicious.Persil

open System
open System.Collections.Generic
open System.Text
open System.Text.RegularExpressions
open YAMLiciousTypes
open Syntax

[<Literal>]
let NewLineChar = '\n'

/// The scanner's decision about one emitted line.
[<RequireQualifiedAccess>]
type LineKind =
    /// Content that none of the other kinds describe. A trailing comment does not change this.
    | Code
    | CommentOnly
    | Blank
    | Directive
    /// A `---` at column 0. Inline content after it stays on the line.
    | DocumentStart
    /// A `...` at column 0.
    | DocumentEnd
    | BlockHeader
    | BlockBody

[<Literal>]
let NoBlock = -1

[<Literal>]
let NoComment = -1

type TokenLine =
    { Text: string
      Kind: LineKind
      /// The header and the body lines of one block scalar share this id. Every other line has `NoBlock`.
      /// A `--- |` line keeps its `DocumentStart` kind and carries the id of the block it opens.
      BlockId: int
      /// The physical line of the input, counted from 1, where the emitted line starts. A quoted scalar
      /// that spans several lines collapses into one emitted line and keeps the number of its first line.
      SourceLine: int
      /// The key of the comment that ends the line in the comment map, or `NoComment`. A comment-only
      /// line has one too.
      CommentId: int
      /// The length of the text before the comment placeholder, which is the whole text without a comment.
      /// A comment-only line has the length of its indentation.
      CodeLength: int }

/// The scanner output. `Text` is the emitted text, with one placeholder per quoted scalar and per comment.
/// `Lines` holds the same text split at the line breaks that the scanner emitted. A line break inside a
/// quoted scalar is not emitted, so a multiline scalar collapses into one placeholder on one line.
type TokenizedYaml =
    { Text: string
      Lines: TokenLine[]
      StringMap: Dictionary<int, StringMapEntry>
      CommentMap: Dictionary<int, string> }

let private mkLine (text: string) (kind: LineKind) (blockId: int) (sourceLine: int) (commentId: int) (codeLength: int) : TokenLine =
    { Text = text
      Kind = kind
      BlockId = blockId
      SourceLine = sourceLine
      CommentId = commentId
      CodeLength = codeLength }

/// Decodes the content of a single-quoted scalar. A line break and the blanks around it fold into one
/// space, and a blank line becomes a line break. A single line needs no folding.
let private foldSingleQuoted (s: string) =
    let lines = s.Split([|NewLineChar|])
    if lines.Length = 1 then
        s.Replace("''", "'")
    else
        let sb = StringBuilder()
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

[<Literal>]
let private Unknown = -2

[<Literal>]
let private Failed = -1

[<Literal>]
let private Unresolved = -1

/// One pass over a newline-normalized string. The scanner owns the lexical decisions: where quoted
/// scalars, comments, directives, document markers and block scalar bodies are.
///
/// It keeps a source cursor and builds the output line by line, which lets a quoted scalar span several
/// source lines while it emits one placeholder. Quotes get a transactional probe that reads ahead
/// without changing any state. A failed probe emits only the opening quote and the scan resumes right
/// after it, so an unclosed quote cannot swallow later strings or comments. The probes memoize their
/// result per source offset, which keeps the total work linear even for malformed input.
///
/// A block scalar body is copied as it is, without any interpretation.
type private Scanner(s: string) =
    let n = s.Length
    let strings = Dictionary<int, StringMapEntry>()
    let comments = Dictionary<int, string>()
    let records = ResizeArray<TokenLine>()
    let line = StringBuilder()

    // Every emitted line starts on the physical line after the previous one, except that the line breaks
    // inside quoted scalars emit no line of their own. Counting those breaks gives the physical line.
    let mutable foldedBreaks = 0
    let mutable lineNumber = 1

    // Memo tables of the quote probes, indexed by the source offset where quoted content may continue.
    // An entry holds the offset behind a valid closing quote, Failed, or Unknown before the first probe.
    let pending = ResizeArray<int>()
    let mutable singleEnds: int[] = Array.empty
    let mutable doubleEnds: int[] = Array.empty

    let mutable i = 0
    let mutable finished = false

    // The emitted line in progress.
    let mutable lineKind = LineKind.Code
    let mutable hasCode = false
    let mutable lineComment = NoComment
    /// Where the comment placeholder starts in the line, or -1 without a comment.
    let mutable codeLength = -1

    // The lexical context of the line, which the quote rules and the property rule read.
    // `prevSig` is the last character that is not a blank. `tokenFirst` is the first character of the
    // token it belongs to. A tag or an anchor may precede a quote, as in `!!str "x"`.
    let mutable prevSig = '\n'
    let mutable tokenFirst = ' '
    let mutable prevSep = true
    // True where a node may start, so a tag or an anchor there is one token.
    let mutable atNodeStart = true

    // The block scalar in progress. The content indentation stays Unresolved until the first
    // nonblank body line sets it.
    let mutable blockCount = 0
    let mutable blockActive = false
    let mutable blockId = NoBlock
    let mutable blockBaseline = 0
    let mutable blockContentIndent = Unresolved

    // A directive can only appear at the start of the input or after a `...`. Content and every `---`
    // end the prelude, so a `%` line behind a `---` is content.
    let mutable inPrelude = true

    let copyBuffer = StringBuilder()

    /// Copies the source text from `start` up to but excluding `stop`. A copy char by char costs time
    /// proportional to its length on every target, while Substring on a large source costs time
    /// proportional to its start offset in Fable Python and would make the scan quadratic.
    let copyRange (start: int) (stop: int) =
        copyBuffer.Clear() |> ignore
        for k in start .. stop - 1 do
            copyBuffer.Append(s.[k]) |> ignore
        copyBuffer.ToString()

    let isBlankChar (c: char) = c = ' ' || c = '\t'

    /// True when a document marker starts at `k`. The caller passes the first index of a line.
    /// YAML forbids markers inside a scalar, so a quote probe stops at one.
    let markerAt (k: int) =
        if k + 2 >= n then
            false
        else
            let c = s.[k]
            if (c = '-' || c = '.') && s.[k + 1] = c && s.[k + 2] = c then
                k + 3 >= n || s.[k + 3] = ' ' || s.[k + 3] = '\t' || s.[k + 3] = '\n'
            else
                false

    /// The closing quote must end its token: only blanks, then a line end, a comment, a flow
    /// indicator or a colon may follow.
    let endsAtBoundary (next: int) =
        let mutable j = next
        while j < n && isBlankChar s.[j] do
            j <- j + 1
        if j >= n then
            true
        else
            let c = s.[j]
            c = '\n' || c = '\r' || c = '#' || c = ',' || c = ']' || c = '}' || c = ':'

    /// The offset behind the closing quote of the single-quoted scalar that opens at `opening`, or
    /// Failed. A doubled quote is content. The probe reads only, and it records the answer for every
    /// offset it passed, so a later opener inside the same stretch gets the answer without a walk.
    let probeSingle (opening: int) =
        if singleEnds.Length = 0 then
            singleEnds <- Array.create (n + 1) Unknown
        pending.Clear()
        let mutable p = opening + 1
        let mutable result = Unknown
        while result = Unknown do
            if p >= n then
                result <- Failed
            else
                let known = singleEnds.[p]
                if known <> Unknown then
                    result <- known
                else
                    pending.Add p
                    let c = s.[p]
                    if c = '\'' then
                        if p + 1 < n && s.[p + 1] = '\'' then
                            p <- p + 2
                        elif endsAtBoundary (p + 1) then
                            result <- p + 1
                        else
                            result <- Failed
                    elif c = '\n' then
                        if markerAt (p + 1) then result <- Failed else p <- p + 1
                    else
                        p <- p + 1
        for k in 0 .. pending.Count - 1 do
            singleEnds.[pending.[k]] <- result
        result

    /// Like probeSingle for a double-quoted scalar. A backslash escapes the next character.
    let probeDouble (opening: int) =
        if doubleEnds.Length = 0 then
            doubleEnds <- Array.create (n + 1) Unknown
        pending.Clear()
        let mutable p = opening + 1
        let mutable result = Unknown
        while result = Unknown do
            if p >= n then
                result <- Failed
            else
                let known = doubleEnds.[p]
                if known <> Unknown then
                    result <- known
                else
                    pending.Add p
                    let c = s.[p]
                    if c = '"' then
                        if endsAtBoundary (p + 1) then result <- p + 1 else result <- Failed
                    elif c = '\\' then
                        // An escaped line break still ends the line, so the marker check below sees it.
                        if p + 1 < n && s.[p + 1] = '\n' then p <- p + 1 else p <- p + 2
                    elif c = '\n' then
                        if markerAt (p + 1) then result <- Failed else p <- p + 1
                    else
                        p <- p + 1
        for k in 0 .. pending.Count - 1 do
            doubleEnds.[pending.[k]] <- result
        result

    /// The start rule for a quote: it opens a scalar at the start of a line, after an indicator and
    /// after a tag or an anchor. It does not open one inside a word, as in `don't` or `rock 'n' roll`.
    let startsToken () =
        match prevSig with
        | '\n' | ':' | '-' | ',' | '[' | '{' | '?' -> true
        | _ -> tokenFirst = '!' || tokenFirst = '&'

    let nextIsBlank (k: int) =
        k >= n || s.[k] = ' ' || s.[k] = '\t' || s.[k] = '\n'

    let appendPlain (c: char) =
        line.Append(c) |> ignore
        prevSig <- c
        if prevSep then
            tokenFirst <- c
            prevSep <- false
        atNodeStart <- false
        hasCode <- true
        i <- i + 1

    /// Appends a character that ends a token. `startsNode` tells whether a node may start behind it.
    let appendSeparator (c: char) (startsNode: bool) =
        line.Append(c) |> ignore
        prevSig <- c
        prevSep <- true
        atNodeStart <- startsNode
        hasCode <- true
        i <- i + 1

    /// Emits one placeholder for the scalar from `opening` to `next`, the offset behind its closing
    /// quote. A double-quoted scalar keeps its raw interior, because Escapes folds its lines and decodes
    /// its escapes in one pass later.
    let commitString (isSingle: bool) (opening: int) (next: int) =
        let raw = copyRange (opening + 1) (next - 1)
        for c in raw do
            if c = NewLineChar then foldedBreaks <- foldedBreaks + 1
        let stringId = strings.Count
        let entry: StringMapEntry =
            if isSingle then
                { Value = foldSingleQuoted raw; Kind = QuotedStringKind.SingleQuotedString }
            else
                { Value = raw; Kind = QuotedStringKind.DoubleQuotedString }
        strings.Add(stringId, entry)
        line.Append("<s f=" + string stringId + "/>") |> ignore
        prevSig <- s.[opening]
        tokenFirst <- s.[opening]
        prevSep <- false
        atNodeStart <- false
        hasCode <- true
        i <- next

    /// The offset behind a text of the shape `<c f=N/>` or `<s f=N/>` that starts at `k`, or -1.
    let placeholderEnd (k: int) =
        let mutable e = k + 5
        if k + 5 < n && (s.[k + 1] = 'c' || s.[k + 1] = 's') && s.[k + 2] = ' ' && s.[k + 3] = 'f' && s.[k + 4] = '=' then
            while e < n && Placeholder.isDigit s.[e] do
                e <- e + 1
            if e > k + 5 && e + 1 < n && s.[e] = '/' && s.[e + 1] = '>' then e + 2 else -1
        else
            -1

    /// A text in the source that has the shape of a placeholder is plain text. Later stages read every
    /// placeholder in the emitted text as the scanner's own, so the text moves into the string table
    /// and a placeholder replaces it. The Reader restores it as the text it was, with the style of a
    /// plain scalar.
    let commitLiteralPlaceholder (stop: int) =
        let stringId = strings.Count
        strings.Add(stringId, { Value = copyRange i stop; Kind = QuotedStringKind.EscapedPlainString })
        line.Append("<s f=" + string stringId + "/>") |> ignore
        prevSig <- '>'
        if prevSep then
            tokenFirst <- '<'
            prevSep <- false
        atNodeStart <- false
        hasCode <- true
        i <- stop

    let tryQuote (isSingle: bool) =
        if startsToken () then
            let next = if isSingle then probeSingle i else probeDouble i
            if next > 0 then
                commitString isSingle i next
                true
            else
                false
        else
            false

    /// Takes everything behind the `#` at the cursor, up to the line break, as one comment.
    let takeComment () =
        let mutable e = i + 1
        while e < n && s.[e] <> '\n' do
            e <- e + 1
        let commentId = comments.Count
        comments.Add(commentId, copyRange (i + 1) e)
        codeLength <- line.Length
        line.Append("<c f=" + string commentId + "/>") |> ignore
        lineComment <- commentId
        i <- e

    /// Looks for a block scalar header in the code of the finished line and records the line. The
    /// code is the emitted text before any comment placeholder, so a quoted scalar is a placeholder
    /// and comment text never shows up in it.
    let finishLine () =
        let text = line.ToString()
        let mutable kind = lineKind
        let mutable opened = NoBlock
        let codeEnd = if lineComment = NoComment then text.Length else codeLength
        if hasCode && (lineKind = LineKind.Code || lineKind = LineKind.DocumentStart) then
            match BlockScalar.tryParseHeaderCode (text.Substring(0, codeEnd)) with
            | Some found ->
                opened <- blockCount
                blockCount <- blockCount + 1
                blockActive <- true
                blockId <- opened
                blockBaseline <- found.Baseline
                blockContentIndent <-
                    match found.Header.Indent with
                    | Some _ -> BlockScalar.contentIndent found.Baseline found.Header.Indent Seq.empty
                    | None -> Unresolved
            | None -> ()

        if lineKind = LineKind.Code then
            if opened <> NoBlock then kind <- LineKind.BlockHeader
            elif hasCode then kind <- LineKind.Code
            elif lineComment <> NoComment then kind <- LineKind.CommentOnly
            else kind <- LineKind.Blank

        match lineKind with
        | LineKind.DocumentEnd -> inPrelude <- true
        | LineKind.DocumentStart -> inPrelude <- false
        | LineKind.Code -> if hasCode then inPrelude <- false
        | _ -> ()

        records.Add(mkLine text kind opened lineNumber lineComment codeEnd)

    /// Copies the line at the cursor into the block scalar in progress when it belongs to the body.
    /// Blank lines belong to it and leave the content indentation open. A nonblank line sets the
    /// indentation when it is still open, and a line indented less than that ends the block.
    let takeBodyLine () =
        let start = i
        let mutable e = start
        while e < n && s.[e] <> '\n' do
            e <- e + 1
        let text = copyRange start e
        let isBody =
            if Line.isBlank text then
                true
            else
                if blockContentIndent = Unresolved then
                    blockContentIndent <- BlockScalar.contentIndent blockBaseline None (Seq.singleton text)
                BlockScalar.isBodyLine blockContentIndent text
        if isBody then
            records.Add(mkLine text LineKind.BlockBody blockId (records.Count + 1 + foldedBreaks) NoComment text.Length)
            if e >= n then
                i <- n
                finished <- true
            else
                i <- e + 1
        isBody

    /// A directive line is copied without reading quotes. A separated hash still starts a comment.
    let scanDirective () =
        lineKind <- LineKind.Directive
        let mutable lineDone = false
        while not lineDone do
            if i >= n || s.[i] = '\n' then
                lineDone <- true
            elif s.[i] = '#' && CommentMarker.startsAt s i then
                takeComment ()
            else
                line.Append(s.[i]) |> ignore
                i <- i + 1

    /// Scans the content of one emitted line up to an unquoted line break or the end of the input.
    let scanContent () =
        let mutable lineDone = false
        while not lineDone do
            if i >= n then
                lineDone <- true
            else
                let c = s.[i]
                match c with
                | '\n' ->
                    lineDone <- true
                | ' ' | '\t' ->
                    line.Append(c) |> ignore
                    prevSep <- true
                    i <- i + 1
                | '\'' ->
                    if not (tryQuote true) then appendPlain c
                | '"' ->
                    if not (tryQuote false) then appendPlain c
                | '#' ->
                    if CommentMarker.startsAt s i then takeComment () else appendPlain c
                | '[' | '{' ->
                    appendSeparator c atNodeStart
                | ',' ->
                    appendSeparator c true
                | '<' ->
                    let stop = placeholderEnd i
                    if stop > 0 then commitLiteralPlaceholder stop else appendPlain c
                | ':' ->
                    appendSeparator c (nextIsBlank (i + 1))
                | '-' | '?' ->
                    appendSeparator c (atNodeStart && nextIsBlank (i + 1))
                | '!' | '&' ->
                    if atNodeStart then
                        // A property is one token. A quote inside it must not start a scalar.
                        let e = Property.endIndex s i
                        for k in i .. e - 1 do
                            line.Append(s.[k]) |> ignore
                        prevSig <- s.[e - 1]
                        tokenFirst <- c
                        prevSep <- false
                        hasCode <- true
                        i <- e
                    else
                        appendPlain c
                | _ ->
                    appendPlain c

    /// Scans one emitted line, starting at a physical line start.
    let scanLine () =
        line.Clear() |> ignore
        lineNumber <- records.Count + 1 + foldedBreaks
        lineKind <- LineKind.Code
        hasCode <- false
        lineComment <- NoComment
        codeLength <- -1
        prevSig <- '\n'
        tokenFirst <- ' '
        prevSep <- true
        atNodeStart <- true

        if i < n && s.[i] = '%' && inPrelude then
            scanDirective ()
        else
            if i < n && (s.[i] = '-' || s.[i] = '.') && markerAt i then
                lineKind <- if s.[i] = '-' then LineKind.DocumentStart else LineKind.DocumentEnd
                prevSig <- s.[i]
                for _ in 1 .. 3 do
                    line.Append(s.[i]) |> ignore
                i <- i + 3
            scanContent ()

        finishLine ()
        if i >= n then finished <- true else i <- i + 1

    member _.Run() : TokenizedYaml =
        while not finished do
            if not (blockActive && i < n && takeBodyLine ()) then
                blockActive <- false
                scanLine ()
        let lines = records.ToArray()
        { Text = String.Join(string NewLineChar, lines |> Array.map (fun l -> l.Text))
          Lines = lines
          StringMap = strings
          CommentMap = comments }

/// Replaces every quoted scalar and every comment with a placeholder, `<s f=N/>` and `<c f=N/>`, and
/// classifies each emitted line. String ids follow the source order across both quote kinds, and
/// comment ids have their own counter. The input may use any line ending.
let tokenize (yamlString: string) : TokenizedYaml =
    Scanner(Line.normalizeNewlines yamlString).Run()

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

/// The input of Preprocessing: the lines that follow the prelude of a document, together with the
/// version and the tag handles that its directives set. `prepareDocuments` returns one value for each
/// document of an input. `pipeline` returns the whole input as one value, so every document marker
/// after the first stays a line.
type PreparedYaml =
    { Lines: TokenLine[]
      StringMap: Dictionary<int, StringMapEntry>
      CommentMap: Dictionary<int, string>
      YAMLVersion: YAMLDirective option
      TagHandles: Map<string, string> }

/// The lines from `start` up to but excluding `stop`. Fable Python rejects Array.sub with a start at
/// the end of the array, so an empty range gets an empty array.
let private slice (lines: TokenLine[]) (start: int) (stop: int) =
    if stop > start then Array.sub lines start (stop - start) else Array.empty

/// Removes the empty first and the empty last line, as the split at a line break leaves them. A blank
/// line that belongs to a block scalar body stays, because chomping counts it.
let private cut (lines: TokenLine[]) =
    let isEmpty (line: TokenLine) = line.Text = "" && line.Kind = LineKind.Blank
    let start = if lines.Length > 0 && isEmpty lines.[0] then 1 else 0
    let stop = if lines.Length > start && isEmpty lines.[lines.Length - 1] then lines.Length - 1 else lines.Length
    slice lines start stop

/// The line that stays behind when a `---` marker is removed: its inline content, a comment that
/// follows it, or nothing. A block header keeps the id of its body.
let private tryInlineContent (marker: TokenLine) : TokenLine option =
    let rest = marker.Text.Substring(3).TrimStart()
    let restCodeLength = marker.CodeLength - (marker.Text.Length - rest.Length)
    if rest = "" then
        None
    elif restCodeLength = 0 then
        Some (mkLine rest LineKind.CommentOnly NoBlock marker.SourceLine marker.CommentId 0)
    elif marker.BlockId <> NoBlock then
        Some (mkLine rest LineKind.BlockHeader marker.BlockId marker.SourceLine marker.CommentId restCodeLength)
    else
        Some (mkLine rest LineKind.Code NoBlock marker.SourceLine marker.CommentId restCodeLength)

let private isPreludeLine (line: TokenLine) =
    line.Kind = LineKind.Directive || line.Kind = LineKind.CommentOnly || line.Kind = LineKind.Blank

/// What the lines in front of a document body contribute.
type private Opening =
    { Version: YAMLDirective option
      Handles: Map<string, string>
      /// The comments and blank lines of the prelude, the comment behind a directive and the content
      /// behind the start marker.
      PreludeLines: TokenLine[]
      /// The index of the first line of the body.
      BodyStart: int }

/// The first content of a document cannot start with `%`. The `---` ended the prelude, so such a line
/// is no directive, and YAML 1.2.2 does not let a plain scalar start with `%`. The content is the text
/// behind the marker or else the first line behind it that is neither a comment nor blank.
let private failOnDirectiveContent (inlineLine: TokenLine option) (lines: TokenLine[]) (afterMarker: int) =
    let mutable first = afterMarker
    while first < lines.Length && (lines.[first].Kind = LineKind.CommentOnly || lines.[first].Kind = LineKind.Blank) do
        first <- first + 1

    let content =
        match inlineLine with
        | Some line when line.Kind <> LineKind.CommentOnly -> Some line
        | _ -> if first < lines.Length then Some lines.[first] else None

    match content with
    | Some line when (line.Kind = LineKind.Code || line.Kind = LineKind.BlockHeader) && line.Text.StartsWith("%") ->
        failwithf "Line %d: a directive is only allowed at the start of the input or after a document end marker." line.SourceLine
    | _ -> ()

/// Reads the prelude of the document that starts at line `start`: the directives, comments and blank
/// lines, then the `---` marker. This is the only place that decides which lines form a prelude. A
/// directive sets the version or a tag handle and leaves no line. A comment stays a line, also the one
/// behind a directive or behind the marker. Content behind the marker becomes the first line.
let private openDocument (lines: TokenLine[]) (start: int) : Opening =
    let prelude = ResizeArray<TokenLine>()
    let mutable yamlVersion = None
    let mutable tagHandles =
        Map.empty
        |> Map.add "!" "!"
        |> Map.add "!!" "tag:yaml.org,2002:"

    let mutable index = start
    while index < lines.Length && isPreludeLine lines.[index] do
        let record = lines.[index]
        if record.Kind = LineKind.Directive then
            let code = record.Text.Substring(0, record.CodeLength)
            match parseYAMLDirective code with
            | Some version ->
                if yamlVersion.IsSome then failwith "Duplicate YAML directive"
                yamlVersion <- Some version
            | None ->
                match parseTagDirective code with
                | Some (handle, prefix) -> tagHandles <- Map.add handle prefix tagHandles
                | None -> ()
            if record.CommentId <> NoComment then
                prelude.Add(mkLine (record.Text.Substring record.CodeLength) LineKind.CommentOnly NoBlock record.SourceLine record.CommentId 0)
        else
            prelude.Add record
        index <- index + 1

    if index < lines.Length && lines.[index].Kind = LineKind.DocumentStart then
        let inlineLine = tryInlineContent lines.[index]
        match inlineLine with
        | Some line -> prelude.Add line
        | None -> ()
        index <- index + 1
        failOnDirectiveContent inlineLine lines index

    { Version = yamlVersion
      Handles = tagHandles
      PreludeLines = prelude.ToArray()
      BodyStart = index }

/// The document that opens with `opening` and takes the lines up to but excluding line `stop`.
let private assemble (tokenized: TokenizedYaml) (lines: TokenLine[]) (opening: Opening) (stop: int) : PreparedYaml =
    { Lines = Array.append opening.PreludeLines (slice lines opening.BodyStart stop)
      StringMap = tokenized.StringMap
      CommentMap = tokenized.CommentMap
      YAMLVersion = opening.Version
      TagHandles = opening.Handles }

/// Splits a tokenized input into its documents and prepares each one for Preprocessing. A document ends
/// at a `...` or in front of the next `---`. The scanner classified the markers, so a marker-like line
/// in a scalar never splits a document.
///
/// The comments between the start of the input or a `...` and the next `---` are the prelude of the
/// document that this `---` starts, and they stay with it. Comments between the content of a document
/// and the next `---` stay in the earlier document. A document with no line besides blank lines, such
/// as one that holds only directives, is dropped.
let prepareDocuments (tokenized: TokenizedYaml) : PreparedYaml list =
    let lines = cut tokenized.Lines
    let documents = ResizeArray<PreparedYaml>()
    let isMarker (line: TokenLine) = line.Kind = LineKind.DocumentStart || line.Kind = LineKind.DocumentEnd

    let mutable index = 0
    while index < lines.Length do
        let opening = openDocument lines index
        let mutable stop = opening.BodyStart
        while stop < lines.Length && not (isMarker lines.[stop]) do
            stop <- stop + 1

        let document = assemble tokenized lines opening stop
        if document.Lines |> Array.exists (fun line -> line.Kind <> LineKind.Blank) then
            documents.Add document
        index <- if stop < lines.Length && lines.[stop].Kind = LineKind.DocumentEnd then stop + 1 else stop

    List.ofSeq documents

/// Tokenizes once and prepares the whole input as one document for Preprocessing. It reads the
/// directives and removes them together with the leading `---`, and the comments of the prelude stay.
/// Every later marker stays a line. The line kinds stay available for the stages that follow.
let pipeline (yamlString: string) : PreparedYaml =
    let tokenized = tokenize yamlString
    let lines = cut tokenized.Lines
    assemble tokenized lines (openDocument lines 0) lines.Length
