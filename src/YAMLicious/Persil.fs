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
      /// The physical line of the input, counted from 1, where the emitted line starts. A quoted scalar,
      /// a plain scalar or a flow collection that spans several lines collapses into one emitted line and
      /// keeps the number of its first line.
      SourceLine: int
      /// The key of the comment that ends the line in the comment map, or `NoComment`. A comment-only
      /// line has one too.
      CommentId: int
      /// The length of the text before the comment placeholder, which is the whole text without a comment.
      /// A comment-only line has the length of its indentation.
      CodeLength: int }

/// The scanner output. `Lines` holds the emitted text split at the line breaks that the scanner emitted,
/// with one placeholder per quoted scalar and per comment. A line break inside a quoted scalar is not
/// emitted, so a multiline scalar collapses into one placeholder on one line. A line break inside a
/// flow collection becomes a space, so the collection stays on the line where it opens.
type TokenizedYaml =
    { Lines: TokenLine[]
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

/// The results of a quote probe that name a failure. A success is the offset behind the closing quote.
[<Literal>]
let private NotClosed = -1

[<Literal>]
let private TextBehind = -2

[<Literal>]
let private Unresolved = -1

/// The owner of a node at the root of the document. Every line is deeper than it, also a line at column 0.
[<Literal>]
let private RootOwner = -1

/// The owner of the flow collection on the line in progress, before a line break inside the collection asks for it.
[<Literal>]
let private OwnerUnknown = -3

/// No plain scalar is open. It differs from `RootOwner`, the owner of a plain scalar at the root.
[<Literal>]
let private NoPlain = -2

/// One pass over a newline-normalized string. The scanner owns the lexical decisions: where quoted
/// scalars, comments, directives, document markers and block scalar bodies are.
///
/// It keeps a source cursor and builds the output line by line, which lets a quoted scalar or a flow
/// collection span several source lines while it emits one line. A quote at a node start opens a quoted
/// scalar. A probe reads ahead to its closing quote without changing any state, and the scan continues
/// behind that quote. A scalar that does not close, or a closing quote with text behind it, is an error,
/// so every probe either skips the text it read or stops the scan, and the total work stays linear.
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

    // The line breaks that the last quote probe passed inside the scalar.
    let mutable probeBreaks = 0

    let mutable i = 0
    let mutable finished = false

    // The emitted line in progress.
    let mutable lineKind = LineKind.Code
    let mutable hasCode = false
    let mutable lineComment = NoComment
    /// Where the comment placeholder starts in the line, or -1 without a comment.
    let mutable codeLength = -1
    // The length of the line and whether it ends in a blank. A StringBuilder of Fable JS adds up its pieces
    // to report the length, so a line that merges a long flow collection keeps the count itself.
    let mutable lineLength = 0
    let mutable lastBlank = false

    // The lexical context of the line, which the quote rules and the property rule read.
    // True where a node may start. A quote opens a scalar there and a tag or an anchor there is one
    // token. Anywhere else a quote is text of a plain scalar, as in `o["k"]`. A property leaves the
    // flag set, so the quote in `!!str "x"` opens a scalar.
    let mutable atNodeStart = true
    // The offset right behind the last quoted scalar or flow collection that ended. A comment needs
    // a blank in front of its hash (YAML 1.2.2 6.6), so a hash at this offset is an error.
    let mutable tokenEnd = -1
    // The flow collections that are open, as the `[` or `{` that opened each one, and the physical line of
    // that opener. A flow collection can span several lines. The scanner merges those lines into the
    // emitted line where the collection opens, so both lists are empty at the start of every line, and a
    // document marker or the end of the input inside a collection is an error. Inside a flow collection,
    // a quote can open a scalar behind `[`, `{`, `,` and `:`. Outside one, a comma or a bracket in a plain
    // scalar is text.
    let flowOpeners = ResizeArray<char>()
    let flowLines = ResizeArray<int>()
    // Whether the current entry of each open flow collection has its colon already.
    let flowColons = ResizeArray<bool>()
    // The indentation that a line inside a flow collection must exceed, or OwnerUnknown. It is the owner of
    // the value that the collection is, which the first line break inside the collection looks up.
    let mutable flowOwner = OwnerUnknown
    // The first line that holds more than blanks behind the line break that a lookahead started at: the
    // offset of its first character, the offset where the line starts, and the line breaks passed on the way.
    let mutable nextAt = 0
    let mutable nextLineStart = 0
    let mutable nextBreaks = 0
    // True right behind a flow collection that closed, until a separator follows. Only a comment, a `,` or
    // a closer can follow it, so text, a bracket or a colon there is an error.
    let mutable afterCloser = false
    // True when a comment inside a flow collection ended a node. Only a `,` or a closer can follow it.
    let mutable commentEndedNode = false
    // True right behind a `[`, a `{` or a `,` of a flow collection, until a node or another indicator follows.
    // A `,` there would leave an entry empty.
    let mutable emptySlot = false
    // True right behind the `?` of an explicit key inside a flow collection, until the key or another
    // indicator follows. Such a key may span several lines.
    let mutable explicitKey = false

    // The columns of the block nodes that the structural lines opened and that are still open, from the
    // outermost to the innermost. A structural line is a line of code that does not continue a plain
    // scalar and does not start inside a flow collection. It pops the nodes at its own indentation or
    // deeper, then pushes the column of the node it owns. That is the key for `key:` and `- key:` and the
    // last indicator for `- -` and `? `. A bare value pushes the indentation of its line. A line that holds
    // only properties is a pending property and pushes nothing, so the value behind it belongs to the node
    // in front of it.
    let owners = ResizeArray<int>()
    // True while the last node of the line in progress is a plain scalar that started on the line. Where
    // it started: the position in the emitted line, the offset in the source and the size of the string
    // table at that moment.
    let mutable plainOpen = false
    let mutable plainStartPos = 0
    let mutable plainStartSrc = 0
    let mutable plainStringMark = 0
    // The owner and the line of a plain scalar that a comment ended, or NoPlain. A comment ends a plain
    // scalar, so a line that is deeper than its owner can neither continue it nor start a node.
    let mutable endedOwner = NoPlain
    let mutable endedLine = 0
    // True once the line holds the colon of an implicit key outside a flow collection. A plain scalar that
    // starts behind it is a value, and a value cannot hold `: `.
    let mutable keySeparated = false

    // The block scalar in progress. The content indentation stays Unresolved until the first
    // body line with more than spaces sets it.
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

    /// Appends a character or a text to the line in progress and keeps its length and its last character.
    let emit (c: char) =
        line.Append(c) |> ignore
        lineLength <- lineLength + 1
        lastBlank <- isBlankChar c

    let emitText (text: string) =
        line.Append(text) |> ignore
        lineLength <- lineLength + text.Length
        lastBlank <- false

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

    let nextIsBlank (k: int) =
        k >= n || s.[k] = ' ' || s.[k] = '\t' || s.[k] = '\n'

    /// The closing quote must end its node. Only blanks, then a line end or a comment may follow. A colon
    /// may follow when it separates a key, and in a flow collection also a `,` or a closer. A line end does
    /// not end the node in a flow collection, so the check looks at the first character of a later line.
    let endsAtBoundary (next: int) (inFlow: bool) =
        let mutable j = next
        while j < n && (isBlankChar s.[j] || (inFlow && s.[j] = '\n')) do
            j <- j + 1
        if j >= n then
            true
        else
            let c = s.[j]
            c = '\n' || c = '#'
            || (c = ':' && (inFlow || nextIsBlank (j + 1)))
            || (inFlow && (c = ',' || c = ']' || c = '}'))

    /// The offset behind the closing quote of the single-quoted scalar that opens at `opening`, NotClosed
    /// when the scalar does not close before the input or a document marker ends, or TextBehind when text
    /// follows the closing quote. A doubled quote is content. The probe reads only, and it counts the
    /// line breaks inside the scalar in probeBreaks.
    let probeSingle (opening: int) (inFlow: bool) =
        probeBreaks <- 0
        let mutable p = opening + 1
        let mutable result = 0
        while result = 0 do
            if p >= n then
                result <- NotClosed
            else
                let c = s.[p]
                if c = '\'' then
                    if p + 1 < n && s.[p + 1] = '\'' then
                        p <- p + 2
                    elif endsAtBoundary (p + 1) inFlow then
                        result <- p + 1
                    else
                        result <- TextBehind
                elif c = '\n' then
                    if markerAt (p + 1) then
                        result <- NotClosed
                    else
                        probeBreaks <- probeBreaks + 1
                        p <- p + 1
                else
                    p <- p + 1
        result

    /// Like probeSingle for a double-quoted scalar. A backslash escapes the next character.
    let probeDouble (opening: int) (inFlow: bool) =
        probeBreaks <- 0
        let mutable p = opening + 1
        let mutable result = 0
        while result = 0 do
            if p >= n then
                result <- NotClosed
            else
                let c = s.[p]
                if c = '"' then
                    result <- if endsAtBoundary (p + 1) inFlow then p + 1 else TextBehind
                elif c = '\\' then
                    // An escaped line break still ends the line, so the marker check below sees it.
                    if p + 1 < n && s.[p + 1] = '\n' then p <- p + 1 else p <- p + 2
                elif c = '\n' then
                    if markerAt (p + 1) then
                        result <- NotClosed
                    else
                        probeBreaks <- probeBreaks + 1
                        p <- p + 1
                else
                    p <- p + 1
        result

    /// True when a token inside a flow collection ends at `k`: at a blank, at a flow indicator or at the end.
    let endsFlowToken (k: int) =
        nextIsBlank k || s.[k] = ',' || s.[k] = '[' || s.[k] = ']' || s.[k] = '{' || s.[k] = '}'

    /// The physical line of the cursor, counted from 1.
    let physicalLine () = records.Count + 1 + foldedBreaks

    /// Notes what the character `c` at a node start begins. A plain scalar cannot start with an indicator,
    /// so a node that begins with an alias or a block scalar indicator is no open plain scalar.
    /// The reserved `%`, `@` and backquote begin no node at all. A comma outside a flow collection and a
    /// hash that starts no comment begin none either, so each of them is an error. The other indicators
    /// never get here, because a flow collection, a closer, a property and a quoted scalar each have a
    /// path of their own.
    let startPlain (c: char) =
        match c with
        | ',' | '#' | '%' | '@' | '`' ->
            failwithf "Line %d: a plain scalar cannot start with `%s`." (physicalLine ()) (string c)
        | '|' | '>' ->
            if flowOpeners.Count > 0 then
                failwithf "Line %d: a block scalar cannot start inside a flow collection." (physicalLine ())
            // The header runs to the line break or to a comment, and nothing but an indentation digit and a
            // chomping sign may follow the indicator.
            let mutable stop = i
            while stop < n && s.[stop] <> NewLineChar && not (s.[stop] = '#' && CommentMarker.startsAt s stop) do
                stop <- stop + 1
            if Option.isNone (BlockScalar.parseHeader (copyRange i stop)) then
                failwithf "Line %d: a block scalar header allows only an indentation digit and a chomping sign behind the `%s`." (physicalLine ()) (string c)
            plainOpen <- false
        | '*' ->
            plainOpen <- false
        | _ ->
            plainOpen <- true
            plainStartPos <- lineLength
            plainStartSrc <- i
            plainStringMark <- strings.Count

    /// True when a plain scalar inside a flow collection ends at `k`: at a flow indicator, at a colon with
    /// a blank or a flow indicator behind it, and at a hash behind a blank (YAML 1.2.2 7.3.3).
    let endsFlowPlain (k: int) =
        let c = s.[k]
        c = ',' || c = '[' || c = ']' || c = '{' || c = '}'
        || (c = ':' && endsFlowToken (k + 1))
        || (c = '#' && k > 0 && isBlankChar s.[k - 1])

    /// The offset where the text of a plain scalar that starts at `from` ends on its line: the character
    /// that ends the scalar, or the line break, or the end of the input.
    let flowPlainEnd (from: int) =
        let mutable e = from
        while e < n && s.[e] <> NewLineChar && not (endsFlowPlain e) do
            e <- e + 1
        e

    /// Looks for the first line behind the line break at `from` that holds more than blanks and records
    /// where it is. Returns false when only blanks follow.
    let lookAhead (from: int) =
        let mutable p = from
        let mutable breaks = 0
        let mutable start = from + 1
        while p < n && (s.[p] = NewLineChar || isBlankChar s.[p]) do
            if s.[p] = NewLineChar then
                breaks <- breaks + 1
                start <- p + 1
            p <- p + 1
        nextAt <- p
        nextLineStart <- start
        nextBreaks <- breaks
        p < n

    /// The indentation that the lines of the flow collection on the line in progress must exceed. It is
    /// the owner of the value that the collection is, found the way trackStructure finds it for the
    /// finished line. The first line break inside a collection asks for it, and the line keeps the answer.
    let ensureFlowOwner () =
        if flowOwner = OwnerUnknown then
            let text = line.ToString()
            let value = LineValue.find text
            if value.Bare then
                let indent = Line.countLeadingSpaces text
                // A document start clears the owners when the line is finished.
                let mutable top = if lineKind = LineKind.DocumentStart then -1 else owners.Count - 1
                while top >= 0 && owners.[top] >= indent do
                    top <- top - 1
                flowOwner <- if top >= 0 then owners.[top] else RootOwner
            else
                flowOwner <- value.Owner

    /// Checks that the line found by the last lookahead keeps the indentation of the flow collection. The
    /// lines of a collection must be indented deeper than the node that owns it. A line that opens with a
    /// closer may sit at the column of the owner, as in the CWL layout `k: [` followed by `]` at column 0.
    /// A comment line may sit anywhere.
    let checkFlowIndent () =
        let c = s.[nextAt]
        if c <> '#' then
            ensureFlowOwner ()
            let mutable spaces = nextLineStart
            while spaces < nextAt && s.[spaces] = ' ' do
                spaces <- spaces + 1
            let indent = spaces - nextLineStart
            if indent < flowOwner || (indent = flowOwner && c <> ']' && c <> '}') then
                failwithf "Line %d: a line inside a flow collection must be indented deeper than the node that owns the collection." (physicalLine () + nextBreaks)

    /// Rejects text or a bracket that follows a flow collection or a comment inside one directly. A node
    /// that has ended needs a `,` or a closer behind it.
    let rejectGlued () =
        if afterCloser then
            failwithf "Line %d: a flow collection ends its node, so only a comment, a `,` or a closer can follow it." (physicalLine ())
        if commentEndedNode then
            failwithf "Line %d: a comment ended the node in front of it, so only a `,` or a closer can follow." (physicalLine ())

    /// The Reader keeps only scalar keys, so a flow collection cannot be the key of an entry.
    let failComplexKey () =
        failwithf "Line %d: a flow collection cannot be the key of an entry, because YAMLicious keeps only scalar keys." (physicalLine ())

    /// Folds a plain scalar that starts at the cursor inside a flow collection and goes on in the next
    /// lines (YAML 1.2.2 7.3.3). Each line of the scalar is one segment of a string table entry, as in
    /// absorbPlain, and one placeholder takes the scalar's place. A flow indicator, a colon that
    /// separates a key, a comment, a document marker and the end of the input end the scalar. The
    /// lines must be indented like the other lines of the collection. A key may span lines in a flow
    /// mapping and behind `?`, but the key of a pair in a flow sequence may not. Returns false for a
    /// scalar that ends on its first line, which the scan reads character by character.
    let foldFlowPlain () =
        let firstEnd = flowPlainEnd i
        if firstEnd >= n || s.[firstEnd] <> NewLineChar then
            false
        else
            let segments = ResizeArray<string>()
            let mutable stop = firstEnd
            while stop > i && isBlankChar s.[stop - 1] do
                stop <- stop - 1
            segments.Add(copyRange i stop)

            // `cursor` is the line break behind the last line that the scalar took, and `breaks` counts the
            // line breaks from the end of the first line to it.
            let mutable cursor = firstEnd
            let mutable breaks = 0
            let mutable scanning = true
            while scanning do
                if not (lookAhead cursor) then
                    scanning <- false
                else
                    let c = s.[nextAt]
                    if markerAt nextLineStart || c = ',' || c = '[' || c = ']' || c = '{' || c = '}' || c = '#' then
                        scanning <- false
                    else
                        checkFlowIndent ()
                        let e = flowPlainEnd nextAt
                        // A key may span lines in a flow mapping and behind `?`. The key of a pair in a flow sequence may not.
                        if e < n && s.[e] = ':' && not explicitKey && flowOpeners.[flowOpeners.Count - 1] = '[' then
                            failwithf "Line %d: the key of a pair in a flow sequence must be on a single line." (physicalLine () + breaks + nextBreaks)
                        if e = nextAt && e < n && s.[e] = ':' then
                            // The colon starts the line, so the scalar ended on the line before.
                            scanning <- false
                        else
                            let mutable textEnd = e
                            while textEnd > nextAt && isBlankChar s.[textEnd - 1] do
                                textEnd <- textEnd - 1
                            for _ in 1 .. nextBreaks - 1 do
                                segments.Add ""
                            segments.Add(copyRange nextAt textEnd)
                            breaks <- breaks + nextBreaks
                            cursor <- e
                            if e >= n || s.[e] <> NewLineChar then scanning <- false

            if segments.Count = 1 then
                false
            else
                let stringId = strings.Count
                strings.Add(stringId, { Value = String.Join(string NewLineChar, segments.ToArray()); Kind = QuotedStringKind.PlainScalar })
                emitText ("<s f=" + string stringId + "/>")
                atNodeStart <- false
                plainOpen <- false
                hasCode <- true
                foldedBreaks <- foldedBreaks + breaks
                i <- cursor
                // The blanks in front of a comment went with the end of the last line.
                if cursor < n && s.[cursor] = '#' then emit ' '
                true

    /// Appends a character of a plain scalar. At a node start it begins the scalar. Inside a flow
    /// collection a scalar that goes on in the next lines becomes one placeholder at once.
    let appendPlain (c: char) =
        rejectGlued ()
        let startsFlowScalar = atNodeStart && flowOpeners.Count > 0
        if atNodeStart then startPlain c
        if not (startsFlowScalar && plainOpen && foldFlowPlain ()) then
            emit c
            atNodeStart <- false
            hasCode <- true
            i <- i + 1
        emptySlot <- false
        explicitKey <- false

    /// Appends a character that ends a token and a node. `startsNode` tells whether a node may start
    /// behind it.
    let appendSeparator (c: char) (startsNode: bool) =
        emit c
        atNodeStart <- startsNode
        plainOpen <- false
        afterCloser <- false
        commentEndedNode <- false
        emptySlot <- flowOpeners.Count > 0 && (c = '[' || c = '{' || c = ',')
        explicitKey <- false
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
        emitText ("<s f=" + string stringId + "/>")
        atNodeStart <- false
        plainOpen <- false
        emptySlot <- false
        explicitKey <- false
        hasCode <- true
        tokenEnd <- next
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
        rejectGlued ()
        if atNodeStart then startPlain '<'
        let stringId = strings.Count
        strings.Add(stringId, { Value = copyRange i stop; Kind = QuotedStringKind.PlainScalar })
        emitText ("<s f=" + string stringId + "/>")
        atNodeStart <- false
        emptySlot <- false
        explicitKey <- false
        hasCode <- true
        i <- stop

    let failUnclosedFlow () =
        failwithf "Line %d: the flow collection opened here is not closed." flowLines.[flowLines.Count - 1]

    /// Ends the innermost flow collection at the closer `c`. A closer of the other kind is an error.
    let closeFlow (c: char) =
        let top = flowOpeners.Count - 1
        let opener = flowOpeners.[top]
        if (c = ']') <> (opener = '[') then
            failwithf "Line %d: `%s` closes a `%s`." flowLines.[top] (string c) (string opener)
        flowOpeners.RemoveAt top
        flowLines.RemoveAt top
        flowColons.RemoveAt top

    /// A quote opens a scalar only where a node may start. It does not open one inside a word, as in
    /// `don't` or `rock 'n' roll`, and not inside a plain scalar either, as in `o["k"]` or `a, "b"`. A
    /// quote at a node start that does not open a valid scalar is an error.
    let tryQuote (isSingle: bool) =
        if atNodeStart then
            let inFlow = flowOpeners.Count > 0
            let next = if isSingle then probeSingle i inFlow else probeDouble i inFlow
            if next = NotClosed then
                failwithf "Line %d: the quoted scalar that starts here is not closed." (physicalLine ())
            elif next = TextBehind then
                failwithf "Line %d: text follows the closing quote of a quoted scalar." (physicalLine () + probeBreaks)
            else
                commitString isSingle i next
                true
        else
            false

    /// Takes everything behind the `#` at the cursor, up to the line break, as one comment. The comment
    /// behind the code of a line is the comment of the line. A comment inside a flow collection stays in
    /// the text as a placeholder, where the Reader's flow parser reads it as an entry of the collection.
    let takeComment (insideFlow: bool) =
        let mutable e = i + 1
        while e < n && s.[e] <> '\n' do
            e <- e + 1
        let commentId = comments.Count
        comments.Add(commentId, copyRange (i + 1) e)
        if not insideFlow then
            codeLength <- lineLength
            lineComment <- commentId
        emitText ("<c f=" + string commentId + "/>")
        i <- e

    /// True when the text holds a colon that a blank or the end of the text follows. Such a colon makes
    /// the text in front of it a mapping key, which a plain scalar cannot hold.
    let hasMappingIndicator (text: string) =
        let mutable found = false
        let mutable k = 0
        while not found && k < text.Length do
            if text.[k] = ':' && (k + 1 >= text.Length || isBlankChar text.[k + 1]) then found <- true
            k <- k + 1
        found

    /// Brings the stack of owners up to date with the finished line. It returns the owner of the plain
    /// scalar that the line opens, or NoPlain.
    let trackStructure (kind: LineKind) (text: string) (codeEnd: int) : int =
        if kind = LineKind.DocumentStart || kind = LineKind.DocumentEnd then
            owners.Clear()
            endedOwner <- NoPlain
        let structural =
            hasCode && (kind = LineKind.Code || kind = LineKind.BlockHeader || kind = LineKind.DocumentStart)
        let mutable opens = NoPlain
        if structural then
            let indent = Line.countLeadingSpaces text
            let code = text.Substring(0, codeEnd)
            let value = LineValue.find code
            // A tab never indents a block node. It may separate the spaces of the indentation from a flow node,
            // as in the line `  <tab>v` behind `k:`. That node must be the value of the node in front of it,
            // so the spaces reach deeper than its owner. At the root a tab alone is enough for a flow collection.
            if Line.hasTabIndent text then
                let first = LineValue.skipBlanks text 0
                let startsFlowCollection = first < text.Length && (text.[first] = '[' || text.[first] = '{')
                let isSeparation =
                    value.Bare
                    && (if owners.Count = 0 then indent > 0 || startsFlowCollection else owners.[owners.Count - 1] < indent)
                if not isSeparation then
                    failwithf "Line %d: a tab cannot be part of the indentation." lineNumber
            if LineValue.hasTabBeforeCollection code then
                failwithf "Line %d: a tab cannot separate a block collection from the indicator in front of it." lineNumber
            if endedOwner <> NoPlain then
                if indent > endedOwner then
                    failwithf "Line %d: a comment ended the plain scalar on line %d, so this more indented line is invalid." lineNumber endedLine
                endedOwner <- NoPlain
            while owners.Count > 0 && owners.[owners.Count - 1] >= indent do
                owners.RemoveAt(owners.Count - 1)
            let parent = if owners.Count > 0 then owners.[owners.Count - 1] else RootOwner
            // A line of properties alone is a pending property, and the value behind it has the parent as its owner.
            if not (value.Bare && value.Start >= code.Length) then owners.Add value.Owner
            if plainOpen && flowOpeners.Count = 0 then
                opens <- if value.Bare then parent else value.Owner
        opens

    /// Merges the lines that continue the plain scalar which the line in progress opened (YAML 1.2.2
    /// 7.3.3) into that line, so that a multiline plain scalar is one token like a multiline quoted
    /// scalar. A nonblank line deeper than the owner continues the scalar, and the blank lines in between
    /// belong to it. A comment line, a document marker and a line that is not deeper end it. The scalar
    /// becomes one entry of the string table, with one segment per line joined by line breaks, and one
    /// placeholder takes its place in the line. An empty segment is an empty line, which Decode folds
    /// into a line break.
    ///
    /// Every character of a continuation line is text, so a quote, a bracket, a dash or a tag opens no
    /// node there. A colon followed by a blank is an error, because a plain scalar cannot hold a mapping
    /// key. A comment ends the scalar, either at the end of its last line, which then keeps the comment,
    /// or as a line of its own.
    let absorbPlain (owner: int) =
        let firstLine = physicalLine ()
        let mutable firstEnd = i
        while firstEnd > plainStartSrc && isBlankChar s.[firstEnd - 1] do
            firstEnd <- firstEnd - 1
        let segments = ResizeArray<string>()
        segments.Add(copyRange plainStartSrc firstEnd)

        // `cursor` is the line break in front of the line under examination, and `ahead` counts the
        // breaks from the end of the first line to it. `taken` and `at` describe the last line that the
        // scalar took, and `pending` counts the blank lines behind it.
        let mutable cursor = i
        let mutable ahead = 0
        let mutable pending = 0
        let mutable taken = 0
        let mutable at = i
        let mutable commentAt = -1
        let mutable scanning = i < n
        while scanning do
            let first = cursor + 1
            if first >= n then
                scanning <- false
            else
                let mutable last = first
                while last < n && s.[last] <> NewLineChar do
                    last <- last + 1
                ahead <- ahead + 1
                let mutable afterSpaces = first
                while afterSpaces < last && s.[afterSpaces] = ' ' do
                    afterSpaces <- afterSpaces + 1
                let indent = afterSpaces - first
                let mutable textStart = afterSpaces
                while textStart < last && isBlankChar s.[textStart] do
                    textStart <- textStart + 1
                if textStart >= last then
                    pending <- pending + 1
                    cursor <- last
                elif s.[textStart] = '#' then
                    endedOwner <- owner
                    endedLine <- firstLine + ahead
                    scanning <- false
                elif markerAt first || indent <= owner then
                    scanning <- false
                else
                    let mutable commentStart = last
                    let mutable h = textStart + 1
                    while commentStart = last && h < last do
                        if s.[h] = '#' && isBlankChar s.[h - 1] then commentStart <- h
                        h <- h + 1
                    let mutable textEnd = commentStart
                    while textEnd > textStart && isBlankChar s.[textEnd - 1] do
                        textEnd <- textEnd - 1
                    let segment = copyRange textStart textEnd
                    if hasMappingIndicator segment then
                        failwithf "Line %d: a multi-line plain scalar cannot hold a mapping key (`: `)." (firstLine + ahead)
                    for _ in 1 .. pending do
                        segments.Add ""
                    pending <- 0
                    segments.Add segment
                    taken <- ahead
                    at <- last
                    cursor <- last
                    if commentStart < last then
                        commentAt <- commentStart
                        endedOwner <- owner
                        endedLine <- firstLine + ahead
                        scanning <- false

        if segments.Count > 1 then
            // Entries that the first line made inside the scalar are part of its text now.
            while strings.Count > plainStringMark do
                strings.Remove(strings.Count - 1) |> ignore
            let prefix = line.ToString().Substring(0, plainStartPos)
            let stringId = strings.Count
            strings.Add(stringId, { Value = String.Join(string NewLineChar, segments.ToArray()); Kind = QuotedStringKind.PlainScalar })
            line.Clear() |> ignore
            lineLength <- 0
            emitText prefix
            emitText ("<s f=" + string stringId + "/>")
            foldedBreaks <- foldedBreaks + taken
            i <- at
            if commentAt >= 0 then
                emit ' '
                i <- commentAt
                takeComment false

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

        let opens = trackStructure kind text codeEnd
        if opens <> NoPlain then
            if lineComment = NoComment then
                absorbPlain opens
            else
                endedOwner <- opens
                endedLine <- lineNumber

        // The scalar may have taken more lines, and the last of them may end in a comment.
        let finalText = line.ToString()
        let finalEnd = if lineComment = NoComment then finalText.Length else codeLength
        records.Add(mkLine finalText kind opened lineNumber lineComment finalEnd)

    /// Copies the line at the cursor into the block scalar in progress when it belongs to the body.
    /// A line of spaces belongs to it and leaves the content indentation open. A line with other
    /// characters sets the indentation when it is still open, and a line indented less than that ends
    /// the block.
    let takeBodyLine () =
        let start = i
        let mutable e = start
        while e < n && s.[e] <> '\n' do
            e <- e + 1
        let text = copyRange start e
        let isBody =
            if blockContentIndent = Unresolved && not (Line.isSpacesOnly text) then
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
                takeComment false
            else
                emit s.[i]
                i <- i + 1

    /// Merges the line break at the cursor into the line in progress, which holds a flow collection
    /// that is still open. The break and the indentation of the next line that holds more than blanks
    /// become one space. Blank lines in between vanish, and the physical lines count on.
    let mergeFlowBreak () =
        if not (lookAhead i) || markerAt nextLineStart then failUnclosedFlow ()
        checkFlowIndent ()
        if not lastBlank then emit ' '
        foldedBreaks <- foldedBreaks + nextBreaks
        i <- nextAt

    /// Scans the content of one emitted line up to an unquoted line break or the end of the input. A line
    /// break inside a flow collection does not end the line.
    let scanContent () =
        let mutable lineDone = false
        while not lineDone do
            if i >= n then
                lineDone <- true
            else
                let c = s.[i]
                match c with
                | '\n' ->
                    if flowOpeners.Count > 0 then mergeFlowBreak () else lineDone <- true
                | ' ' | '\t' ->
                    emit c
                    i <- i + 1
                | '\'' ->
                    if not (tryQuote true) then appendPlain c
                | '"' ->
                    if not (tryQuote false) then appendPlain c
                | '#' ->
                    if CommentMarker.startsAt s i then
                        if flowOpeners.Count > 0 then
                            // The comment ends a node when it follows one, and then no text may follow.
                            let endsNode = not atNodeStart
                            takeComment true
                            if endsNode then commentEndedNode <- true
                        else
                            takeComment false
                    elif i = tokenEnd then
                        failwithf "Line %d: a comment needs a space or a tab before the #." (physicalLine ())
                    else
                        appendPlain c
                | '[' | '{' ->
                    // A bracket in the middle of a plain scalar is text, as in `o[1]`. Inside a flow collection
                    // a plain scalar ends at a bracket, and a node in front of the bracket needs a `,` or a `:`.
                    if atNodeStart || flowOpeners.Count > 0 then
                        rejectGlued ()
                        if not atNodeStart then
                            failwithf "Line %d: a node in front of this flow collection needs a `,` or a `:` behind it." (physicalLine ())
                        if explicitKey then failComplexKey ()
                        flowOpeners.Add c
                        flowLines.Add(physicalLine ())
                        flowColons.Add false
                        appendSeparator c true
                    else
                        appendPlain c
                | ']' | '}' ->
                    // Inside a plain scalar a closer is text, as in `a]`. Anywhere else it needs an open flow collection.
                    if flowOpeners.Count > 0 then
                        closeFlow c
                        appendSeparator c false
                        tokenEnd <- i
                        afterCloser <- true
                    elif plainOpen then
                        appendPlain c
                    else
                        failwithf "Line %d: `%s` closes no flow collection." (physicalLine ()) (string c)
                | ',' ->
                    if flowOpeners.Count > 0 then
                        // A `,` directly behind a `[`, a `{` or another `,` leaves an entry empty.
                        if emptySlot then
                            failwithf "Line %d: a `,` cannot follow a `[`, a `{` or another `,` directly, because it would leave an entry empty." (physicalLine ())
                        flowColons.[flowColons.Count - 1] <- false
                        appendSeparator c true
                    else
                        appendPlain c
                | '<' ->
                    let stop = placeholderEnd i
                    if stop > 0 then commitLiteralPlaceholder stop else appendPlain c
                | ':' ->
                    // A colon separates a key from its value when a blank follows it. In a flow collection
                    // a flow indicator can follow it too, and a quoted scalar or a flow collection can stand
                    // directly in front of it, as in `{"a":"b"}`. Anywhere else the colon is text, as in `a:b`.
                    let separates =
                        if flowOpeners.Count > 0 then i = tokenEnd || endsFlowToken (i + 1) else nextIsBlank (i + 1)
                    if not separates then
                        appendPlain c
                    else
                        if afterCloser then failComplexKey ()
                        if flowOpeners.Count > 0 then
                            let top = flowColons.Count - 1
                            if flowColons.[top] then
                                failwithf "Line %d: an entry of a flow collection can hold only one `:`." (physicalLine ())
                            flowColons.[top] <- true
                        else
                            // The first colon ends the key. The value behind it, plain or quoted, ends at
                            // the line break, so another colon that separates is an error.
                            if keySeparated then
                                if plainOpen then
                                    failwithf "Line %d: a plain scalar cannot contain `: `." (physicalLine ())
                                else
                                    failwithf "Line %d: a second `: ` follows the colon that separated the key from its value." (physicalLine ())
                            if not atNodeStart then keySeparated <- true
                        appendSeparator c true
                | '-' | '?' ->
                    // An indicator needs a blank behind it. Without one the character begins a plain scalar.
                    // A flow collection holds no block sequence, so a dash there needs a plain character behind it.
                    // The `?` of an explicit key inside a flow collection may also stand in front of a flow indicator.
                    let inFlow = flowOpeners.Count > 0
                    if c = '-' && atNodeStart && inFlow && endsFlowToken (i + 1) then
                        failwithf "Line %d: inside a flow collection a `-` needs a plain character behind it." (physicalLine ())
                    if atNodeStart && (if inFlow && c = '?' then endsFlowToken (i + 1) else nextIsBlank (i + 1)) then
                        // A block collection cannot start on the line of its key, as in `k: - a`. The
                        // compact forms `- - a`, `- ? a`, `? - a` and `: - a` hold no key colon in front.
                        // A dash with nothing behind it, as in `k: -`, is an item that yaml accepts there.
                        if keySeparated && not inFlow then
                            let mutable q = i + 1
                            while q < n && isBlankChar s.[q] do
                                q <- q + 1
                            let contentBehind = q < n && s.[q] <> NewLineChar && s.[q] <> '#'
                            if c = '?' || contentBehind then
                                failwithf "Line %d: a block %s cannot start on the line of its key." (physicalLine ()) (if c = '-' then "sequence" else "mapping")
                        appendSeparator c true
                        if inFlow && c = '?' then explicitKey <- true
                    else
                        appendPlain c
                | '!' | '&' ->
                    if atNodeStart then
                        // A property is one token. A quote inside it must not start a scalar.
                        let e = Property.endIndex s i
                        if c = '&' && e = i + 1 && flowOpeners.Count > 0 then
                            failwithf "Line %d: an anchor needs a name behind the `&`." (physicalLine ())
                        for k in i .. e - 1 do
                            emit s.[k]
                        hasCode <- true
                        emptySlot <- false
                        i <- e
                    else
                        appendPlain c
                | _ ->
                    appendPlain c

    /// Scans one emitted line, starting at a physical line start.
    let scanLine () =
        line.Clear() |> ignore
        lineLength <- 0
        lastBlank <- false
        lineNumber <- records.Count + 1 + foldedBreaks
        lineKind <- LineKind.Code
        hasCode <- false
        lineComment <- NoComment
        codeLength <- -1
        plainOpen <- false
        keySeparated <- false
        flowOwner <- OwnerUnknown
        afterCloser <- false
        commentEndedNode <- false
        emptySlot <- false
        explicitKey <- false
        atNodeStart <- true

        if i < n && s.[i] = '%' && inPrelude then
            scanDirective ()
        else
            if i < n && (s.[i] = '-' || s.[i] = '.') && markerAt i then
                lineKind <- if s.[i] = '-' then LineKind.DocumentStart else LineKind.DocumentEnd
                for _ in 1 .. 3 do
                    emit s.[i]
                i <- i + 3
            scanContent ()

        finishLine ()
        if i >= n then finished <- true else i <- i + 1

    member _.Run() : TokenizedYaml =
        while not finished do
            if not (blockActive && i < n && takeBodyLine ()) then
                blockActive <- false
                scanLine ()
        if flowOpeners.Count > 0 then failUnclosedFlow ()
        let lines = records.ToArray()
        { Lines = lines
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

/// The index of the first line from `start` on that matches, or the number of lines.
let private indexOfLine (lines: TokenLine[]) (start: int) (matches: TokenLine -> bool) =
    let mutable index = start
    while index < lines.Length && not (matches lines.[index]) do
        index <- index + 1
    index

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
        let stop = indexOfLine lines opening.BodyStart isMarker

        let document = assemble tokenized lines opening stop
        if document.Lines |> Array.exists (fun line -> line.Kind <> LineKind.Blank) then
            documents.Add document
        index <- if stop < lines.Length && lines.[stop].Kind = LineKind.DocumentEnd then stop + 1 else stop

    List.ofSeq documents

/// Tokenizes once and prepares the first document for Preprocessing. It reads the directives and removes
/// them together with the leading `---`, and the comments of the prelude stay. The document ends at the
/// first line that the scanner classified as a `...`, and the lines behind it are dropped. Every `---`
/// before that stays a line. The line kinds stay available for the stages that follow.
let pipeline (yamlString: string) : PreparedYaml =
    let tokenized = tokenize yamlString
    let lines = cut tokenized.Lines
    let opening = openDocument lines 0
    let stop = indexOfLine lines opening.BodyStart (fun line -> line.Kind = LineKind.DocumentEnd)
    assemble tokenized lines opening stop
