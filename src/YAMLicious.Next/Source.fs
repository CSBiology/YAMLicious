namespace YAMLicious

/// A value together with the range of the input that it came from.
type Located<'T> =
    { Value: 'T
      Span: SourceSpan }

/// A lexical or syntactic error. `Code` is a short stable label such as `unclosed-quote`, `Message` is the
/// text for a person, and `Span` is the offending text. `Related` holds further ranges that explain the
/// error, such as the quote that opened the scalar.
type YamlError =
    { Code: string
      Message: string
      Span: SourceSpan
      Related: SourceSpan list }

/// The exception that the lexer raises inside its routines. `Lexer.next` catches it and returns the error.
/// No other code needs it.
exception internal LexFailure of YamlError

/// The state of a snapshot of a cursor. The last four fields are the tracker that turns an index into a
/// mark, so a restore puts the tracker back where it stood and costs no rescan.
type internal CursorState =
    { Index: int
      LineBegin: bool
      Started: bool
      InPrelude: bool
      BomAllowed: bool
      JsonLikeEnd: int
      SeparationHadTab: bool
      FirstOnLine: bool
      LeadingTab: bool
      AfterIndicator: bool
      TrackIndex: int
      TrackLine: int
      TrackLineStart: int
      TrackPairs: int }

/// The position in the text and the state that the lexer keeps between two lexemes.
///
/// Positions. `Index` counts UTF-16 code units from the start of the string on .NET and JavaScript, and code
/// points on Python, because a Python string indexes by code point. `MarkAt` turns an index into a
/// line and a column. Lines count from 1, and a line ends at LF, CRLF or a lone CR. Columns count from 1 in
/// code points, so a surrogate pair is one column on .NET and JavaScript. Byte order marks at the start of a
/// line take no column, which covers the mark at the start of the text and the ones in front of a later
/// document ([202] l-document-prefix, repeated by [211] l-yaml-stream).
///
/// `MarkAt` moves one tracker forward through the text, so the calls cost time proportional to the distance
/// that they cover. The lexer asks for the marks of a lexeme in the order of the text on every path that
/// succeeds, which makes the cost linear in the length of the input. A call for an index behind the tracker
/// restarts it at the start of the text and costs time proportional to that index. Only the error paths of
/// the lexer make such a call, because they build the span of an error from a position behind the lexeme
/// that they were scanning, and an error ends the lexing. `Restore` puts the tracker back to the place it had
/// at the `Snapshot`, so it forces no rescan.
///
/// The other members hold the state of the lexer. The parser reads `SeparationHadTab`.
[<AllowNullLiteral>]
type internal Cursor(text: string) =
    let length = text.Length
    let origin = if length > 0 && text.[0] = '\uFEFF' then 1 else 0

    // The tracker that turns an index into a mark.
    let mutable trackIndex = origin
    let mutable trackLine = 1
    let mutable trackLineStart = origin
    let mutable trackPairs = 0

    let mutable index = origin
    let mutable lineBegin = true
    let mutable started = false
    let mutable inPrelude = true
    let mutable bomAllowed = true
    let mutable jsonLikeEnd = -1
    let mutable separationHadTab = false
    let mutable firstOnLine = false
    let mutable leadingTab = false
    let mutable afterIndicator = false

    /// The text, including a byte order mark at its start.
    member _.Text = text

    member _.Length = length

    /// The index of the first character after an optional byte order mark.
    member _.Origin = origin

    /// The index of the next character that the lexer has not consumed.
    member _.Index
        with get () = index
        and set (value: int) = index <- value

    /// True when `Index` is the first character of a line that the lexer has not looked at yet.
    member _.LineBegin
        with get () = lineBegin
        and set (value: bool) = lineBegin <- value

    /// True once `StreamStart` has been returned.
    member _.Started
        with get () = started
        and set (value: bool) = started <- value

    /// True at the start of the stream and right behind a `...`, until content or a `---` follows. A
    /// directive is only valid there.
    member _.InPrelude
        with get () = inPrelude
        and set (value: bool) = inPrelude <- value

    /// True at the start of the stream and right behind a `...`, until a directive or content follows. A
    /// byte order mark at the start of a line is only valid there: [202] l-document-prefix puts it in front
    /// of the comment lines of the prefix, and the directives of [209] come behind the prefix. Comment lines
    /// and blank lines leave it set.
    member _.BomAllowed
        with get () = bomAllowed
        and set (value: bool) = bomAllowed <- value

    /// The index right behind the last quoted scalar or flow collection that ended, or -1. A colon at this
    /// index is the adjacent value indicator of a JSON-like node, and a hash at this index is an error.
    member _.JsonLikeEnd
        with get () = jsonLikeEnd
        and set (value: int) = jsonLikeEnd <- value

    /// True when the blanks in front of the last lexeme held a tab. The blanks run from the end of the
    /// lexeme before it. The first lexeme of a line is false, because the indentation of the line is not
    /// its separation. A block line reports the tab of its indentation on the `LineStart` lexeme, which
    /// sets this member too, and so does a comment on a line of its own. A line inside a flow collection
    /// reports no tab.
    member _.SeparationHadTab
        with get () = separationHadTab
        and set (value: bool) = separationHadTab <- value

    /// True right after a `LineStart` lexeme.
    member _.FirstOnLine
        with get () = firstOnLine
        and set (value: bool) = firstOnLine <- value

    /// True when the indentation of the current line holds a tab.
    member _.LeadingTab
        with get () = leadingTab
        and set (value: bool) = leadingTab <- value

    /// True when the last lexeme was `-`, `?` or `:`.
    member _.AfterIndicator
        with get () = afterIndicator
        and set (value: bool) = afterIndicator <- value

    /// True when the lexer has consumed the whole text.
    member _.AtEnd = index >= length

    /// The mark of the character at `position`. See the notes on the type for the cost.
    member _.MarkAt(position: int) : SourceMark =
        if position < trackIndex then
            trackIndex <- origin
            trackLine <- 1
            trackLineStart <- origin
            trackPairs <- 0
        while trackIndex < position do
            let c = text.[trackIndex]
            if c = '\n' then
                trackIndex <- trackIndex + 1
                trackLine <- trackLine + 1
                trackLineStart <- trackIndex
                trackPairs <- 0
            elif c = '\r' then
                if trackIndex + 1 < length && text.[trackIndex + 1] = '\n' then
                    trackIndex <- trackIndex + 2
                else
                    trackIndex <- trackIndex + 1
                trackLine <- trackLine + 1
                trackLineStart <- trackIndex
                trackPairs <- 0
            else
                let code = int c
                if code = 0xFEFF && trackIndex = trackLineStart then
                    // A byte order mark with no column-taking character in front of it on its line takes no
                    // column, so a run of them at the start of a line counts as none.
                    trackIndex <- trackIndex + 1
                    trackLineStart <- trackIndex
                else
                    if code >= 0xDC00 && code <= 0xDFFF && trackIndex > trackLineStart then
                        let before = int text.[trackIndex - 1]
                        if before >= 0xD800 && before <= 0xDBFF then trackPairs <- trackPairs + 1
                    trackIndex <- trackIndex + 1
        { Line = trackLine
          Column = position - trackLineStart - trackPairs + 1 }

    /// The range from `first` up to but excluding `finish`.
    member this.SpanOf(first: int, finish: int) : SourceSpan =
        let start = this.MarkAt first
        let stop = this.MarkAt finish
        { Start = start; Finish = stop }

    /// Captures the position and the lexer state, so that `Restore` can return to them.
    member _.Snapshot() : CursorState =
        { Index = index
          LineBegin = lineBegin
          Started = started
          InPrelude = inPrelude
          BomAllowed = bomAllowed
          JsonLikeEnd = jsonLikeEnd
          SeparationHadTab = separationHadTab
          FirstOnLine = firstOnLine
          LeadingTab = leadingTab
          AfterIndicator = afterIndicator
          TrackIndex = trackIndex
          TrackLine = trackLine
          TrackLineStart = trackLineStart
          TrackPairs = trackPairs }

    /// Returns to a captured state, the tracker of the marks included.
    member _.Restore(state: CursorState) =
        index <- state.Index
        lineBegin <- state.LineBegin
        started <- state.Started
        inPrelude <- state.InPrelude
        bomAllowed <- state.BomAllowed
        jsonLikeEnd <- state.JsonLikeEnd
        separationHadTab <- state.SeparationHadTab
        firstOnLine <- state.FirstOnLine
        leadingTab <- state.LeadingTab
        afterIndicator <- state.AfterIndicator
        trackIndex <- state.TrackIndex
        trackLine <- state.TrackLine
        trackLineStart <- state.TrackLineStart
        trackPairs <- state.TrackPairs
