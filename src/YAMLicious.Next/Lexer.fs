namespace YAMLicious

open System.Text

/// The context-directed lexer.
///
/// The parser pulls one lexeme at a time with `Lexer.next`, and tells the lexer which context the next
/// lexeme stands in. The lexer owns the lexical terminals: indicators, properties, aliases, comments,
/// directives, document markers and the cooked value of every scalar. It never decides which node is a
/// key, which collection an entry belongs to or which node a comment belongs to. It builds no
/// reconstructed text and no placeholder.
///
/// Cost. Every routine scans forward through the text. A plain scalar looks ahead over the empty lines
/// behind its last line to see whether the scalar goes on, and the lookahead stops at the first line that
/// ends the scalar. The empty lines are scanned a second time as `BlankLine` lexemes, so each character is
/// read a bounded number of times. The marks of the spans come from the tracker of the cursor, which moves
/// forward once through the text. A scalar is materialized once, with one `StringBuilder` where it needs
/// folding or escapes, and no routine uses `Substring`, because that costs time proportional to the start
/// offset in Fable Python.
///
/// Errors. A routine raises `LexFailure` and `next` turns it into `Error`. After an error the cursor is in
/// no defined state, and the parser stops.
module Lexer =

    // ---------------------------------------------------------------------------------------------
    // Characters and text helpers

    let private isBlank (c: char) = c = ' ' || c = '\t'

    let private isBreak (c: char) = c = '\n' || c = '\r'

    let private isFlowIndicator (c: char) =
        c = ',' || c = '[' || c = ']' || c = '{' || c = '}'

    let private isFlowGrammar (grammar: GrammarContext) =
        match grammar with
        | GrammarContext.FlowIn -> true
        | GrammarContext.FlowKey -> true
        | _ -> false

    let private isKeyGrammar (grammar: GrammarContext) =
        match grammar with
        | GrammarContext.BlockKey -> true
        | GrammarContext.FlowKey -> true
        | _ -> false

    /// The index of the first character at or behind `from` that is no blank.
    let private skipBlanks (text: string) (from: int) =
        let mutable p = from
        while p < text.Length && isBlank text.[p] do
            p <- p + 1
        p

    /// The index behind the line break at `k`. A CRLF is one break.
    let private afterBreak (text: string) (k: int) =
        if text.[k] = '\r' && k + 1 < text.Length && text.[k + 1] = '\n' then k + 2 else k + 1

    /// True at the end of the text, at a blank and at a line break.
    let private blankOrEnd (text: string) (k: int) =
        k >= text.Length || isBlank text.[k] || isBreak text.[k]

    /// The index of the line break at or behind `from`, or the length of the text.
    let private lineEnd (text: string) (from: int) =
        let mutable p = from
        while p < text.Length && not (isBreak text.[p]) do
            p <- p + 1
        p

    /// True when `k` is the first character of a line.
    let private atLineStart (cursor: Cursor) (k: int) =
        k <= cursor.Origin || isBreak cursor.Text.[k - 1]

    /// True when a document marker, `---` or `...` with a blank or the end behind it, starts at `k`, which
    /// has to be the first index of a line.
    let private markerAt (text: string) (k: int) =
        if k + 2 >= text.Length then
            false
        else
            let c = text.[k]
            if (c = '-' || c = '.') && text.[k + 1] = c && text.[k + 2] = c then blankOrEnd text (k + 3) else false

    /// A copy of the characters from `first` up to but excluding `finish`.
    let private copyRange (text: string) (first: int) (finish: int) =
        let sb = StringBuilder()
        for k in first .. finish - 1 do
            sb.Append(text.[k]) |> ignore
        sb.ToString()

    let private appendRange (sb: StringBuilder) (text: string) (first: int) (finish: int) =
        for k in first .. finish - 1 do
            sb.Append(text.[k]) |> ignore

    let private appendLineFeeds (sb: StringBuilder) (count: int) =
        for _ in 1 .. count do
            sb.Append('\n') |> ignore

    /// Appends one code point. A .NET string and a JavaScript string hold a code point above U+FFFF as a
    /// surrogate pair, and a Python string holds it as one character.
    let private appendCodePoint (sb: StringBuilder) (codePoint: int) =
        if codePoint < 0x10000 then
            sb.Append(char codePoint) |> ignore
        else
#if FABLE_COMPILER_PYTHON
            sb.Append(PyInterop.fromCodePoint codePoint) |> ignore
#else
#if FABLE_COMPILER
            let offset = codePoint - 0x10000
            sb.Append(char (0xD800 + offset / 0x400)).Append(char (0xDC00 + offset % 0x400)) |> ignore
#else
            sb.Append(System.Char.ConvertFromUtf32 codePoint) |> ignore
#endif
#endif

    /// The value of a hexadecimal digit, or -1.
    let private hexValue (c: char) =
        if c >= '0' && c <= '9' then int c - int '0'
        elif c >= 'a' && c <= 'f' then int c - int 'a' + 10
        elif c >= 'A' && c <= 'F' then int c - int 'A' + 10
        else -1

    let private fail (cursor: Cursor) (code: string) (message: string) (first: int) (finish: int) (related: SourceSpan list) : 'a =
        raise (
            LexFailure
                { Code = code
                  Message = message
                  Span = cursor.SpanOf(first, finish)
                  Related = related }
        )

    // ---------------------------------------------------------------------------------------------
    // Properties and aliases

    /// The end of the anchor or alias name that starts at `from`. A name runs to a blank, a line break or a
    /// flow indicator and may hold every other character, quotes, hashes and colons included.
    let private nameEnd (text: string) (from: int) =
        let mutable p = from
        while p < text.Length && not (isBlank text.[p]) && not (isBreak text.[p]) && not (isFlowIndicator text.[p]) do
            p <- p + 1
        p

    let private isWordChar (c: char) =
        (c >= '0' && c <= '9') || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '-'

    /// A character of a shorthand tag besides the word characters and the `%` escape: the URI characters
    /// without `!` and the flow indicators.
    let private isTagPunctuation (c: char) =
        c = '#' || c = ';' || c = '/' || c = '?' || c = ':' || c = '@' || c = '&' || c = '=' || c = '+'
        || c = '$' || c = '_' || c = '.' || c = '~' || c = '*' || c = '\'' || c = '(' || c = ')'

    /// The end of the suffix of a shorthand tag that starts at `from`. A `%` has to be followed by two hexadecimal digits.
    let private tagSuffixEnd (cursor: Cursor) (from: int) =
        let text = cursor.Text
        let n = text.Length
        let mutable p = from
        let mutable scanning = true
        while scanning && p < n do
            let c = text.[p]
            if isWordChar c || isTagPunctuation c then
                p <- p + 1
            elif c = '%' then
                if p + 2 < n && hexValue text.[p + 1] >= 0 && hexValue text.[p + 2] >= 0 then
                    p <- p + 3
                else
                    fail cursor "invalid-tag" "A `%` in a tag has to be followed by two hexadecimal digits." p (min n (p + 3)) []
            else
                scanning <- false
        p

    /// Scans the tag that starts with the `!` at `q`. Returns the tag and the index behind it.
    let private scanTag (cursor: Cursor) (q: int) : TagToken * int =
        let text = cursor.Text
        let n = text.Length
        if q + 1 < n && text.[q + 1] = '<' then
            let mutable p = q + 2
            while p < n && text.[p] <> '>' && not (isBlank text.[p]) && not (isBreak text.[p]) do
                p <- p + 1
            if p >= n || text.[p] <> '>' then
                fail cursor "invalid-tag" "A verbatim tag needs a closing `>`." q p []
            if p = q + 2 then
                fail cursor "invalid-tag" "A verbatim tag needs a name between the angle brackets." q (p + 1) []
            TagToken.Verbatim(copyRange text (q + 2) p), p + 1
        elif q + 1 < n && text.[q + 1] = '!' then
            let finish = tagSuffixEnd cursor (q + 2)
            if finish = q + 2 then
                fail cursor "invalid-tag" "A tag needs a name behind its handle." q finish []
            TagToken.Shorthand("!!", copyRange text (q + 2) finish), finish
        else
            let mutable w = q + 1
            while w < n && isWordChar text.[w] do
                w <- w + 1
            if w > q + 1 && w < n && text.[w] = '!' then
                let finish = tagSuffixEnd cursor (w + 1)
                if finish = w + 1 then
                    fail cursor "invalid-tag" "A tag needs a name behind its handle." q finish []
                TagToken.Shorthand(copyRange text q (w + 1), copyRange text (w + 1) finish), finish
            else
                let finish = tagSuffixEnd cursor (q + 1)
                TagToken.Shorthand("!", copyRange text (q + 1) finish), finish

    // ---------------------------------------------------------------------------------------------
    // Directives

    /// The number that the digits of `text` between `first` and `finish` spell, or -1 when they are no
    /// digits or too many.
    let private digitsValue (text: string) (first: int) (finish: int) =
        if finish <= first || finish - first > 9 then
            -1
        else
            let mutable value = 0
            let mutable valid = true
            for k in first .. finish - 1 do
                let c = text.[k]
                if c >= '0' && c <= '9' then value <- value * 10 + (int c - int '0') else valid <- false
            if valid then value else -1

    /// Scans the directive line that starts with the `%` at `q`. It ends at the line break or in front of a comment.
    let private scanDirective (cursor: Cursor) (q: int) : DirectiveKind * int =
        let text = cursor.Text
        let n = text.Length
        let mutable p = q + 1
        let mutable scanning = true
        while scanning && p < n do
            let c = text.[p]
            if isBreak c then scanning <- false
            elif c = '#' && isBlank text.[p - 1] then scanning <- false
            else p <- p + 1
        let mutable finish = p
        while finish > q + 1 && isBlank text.[finish - 1] do
            finish <- finish - 1
        let starts = ResizeArray<int>()
        let ends = ResizeArray<int>()
        let mutable k = q + 1
        while k < finish do
            let s = skipBlanks text k
            if s >= finish then
                k <- finish
            else
                let mutable w = s
                while w < finish && not (isBlank text.[w]) do
                    w <- w + 1
                starts.Add s
                ends.Add w
                k <- w
        if starts.Count = 0 then
            fail cursor "invalid-directive" "A directive needs a name behind the `%`." q (q + 1) []
        let directiveName = copyRange text starts.[0] ends.[0]
        let arguments = starts.Count - 1
        if directiveName = "YAML" then
            if arguments <> 1 then
                fail cursor "invalid-directive" "The %YAML directive needs exactly one version." q finish []
            let first = starts.[1]
            let last = ends.[1]
            let mutable dot = first
            while dot < last && text.[dot] <> '.' do
                dot <- dot + 1
            let major = digitsValue text first dot
            let minor = digitsValue text (dot + 1) last
            if dot >= last || major < 0 || minor < 0 then
                fail cursor "invalid-directive" "The version of the %YAML directive has the form 1.2." first last []
            DirectiveKind.YamlVersion(major, minor), finish
        elif directiveName = "TAG" then
            if arguments <> 2 then
                fail cursor "invalid-directive" "The %TAG directive needs a handle and a prefix." q finish []
            let handleFirst = starts.[1]
            let handleLast = ends.[1]
            // A handle is `!`, `!!` or `!word!`.
            let mutable valid = text.[handleFirst] = '!'
            if valid && handleLast - handleFirst > 1 then
                valid <- text.[handleLast - 1] = '!'
                for j in handleFirst + 1 .. handleLast - 2 do
                    if not (isWordChar text.[j]) then valid <- false
            if not valid then
                fail cursor "invalid-directive" "A tag handle is `!`, `!!` or a word between two `!`." handleFirst handleLast []
            DirectiveKind.TagDirective(copyRange text handleFirst handleLast, copyRange text starts.[2] ends.[2]), finish
        else
            let words = ResizeArray<string>()
            for j in 1 .. starts.Count - 1 do
                words.Add(copyRange text starts.[j] ends.[j])
            DirectiveKind.ReservedDirective(directiveName, List.ofSeq words), finish

    // ---------------------------------------------------------------------------------------------
    // Plain scalars

    /// Scans one line of a plain scalar from `from`. Returns the end of its text, without the blanks behind
    /// it, and the index where the scan stopped: the line break, the end of the text, or the character that
    /// ends the scalar. A colon ends the scalar when a blank, a line break or the end follows it, and in a
    /// flow collection also a flow indicator. A hash that follows a blank starts a comment. In a flow
    /// collection a flow indicator ends the scalar.
    let private plainLine (text: string) (from: int) (flow: bool) : int * int =
        let n = text.Length
        let mutable p = from
        let mutable textEnd = from
        let mutable scanning = true
        while scanning && p < n do
            let c = text.[p]
            if isBreak c then
                scanning <- false
            elif isBlank c then
                p <- p + 1
            elif c = ':' then
                if blankOrEnd text (p + 1) || (flow && isFlowIndicator text.[p + 1]) then
                    scanning <- false
                else
                    p <- p + 1
                    textEnd <- p
            elif c = '#' then
                if p > from && isBlank text.[p - 1] then
                    scanning <- false
                else
                    p <- p + 1
                    textEnd <- p
            elif flow && isFlowIndicator c then
                scanning <- false
            else
                p <- p + 1
                textEnd <- p
        textEnd, p

    /// Scans the plain scalar that starts at `start`. Returns its folded value and the end of its last line.
    ///
    /// Where the context allows more than one line, a line behind the scalar continues it when it is
    /// indented deeper than the parent, does not start a document marker or a comment, and does not start
    /// with a colon that ends the scalar or, in a flow collection, a flow indicator. The empty lines in
    /// between fold into line feeds, and a single line break folds into a space.
    let private scanPlain (ctx: LexContext) (cursor: Cursor) (start: int) : string * int =
        let text = cursor.Text
        let n = text.Length
        let flow = isFlowGrammar ctx.Grammar
        let singleLine = ctx.SimpleKey || isKeyGrammar ctx.Grammar
        let firstEnd, firstStop = plainLine text start flow
        let mutable textEnd = firstEnd
        let mutable builder: StringBuilder = null
        let mutable stop = firstStop
        let mutable more = (not singleLine) && (stop >= n || isBreak text.[stop])
        while more do
            if stop >= n then
                more <- false
            else
                // Over the line break and the empty lines to the next line with content.
                let mutable r = afterBreak text stop
                let mutable empties = 0
                let mutable contentAt = -1
                let mutable looking = true
                while looking do
                    let s = skipBlanks text r
                    if s >= n then
                        looking <- false
                    elif isBreak text.[s] then
                        empties <- empties + 1
                        r <- afterBreak text s
                    else
                        contentAt <- s
                        looking <- false
                if contentAt < 0 then
                    more <- false
                else
                    let mutable spaces = r
                    while text.[spaces] = ' ' do
                        spaces <- spaces + 1
                    let c = text.[contentAt]
                    let ends =
                        markerAt text r
                        || spaces - r <= ctx.ParentIndent
                        || c = '#'
                        || (c = ':' && (blankOrEnd text (contentAt + 1) || (flow && isFlowIndicator text.[contentAt + 1])))
                        || (flow && isFlowIndicator c)
                    if ends then
                        more <- false
                    else
                        let lineTextEnd, lineStop = plainLine text contentAt flow
                        if isNull builder then
                            builder <- StringBuilder()
                            appendRange builder text start firstEnd
                        if empties = 0 then builder.Append(' ') |> ignore else appendLineFeeds builder empties
                        appendRange builder text contentAt lineTextEnd
                        textEnd <- lineTextEnd
                        stop <- lineStop
                        more <- stop >= n || isBreak text.[stop]
        let value = if isNull builder then copyRange text start firstEnd else builder.ToString()
        value, textEnd

    // ---------------------------------------------------------------------------------------------
    // Quoted scalars

    /// The longest implicit key, in characters (YAML 1.2.2, 7.4.2).
    [<Literal>]
    let private MaxKeyLength = 1024

    /// Consumes the line break at `p` inside a quoted scalar together with the empty lines behind it. Returns
    /// the number of empty lines and the index of the first character with content on the next line, or the
    /// end of the text. A document marker at the start of a line is an error, and so is a line that is not
    /// indented deeper than the parent.
    let private foldBreak (ctx: LexContext) (cursor: Cursor) (opening: SourceSpan) (p: int) : int * int =
        let text = cursor.Text
        let n = text.Length
        let mutable r = afterBreak text p
        let mutable empties = 0
        let mutable result = -1
        while result < 0 do
            if markerAt text r then
                fail cursor "marker-in-quote" "A document marker cannot stand inside a quoted scalar." r (r + 3) [ opening ]
            let s = skipBlanks text r
            if s >= n then
                result <- s
            elif isBreak text.[s] then
                empties <- empties + 1
                r <- afterBreak text s
            else
                let mutable spaces = r
                while text.[spaces] = ' ' do
                    spaces <- spaces + 1
                if spaces - r <= ctx.ParentIndent then
                    fail cursor "quoted-indent" "A line of a quoted scalar has to be indented deeper than its parent." s (s + 1) [ opening ]
                result <- s
        empties, result

    /// Reads `count` hexadecimal digits at `first`. Returns the value, or -1.
    let private readHex (text: string) (first: int) (count: int) =
        if first + count > text.Length then
            -1
        else
            let mutable value = 0
            let mutable valid = true
            for k in first .. first + count - 1 do
                let digit = hexValue text.[k]
                if digit < 0 then valid <- false else value <- value * 16 + digit
            if valid then value else -1

    /// Decodes the escape that starts with the backslash at `p` in a double-quoted scalar and appends its
    /// text. Returns the index behind the escape. An escaped line break returns the index of the next
    /// character with content.
    let private scanEscape (ctx: LexContext) (cursor: Cursor) (opening: SourceSpan) (sb: StringBuilder) (p: int) (singleLine: bool) : int =
        let text = cursor.Text
        let n = text.Length
        if p + 1 >= n then
            raise (
                LexFailure
                    { Code = "unclosed-quote"
                      Message = sprintf "The quoted scalar that starts on line %d is not closed." opening.Start.Line
                      Span = opening
                      Related = [] }
            )
        let e = text.[p + 1]
        match e with
        | '0' -> sb.Append('\u0000') |> ignore; p + 2
        | 'a' -> sb.Append('\u0007') |> ignore; p + 2
        | 'b' -> sb.Append('\u0008') |> ignore; p + 2
        | 't' -> sb.Append('\t') |> ignore; p + 2
        | '\t' -> sb.Append('\t') |> ignore; p + 2
        | 'n' -> sb.Append('\n') |> ignore; p + 2
        | 'v' -> sb.Append('\u000B') |> ignore; p + 2
        | 'f' -> sb.Append('\u000C') |> ignore; p + 2
        | 'r' -> sb.Append('\r') |> ignore; p + 2
        | 'e' -> sb.Append('\u001B') |> ignore; p + 2
        | ' ' -> sb.Append(' ') |> ignore; p + 2
        | '"' -> sb.Append('"') |> ignore; p + 2
        | '/' -> sb.Append('/') |> ignore; p + 2
        | '\\' -> sb.Append('\\') |> ignore; p + 2
        | 'N' -> sb.Append('\u0085') |> ignore; p + 2
        | '_' -> sb.Append('\u00A0') |> ignore; p + 2
        | 'L' -> sb.Append('\u2028') |> ignore; p + 2
        | 'P' -> sb.Append('\u2029') |> ignore; p + 2
        | 'x' ->
            let value = readHex text (p + 2) 2
            if value < 0 then
                fail cursor "invalid-escape" "The escape \\x needs two hexadecimal digits." p (min n (p + 4)) [ opening ]
            sb.Append(char value) |> ignore
            p + 4
        | 'u' ->
            let value = readHex text (p + 2) 4
            if value < 0 then
                fail cursor "invalid-escape" "The escape \\u needs four hexadecimal digits." p (min n (p + 6)) [ opening ]
            // Two escapes that spell a surrogate pair are one code point on every target.
            let low = if value >= 0xD800 && value <= 0xDBFF && p + 11 < n && text.[p + 6] = '\\' && text.[p + 7] = 'u' then readHex text (p + 8) 4 else -1
            if low >= 0xDC00 && low <= 0xDFFF then
                appendCodePoint sb (0x10000 + (value - 0xD800) * 0x400 + (low - 0xDC00))
                p + 12
            elif value >= 0xD800 && value <= 0xDFFF then
                // A lone surrogate is no character, and a Python string cannot hold it through a StringBuilder.
                fail cursor "invalid-escape" "The escape \\u names a surrogate that no second escape completes to a character." p (p + 6) [ opening ]
            else
                sb.Append(char value) |> ignore
                p + 6
        | 'U' ->
            let value = readHex text (p + 2) 8
            if value < 0 then
                fail cursor "invalid-escape" "The escape \\U needs eight hexadecimal digits." p (min n (p + 10)) [ opening ]
            if value > 0x10FFFF || (value >= 0xD800 && value <= 0xDFFF) then
                fail cursor "invalid-escape" "The escape \\U does not name a Unicode scalar value." p (p + 10) [ opening ]
            appendCodePoint sb value
            p + 10
        | '\n' | '\r' ->
            if singleLine then
                fail cursor "implicit-key-multiline" "An implicit key has to stay on one line." p (p + 2) []
            let empties, next = foldBreak ctx cursor opening (p + 1)
            appendLineFeeds sb empties
            next
        | _ ->
            fail cursor "invalid-escape" "The backslash starts an escape that YAML does not define." p (p + 2) [ opening ]

    /// Scans the quoted scalar whose opening quote stands at `q`. Returns the cooked value and the index of
    /// the closing quote.
    ///
    /// A line break folds like it does in a plain scalar: the blanks in front of the break and behind it go
    /// away, and the break becomes a space or, with empty lines behind it, one line feed for each empty
    /// line. Only the literal blanks in front of a break go away, so a blank that an escape produced stays.
    let private scanQuoted (ctx: LexContext) (cursor: Cursor) (q: int) : string * int =
        let text = cursor.Text
        let n = text.Length
        let quote = text.[q]
        let isDouble = quote = '"'
        let opening = cursor.SpanOf(q, q + 1)
        let singleLine = ctx.SimpleKey || isKeyGrammar ctx.Grammar
        let sb = StringBuilder()
        let mutable p = q + 1
        let mutable runStart = p
        let mutable closed = false
        while not closed do
            if p >= n then
                fail cursor "unclosed-quote" (sprintf "The quoted scalar that starts on line %d is not closed." opening.Start.Line) q (q + 1) []
            let c = text.[p]
            if c = quote then
                if (not isDouble) && p + 1 < n && text.[p + 1] = '\'' then
                    appendRange sb text runStart p
                    sb.Append('\'') |> ignore
                    p <- p + 2
                    runStart <- p
                else
                    appendRange sb text runStart p
                    closed <- true
            elif isDouble && c = '\\' then
                appendRange sb text runStart p
                p <- scanEscape ctx cursor opening sb p singleLine
                runStart <- p
            elif isBlank c then
                let r = skipBlanks text p
                if r < n && isBreak text.[r] then
                    appendRange sb text runStart p
                    p <- r
                    runStart <- r
                else
                    p <- r
            elif isBreak c then
                if singleLine then
                    fail cursor "implicit-key-multiline" "An implicit key has to stay on one line." q p []
                appendRange sb text runStart p
                let empties, next = foldBreak ctx cursor opening p
                if empties = 0 then sb.Append(' ') |> ignore else appendLineFeeds sb empties
                p <- next
                runStart <- next
            else
                p <- p + 1
        sb.ToString(), p

    // ---------------------------------------------------------------------------------------------
    // Block scalars

    /// Scans the block scalar whose indicator, `|` or `>`, stands at `q`. Returns the scalar, the index of
    /// the end of its last content line (or of its header) and sets the cursor to the start of the line
    /// that ends the body.
    ///
    /// The content indentation is `ParentIndent` plus the indentation digit, counting a top-level parent as
    /// 0. Without a digit it is the number of spaces of the first line that holds more than spaces, but at
    /// least `ParentIndent + 1`, and the lines of spaces in front of that line may not be longer. The body
    /// is the run of lines that are indented at least as deep as the content, with the lines of spaces
    /// among them. A line of spaces is empty when it is no longer than the indentation and holds the
    /// spaces beyond it otherwise. A document marker ends the body.
    let private scanBlockScalar (ctx: LexContext) (cursor: Cursor) (q: int) : YamlScalar * int =
        let text = cursor.Text
        let n = text.Length
        let literal = text.[q] = '|'

        // The header: an indentation digit and a chomping sign in either order, then a comment.
        let mutable p = q + 1
        let mutable chomping = ChompingMode.Clip
        let mutable chompingSet = false
        let mutable digit = 0
        let mutable inHeader = true
        while inHeader && p < n do
            let c = text.[p]
            if c >= '1' && c <= '9' then
                if digit <> 0 then
                    fail cursor "invalid-block-header" "A block scalar header holds one indentation digit." p (p + 1) []
                digit <- int c - int '0'
                p <- p + 1
            elif c = '-' || c = '+' then
                if chompingSet then
                    fail cursor "invalid-block-header" "A block scalar header holds one chomping sign." p (p + 1) []
                chompingSet <- true
                chomping <- if c = '-' then ChompingMode.Strip else ChompingMode.Keep
                p <- p + 1
            else
                inHeader <- false
        let headerEnd = p
        let afterBlanks = skipBlanks text p
        let mutable headerComment: YamlComment option = None
        let mutable headerLineEnd = afterBlanks
        if afterBlanks < n && not (isBreak text.[afterBlanks]) then
            if text.[afterBlanks] = '#' then
                if afterBlanks = headerEnd then
                    fail cursor "glued-comment" "A comment needs a space or a tab before the #." afterBlanks (afterBlanks + 1) []
                let commentEnd = lineEnd text afterBlanks
                headerComment <-
                    Some
                        { Text = copyRange text (afterBlanks + 1) commentEnd
                          Span = Some (cursor.SpanOf(afterBlanks, commentEnd)) }
                headerLineEnd <- commentEnd
            else
                let mutable bad = afterBlanks
                while bad < n && not (isBlank text.[bad]) && not (isBreak text.[bad]) do
                    bad <- bad + 1
                fail cursor "invalid-block-header" "A block scalar header allows only an indentation digit and a chomping sign behind the indicator." q bad []
        let bodyStart = if headerLineEnd >= n then n else afterBreak text headerLineEnd

        // The content indentation.
        let mutable indent = 0
        if digit > 0 then
            indent <- (if ctx.ParentIndent < 0 then 0 else ctx.ParentIndent) + digit
        else
            let mutable r = bodyStart
            let mutable longestEmpty = 0
            let mutable firstSpaces = -1
            let mutable seeking = true
            while seeking && r < n do
                let le = lineEnd text r
                let mutable spaces = r
                while spaces < le && text.[spaces] = ' ' do
                    spaces <- spaces + 1
                if spaces = le then
                    if le - r > longestEmpty then longestEmpty <- le - r
                    r <- if le >= n then n else afterBreak text le
                elif markerAt text r || spaces - r <= ctx.ParentIndent then
                    // A line that is no deeper than the parent ends the scalar, so the scalar has no content.
                    seeking <- false
                else
                    firstSpaces <- spaces - r
                    seeking <- false
            indent <- max (ctx.ParentIndent + 1) (if firstSpaces >= 0 then firstSpaces else longestEmpty)
            if firstSpaces >= 0 && longestEmpty > indent then
                fail
                    cursor
                    "leading-empty-line"
                    (sprintf "A line of spaces in front of the first content line of a block scalar has %d spaces, more than the %d spaces of the content." longestEmpty indent)
                    bodyStart
                    r
                    []

        // The body. The lines are cooked while they are read. `empties` counts the empty lines since the last
        // content line, and the line feeds that the end of the value owes depend on it and on the chomping.
        let sb = StringBuilder()
        let mutable r = bodyStart
        let mutable hasContent = false
        let mutable empties = 0
        let mutable previousMoreIndented = false
        let mutable lastContentEnd = headerEnd
        let mutable reading = true
        while reading && r < n do
            let le = lineEnd text r
            let mutable spaces = r
            while spaces < le && text.[spaces] = ' ' do
                spaces <- spaces + 1
            let onlySpaces = spaces = le
            if markerAt text r then
                reading <- false
            elif (not onlySpaces) && spaces - r < indent then
                // A comment or a line that is no deeper than the parent ends the scalar. Any other line is
                // deeper than the node of the scalar without belonging to it.
                if text.[spaces] <> '#' && spaces - r > ctx.ParentIndent then
                    fail
                        cursor
                        "block-indent"
                        "A line that is more indented than the node of a block scalar, but not enough to continue the scalar, is invalid."
                        spaces
                        (spaces + 1)
                        []
                reading <- false
            else
                if onlySpaces && le - r <= indent then
                    empties <- empties + 1
                else
                    let first = r + indent
                    let moreIndented = first < le && (text.[first] = ' ' || text.[first] = '\t')
                    if literal then
                        // Every line owes the value a line feed, and the empty lines in front of this one have been owed too.
                        appendLineFeeds sb (if hasContent then empties + 1 else empties)
                    elif not hasContent then
                        appendLineFeeds sb empties
                    elif previousMoreIndented || moreIndented then
                        appendLineFeeds sb (empties + 1)
                    elif empties = 0 then
                        sb.Append(' ') |> ignore
                    else
                        appendLineFeeds sb empties
                    appendRange sb text first le
                    hasContent <- true
                    previousMoreIndented <- moreIndented
                    lastContentEnd <- le
                    empties <- 0
                r <- if le >= n then n else afterBreak text le
        // The line feed that ends the last content line and the empty lines behind it: none when stripped,
        // one when clipped, and all of them when kept.
        let feeds =
            match chomping with
            | ChompingMode.Strip -> 0
            | ChompingMode.Clip -> if hasContent then 1 else 0
            | ChompingMode.Keep -> if hasContent then empties + 1 else empties
        appendLineFeeds sb feeds
        cursor.Index <- r
        cursor.LineBegin <- true
        let style = if literal then BlockScalarStyle.Literal else BlockScalarStyle.Folded
        let indicator = if digit > 0 then Some digit else None
        { Value = sb.ToString()
          Style = ScalarStyle.Block(style, chomping, indicator)
          HeaderComment = headerComment },
        lastContentEnd

    // ---------------------------------------------------------------------------------------------
    // Lexemes

    /// The lexeme with the range that starts at `start` and ends in front of `finish`.
    let private located (cursor: Cursor) (start: SourceMark) (finish: int) (lexeme: Lexeme) : Located<Lexeme> =
        { Value = lexeme
          Span = { Start = start; Finish = cursor.MarkAt finish } }

    /// A lexeme of one character at `q`.
    let private single (cursor: Cursor) (q: int) (start: SourceMark) (lexeme: Lexeme) : Located<Lexeme> =
        cursor.Index <- q + 1
        located cursor start (q + 1) lexeme

    /// Rejects a block indicator behind a tab. YAML 1.2.2 allows no tab in the indentation of a line, and
    /// no tab between an indicator and a block collection that starts behind it. `separation` is the
    /// index where the blanks in front of the indicator start.
    let private checkIndicatorTab (cursor: Cursor) (q: int) (tab: bool) (separation: int) =
        if cursor.FirstOnLine && cursor.LeadingTab then
            let mutable lineBegin = q
            while lineBegin > cursor.Origin && not (isBreak cursor.Text.[lineBegin - 1]) do
                lineBegin <- lineBegin - 1
            fail cursor "tab-indentation" "A tab cannot be part of the indentation of a line." lineBegin q []
        elif tab && cursor.AfterIndicator then
            fail cursor "tab-indentation" "A tab cannot separate a block collection from the indicator in front of it." separation q []

    /// Scans the lexeme that starts at `q`, a character that is no blank, no line break and no comment.
    /// `separation` is the index where the blanks in front of it start, and `tab` tells whether they hold a tab.
    let private scanToken (ctx: LexContext) (cursor: Cursor) (q: int) (separation: int) (tab: bool) : Located<Lexeme> =
        let text = cursor.Text
        let n = text.Length
        let c = text.[q]
        let flow = isFlowGrammar ctx.Grammar
        let start = cursor.MarkAt q
        if (c = '-' || c = '.') && q + 2 < n && text.[q + 1] = c && text.[q + 2] = c && blankOrEnd text (q + 3) && atLineStart cursor q then
            cursor.Index <- q + 3
            located cursor start (q + 3) (if c = '-' then Lexeme.DocumentStart else Lexeme.DocumentEnd)
        elif c = '-' && (blankOrEnd text (q + 1) || (flow && isFlowIndicator text.[q + 1])) then
            if flow then
                fail cursor "flow-dash" "Inside a flow collection a `-` needs a plain character behind it." q (q + 1) []
            checkIndicatorTab cursor q tab separation
            single cursor q start Lexeme.BlockEntry
        elif c = '?' && (blankOrEnd text (q + 1) || (flow && isFlowIndicator text.[q + 1])) then
            if not flow then checkIndicatorTab cursor q tab separation
            single cursor q start Lexeme.ExplicitKey
        elif c = ':' && (blankOrEnd text (q + 1) || (flow && (isFlowIndicator text.[q + 1] || q = cursor.JsonLikeEnd))) then
            if not flow then checkIndicatorTab cursor q tab separation
            single cursor q start (Lexeme.Value(q = cursor.JsonLikeEnd))
        elif c = ',' then
            if not flow then
                fail cursor "reserved-indicator" "A plain scalar cannot start with a `,`." q (q + 1) []
            single cursor q start Lexeme.FlowEntry
        elif c = '[' then
            single cursor q start Lexeme.FlowSequenceStart
        elif c = '{' then
            single cursor q start Lexeme.FlowMappingStart
        elif c = ']' || c = '}' then
            if not flow then
                fail cursor "unopened-flow" (sprintf "A `%c` closes no flow collection." c) q (q + 1) []
            cursor.JsonLikeEnd <- q + 1
            single cursor q start (if c = ']' then Lexeme.FlowSequenceEnd else Lexeme.FlowMappingEnd)
        elif c = '&' then
            let finish = nameEnd text (q + 1)
            if finish = q + 1 then
                fail cursor "empty-anchor" "An anchor needs a name behind the `&`." q (q + 1) []
            cursor.Index <- finish
            located cursor start finish (Lexeme.Anchor(copyRange text (q + 1) finish))
        elif c = '*' then
            let finish = nameEnd text (q + 1)
            if finish = q + 1 then
                fail cursor "empty-alias" "An alias needs a name behind the `*`." q (q + 1) []
            cursor.Index <- finish
            located cursor start finish (Lexeme.Alias(copyRange text (q + 1) finish))
        elif c = '!' then
            let tag, finish = scanTag cursor q
            cursor.Index <- finish
            located cursor start finish (Lexeme.Tag tag)
        elif c = '|' || c = '>' then
            if flow || ctx.Grammar = GrammarContext.BlockKey then
                fail cursor "block-scalar-position" "A block scalar cannot start inside a flow collection or as an implicit key." q (q + 1) []
            let scalar, finish = scanBlockScalar ctx cursor q
            located cursor start finish (Lexeme.Scalar scalar)
        elif c = '\'' || c = '"' then
            let value, closing = scanQuoted ctx cursor q
            cursor.Index <- closing + 1
            cursor.JsonLikeEnd <- closing + 1
            let finish = cursor.MarkAt(closing + 1)
            if (ctx.SimpleKey || isKeyGrammar ctx.Grammar) && finish.Column - start.Column > MaxKeyLength then
                fail cursor "implicit-key-length" "An implicit key can have at most 1024 characters." q (closing + 1) []
            let style = if c = '"' then ScalarStyle.DoubleQuoted else ScalarStyle.SingleQuoted
            { Value =
                Lexeme.Scalar
                    { Value = value
                      Style = style
                      HeaderComment = None }
              Span = { Start = start; Finish = finish } }
        elif c = '%' then
            if atLineStart cursor q && cursor.InPrelude then
                let kind, finish = scanDirective cursor q
                cursor.Index <- finish
                located cursor start finish (Lexeme.Directive kind)
            else
                fail cursor "reserved-indicator" "A plain scalar cannot start with a `%`." q (q + 1) []
        elif c = '@' || c = '`' then
            fail cursor "reserved-indicator" (sprintf "A plain scalar cannot start with a `%c`." c) q (q + 1) []
        elif c = '#' then
            if q = cursor.JsonLikeEnd then
                fail cursor "glued-comment" "A comment needs a space or a tab before the #." q (q + 1) []
            else
                fail cursor "reserved-indicator" "A plain scalar cannot start with a `#`." q (q + 1) []
        else
            let value, finish = scanPlain ctx cursor q
            cursor.Index <- finish
            let finishMark = cursor.MarkAt finish
            if (ctx.SimpleKey || isKeyGrammar ctx.Grammar) && finishMark.Column - start.Column > MaxKeyLength then
                fail cursor "implicit-key-length" "An implicit key can have at most 1024 characters." q finish []
            { Value =
                Lexeme.Scalar
                    { Value = value
                      Style = ScalarStyle.Plain
                      HeaderComment = None }
              Span = { Start = start; Finish = finishMark } }

    /// Handles the start of a line that the lexer has not looked at yet. A blank line, a comment line and
    /// the first content line are lexemes. A content line inside a flow collection is checked here and lexed
    /// by the caller, so this returns None for it.
    let private lineBegin (ctx: LexContext) (cursor: Cursor) : Located<Lexeme> option =
        let text = cursor.Text
        let n = text.Length
        let i = cursor.Index
        let mutable q = i
        let mutable tabbed = false
        while q < n && isBlank text.[q] do
            if text.[q] = '\t' then tabbed <- true
            q <- q + 1
        let mutable spaces = i
        while spaces < q && text.[spaces] = ' ' do
            spaces <- spaces + 1
        let indent = spaces - i
        let start = cursor.MarkAt i
        if q >= n then
            // Blanks up to the end of the text, so a line of its own.
            cursor.Index <- q
            cursor.LineBegin <- false
            cursor.SeparationHadTab <- false
            Some (located cursor start q Lexeme.BlankLine)
        elif isBreak text.[q] then
            let lexeme = located cursor start q Lexeme.BlankLine
            cursor.Index <- afterBreak text q
            cursor.SeparationHadTab <- false
            Some lexeme
        elif text.[q] = '#' then
            let finish = lineEnd text q
            let span = cursor.SpanOf(q, finish)
            cursor.Index <- finish
            cursor.LineBegin <- false
            cursor.SeparationHadTab <- tabbed
            let comment: YamlComment =
                { Text = copyRange text (q + 1) finish
                  Span = Some span }
            Some { Value = Lexeme.Comment(comment, true); Span = span }
        elif isFlowGrammar ctx.Grammar then
            let c = text.[q]
            if i = q && markerAt text i then
                fail cursor "marker-in-flow" "A document marker cannot stand inside a flow collection." i (i + 3) []
            let isCloser = c = ']' || c = '}'
            if indent < ctx.ParentIndent || (indent = ctx.ParentIndent && not isCloser) then
                fail cursor "flow-indent" "A line inside a flow collection has to be indented deeper than the node that owns the collection." i (q + 1) []
            cursor.Index <- q
            cursor.LineBegin <- false
            cursor.SeparationHadTab <- false
            None
        else
            cursor.Index <- q
            cursor.LineBegin <- false
            cursor.LeadingTab <- tabbed
            cursor.SeparationHadTab <- tabbed
            Some (located cursor start q (Lexeme.LineStart(indent, tabbed)))

    /// Handles the text from the cursor on within a line: the blanks, a comment, a line break and the next
    /// lexeme. A line break returns None, because the next line starts then.
    let private inLine (ctx: LexContext) (cursor: Cursor) : Located<Lexeme> option =
        let text = cursor.Text
        let n = text.Length
        let i = cursor.Index
        let q = skipBlanks text i
        let mutable tab = false
        for k in i .. q - 1 do
            if text.[k] = '\t' then tab <- true
        cursor.SeparationHadTab <- tab
        if q >= n then
            cursor.Index <- n
            let mark = cursor.MarkAt n
            Some { Value = Lexeme.StreamEnd; Span = { Start = mark; Finish = mark } }
        elif isBreak text.[q] then
            cursor.Index <- afterBreak text q
            cursor.LineBegin <- true
            None
        elif text.[q] = '#' && q > i then
            let finish = lineEnd text q
            let span = cursor.SpanOf(q, finish)
            cursor.Index <- finish
            cursor.SeparationHadTab <- tab
            let comment: YamlComment =
                { Text = copyRange text (q + 1) finish
                  Span = Some span }
            Some { Value = Lexeme.Comment(comment, false); Span = span }
        else
            cursor.Index <- q
            cursor.SeparationHadTab <- tab
            Some (scanToken ctx cursor q i tab)

    /// The lexeme at the cursor, for the context.
    let private lexNext (ctx: LexContext) (cursor: Cursor) : Located<Lexeme> =
        if not cursor.Started then
            cursor.Started <- true
            let mark = cursor.MarkAt cursor.Origin
            { Value = Lexeme.StreamStart; Span = { Start = mark; Finish = mark } }
        else
            let mutable result: Located<Lexeme> option = None
            while Option.isNone result do
                if cursor.LineBegin then
                    if cursor.Index >= cursor.Length then
                        let mark = cursor.MarkAt cursor.Length
                        result <- Some { Value = Lexeme.StreamEnd; Span = { Start = mark; Finish = mark } }
                    else
                        result <- lineBegin ctx cursor
                else
                    result <- inLine ctx cursor
            result.Value

    /// Returns the next lexeme for the context, or the error of the text at the cursor.
    ///
    /// A call moves the cursor behind the lexeme. A call at the end of the text returns `StreamEnd` again.
    /// After an error the cursor is in no defined state.
    let next (ctx: LexContext) (cursor: Cursor) : Result<Located<Lexeme>, YamlError> =
        try
            let lexeme = lexNext ctx cursor
            match lexeme.Value with
            | Lexeme.LineStart _ ->
                cursor.FirstOnLine <- true
                cursor.AfterIndicator <- false
            | Lexeme.BlockEntry ->
                cursor.FirstOnLine <- false
                cursor.AfterIndicator <- true
                cursor.InPrelude <- false
            | Lexeme.ExplicitKey ->
                cursor.FirstOnLine <- false
                cursor.AfterIndicator <- true
                cursor.InPrelude <- false
            | Lexeme.Value _ ->
                cursor.FirstOnLine <- false
                cursor.AfterIndicator <- true
                cursor.InPrelude <- false
            | Lexeme.DocumentEnd ->
                cursor.FirstOnLine <- false
                cursor.AfterIndicator <- false
                cursor.InPrelude <- true
            | Lexeme.Directive _ ->
                cursor.FirstOnLine <- false
                cursor.AfterIndicator <- false
            | Lexeme.Comment _ ->
                cursor.FirstOnLine <- false
                cursor.AfterIndicator <- false
            | Lexeme.BlankLine ->
                cursor.FirstOnLine <- false
                cursor.AfterIndicator <- false
            | Lexeme.StreamStart ->
                cursor.FirstOnLine <- false
                cursor.AfterIndicator <- false
            | Lexeme.StreamEnd ->
                cursor.FirstOnLine <- false
                cursor.AfterIndicator <- false
            | _ ->
                cursor.FirstOnLine <- false
                cursor.AfterIndicator <- false
                cursor.InPrelude <- false
            Ok lexeme
        with LexFailure error ->
            Error error
