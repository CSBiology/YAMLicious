namespace YAMLicious

open System
open System.Collections.Generic
open System.Text

/// What the writer does with the styles that the tree stores.
///
/// `PreferStored` keeps a stored style when it can express the value where the node stands, and picks the
/// nearest valid style when it cannot. `RequireStored` raises `YamlWriteException` when a stored style cannot
/// express the value. `Canonical` ignores the stored styles: a scalar is plain where that is valid and cannot
/// change its meaning under the core schema and double-quoted otherwise, and a collection is a block collection
/// unless it is empty.
///
/// No policy drops a comment or a blank line.
type StylePolicy =
    | PreferStored
    | RequireStored
    | Canonical

/// The options of the writer. `Indent` is the number of spaces for one level of a block collection. It has to be
/// at least 1. `NewLine` ends every line, the last one included, and has to be a line feed, a carriage return
/// or both.
type WriterOptions =
    { Indent: int
      NewLine: string
      Styles: StylePolicy }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module WriterOptions =

    /// Two spaces, "\n" and `PreferStored`.
    let defaults: WriterOptions =
        { Indent = 2
          NewLine = "\n"
          Styles = PreferStored }

/// The reason the writer refuses a tree, and the places that lead to the node. A segment of `Path` is the text of
/// a scalar key, `*name` for an alias key, `?` and the position for any other key, and the position of an item of
/// a sequence. `Message` of an invalid tree holds the messages of `Validate`, one on each line, and its `Path` is
/// empty.
type WriteError = { Message: string; Path: string list }

exception YamlWriteException of WriteError

/// The text rules of scalars. They say which characters stand raw and when a plain scalar is safe, and they spell
/// the escapes of the quoted styles. The functions look at a value one character at a time and build no substring.
module internal WriterText =

    let isBlank (c: char) = c = ' ' || c = '\t'

    let isFlowIndicator (c: char) =
        c = ',' || c = '[' || c = ']' || c = '{' || c = '}'

    let isHighSurrogate (c: char) = int c >= 0xD800 && int c <= 0xDBFF

    let isLowSurrogate (c: char) = int c >= 0xDC00 && int c <= 0xDFFF

    /// True for a character that can stand unescaped in a plain scalar, a block scalar and a comment: an nb-char
    /// ([27]). These are the printable characters of YAML 1.2.2 [1] without the line breaks and the byte order mark.
    /// NEL, NBSP and the two Unicode line separators are among them, because YAML 1.2 ends a line at a line feed or
    /// a carriage return only. A surrogate half is no character of its own, so the writer checks the pairs apart.
    let isRawChar (c: char) = Validate.isNbChar c

    /// True for a character that can stand unescaped in a single-quoted scalar: the tab and every character from
    /// U+0020 up, because a quoted scalar holds nb-json characters ([2], [118]) and single quotes have no escape
    /// that could spell one. This includes DEL, the C1 controls, U+FFFE and the byte order mark.
    let isQuotedChar (c: char) = c = '\t' || int c >= 0x20

    /// U+FEFF. A line that holds only this mark is a blank line that ends a document body ([202], [211]).
    let byteOrderMark: char = char 0xFEFF

    /// The mark of a line behind the root scalar of a document that ends its body: a tab.
    let tabMark: char = '\t'

    /// No mark.
    let noMark: char = char 0

    /// True when the text holds a surrogate half without its partner. A Python string never does.
    let hasLoneSurrogate (text: string) =
        let mutable found = false
        let mutable i = 0
        while not found && i < text.Length do
            let c = text.[i]
            if isHighSurrogate c then
                if i + 1 < text.Length && isLowSurrogate text.[i + 1] then i <- i + 2 else found <- true
            elif isLowSurrogate c then
                found <- true
            else
                i <- i + 1
        found

    let private isSingleChar (c: char) = c = '\n' || isQuotedChar c

    /// True when every character of the text except the line feed can stand raw in single quotes.
    let isSingleQuotable (text: string) = Validate.allChars isSingleChar text

    /// The number of code points of the text, which is the unit of the limit on an implicit key.
    let codePointLength (text: string) =
        let mutable count = 0
        let mutable i = 0
        while i < text.Length do
            if isHighSurrogate text.[i] && i + 1 < text.Length && isLowSurrogate text.[i + 1] then i <- i + 2 else i <- i + 1
            count <- count + 1
        count

    let private isPlainStartIndicator (c: char) =
        c = ',' || c = '[' || c = ']' || c = '{' || c = '}' || c = '#' || c = '&' || c = '*' || c = '!' || c = '|'
        || c = '>' || c = '\'' || c = '"' || c = '%' || c = '@' || c = '`'

    /// True when the value can stand as a plain scalar and read back as the same text.
    ///
    /// `flow` is true inside a flow collection, where the flow indicators end a plain scalar. `multiOk` is true
    /// when the scalar may run over several lines. Each line feed of the value then becomes an empty line, so the
    /// value cannot start or end with one, and no line may start or end with a blank. `atLineStart` is true when
    /// the scalar starts at the first column of a line, where `---` and `...` mean a document marker.
    /// `beforeColon` is true for an implicit key, which the colon of the entry follows. A colon at the end of the
    /// text and a lone `-`, `?` or `:` then read as part of the scalar.
    let canBePlain (text: string) (flow: bool) (multiOk: bool) (atLineStart: bool) (beforeColon: bool) =
        let n = text.Length
        if n = 0 || not (Validate.isBlockText text) then
            false
        else
            let mutable ok = true
            let first = text.[0]
            let last = text.[n - 1]
            if first = '\n' || isBlank first || last = '\n' || isBlank last then ok <- false
            if ok && isPlainStartIndicator first then ok <- false
            if ok && (first = '-' || first = '?' || first = ':') then
                if n = 1 then
                    if not beforeColon then ok <- false
                else
                    let next = text.[1]
                    if isBlank next || next = '\n' then ok <- false
                    elif flow && isFlowIndicator next then ok <- false
            if ok && atLineStart && n >= 3 && (text.[0] = '-' || text.[0] = '.') && text.[1] = text.[0] && text.[2] = text.[0] then
                if (n = 3 && not beforeColon) || (n > 3 && (isBlank text.[3] || text.[3] = '\n')) then ok <- false
            let mutable i = 0
            while ok && i < n do
                let c = text.[i]
                if c = '\n' then
                    if not multiOk then ok <- false
                    elif i > 0 && text.[i - 1] <> '\n' && isBlank text.[i - 1] then ok <- false
                    elif i + 1 < n && isBlank text.[i + 1] then ok <- false
                    elif i + 1 < n && text.[i + 1] = '#' then ok <- false
                elif c = ':' then
                    if i + 1 >= n then
                        if not beforeColon then ok <- false
                    else
                        let next = text.[i + 1]
                        if isBlank next || next = '\n' then ok <- false
                        elif flow && isFlowIndicator next then ok <- false
                elif c = '#' then
                    if i > 0 && (isBlank text.[i - 1] || text.[i - 1] = '\n') then ok <- false
                elif flow && isFlowIndicator c then
                    ok <- false
                i <- i + 1
            ok

    /// True when the value can stand in single quotes. A line feed becomes an empty line, so a line of the
    /// value cannot start or end with a blank where a line break trims it.
    let canBeSingle (text: string) (multiOk: bool) =
        if not (isSingleQuotable text) then
            false
        else
            let n = text.Length
            let mutable ok = true
            let mutable i = 0
            while ok && i < n do
                if text.[i] = '\n' then
                    if not multiOk then ok <- false
                    elif i > 0 && text.[i - 1] <> '\n' && isBlank text.[i - 1] then ok <- false
                    elif i + 1 < n && text.[i + 1] <> '\n' && isBlank text.[i + 1] then ok <- false
                i <- i + 1
            ok

    /// The single-quoted spelling of a value without a line feed.
    let renderSingle (text: string) = "'" + text.Replace("'", "''") + "'"

    let private hexDigits = "0123456789ABCDEF"

    let private appendHex (sb: StringBuilder) (value: int) (digits: int) =
        let mutable shift = (digits - 1) * 4
        while shift >= 0 do
            sb.Append(hexDigits.[(value >>> shift) &&& 0xF]) |> ignore
            shift <- shift - 4

    /// The double-quoted spelling of a value. A line feed becomes an escape, so the result is one line. The tab stands
    /// raw. The quote, the backslash, the line breaks, the C0 controls, NEL, NBSP and the line and paragraph separators
    /// have named escapes where YAML has them, which readers of YAML 1.1 keep as well. The other controls get a
    /// hexadecimal escape. DEL, the C1 controls, U+FFFE, U+FFFF and the byte order mark get one too, so a reader that
    /// follows [1] and a person who reads the text see no control character. The remaining printable characters ([1])
    /// stand raw. `verbatim` leaves NEL, NBSP, the separators and those controls raw, which is the shortest spelling
    /// that [2] allows. `escapeFirst` and `escapeLast` escape a blank at the start or at the end of the range, because
    /// a line break in a double-quoted scalar discards the blanks at the end of the line in front of it and at the start
    /// of the line behind it.
    let appendDoubleRange (sb: StringBuilder) (text: string) (first: int) (finish: int) (verbatim: bool) (escapeFirst: bool) (escapeLast: bool) =
        let mutable i = first
        while i < finish do
            let c = text.[i]
            let code = int c
            if (c = ' ' || c = '\t') && ((escapeFirst && i = first) || (escapeLast && i = finish - 1)) then
                sb.Append(if c = ' ' then "\\ " else "\\t") |> ignore
            elif c = '"' then sb.Append("\\\"") |> ignore
            elif c = '\\' then sb.Append("\\\\") |> ignore
            elif c = '\n' then sb.Append("\\n") |> ignore
            elif c = '\r' then sb.Append("\\r") |> ignore
            elif code = 0x00 then sb.Append("\\0") |> ignore
            elif code = 0x07 then sb.Append("\\a") |> ignore
            elif code = 0x08 then sb.Append("\\b") |> ignore
            elif code = 0x0B then sb.Append("\\v") |> ignore
            elif code = 0x0C then sb.Append("\\f") |> ignore
            elif code = 0x1B then sb.Append("\\e") |> ignore
            elif isHighSurrogate c && i + 1 < finish && isLowSurrogate text.[i + 1] then
                sb.Append(c).Append(text.[i + 1]) |> ignore
                i <- i + 1
            elif code = 0x85 && not verbatim then sb.Append("\\N") |> ignore
            elif code = 0xA0 && not verbatim then sb.Append("\\_") |> ignore
            elif code = 0x2028 && not verbatim then sb.Append("\\L") |> ignore
            elif code = 0x2029 && not verbatim then sb.Append("\\P") |> ignore
            elif isRawChar c || (verbatim && isQuotedChar c) then sb.Append(c) |> ignore
            elif code < 0x100 then
                sb.Append("\\x") |> ignore
                appendHex sb code 2
            else
                sb.Append("\\u") |> ignore
                appendHex sb code 4
            i <- i + 1

    /// The double-quoted spelling of a value, see `appendDoubleRange`.
    let renderDouble (text: string) (verbatim: bool) =
        let sb = StringBuilder()
        sb.Append('"') |> ignore
        appendDoubleRange sb text 0 text.Length verbatim false false
        sb.Append('"') |> ignore
        sb.ToString()

    /// ns-tag-char of the YAML 1.2.2 specification ([40], [99]): the word characters and the URI punctuation
    /// without `!` and the flow indicators. `%` is no tag character, because it starts an escape.
    let private isTagChar (c: char) =
        (c >= '0' && c <= '9') || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '-' || c = '#' || c = ';'
        || c = '/' || c = '?' || c = ':' || c = '@' || c = '&' || c = '=' || c = '+' || c = '$' || c = '_' || c = '.'
        || c = '~' || c = '*' || c = '\'' || c = '(' || c = ')'

    let private appendByte (sb: StringBuilder) (value: int) =
        sb.Append('%') |> ignore
        appendHex sb value 2

    /// The suffix of a shorthand tag with every character that is no tag character written as `%XX` for each byte of
    /// its UTF-8 form. The result is None when the text holds a lone surrogate.
    let escapeTagSuffix (text: string) : string option =
        let mutable plain = true
        for c in text do
            if not (isTagChar c) then plain <- false
        if plain then
            Some text
        else
            let sb = StringBuilder()
            let mutable i = 0
            let mutable broken = false
            while not broken && i < text.Length do
                let c = text.[i]
                if isTagChar c then
                    sb.Append(c) |> ignore
                    i <- i + 1
                else
                    let mutable code = int c
                    if isHighSurrogate c then
                        if i + 1 < text.Length && isLowSurrogate text.[i + 1] then
                            code <- 0x10000 + ((int c - 0xD800) <<< 10) + (int text.[i + 1] - 0xDC00)
                            i <- i + 1
                        else
                            broken <- true
                    elif isLowSurrogate c then
                        broken <- true
                    if not broken then
                        if code < 0x80 then
                            appendByte sb code
                        elif code < 0x800 then
                            appendByte sb (0xC0 ||| (code >>> 6))
                            appendByte sb (0x80 ||| (code &&& 0x3F))
                        elif code < 0x10000 then
                            appendByte sb (0xE0 ||| (code >>> 12))
                            appendByte sb (0x80 ||| ((code >>> 6) &&& 0x3F))
                            appendByte sb (0x80 ||| (code &&& 0x3F))
                        else
                            appendByte sb (0xF0 ||| (code >>> 18))
                            appendByte sb (0x80 ||| ((code >>> 12) &&& 0x3F))
                            appendByte sb (0x80 ||| ((code >>> 6) &&& 0x3F))
                            appendByte sb (0x80 ||| (code &&& 0x3F))
                        i <- i + 1
            if broken then None else Some (sb.ToString())

    let private isHexDigit (c: char) =
        (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

    /// True when the text can stand between `!<` and `>`. The parser keeps a verbatim tag as written and decodes no
    /// escape in it, so only the URI characters and an escape that is already there are allowed.
    let isVerbatimText (text: string) =
        let mutable ok = text.Length > 0
        let mutable i = 0
        while ok && i < text.Length do
            let c = text.[i]
            if c = '%' then
                if i + 2 < text.Length && isHexDigit text.[i + 1] && isHexDigit text.[i + 2] then i <- i + 3 else ok <- false
            elif isTagChar c || c = '!' || c = ',' || c = '[' || c = ']' then
                i <- i + 1
            else
                ok <- false
        ok

/// How a scalar is written.
[<RequireQualifiedAccess>]
type internal Shape =
    /// No text. The scalar is empty, and its properties stand alone.
    | Empty
    /// One line. The text is the whole spelling, quotes included.
    | Line of string
    /// A plain scalar over several lines.
    | PlainLines
    /// A single-quoted scalar over several lines.
    | SingleLines
    /// A plain scalar that goes back to the lines of its layout.
    | PlainLayout
    /// A single-quoted scalar that goes back to the lines of its layout.
    | SingleLayout
    /// A double-quoted scalar that goes back to the lines of its layout.
    | DoubleLayout
    /// A literal or folded block scalar.
    | Block

type internal ScalarPlan =
    { Shape: Shape
      IsLiteral: bool
      Chomp: ChompingMode
      /// The indentation indicator of a block scalar, or 0 for none.
      Digit: int
      /// True when the last line of a block scalar with the chomping Clip or Keep has no line break, because the
      /// value has no final line feed and the scalar is the last node of the text.
      Unterminated: bool
      /// The pieces of the layout that the writer replays, or none. A folded block scalar holds its folds in them too.
      Pieces: LayoutPiece[] }

/// The output buffer. It counts the width of the current line, so no code asks the builder for its length.
type internal Out(newLine: string) =
    let sb = StringBuilder()
    let mutable width = 0
    let mutable hasContent = false

    /// The number of characters written on the current line, indentation included.
    member _.Width = width

    /// True when nothing but indentation stands on the current line.
    member _.IsEmptyLine = not hasContent

    member _.Put(text: string) =
        if text.Length > 0 then
            sb.Append(text) |> ignore
            width <- width + text.Length
            hasContent <- true

    member _.PutChar(c: char) =
        sb.Append(c) |> ignore
        width <- width + 1
        hasContent <- true

    member _.Spaces(count: int) =
        if count > 0 then
            sb.Append(String(' ', count)) |> ignore
            width <- width + count

    member _.EndLine() =
        sb.Append(newLine) |> ignore
        width <- 0
        hasContent <- false

    member _.Text() = sb.ToString()

/// What an open block collection waits for.
type internal BlockPhase =
    /// The next entry or item.
    | NextEntry
    /// The colon line of an explicit entry whose key is written.
    | ColonNext

/// What an open flow collection waits for.
type internal FlowPhase =
    /// The next item or entry.
    | NextItem
    /// The colon, the value and what follows, behind the key of an entry.
    | AfterKey
    /// Only the comma, the comment of the last token and the line break are left.
    | ItemWritten

/// The values that a frame holds in front of its first entry or item. A step sets the entry or the node in progress before
/// it reads it, so these values are never read. A frame sets them once for every entry, which an option would cost an
/// allocation for each time.
module internal Placeholders =
    let entry: MappingEntry = YamlNode.entry YamlNode.empty YamlNode.empty
    let node: YamlNode = YamlNode.empty

/// An open block collection. The frames on the stack are the collections that the writer is inside of.
type internal BlockFrame(isMap: bool, col: int, entries: MappingEntry list, items: SequenceItem list, endTrivia: Trivia list, compact: bool) =
    member _.IsMap = isMap
    /// The column of the dashes or of the entries.
    member _.Col = col
    member val Entries = entries with get, set
    member val Items = items with get, set
    member _.End = endTrivia
    /// True when the first entry continues the line of the indicator in front of the collection.
    member _.Compact = compact
    /// The position of the entry in progress, from 0, or -1 before the first one.
    member val Index = -1 with get, set
    member val Phase = BlockPhase.NextEntry with get, set
    /// The entry in progress of a mapping.
    member val Current = Placeholders.entry with get, set
    /// True when the frame still has something to write behind the node in progress, which keeps that node from being
    /// the last one of the text. The engine counts the frames for which this is true.
    member val RulesOutEnd = false with get, set
    /// True when the last entry was an explicit key without a colon. An empty implicit key behind it would read
    /// as the value of that key.
    member val PrevColonless = false with get, set

/// An open flow collection.
type internal FlowFrame(isMap: bool, multi: bool, closeCol: int, itemsCol: int, entries: MappingEntry list, items: SequenceItem list, endTrivia: Trivia list) =
    member _.IsMap = isMap
    /// True when the collection is written with one item on each line.
    member _.Multi = multi
    member _.CloseCol = closeCol
    member _.ItemsCol = itemsCol
    member val Entries = entries with get, set
    member val Items = items with get, set
    member _.End = endTrivia
    member val Index = -1 with get, set
    member val Phase = FlowPhase.NextItem with get, set
    /// The entry in progress of a flow mapping.
    member val Current = Placeholders.entry with get, set
    /// The node of the item in progress in a flow sequence.
    member val Node = Placeholders.node with get, set
    /// True when a line break stands behind the last token, so the next token starts a line.
    member val LineBroken = false with get, set
    /// True for a flow pair, the one-entry mapping that stands in a flow sequence without braces, as in `[a: b]`.
    member val Bare = false with get, set

/// The engine that holds the state of one write. The traversal keeps two explicit stacks, one for the block
/// collections and one for the flow collection that is open inside the current entry, so a tree of any depth
/// writes without recursion. Each node is visited once, each flow subtree is probed once at its outermost
/// collection, and every character of the output is appended once.
type internal Engine(options: WriterOptions) =
    let indent = options.Indent
    let policy = options.Styles
    let newLine = options.NewLine
    let mutable out = Out(newLine)
    let stack = ResizeArray<BlockFrame>()
    let flowStack = ResizeArray<FlowFrame>()
    // One flag for each collection of the flow subtree that is about to be written, in the order of the text. The
    // flag is true when the collection needs one item on each line.
    let flowFlags = ResizeArray<bool>()
    let probe = ResizeArray<YamlNode * int * int>()
    let mutable flowCounter = 0
    // The handle that a %TAG directive of the current document gives to a prefix, and every declared handle.
    let handlesByPrefix = Dictionary<string, string>()
    let declaredHandles = HashSet<string>()
    let mutable partPrefix = ""
    // True while the document in progress is the last part of the stream and nothing follows its root.
    let mutable tailOpen = false
    // True while the first line behind the root of the document in progress that is not blank is a comment line, which
    // stands at the first column. The lines are the ones behind the root up to the next marker, the prelude of the next
    // document included.
    let mutable commentFollowsRoot = false
    // True while the first line that the writer prints behind the root of the document in progress is a blank line.
    let mutable blankFollowsRoot = false
    // True when the root of the document in progress is a block scalar written with the chomping Keep.
    let mutable rootKept = false
    // True while the last thing written is a block scalar with the chomping Keep, so a blank line next would belong to it.
    let mutable keepTail = false
    // True while the writer renders a flow collection that is an implicit key. Such a key takes the shortest spelling that
    // the productions allow, because the limit of 1024 characters counts the spelling.
    let mutable compactFlow = false
    // The number of open block frames that still have something to write behind the node in progress.
    let mutable blockers = 0
    // The trivia that a flow collection leaves behind when the policy writes it as a block collection that something
    // follows (the lines in front of its closer and the comment behind it), each run with the depth of the frame of that
    // block collection. A run is written in front of the next entry or item after the frame is closed.
    let carried = ResizeArray<int * Trivia list>()

    let noBreak: Separation = Separation.NewLine { Comment = None; Following = [] }

    // ---------------------------------------------------------------------------------------------
    // Failures

    member private _.KeyName(key: YamlNode, index: int) : string =
        match key.Kind with
        | YamlNodeKind.Scalar scalar -> scalar.Value
        | YamlNodeKind.Alias name -> "*" + name
        | _ -> "?" + string index

    member private this.Path() : string list =
        let segments = ResizeArray<string>()
        if partPrefix.Length > 0 then segments.Add partPrefix
        for frame in stack do
            if frame.Index >= 0 then
                segments.Add(if frame.IsMap then this.KeyName(frame.Current.Key, frame.Index) else string frame.Index)
        for frame in flowStack do
            if frame.Index >= 0 then
                segments.Add(if frame.IsMap then this.KeyName(frame.Current.Key, frame.Index) else string frame.Index)
        List.ofSeq segments

    member this.Fail<'a>(message: string) : 'a =
        raise (YamlWriteException { Message = message; Path = this.Path() })

    // ---------------------------------------------------------------------------------------------
    // Comments and trivia

    /// A comment behind the text of the line. `Validate` has checked that the text holds nb-char characters only.
    member private this.PutComment(comment: YamlComment) =
        out.Put " #"
        out.Put comment.Text

    member private this.PutOptionalComment(comment: YamlComment option) =
        match comment with
        | Some c -> this.PutComment c
        | None -> ()

    /// Ends the line with the comment behind its text, when there is one.
    member private this.EndLineWith(comment: YamlComment option) =
        this.PutOptionalComment comment
        out.EndLine()

    member private this.WriteTrivia(lines: Trivia list, col: int) =
        let mutable rest = lines
        while not rest.IsEmpty do
            match rest.Head with
            | Trivia.FullLineComment comment ->
                out.Spaces col
                out.Put "#"
                out.Put comment.Text
                out.EndLine()
            | Trivia.BlankLine _ -> out.EndLine()
            rest <- rest.Tail

    /// Writes the trivia behind a block scalar with the chomping Keep when only trivia and markers follow. The empty lines
    /// behind a Keep scalar are part of its value, so a blank line at the start of the trivia is written as a line that
    /// holds `mark`. The parser reads such a line as a blank line that ends the body. The mark is a tab behind the root
    /// scalar of a document and a byte order mark behind a scalar in a collection, where a tab line is no blank line.
    member private this.WriteTriviaBehindKeep(lines: Trivia list, col: int, mark: char) =
        match lines with
        | Trivia.BlankLine _ :: rest ->
            out.PutChar mark
            out.EndLine()
            this.WriteTrivia(rest, col)
        | _ -> this.WriteTrivia(lines, col)

    // ---------------------------------------------------------------------------------------------
    // Properties

    member private this.TagText(tag: YamlTag) : string =
        match tag with
        | YamlTag.NonSpecific -> "!"
        | YamlTag.Named name ->
            match WriterText.escapeTagSuffix name.Suffix with
            | None -> this.Fail "The tag holds a lone surrogate, which is no character."
            | Some suffix ->
                match handlesByPrefix.TryGetValue name.Prefix with
                | true, handle -> handle + suffix
                | _ ->
                    if name.Prefix = YamlTag.coreNamespace && not (declaredHandles.Contains "!!") then
                        "!!" + suffix
                    elif name.Prefix = "!" && not (declaredHandles.Contains "!") then
                        "!" + suffix
                    else
                        let identifier = name.Prefix + name.Suffix
                        if WriterText.isVerbatimText identifier then
                            "!<" + identifier + ">"
                        else
                            this.Fail("The tag " + identifier + " has characters that a verbatim tag cannot hold, and no %TAG directive of the document gives it a handle.")

    member private this.PropertyText(node: YamlNode, property: PropertyName) : string =
        match property with
        | PropertyName.Tag ->
            if node.Properties.Tag.IsSome then this.TagText node.Properties.Tag.Value else ""
        | PropertyName.Anchor ->
            if node.Properties.Anchor.IsSome then "&" + node.Properties.Anchor.Value else ""

    /// The properties on one line, each followed by a blank. It is empty for a node without properties.
    member private this.InlinePropsText(node: YamlNode) : string =
        if node.PropertyLayout.IsEmpty then
            ""
        else
            let sb = StringBuilder()
            for placement in node.PropertyLayout do
                sb.Append(this.PropertyText(node, placement.Property)).Append(' ') |> ignore
            sb.ToString()

    /// Writes the properties of a node. Between two properties the stored separation applies. Behind the last one
    /// the content follows on the same line or on a later one, except for a block collection, whose items always
    /// start on a later line, and for a node without content, which ends with its last property. The line break
    /// behind the last property of a block collection is in the tree already, except for a flow collection that the
    /// policy `Canonical` writes as a block collection.
    member private this.WriteProps(node: YamlNode, ownCol: int, blockLike: bool, propOnly: bool) =
        let mutable rest = node.PropertyLayout
        while not rest.IsEmpty do
            let placement = rest.Head
            out.Put(this.PropertyText(node, placement.Property))
            rest <- rest.Tail
            if rest.IsEmpty && (blockLike || propOnly) then
                let afterProperty =
                    match placement.After with
                    | Separation.NewLine lineBreak -> lineBreak.Comment
                    | Separation.Inline -> None
                // A node with properties only keeps a comment behind it in its TrailingComment while it stands in a flow
                // collection, and the policy Canonical moves such a node to a block position. The comment stands behind the
                // properties then. It goes in front of the next entry or item when the line holds the comment of the line
                // break already.
                let behindNode = if propOnly then node.TrailingComment else None
                match afterProperty, behindNode with
                | Some _, Some comment ->
                    this.PutOptionalComment afterProperty
                    carried.Add((stack.Count + 1, [ Trivia.FullLineComment comment ]))
                | Some _, None -> this.PutOptionalComment afterProperty
                | None, _ -> this.PutOptionalComment behindNode
                if blockLike then out.EndLine()
            else
                match placement.After with
                | Separation.Inline -> out.Put " "
                | Separation.NewLine lineBreak ->
                    this.EndLineWith lineBreak.Comment
                    this.WriteTrivia(lineBreak.Following, ownCol)
                    out.Spaces ownCol

    // ---------------------------------------------------------------------------------------------
    // Planning a scalar

    member private _.ShapePlan(shape: Shape) : ScalarPlan =
        { Shape = shape
          IsLiteral = false
          Chomp = ChompingMode.Clip
          Digit = 0
          Unterminated = false
          Pieces = [||] }

    member private this.LinePlan(text: string) : ScalarPlan = this.ShapePlan(Shape.Line text)

    /// True when nothing but the line break of the current line can follow in the output: the document is the last
    /// part of the stream and ends without a `...` or trivia, every open collection has written its last entry, and no
    /// carried trivia wait for a later line. The engine counts the open frames that still have something to write, so
    /// the answer takes no walk of the stack.
    member private _.AtEndOfText() : bool = tailOpen && flowStack.Count = 0 && blockers = 0 && carried.Count = 0

    /// True when the frame still has an entry, an item or end trivia to write, or when the colon line of the entry
    /// in progress follows. Such a frame keeps the node in progress from being the last one of the text.
    member private _.FrameRulesOutEnd(frame: BlockFrame) : bool =
        if frame.IsMap then
            not frame.Entries.IsEmpty
            || not frame.End.IsEmpty
            || (frame.Phase = BlockPhase.ColonNext && frame.Current.Colon.IsSome)
        else
            not frame.Items.IsEmpty || not frame.End.IsEmpty

    /// Updates the flag of the frame and the count of the frames with the flag. It runs where a frame is pushed, where
    /// it takes its next entry or item and where its phase changes.
    member private this.RefreshFrame(frame: BlockFrame) =
        let now = this.FrameRulesOutEnd frame
        if now <> frame.RulesOutEnd then
            blockers <- blockers + (if now then 1 else -1)
            frame.RulesOutEnd <- now

    /// True when the first line that the writer prints behind the node in progress is a blank line. It is the Before of the
    /// next entry or item, the gap in front of the colon of an explicit key, the End of a collection, or what follows the
    /// root. The walk stops at the first open frame that still has something to write. Such a frame is passed by the node
    /// that ends its collection only, so the walks of all nodes together cost a step for each frame.
    member private _.BlankLineFollows() : bool =
        let mutable decided = false
        let mutable result = false
        let mutable i = stack.Count - 1
        while not decided && i >= 0 do
            let frame = stack.[i]
            if frame.IsMap && frame.Phase = BlockPhase.ColonNext && frame.Current.Colon.IsSome then
                decided <- true
                result <- Validate.startsWithBlankLine frame.Current.BeforeColon
            elif frame.IsMap && not frame.Entries.IsEmpty then
                decided <- true
                result <- Validate.startsWithBlankLine frame.Entries.Head.Before
            elif not frame.IsMap && not frame.Items.IsEmpty then
                decided <- true
                result <- Validate.startsWithBlankLine frame.Items.Head.Before
            elif not frame.End.IsEmpty then
                decided <- true
                result <- Validate.startsWithBlankLine frame.End
            else
                i <- i - 1
        if decided then result else blankFollowsRoot

    member private this.Preview(value: string) : string =
        if value.Length > 40 then "\"" + value.Replace("\n", "\\n").Substring(0, 40) + "...\"" else "\"" + value.Replace("\n", "\\n") + "\""

    /// The style that replaces a stored style that cannot express the value. `RequireStored` raises instead.
    member private this.Fallback(value: string, reason: string) : ScalarPlan =
        if policy = StylePolicy.RequireStored then
            this.Fail("The stored style cannot express the value " + this.Preview value + ": " + reason)
        else
            this.LinePlan(WriterText.renderDouble value compactFlow)

    /// The header of a block scalar for the value. The chomping follows the line feeds at the end of the value
    /// and the indentation indicator is there when the first line starts with a blank. A value without a final line
    /// feed fits Clip and Keep only when the scalar is the last node of the text, where the writer leaves out the
    /// last line break ([165] b-chomped-last takes the end of the input in place of a line feed). Anywhere else
    /// the stored chomping conflicts with the value.
    member private this.TryBlock(value: string, isLiteral: bool, storedChomp: ChompingMode, storedDigit: int option) : ScalarPlan option =
        if not (Validate.isBlockText value) then
            None
        else
            let n = value.Length
            let endsText = n > 0 && value.[n - 1] <> '\n' && storedChomp <> ChompingMode.Strip && this.AtEndOfText()
            let (chosen, fits) = Validate.blockChomping value storedChomp endsText
            let chomp =
                if fits || policy <> StylePolicy.RequireStored then
                    chosen
                else
                    this.Fail("The stored chomping does not match the line feeds at the end of the value " + this.Preview value + ".")
            let mutable start = 0
            while start < n && value.[start] = '\n' do
                start <- start + 1
            let needsDigit = start < n && value.[start] = ' '
            let digit =
                match storedDigit with
                | Some stored -> stored
                | None ->
                    if not needsDigit then 0
                    elif policy = StylePolicy.RequireStored then
                        this.Fail("The value " + this.Preview value + " starts with a blank, so the block scalar needs an indentation indicator that the stored style lacks.")
                    else min indent 9
            // The empty lines behind a Keep scalar belong to its value ([165] to [169]), so a blank line that the tree
            // holds behind it would read as part of the value. No other chomping gives the same value, so a scalar that
            // gets Keep from its value takes the double-quoted style. A stored Keep that fits the value stays, because
            // Validate (`storedKeepApplies`) lets a blank line follow it only where the writer prints that line so that
            // it ends the body. A stored Keep that does not fit has been replaced by the chomping of the value above.
            if chomp = ChompingMode.Keep && storedChomp <> ChompingMode.Keep && this.BlankLineFollows() then
                Some(this.Fallback(value, "a blank line follows it, and the chomping Keep would read that line as part of the value."))
            else
                Some
                    { Shape = Shape.Block
                      IsLiteral = isLiteral
                      Chomp = chomp
                      Digit = digit
                      Unterminated = endsText && fits
                      Pieces = [||] }

    /// The pieces of the layout of the scalar when the writer replays them at this place, and None otherwise. The place has
    /// to allow more than one line: `multiOk` is false for an implicit key and for every item of a flow collection that stays
    /// on one line, and `compactFlow` is true while the writer spells a key of a flow collection with the fewest characters.
    /// The layout has to fit the value and the style, and a plain or single-quoted scalar has to be able to stand in that
    /// style here, which `canBePlain` and `canBeSingle` decide as they do for a scalar without a layout. `PlanScalar` and
    /// `NodeNeedsLines` ask this one question, so a flow collection goes over several lines exactly when one of its scalars
    /// does. The policy `Canonical` never asks: its branch of `PlanScalar` comes first, and it writes no flow collection with items.
    member private _.LayoutHere(scalar: YamlScalar, flow: bool, multiOk: bool, atLineStart: bool, beforeColon: bool) : LayoutPiece[] option =
        match scalar.Layout with
        | Some layout when multiOk && not compactFlow ->
            let value = scalar.Value
            let holds =
                match scalar.Style with
                | ScalarStyle.Plain -> WriterText.canBePlain value flow multiOk atLineStart beforeColon
                | ScalarStyle.SingleQuoted -> WriterText.canBeSingle value multiOk
                | _ -> true
            if holds then Layout.tryPlan layout value scalar.Style else None
        | _ -> None

    member private this.LayoutPlan(shape: Shape, pieces: LayoutPiece[]) : ScalarPlan = { this.ShapePlan shape with Pieces = pieces }

    /// Plans a scalar that is neither missing nor a property alone.
    ///
    /// `flow` tells that the scalar stands inside a flow collection, `blockOk` that a block scalar may stand
    /// here, `multiOk` that the scalar may run over several lines, `atLineStart` that it starts at the
    /// first column and `beforeColon` that it is an implicit key.
    member private this.PlanScalar(node: YamlNode, scalar: YamlScalar, flow: bool, blockOk: bool, multiOk: bool, atLineStart: bool, beforeColon: bool) : ScalarPlan =
        let value = scalar.Value
        if WriterText.hasLoneSurrogate value then
            this.Fail "The scalar holds a lone surrogate, which is no character."
        elif policy = StylePolicy.Canonical then
            let storedPlain =
                match scalar.Style with
                | ScalarStyle.Plain -> true
                | _ -> false
            if value.Length = 0 then
                if storedPlain then this.ShapePlan Shape.Empty else this.LinePlan "\"\""
            elif WriterText.canBePlain value flow false atLineStart beforeColon
                 && (storedPlain || node.Properties.Tag.IsSome || not (CoreSchema.resolvesToNonString value)) then
                this.LinePlan value
            else
                this.LinePlan(WriterText.renderDouble value compactFlow)
        else
            let layout = this.LayoutHere(scalar, flow, multiOk, atLineStart, beforeColon)
            match scalar.Style with
            | ScalarStyle.Plain ->
                if value.Length = 0 then
                    this.ShapePlan Shape.Empty
                elif layout.IsSome then
                    this.LayoutPlan(Shape.PlainLayout, layout.Value)
                elif WriterText.canBePlain value flow multiOk atLineStart beforeColon then
                    if value.IndexOf('\n') >= 0 then this.ShapePlan Shape.PlainLines else this.LinePlan value
                else
                    this.Fallback(value, "it cannot stand as a plain scalar here.")
            | ScalarStyle.SingleQuoted ->
                if layout.IsSome then
                    this.LayoutPlan(Shape.SingleLayout, layout.Value)
                elif WriterText.canBeSingle value multiOk then
                    if value.IndexOf('\n') >= 0 then this.ShapePlan Shape.SingleLines else this.LinePlan(WriterText.renderSingle value)
                else
                    this.Fallback(value, "it cannot stand in single quotes here.")
            | ScalarStyle.DoubleQuoted ->
                if layout.IsSome then
                    this.LayoutPlan(Shape.DoubleLayout, layout.Value)
                else
                    this.LinePlan(WriterText.renderDouble value compactFlow)
            | ScalarStyle.Block (style, chomp, digit) ->
                if not blockOk then
                    this.Fallback(value, "a block scalar cannot stand here.")
                else
                    match this.TryBlock(value, (style = BlockScalarStyle.Literal), chomp, digit) with
                    | Some plan ->
                        match plan.Shape, layout with
                        | Shape.Block, Some pieces -> { plan with Pieces = pieces }
                        | _ -> plan
                    | None -> this.Fallback(value, "it holds a character that a block scalar cannot hold.")

    // ---------------------------------------------------------------------------------------------
    // Writing a scalar

    member private _.ChildCol(n: int) : int = if n < 0 then 0 else n + indent

    /// The column of the continuation lines of a scalar whose parent has the indentation `n`.
    member private _.ContentCol(n: int) : int = (if n < 0 then 0 else n) + indent

    member private _.PutSegment(value: string, first: int, finish: int, doubleApostrophes: bool) =
        if doubleApostrophes then
            for k in first .. finish - 1 do
                if value.[k] = '\'' then out.PutChar '\''
                out.PutChar value.[k]
        else
            for k in first .. finish - 1 do
                out.PutChar value.[k]

    /// Writes the text from `first` up to `finish` of a plain or single-quoted value. A run of `m` line feeds becomes `m`
    /// empty lines, so `m + 1` line breaks stand between the two lines of text. The text starts at the cursor.
    member private this.WriteFoldedRange(value: string, first: int, finish: int, contentCol: int, quoted: bool) =
        let mutable i = first
        while i < finish do
            let mutable j = i
            while j < finish && value.[j] <> '\n' do
                j <- j + 1
            this.PutSegment(value, i, j, quoted)
            let mutable k = j
            while k < finish && value.[k] = '\n' do
                k <- k + 1
            if k > j then
                for _ in 0 .. k - j do
                    out.EndLine()
                out.Spaces contentCol
            i <- k

    /// Writes the lines of a plain or single-quoted value.
    member private this.WriteFoldedLines(value: string, contentCol: int, quoted: bool) =
        if quoted then out.Put "'"
        this.WriteFoldedRange(value, 0, value.Length, contentCol, quoted)
        if quoted then out.Put "'"

    /// Ends the line at a break of a layout and starts the next line at `contentCol`.
    member private this.PutLayoutBreak(contentCol: int) =
        out.EndLine()
        out.Spaces contentCol

    /// Writes the pieces of a plain or single-quoted scalar, one line for each piece. The reader folds the break between
    /// two pieces into the space that the layout names, and the line feeds inside a piece stand as empty lines.
    member private this.WriteLayoutLines(value: string, pieces: LayoutPiece[], contentCol: int, quoted: bool) =
        if quoted then out.Put "'"
        for piece in pieces do
            this.WriteFoldedRange(value, piece.First, piece.Limit, contentCol, quoted)
            if piece.Behind.IsSome then this.PutLayoutBreak contentCol
        if quoted then out.Put "'"

    /// Writes the pieces of a double-quoted scalar. A fold ends the line of a piece, and the blank at its end would go
    /// away, so the piece escapes it. A piece behind a break escapes the blank at its start for the same reason. An
    /// escaped break keeps the blanks in front of it. A fold with empty lines and an escaped break with empty lines
    /// write one empty line for each of them. A line feed inside a piece stays an escape.
    member private this.WriteDoubleLayout(value: string, pieces: LayoutPiece[], contentCol: int) =
        out.Put "\""
        let sb = StringBuilder()
        for p in 0 .. pieces.Length - 1 do
            let piece = pieces.[p]
            let folds =
                match piece.Behind with
                | Some (LayoutKind.Fold _) -> true
                | _ -> false
            sb.Clear() |> ignore
            WriterText.appendDoubleRange sb value piece.First piece.Limit false (p > 0) folds
            out.Put(sb.ToString())
            match piece.Behind with
            | Some kind ->
                let emptyLines =
                    match kind with
                    | LayoutKind.Fold lines -> lines
                    | LayoutKind.EscapedBreak lines ->
                        out.Put "\\"
                        lines
                for _ in 0 .. emptyLines do
                    out.EndLine()
                out.Spaces contentCol
            | None -> ()
        out.Put "\""

    /// Writes the header and the body of a block scalar. The cursor stands behind the properties. The header ends
    /// the line, and every body line ends its line too.
    member private this.WriteBlockScalar(plan: ScalarPlan, scalar: YamlScalar, n: int) =
        let value = scalar.Value
        keepTail <- plan.Chomp = ChompingMode.Keep
        if n < 0 && keepTail then rootKept <- true
        out.Put(if plan.IsLiteral then "|" else ">")
        if plan.Digit > 0 then out.Put(string plan.Digit)
        match plan.Chomp with
        | ChompingMode.Strip -> out.Put "-"
        | ChompingMode.Clip -> ()
        | ChompingMode.Keep -> out.Put "+"
        this.EndLineWith scalar.HeaderComment
        let contentCol = (if n < 0 then 0 else n) + (if plan.Digit > 0 then plan.Digit else indent)
        let len = value.Length
        let mutable bodyEnd = len
        while bodyEnd > 0 && value.[bodyEnd - 1] = '\n' do
            bodyEnd <- bodyEnd - 1
        let trailing = len - bodyEnd
        let mutable foldIndex = 0
        if bodyEnd = 0 then
            for _ in 1 .. trailing do
                out.EndLine()
        else
            let mutable i = 0
            while i < bodyEnd && value.[i] = '\n' do
                out.EndLine()
                i <- i + 1
            while i < bodyEnd do
                let mutable j = i
                while j < bodyEnd && value.[j] <> '\n' do
                    j <- j + 1
                let more = value.[i] = ' ' || value.[i] = '\t'
                out.Spaces contentCol
                for k in i .. j - 1 do
                    // The space of a fold is the line break of the layout.
                    if foldIndex < plan.Pieces.Length - 1 && plan.Pieces.[foldIndex].Limit = k then
                        this.PutLayoutBreak contentCol
                        foldIndex <- foldIndex + 1
                    else
                        out.PutChar value.[k]
                let mutable k = j
                while k < bodyEnd && value.[k] = '\n' do
                    k <- k + 1
                if not (k = j && plan.Unterminated) then out.EndLine()
                if k > j then
                    let run = k - j
                    let nextMore = value.[k] = ' ' || value.[k] = '\t'
                    let empties = if plan.IsLiteral || more || nextMore then run - 1 else run
                    for _ in 1 .. empties do
                        out.EndLine()
                i <- k
            if plan.Chomp = ChompingMode.Keep then
                for _ in 2 .. trailing do
                    out.EndLine()

    /// The comment that ends the line of a scalar that is not a block scalar. A block scalar that lost its block
    /// form hands its header comment on to the end of the line.
    member private _.TrailingOf(node: YamlNode) : YamlComment option =
        match node.TrailingComment with
        | Some comment -> Some comment
        | None ->
            match node.Kind with
            | YamlNodeKind.Scalar scalar -> scalar.HeaderComment
            | _ -> None

    /// The plan of the block scalar at the root of a document. Without an indentation indicator the content of a root
    /// scalar starts at the first column, so a body without content lines takes the first line behind it as content
    /// when that line is a comment line or the tab line that ends the body of a root scalar with the chomping Keep. An
    /// indicator of 1 puts the content at the column 1, and a line at the first column is no content then. Blank lines
    /// behind such a body are no problem, because the parser reads them as trivia behind the root and they read as the
    /// same lines again. The comment line may stand in the prelude of the next document, which `commentFollowsRoot` and
    /// `blankFollowsRoot` include.
    member private this.WithRootIndicator(plan: ScalarPlan, value: string, n: int) : ScalarPlan =
        match plan.Shape with
        | Shape.Block when n < 0 && plan.Digit = 0 && (commentFollowsRoot || (plan.Chomp = ChompingMode.Keep && blankFollowsRoot)) && value.Replace("\n", "").Length = 0 ->
            if policy = StylePolicy.RequireStored then
                this.Fail "The block scalar has no content and a comment line or a tab line follows it, so it needs an indentation indicator that the stored style lacks."
            else
                { plan with Digit = 1 }
        | _ -> plan

    /// Writes a scalar in a block position, behind its properties.
    member private this.WriteBlockScalarNode(node: YamlNode, scalar: YamlScalar, n: int) =
        let plan = this.WithRootIndicator(this.PlanScalar(node, scalar, false, true, true, (out.Width = 0), false), scalar.Value, n)
        match plan.Shape with
        | Shape.Empty -> out.EndLine()
        | Shape.Line text ->
            out.Put text
            this.EndLineWith(this.TrailingOf node)
        | Shape.PlainLines ->
            this.WriteFoldedLines(scalar.Value, this.ContentCol n, false)
            this.EndLineWith(this.TrailingOf node)
        | Shape.SingleLines ->
            this.WriteFoldedLines(scalar.Value, this.ContentCol n, true)
            this.EndLineWith(this.TrailingOf node)
        | Shape.PlainLayout ->
            this.WriteLayoutLines(scalar.Value, plan.Pieces, this.ContentCol n, false)
            this.EndLineWith(this.TrailingOf node)
        | Shape.SingleLayout ->
            this.WriteLayoutLines(scalar.Value, plan.Pieces, this.ContentCol n, true)
            this.EndLineWith(this.TrailingOf node)
        | Shape.DoubleLayout ->
            this.WriteDoubleLayout(scalar.Value, plan.Pieces, this.ContentCol n)
            this.EndLineWith(this.TrailingOf node)
        | Shape.Block -> this.WriteBlockScalar(plan, scalar, n)

    // ---------------------------------------------------------------------------------------------
    // Flow collections

    member private _.IsCollection(node: YamlNode) : bool =
        match node.Kind with
        | YamlNodeKind.Mapping _ -> true
        | YamlNodeKind.Sequence _ -> true
        | _ -> false

    member private _.Breaks(separation: Separation option) : bool =
        match separation with
        | Some (Separation.NewLine _) -> true
        | _ -> false

    /// The separation behind the colon of a flow mapping entry. The colon of a block mapping entry has a plain line
    /// break behind it when the value is a block collection, because that collection starts on a later line. A flow
    /// collection takes the value on the line of the colon, so such a line break goes away.
    member private _.FlowColon(entry: MappingEntry) : Separation =
        let isBlockWithItems =
            match entry.Value.Kind with
            | YamlNodeKind.Mapping collection -> collection.Style = CollectionStyle.Block && not collection.Items.IsEmpty
            | YamlNodeKind.Sequence collection -> collection.Style = CollectionStyle.Block && not collection.Items.IsEmpty
            | _ -> false
        match entry.Colon with
        | Some (Separation.NewLine lineBreak) ->
            if isBlockWithItems && lineBreak.Comment.IsNone && lineBreak.Following.IsEmpty then Separation.Inline
            else Separation.NewLine lineBreak
        | Some separation -> separation
        | None -> Separation.Inline

    member private _.DashTrivia(dash: Separation option) : Trivia list =
        match dash with
        | Some (Separation.NewLine lineBreak) ->
            (match lineBreak.Comment with
             | Some comment -> [ Trivia.FullLineComment comment ]
             | None -> [])
            @ lineBreak.Following
        | _ -> []

    /// True when a node carries something that needs a line of its own: a comment, a property that is followed
    /// by a line break, a scalar over several lines or a block scalar that loses its header.
    member private this.NodeNeedsLines(node: YamlNode, beforeColon: bool) : bool =
        if node.TrailingComment.IsSome then
            true
        elif node.PropertyLayout
             |> List.exists (fun placement ->
                 match placement.After with
                 | Separation.NewLine _ -> true
                 | Separation.Inline -> false) then
            true
        else
            match node.Kind with
            | YamlNodeKind.Scalar scalar ->
                let value = scalar.Value
                // A plain or single-quoted scalar with a line feed goes over several lines when its style can hold the value
                // in a flow collection, and PlanScalar asks the same questions. Otherwise it falls back to one quoted line.
                let overLines =
                    policy <> StylePolicy.Canonical
                    && value.IndexOf('\n') >= 0
                    && (match scalar.Style with
                        | ScalarStyle.Plain -> WriterText.canBePlain value true true false beforeColon
                        | ScalarStyle.SingleQuoted -> WriterText.canBeSingle value true
                        | _ -> false)
                let layout =
                    match scalar.Style with
                    | ScalarStyle.Block _ -> false
                    | _ -> (this.LayoutHere(scalar, true, true, false, beforeColon)).IsSome
                scalar.HeaderComment.IsSome || layout || overLines
            | _ -> false

    /// Sets the flag of every collection of the flow subtree: true when the collection or one below it needs one
    /// item on each line. The flags stand in the order in which the writer opens the collections.
    member private this.ProbeFlow(root: YamlNode) =
        flowFlags.Clear()
        probe.Clear()
        probe.Add((root, -1, -1))
        while probe.Count > 0 do
            let last = probe.Count - 1
            let (node, parent, exitIndex) = probe.[last]
            probe.RemoveAt last
            if exitIndex >= 0 then
                if flowFlags.[exitIndex] && parent >= 0 then flowFlags.[parent] <- true
            else
                let index = flowFlags.Count
                let children = ResizeArray<YamlNode>()
                let mutable own = false
                match node.Kind with
                | YamlNodeKind.Mapping collection ->
                    own <- collection.OpeningComment.IsSome || not collection.End.IsEmpty
                    for entry in collection.Items do
                        if not entry.Before.IsEmpty || this.Breaks entry.Question || this.Breaks (Some (this.FlowColon entry)) || not entry.BeforeColon.IsEmpty then
                            own <- true
                        if this.NodeNeedsLines(entry.Key, entry.Colon.IsSome) || this.NodeNeedsLines(entry.Value, false) then own <- true
                        if this.IsCollection entry.Key then children.Add entry.Key
                        if this.IsCollection entry.Value then children.Add entry.Value
                | YamlNodeKind.Sequence collection ->
                    own <- collection.OpeningComment.IsSome || not collection.End.IsEmpty
                    for item in collection.Items do
                        if not item.Before.IsEmpty || not (this.DashTrivia item.Dash).IsEmpty || this.NodeNeedsLines(item.Node, false) then
                            own <- true
                        if this.IsCollection item.Node then children.Add item.Node
                | _ -> ()
                if this.IsCollection node then
                    flowFlags.Add own
                    probe.Add((node, parent, index))
                    for i in children.Count - 1 .. -1 .. 0 do
                        probe.Add((children.[i], index, -1))

    /// True when the one-entry mapping can stand in a flow sequence as a flow pair, `[a: b]`, which reads as the tree of
    /// `[{a: b}]`. The entry is implicit and has a colon, and nothing stands in front of it, behind it or around the
    /// mapping that needs a line of its own.
    member private _.IsFlowPair(node: YamlNode, collection: Collection<MappingEntry>) : bool =
        match collection.Items with
        | [ entry ] ->
            node.PropertyLayout.IsEmpty
            && node.TrailingComment.IsNone
            && collection.OpeningComment.IsNone
            && collection.End.IsEmpty
            && entry.Question.IsNone
            && entry.Colon.IsSome
            && entry.Before.IsEmpty
            && entry.BeforeColon.IsEmpty
        | _ -> false

    /// Opens the flow collection: writes the opener and, when the collection has items or needs lines, pushes its
    /// frame. Returns true when a frame was pushed. The items stand at `itemsCol` and the closer at `closeCol`.
    /// `pairOk` allows a one-entry mapping in a flow sequence to stand as a flow pair without braces, which is the
    /// shortest spelling.
    member private this.OpenFlow(node: YamlNode, closeCol: int, itemsCol: int, pairOk: bool) : bool =
        let index = flowCounter
        flowCounter <- flowCounter + 1
        let multi = flowFlags.[index]
        match node.Kind with
        | YamlNodeKind.Mapping collection ->
            if collection.Items.IsEmpty && not multi then
                out.Put "{}"
                false
            else
                let bare = pairOk && not multi && this.IsFlowPair(node, collection)
                if not bare then out.Put "{"
                if multi then this.EndLineWith collection.OpeningComment
                let frame = FlowFrame(true, multi, closeCol, itemsCol, collection.Items, [], collection.End)
                frame.Bare <- bare
                flowStack.Add frame
                true
        | YamlNodeKind.Sequence collection ->
            if collection.Items.IsEmpty && not multi then
                out.Put "[]"
                false
            else
                out.Put "["
                if multi then this.EndLineWith collection.OpeningComment
                flowStack.Add(FlowFrame(false, multi, closeCol, itemsCol, [], collection.Items, collection.End))
                true
        | _ -> this.Fail "A flow collection was expected."

    member private this.CloseFlow(frame: FlowFrame) =
        if frame.Multi then
            this.WriteTrivia(frame.End, frame.ItemsCol)
            out.Spaces frame.CloseCol
        if not frame.Bare then out.Put(if frame.IsMap then "}" else "]")
        flowStack.RemoveAt(flowStack.Count - 1)

    /// Writes the properties and the content of a node inside a flow collection. Returns true when the node is a
    /// collection that pushed a frame, so the item is not complete yet. `beforeColon` is true for a key that a colon
    /// follows. A plain key may then end with a colon, which the colon of the entry follows without a blank.
    member private this.FlowNode(node: YamlNode, frame: FlowFrame, emptyOk: bool, multiOk: bool, beforeColon: bool) : bool =
        let propOnly = YamlNode.isPropertyOnly node
        let mutable rest = node.PropertyLayout
        while not rest.IsEmpty do
            let placement = rest.Head
            out.Put(this.PropertyText(node, placement.Property))
            rest <- rest.Tail
            if rest.IsEmpty && propOnly then
                match placement.After with
                | Separation.NewLine lineBreak ->
                    this.EndLineWith lineBreak.Comment
                    frame.LineBroken <- true
                | Separation.Inline -> ()
            else
                match placement.After with
                | Separation.Inline -> out.Put " "
                | Separation.NewLine lineBreak ->
                    this.EndLineWith lineBreak.Comment
                    this.WriteTrivia(lineBreak.Following, frame.ItemsCol)
                    out.Spaces frame.ItemsCol
        match node.Kind with
        | YamlNodeKind.Alias name ->
            out.Put("*" + name)
            false
        | YamlNodeKind.Scalar scalar ->
            if propOnly then
                false
            elif YamlNode.isMissingValue node && emptyOk then
                false
            else
                let plan =
                    if YamlNode.isMissingValue node then
                        this.Fallback("", "an empty item of a flow sequence cannot stand plain.")
                    else
                        this.PlanScalar(node, scalar, true, false, (multiOk && frame.Multi), false, beforeColon)
                match plan.Shape with
                | Shape.Line text -> out.Put text
                | Shape.PlainLines -> this.WriteFoldedLines(scalar.Value, frame.ItemsCol, false)
                | Shape.SingleLines -> this.WriteFoldedLines(scalar.Value, frame.ItemsCol, true)
                | Shape.PlainLayout -> this.WriteLayoutLines(scalar.Value, plan.Pieces, frame.ItemsCol, false)
                | Shape.SingleLayout -> this.WriteLayoutLines(scalar.Value, plan.Pieces, frame.ItemsCol, true)
                | Shape.DoubleLayout -> this.WriteDoubleLayout(scalar.Value, plan.Pieces, frame.ItemsCol)
                | Shape.Empty -> ()
                | Shape.Block -> this.Fail "A block scalar cannot stand inside a flow collection."
                false
        | YamlNodeKind.Mapping _ ->
            this.CheckFlowChild node
            this.OpenFlow(node, frame.ItemsCol, frame.ItemsCol + indent, compactFlow && not frame.IsMap)
        | YamlNodeKind.Sequence _ ->
            this.CheckFlowChild node
            this.OpenFlow(node, frame.ItemsCol, frame.ItemsCol + indent, false)

    /// A block collection inside a flow collection turns into a flow collection, unless the policy keeps the stored style.
    member private this.CheckFlowChild(node: YamlNode) =
        if policy = StylePolicy.RequireStored then
            let isBlock =
                match node.Kind with
                | YamlNodeKind.Mapping collection -> collection.Style = CollectionStyle.Block && not collection.Items.IsEmpty
                | YamlNodeKind.Sequence collection -> collection.Style = CollectionStyle.Block && not collection.Items.IsEmpty
                | _ -> false
            if isBlock then
                this.Fail "A block collection cannot stand inside a flow collection, and the policy requires the stored style."

    /// The node that ends the line of the item in progress: the node of a sequence item, the value of an entry, or the
    /// key of an entry that has neither a value nor a colon. The last token of an entry with a colon and no value is
    /// the colon, and a comment behind it stands in the separation of the colon, so no node ends the line then.
    member private _.ItemTailNode(frame: FlowFrame) : YamlNode =
        if frame.IsMap then
            let entry = frame.Current
            if not (YamlNode.isMissingValue entry.Value) then entry.Value
            elif entry.Colon.IsSome then YamlNode.empty
            else entry.Key
        else
            frame.Node

    /// The comma behind a flow item, the comment of its last token and the line break. The last item has no comma,
    /// unless it holds properties only and has a comment. The comment would stand behind the property then and read as
    /// the comment of its line break (rules R1 and R6), so the comma goes in between.
    member private this.FlowComplete(frame: FlowFrame) =
        frame.Phase <- FlowPhase.NextItem
        if frame.Multi then
            let tailNode = this.ItemTailNode frame
            let tail = this.TrailingOf tailNode
            let isLast = if frame.IsMap then frame.Entries.IsEmpty else frame.Items.IsEmpty
            if frame.LineBroken then
                frame.LineBroken <- false
                if not isLast then out.Spaces frame.ItemsCol
            if not isLast || (tail.IsSome && YamlNode.isPropertyOnly tailNode) then out.Put ","
            this.PutOptionalComment tail
            if not out.IsEmptyLine then out.EndLine()

    /// Starts the next item of a flow sequence, or closes the sequence when no item is left.
    member private this.NextFlowSeqItem(frame: FlowFrame) =
        match frame.Items with
        | [] -> this.CloseFlow frame
        | item :: rest ->
            frame.Items <- rest
            frame.Index <- frame.Index + 1
            frame.Node <- item.Node
            if frame.Multi then
                this.WriteTrivia(item.Before @ this.DashTrivia item.Dash, frame.ItemsCol)
                out.Spaces frame.ItemsCol
            elif frame.Index > 0 then
                out.Put (if compactFlow then "," else ", ")
            frame.Phase <- FlowPhase.ItemWritten
            let pushed = this.FlowNode(item.Node, frame, false, true, false)
            if not pushed then this.FlowComplete frame

    member private this.FlowAfterKey(frame: FlowFrame) =
        let entry = frame.Current
        let valueMissing = YamlNode.isMissingValue entry.Value
        let keyComment = this.TrailingOf entry.Key
        let mutable pushed = false
        let hasColon = entry.Colon.IsSome
        if hasColon then
            // A comment behind the key ends the line, so the colon stands on a later line, with or without a value.
            if frame.Multi && (not entry.BeforeColon.IsEmpty || keyComment.IsSome) then
                if frame.LineBroken then
                    // The line break behind the last property ended the line already, so a comment of the key goes on a line of its own.
                    match keyComment with
                    | Some comment -> this.WriteTrivia([ Trivia.FullLineComment comment ], frame.ItemsCol)
                    | None -> ()
                else
                    this.EndLineWith keyComment
                frame.LineBroken <- false
                this.WriteTrivia(entry.BeforeColon, frame.ItemsCol)
                out.Spaces frame.ItemsCol
            elif frame.LineBroken then
                frame.LineBroken <- false
                out.Spaces frame.ItemsCol
            else
                // A blank keeps the colon out of an alias name or of a property.
                let spaceBefore =
                    match entry.Key.Kind with
                    | YamlNodeKind.Alias _ -> true
                    | _ -> YamlNode.isPropertyOnly entry.Key
                if spaceBefore then out.Put " "
            out.Put ":"
            if valueMissing then
                match entry.Colon with
                | Some (Separation.NewLine lineBreak) when lineBreak.Comment.IsSome && frame.Multi ->
                    this.EndLineWith lineBreak.Comment
                    frame.LineBroken <- true
                | _ -> ()
            else
                let separation = this.FlowColon entry
                match separation with
                | Separation.Inline -> out.Put " "
                | Separation.NewLine lineBreak ->
                    this.EndLineWith lineBreak.Comment
                    this.WriteTrivia(lineBreak.Following, frame.ItemsCol + indent)
                    out.Spaces(frame.ItemsCol + indent)
                pushed <- this.FlowNode(entry.Value, frame, true, true, false)
        frame.Phase <- FlowPhase.ItemWritten
        if not pushed then this.FlowComplete frame

    /// Starts the next entry of a flow mapping, or closes the mapping when no entry is left.
    member private this.NextFlowMapEntry(frame: FlowFrame) =
        match frame.Entries with
        | [] -> this.CloseFlow frame
        | entry :: rest ->
            frame.Entries <- rest
            frame.Index <- frame.Index + 1
            frame.Current <- entry
            if frame.Multi then
                this.WriteTrivia(entry.Before, frame.ItemsCol)
                out.Spaces frame.ItemsCol
            elif frame.Index > 0 then
                out.Put (if compactFlow then "," else ", ")
            // A key in a flow mapping may run over several lines, and so may a flow collection that is a key ([143] to [147]).
            // Only the key of a block mapping and of a flow pair stands on one line.
            if entry.Question.IsSome then
                out.Put "?"
                match entry.Question with
                | Some (Separation.NewLine lineBreak) ->
                    this.EndLineWith lineBreak.Comment
                    this.WriteTrivia(lineBreak.Following, frame.ItemsCol + indent)
                    // A missing key writes nothing on the new line, so the next token starts that line. Indentation alone
                    // would make a line that reads back as a blank line.
                    if YamlNode.isMissingValue entry.Key then frame.LineBroken <- true else out.Spaces(frame.ItemsCol + indent)
                | _ ->
                    // [142] and [150] ask for a separation behind the question mark, even when the key is empty.
                    out.Put " "
            frame.Phase <- FlowPhase.AfterKey
            let hasColon = entry.Colon.IsSome
            let pushed = this.FlowNode(entry.Key, frame, true, true, hasColon)
            if not pushed then this.FlowAfterKey frame

    member private this.StepFlow(frame: FlowFrame) =
        match frame.Phase with
        | FlowPhase.ItemWritten -> this.FlowComplete frame
        | FlowPhase.AfterKey -> this.FlowAfterKey frame
        | FlowPhase.NextItem -> if frame.IsMap then this.NextFlowMapEntry frame else this.NextFlowSeqItem frame

    /// Writes the flow collection at the cursor. The flags of the subtree have to be set.
    member private this.RunFlow(node: YamlNode, closeCol: int, itemsCol: int) =
        flowStack.Clear()
        flowCounter <- 0
        this.OpenFlow(node, closeCol, itemsCol, false) |> ignore
        while flowStack.Count > 0 do
            this.StepFlow flowStack.[flowStack.Count - 1]

    /// Writes a flow collection that stands in a block position whose parent has the indentation `n`. Every line of a
    /// flow collection that a block node owns, the line of the closer included, stands at the indentation n + 1 or
    /// deeper ([69] and [80]). The items and the closer therefore stand at the column of a child of the owner. A
    /// collection at the root has no owner, so its closer stands at the first column.
    member private this.WriteFlowValue(node: YamlNode, n: int) =
        this.ProbeFlow node
        if n < 0 then this.RunFlow(node, 0, indent) else this.RunFlow(node, n + indent, n + indent)

    // ---------------------------------------------------------------------------------------------
    // Block collections

    /// True when the node is written as a block collection: it has items, and it is a block collection or the
    /// policy writes every collection with items as one.
    member private _.IsBlockLike(node: YamlNode) : bool =
        match node.Kind with
        | YamlNodeKind.Mapping collection -> not collection.Items.IsEmpty && (collection.Style = CollectionStyle.Block || policy = StylePolicy.Canonical)
        | YamlNodeKind.Sequence collection -> not collection.Items.IsEmpty && (collection.Style = CollectionStyle.Block || policy = StylePolicy.Canonical)
        | _ -> false

    member private _.HasProperties(node: YamlNode) : bool =
        node.Properties.Tag.IsSome || node.Properties.Anchor.IsSome

    /// True when the first entry or item can start on the line of the indicator in front of the collection: no
    /// trivia stand in front of it, and no opening comment of a flow collection moves in front of it.
    member private _.FirstBeforeEmpty(node: YamlNode) : bool =
        match node.Kind with
        | YamlNodeKind.Mapping collection ->
            (collection.Style = CollectionStyle.Block || collection.OpeningComment.IsNone)
            && (match collection.Items with
                | first :: _ -> first.Before.IsEmpty
                | [] -> true)
        | YamlNodeKind.Sequence collection ->
            (collection.Style = CollectionStyle.Block || collection.OpeningComment.IsNone)
            && (match collection.Items with
                | first :: _ -> first.Before.IsEmpty
                | [] -> true)
        | _ -> true

    member private this.AddFrame(frame: BlockFrame) =
        stack.Add frame
        this.RefreshFrame frame

    /// The trivia at the end of the block that a flow collection becomes: the lines in front of the closer and the
    /// comment behind it. They stay there when the block ends the text. Otherwise the parser reads them as trivia in
    /// front of whatever follows the block (R4), so the writer carries them to that place and writes them after the
    /// frame is closed.
    member private this.EndOfMovedBlock(node: YamlNode, written: Trivia list, moved: bool) : Trivia list =
        if not moved then
            written
        else
            let lines =
                match node.TrailingComment with
                | Some comment -> written @ [ Trivia.FullLineComment comment ]
                | None -> written
            if lines.IsEmpty || this.AtEndOfText() then
                lines
            else
                // The frame is pushed behind this call, so its depth is one more than the stack holds.
                carried.Add((stack.Count + 1, lines))
                []

    /// Writes the trivia that blocks closed since the last call left behind, the innermost first, as lines at `col`.
    member private this.WriteCarried(col: int) =
        while carried.Count > 0 && fst carried.[carried.Count - 1] > stack.Count do
            let (_, lines) = carried.[carried.Count - 1]
            carried.RemoveAt(carried.Count - 1)
            this.WriteTrivia(lines, col)

    /// The trivia lines that a flow collection leaves in front of its first item when the policy writes it as a block
    /// collection: the lines between its last property and its opener, and then the comment behind the opener. A
    /// block collection holds the lines behind its last property in the Before of its first entry or item (R2).
    member private _.LeadOfMoved(node: YamlNode, openingComment: YamlComment option) : Trivia list =
        let following =
            match List.tryLast node.PropertyLayout with
            | Some placement ->
                match placement.After with
                | Separation.NewLine lineBreak -> lineBreak.Following
                | Separation.Inline -> []
            | None -> []
        match openingComment with
        | Some comment -> following @ [ Trivia.FullLineComment comment ]
        | None -> following

    /// Pushes the frame of a block collection. A flow collection that the policy writes as a block collection
    /// hands the lines behind its last property and its opening comment to the trivia in front of its first item, and
    /// the comment behind its closer to the trivia that follow the block.
    member private this.PushBlock(node: YamlNode, col: int, compact: bool) =
        match node.Kind with
        | YamlNodeKind.Mapping collection ->
            let moved = collection.Style = CollectionStyle.Flow
            let lead = if moved then this.LeadOfMoved(node, collection.OpeningComment) else []
            let entries =
                match collection.Items with
                | first :: rest when not lead.IsEmpty -> { first with Before = lead @ first.Before } :: rest
                | items -> items
            let finish = this.EndOfMovedBlock(node, collection.End, moved)
            this.AddFrame(BlockFrame(true, col, entries, [], finish, compact))
        | YamlNodeKind.Sequence collection ->
            let moved = collection.Style = CollectionStyle.Flow
            let lead = if moved then this.LeadOfMoved(node, collection.OpeningComment) else []
            let items =
                match collection.Items with
                | first :: rest when not lead.IsEmpty -> { first with Before = lead @ first.Before } :: rest
                | items -> items
            let finish = this.EndOfMovedBlock(node, collection.End, moved)
            this.AddFrame(BlockFrame(false, col, [], items, finish, compact))
        | _ -> this.Fail "A collection was expected."

    /// Writes the properties and the content of a node in a block position. The cursor stands where the first
    /// token goes. `n` is the indentation of the collection that holds the node and `ownCol` the column of a line
    /// that the node starts. The node is not a block collection without properties.
    member private this.EmitNode(node: YamlNode, n: int, ownCol: int) =
        let blockLike = this.IsBlockLike node
        let propOnly = YamlNode.isPropertyOnly node
        this.WriteProps(node, ownCol, blockLike, propOnly)
        if blockLike then
            this.PushBlock(node, ownCol, false)
        else
            match node.Kind with
            | YamlNodeKind.Alias name ->
                out.Put("*" + name)
                this.EndLineWith node.TrailingComment
            | YamlNodeKind.Scalar scalar ->
                if propOnly then out.EndLine() else this.WriteBlockScalarNode(node, scalar, n)
            | YamlNodeKind.Mapping _ ->
                this.WriteFlowValue(node, n)
                this.EndLineWith node.TrailingComment
            | YamlNodeKind.Sequence _ ->
                this.WriteFlowValue(node, n)
                this.EndLineWith node.TrailingComment

    /// Writes what follows an indicator (`-`, `?`, `:` or the colon of a key): the separation and the node. The
    /// cursor stands right behind the indicator. `compactOk` is true when a block collection may continue the
    /// line of the indicator.
    ///
    /// `Validate` has checked that a block collection of the tree starts on a later line behind the colon of an implicit
    /// entry, and that one which continues the line of its indicator has no trivia in front of its first entry or item.
    /// An inline separation in front of a block collection can still arise from the policy `Canonical`, which writes a
    /// flow collection as a block collection. The tree is valid then, because it holds a flow collection, and the trivia
    /// in front of the first item or the opening comment that moves there make the collection start on a later line.
    member private this.EmitAfterIndicator(separation: Separation, node: YamlNode, n: int, compactOk: bool) =
        if YamlNode.isMissingValue node then
            match separation with
            | Separation.NewLine lineBreak -> this.EndLineWith lineBreak.Comment
            | Separation.Inline -> out.EndLine()
        else
            let childCol = this.ChildCol n
            let bare = this.IsBlockLike node && not (this.HasProperties node)
            let effective =
                match separation with
                | Separation.Inline when bare && (not compactOk || not (this.FirstBeforeEmpty node)) -> noBreak
                | other -> other
            match effective with
            | Separation.Inline ->
                out.Put " "
                if bare then this.PushBlock(node, n + 2, true) else this.EmitNode(node, n, childCol)
            | Separation.NewLine lineBreak ->
                this.EndLineWith lineBreak.Comment
                this.WriteTrivia(lineBreak.Following, childCol)
                if bare then
                    this.PushBlock(node, childCol, false)
                else
                    out.Spaces childCol
                    this.EmitNode(node, n, childCol)

    /// The text of a flow collection that is an implicit key. `compact` writes the shortest spelling that the productions
    /// allow: a comma without a blank behind it, a pair of a flow sequence without braces, and the characters of a
    /// double-quoted scalar raw.
    member private this.FlowKeyText(key: YamlNode, compact: bool) : string =
        let saved = out
        out <- Out(newLine)
        compactFlow <- compact
        this.RunFlow(key, 0, indent)
        compactFlow <- false
        let text = out.Text()
        out <- saved
        text

    /// The key of an implicit entry as one line, properties included, or None when the entry needs `?`. The second
    /// value is the reason that the entry gives for the question mark, when it gives one.
    member private this.ImplicitKeyChoice(entry: MappingEntry, col: int, prevColonless: bool) : string option * string option =
        let key = entry.Key
        let blockKey = this.IsBlockLike key
        let keyHeader =
            match key.Kind with
            | YamlNodeKind.Scalar scalar -> scalar.HeaderComment.IsSome
            | _ -> false
        let propsBreak =
            key.PropertyLayout
            |> List.exists (fun placement ->
                match placement.After with
                | Separation.NewLine _ -> true
                | Separation.Inline -> false)
        let emptyAfterColonless = prevColonless && YamlNode.isMissingValue key
        let reason: string option =
            if not entry.BeforeColon.IsEmpty then Some "a comment or a blank line stands between the key and its colon"
            elif key.TrailingComment.IsSome || keyHeader then Some "a comment stands behind the key"
            elif propsBreak then Some "a line break stands inside the properties of the key"
            elif blockKey then Some "the key is a block collection"
            elif emptyAfterColonless then Some "an empty key behind an explicit key without a value would read as the value of that key"
            else None
        let oneLine: string option =
            match reason with
            | Some _ -> None
            | None ->
                let propsText = this.InlinePropsText key
                match key.Kind with
                | YamlNodeKind.Alias name -> Some("*" + name + " ")
                | YamlNodeKind.Scalar scalar ->
                    if YamlNode.isMissingValue key then
                        Some ""
                    elif YamlNode.isPropertyOnly key then
                        Some propsText
                    else
                        match (this.PlanScalar(key, scalar, false, false, false, (col = 0 && propsText.Length = 0), true)).Shape with
                        | Shape.Line spelling ->
                            // A double-quoted key whose escapes pass the limit may meet it with the raw characters.
                            if spelling.[0] = '"' && WriterText.codePointLength (propsText + spelling) > 1024 then
                                Some(propsText + WriterText.renderDouble scalar.Value true)
                            else
                                Some(propsText + spelling)
                        | _ -> None
                | _ ->
                    this.ProbeFlow key
                    if flowFlags.[0] then
                        None
                    else
                        // The normal spelling comes first. A flow key that passes the limit may meet it with the shortest spelling.
                        let normal = this.FlowKeyText(key, false)
                        let flowText = if WriterText.codePointLength (propsText + normal) > 1024 then this.FlowKeyText(key, true) else normal
                        Some(propsText + flowText)
        // The limit counts code points from the first property to the colon.
        let text =
            match oneLine with
            | Some line -> if WriterText.codePointLength line > 1024 then None else oneLine
            | None -> None
        (text, reason)

    /// The key of an implicit entry as one line, or None when the entry needs `?`. `RequireStored` raises there, because
    /// the tree states no question mark.
    member private this.ImplicitKeyText(entry: MappingEntry, col: int, prevColonless: bool) : string option =
        let (text, reason) = this.ImplicitKeyChoice(entry, col, prevColonless)
        if text.IsNone && policy = StylePolicy.RequireStored then
            let why =
                match reason with
                | Some stated -> stated
                | None -> "it is longer than 1024 characters or runs over several lines"
            this.Fail("The key cannot stand without `?`: " + why + ".")
        text

    member private this.CloseBlock(frame: BlockFrame) =
        if not frame.End.IsEmpty then
            this.WriteCarried frame.Col
            if keepTail then this.WriteTriviaBehindKeep(frame.End, frame.Col, WriterText.byteOrderMark) else this.WriteTrivia(frame.End, frame.Col)
            keepTail <- false
        if frame.RulesOutEnd then blockers <- blockers - 1
        stack.RemoveAt(stack.Count - 1)

    member private this.ExplicitColon(frame: BlockFrame) =
        let entry = frame.Current
        frame.Phase <- BlockPhase.NextEntry
        this.RefreshFrame frame
        let col = frame.Col
        if entry.Colon.IsNone then
            frame.PrevColonless <- true
        else
            keepTail <- false
            frame.PrevColonless <- false
            this.WriteCarried col
            this.WriteTrivia(entry.BeforeColon, col)
            out.Spaces col
            out.Put ":"
            let separation = match entry.Colon with Some s -> s | None -> Separation.Inline
            this.EmitAfterIndicator(separation, entry.Value, col, true)

    /// Starts the next entry of a block mapping, or closes the mapping when no entry is left.
    member private this.NextMapEntry(frame: BlockFrame) =
        match frame.Entries with
        | [] -> this.CloseBlock frame
        | entry :: rest ->
            keepTail <- false
            frame.Entries <- rest
            frame.Index <- frame.Index + 1
            frame.Current <- entry
            this.RefreshFrame frame
            let col = frame.Col
            if not (frame.Index = 0 && frame.Compact) then
                this.WriteCarried col
                this.WriteTrivia(entry.Before, col)
                out.Spaces col
            let keyText = if entry.Question.IsSome then None else this.ImplicitKeyText(entry, col, frame.PrevColonless)
            match keyText with
            | Some text ->
                frame.PrevColonless <- false
                out.Put text
                out.Put ":"
                let separation = match entry.Colon with Some s -> s | None -> Separation.Inline
                this.EmitAfterIndicator(separation, entry.Value, col, false)
            | None ->
                out.Put "?"
                let separation = match entry.Question with Some s -> s | None -> Separation.Inline
                frame.Phase <- BlockPhase.ColonNext
                this.RefreshFrame frame
                this.EmitAfterIndicator(separation, entry.Key, col, true)

    member private this.StepMap(frame: BlockFrame) =
        match frame.Phase with
        | BlockPhase.ColonNext -> this.ExplicitColon frame
        | BlockPhase.NextEntry -> this.NextMapEntry frame

    member private this.StepSeq(frame: BlockFrame) =
        match frame.Items with
        | [] -> this.CloseBlock frame
        | item :: rest ->
            keepTail <- false
            frame.Items <- rest
            frame.Index <- frame.Index + 1
            this.RefreshFrame frame
            let col = frame.Col
            if not (frame.Index = 0 && frame.Compact) then
                this.WriteCarried col
                this.WriteTrivia(item.Before, col)
                out.Spaces col
            out.Put "-"
            let separation = match item.Dash with Some s -> s | None -> Separation.Inline
            this.EmitAfterIndicator(separation, item.Node, col, true)

    member private this.Drain() =
        while stack.Count > 0 do
            let frame = stack.[stack.Count - 1]
            if frame.IsMap then this.StepMap frame else this.StepSeq frame

    // ---------------------------------------------------------------------------------------------
    // Documents and streams

    member private this.WriteDirective(directive: YamlDirective) =
        match directive.Kind with
        | DirectiveKind.YamlVersion (major, minor) -> out.Put("%YAML " + string major + "." + string minor)
        | DirectiveKind.TagDirective (handle, prefix) -> out.Put("%TAG " + handle + " " + prefix)
        | DirectiveKind.ReservedDirective (name, arguments) ->
            out.Put("%" + name)
            for argument in arguments do
                out.Put(" " + argument)
        this.EndLineWith directive.TrailingComment

    member private this.WriteMarker(text: string, marker: DocumentMarker option) =
        out.Put text
        match marker with
        | Some m -> this.PutOptionalComment m.TrailingComment
        | None -> ()

    /// True when a plain scalar with this value and the policy stands plain except for a `---` or `...` at the first
    /// column. A text that starts with either of the two marks a document boundary there. Behind one blank it is text.
    member private this.WantsPlainBehindBlank(node: YamlNode, scalar: YamlScalar, multiOk: bool, beforeColon: bool) : bool =
        let value = scalar.Value
        let storedPlain =
            match scalar.Style with
            | ScalarStyle.Plain -> true
            | _ -> false
        let wanted =
            if policy = StylePolicy.Canonical then
                WriterText.canBePlain value false false false beforeColon
                && (storedPlain || node.Properties.Tag.IsSome || not (CoreSchema.resolvesToNonString value))
            else
                storedPlain && WriterText.canBePlain value false multiOk false beforeColon
        value.Length > 0 && wanted && not (WriterText.canBePlain value false (multiOk && policy <> StylePolicy.Canonical) true beforeColon)

    /// True when the root of a document has to stand one column in. A root plain scalar and the key of an entry of a
    /// root mapping start at the first column, where the text `---` or `...` followed by a blank or the end of the text
    /// reads as a document marker. One blank in front of the root makes the same text a plain scalar again, and the
    /// parser reads the tree back without a change of style.
    member private this.RootNeedsBlank(root: YamlNode, onMarkerLine: bool) : bool =
        match root.Kind with
        | YamlNodeKind.Scalar scalar ->
            // A scalar on the line of the `---` marker or behind a property on its line is behind a blank already.
            let startsLine =
                match List.tryLast root.PropertyLayout with
                | Some placement ->
                    match placement.After with
                    | Separation.NewLine _ -> true
                    | Separation.Inline -> false
                | None -> not onMarkerLine
            startsLine && this.WantsPlainBehindBlank(root, scalar, true, false)
        | YamlNodeKind.Mapping collection when this.IsBlockLike root ->
            // Only a key that the writer writes without `?` stands at the start of a line. `ImplicitKeyChoice` decides that for the
            // column that the root takes when it moves in, so a key that needs the question mark for another reason, as a comment
            // behind it, a gap in front of its colon or a length over the limit, asks for no blank.
            collection.Items
            |> List.exists (fun entry ->
                entry.Question.IsNone
                && (match entry.Key.Kind with
                    | YamlNodeKind.Scalar scalar -> not (this.HasProperties entry.Key) && this.WantsPlainBehindBlank(entry.Key, scalar, false, true)
                    | _ -> false)
                && (fst (this.ImplicitKeyChoice(entry, 1, false))).IsSome)
        | _ -> false

    /// Writes a document. `preludeMark` is the mark of the line that ends the block scalar with the chomping Keep at the end
    /// of the previous document, or no mark: a blank line at the start of the prelude is written as such a line.
    member private this.WriteDocument(document: YamlDocument, needsStart: bool, isLastPart: bool, next: YamlDocument option, preludeMark: char) =
        tailOpen <- isLastPart && document.AfterRoot.IsEmpty && document.EndMarker.IsNone
        blankFollowsRoot <- Validate.blankLineFollowsRoot document next
        commentFollowsRoot <- Validate.commentFollowsRoot document next
        rootKept <- false
        keepTail <- false
        handlesByPrefix.Clear()
        declaredHandles.Clear()
        for item in document.Prelude do
            match item with
            | PreludeItem.Directive directive ->
                match directive.Kind with
                | DirectiveKind.TagDirective (handle, prefix) ->
                    declaredHandles.Add handle |> ignore
                    if not (handlesByPrefix.ContainsKey prefix) then handlesByPrefix.[prefix] <- handle
                | _ -> ()
            | PreludeItem.PreludeTrivia _ -> ()
        let mutable first = true
        for item in document.Prelude do
            match item with
            | PreludeItem.Directive directive -> this.WriteDirective directive
            | PreludeItem.PreludeTrivia trivia ->
                if first && preludeMark <> WriterText.noMark then this.WriteTriviaBehindKeep([ trivia ], 0, preludeMark) else this.WriteTrivia([ trivia ], 0)
            first <- false
        let root = document.Root
        let missing = YamlNode.isMissingValue root
        let bare = this.IsBlockLike root && not (this.HasProperties root)
        let markerComment =
            match document.StartMarker with
            | Some marker -> marker.TrailingComment
            | None -> None
        let inlineRoot = needsStart && markerComment.IsNone && document.BeforeRoot.IsEmpty && not bare && not missing
        if needsStart then
            this.WriteMarker("---", document.StartMarker)
            if not inlineRoot then out.EndLine()
        // A root at the start of a line stands one column in when its first column would hold a document marker.
        let rootCol = if not missing && this.RootNeedsBlank(root, inlineRoot) then 1 else 0
        // The lines in front of a root block collection are the Before of its first entry or item (R2), so they stand in its column.
        this.WriteTrivia(document.BeforeRoot, (if bare then rootCol else 0))
        // The column of the trivia that follow a root collection.
        let mutable behindCol = 0
        if not missing then
            if this.IsBlockLike root then behindCol <- rootCol
            if inlineRoot then out.Put " "
            elif rootCol > 0 && not (this.IsBlockLike root) && not (this.HasProperties root) then out.Spaces rootCol
            if bare then this.PushBlock(root, rootCol, false) else this.EmitNode(root, -1, rootCol)
            this.Drain()
        this.WriteCarried behindCol
        if rootKept then this.WriteTriviaBehindKeep(document.AfterRoot, 0, WriterText.tabMark) else this.WriteTrivia(document.AfterRoot, behindCol)
        if document.EndMarker.IsSome then
            this.WriteMarker("...", document.EndMarker)
            out.EndLine()

    member this.WriteStream(stream: YamlStream, prefixParts: bool) =
        let mutable open_ = false
        let mutable index = 0
        // The mark of the line that ends a block scalar with the chomping Keep at the end of the previous part, when that
        // part is a document without a marker and the scalar is its last node.
        let mutable keepMark = WriterText.noMark
        let parts = List.toArray stream.Parts
        let total = parts.Length
        for part in parts do
            match part with
            | StreamPart.Document document ->
                if prefixParts then partPrefix <- "Parts[" + string index + "]"
                let hasDirective =
                    document.Prelude
                    |> List.exists (fun item ->
                        match item with
                        | PreludeItem.Directive _ -> true
                        | PreludeItem.PreludeTrivia _ -> false)
                if open_ && hasDirective then
                    out.Put "..."
                    out.EndLine()
                let needsStart =
                    document.StartMarker.IsSome
                    || hasDirective
                    || not document.Prelude.IsEmpty
                    || open_
                    || YamlNode.isMissingValue document.Root
                // The document that follows with no marker in between puts its prelude behind the root of this one.
                let nextDocument =
                    if index + 1 < total then
                        match parts.[index + 1] with
                        | StreamPart.Document following -> Some following
                        | _ -> None
                    else
                        None
                this.WriteDocument(document, needsStart, (index = total - 1), nextDocument, (if hasDirective then WriterText.noMark else keepMark))
                open_ <- document.EndMarker.IsNone
                keepMark <-
                    if not (document.AfterRoot.IsEmpty && document.EndMarker.IsNone) then WriterText.noMark
                    elif rootKept then WriterText.tabMark
                    elif keepTail then WriterText.byteOrderMark
                    else WriterText.noMark
            | StreamPart.Trivia trivia ->
                this.WriteTrivia([ trivia ], 0)
                keepMark <- WriterText.noMark
            | StreamPart.ExtraEndMarker marker ->
                this.WriteMarker("...", Some marker)
                out.EndLine()
                open_ <- false
                keepMark <- WriterText.noMark
            index <- index + 1
        partPrefix <- ""

    member _.Result() : string = out.Text()

/// The writer. It turns a stream of documents or a single node into YAML text.
///
/// The writer checks its input with `Validate` first and raises `YamlWriteException` with the messages of the
/// validator when the tree breaks a rule. It then puts every comment and blank line back in the place where the
/// reader puts it into the same slot, so `Reader.tryReadDocuments` of the output gives a tree that is equal to the
/// input except for the spans. A tree that a program built can differ in styles and layouts that the writer had to change,
/// unless the policy is `RequireStored`, which raises instead for a style. The policy `Canonical` rewrites the styles on
/// purpose. A tree that the parser produced has one exception, which the paragraph on the empty root block scalar names.
///
/// A scalar that the parser read over several lines keeps those lines in its `Layout`, and the writer writes them
/// again. A fold stands for one space of the value, so the writer ends the line at that space. The
/// layout fits while each boundary still stands at the text that it replaces and the lines next to a fold keep
/// their text. A double-quoted scalar escapes a blank at the edge of a line, because the reader drops it there.
/// The scalar has to stand where more than one line is possible. An implicit key never goes over several lines, and
/// neither does a flow collection that the writer keeps on one line. A flow collection with a scalar over several lines
/// puts each item on a line of its own. `PreferStored` and `RequireStored` write the normal spelling where the layout
/// does not fit, and `RequireStored` raises only where the stored style cannot express the value. `Canonical` ignores
/// the layout. The continuation lines stand one indentation step deeper than the parent, and the lines of a folded block
/// scalar stand at the indentation of its body. The output does not keep the indentation of the author.
///
/// The output ends every line with `WriterOptions.NewLine`, the last line included. One case differs: a block scalar
/// with the chomping Clip or Keep whose value has no final line feed can only be the last node of the text. The
/// writer leaves out its last line break there, because [165] b-chomped-last takes the end of the input in place
/// of a line feed, and the output depends on the tree alone. Anywhere else such a chomping conflicts with the
/// value: `PreferStored` writes Strip and `RequireStored` raises.
///
/// A block scalar with the chomping Keep owns the empty lines behind it (l-chomped-empty, [165] to [169]), so a blank
/// line that the tree holds directly behind it would read as part of the value. `Validate` rejects a stored Keep that
/// fits the value in front of more content. A stored Keep fits a value that is empty or ends in a line feed. On a
/// value without a final line feed it is a style mismatch, which `PreferStored` writes as Strip and `RequireStored`
/// raises for, and the blank line behind it is trivia. The writer picks Keep for a stored Strip or Clip when the
/// value ends in several line feeds, and with a blank line behind it the scalar becomes double-quoted, because no
/// other chomping gives the same value. `RequireStored` raises there already, since the stored chomping does not
/// match the line feeds of the value. A comment line in between ends the empty lines (l-trail-comments), so the
/// scalar stays a block scalar then. `endsWithKeptLines` tells a caller that puts a blank line behind a node whether
/// the node ends in such a scalar, by the same rule.
///
/// Behind the last scalar of a document only trivia and markers follow, in the End of the last collection, in
/// AfterRoot and in the prelude of the next document. A stored Keep scalar may have a blank line there, because the
/// parser reads a line that holds a byte order mark as a blank line that ends the body of the scalar ([202], [211]),
/// and a line that holds a tab does the same behind a root scalar. The writer prints the first blank line there as
/// such a line, a tab line behind a root scalar and a byte order mark line behind a scalar in a collection.
///
/// The one exception to the equal tree is an empty root block scalar without an indentation indicator. Its body has
/// no content lines, and the content of a root scalar starts at the first column, so the first line behind it that is
/// not blank would read as content when it is a comment line, and so would the tab line behind a Keep scalar. Only a
/// text with a byte order mark line behind the scalar produces such a tree. The writer writes the indicator 1, the
/// tree that comes back has it, and `RequireStored` raises instead. The lines that count are the ones behind the
/// root up to the next marker, the prelude of the next document included.
///
/// A character stands raw wherever the productions allow it. Plain and block scalars and comments hold nb-char
/// characters ([27]), which include NEL, NBSP and the two Unicode line separators, because YAML 1.2 ends a line at a
/// line feed or a carriage return only. Single quotes hold every nb-json character ([2], [118]) from U+0020 up and
/// the tab, since they have no escape. Double quotes write the printable characters raw, the tab included, and escape
/// the line breaks, the C0 controls, NEL, NBSP, the two Unicode line separators, DEL, the C1 controls, U+FFFE, U+FFFF
/// and the byte order mark, with the named escapes of the specification where it has one. The tree keeps no spelling
/// of an escape, so a consumer that double-quotes a string gets the named escapes. Readers of YAML 1.1 keep them,
/// while they read a raw NEL or separator as a line break.
///
/// A root plain scalar and the key of an entry of a root mapping start at the first column. When such a text begins
/// with `---` or `...` and a blank or the end of the text follows, the writer puts the whole root one column in,
/// where the text is a scalar and no document marker.
///
/// An implicit key is limited to 1024 characters. The writer measures the key as it writes it. It writes the normal
/// spelling first and spells the key as short as the productions allow only when the normal one passes the limit:
/// a flow collection as a key has a `,` without a blank behind it and writes a one-entry mapping in a sequence as a
/// flow pair, and a double-quoted scalar of the key writes NEL, NBSP, the line and paragraph separators, DEL, the C1
/// controls, U+FFFE, U+FFFF and the byte order mark raw.
///
/// The writer adds a `---` where the text would otherwise change the document count or lose the directives. That
/// happens for a document with directives or trivia in its prelude and for a document that follows one without
/// `...`. A document with a missing root gets a `---` too, because an empty text holds no document. The writer
/// adds a `...` where a document with directives follows one without it.
module Writer =

    let private fail (message: string) : 'a =
        raise (YamlWriteException { Message = message; Path = [] })

    let private checkOptions (options: WriterOptions) =
        if options.Indent < 1 then
            fail "The indentation of the writer has to be at least 1."
        if options.NewLine <> "\n" && options.NewLine <> "\r\n" && options.NewLine <> "\r" then
            fail "The line break of the writer has to be \"\\n\", \"\\r\\n\" or \"\\r\"."

    let private checkTree (problems: string list) =
        if not problems.IsEmpty then
            fail ("The tree breaks the rules of the model:\n" + String.concat "\n" problems)

    /// Writes one document. A missing root and directives get their markers.
    let write (options: WriterOptions) (document: YamlDocument) : string =
        checkOptions options
        checkTree (Validate.document document)
        let engine = Engine(options)
        engine.WriteStream({ Parts = [ StreamPart.Document document ] }, false)
        engine.Result()

    /// Writes the parts of a stream in their order.
    let writeDocuments (options: WriterOptions) (stream: YamlStream) : string =
        checkOptions options
        checkTree (Validate.stream stream)
        let engine = Engine(options)
        engine.WriteStream(stream, true)
        engine.Result()

    /// Writes a node as the root of a document without markers. A missing root gets a `---`, because an empty
    /// text holds no document.
    let writeNode (options: WriterOptions) (node: YamlNode) : string = write options (YamlDocument.create node)

    /// True when the text of the node ends in a block scalar that the writer writes with the chomping Keep. A caller
    /// that joins the text of several nodes leaves out the blank line behind such a node. The empty lines behind a Keep
    /// scalar belong to its value, so `Validate` rejects a blank line behind a stored Keep that fits the value, and a
    /// scalar that needs Keep because its value ends in several line feeds would become a double-quoted scalar when a
    /// blank line followed it.
    ///
    /// The answer follows the last node of the text, which is the value of the last entry of a block mapping (its key
    /// when no colon follows) and the node of the last item of a block sequence, one level after the other. It stops
    /// at a collection that has trivia at its end, because those lines end the text and end the empty lines of the
    /// scalar. It stops at a flow collection and at every scalar that is no block scalar. The chomping is the one that
    /// the writer writes when more text follows the node: the stored chomping when it fits the value, and the one that
    /// the value needs otherwise. A stored Keep on a value without a final line feed does not fit, so the writer
    /// writes Strip there and the answer is false. The policy `Canonical` writes no block scalar, so the answer is
    /// true only for a stored Keep that fits the value, because `Validate` rejects the blank line behind it under
    /// every policy.
    let endsWithKeptLines (options: WriterOptions) (node: YamlNode) : bool =
        let mutable current = node
        let mutable result = false
        let mutable walking = true
        while walking do
            match current.Kind with
            | YamlNodeKind.Scalar scalar ->
                walking <- false
                match scalar.Style with
                | ScalarStyle.Block (_, stored, _) ->
                    result <-
                        if options.Styles = StylePolicy.Canonical then
                            Validate.storedKeepApplies scalar.Style scalar.Value
                        else
                            Validate.isBlockText scalar.Value && fst (Validate.blockChomping scalar.Value stored false) = ChompingMode.Keep
                | _ -> ()
            | YamlNodeKind.Mapping collection when collection.Style = CollectionStyle.Block && collection.End.IsEmpty && not collection.Items.IsEmpty ->
                let last = List.last collection.Items
                current <- if last.Colon.IsSome then last.Value else last.Key
            | YamlNodeKind.Sequence collection when collection.Style = CollectionStyle.Block && collection.End.IsEmpty && not collection.Items.IsEmpty ->
                current <- (List.last collection.Items).Node
            | _ -> walking <- false
        result
