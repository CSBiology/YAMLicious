namespace YAMLicious

open System
open System.Text

/// Projections of a stream into plain JSON, for the comparison of a parse with another parser. They do not
/// depend on the F# types, so the output of this project, of the old library and of yaml@2.6.1 can be
/// compared as JSON, one line for each input.
///
/// Three contracts, which stay apart on purpose:
///
/// - `data`: documents, node kinds, ordered entries, the type that the core schema gives a scalar, its
///   cooked text, its explicit tag and its anchor. Aliases stay symbolic.
/// - `presentation`: scalar styles, collection styles, the layout of the properties, directives and
///   markers, and every comment and blank line in source order with the slot of the model that holds it.
/// - `failure`: the error codes and the lines of a rejected input, as pairs of a code and a 1-based line.
///
/// A walk keeps its own stack on the heap, so a tree of any depth is safe, and each walk visits a node
/// once. All text goes into one StringBuilder for each list, and a path is built only when an item that
/// needs it is written.
module Projection =

    open NodePath

    /// The result of a parse for the line of a corpus run: a stream, or the errors as pairs of an error code
    /// and a 1-based line.
    [<RequireQualifiedAccess; NoEquality; NoComparison>]
    type Outcome =
        | Accepted of YamlStream
        | Rejected of (string * int) list

    // ------------------------------------------------------------------------------------------
    // JSON text
    // ------------------------------------------------------------------------------------------

    let private hexDigits = "0123456789abcdef"

    let private appendUnicodeEscape (sb: StringBuilder) (code: int) =
        sb.Append("\\u") |> ignore
        sb.Append(hexDigits.[(code >>> 12) &&& 15]) |> ignore
        sb.Append(hexDigits.[(code >>> 8) &&& 15]) |> ignore
        sb.Append(hexDigits.[(code >>> 4) &&& 15]) |> ignore
        sb.Append(hexDigits.[code &&& 15]) |> ignore

    /// Appends the text as a JSON string with the escapes of JSON.stringify: the quote, the backslash, the
    /// control characters and a surrogate that has no partner. A pair of surrogates stays as it is.
    let private appendJson (sb: StringBuilder) (text: string) =
        sb.Append('"') |> ignore
        let mutable i = 0
        while i < text.Length do
            let c = text.[i]
            let code = int c
            if c = '"' then
                sb.Append("\\\"") |> ignore
            elif c = '\\' then
                sb.Append("\\\\") |> ignore
            elif code < 32 then
                if code = 8 then sb.Append("\\b") |> ignore
                elif code = 9 then sb.Append("\\t") |> ignore
                elif code = 10 then sb.Append("\\n") |> ignore
                elif code = 12 then sb.Append("\\f") |> ignore
                elif code = 13 then sb.Append("\\r") |> ignore
                else appendUnicodeEscape sb code
            elif code >= 0xD800 && code <= 0xDBFF then
                if i + 1 < text.Length && int text.[i + 1] >= 0xDC00 && int text.[i + 1] <= 0xDFFF then
                    sb.Append(c) |> ignore
                    sb.Append(text.[i + 1]) |> ignore
                    i <- i + 1
                else
                    appendUnicodeEscape sb code
            elif code >= 0xDC00 && code <= 0xDFFF then
                appendUnicodeEscape sb code
            else
                sb.Append(c) |> ignore
            i <- i + 1
        sb.Append('"') |> ignore

    let private appendJsonOption (sb: StringBuilder) (text: string option) =
        match text with
        | Some value -> appendJson sb value
        | None -> sb.Append("null") |> ignore

    let private jsonString (text: string) : string =
        let sb = StringBuilder()
        appendJson sb text
        sb.ToString()

    // ------------------------------------------------------------------------------------------
    // The type of a scalar
    // ------------------------------------------------------------------------------------------

    let private isDigit (c: char) = c >= '0' && c <= '9'
    let private isOctalDigit (c: char) = c >= '0' && c <= '7'
    let private isHexDigit (c: char) = isDigit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

    /// True when the text has at least one character from `start` on, and each one passes the test.
    let private allFrom (text: string) (start: int) (test: char -> bool) =
        if start >= text.Length then
            false
        else
            let mutable ok = true
            let mutable i = start
            while ok && i < text.Length do
                if not (test text.[i]) then ok <- false
                i <- i + 1
            ok

    let private isDecimalInt (text: string) =
        if text.Length = 0 then
            false
        else
            let start = if text.[0] = '-' || text.[0] = '+' then 1 else 0
            allFrom text start isDigit

    /// The form of an int in the core schema, without a limit on its size: decimal with a sign, 0o octal
    /// and 0x hexadecimal. CoreSchema.tryParseInt64 stops at 64 bits, and a longer int is still an int.
    let private isIntForm (text: string) =
        isDecimalInt text
        || (text.Length > 2 && text.[0] = '0' && text.[1] = 'o' && allFrom text 2 isOctalDigit)
        || (text.Length > 2 && text.[0] = '0' && text.[1] = 'x' && allFrom text 2 isHexDigit)

    /// The type that the core schema gives a scalar: "null", "bool", "int", "float", "str" or "other".
    ///
    /// A plain scalar without a tag has the type that its text says. A quoted or block scalar is a string.
    /// An explicit tag of the core namespace fixes the type, whatever the style, but only when the text has
    /// the form of that type. Otherwise the tag stays unresolved and the scalar is a string, as in
    /// yaml@2.6.1 (`!!int abc`, `!!float 5` and `!!bool maybe` are strings). A tag of another namespace, a
    /// local tag and the non-specific tag `!` make strings. The tags binary and timestamp make "other", and a
    /// scalar under `!!set` or `!!omap` is a string, because yaml@2.6.1 cannot resolve the tag for a scalar.
    let private scalarType (tag: YamlTag option) (scalar: YamlScalar) : string =
        let text = scalar.Value
        match tag with
        | Some explicit ->
            let identifier = YamlTag.identifier explicit
            let namespaceLength = YamlTag.coreNamespace.Length
            if identifier.Length > namespaceLength && identifier.Substring(0, namespaceLength) = YamlTag.coreNamespace then
                let suffix = identifier.Substring namespaceLength
                if suffix = "int" then
                    (if isIntForm text then "int" else "str")
                elif suffix = "float" then
                    (if (CoreSchema.tryParseFloat tag scalar).IsSome && not (isDecimalInt text) then "float" else "str")
                elif suffix = "bool" then
                    (if (CoreSchema.tryParseBool tag scalar).IsSome then "bool" else "str")
                elif suffix = "null" then
                    (if CoreSchema.isNull tag scalar then "null" else "str")
                elif suffix = "binary" || suffix = "timestamp" then
                    "other"
                else
                    "str"
            else
                "str"
        | None ->
            match scalar.Style with
            | ScalarStyle.Plain ->
                if CoreSchema.isNull None scalar then "null"
                elif (CoreSchema.tryParseBool None scalar).IsSome then "bool"
                elif isIntForm text then "int"
                elif (CoreSchema.tryParseFloat None scalar).IsSome then "float"
                else "str"
            | _ -> "str"

    let private appendTagField (sb: StringBuilder) (properties: NodeProperties) =
        sb.Append("\"tag\":") |> ignore
        match properties.Tag with
        | Some tag -> appendJson sb (YamlTag.identifier tag)
        | None -> sb.Append("null") |> ignore
        sb.Append(",\"anchor\":") |> ignore
        appendJsonOption sb properties.Anchor

    let private collectionStyleName (style: CollectionStyle) =
        match style with
        | CollectionStyle.Block -> "block"
        | CollectionStyle.Flow -> "flow"

    // ------------------------------------------------------------------------------------------
    // The data projection
    // ------------------------------------------------------------------------------------------

    [<RequireQualifiedAccess; NoEquality; NoComparison>]
    type private DataStep =
        | Text of string
        | Visit of node: YamlNode * depth: int

    /// Writes the data tree of a node and returns the deepest nesting of collections. A scalar root has the
    /// depth 0, a collection of scalars the depth 1, and an alias is no collection.
    let private appendData (sb: StringBuilder) (root: YamlNode) : int =
        let stack = ResizeArray<DataStep>()
        stack.Add(DataStep.Visit(root, 1))
        let mutable deepest = 0
        while stack.Count > 0 do
            let last = stack.Count - 1
            let step = stack.[last]
            stack.RemoveAt last
            match step with
            | DataStep.Text text -> sb.Append(text) |> ignore
            | DataStep.Visit (node, depth) ->
                match node.Kind with
                | YamlNodeKind.Scalar scalar ->
                    sb.Append("{\"k\":\"scalar\",\"t\":\"").Append(scalarType node.Properties.Tag scalar).Append("\",\"v\":") |> ignore
                    appendJson sb scalar.Value
                    sb.Append(',') |> ignore
                    appendTagField sb node.Properties
                    sb.Append('}') |> ignore
                | YamlNodeKind.Alias target ->
                    sb.Append("{\"k\":\"alias\",\"name\":") |> ignore
                    appendJson sb target
                    sb.Append('}') |> ignore
                | YamlNodeKind.Mapping collection ->
                    if depth > deepest then deepest <- depth
                    sb.Append("{\"k\":\"map\",") |> ignore
                    appendTagField sb node.Properties
                    sb.Append(",\"style\":\"").Append(collectionStyleName collection.Style).Append("\",\"entries\":[") |> ignore
                    stack.Add(DataStep.Text "]}")
                    let entries = List.toArray collection.Items
                    for i in entries.Length - 1 .. -1 .. 0 do
                        let entry = entries.[i]
                        stack.Add(DataStep.Text "]")
                        stack.Add(DataStep.Visit(entry.Value, depth + 1))
                        stack.Add(DataStep.Text ",")
                        stack.Add(DataStep.Visit(entry.Key, depth + 1))
                        stack.Add(DataStep.Text "[")
                        if i > 0 then stack.Add(DataStep.Text ",")
                | YamlNodeKind.Sequence collection ->
                    if depth > deepest then deepest <- depth
                    sb.Append("{\"k\":\"seq\",") |> ignore
                    appendTagField sb node.Properties
                    sb.Append(",\"style\":\"").Append(collectionStyleName collection.Style).Append("\",\"items\":[") |> ignore
                    stack.Add(DataStep.Text "]}")
                    let items = List.toArray collection.Items
                    for i in items.Length - 1 .. -1 .. 0 do
                        stack.Add(DataStep.Visit(items.[i].Node, depth + 1))
                        if i > 0 then stack.Add(DataStep.Text ",")
        deepest

    /// The fields of the data projection without the braces: ok, docCount, maxDepth and the documents.
    let private dataFields (stream: YamlStream) : string =
        let documents = StringBuilder()
        let mutable count = 0
        let mutable deepest = 0
        for part in stream.Parts do
            match part with
            | StreamPart.Document document ->
                if count > 0 then documents.Append(',') |> ignore
                documents.Append("{\"data\":") |> ignore
                let depth = appendData documents document.Root
                documents.Append(",\"maxDepth\":").Append(string depth).Append('}') |> ignore
                if depth > deepest then deepest <- depth
                count <- count + 1
            | _ -> ()
        "\"ok\":true,\"docCount\":" + string count + ",\"maxDepth\":" + string deepest + ",\"docs\":[" + documents.ToString() + "]"

    /// The data projection of a stream:
    /// `{"ok":true,"docCount":N,"maxDepth":D,"docs":[{"data":NODE,"maxDepth":d},...]}`.
    ///
    /// A node is one of
    /// `{"k":"scalar","t":TYPE,"v":TEXT,"tag":ID|null,"anchor":NAME|null}`,
    /// `{"k":"map","tag":..,"anchor":..,"style":"block"|"flow","entries":[[KEY,VALUE],...]}`,
    /// `{"k":"seq","tag":..,"anchor":..,"style":"block"|"flow","items":[...]}` and `{"k":"alias","name":NAME}`.
    /// TYPE is null, bool, int, float, str or other. The tag is the expanded identifier of the tag.
    let data (stream: YamlStream) : string = "{" + dataFields stream + "}"

    // ------------------------------------------------------------------------------------------
    // The presentation projection
    // ------------------------------------------------------------------------------------------

    [<RequireQualifiedAccess; NoEquality; NoComparison>]
    type private Step =
        | Visit of node: YamlNode * path: PathInfo * isKey: bool
        | Comment of comment: YamlComment * path: PathInfo * suffix: string * ownLine: bool
        | Blank of span: SourceSpan option * path: PathInfo * suffix: string

    /// The lists of one document.
    type private DocumentLists =
        { Styles: StringBuilder
          Collections: StringBuilder
          Layouts: StringBuilder
          mutable StyleCount: int
          mutable CollectionCount: int
          mutable LayoutCount: int }

    /// The trivia list of the whole stream, in source order.
    type private TriviaList =
        { Items: StringBuilder
          mutable Count: int
          /// The index of the document that the items belong to, or -1 for a part of the stream.
          mutable Document: int }

    let private newLists () =
        { Styles = StringBuilder()
          Collections = StringBuilder()
          Layouts = StringBuilder()
          StyleCount = 0
          CollectionCount = 0
          LayoutCount = 0 }

    let private startItem (sb: StringBuilder) (count: int) =
        if count > 0 then sb.Append(',') |> ignore

    let private appendDocumentField (trivia: TriviaList) =
        trivia.Items.Append(",\"doc\":") |> ignore
        if trivia.Document >= 0 then trivia.Items.Append(string trivia.Document) |> ignore else trivia.Items.Append("null") |> ignore

    let private appendComment (trivia: TriviaList) (comment: YamlComment) (path: PathInfo) (suffix: string) (ownLine: bool) =
        startItem trivia.Items trivia.Count
        trivia.Count <- trivia.Count + 1
        let sb = trivia.Items
        sb.Append("{\"kind\":\"comment\",\"text\":") |> ignore
        appendJson sb comment.Text
        match comment.Span with
        | Some span -> sb.Append(",\"line\":").Append(string span.Start.Line).Append(",\"col\":").Append(string span.Start.Column) |> ignore
        | None -> sb.Append(",\"line\":null,\"col\":null") |> ignore
        sb.Append(",\"placement\":\"").Append(if ownLine then "ownLine" else "endOfLine").Append("\",\"slot\":") |> ignore
        appendJson sb (slotOf path + suffix)
        sb.Append(",\"path\":") |> ignore
        appendJson sb (pointerOf path)
        appendDocumentField trivia
        sb.Append('}') |> ignore

    /// A blank line has the line of its span and no column, because the lines of the reference parser carry
    /// no column either. A constructed blank line has no line.
    let private appendBlank (trivia: TriviaList) (span: SourceSpan option) (path: PathInfo) (suffix: string) =
        startItem trivia.Items trivia.Count
        trivia.Count <- trivia.Count + 1
        let sb = trivia.Items
        sb.Append("{\"kind\":\"blank\",\"text\":null,\"line\":") |> ignore
        match span with
        | Some range -> sb.Append(string range.Start.Line) |> ignore
        | None -> sb.Append("null") |> ignore
        sb.Append(",\"col\":null,\"placement\":\"ownLine\",\"slot\":") |> ignore
        appendJson sb (slotOf path + suffix)
        sb.Append(",\"path\":") |> ignore
        appendJson sb (pointerOf path)
        appendDocumentField trivia
        sb.Append('}') |> ignore

    /// Pushes the trivia lines so that they pop in their order. `prefix` is the slot of the list, as in
    /// ".entries[2].Before", and the index of the line is added in brackets.
    let private pushTrivia (stack: ResizeArray<Step>) (lines: Trivia list) (path: PathInfo) (prefix: string) =
        let all = List.toArray lines
        for k in all.Length - 1 .. -1 .. 0 do
            let suffix = prefix + "[" + string k + "]"
            match all.[k] with
            | FullLineComment comment -> stack.Add(Step.Comment(comment, path, suffix, true))
            | BlankLine span -> stack.Add(Step.Blank(span, path, suffix))

    /// Pushes the comment and the following lines of a separation behind an indicator so that they pop in order.
    let private pushSeparation (stack: ResizeArray<Step>) (separation: Separation) (path: PathInfo) (prefix: string) =
        match separation with
        | Separation.Inline -> ()
        | Separation.NewLine lineBreak ->
            pushTrivia stack lineBreak.Following path (prefix + ".Following")
            match lineBreak.Comment with
            | Some comment -> stack.Add(Step.Comment(comment, path, prefix + ".Comment", false))
            | None -> ()

    let private pushSeparationOption (stack: ResizeArray<Step>) (separation: Separation option) (path: PathInfo) (prefix: string) =
        match separation with
        | Some value -> pushSeparation stack value path prefix
        | None -> ()

    let private scalarStyleName (style: ScalarStyle) =
        match style with
        | ScalarStyle.Plain -> "plain"
        | ScalarStyle.SingleQuoted -> "single"
        | ScalarStyle.DoubleQuoted -> "double"
        | ScalarStyle.Block (BlockScalarStyle.Literal, _, _) -> "literal"
        | ScalarStyle.Block (BlockScalarStyle.Folded, _, _) -> "folded"

    let private chompName (chomping: ChompingMode) =
        match chomping with
        | ChompingMode.Strip -> "strip"
        | ChompingMode.Clip -> "clip"
        | ChompingMode.Keep -> "keep"

    let private appendStyle (lists: DocumentLists) (path: PathInfo) (isKey: bool) (scalar: YamlScalar) =
        startItem lists.Styles lists.StyleCount
        lists.StyleCount <- lists.StyleCount + 1
        let sb = lists.Styles
        sb.Append("{\"path\":") |> ignore
        appendJson sb (pointerOf path)
        sb.Append(",\"role\":\"").Append(if isKey then "key" else "value").Append("\",\"style\":\"").Append(scalarStyleName scalar.Style).Append('"') |> ignore
        match scalar.Style with
        | ScalarStyle.Block (_, chomping, indentation) ->
            sb.Append(",\"chomp\":\"").Append(chompName chomping).Append("\",\"indent\":") |> ignore
            match indentation with
            | Some digit -> sb.Append(string digit) |> ignore
            | None -> sb.Append("null") |> ignore
        | _ -> ()
        sb.Append('}') |> ignore

    let private appendCollection (lists: DocumentLists) (path: PathInfo) (isKey: bool) (style: CollectionStyle) =
        startItem lists.Collections lists.CollectionCount
        lists.CollectionCount <- lists.CollectionCount + 1
        let sb = lists.Collections
        sb.Append("{\"path\":") |> ignore
        appendJson sb (pointerOf path)
        sb.Append(",\"role\":\"").Append(if isKey then "key" else "value").Append("\",\"style\":\"").Append(collectionStyleName style).Append("\"}") |> ignore

    let private appendLayout (lists: DocumentLists) (path: PathInfo) (isKey: bool) (layout: (PropertyName * Separation) list) =
        startItem lists.Layouts lists.LayoutCount
        lists.LayoutCount <- lists.LayoutCount + 1
        let sb = lists.Layouts
        sb.Append("{\"path\":") |> ignore
        appendJson sb (pointerOf path)
        sb.Append(",\"role\":\"").Append(if isKey then "key" else "value").Append("\",\"layout\":[") |> ignore
        let mutable first = true
        for (property, after) in layout do
            if not first then sb.Append(',') |> ignore
            first <- false
            let name = if property = PropertyName.Tag then "tag" else "anchor"
            let separation =
                match after with
                | Separation.Inline -> "inline"
                | Separation.NewLine _ -> "newline"
            sb.Append("{\"property\":\"").Append(name).Append("\",\"after\":\"").Append(separation).Append("\"}") |> ignore
        sb.Append("]}") |> ignore

    /// Walks a node and everything under it in the order of the text. It writes the styles, the collection
    /// styles and the layouts into the lists of the document, and the comments and blank lines into the
    /// trivia list.
    let private walkNode (lists: DocumentLists) (trivia: TriviaList) (root: YamlNode) =
        let stack = ResizeArray<Step>()
        stack.Add(Step.Visit(root, rootPath, false))
        while stack.Count > 0 do
            let last = stack.Count - 1
            let step = stack.[last]
            stack.RemoveAt last
            match step with
            | Step.Comment (comment, path, suffix, ownLine) -> appendComment trivia comment path suffix ownLine
            | Step.Blank (span, path, suffix) -> appendBlank trivia span path suffix
            | Step.Visit (node, path, isKey) ->
                let layout = node.PropertyLayout |> List.map (fun placement -> placement.Property, placement.After)
                if not layout.IsEmpty then appendLayout lists path isKey layout
                // The comments behind the properties come first, because the properties stand in front of the content.
                let mutable index = 0
                for placement in node.PropertyLayout do
                    match placement.After with
                    | Separation.NewLine lineBreak ->
                        let prefix = ".PropertyLayout[" + string index + "].After"
                        match lineBreak.Comment with
                        | Some comment -> appendComment trivia comment path (prefix + ".Comment") false
                        | None -> ()
                        let mutable k = 0
                        for line in lineBreak.Following do
                            let suffix = prefix + ".Following[" + string k + "]"
                            match line with
                            | FullLineComment comment -> appendComment trivia comment path suffix true
                            | BlankLine span -> appendBlank trivia span path suffix
                            k <- k + 1
                    | Separation.Inline -> ()
                    index <- index + 1
                // The comment behind the node is the last field of the walk, so it goes on the stack first.
                match node.TrailingComment with
                | Some comment -> stack.Add(Step.Comment(comment, path, ".TrailingComment", false))
                | None -> ()
                match node.Kind with
                | YamlNodeKind.Scalar scalar ->
                    appendStyle lists path isKey scalar
                    match scalar.HeaderComment with
                    | Some comment -> appendComment trivia comment path ".HeaderComment" false
                    | None -> ()
                | YamlNodeKind.Alias _ -> ()
                | YamlNodeKind.Mapping collection ->
                    appendCollection lists path isKey collection.Style
                    match collection.OpeningComment with
                    | Some comment -> appendComment trivia comment path ".OpeningComment" false
                    | None -> ()
                    pushTrivia stack collection.End path ".End"
                    let entries = List.toArray collection.Items
                    for i in entries.Length - 1 .. -1 .. 0 do
                        let entry = entries.[i]
                        let keyText =
                            match entry.Key.Kind with
                            | YamlNodeKind.Scalar scalar -> scalar.Value
                            | _ -> "?" + string i
                        let pointer = "/" + escapePointer keyText
                        let prefix = ".entries[" + string i + "]"
                        stack.Add(Step.Visit(entry.Value, child path pointer (prefix + ".Value"), false))
                        pushSeparationOption stack entry.Colon path (prefix + ".Colon")
                        pushTrivia stack entry.BeforeColon path (prefix + ".BeforeColon")
                        stack.Add(Step.Visit(entry.Key, child path pointer (prefix + ".Key"), true))
                        pushSeparationOption stack entry.Question path (prefix + ".Question")
                        pushTrivia stack entry.Before path (prefix + ".Before")
                | YamlNodeKind.Sequence collection ->
                    appendCollection lists path isKey collection.Style
                    match collection.OpeningComment with
                    | Some comment -> appendComment trivia comment path ".OpeningComment" false
                    | None -> ()
                    pushTrivia stack collection.End path ".End"
                    let items = List.toArray collection.Items
                    for i in items.Length - 1 .. -1 .. 0 do
                        let item = items.[i]
                        let prefix = ".items[" + string i + "]"
                        stack.Add(Step.Visit(item.Node, child path ("/" + string i) (prefix + ".Node"), false))
                        pushSeparationOption stack item.Dash path (prefix + ".Dash")
                        pushTrivia stack item.Before path (prefix + ".Before")

    /// Walks the trivia lines and comments of the document that stand outside the tree. `stack` is only
    /// used to run the shared push helpers.
    let private walkTriviaList (trivia: TriviaList) (path: PathInfo) (prefix: string) (lines: Trivia list) =
        let stack = ResizeArray<Step>()
        pushTrivia stack lines path prefix
        while stack.Count > 0 do
            let last = stack.Count - 1
            let step = stack.[last]
            stack.RemoveAt last
            match step with
            | Step.Comment (comment, p, suffix, ownLine) -> appendComment trivia comment p suffix ownLine
            | Step.Blank (span, p, suffix) -> appendBlank trivia span p suffix
            | Step.Visit _ -> ()

    let private appendDirectives (sb: StringBuilder) (prelude: PreludeItem list) =
        let mutable version: string option = None
        let tags = ResizeArray<string * string>()
        let reserved = ResizeArray<string * string list>()
        for item in prelude do
            match item with
            | Directive directive ->
                match directive.Kind with
                | YamlVersion (major, minor) -> if version.IsNone then version <- Some (string major + "." + string minor)
                | TagDirective (handle, prefix) -> tags.Add((handle, prefix))
                | ReservedDirective (directiveName, arguments) -> reserved.Add((directiveName, arguments))
            | PreludeTrivia _ -> ()
        sb.Append("\"directives\":{\"yaml\":") |> ignore
        appendJsonOption sb version
        sb.Append(",\"tags\":[") |> ignore
        for i in 0 .. tags.Count - 1 do
            if i > 0 then sb.Append(',') |> ignore
            let (handle, prefix) = tags.[i]
            sb.Append('[') |> ignore
            appendJson sb handle
            sb.Append(',') |> ignore
            appendJson sb prefix
            sb.Append(']') |> ignore
        sb.Append("],\"reserved\":[") |> ignore
        for i in 0 .. reserved.Count - 1 do
            if i > 0 then sb.Append(',') |> ignore
            let (directiveName, arguments) = reserved.[i]
            sb.Append('[') |> ignore
            appendJson sb directiveName
            sb.Append(",[") |> ignore
            let mutable first = true
            for argument in arguments do
                if not first then sb.Append(',') |> ignore
                first <- false
                appendJson sb argument
            sb.Append("]]") |> ignore
        sb.Append("]}") |> ignore

    /// The presentation projection of a stream:
    /// `{"docCount":N,"docs":[DOC,...],"trivia":[ITEM,...]}`.
    ///
    /// DOC is `{"styles":[...],"collections":[...],"layouts":[...],"directives":{"yaml":V|null,"tags":[[handle,prefix],...],"reserved":[...]},"docStart":B,"docEnd":B}`.
    /// A style is `{"path":P,"role":"key"|"value","style":S}`, with `"chomp"` and `"indent"` on a block scalar. S is plain,
    /// single, double, literal or folded. A collection is `{"path":P,"role":R,"style":"block"|"flow"}` and a
    /// layout is `{"path":P,"role":R,"layout":[{"property":"tag"|"anchor","after":"inline"|"newline"},...]}`.
    /// P is the key path in the style of a JSON pointer, with `~` and `/` escaped and a non-scalar key written as `?i`.
    ///
    /// ITEM is a comment `{"kind":"comment","text":T,"line":L|null,"col":C|null,"placement":"ownLine"|"endOfLine",
    /// "slot":S,"path":P,"doc":I|null}` or a blank line with `"kind":"blank"` and null text and column. The
    /// items are in source order. `slot` names the field of the model that holds the item, for example
    /// `root.entries[2].Before[0]`, `root.TrailingComment` or `root.entries[0].Colon.Comment`. The slots outside the
    /// tree are `Prelude[i]`, `Prelude[i].TrailingComment`, `StartMarker.TrailingComment`, `BeforeRoot[i]`,
    /// `AfterRoot[i]`, `EndMarker.TrailingComment`, `Parts[i]` and `Parts[i].TrailingComment`. `doc` is the index
    /// of the document, or null for a part of the stream. A path of a node deeper than 64 levels shows `(N)`
    /// and its last 8 segments. The line and the column come from the span of the comment. A blank line has
    /// the line of its span, or a null line when a program constructed it.
    let presentation (stream: YamlStream) : string =
        let documents = StringBuilder()
        let trivia =
            { Items = StringBuilder()
              Count = 0
              Document = -1 }
        let mutable count = 0
        let mutable partIndex = 0
        for part in stream.Parts do
            match part with
            | StreamPart.Document document ->
                trivia.Document <- count
                let lists = newLists ()
                // The order of the text: prelude, start marker, trivia in front of the root, the root, trivia behind it, end marker.
                let mutable index = 0
                for item in document.Prelude do
                    match item with
                    | Directive directive ->
                        match directive.TrailingComment with
                        | Some comment -> appendComment trivia comment documentPath ("Prelude[" + string index + "].TrailingComment") false
                        | None -> ()
                    | PreludeTrivia line ->
                        let suffix = "Prelude[" + string index + "]"
                        match line with
                        | FullLineComment comment -> appendComment trivia comment documentPath suffix true
                        | BlankLine span -> appendBlank trivia span documentPath suffix
                    index <- index + 1
                match document.StartMarker with
                | Some marker ->
                    match marker.TrailingComment with
                    | Some comment -> appendComment trivia comment documentPath "StartMarker.TrailingComment" false
                    | None -> ()
                | None -> ()
                walkTriviaList trivia documentPath "BeforeRoot" document.BeforeRoot
                walkNode lists trivia document.Root
                walkTriviaList trivia documentPath "AfterRoot" document.AfterRoot
                match document.EndMarker with
                | Some marker ->
                    match marker.TrailingComment with
                    | Some comment -> appendComment trivia comment documentPath "EndMarker.TrailingComment" false
                    | None -> ()
                | None -> ()
                trivia.Document <- -1
                if count > 0 then documents.Append(',') |> ignore
                documents.Append("{\"styles\":[").Append(lists.Styles.ToString()).Append("],\"collections\":[").Append(lists.Collections.ToString()).Append("],\"layouts\":[").Append(lists.Layouts.ToString()).Append("],") |> ignore
                appendDirectives documents document.Prelude
                documents.Append(",\"docStart\":").Append(if document.StartMarker.IsSome then "true" else "false").Append(",\"docEnd\":").Append(if document.EndMarker.IsSome then "true" else "false").Append('}') |> ignore
                count <- count + 1
            | StreamPart.Trivia line ->
                let suffix = "Parts[" + string partIndex + "]"
                match line with
                | FullLineComment comment -> appendComment trivia comment documentPath suffix true
                | BlankLine span -> appendBlank trivia span documentPath suffix
            | StreamPart.ExtraEndMarker marker ->
                match marker.TrailingComment with
                | Some comment -> appendComment trivia comment documentPath ("Parts[" + string partIndex + "].TrailingComment") false
                | None -> ()
            partIndex <- partIndex + 1
        "{\"docCount\":" + string count + ",\"docs\":[" + documents.ToString() + "],\"trivia\":[" + trivia.Items.ToString() + "]}"

    // ------------------------------------------------------------------------------------------
    // The failure projection and the line of a run
    // ------------------------------------------------------------------------------------------

    let private failureFields (errors: (string * int) list) : string =
        let sb = StringBuilder()
        sb.Append("\"ok\":false,\"errors\":[") |> ignore
        let mutable first = true
        for (code, errorLine) in errors do
            if not first then sb.Append(',') |> ignore
            first <- false
            sb.Append("{\"code\":") |> ignore
            appendJson sb code
            sb.Append(",\"line\":").Append(string errorLine).Append('}') |> ignore
        sb.Append(']') |> ignore
        sb.ToString()

    /// The failure projection: `{"ok":false,"errors":[{"code":C,"line":L},...]}` with 1-based lines. A line
    /// of 0 stands for an error that names no line. The parser decides the codes, and this projection writes
    /// them as it gets them.
    let failure (errors: (string * int) list) : string = "{" + failureFields errors + "}"

    /// One line of a corpus run, in the format of the files in m0/ref and m0/old, without a line break:
    /// `{"set":S,"id":I,"sha256":H, ...}`. An accepted stream adds the fields of `data` and a `presentation`
    /// member. A rejected one adds the fields of `failure`.
    let line (set: string) (id: string) (sha256: string) (outcome: Outcome) : string =
        let sb = StringBuilder()
        sb.Append("{\"set\":") |> ignore
        appendJson sb set
        sb.Append(",\"id\":") |> ignore
        appendJson sb id
        sb.Append(",\"sha256\":") |> ignore
        appendJson sb sha256
        sb.Append(',') |> ignore
        match outcome with
        | Outcome.Accepted stream ->
            sb.Append(dataFields stream).Append(",\"presentation\":").Append(presentation stream) |> ignore
        | Outcome.Rejected errors ->
            sb.Append(failureFields errors) |> ignore
        sb.Append('}') |> ignore
        sb.ToString()
