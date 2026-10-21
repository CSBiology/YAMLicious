namespace YAMLicious

open System
open System.Text

/// Equality and printing for trees, without recursion in the depth of the tree.
///
/// The model has no structural equality, because the generic one recurses through the whole tree. Both
/// operations here walk the tree with an explicit stack that lives on the heap, so a sequence nested
/// 10000 levels deep needs no more call stack than a flat one. They visit each node once, so the time
/// is linear in the number of nodes and comments.
///
/// Source spans are ignored in the comparison and absent from the printout. The comparison does not
/// ignore comments, trivia, styles, properties or the layout of the properties. A comment in another slot
/// makes two trees different.
module internal NodeComparison =

    let private commentsEqual (a: YamlComment option) (b: YamlComment option) =
        match a, b with
        | None, None -> true
        | Some x, Some y -> x.Text = y.Text
        | _ -> false

    let private triviaEqual (a: Trivia) (b: Trivia) =
        match a, b with
        | FullLineComment x, FullLineComment y -> x.Text = y.Text
        | BlankLine _, BlankLine _ -> true
        | _ -> false

    let private triviaListEqual (a: Trivia list) (b: Trivia list) =
        List.length a = List.length b && List.forall2 triviaEqual a b

    let private separationEqual (a: Separation) (b: Separation) =
        match a, b with
        | Separation.Inline, Separation.Inline -> true
        | Separation.NewLine x, Separation.NewLine y -> commentsEqual x.Comment y.Comment && triviaListEqual x.Following y.Following
        | _ -> false

    let private separationOptionEqual (a: Separation option) (b: Separation option) =
        match a, b with
        | None, None -> true
        | Some x, Some y -> separationEqual x y
        | _ -> false

    let private propertiesEqual (a: NodeProperties) (b: NodeProperties) =
        a.Anchor = b.Anchor
        && (match a.Tag, b.Tag with
            | None, None -> true
            | Some x, Some y -> YamlTag.identifier x = YamlTag.identifier y
            | _ -> false)

    let private placementEqual (a: PropertyPlacement) (b: PropertyPlacement) =
        a.Property = b.Property && separationEqual a.After b.After

    let private scalarEqual (a: YamlScalar) (b: YamlScalar) =
        a.Value = b.Value && a.Style = b.Style && commentsEqual a.HeaderComment b.HeaderComment

    let private collectionEqual (a: Collection<'Item>) (b: Collection<'Item>) =
        a.Style = b.Style
        && commentsEqual a.OpeningComment b.OpeningComment
        && triviaListEqual a.End b.End
        && List.length a.Items = List.length b.Items

    let private entryEqual (a: MappingEntry) (b: MappingEntry) =
        triviaListEqual a.Before b.Before
        && separationOptionEqual a.Question b.Question
        && triviaListEqual a.BeforeColon b.BeforeColon
        && separationOptionEqual a.Colon b.Colon

    let private itemEqual (a: SequenceItem) (b: SequenceItem) =
        triviaListEqual a.Before b.Before && separationOptionEqual a.Dash b.Dash

    /// True when both trees are equal except for their source spans.
    let nodesEqualIgnoringSpans (left: YamlNode) (right: YamlNode) : bool =
        let pending = ResizeArray<YamlNode * YamlNode>()
        pending.Add((left, right))
        let mutable equal = true
        while equal && pending.Count > 0 do
            let last = pending.Count - 1
            let (a, b) = pending.[last]
            pending.RemoveAt last
            let sameOwnFields =
                propertiesEqual a.Properties b.Properties
                && List.length a.PropertyLayout = List.length b.PropertyLayout
                && List.forall2 placementEqual a.PropertyLayout b.PropertyLayout
                && commentsEqual a.TrailingComment b.TrailingComment
            if not sameOwnFields then
                equal <- false
            else
                match a.Kind, b.Kind with
                | YamlNodeKind.Scalar x, YamlNodeKind.Scalar y ->
                    if not (scalarEqual x y) then equal <- false
                | YamlNodeKind.Alias x, YamlNodeKind.Alias y ->
                    if x <> y then equal <- false
                | YamlNodeKind.Mapping x, YamlNodeKind.Mapping y ->
                    if not (collectionEqual x y) then
                        equal <- false
                    else
                        let mutable xs = x.Items
                        let mutable ys = y.Items
                        while equal && not xs.IsEmpty do
                            let ex = xs.Head
                            let ey = ys.Head
                            if not (entryEqual ex ey) then
                                equal <- false
                            else
                                pending.Add((ex.Value, ey.Value))
                                pending.Add((ex.Key, ey.Key))
                            xs <- xs.Tail
                            ys <- ys.Tail
                | YamlNodeKind.Sequence x, YamlNodeKind.Sequence y ->
                    if not (collectionEqual x y) then
                        equal <- false
                    else
                        let mutable xs = x.Items
                        let mutable ys = y.Items
                        while equal && not xs.IsEmpty do
                            let ix = xs.Head
                            let iy = ys.Head
                            if not (itemEqual ix iy) then
                                equal <- false
                            else
                                pending.Add((ix.Node, iy.Node))
                            xs <- xs.Tail
                            ys <- ys.Tail
                | _ ->
                    equal <- false
        equal

    let private markerEqual (a: DocumentMarker option) (b: DocumentMarker option) =
        match a, b with
        | None, None -> true
        | Some x, Some y -> commentsEqual x.TrailingComment y.TrailingComment
        | _ -> false

    let private preludeItemEqual (a: PreludeItem) (b: PreludeItem) =
        match a, b with
        | Directive x, Directive y -> x.Kind = y.Kind && commentsEqual x.TrailingComment y.TrailingComment
        | PreludeTrivia x, PreludeTrivia y -> triviaEqual x y
        | _ -> false

    /// True when both documents are equal except for their source spans.
    let documentsEqualIgnoringSpans (left: YamlDocument) (right: YamlDocument) : bool =
        List.length left.Prelude = List.length right.Prelude
        && List.forall2 preludeItemEqual left.Prelude right.Prelude
        && markerEqual left.StartMarker right.StartMarker
        && triviaListEqual left.BeforeRoot right.BeforeRoot
        && triviaListEqual left.AfterRoot right.AfterRoot
        && markerEqual left.EndMarker right.EndMarker
        && nodesEqualIgnoringSpans left.Root right.Root

    let private partEqual (a: StreamPart) (b: StreamPart) =
        match a, b with
        | StreamPart.Document x, StreamPart.Document y -> documentsEqualIgnoringSpans x y
        | StreamPart.Trivia x, StreamPart.Trivia y -> triviaEqual x y
        | StreamPart.ExtraEndMarker x, StreamPart.ExtraEndMarker y -> markerEqual (Some x) (Some y)
        | _ -> false

    /// True when both streams hold equal parts in the same order, except for their source spans.
    let streamsEqualIgnoringSpans (left: YamlStream) (right: YamlStream) : bool =
        List.length left.Parts = List.length right.Parts && List.forall2 partEqual left.Parts right.Parts

    // The printer indents two spaces for each level up to this depth. A deeper line keeps the indentation of
    // this depth and names its depth in brackets, so that the printout of a tree stays linear in its size.
    let private maxIndentedDepth = 24

    let private indent (depth: int) =
        if depth <= maxIndentedDepth then
            String.replicate (2 * depth) " "
        else
            String.replicate (2 * maxIndentedDepth) " " + "[" + string depth + "] "

    let private escape (text: string) =
        text.Replace("\\", "\\\\").Replace("\"", "\\\"").Replace("\n", "\\n").Replace("\r", "\\r").Replace("\t", "\\t")

    let private quoted (text: string) = "\"" + escape text + "\""

    let private showComment (comment: YamlComment) = "#" + quoted comment.Text

    let private showTrivia (trivia: Trivia) =
        match trivia with
        | FullLineComment comment -> showComment comment
        | BlankLine _ -> "blank"

    let private showTriviaList (items: Trivia list) =
        "[" + String.Join("; ", items |> List.map showTrivia) + "]"

    let private showSeparation (separation: Separation) =
        match separation with
        | Separation.Inline -> "inline"
        | Separation.NewLine lineBreak ->
            let comment =
                match lineBreak.Comment with
                | Some c -> showComment c + " "
                | None -> ""
            "newline(" + comment + showTriviaList lineBreak.Following + ")"

    let private showTag (tag: YamlTag) =
        match tag with
        | YamlTag.NonSpecific -> "!"
        | YamlTag.Named _ -> escape (YamlTag.identifier tag)

    let private showStyle (style: ScalarStyle) =
        match style with
        | ScalarStyle.Plain -> "plain"
        | ScalarStyle.SingleQuoted -> "single"
        | ScalarStyle.DoubleQuoted -> "double"
        | ScalarStyle.Block (blockStyle, chomping, indentation) ->
            let indicator = if blockStyle = BlockScalarStyle.Literal then "|" else ">"
            let digit =
                match indentation with
                | Some n -> string n
                | None -> ""
            let chomp =
                match chomping with
                | ChompingMode.Strip -> "-"
                | ChompingMode.Clip -> ""
                | ChompingMode.Keep -> "+"
            "block(" + indicator + digit + chomp + ")"

    let private showCollectionStyle (style: CollectionStyle) =
        match style with
        | CollectionStyle.Block -> "block"
        | CollectionStyle.Flow -> "flow"

    let private showLayout (placement: PropertyPlacement) =
        let name =
            match placement.Property with
            | PropertyName.Tag -> "tag"
            | PropertyName.Anchor -> "anchor"
        name + ":" + showSeparation placement.After

    /// The line of a node without its children: the kind, then the properties, the layout and the comments
    /// in the order of their slots.
    let private nodeLine (role: string) (node: YamlNode) =
        let sb = StringBuilder()
        sb.Append(role).Append(": ") |> ignore
        match node.Kind with
        | YamlNodeKind.Scalar scalar ->
            sb.Append("scalar ").Append(showStyle scalar.Style).Append(' ').Append(quoted scalar.Value) |> ignore
        | YamlNodeKind.Mapping collection ->
            sb.Append("mapping ").Append(showCollectionStyle collection.Style) |> ignore
        | YamlNodeKind.Sequence collection ->
            sb.Append("sequence ").Append(showCollectionStyle collection.Style) |> ignore
        | YamlNodeKind.Alias name ->
            sb.Append("alias *").Append(escape name) |> ignore
        match node.Properties.Tag with
        | Some tag -> sb.Append(" tag=").Append(showTag tag) |> ignore
        | None -> ()
        match node.Properties.Anchor with
        | Some anchor -> sb.Append(" anchor=&").Append(escape anchor) |> ignore
        | None -> ()
        if not node.PropertyLayout.IsEmpty then
            sb.Append(" layout=[").Append(String.Join("; ", node.PropertyLayout |> List.map showLayout)).Append(']') |> ignore
        match node.Kind with
        | YamlNodeKind.Scalar scalar ->
            match scalar.HeaderComment with
            | Some comment -> sb.Append(" header=").Append(showComment comment) |> ignore
            | None -> ()
        | YamlNodeKind.Mapping collection ->
            match collection.OpeningComment with
            | Some comment -> sb.Append(" opening=").Append(showComment comment) |> ignore
            | None -> ()
        | YamlNodeKind.Sequence collection ->
            match collection.OpeningComment with
            | Some comment -> sb.Append(" opening=").Append(showComment comment) |> ignore
            | None -> ()
        | YamlNodeKind.Alias _ -> ()
        match node.TrailingComment with
        | Some comment -> sb.Append(" comment=").Append(showComment comment) |> ignore
        | None -> ()
        sb.ToString()

    let private entryLine (entry: MappingEntry) =
        let sb = StringBuilder()
        sb.Append("entry") |> ignore
        if not entry.Before.IsEmpty then
            sb.Append(" before=").Append(showTriviaList entry.Before) |> ignore
        match entry.Question with
        | Some separation -> sb.Append(" question=").Append(showSeparation separation) |> ignore
        | None -> ()
        if not entry.BeforeColon.IsEmpty then
            sb.Append(" before-colon=").Append(showTriviaList entry.BeforeColon) |> ignore
        match entry.Colon with
        | Some Separation.Inline -> ()
        | Some separation -> sb.Append(" colon=").Append(showSeparation separation) |> ignore
        | None -> sb.Append(" colon=omitted") |> ignore
        sb.ToString()

    let private itemLine (item: SequenceItem) =
        let sb = StringBuilder()
        sb.Append("item") |> ignore
        if not item.Before.IsEmpty then
            sb.Append(" before=").Append(showTriviaList item.Before) |> ignore
        match item.Dash with
        | Some Separation.Inline -> ()
        | Some separation -> sb.Append(" dash=").Append(showSeparation separation) |> ignore
        | None -> ()
        sb.ToString()

    [<RequireQualifiedAccess>]
    type private Work =
        | Line of depth: int * text: string
        | Node of depth: int * role: string * node: YamlNode

    // Prints the node and its descendants, one line each, at the given depth.
    let private render (output: StringBuilder) (startDepth: int) (role: string) (node: YamlNode) =
        let stack = ResizeArray<Work>()
        stack.Add(Work.Node(startDepth, role, node))
        while stack.Count > 0 do
            let last = stack.Count - 1
            let work = stack.[last]
            stack.RemoveAt last
            match work with
            | Work.Line (depth, text) ->
                output.Append(indent depth).Append(text).Append('\n') |> ignore
            | Work.Node (depth, nodeRole, current) ->
                output.Append(indent depth).Append(nodeLine nodeRole current).Append('\n') |> ignore
                // The stack pops in reverse, so the pushes run from the last line to the first.
                match current.Kind with
                | YamlNodeKind.Mapping collection ->
                    if not collection.End.IsEmpty then
                        stack.Add(Work.Line(depth + 1, "end=" + showTriviaList collection.End))
                    for entry in List.rev collection.Items do
                        stack.Add(Work.Node(depth + 2, "value", entry.Value))
                        stack.Add(Work.Node(depth + 2, "key", entry.Key))
                        stack.Add(Work.Line(depth + 1, entryLine entry))
                | YamlNodeKind.Sequence collection ->
                    if not collection.End.IsEmpty then
                        stack.Add(Work.Line(depth + 1, "end=" + showTriviaList collection.End))
                    for item in List.rev collection.Items do
                        stack.Add(Work.Node(depth + 2, "node", item.Node))
                        stack.Add(Work.Line(depth + 1, itemLine item))
                | YamlNodeKind.Scalar _
                | YamlNodeKind.Alias _ -> ()

    /// A readable printout of the tree, one line for each node, entry and item, without spans. The printout
    /// is meant for the messages of failing tests. A line deeper than 24 levels names its depth in brackets.
    let showNode (node: YamlNode) : string =
        let output = StringBuilder()
        render output 0 "root" node
        output.ToString()

    let private showDirective (directive: YamlDirective) =
        let kind =
            match directive.Kind with
            | YamlVersion (major, minor) -> "version " + string major + "." + string minor
            | TagDirective (handle, prefix) -> "tag " + handle + " " + prefix
            | ReservedDirective (directiveName, arguments) -> "reserved " + directiveName + " [" + String.Join(" ", arguments) + "]"
        match directive.TrailingComment with
        | Some comment -> kind + " comment=" + showComment comment
        | None -> kind

    let private appendMarker (output: StringBuilder) (indentation: string) (name: string) (marker: DocumentMarker option) =
        match marker with
        | None -> ()
        | Some m ->
            output.Append(indentation).Append(name) |> ignore
            match m.TrailingComment with
            | Some comment -> output.Append(" comment=").Append(showComment comment) |> ignore
            | None -> ()
            output.Append('\n') |> ignore

    let private appendDocument (output: StringBuilder) (document: YamlDocument) =
        output.Append("document\n") |> ignore
        for item in document.Prelude do
            match item with
            | Directive directive -> output.Append("  prelude: directive ").Append(showDirective directive).Append('\n') |> ignore
            | PreludeTrivia trivia -> output.Append("  prelude: ").Append(showTrivia trivia).Append('\n') |> ignore
        appendMarker output "  " "start-marker" document.StartMarker
        if not document.BeforeRoot.IsEmpty then
            output.Append("  before-root=").Append(showTriviaList document.BeforeRoot).Append('\n') |> ignore
        render output 1 "root" document.Root
        if not document.AfterRoot.IsEmpty then
            output.Append("  after-root=").Append(showTriviaList document.AfterRoot).Append('\n') |> ignore
        appendMarker output "  " "end-marker" document.EndMarker

    /// A readable printout of the document, without spans.
    let showDocument (document: YamlDocument) : string =
        let output = StringBuilder()
        appendDocument output document
        output.ToString()

    /// A readable printout of the stream, without spans: the line `stream`, then the printout of each
    /// document, and one line for each piece of trivia and each extra end marker, in order.
    let showStream (stream: YamlStream) : string =
        let output = StringBuilder()
        output.Append("stream\n") |> ignore
        for part in stream.Parts do
            match part with
            | StreamPart.Document document -> appendDocument output document
            | StreamPart.Trivia trivia -> output.Append("trivia: ").Append(showTrivia trivia).Append('\n') |> ignore
            | StreamPart.ExtraEndMarker marker -> appendMarker output "" "extra-end-marker" (Some marker)
        output.ToString()
