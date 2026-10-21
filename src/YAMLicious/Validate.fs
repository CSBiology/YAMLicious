namespace YAMLicious

open System
open System.Text

/// Paths to the places of a tree, shared by the projection and the validator. A path has two spellings: a
/// key path in the style of a JSON pointer and a path of slots in the model.
module internal NodePath =

    /// The place of a node. A node deeper than `fullPathDepth` shows its depth and the last `tailSegments`
    /// segments only, so the size of the output stays linear in the size of the tree.
    type PathInfo =
        { Parent: PathInfo option
          Pointer: string
          Slot: string
          Depth: int }

    let fullPathDepth = 64
    let tailSegments = 8

    /// `head` stands in front of the first segment of a deep path, which does not reach the root: "root"
    /// for slots and nothing for pointers.
    let materialize (path: PathInfo) (head: string) (select: PathInfo -> string) : string =
        let segments = ResizeArray<string>()
        let mutable current = Some path
        let mutable remaining = if path.Depth <= fullPathDepth then Int32.MaxValue else tailSegments
        while remaining > 0 && current.IsSome do
            let here = current.Value
            segments.Add(select here)
            remaining <- remaining - 1
            current <- here.Parent
        let sb = StringBuilder()
        if path.Depth > fullPathDepth then
            sb.Append(head).Append("(").Append(string path.Depth).Append(")") |> ignore
        for i in segments.Count - 1 .. -1 .. 0 do
            sb.Append(segments.[i]) |> ignore
        sb.ToString()

    let pointerOf (path: PathInfo) = materialize path "" (fun p -> p.Pointer)
    let slotOf (path: PathInfo) = materialize path "root" (fun p -> p.Slot)

    let child (parent: PathInfo) (pointer: string) (slot: string) : PathInfo =
        { Parent = Some parent
          Pointer = pointer
          Slot = slot
          Depth = parent.Depth + 1 }

    let rootPath: PathInfo =
        { Parent = None
          Pointer = ""
          Slot = "root"
          Depth = 0 }

    /// The path for the slots of a document that are not in the tree: the prelude, the markers, and the
    /// trivia around the root.
    let documentPath: PathInfo =
        { Parent = None
          Pointer = ""
          Slot = ""
          Depth = 0 }

    let escapePointer (text: string) = text.Replace("~", "~0").Replace("/", "~1")

/// Checks of a document or a stream against the invariants of the model and the slot rules R1 to R8 that
/// the tree alone can show. An empty list means that the tree is valid. Each message starts with the slot
/// that is wrong, written as the presentation projection writes slots, for example
/// `root.entries[2].Value.TrailingComment`. The slots of a document in a stream start with `Parts[i].`.
///
/// The checks, by the place of the rule:
///
/// - A node: an alias has no properties. `PropertyLayout` names each present property once and nothing
///   else. A block collection and a block scalar have no `TrailingComment` (R8). A missing value has none
///   either (R1), and neither has a node that has properties and no content unless it is an item of a flow
///   sequence or a key or value of a flow mapping entry (R1, R6). `HeaderComment` stands on block
///   scalars only, and `OpeningComment` on flow collections only. An empty collection is a flow collection.
/// - An item: a block item has a dash and a flow item has none.
/// - A separation behind an indicator (`?`, `:` or `-`) or behind the last property: `Following` is empty
///   when a block collection without a property follows or when the content is missing (R2). A block
///   collection starts on a later line than its last property, so that property has a line break behind it,
///   and the colon of an implicit entry has one when a block collection without a property is the value (R1).
///   Behind a dash, a question mark or an explicit colon with an inline separation such a collection continues
///   the line, and then no trivia stand in front of its first entry or item (R2, R4).
/// - An entry: the gap before the colon is empty when no colon follows and for an implicit key in a block
///   mapping (R7).
/// - A block collection: `End` is empty unless the collection is the last node of the text of its document
///   (R4). A trivia run between two nodes goes to the `Before` of the next one, and the run in front of a
///   following document without a `...` goes to its `Prelude`, so no other collection has an `End`.
/// - A document: `BeforeRoot` is empty in front of a block collection without a property and `AfterRoot`
///   is empty behind a block collection, behind a missing root and in front of a document that follows
///   without a `...` between (R3).
/// - A stream: `StreamPart.Trivia` does not stand in front of a document, and in a stream with a document
///   it stands behind a `...` or at the start of the stream in front of an `ExtraEndMarker` only (R3). An
///   `ExtraEndMarker` stands at the start of the stream or behind a part that ends with a `...`, because
///   behind a document without one the parser reads the `...` as the end marker of that document (R3).
///
/// A node has at most one tag and one anchor by its type, so no check is needed for that. The rules R4 to
/// R6 depend on the columns and the lines of the text and are not checked.
///
/// The walk keeps its own stack, so a tree of any depth is safe.
module Validate =

    let private isBlockCollection (node: YamlNode) =
        match node.Kind with
        | YamlNodeKind.Mapping collection -> collection.Style = CollectionStyle.Block
        | YamlNodeKind.Sequence collection -> collection.Style = CollectionStyle.Block
        | _ -> false

    /// A block collection without a property has no token of its own on the line of the indicator.
    let private isBareBlockCollection (node: YamlNode) =
        isBlockCollection node && node.Properties.Tag.IsNone && node.Properties.Anchor.IsNone

    let private isBlockScalar (scalar: YamlScalar) =
        match scalar.Style with
        | ScalarStyle.Block _ -> true
        | _ -> false

    let private followingOf (separation: Separation option) : Trivia list =
        match separation with
        | Some (Separation.NewLine lineBreak) -> lineBreak.Following
        | _ -> []

    let private followingText =
        "the trivia belong to the Before of the first entry or item of the block collection behind the line break (R2)"

    let private missingText =
        "the content behind the indicator is missing, so the trivia belong to the Before of the next entry or item, or to BeforeColon behind a question mark when a colon follows (R2)"

    let private endText =
        "the trivia behind a block collection belong to its End only when the collection is the last node of its document, and otherwise to the Before of the next entry or item (R4)"

    let private startsLaterText =
        "a block collection starts on a later line than its last property, so a line break follows that property (R1)"

    let private colonText =
        "a block collection without a property starts on a later line than the colon of an implicit entry, so a line break follows the colon (R1)"

    let private compactText =
        "a block collection that continues the line of an indicator has no trivia in front of its first entry or item, because they would stand in front of the indicator (R2, R4)"

    /// Reports the trivia in front of the first entry or item of a block collection without a property that continues
    /// the line of its indicator, as in `- - a`, `- a: 1` and `? - a`.
    let private checkCompact (report: string -> string -> unit) (slot: string) (content: YamlNode) =
        if isBareBlockCollection content then
            match content.Kind with
            | YamlNodeKind.Mapping collection ->
                match collection.Items with
                | first :: _ when not first.Before.IsEmpty -> report (slot + ".entries[0].Before") compactText
                | _ -> ()
            | YamlNodeKind.Sequence collection ->
                match collection.Items with
                | first :: _ when not first.Before.IsEmpty -> report (slot + ".items[0].Before") compactText
                | _ -> ()
            | _ -> ()

    /// Reports the trivia behind an indicator when the content behind it is a block collection without a
    /// property or is missing.
    let private checkFollowing (report: string -> string -> unit) (slot: string) (separation: Separation option) (content: YamlNode) =
        if not (followingOf separation).IsEmpty then
            if isBareBlockCollection content then
                report slot followingText
            elif YamlNode.isMissingValue content then
                report slot missingText

    let private walkNode (errors: ResizeArray<string>) (prefix: string) (canEnd: bool) (root: YamlNode) =
        // The first flag tells whether the node is an item or a key or value of a flow collection. The second tells
        // whether the node is the last node of the text of its document, where the trivia behind it can go to its End.
        let stack = ResizeArray<YamlNode * NodePath.PathInfo * bool * bool>()
        stack.Add((root, NodePath.rootPath, false, canEnd))
        while stack.Count > 0 do
            let last = stack.Count - 1
            let (node, path, inFlow, isLastNode) = stack.[last]
            stack.RemoveAt last
            // The slot of the node is built only for an error, so a tree without errors builds no path.
            let report (suffix: string) (text: string) =
                errors.Add(prefix + NodePath.slotOf path + suffix + ": " + text)

            let layoutCount (name: PropertyName) =
                node.PropertyLayout |> List.filter (fun placement -> placement.Property = name) |> List.length
            if layoutCount PropertyName.Tag <> (if node.Properties.Tag.IsSome then 1 else 0) then
                report ".PropertyLayout" "the layout names the tag once when the node has a tag and never otherwise"
            if layoutCount PropertyName.Anchor <> (if node.Properties.Anchor.IsSome then 1 else 0) then
                report ".PropertyLayout" "the layout names the anchor once when the node has an anchor and never otherwise"

            if (isBlockCollection node || YamlNode.isPropertyOnly node) && not node.PropertyLayout.IsEmpty then
                let placements = List.toArray node.PropertyLayout
                let lastIndex = placements.Length - 1
                if not (followingOf (Some placements.[lastIndex].After)).IsEmpty then
                    report
                        (".PropertyLayout[" + string lastIndex + "].After.Following")
                        (if isBlockCollection node then followingText else missingText)
                if isBlockCollection node && placements.[lastIndex].After = Separation.Inline then
                    report (".PropertyLayout[" + string lastIndex + "].After") startsLaterText

            match node.Kind with
            | YamlNodeKind.Alias _ ->
                if node.Properties.Tag.IsSome || node.Properties.Anchor.IsSome || not node.PropertyLayout.IsEmpty then
                    report ".Properties" "an alias has no properties"
            | YamlNodeKind.Scalar scalar ->
                if scalar.HeaderComment.IsSome && not (isBlockScalar scalar) then
                    report ".HeaderComment" "only a block scalar has a header comment (R8)"
                if node.TrailingComment.IsSome then
                    if isBlockScalar scalar then
                        report ".TrailingComment" "a block scalar takes the comment behind its header as HeaderComment (R8)"
                    elif YamlNode.isMissingValue node then
                        report ".TrailingComment" "a missing value has no trailing comment, the comment belongs to the separation behind the indicator (R1)"
                    elif YamlNode.isPropertyOnly node && not inFlow then
                        report ".TrailingComment" "a node with properties and no content has no trailing comment outside a flow collection, the comment belongs to the After of its last property (R1)"
            | YamlNodeKind.Mapping collection ->
                if collection.Style = CollectionStyle.Block then
                    if node.TrailingComment.IsSome then
                        report ".TrailingComment" "a block mapping has no closing token to hang a trailing comment on (R8)"
                    if collection.OpeningComment.IsSome then
                        report ".OpeningComment" "only a flow collection has an opening comment"
                    if collection.Items.IsEmpty then
                        report "" "an empty collection is a flow collection"
                    if not collection.End.IsEmpty && not isLastNode then
                        report ".End" endText
                let entries = List.toArray collection.Items
                for i in 0 .. entries.Length - 1 do
                    let entry = entries.[i]
                    let at = ".entries[" + string i + "]"
                    checkFollowing report (at + ".Question.Following") entry.Question entry.Key
                    checkFollowing report (at + ".Colon.Following") entry.Colon entry.Value
                    if collection.Style = CollectionStyle.Block && entry.Question.IsNone && entry.Colon = Some Separation.Inline && isBareBlockCollection entry.Value then
                        report (at + ".Colon") colonText
                    if collection.Style = CollectionStyle.Block && entry.Question = Some Separation.Inline then
                        checkCompact report (at + ".Key") entry.Key
                    if collection.Style = CollectionStyle.Block && entry.Question.IsSome && entry.Colon = Some Separation.Inline then
                        checkCompact report (at + ".Value") entry.Value
                    if entry.Colon.IsNone && not entry.BeforeColon.IsEmpty then
                        report (at + ".BeforeColon") "an entry without a colon has no gap before it, the trivia belong to the Before of the next entry or item (R7)"
                    elif collection.Style = CollectionStyle.Block && entry.Question.IsNone && not entry.BeforeColon.IsEmpty then
                        report (at + ".BeforeColon") "the key of an entry without a question mark stands on one line with its colon in a block mapping (R7)"
                let childInFlow = collection.Style = CollectionStyle.Flow
                for i in entries.Length - 1 .. -1 .. 0 do
                    let at = ".entries[" + string i + "]"
                    // The text of the last entry ends with its value, or with its key when no colon follows.
                    let lastEntry = isLastNode && not childInFlow && i = entries.Length - 1
                    stack.Add((entries.[i].Value, NodePath.child path "" (at + ".Value"), childInFlow, lastEntry && entries.[i].Colon.IsSome))
                    stack.Add((entries.[i].Key, NodePath.child path "" (at + ".Key"), childInFlow, lastEntry && entries.[i].Colon.IsNone))
            | YamlNodeKind.Sequence collection ->
                if collection.Style = CollectionStyle.Block then
                    if node.TrailingComment.IsSome then
                        report ".TrailingComment" "a block sequence has no closing token to hang a trailing comment on (R8)"
                    if collection.OpeningComment.IsSome then
                        report ".OpeningComment" "only a flow collection has an opening comment"
                    if collection.Items.IsEmpty then
                        report "" "an empty collection is a flow collection"
                    if not collection.End.IsEmpty && not isLastNode then
                        report ".End" endText
                let items = List.toArray collection.Items
                for i in 0 .. items.Length - 1 do
                    let item = items.[i]
                    let at = ".items[" + string i + "]"
                    if collection.Style = CollectionStyle.Block && item.Dash.IsNone then
                        report (at + ".Dash") "an item of a block sequence has a dash"
                    elif collection.Style = CollectionStyle.Flow && item.Dash.IsSome then
                        report (at + ".Dash") "an item of a flow sequence has no dash"
                    checkFollowing report (at + ".Dash.Following") item.Dash item.Node
                    if collection.Style = CollectionStyle.Block && item.Dash = Some Separation.Inline then
                        checkCompact report (at + ".Node") item.Node
                let childInFlow = collection.Style = CollectionStyle.Flow
                for i in items.Length - 1 .. -1 .. 0 do
                    let lastItem = isLastNode && not childInFlow && i = items.Length - 1
                    stack.Add((items.[i].Node, NodePath.child path "" (".items[" + string i + "].Node"), childInFlow, lastItem))

    let private walkDocument (errors: ResizeArray<string>) (prefix: string) (followedByDocument: bool) (document: YamlDocument) =
        let root = document.Root
        if not document.BeforeRoot.IsEmpty && isBareBlockCollection root then
            errors.Add(prefix + "BeforeRoot: the trivia belong to the Before of the first entry or item of the root collection (R3)")
        // The trivia behind the last node of a document go to the Prelude of the next document when no `...` stands between.
        walkNode errors prefix (not (followedByDocument && document.EndMarker.IsNone)) root
        if not document.AfterRoot.IsEmpty then
            if isBlockCollection root then
                errors.Add(prefix + "AfterRoot: the trivia behind a block collection belong to its End (R3)")
            elif YamlNode.isMissingValue root then
                errors.Add(prefix + "AfterRoot: a document whose root is missing keeps all its trivia in BeforeRoot (R3)")
            elif followedByDocument && document.EndMarker.IsNone then
                errors.Add(prefix + "AfterRoot: the trivia in front of the next document belong to its Prelude when no `...` stands between (R3)")

    /// The problems of a document, in the order of the tree. The list is empty when the document is valid.
    let document (document: YamlDocument) : string list =
        let errors = ResizeArray<string>()
        walkDocument errors "" false document
        List.ofSeq errors

    /// The problems of a stream, in the order of its parts. The list is empty when the stream is valid.
    let stream (stream: YamlStream) : string list =
        let errors = ResizeArray<string>()
        let parts = List.toArray stream.Parts
        let hasDocument =
            parts
            |> Array.exists (fun part ->
                match part with
                | StreamPart.Document _ -> true
                | _ -> false)
        // The first part behind each part that is not trivia, so that a run of trivia parts looks at the part
        // behind the run.
        let following: StreamPart option[] = Array.create parts.Length None
        let mutable next: StreamPart option = None
        for i in parts.Length - 1 .. -1 .. 0 do
            following.[i] <- next
            match parts.[i] with
            | StreamPart.Trivia _ -> ()
            | other -> next <- Some other
        let endsWithMarker (part: StreamPart) =
            match part with
            | StreamPart.Document document -> document.EndMarker.IsSome
            | StreamPart.ExtraEndMarker _ -> true
            | StreamPart.Trivia _ -> false
        // The last part before the current one that is not trivia.
        let mutable previous: StreamPart option = None
        for i in 0 .. parts.Length - 1 do
            let at = "Parts[" + string i + "]"
            match parts.[i] with
            | StreamPart.Document document ->
                let followedByDocument =
                    match following.[i] with
                    | Some (StreamPart.Document _) -> true
                    | _ -> false
                walkDocument errors (at + ".") followedByDocument document
                previous <- Some parts.[i]
            | StreamPart.ExtraEndMarker _ ->
                // Behind a document without a `...` the marker would be the end marker of that document.
                match previous with
                | Some part when not (endsWithMarker part) ->
                    errors.Add(at + ": an extra end marker stands at the start of the stream or behind a `...`, and behind a document without an end marker it is the end marker of that document (R3)")
                | _ -> ()
                previous <- Some parts.[i]
            | StreamPart.Trivia _ ->
                let beforeDocument =
                    match following.[i] with
                    | Some (StreamPart.Document _) -> true
                    | _ -> false
                let behindMarker =
                    match previous with
                    | Some part -> endsWithMarker part
                    | None -> false
                // A `...` that ends no document makes an ExtraEndMarker, and the trivia in front of it belong to no document
                // either, but only at the start of the stream. Behind a document without a `...` the marker would end that
                // document, and behind another marker `behindMarker` has accepted the trivia already.
                let beforeExtraEndMarker =
                    match following.[i] with
                    | Some (StreamPart.ExtraEndMarker _) -> previous.IsNone
                    | _ -> false
                if beforeDocument then
                    errors.Add(at + ": trivia in front of a document belong to that document (R3)")
                elif hasDocument && not behindMarker && not beforeExtraEndMarker then
                    errors.Add(at + ": trivia stand in the stream only behind a `...` or at the start of the stream in front of an extra end marker, and otherwise they belong to a document (R3)")
        List.ofSeq errors
