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
///   when a block collection without a property follows or when the content is missing (R2).
/// - An entry: the gap before the colon is empty when no colon follows and for an implicit key in a block
///   mapping (R7).
/// - A document: `BeforeRoot` is empty in front of a block collection without a property and `AfterRoot`
///   is empty behind a block collection, behind a missing root and in front of a document that follows
///   without a `...` between (R3).
/// - A stream: `StreamPart.Trivia` does not stand in front of a document, and in a stream with a document
///   it stands behind a `...` only (R3).
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

    /// Reports the trivia behind an indicator when the content behind it is a block collection without a
    /// property or is missing.
    let private checkFollowing (report: string -> string -> unit) (slot: string) (separation: Separation option) (content: YamlNode) =
        if not (followingOf separation).IsEmpty then
            if isBareBlockCollection content then
                report slot followingText
            elif YamlNode.isMissingValue content then
                report slot missingText

    let private walkNode (errors: ResizeArray<string>) (prefix: string) (root: YamlNode) =
        // The flag tells whether the node is an item or a key or value of a flow collection.
        let stack = ResizeArray<YamlNode * NodePath.PathInfo * bool>()
        stack.Add((root, NodePath.rootPath, false))
        while stack.Count > 0 do
            let last = stack.Count - 1
            let (node, path, inFlow) = stack.[last]
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
                let entries = List.toArray collection.Items
                for i in 0 .. entries.Length - 1 do
                    let entry = entries.[i]
                    let at = ".entries[" + string i + "]"
                    checkFollowing report (at + ".Question.Following") entry.Question entry.Key
                    checkFollowing report (at + ".Colon.Following") entry.Colon entry.Value
                    if entry.Colon.IsNone && not entry.BeforeColon.IsEmpty then
                        report (at + ".BeforeColon") "an entry without a colon has no gap before it, the trivia belong to the Before of the next entry or item (R7)"
                    elif collection.Style = CollectionStyle.Block && entry.Question.IsNone && not entry.BeforeColon.IsEmpty then
                        report (at + ".BeforeColon") "the key of an entry without a question mark stands on one line with its colon in a block mapping (R7)"
                let childInFlow = collection.Style = CollectionStyle.Flow
                for i in entries.Length - 1 .. -1 .. 0 do
                    let at = ".entries[" + string i + "]"
                    stack.Add((entries.[i].Value, NodePath.child path "" (at + ".Value"), childInFlow))
                    stack.Add((entries.[i].Key, NodePath.child path "" (at + ".Key"), childInFlow))
            | YamlNodeKind.Sequence collection ->
                if collection.Style = CollectionStyle.Block then
                    if node.TrailingComment.IsSome then
                        report ".TrailingComment" "a block sequence has no closing token to hang a trailing comment on (R8)"
                    if collection.OpeningComment.IsSome then
                        report ".OpeningComment" "only a flow collection has an opening comment"
                    if collection.Items.IsEmpty then
                        report "" "an empty collection is a flow collection"
                let items = List.toArray collection.Items
                for i in 0 .. items.Length - 1 do
                    let item = items.[i]
                    let at = ".items[" + string i + "]"
                    if collection.Style = CollectionStyle.Block && item.Dash.IsNone then
                        report (at + ".Dash") "an item of a block sequence has a dash"
                    elif collection.Style = CollectionStyle.Flow && item.Dash.IsSome then
                        report (at + ".Dash") "an item of a flow sequence has no dash"
                    checkFollowing report (at + ".Dash.Following") item.Dash item.Node
                let childInFlow = collection.Style = CollectionStyle.Flow
                for i in items.Length - 1 .. -1 .. 0 do
                    stack.Add((items.[i].Node, NodePath.child path "" (".items[" + string i + "].Node"), childInFlow))

    let private walkDocument (errors: ResizeArray<string>) (prefix: string) (followedByDocument: bool) (document: YamlDocument) =
        let root = document.Root
        if not document.BeforeRoot.IsEmpty && isBareBlockCollection root then
            errors.Add(prefix + "BeforeRoot: the trivia belong to the Before of the first entry or item of the root collection (R3)")
        walkNode errors prefix root
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
            | StreamPart.ExtraEndMarker _ -> previous <- Some parts.[i]
            | StreamPart.Trivia _ ->
                let beforeDocument =
                    match following.[i] with
                    | Some (StreamPart.Document _) -> true
                    | _ -> false
                let behindMarker =
                    match previous with
                    | Some part -> endsWithMarker part
                    | None -> false
                if beforeDocument then
                    errors.Add(at + ": trivia in front of a document belong to that document (R3)")
                elif hasDocument && not behindMarker then
                    errors.Add(at + ": trivia stand in the stream only behind a `...`, and otherwise they belong to a document (R3)")
        List.ofSeq errors
