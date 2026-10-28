namespace YAMLicious

open System.Collections.Generic

/// The options of the alias expansion.
///
/// `MaxCopiedNodes` is the most nodes that the copies of the aliases of one document may hold together. An alias
/// counts the nodes of the node that its anchor names, with the copies inside that node counted too. A document
/// that is built to blow up, such as a chain of anchors that each hold nine aliases of the one before, passes the
/// limit after a few aliases and fails before the expansion builds anything. 0 lets no alias through. A limit below
/// 0 is a mistake of the program and raises an exception.
type AliasOptions = { MaxCopiedNodes: int }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module AliasOptions =

    /// 100000 copied nodes. A hand-written document reuses far fewer nodes than that, and a program that walks every
    /// copy, as a decoder does, has bounded work.
    let defaults: AliasOptions = { MaxCopiedNodes = 100000 }

/// The expansion of the aliases of one tree.
module internal AliasExpansion =

    /// An anchor that is defined at some point of the walk. `Expanded` stays None while the walk is inside the
    /// anchored node, which is how an alias in its own node shows up.
    type Definition(span: SourceSpan option) =
        member _.Span = span
        member val Expanded: YamlNode option = None with get, set
        // A float, because the size of a chain of anchors that each hold nine aliases of the one before passes any int
        // after a few links. A float counts exactly up to 2^53, and every limit of an int stays far below that.
        member val Size = 0.0 with get, set

    [<RequireQualifiedAccess>]
    type Step =
        | Visit of YamlNode
        /// A collection whose nodes below it are done, with those nodes and the definition of its anchor.
        | Finish of YamlNode * YamlNode[] * Definition option

    /// The separation with the comment behind it. A comment forces the line break.
    let private commentBehind (separation: Separation) (comment: YamlComment option) : Separation =
        match separation with
        | Separation.NewLine lineBreak -> Separation.NewLine { lineBreak with Comment = comment }
        | Separation.Inline ->
            match comment with
            | Some _ -> Separation.NewLine { Comment = comment; Following = [] }
            | None -> Separation.Inline

    /// The node with the comment behind its last property. A copy always has the anchor of its node, so it has a
    /// property.
    let private onLastProperty (comment: YamlComment option) (node: YamlNode) : YamlNode =
        match List.rev node.PropertyLayout with
        | last :: before ->
            { node with PropertyLayout = List.rev ({ last with After = commentBehind last.After comment } :: before) }
        | [] -> node

    /// The copy as it stands in the place of the alias. The comment behind the alias goes to the slot where the
    /// parser puts a comment behind the copy (rules R1 and R8), and the comment that stood behind the anchored node
    /// goes away. The slot is the trailing comment of a scalar or a flow collection. A block scalar takes the comment
    /// as its header comment. A block collection, and a node with properties and no content outside a flow collection,
    /// take it behind their last property. A block scalar whose empty lines at the end are part of its value becomes a
    /// double-quoted scalar, because a blank line behind the copy would join those lines (rule R5).
    let private onLineOfAlias (comment: YamlComment option) (inFlow: bool) (copy: YamlNode) : YamlNode =
        match copy.Kind with
        | YamlNodeKind.Scalar scalar ->
            match scalar.Style with
            | ScalarStyle.Block _ when Validate.storedKeepApplies scalar.Style scalar.Value ->
                let quoted = { scalar with Style = ScalarStyle.DoubleQuoted; HeaderComment = None; Layout = None }
                { copy with Kind = YamlNodeKind.Scalar quoted; TrailingComment = comment }
            | ScalarStyle.Block _ -> { copy with Kind = YamlNodeKind.Scalar { scalar with HeaderComment = comment } }
            | _ ->
                if YamlNode.isPropertyOnly copy && not inFlow then
                    onLastProperty comment copy
                else
                    { copy with TrailingComment = comment }
        | YamlNodeKind.Mapping collection when collection.Style = CollectionStyle.Block -> onLastProperty comment copy
        | YamlNodeKind.Sequence collection when collection.Style = CollectionStyle.Block -> onLastProperty comment copy
        | _ -> { copy with TrailingComment = comment }

    /// Starts the definition of the anchor of the node. A later anchor of the same name replaces it.
    let private define (definitions: Dictionary<string, Definition>) (node: YamlNode) : Definition option =
        match node.Properties.Anchor with
        | Some name ->
            let definition = Definition(node.Span)
            definitions.[name] <- definition
            Some definition
        | None -> None

    /// Replaces the aliases below `root` with the copies. The options give the most nodes that the copies may hold.
    let expand (options: AliasOptions) (root: YamlNode) : Result<YamlNode, YamlError> =
        let limit = options.MaxCopiedNodes
        if limit < 0 then
            failwith "The limit of the copied nodes has to be at least 0."
        let definitions = Dictionary<string, Definition>()
        let steps = ResizeArray<Step>()
        // The results of the nodes that the walk has finished and that no parent has taken yet, with whether each
        // one differs from its original and the number of nodes it holds.
        let built = ResizeArray<YamlNode>()
        let changed = ResizeArray<bool>()
        let sizes = ResizeArray<float>()
        let mutable copied = 0.0
        let mutable failure: YamlError option = None
        steps.Add(Step.Visit root)
        while failure.IsNone && steps.Count > 0 do
            let step = steps.[steps.Count - 1]
            steps.RemoveAt(steps.Count - 1)
            match step with
            | Step.Visit node ->
                match node.Kind with
                | YamlNodeKind.Alias name ->
                    if not (definitions.ContainsKey name) then
                        failure <-
                            Some (
                                TreeWalk.errorAt
                                    YamlErrorCode.UndefinedAlias
                                    ("The alias *" + name + " names no anchor that stands earlier in the document.")
                                    node
                                    []
                            )
                    else
                        let definition = definitions.[name]
                        match definition.Expanded with
                        | Some expanded ->
                            copied <- copied + definition.Size
                            if copied > float limit then
                                failure <-
                                    Some (
                                        TreeWalk.errorAt
                                            YamlErrorCode.AliasLimit
                                            ("The copies of the aliases would pass the limit of " + string limit + " nodes at the alias *" + name + ".")
                                            node
                                            [ definition.Span ]
                                    )
                            else
                                built.Add expanded
                                changed.Add true
                                sizes.Add definition.Size
                        | None ->
                            failure <-
                                Some (
                                    TreeWalk.errorAt
                                        YamlErrorCode.RecursiveAlias
                                        ("The alias *" + name + " stands inside the node that its anchor names, so a copy of that node would contain the alias again.")
                                        node
                                        [ definition.Span ]
                                )
                | YamlNodeKind.Scalar _ ->
                    match define definitions node with
                    | Some definition ->
                        definition.Expanded <- Some node
                        definition.Size <- 1.0
                    | None -> ()
                    built.Add node
                    changed.Add false
                    sizes.Add 1.0
                | _ ->
                    let below = TreeWalk.children node
                    steps.Add(Step.Finish(node, below, define definitions node))
                    for i in below.Length - 1 .. -1 .. 0 do
                        steps.Add(Step.Visit below.[i])
            | Step.Finish (node, below, definition) ->
                let start = built.Count - below.Length
                let mutable anyChanged = false
                let mutable size = 1.0
                for i in start .. built.Count - 1 do
                    if changed.[i] then anyChanged <- true
                    size <- size + sizes.[i]
                let rebuilt =
                    if anyChanged then
                        let inFlow = TreeWalk.isFlowCollection node
                        TreeWalk.withChildren node (fun i ->
                            match below.[i].Kind with
                            | YamlNodeKind.Alias _ -> onLineOfAlias below.[i].TrailingComment inFlow built.[start + i]
                            | _ -> built.[start + i])
                    else
                        node
                built.RemoveRange(start, below.Length)
                changed.RemoveRange(start, below.Length)
                sizes.RemoveRange(start, below.Length)
                built.Add rebuilt
                changed.Add anyChanged
                sizes.Add size
                match definition with
                | Some found ->
                    found.Expanded <- Some rebuilt
                    found.Size <- size
                | None -> ()
        match failure with
        | Some error -> Error error
        | None -> Ok built.[0]

/// Replaces the aliases of a tree with copies of the nodes that their anchors name.
///
/// The parser keeps an alias as a node and never resolves it, so every decoder except `Decode.alias` and
/// `Decode.node` rejects one. A caller that wants the data behind the aliases expands them first. The writer prints
/// an expanded tree with its copies and a tree with aliases as it is. A tree without aliases comes back unchanged.
///
/// An alias names the most recent anchor of its name in front of it in the text (YAML 1.2.2, sections 3.2.2.2 and
/// 7.1), so a later anchor of the same name takes over for the aliases behind it. The anchors of a document end with
/// the document, and `expandStream` expands each document on its own.
///
/// The copy is the node of the anchor after the aliases inside that node are replaced. It has the spans of the
/// anchored node and so has every node below it, so a decoder that fails inside a copy points at the text that holds
/// the value. It keeps the tag and the anchor of the node. A copy without the anchor would be a bare block
/// collection or a missing value in some places, and the separation behind the indicator in front of it would have
/// to change with it. With the anchor the copy keeps the layout of its node in every place. The writer writes the
/// anchor on every copy. YAML allows that, because a later anchor replaces the earlier one for the aliases behind
/// it and the expanded tree has no alias left. ruamel warns about a repeated anchor and reads the text.
///
/// The comments inside the anchored node stay in the copy. The comment behind the alias goes to the slot where the
/// parser puts a comment behind the copy, and the comment that stood behind the anchored node stays with that node.
/// That slot is the trailing comment of a scalar or a flow collection. A block scalar takes it as the header
/// comment, and a block collection or a node with properties and no content takes it behind its last property. A
/// block scalar with the chomping Keep and empty lines at the end of its value becomes a double-quoted scalar in the
/// copy, because a blank line behind the copy would join those lines (rule R5).
///
/// A copy shares the nodes below its top node with the anchored node. Nodes are immutable, so the sharing is
/// invisible, and the memory of the result grows with the input. A walk over the result still visits every copy.
/// `AliasOptions.MaxCopiedNodes` limits the nodes that the copies of one document hold. The expansion adds up the
/// sizes of the anchored nodes as it goes and fails at the alias that passes the limit, before it builds that copy.
///
/// A block collection or a block scalar that an alias copies into a flow collection keeps its style in the tree, and
/// the writer writes it in the nearest style that the flow collection allows. A scalar with several lines that an
/// alias copies into an implicit key loses its lines in the written text. The written text reads as the same value
/// and as a tree with another style or layout.
///
/// An alias without an anchor in front of it is the error `undefined-alias`. The parser rejects it, so only a tree
/// that a program built, or a part of a tree, can hold one. An alias inside its own anchored node, as in `&a [*a]`,
/// is the error `recursive-alias`, because the copy would hold the alias again. A document whose copies pass the
/// limit is the error `alias-limit`. The span of an error is the alias, and `Related` holds the span of the anchored
/// node for the last two. A node that a program built has no span, and its error points at the start of the text.
module Aliases =

    /// The node with the aliases below it replaced by copies. The node is the root of a document, or a node whose
    /// anchors all stand inside it.
    let expandNode (options: AliasOptions) (node: YamlNode) : Result<YamlNode, YamlError> =
        AliasExpansion.expand options node

    /// The document with the aliases of its root replaced by copies.
    let expandDocument (options: AliasOptions) (document: YamlDocument) : Result<YamlDocument, YamlError> =
        TreeWalk.mapDocument (expandNode options) document

    /// The stream with the aliases of each document replaced by copies. The limit applies to each document.
    let expandStream (options: AliasOptions) (stream: YamlStream) : Result<YamlStream, YamlError> =
        TreeWalk.mapStream (expandNode options) stream
