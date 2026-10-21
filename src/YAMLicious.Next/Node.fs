namespace YAMLicious

/// Constructors, accessors and test helpers for nodes.
///
/// A constructed node has no span, no comment, no property and an empty `PropertyLayout`, which asks the
/// writer for the canonical order of the properties, the tag first. The `with` functions add one piece of
/// presentation each and reject a combination that the invariants of `YamlNode` forbid.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module YamlNode =

    let private create (kind: YamlNodeKind) : YamlNode =
        { Kind = kind
          Properties =
            { Tag = None
              Anchor = None }
          PropertyLayout = []
          TrailingComment = None
          Span = None }

    let private collection (style: CollectionStyle) (items: 'Item list) : Collection<'Item> =
        { Style = style
          OpeningComment = None
          Items = items
          End = [] }

    /// A scalar with the cooked text `value` in the given style.
    let scalar (style: ScalarStyle) (value: string) : YamlNode =
        let content: YamlScalar =
            { Value = value
              Style = style
              HeaderComment = None }
        create (YamlNodeKind.Scalar content)

    /// A plain scalar. The text is not checked, so the writer must quote a text that cannot stand plain.
    let plain (value: string) : YamlNode = scalar ScalarStyle.Plain value

    /// A scalar with omitted content, as in `k:` or in a lone `-`. It is a plain scalar with the text "".
    let empty: YamlNode = plain ""

    /// An alias of the anchor `name`. The name has no `*` in front.
    let alias (name: string) : YamlNode = create (YamlNodeKind.Alias name)

    /// A mapping with the entries in their order. An empty mapping is the flow mapping `{}`, and any other
    /// mapping is a block mapping.
    let mapping (entries: MappingEntry list) : YamlNode =
        let style = if entries.IsEmpty then CollectionStyle.Flow else CollectionStyle.Block
        create (YamlNodeKind.Mapping (collection style entries))

    /// A flow mapping, as in `{a: 1}`.
    let flowMapping (entries: MappingEntry list) : YamlNode =
        create (YamlNodeKind.Mapping (collection CollectionStyle.Flow entries))

    /// A sequence with the items in their order. An empty sequence is the flow sequence `[]`, and any other
    /// sequence is a block sequence.
    let sequence (items: SequenceItem list) : YamlNode =
        let style = if items.IsEmpty then CollectionStyle.Flow else CollectionStyle.Block
        create (YamlNodeKind.Sequence (collection style items))

    /// A flow sequence, as in `[1, 2]`. A flow item has no dash, so the dash of each item is dropped.
    let flowSequence (items: SequenceItem list) : YamlNode =
        let withoutDash (item: SequenceItem) : SequenceItem = { item with Dash = None }
        create (YamlNodeKind.Sequence (collection CollectionStyle.Flow (List.map withoutDash items)))

    /// An entry with an inline colon, no explicit `?` and no comment.
    let entry (key: YamlNode) (value: YamlNode) : MappingEntry =
        { Before = []
          Question = None
          Key = key
          BeforeColon = []
          Colon = Some Separation.Inline
          Value = value
          Span = None }

    /// An item with an inline dash and no comment.
    let item (node: YamlNode) : SequenceItem =
        { Before = []
          Dash = Some Separation.Inline
          Node = node
          Span = None }

    let private canonicalLayout (properties: NodeProperties) : PropertyPlacement list =
        let placement (property: PropertyName) : PropertyPlacement =
            { Property = property
              After = Separation.Inline
              Span = None }
        [ if properties.Tag.IsSome then placement PropertyName.Tag
          if properties.Anchor.IsSome then placement PropertyName.Anchor ]

    let private describe (node: YamlNode) =
        match node.Kind with
        | YamlNodeKind.Scalar _ -> "a scalar"
        | YamlNodeKind.Mapping _ -> "a mapping"
        | YamlNodeKind.Sequence _ -> "a sequence"
        | YamlNodeKind.Alias _ -> "an alias"

    // A layout that already exists must name the new property too. The tag goes first and the anchor last,
    // which is the canonical order.
    let private addPlacement (property: PropertyName) (node: YamlNode) : PropertyPlacement list =
        if node.PropertyLayout.IsEmpty then
            []
        else
            let added: PropertyPlacement =
                { Property = property
                  After = Separation.Inline
                  Span = None }
            match property with
            | PropertyName.Tag -> added :: node.PropertyLayout
            | PropertyName.Anchor -> node.PropertyLayout @ [ added ]

    /// The node with an explicit tag. An alias takes no tag, and a node that has a tag already takes no second one.
    let withTag (tag: YamlTag) (node: YamlNode) : YamlNode =
        match node.Kind with
        | YamlNodeKind.Alias name -> failwith ("The alias *" + name + " cannot have a tag.")
        | _ when node.Properties.Tag.IsSome -> failwith "A node cannot have two tags."
        | _ ->
            { node with
                Properties = { node.Properties with Tag = Some tag }
                PropertyLayout = addPlacement PropertyName.Tag node }

    /// The node with an anchor. An alias takes no anchor, and a node that has an anchor already takes no second one.
    let withAnchor (name: string) (node: YamlNode) : YamlNode =
        match node.Kind with
        | YamlNodeKind.Alias target -> failwith ("The alias *" + target + " cannot have an anchor.")
        | _ when node.Properties.Anchor.IsSome -> failwith "A node cannot have two anchors."
        | _ ->
            { node with
                Properties = { node.Properties with Anchor = Some name }
                PropertyLayout = addPlacement PropertyName.Anchor node }

    let private isBlockScalar (content: YamlScalar) =
        match content.Style with
        | ScalarStyle.Block _ -> true
        | _ -> false

    /// The node with the comment behind its last line, as in `k: value # c`. A block collection and a block
    /// scalar end in a line that cannot hold a comment of the node, so they reject it. A comment replaces the earlier one.
    let withTrailingComment (comment: YamlComment) (node: YamlNode) : YamlNode =
        match node.Kind with
        | YamlNodeKind.Mapping content when content.Style = CollectionStyle.Block ->
            failwith "A block mapping has no closing token for a trailing comment. Put the comment on an entry or the end of the collection."
        | YamlNodeKind.Sequence content when content.Style = CollectionStyle.Block ->
            failwith "A block sequence has no closing token for a trailing comment. Put the comment on an item or the end of the collection."
        | YamlNodeKind.Scalar content when isBlockScalar content ->
            failwith "A block scalar has no place for a trailing comment. Use its header comment."
        | _ -> { node with TrailingComment = Some comment }

    /// The node with the comment behind the header of a block scalar, as in `| # c`. Only a block scalar
    /// has a header. A comment replaces the earlier one.
    let withHeaderComment (comment: YamlComment) (node: YamlNode) : YamlNode =
        match node.Kind with
        | YamlNodeKind.Scalar content when isBlockScalar content ->
            { node with Kind = YamlNodeKind.Scalar { content with HeaderComment = Some comment } }
        | _ -> failwith ("Only a block scalar has a header comment, not " + describe node + ".")

    /// The node with the comment behind the opener of a flow collection, as in `[ # c`. Only a flow
    /// collection has an opener. A comment replaces the earlier one.
    let withOpeningComment (comment: YamlComment) (node: YamlNode) : YamlNode =
        match node.Kind with
        | YamlNodeKind.Mapping content when content.Style = CollectionStyle.Flow ->
            { node with Kind = YamlNodeKind.Mapping { content with OpeningComment = Some comment } }
        | YamlNodeKind.Sequence content when content.Style = CollectionStyle.Flow ->
            { node with Kind = YamlNodeKind.Sequence { content with OpeningComment = Some comment } }
        | _ -> failwith ("Only a flow collection has an opening comment, not " + describe node + ".")

    /// The node with the comment behind one of its properties, which puts the next token on a later line,
    /// as in `&a # c` on a line of its own. The node must have the property. A comment replaces the earlier one.
    let withPropertyComment (property: PropertyName) (comment: YamlComment) (node: YamlNode) : YamlNode =
        let present =
            match property with
            | PropertyName.Tag -> node.Properties.Tag.IsSome
            | PropertyName.Anchor -> node.Properties.Anchor.IsSome
        if not present then
            failwith "The node does not have the property that the comment belongs to."
        else
            let layout =
                if node.PropertyLayout.IsEmpty then canonicalLayout node.Properties else node.PropertyLayout
            let commented (placement: PropertyPlacement) : PropertyPlacement =
                if placement.Property <> property then
                    placement
                else
                    let following =
                        match placement.After with
                        | Separation.NewLine lineBreak -> lineBreak.Following
                        | Separation.Inline -> []
                    { placement with
                        After =
                            Separation.NewLine
                                { Comment = Some comment
                                  Following = following } }
            { node with PropertyLayout = List.map commented layout }

    /// The scalar of a scalar node.
    let tryScalar (node: YamlNode) : YamlScalar option =
        match node.Kind with
        | YamlNodeKind.Scalar content -> Some content
        | _ -> None

    /// The collection of a mapping node. It holds the style, the opening comment, the entries and the end trivia.
    let tryMapping (node: YamlNode) : Collection<MappingEntry> option =
        match node.Kind with
        | YamlNodeKind.Mapping content -> Some content
        | _ -> None

    /// The collection of a sequence node. It holds the style, the opening comment, the items and the end trivia.
    let trySequence (node: YamlNode) : Collection<SequenceItem> option =
        match node.Kind with
        | YamlNodeKind.Sequence content -> Some content
        | _ -> None

    /// The entries of a mapping node in their order. Any other node is an error.
    let entries (node: YamlNode) : MappingEntry list =
        match node.Kind with
        | YamlNodeKind.Mapping content -> content.Items
        | _ -> failwith ("The node is " + describe node + ", not a mapping.")

    /// The items of a sequence node in their order. Any other node is an error.
    let items (node: YamlNode) : SequenceItem list =
        match node.Kind with
        | YamlNodeKind.Sequence content -> content.Items
        | _ -> failwith ("The node is " + describe node + ", not a sequence.")

    /// The explicit tag and anchor of the node.
    let properties (node: YamlNode) : NodeProperties = node.Properties

    /// True when both trees are equal except for their source spans. The walk keeps its own stack, so a
    /// tree of any depth is safe. Comments, trivia, styles, properties and the layout of the properties
    /// all count.
    let equalIgnoringSpans (left: YamlNode) (right: YamlNode) : bool =
        NodeComparison.nodesEqualIgnoringSpans left right

    /// A readable printout of the tree, one line for each node, entry and item, without spans. It is meant
    /// for the messages of failing tests. A line deeper than 24 levels names its depth in brackets.
    let show (node: YamlNode) : string = NodeComparison.showNode node

/// Constructors and test helpers for documents.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module YamlDocument =

    /// A document that holds only the root. It has no directive, no marker, no comment and no span.
    let create (root: YamlNode) : YamlDocument =
        { Prelude = []
          StartMarker = None
          BeforeRoot = []
          Root = root
          AfterRoot = []
          EndMarker = None
          Span = None }

    /// True when both documents are equal except for their source spans.
    let equalIgnoringSpans (left: YamlDocument) (right: YamlDocument) : bool =
        NodeComparison.documentsEqualIgnoringSpans left right

    /// A readable printout of the document, without spans.
    let show (document: YamlDocument) : string = NodeComparison.showDocument document
