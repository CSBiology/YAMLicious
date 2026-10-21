namespace YAMLicious

/// Constructors, accessors and test helpers for nodes.
///
/// A constructed node has no span, no comment, no property and an empty `PropertyLayout`. The `with`
/// functions add one piece of presentation each and reject a combination that the invariants of `YamlNode`
/// or the slot rules forbid. `withTag` and `withAnchor` keep the layout complete, so a node with properties
/// always has one placement for each of them.
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

    // ns-anchor-char of the YAML 1.2 specification: a printable character other than a white space, a line
    // break, the byte order mark and the flow indicators. A .NET or JavaScript string holds a character
    // beyond the BMP as a pair of surrogates, which stand outside these ranges and are checked apart. A
    // Python string holds such a character as one code point, which the last range covers.
    let private isAnchorChar (c: char) =
        let code = int c
        let printable =
            (code >= 0x21 && code <= 0x7E)
            || code = 0x85
            || (code >= 0xA0 && code <= 0xD7FF)
            || (code >= 0xE000 && code <= 0xFFFD && code <> 0xFEFF)
            || (code >= 0x10000 && code <= 0x10FFFF)
        printable && c <> ',' && c <> '[' && c <> ']' && c <> '{' && c <> '}'

    let private isHighSurrogate (c: char) = int c >= 0xD800 && int c <= 0xDBFF
    let private isLowSurrogate (c: char) = int c >= 0xDC00 && int c <= 0xDFFF

    // `what` is "alias" or "anchor" and names the owner of the name in the message.
    let private checkName (what: string) (name: string) =
        if name.Length = 0 then
            failwith ("The " + what + " name must not be empty.")
        let mutable valid = true
        let mutable i = 0
        while valid && i < name.Length do
            if isHighSurrogate name.[i] && i + 1 < name.Length && isLowSurrogate name.[i + 1] then
                i <- i + 2
            elif isAnchorChar name.[i] then
                i <- i + 1
            else
                valid <- false
        if not valid then
            failwith ("The " + what + " name " + name + " holds a character that a name cannot hold.")

    /// An alias of the anchor `name`. The name has no `*` in front. It must not be empty and must not hold a
    /// white space, a line break, a flow indicator or a control character.
    let alias (name: string) : YamlNode =
        checkName "alias" name
        create (YamlNodeKind.Alias name)

    /// A mapping with the entries in their order. An empty mapping is the flow mapping `{}`, and any other
    /// mapping is a block mapping.
    let mapping (entries: MappingEntry list) : YamlNode =
        let style = if entries.IsEmpty then CollectionStyle.Flow else CollectionStyle.Block
        create (YamlNodeKind.Mapping (collection style entries))

    /// A flow mapping, as in `{a: 1}`.
    let flowMapping (entries: MappingEntry list) : YamlNode =
        create (YamlNodeKind.Mapping (collection CollectionStyle.Flow entries))

    /// A sequence with the items in their order. An empty sequence is the flow sequence `[]`, and any other
    /// sequence is a block sequence. A block item has a dash, so an item without one gets an inline dash.
    let sequence (items: SequenceItem list) : YamlNode =
        let style = if items.IsEmpty then CollectionStyle.Flow else CollectionStyle.Block
        let withDash (item: SequenceItem) : SequenceItem =
            if item.Dash.IsNone then { item with Dash = Some Separation.Inline } else item
        create (YamlNodeKind.Sequence (collection style (List.map withDash items)))

    /// A flow sequence, as in `[1, 2]`. A flow item has no dash, so the dash of each item is dropped.
    let flowSequence (items: SequenceItem list) : YamlNode =
        let withoutDash (item: SequenceItem) : SequenceItem = { item with Dash = None }
        create (YamlNodeKind.Sequence (collection CollectionStyle.Flow (List.map withoutDash items)))

    // A block collection with items has no token on the line of its indicator or its last property, so its
    // first entry or item starts on a later line.
    let private isBlockCollectionWithItems (node: YamlNode) =
        match node.Kind with
        | YamlNodeKind.Mapping content -> content.Style = CollectionStyle.Block && not content.Items.IsEmpty
        | YamlNodeKind.Sequence content -> content.Style = CollectionStyle.Block && not content.Items.IsEmpty
        | _ -> false

    // A line break without a comment and without trivia.
    let private plainLineBreak: Separation = Separation.NewLine { Comment = None; Following = [] }

    /// An entry without an explicit `?` and without a comment. The colon is inline, except behind a key whose
    /// value is a block collection with items and without properties: that collection starts on a later line,
    /// so the colon is followed by a line break. A block collection with properties starts behind its last
    /// property, which `withTag` and `withAnchor` follow with a line break.
    let entry (key: YamlNode) (value: YamlNode) : MappingEntry =
        let startsOnLaterLine =
            isBlockCollectionWithItems value && value.Properties.Tag.IsNone && value.Properties.Anchor.IsNone
        { Before = []
          Question = None
          Key = key
          BeforeColon = []
          Colon = Some (if startsOnLaterLine then plainLineBreak else Separation.Inline)
          Value = value
          Span = None }

    /// An item with an inline dash and no comment.
    let item (node: YamlNode) : SequenceItem =
        { Before = []
          Dash = Some Separation.Inline
          Node = node
          Span = None }

    let private describe (node: YamlNode) =
        match node.Kind with
        | YamlNodeKind.Scalar _ -> "a scalar"
        | YamlNodeKind.Mapping _ -> "a mapping"
        | YamlNodeKind.Sequence _ -> "a sequence"
        | YamlNodeKind.Alias _ -> "an alias"

    // The layout names each property of the node once. The canonical order puts the tag first and the anchor
    // last, so a new tag goes to the front and a new anchor to the end. The properties of a block collection
    // with items stand in front of its first entry or item, which starts on a later line, so the last
    // placement has a line break behind it and the others have a blank.
    let private addPlacement (property: PropertyName) (node: YamlNode) : PropertyPlacement list =
        let onLaterLine = isBlockCollectionWithItems node
        let layout = node.PropertyLayout
        match property with
        | PropertyName.Tag ->
            let added: PropertyPlacement =
                { Property = PropertyName.Tag
                  After = (if onLaterLine && layout.IsEmpty then plainLineBreak else Separation.Inline)
                  Span = None }
            added :: layout
        | PropertyName.Anchor ->
            let added: PropertyPlacement =
                { Property = PropertyName.Anchor
                  After = (if onLaterLine then plainLineBreak else Separation.Inline)
                  Span = None }
            // The placement that was the last one gives up its plain line break, which the new one takes over.
            let relaxed (placement: PropertyPlacement) : PropertyPlacement =
                match placement.After with
                | Separation.NewLine { Comment = None; Following = [] } when onLaterLine -> { placement with After = Separation.Inline }
                | _ -> placement
            List.map relaxed layout @ [ added ]

    /// The node with an explicit tag and its placement in front of the content. An alias takes no tag, and
    /// a node that has a tag already takes no second one. On a block collection with items the placement of the
    /// last property has a line break behind it.
    let withTag (tag: YamlTag) (node: YamlNode) : YamlNode =
        match node.Kind with
        | YamlNodeKind.Alias name -> failwith ("The alias *" + name + " cannot have a tag.")
        | _ when node.Properties.Tag.IsSome -> failwith "A node cannot have two tags."
        | _ ->
            { node with
                Properties = { node.Properties with Tag = Some tag }
                PropertyLayout = addPlacement PropertyName.Tag node }

    /// The node with an anchor and its placement in front of the content. An alias takes no anchor, a node
    /// that has an anchor already takes no second one, and the name follows the rules of the name of an alias.
    /// On a block collection with items the placement of the last property has a line break behind it.
    let withAnchor (name: string) (node: YamlNode) : YamlNode =
        match node.Kind with
        | YamlNodeKind.Alias target -> failwith ("The alias *" + target + " cannot have an anchor.")
        | _ when node.Properties.Anchor.IsSome -> failwith "A node cannot have two anchors."
        | _ ->
            checkName "anchor" name
            { node with
                Properties = { node.Properties with Anchor = Some name }
                PropertyLayout = addPlacement PropertyName.Anchor node }

    let private isBlockScalar (content: YamlScalar) =
        match content.Style with
        | ScalarStyle.Block _ -> true
        | _ -> false

    let private isEmptyPlain (node: YamlNode) =
        match node.Kind with
        | YamlNodeKind.Scalar content -> content.Value.Length = 0 && content.Style = ScalarStyle.Plain
        | _ -> false

    /// True when the node is a missing value: an empty plain scalar without properties, as in `k:` or in a
    /// lone `-`. A scalar with a property is not missing, because the property is a token on the line.
    let isMissingValue (node: YamlNode) =
        isEmptyPlain node && node.Properties.Tag.IsNone && node.Properties.Anchor.IsNone

    /// True when the node has properties and no content, as in `k: !!str`. Its last token is its last
    /// property, so outside a flow collection a comment on the same line stands behind that property (rule
    /// R1). Inside a flow collection the comment stands behind the comma and is the trailing comment of the
    /// node (rule R6).
    let isPropertyOnly (node: YamlNode) =
        isEmptyPlain node && (node.Properties.Tag.IsSome || node.Properties.Anchor.IsSome)

    /// The node with the comment behind its last line, as in `k: value # c` (rule R8). A block collection and
    /// a block scalar end in a line that cannot hold a comment of the node, so they reject it. A missing value
    /// rejects it too, because the comment belongs to the separation behind the indicator (rule R1). A node
    /// that has properties and no content accepts it, because the constructor cannot see whether the node
    /// stands in a flow collection, where the comment behind the comma is its trailing comment (rule R6).
    /// `Validate` rejects the comment on such a node outside a flow collection. A comment replaces the earlier
    /// one.
    let withTrailingComment (comment: YamlComment) (node: YamlNode) : YamlNode =
        match node.Kind with
        | YamlNodeKind.Mapping content when content.Style = CollectionStyle.Block ->
            failwith "A block mapping has no closing token for a trailing comment. Put the comment on an entry or the end of the collection."
        | YamlNodeKind.Sequence content when content.Style = CollectionStyle.Block ->
            failwith "A block sequence has no closing token for a trailing comment. Put the comment on an item or the end of the collection."
        | YamlNodeKind.Scalar content when isBlockScalar content ->
            failwith "A block scalar has no place for a trailing comment. Use its header comment."
        | _ when isMissingValue node ->
            failwith "A missing value has no trailing comment. Put the comment on the separation behind the indicator."
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
    /// as in `&a # c` on a line of its own (rule R1). The node must have the property. A comment replaces the
    /// earlier one.
    let withPropertyComment (property: PropertyName) (comment: YamlComment) (node: YamlNode) : YamlNode =
        if not (node.PropertyLayout |> List.exists (fun placement -> placement.Property = property)) then
            failwith "The node does not have the property that the comment belongs to."
        else
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
            { node with PropertyLayout = List.map commented node.PropertyLayout }

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

/// Test helpers for streams.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module YamlStream =

    /// True when both streams hold the same parts in the same order, equal except for their source spans.
    let equalIgnoringSpans (left: YamlStream) (right: YamlStream) : bool =
        NodeComparison.streamsEqualIgnoringSpans left right

    /// A readable printout of the stream, without spans.
    let show (stream: YamlStream) : string = NodeComparison.showStream stream
