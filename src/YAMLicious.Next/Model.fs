namespace YAMLicious

// The element model of the next YAMLicious. It describes the serialization tree of a YAML stream: values,
// properties, scalar and collection styles, the place of every comment, directives and document boundaries.
// It keeps no original spacing, no quote escapes and no line wrapping of scalars.
//
// Several invariants cannot be stated in the types. Each type names the ones that concern it. The
// constructors in YamlNode, the parser and the writer enforce them.
//
// Records that hold nodes carry NoEquality and NoComparison, because the generic structural equality,
// the hash and the %A printer recurse through the whole tree and overflow the stack on a deep one. Use
// YamlNode.equalIgnoringSpans, YamlDocument.equalIgnoringSpans and YamlNode.show instead.

/// A position in the input text.
///
/// `Line` counts physical lines from 1. A line ends at LF, CRLF or a lone CR.
///
/// `Column` counts Unicode code points from 1 within the line. An astral character such as an emoji is one
/// column on every target. A .NET string and a JavaScript string index by UTF-16 code unit, so the same
/// text has more indexes than columns there. A Python string indexes by code point. Only the code point
/// count is the same on .NET, JavaScript and Python, so the lexer counts a surrogate pair as one column on
/// .NET and JavaScript. A tab is one column, and a byte order mark at the start is not counted.
type SourceMark =
    { Line: int
      Column: int }

/// A range of the input text. `Finish` is exclusive, so an empty range has the same mark twice. A missing
/// value has such a zero-width range at the place where it would stand.
type SourceSpan =
    { Start: SourceMark
      Finish: SourceMark }

/// A comment. `Text` is everything after the `#`, without the line break. `Span` is None on a comment that
/// a program constructed.
type YamlComment =
    { Text: string
      Span: SourceSpan option }

/// What stands on a line of its own between two nodes.
type Trivia =
    /// A line that holds only a comment.
    | FullLineComment of YamlComment
    /// A line that holds nothing.
    | BlankLine

/// A line break behind an indicator, with the comment that stands before the break and the trivia lines
/// that follow it.
type LineBreak =
    { Comment: YamlComment option
      Following: Trivia list }

/// How the next token follows the previous one.
[<RequireQualifiedAccess>]
type Separation =
    /// On the same line.
    | Inline
    /// On a later line.
    | NewLine of LineBreak

type BlockScalarStyle =
    | Literal
    | Folded

type ChompingMode =
    | Strip
    | Clip
    | Keep

/// The spelling of a scalar. `Block` holds the style, the chomping and the explicit indentation indicator
/// of the header. The value of a block scalar is the cooked text, so the indicator is a presentation hint
/// that the writer may recompute.
[<RequireQualifiedAccess>]
type ScalarStyle =
    | Plain
    | SingleQuoted
    | DoubleQuoted
    | Block of BlockScalarStyle * ChompingMode * int option

[<RequireQualifiedAccess>]
type CollectionStyle =
    | Block
    | Flow

/// The expanded name of a tag, split in two parts. The prefix is the string that a `%TAG` directive binds to
/// a handle, and the suffix is the text behind the handle. Nodes can share the prefix string, so the
/// parser does not concatenate a long prefix for every tag. The identifier of the tag is the prefix and the
/// suffix joined.
type TagName =
    { Prefix: string
      Suffix: string }

/// An explicit tag.
[<RequireQualifiedAccess>]
type YamlTag =
    /// The non-specific tag `!`.
    | NonSpecific
    /// A tag with its expanded name, such as `tag:yaml.org,2002:str` for `!!str`.
    | Named of TagName

/// The explicit tag and anchor of a node. A node has at most one of each, and an alias has neither. A node
/// without a tag has no explicit tag. It does not have the tag that its schema would resolve.
type NodeProperties =
    { Tag: YamlTag option
      Anchor: string option }

[<RequireQualifiedAccess>]
type PropertyName =
    | Tag
    | Anchor

/// Where one property of a node stands in the text. The record names the property and does not copy its
/// value, which lives in `NodeProperties`.
type PropertyPlacement =
    { Property: PropertyName
      /// What separates this property from the next token, which is the next property or the content.
      After: Separation
      Span: SourceSpan option }

/// The cooked value of a scalar and its spelling.
///
/// `Value` is the text after the escapes, the line folding and the chomping, so nothing downstream folds
/// it again. An empty scalar has the value "". A missing value, as in `k:` or in an empty sequence item, is
/// a plain scalar with the value "" and a zero-width span. The spellings null and ~ stay plain scalars with
/// that text, and the core schema decides what they mean.
type YamlScalar =
    { Value: string
      Style: ScalarStyle
      /// The comment behind the header of a block scalar, as in `| # c`. It is None for every other style.
      HeaderComment: YamlComment option }

/// The shared part of a mapping and a sequence.
///
/// A block collection has no closing token, so its `End` trivia hold the comments and blank lines that
/// follow its last item at the depth of the collection. A flow collection has `[` or `{` and a closer. Its
/// `OpeningComment` is the comment behind the opener, and `End` holds the trivia in front of the closer. An
/// empty collection is always a flow collection.
[<NoEquality; NoComparison>]
type Collection<'Item> =
    { Style: CollectionStyle
      /// The comment behind `[` or `{`. It is None for a block collection.
      OpeningComment: YamlComment option
      Items: 'Item list
      /// The trivia behind the last item, in front of the closer or the end of the block.
      End: Trivia list }

/// A node of the tree.
///
/// Invariants that the type does not enforce:
///
/// - A node has at most one tag and one anchor. An alias has neither. An alias names an anchor that stands
///   earlier in its document, and an anchor name can be used again. The parser registers the anchor of a
///   collection before it reads the children, so a collection may contain an alias of itself.
/// - `PropertyLayout` of a parsed node names each present property exactly once, in the order of the
///   text. The layout of a constructed node is empty and asks for the canonical order, tag first.
/// - Each comment occupies exactly one field. A comment behind the completed value of an entry belongs to
///   that value, and no second field of the entry competes for it.
/// - A block collection has no closing token to hang a trailing comment on. Its comments go to an entry
///   header, a property line or the `End` trivia of the collection instead.
/// - A block scalar header comment is a block scalar field and an opening comment is a flow collection
///   field. No other node uses them.
/// - A parsed node has a span. A constructed node may have none. A missing value is a real node with a
///   zero-width span.
[<NoEquality; NoComparison>]
type YamlNode =
    { Kind: YamlNodeKind
      Properties: NodeProperties
      PropertyLayout: PropertyPlacement list
      /// The comment behind the node on its last line, as in `k: value # c`, `? key # c` or `] # c`.
      TrailingComment: YamlComment option
      Span: SourceSpan option }

and [<RequireQualifiedAccess; NoEquality; NoComparison>] YamlNodeKind =
    | Scalar of YamlScalar
    | Mapping of Collection<MappingEntry>
    | Sequence of Collection<SequenceItem>
    /// A reference to an anchor. The tree keeps it symbolic and never resolves it.
    | Alias of string

/// An entry of a mapping. The key is a full node, so it can be a scalar, a collection or an alias, and it
/// can carry properties and comments.
///
/// Duplicate keys stay in the list in their order. Neither construction nor parsing removes or merges
/// them. A decoder that builds a map rejects the duplicates.
and [<NoEquality; NoComparison>] MappingEntry =
    {
        /// The trivia lines in front of the entry. A comment in the gap in front of an entry belongs to it.
        Before: Trivia list
        /// Some when an explicit `?` stands in front of the key. The separation is the one behind the `?`.
        Question: Separation option
        Key: YamlNode
        /// The trivia between an explicit key and its `:`.
        BeforeColon: Trivia list
        /// The separation behind the `:`. None means that the value indicator is omitted, as it is behind
        /// an explicit key without a value.
        Colon: Separation option
        Value: YamlNode
        Span: SourceSpan option
    }

/// An item of a sequence.
and [<NoEquality; NoComparison>] SequenceItem =
    {
        /// The trivia lines in front of the item.
        Before: Trivia list
        /// The separation behind the dash. Some for a block item, None for a flow item.
        Dash: Separation option
        Node: YamlNode
        Span: SourceSpan option
    }

type DirectiveKind =
    | YamlVersion of major: int * minor: int
    | TagDirective of handle: string * prefix: string
    /// A directive with a name that YAML 1.2 does not define. The label is not `name`, because a field with
    /// that name clashes with a field of the union base class in the Python output of Fable.
    | ReservedDirective of directiveName: string * arguments: string list

type YamlDirective =
    { Kind: DirectiveKind
      TrailingComment: YamlComment option
      Span: SourceSpan option }

/// What stands in front of a document marker. The order of the directives and the comments between them
/// is kept.
type PreludeItem =
    | Directive of YamlDirective
    | PreludeTrivia of Trivia

/// A `---` or a `...`.
type DocumentMarker =
    { TrailingComment: YamlComment option
      Span: SourceSpan option }

/// A document.
///
/// A `%TAG` binding and the anchors of a document hold for that document only. A marker-like line in the
/// content of a scalar splits no document. A document without a node has a root that is an empty plain
/// scalar with a zero-width span.
[<NoEquality; NoComparison>]
type YamlDocument =
    { Prelude: PreludeItem list
      StartMarker: DocumentMarker option
      BeforeRoot: Trivia list
      Root: YamlNode
      AfterRoot: Trivia list
      EndMarker: DocumentMarker option
      Span: SourceSpan option }

/// A piece of a stream.
[<RequireQualifiedAccess; NoEquality; NoComparison>]
type StreamPart =
    | Document of YamlDocument
    | Trivia of Trivia
    /// A `...` that closes no document of its own, as the extra one between the two documents of the
    /// example 9.3 of the YAML specification. The stream keeps it without inventing a document.
    | ExtraEndMarker of DocumentMarker

/// A stream of documents. It may hold no document.
[<NoEquality; NoComparison>]
type YamlStream =
    { Parts: StreamPart list }
