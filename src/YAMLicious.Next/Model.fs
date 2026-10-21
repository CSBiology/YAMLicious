namespace YAMLicious

// The element model of the next YAMLicious. It describes the serialization tree of a YAML stream: values,
// properties, scalar and collection styles, the place of every comment, directives and document boundaries.
// It keeps none of the original spacing, quote escapes, line wrapping of scalars or indentation of
// collections.
//
// Two pairs of spellings read the same, and the writer picks one form of each. A flow pair such as
// `[a: b]` and the one-entry flow mapping `[{a: b}]` give the same tree. So do an indentless block
// sequence (`k:` followed by `- a` at the column of the key) and an indented one.
//
// Several invariants cannot be stated in the types. Each type names the ones that concern it. The
// constructors in YamlNode, the parser and the writer enforce them, and `Validate` checks every one that
// the tree alone shows. Eight slot rules, R1 to R8, say which field holds a comment or a blank line when
// the text allows more than one. R1 is on `Separation`, R2 on `LineBreak`, R3 on `YamlDocument` and
// `YamlStream`, R4 and R6 on `Collection`, R5 on `ScalarStyle`, R7 on `MappingEntry` and R8 on `YamlNode`.
//
// Records that hold nodes carry NoEquality and NoComparison, because the generic structural equality,
// the hash and the %A printer recurse through the whole tree and overflow the stack on a deep one. Use
// YamlNode.equalIgnoringSpans, YamlDocument.equalIgnoringSpans, YamlStream.equalIgnoringSpans and the show
// functions of the three modules instead.

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

/// What stands on a line of its own between two nodes. The rules R2 to R4 and R6 say which field holds a
/// run of trivia.
type Trivia =
    /// A line that holds only a comment.
    | FullLineComment of YamlComment
    /// A line that holds nothing. The span covers the line without its line break and is None on a
    /// constructed line. A blank line inside a scalar is part of the value and never trivia.
    | BlankLine of SourceSpan option

/// A line break behind an indicator, with the comment that stands before the break and the trivia lines
/// that follow it.
///
/// Rule R2 (trivia behind the line break of an indicator). `Following` holds the trivia lines between the
/// line of the indicator and the first token of the node, which is a property, a scalar, an alias or a
/// flow opener. A block collection without a property has no token of its own, so these trivia go to the
/// `Before` of its first entry or item and `Following` stays empty. The same holds behind the last
/// property of a block collection. When the content is missing, `Following` stays empty as well, and the
/// trivia lines go to the `Before` of the next entry or item under rule R4. Behind the `?` of an entry whose
/// key is missing they go to its `BeforeColon`. The same holds behind the last property of a node that has
/// no content.
type LineBreak =
    { Comment: YamlComment option
      Following: Trivia list }

/// How the next token follows the previous one.
///
/// Rule R1 (comment behind an indicator without content). A comment on the same line behind `:`, `-`, `?`
/// or a property goes to the separation of that indicator when the content of the node does not start on
/// that line. The separation is `NewLine` and its `Comment` holds the comment. A comment behind content on
/// the same line is the trailing comment of that node (rule R8). A missing value, which is an empty plain
/// scalar without properties, never has a trailing comment. A node that has properties and no content, as
/// in `k: !!str # c`, puts the comment on the `After` of its last property, and it never has a trailing
/// comment either.
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
///
/// A plain scalar that the parser produced can stand plain in its context. A plain scalar that a program
/// constructed (it has no span) states a preference, and the writer quotes a text that cannot stand plain,
/// for example one that starts with an indicator or holds `: ` or ` #`.
///
/// Rule R5 (blank lines behind a block scalar). A blank line behind the body of a block scalar with the
/// chomping Clip or Strip is trivia under rule R4. With the chomping Keep it is content of the scalar and
/// part of its value.
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

/// An explicit tag. The non-specific tag `!` is always `NonSpecific`. A `Named` tag never has an empty
/// suffix: the parser does not produce `Named { Prefix = "!"; Suffix = "" }`, and `YamlTag.local` and
/// `YamlTag.core` reject an empty suffix. Two tags are the same tag exactly when their identifiers are equal.
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
      /// Rules R1 and R2 apply to it as they apply to the separation behind an indicator.
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

/// The shared part of a mapping and a sequence. An empty collection is always a flow collection.
///
/// Rule R4 (trivia in a block collection). A run of trivia between two nodes belongs to the `Before` of the
/// next entry or item that follows it, at whatever level that entry or item stands and whatever the columns
/// of the comments are. A run of trivia behind the last node of a document is split by column when the
/// document ends at the end of the stream or in front of a `...`. In front of a `---` or a directive the run
/// goes to the `Prelude` of the next document (rule R3). Each split comment goes to the `End` of the
/// deepest open block collection whose entries stand at a column less than or equal to the column of the
/// comment, and to the `End` of the root collection when no deeper collection qualifies. A blank line goes
/// with the comment that follows it. A blank line behind the last comment goes to the same `End` as that
/// comment, or to the `End` of the root collection when no comment follows. A block collection has no
/// closing token, so `End` is the only place for the trivia behind it.
///
/// Rule R6 (flow collection). A comment behind the opener on its line is the `OpeningComment`. A comment on
/// the same line behind an item or an entry and its comma is the trailing comment of the node of the item,
/// of the value of the entry, or of the key of an entry whose value is missing. Trivia lines between items
/// go to the `Before` of the next item or entry. Trivia lines in front of the closer go to `End`. A comment
/// behind the closer is the trailing comment of the collection node.
[<NoEquality; NoComparison>]
type Collection<'Item> =
    { Style: CollectionStyle
      /// The comment behind `[` or `{`. It is None for a block collection.
      OpeningComment: YamlComment option
      Items: 'Item list
      /// The trivia behind the last item, in front of the closer of a flow collection or at the end of a
      /// block collection (rules R4 and R6).
      End: Trivia list }

/// A node of the tree.
///
/// Invariants that the type does not enforce:
///
/// - A node has at most one tag and one anchor. An alias has neither. An alias names an anchor that stands
///   earlier in its document, and an anchor name can be used again. The parser registers the anchor of a
///   collection before it reads the children, so a collection may contain an alias of itself.
/// - `PropertyLayout` names each present property exactly once, in the order of the text, and nothing
///   else. A node without properties has an empty layout. `YamlNode.withTag` and `YamlNode.withAnchor` add
///   the placement of a constructed node, the tag first, each with an inline separation.
/// - Each comment occupies exactly one field. The rules R1 to R8 pick that field when the text allows
///   more than one. A comment behind the completed value of an entry belongs to that value, and no second
///   field of the entry competes for it.
/// - A block scalar header comment is a block scalar field and an opening comment is a flow collection
///   field. No other node uses them.
/// - A parsed node has a span. A constructed node may have none. A missing value is a real node with a
///   zero-width span.
///
/// Rule R8 (trailing comment). A comment on the same line behind the last token of a node is the
/// `TrailingComment` of that node. The last token is the text of a scalar or an alias, or the closer of a
/// flow collection. A block scalar takes the comment behind its header as `HeaderComment` and never has a
/// `TrailingComment`. A block collection has no closing token and has none either. A missing value and a
/// node that has properties and no content have none (rule R1). A comment behind a block collection goes to
/// the `End` of the collection or to the `Before` of the next entry or item.
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
        /// The trivia lines in front of the entry. A comment in the gap in front of an entry belongs to it
        /// (rule R4).
        Before: Trivia list
        /// Some when an explicit `?` stands in front of the key. The separation is the one behind the `?`.
        Question: Separation option
        Key: YamlNode
        /// Rule R7 (gap before a colon). The trivia between a key and its `:` go here, for an explicit key
        /// and for an implicit key in a flow mapping, as in `{ a`, a comment line and `: b }`. An implicit
        /// key in a block mapping stands on one line with its colon, so the gap is empty there.
        BeforeColon: Trivia list
        /// The separation behind the `:`. None means that the value indicator is omitted, as it is behind
        /// an explicit key without a value and in a flow mapping entry that is a key alone.
        Colon: Separation option
        Value: YamlNode
        Span: SourceSpan option
    }

/// An item of a sequence.
and [<NoEquality; NoComparison>] SequenceItem =
    {
        /// The trivia lines in front of the item (rules R4 and R6).
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
///
/// Rule R3 (trivia around the root). Trivia in front of a `---` or a directive always go to the `Prelude` of
/// the document that the `---` or the directive opens, as `PreludeTrivia` in the order of the text. That
/// holds also for trivia that stand behind the root of the document before when no `...` stands between.
/// Trivia between directives go to the same `Prelude`. Trivia behind the `---`, or at the top of a document
/// without `---`, and in front of the root go to `BeforeRoot`. When the root is a block collection without a
/// property, they go to the `Before` of its first entry or item instead, and `BeforeRoot` stays empty. A
/// document whose root is missing keeps all its trivia in `BeforeRoot`. `AfterRoot` holds trivia behind the
/// root only when the document ends at the end of the stream or in front of a `...`, and only when the root
/// is not a block collection, because the trivia behind a block collection go to its `End` (rule R4).
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
    /// Trivia that belong to no document (rule R3 on `YamlStream`).
    | Trivia of Trivia
    /// A `...` that closes no document of its own, as the extra one between the two documents of the
    /// example 9.3 of the YAML specification. The stream keeps it without inventing a document.
    | ExtraEndMarker of DocumentMarker

/// A stream of documents. It may hold no document.
///
/// Rule R3 (trivia that belong to no document). `StreamPart.Trivia` holds trivia that stand behind a `...`
/// and in front of no document. That includes the trivia between a `...` and an `ExtraEndMarker`. A `...`
/// is the end marker of a document part or an `ExtraEndMarker` part. When the stream has no document,
/// `StreamPart.Trivia` holds the whole input. Trivia behind a `...` and in front of a following document go
/// to that document, to its `Prelude` when it has directives or a `---` and otherwise by the rule on
/// `YamlDocument`.
///
/// yaml@2.6.1 keeps the trivia in front of a following `---` in the document before. The model gives them
/// to the next document. Both write the same text when the stream is written again.
[<NoEquality; NoComparison>]
type YamlStream =
    { Parts: StreamPart list }
