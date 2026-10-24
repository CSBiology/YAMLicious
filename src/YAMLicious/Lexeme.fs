namespace YAMLicious

/// The grammar context in which the parser asks for the next lexeme. The contexts follow the YAML 1.2.2
/// specification, with the nuances that the lexer needs.
///
/// - `BlockIn` and `BlockOut` stand for a position in the block structure. A block indicator is an
///   indicator there, a block scalar header opens a scalar, and a plain scalar may go on in the lines below.
/// - `FlowOut` is a flow node inside the block structure. The lexer treats it like the block contexts and
///   leaves it to the parser to reject a block indicator in it.
/// - `FlowIn` is a position inside a flow collection. A flow indicator ends a plain scalar, a dash is no
///   block entry, and the lines of the collection stand at any indentation. See `LexContext`.
/// - `BlockKey` and `FlowKey` mark the key of an implicit entry. A plain scalar stays on its line there,
///   and `FlowKey` also ends it at a flow indicator.
///
/// The parser passes `BlockIn` for every position of the block structure and `FlowIn` for every position
/// inside a flow collection. It cannot know that a node is the implicit key of an entry before the colon
/// follows, so it checks the line and the length of the key afterwards. `BlockOut`, `FlowOut`, `BlockKey`
/// and `FlowKey` are for callers that know the production, such as the tests of the lexer.
type internal GrammarContext =
    | BlockIn
    | BlockOut
    | FlowIn
    | FlowOut
    | BlockKey
    | FlowKey

/// What the parser tells the lexer about the position of the next lexeme.
///
/// `ParentIndent` is the indentation of the block node that owns the position: the column, counted from 0,
/// of the key of a value, of the dash of a sequence item, of the `?` or `:` of an explicit entry, or of the
/// node that owns a flow collection. A top-level node has -1. A continuation line of a plain scalar in the
/// block structure has to stand deeper than it, and the content of a block scalar is counted from it.
///
/// The indentation of a line is no error inside a flow collection and inside a quoted scalar. [69]
/// s-flow-line-prefix(n) asks for the lines of a flow node to stand deeper than the owner, and [80] s-separate
/// and [81] s-separate-lines ask for that prefix with n one above the owner. The lexer deviates on purpose.
/// A flow collection runs to its closer and a quoted scalar runs to its closing quote, so the indentation of
/// their lines cannot change where they end or what the parser reads around them. That covers the items, the
/// keys, the values and the closers of a collection, the lines of a plain scalar in it and the lines of a
/// quoted scalar everywhere. ruamel reads these layouts and yaml@2.6.1 rejects them, except for the closer of
/// the outermost flow collection at the column of the owner. The layout of a scalar records its folds and no
/// indentation, so the writer indents every line.
///
/// Two limits stay. A document marker at the start of a line is an error inside a flow collection and inside
/// a quoted scalar, as it is in ruamel. A tab is no indentation ([63] s-indent(n) counts spaces only, and [66]
/// s-separate-in-line makes a tab behind them a separator), so a plain scalar ends in front of an empty line
/// that holds a tab behind too few spaces ([70] l-empty). In a flow collection it ends in front of a line with
/// text that starts with a tab behind too few spaces as well, because ruamel does not continue the scalar
/// there. A plain scalar in the block structure keeps the rule of [69], because a line at the column of its
/// owner starts the next entry.
///
/// The key of a flow mapping entry without `?` keeps the rule of [69] when it goes over several lines or its
/// colon stands on a later line, as in `{ matches` followed by `% : 20 }` (Example 9.4). The productions
/// [143] to [147] allow that layout, with the lines deeper than the owner. ruamel rejects it at every
/// indentation, so no layout of it lies in the tolerance. The parser checks the lines when it reaches the colon.
///
/// `SimpleKey` is true when the parser knows that the node can only be an implicit key. The lexer keeps a
/// plain scalar on its first line then, rejects a quoted scalar with a line break, and rejects a scalar of
/// more than 1024 characters. A parser that cannot know yet passes false and checks the span after it has
/// seen the value indicator.
type internal LexContext =
    { Grammar: GrammarContext
      ParentIndent: int
      SimpleKey: bool }

/// A tag as the text spells it. The parser resolves a handle with the `%TAG` bindings of the document.
[<RequireQualifiedAccess>]
type internal TagToken =
    /// `!<uri>`. The text is the part between the angle brackets.
    | Verbatim of uri: string
    /// A handle and a suffix. The handle is `!`, `!!` or a named handle such as `!e!`, and the suffix is the
    /// text behind it, as in `str` for `!!str`. The suffix of the non-specific tag `!` is empty. The
    /// suffix keeps its `%` escapes.
    | Shorthand of handle: string * suffix: string

/// A lexical terminal. The span of the lexeme comes with it in `Located`.
///
/// Spans. The span of a lexeme covers its text. The span of a `LineStart` covers the blanks in front of the
/// first lexeme of the line. The span of a `BlankLine` covers its blanks. A line break belongs to no lexeme.
///
/// The lexer returns the trivia in the order of the text. A `Comment` or a `BlankLine` between two lexemes
/// is one lexeme of its own, and the parser decides which slot of the tree takes it.
///
/// First on a line. A lexeme is the first one on its line when a `LineStart` comes right before it. Inside
/// a flow context no `LineStart` is returned, and the parser compares the lines of the spans instead.
[<RequireQualifiedAccess>]
type internal Lexeme =
    /// The start of the text. Its span is empty and stands behind the byte order mark.
    | StreamStart
    /// The end of the text. The lexer returns it again for every later call.
    | StreamEnd
    /// A `---` at the start of a line.
    | DocumentStart
    /// A `...` at the start of a line.
    | DocumentEnd
    /// A run of byte order marks at the start of a block line. [202] l-document-prefix allows one in front of
    /// a document, and [211] l-yaml-stream lets one follow any document, so the lexer cannot tell whether
    /// the mark is valid. The parser applies the stream rule. Marks take no column, so the span is the one
    /// column that the run would fill at the start of the line, which gives an error a range to point at. A
    /// quoted scalar takes a mark as content ([107] nb-double-char, [118] nb-single-char). Every other mark is
    /// invalid and the lexer reports it, because [27] nb-char and [34] ns-char exclude the mark from the other
    /// scalars, from comments, names and directives, and a flow collection or the middle of a line has no
    /// place for it. The tree has no slot for a mark, so the parser drops it and the writer never writes one.
    | ByteOrderMark
    /// A directive line without its trailing comment.
    | Directive of DirectiveKind
    /// The first lexeme of a content line is about to follow. `indent` is the number of spaces in front of it,
    /// which is the indentation of YAML. `tabbed` is true when a tab stands among the blanks in front of it.
    | LineStart of indent: int * tabbed: bool
    /// A `-` that a blank or the end of the line follows.
    | BlockEntry
    /// A `?` that a blank or the end of the line follows. In a flow collection [142] and [150] ask for
    /// s-separate behind it, so a `?` in front of a flow indicator is an error.
    | ExplicitKey
    /// A `:` that a blank or the end of the line follows. Inside a flow collection a flow indicator may
    /// follow it as well, and so may any character when the colon follows a quoted scalar or a flow
    /// collection, as in `"a":b` and `"a" :b` ([149] c-ns-flow-map-adjacent-value).
    | Value
    | FlowSequenceStart
    | FlowSequenceEnd
    | FlowMappingStart
    | FlowMappingEnd
    | FlowEntry
    /// `&anchor`. The text is the anchor name without the `&`.
    | Anchor of anchorName: string
    /// `*alias`. The text is the alias name without the `*`.
    | Alias of aliasName: string
    | Tag of TagToken
    /// A scalar with its cooked value and style. A plain scalar may span lines. Its value is folded, and its
    /// span runs from its first to its last character. A block scalar carries the comment behind its
    /// header. Its span ends with its last content line. The empty lines behind a body with the chomping
    /// Keep belong to the value, so the lexer has consumed them. Behind a body with the chomping Strip or
    /// Clip they come as `BlankLine` lexemes (rule R5).
    | Scalar of YamlScalar
    /// A comment. `ownLine` is true when the comment is the first lexeme of its line.
    | Comment of comment: YamlComment * ownLine: bool
    /// A line that holds nothing but blanks.
    | BlankLine
