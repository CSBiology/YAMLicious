### 2.0.0 (Unreleased)

YAMLicious 2.0.0 replaces the element model and the reader and writer around it. The new parser reads strict YAML 1.2.2 into a tree of nodes that keeps comments, blank lines, scalar styles, properties and source spans. The decoders and encoders keep their Thoth.Json style and work on the new nodes. The release breaks the 1.x API, and the migration table below maps each old call to its replacement.

#### Model

* `YamlNode` is the one type for every value. Its `Kind` is a scalar, a mapping, a sequence or an alias. A mapping entry holds a key node and a value node, so a key can be a collection or an alias as well as a scalar.
* Every node has `Properties` with an optional tag and an optional anchor, and a `PropertyLayout` that records where each property stood in the text. A tag is the non-specific tag `!` or a named tag with its expanded prefix and suffix.
* A `YamlScalar` keeps the cooked `Value` and the `Style`. The style is one of plain, single-quoted, double-quoted and block. A block scalar keeps the style and the chomping mode of its header and its optional indentation indicator. A missing value, as in `k:`, is a plain scalar with the text "" and a zero-width span.
* Every comment and blank line has one slot in the tree. A node has a `TrailingComment` for the comment behind its last token. A block scalar keeps the comment behind its header in `HeaderComment`, and a flow collection keeps the comment behind its opener in `OpeningComment`. Entries and items have a `Before` list for the trivia in front of them, and collections have an `End` list. A line break behind an indicator holds its comment and the lines that follow. A trivia line is a comment line or a blank line. The rules R1 to R8 in the comments of `Model.fs` decide the slot when the text allows more than one.
* A `YamlDocument` has a `Root`, a `Prelude` for its directives, a `StartMarker` and an `EndMarker`, and the trivia in `BeforeRoot` and `AfterRoot`. A `YamlStream` holds the documents with the trivia and the extra end markers that belong to no document.
* Every parsed node, entry, item, comment, directive and document has a `SourceSpan` with 1-based lines and columns. A column counts Unicode code points, so an emoji is one column on every target. Constructed nodes have no span.
* `YamlNode` has constructors for trees that a program builds, such as `plain`, `scalar`, `mapping`, `sequence`, `flowSequence`, `entry`, `item`, `withTag`, `withAnchor` and the `with` functions for comments. `Validate.document` and `Validate.stream` list the rules that a tree breaks.
* The records that hold nodes have no structural equality, because the generic one recurses through the whole tree and overflows the stack on a deep one. `YamlNode.equalIgnoringSpans` compares two trees and `YamlNode.show` prints one. `YamlDocument` and `YamlStream` have the same two functions.

#### Reading

* `Reader.readRoot` reads a text with one document and returns its root node. `Reader.read` returns the `YamlDocument`. `Reader.readDocuments` returns the `YamlStream` of a text with any number of documents, and `Reader.documents` lists the documents of a stream. `Reader.tryRead` and `Reader.tryReadDocuments` return a `Result` with a `YamlError`. The functions without `try` raise `YamlParseException`.
* `Reader.read` and `Reader.readRoot` reject a text with several documents with the code `multiple-documents`. A text without any document reads as a document with a missing root that keeps the comments of the text.
* The parser follows the productions of YAML 1.2.2 and is strict. Examples of what it rejects are a tab as indentation, a `#` glued to a closed quoted scalar or flow collection, a flow collection that the text never closes and an alias without an anchor.
* A `#` starts a comment at the start of a line or behind white space. Values such as `tool.cwl#main` and `C#` keep the `#`.
* A `YamlError` has a `Code`, a `Message` that names the line, a `Span` for the offending text and `Related` spans that explain it, such as the opener of a flow collection that the text never closes. `YamlErrorCode.name` gives each of the 45 codes a stable text such as `unclosed-quote`.
* The parser keeps its state on the heap, so a collection nested 10000 levels deep parses on every target. The time grows linearly with the length of the input.

#### Differences from yaml@2.6.1

The tests record the result of yaml@2.6.1 above each case. The parser differs from yaml@2.6.1 on purpose in the following places.

* A block scalar with the chomping clip or keep whose last line ends the input has no final line feed ([165]). `k: |` with the body `  a` reads as "a", and yaml@2.6.1 reads "a\n".
* A line of spaces beyond an indentation indicator is content ([167], [171]). `k: |1` with a body line of three spaces reads as two spaces and a line feed, and yaml@2.6.1 reads an empty value.
* Each empty line behind an escaped line break in a double-quoted scalar is a line feed ([112]). With an empty line between `"a\` and `  b"` the value is "a\nb", and yaml@2.6.1 reads "a b".
* A lone carriage return is a line break. yaml@2.6.1 reads it as a character.
* A `?` in front of a flow indicator is an error ([142], [150]). `[?]` and `{?}` fail with `reserved-indicator`, and yaml@2.6.1 reads an empty key with a null value.
* An escape that spells a lone surrogate, such as `"\ud800"`, is the error `invalid-escape` on every target, because a Python string cannot hold it. yaml@2.6.1 reads it.
* A byte order mark is valid at the start of a document and inside a quoted scalar. Anywhere else it is the error `invalid-character`, and yaml@2.6.1 keeps it in the text.
* A verbatim tag and a `%TAG` prefix hold only the URI characters of the specification, and a directive name starts right behind the `%`. `!<{x}> a` and `%TAG !e! {bad}` are errors, and so is `% YAML 1.2` with a space behind the percent sign. yaml@2.6.1 accepts all three.
* An alias without an earlier anchor is the error `undefined-alias` when the parser reads it. yaml@2.6.1 fails when it builds the value.
* A `...` that no document precedes closes nothing and becomes an extra end marker of the stream. Example 9.3 of the specification has two documents here, and yaml@2.6.1 reads three ([205]).
* A line of blanks with a tab behind a block scalar at the root belongs to the prefix of the next document, so the parser reads it. yaml@2.6.1 rejects it with `BAD_INDENT`.
* The parser keeps duplicate keys in the tree, and the object getters and the map and dict decoders reject them. yaml@2.6.1 rejects them while it parses.
* A collection or an alias as a key stays a node in the tree. yaml@2.6.1 turns a collection key into a string such as "[ a, b ]" and resolves an alias key to its value.

#### CWL flow layout

* The closer `]` or `}` of an outermost flow collection may stand at the column of the node that owns the collection, as in `expressionLib: [` with `]` at column 0. YAML 1.2.2 asks for one more space ([69], [80]). The parser accepts the layout on purpose, because ruamel (cwltool) and yaml@2.6.1 read it and real CWL files use it. The items of the collection and the closers of nested collections still have to stand deeper than the owner.

#### Decoding

* A `Decoder<'T>` is a function from `YamlNode` to `'T`. `Decode.object` reads a mapping through getters. `get.Required.Field` and `get.Optional.Field` read one field. `get.MultipleOptional.FieldList` returns the entries with the given names, and `get.Overflow.FieldList` returns the other entries. Both return a `MappingEntry list` in source order. `get.Entries` returns every entry.
* A decoder raises `YamlDecodeException` with a `DecodeError`, and `Decode.tryDecode` returns the error as a result. The error has a `Message` and a `Path`, and its `Span` is the range of the node that failed. `DecodeError.text` gives one line, such as `At inspection_packages[0]: The mapping has no field name. (line 3, column 5)`.
* A path leads from the node that the caller passed to the node that failed. A field adds its name and a sequence item adds `[i]`. An entry of a map or dict adds `{key}`.
* The scalar decoders follow the YAML 1.2 core schema. A plain scalar is null, a bool, an int or a float when it matches the schema, and `yes`, `no`, `on` and `off` are strings. A quoted scalar is a string unless a tag says otherwise, so `"1"` is a string for `Decode.int` and `!!int "1"` is an int. An explicit `!!null` needs a null spelling or an empty text. Ints read decimal digits. `0o` starts octal digits and `0x` starts hexadecimal digits. Floats read `.nan` and the infinities `.inf` and `-.inf`.
* `Decode.int64`, `Decode.tryDecode`, `Decode.entries`, `Decode.node` and `Decode.alias` are new. `Decode.int64` reads the same forms as `Decode.int` in 64 bits.
* `Decode.string` reads the text of any scalar, so `1` reads as "1" and a missing value reads as "". `Decode.option` returns `None` for a null and for a missing value. `get.Optional.Field` returns `None` for a missing field and for a null value, and `get.Entries` shows the difference.
* `Decode.object` rejects a mapping with a repeated field name, and `Decode.map` and `Decode.dict` reject two entries with equal decoded keys. The message names both entries.
* Aliases stay symbolic and no decoder resolves an anchor. Every decoder except `Decode.alias` and `Decode.node` rejects an alias and names it.
* `Decode.datetime` and `Decode.datetimeOffset` read ISO 8601 forms only: a date, or a date and a time with a `T` or a blank between them. The time needs seconds and may have a fraction of one to seven digits and a zone. A date alone is midnight, and `Decode.datetimeOffset` needs the zone. Any other text is an error on every target, also `1`, `2020`, `5` and `12:00`, which the parse of a target would read. The YAML 1.1 timestamp forms are not read.
* The kind of a `DateTime` and the offset of a `DateTimeOffset` depend on the target. .NET keeps the UTC kind of a `Z`, while Fable gives the kind Local on Python and Unspecified on JavaScript. .NET and Python keep the offset of the text, and JavaScript gives the offset of the machine.

#### Encoding

* The value encoders keep their names and build `YamlNode` values. `Encode.string` double-quotes a text that the core schema reads as null, a bool, an int or a float, so `"1"` and `"true"` read back as strings. `Encode.float` writes `.nan`, `.inf`, `-.inf` and `-0.0` in forms that `Decode.float` reads back.
* `Encode.int64` and `Encode.nullValue` are new. `Encode.int64` writes a plain scalar that `Decode.int64` reads back.
* `Encode.optionalField name encoder value` returns `None` for a missing value. Combine the fields with `List.choose id` before they go to `Encode.object`.
* `Encode.map` and `Encode.dict` take a key encoder that returns a node, so a key can be any node. `Encode.values` writes a block sequence of strings.
* A tree that needs comments or properties takes them from the constructors of `YamlNode`.

#### Writing

* `Writer.writeNode` writes a node as the root of a document. `Writer.write` writes a `YamlDocument`, and `Writer.writeDocuments` writes a `YamlStream`. A `WriterOptions` value goes first. `WriterOptions.defaults` indents by two spaces and ends lines with `"\n"`. Its policy is `PreferStored`.
* `StylePolicy` decides what the writer does with the styles in the tree. `PreferStored` keeps a stored style that can express the value and picks the nearest valid style otherwise. `RequireStored` raises `YamlWriteException` where a stored style cannot express the value. `Canonical` ignores the stored styles. It writes a scalar plain where that keeps its meaning under the core schema and double-quoted otherwise, and it writes every non-empty collection as a block collection. No policy drops a comment or a blank line.
* The writer puts every comment and blank line back in its slot. The output of a tree that the parser produced parses to the same tree except for the spans, and writing that tree again gives the same text.
* The writer checks the tree with `Validate` first and raises `YamlWriteException` with the messages of the validator when the tree breaks a rule.
* A constructed plain scalar states a preference. The writer quotes a text that cannot stand plain. Examples are a text that starts with an indicator and a text that holds `: ` or ` #`.
* The writer adds a `---` where the text would otherwise change the document count or lose the directives, and a `...` where a document with directives follows one without it.
* A blank line directly behind a block scalar with the chomping keep belongs to the value of the scalar ([165] to [169]), so `Validate` rejects a tree with a blank trivia line there. A value that needs the chomping keep and has a blank line behind it is written double-quoted under `PreferStored` and `Canonical`, and `RequireStored` raises.

#### Behavior changes for callers of 1.x

* A missing value, as in `k:`, is an empty plain scalar, and `Decode.string` reads it as "".
* Comments are no longer elements. The decoders ignore them, and the writer prints them from their slots.
* The writer indents by two spaces by default. `Config.init` of 1.x used four.
* A mapping inside a sequence item starts on the line of its dash. 1.x wrote the dash alone on its line.
* `Decode.object` rejects duplicate keys, and 1.x read the first one.
* A block scalar whose last line ends the input has no final line feed ([165]). 1.x kept one.
* `ScalarStyle` needs qualified access. Write `ScalarStyle.Plain` where 1.x let you write `Plain`.
* The exceptions have their own types. Decoders raise `YamlDecodeException` where 1.x raised `ArgumentException`, and the reader raises `YamlParseException` where 1.x called `failwith`.
* Input that 1.x read as text is an error. `a: [1, 2` and `a: "abc` read as the text `[1, 2` and `"abc` in 1.x.

#### Migration from 1.x

| 1.x | 2.0.0 |
|---|---|
| `open YAMLicious.YAMLiciousTypes` | `open YAMLicious` |
| `YAMLElement.Value (YAMLContent.create "x")` | `YamlNode.plain "x"`, or `YamlNode.scalar style "x"` for another style |
| `YAMLElement.Mapping (key, value)` | `YamlNode.entry keyNode valueNode`, a `MappingEntry` |
| `YAMLElement.Object [ ... ]` | `YamlNode.mapping [ entries ]` |
| `YAMLElement.Sequence [ ... ]` | `YamlNode.sequence [ YamlNode.item node ]`, or `YamlNode.flowSequence` for `[a, b]` |
| `YAMLElement.Comment text` | No element. A comment is a `YamlComment` in a slot, for example `YamlNode.withTrailingComment (YamlComment.create " text") node` or a `FullLineComment` in the `Before` of an entry |
| `YAMLElement.Alias name` | `YamlNode.alias name` |
| `YAMLElement.Nil` | `YamlNode.empty`, a plain scalar with the text "" |
| `YAMLElement.DocumentStart` and `DocumentEnd` | `StartMarker` and `EndMarker` of `YamlDocument` |
| `Comment`, `Anchor`, `Tag` and `Style` of `YAMLContent` | `TrailingComment` of `YamlNode`, `Properties.Anchor` and `Properties.Tag` (set with `YamlNode.withAnchor` and `YamlNode.withTag`) and `Style` of `YamlScalar` |
| `Decode.read text` | `Reader.readRoot text` |
| `Reader.read text` | `Reader.readRoot text` for the root node, or `Reader.read text` for the `YamlDocument` |
| `Reader.readDocuments text`, a `YAMLElement list` | `Reader.readDocuments text`, a `YamlStream`, and `Reader.documents stream`, a `YamlDocument list` |
| `Decode.string`, `Decode.int` and the other decoders | The same names with the type `YamlNode -> 'T` |
| `Decode.ObjectHelper.IRequiredGetter` and its siblings | `Decode.IRequiredGetter` and its siblings |
| `get.Overflow.FieldList names` and `get.MultipleOptional.FieldList names`, a `Dictionary<string, YAMLElement>` | A `MappingEntry list` in source order |
| `Decode.map keyDecoder valueDecoder` with `keyDecoder: string -> 'K` | The same call with `keyDecoder: Decoder<'K>`, which reads the key node |
| `Encode.map` and `Encode.dict` with `keyEncoder: 'K -> string` | The same calls with `keyEncoder: 'K -> YamlNode` |
| `Encode.tryInclude name encoder value` and `Encode.choose` | `Encode.optionalField name encoder value` and `List.choose id` |
| `Encode.nil` | `YamlNode.empty` |
| `Encode.withComment text element` and `Encode.comment text` | `YamlNode.withTrailingComment`, `YamlNode.withHeaderComment`, `YamlNode.withOpeningComment` and `YamlNode.withPropertyComment` |
| `Encode.write 2 element` | `Writer.writeNode { WriterOptions.defaults with Indent = 2 } node` |
| `Writer.write element (Some (fun c -> { c with Whitespace = 4 }))` | `Writer.writeNode { WriterOptions.defaults with Indent = 4 } node` |
| `Writer.writeWithOptions` with the old `WriterOptions` | `WriterOptions.Styles`, a `StylePolicy`. A node stores its own style |
| `Writer.StyleVerifier.isPlainSafe` | Removed. The writer decides whether a scalar can stand plain |

#### Removed API

* The module `YAMLiciousTypes` with `YAMLElement`, `YAMLContent`, `YAMLDirective`, `Config`, the old `WriterOptions`, `Preprocessor`, `PreprocessorElement`, `QuotedStringKind`, `StringMapEntry` and `YAMLElementNormalization`.
* The modules of the 1.x pipeline: `Persil`, `Preprocessing`, `Syntax`, `Regex`, `RegexActivePatterns`, `Escapes` and `JsInterop`. `PyInterop.isString` and the `TestCases` literals of `Library` are gone as well.
* `Decode.read`, `Decode.Helper`, `Decode.Interop`, `Decode.ObjectHelper` and `Decode.Getter`.
* `Encode.write`, `Encode.tryInclude`, `Encode.choose`, `Encode.withComment`, `Encode.comment`, `Encode.nil` and `Encode.Helper`.
* `Writer.StyleVerifier`, `Writer.Formatting`, `Writer.detokenize`, `Writer.detokenizeWithOptions` and `Writer.writeWithOptions`. `Writer.write` has a new signature.

#### Fable

* The sources need Fable 5.23.0 or newer. The library is built and tested with Fable 5.23.0 and Fable.Core 5.5.0.
* The NuGet package carries the F# sources in its `fable/` folder. The Python output needs `fable-library` in the range `>= 5.22.0, < 6.0.0`, because the output of Fable 5.23.0 imports names that older versions lack, such as `narrow`. The JavaScript output needs `@fable-org/fable-library-js` in the range `^2.9.0`.
* The .NET package depends on FSharp.Core 8.0.403 or newer, the lowest version that the library builds and tests with.

### 1.0.2 (Released 2026-07-23)
* Fix a reader bug where a lone apostrophe inside a plain scalar — a legal, non-delimiting character in plain and double-quoted scalars — caused the double-quote extraction pass to consume the remainder of the document, dropping every subsequent quoted token (notably double-quoted keys). Apostrophes are now treated as ordinary characters in the double-quote pass, and an unclosed quote of either kind no longer swallows the rest of the input.

### 1.0.1 (Released 2026-07-23)
* Automatically select a safe scalar style when writing style-less scalars: single-line values that are not valid unquoted (e.g. containing `#` or `: `, or starting with an indicator such as `*`, `@`, `-`) are now emitted as double-quoted scalars instead of broken plain scalars, mirroring the existing automatic block-scalar selection for multiline values. An explicit `Plain` style is still respected as an opt-out.

### 1.0.0 (Released 2026-06-03)
* Update Fable compiler and runtime dependencies to 5.1.0, including @fable-org/fable-library-js 2.1.0 for JavaScript
* Use System.Text.StringBuilder directly across targets and remove the custom string buffer workaround

### 1.0.0-alpha.11 (Released 2026-05-07)
* Preserve block structure across lower-indented full-line comments in open mappings and sequences
* Decode multiline plain scalar expressions as single scalar values

### 1.0.0-alpha.10 (Released 2026-05-07)
* Enable blank line handling

### 1.0.0-alpha.9 (Released 2026-05-06)
* Fix parsing for mapping keys followed by full-line comments and block sequence values.

### 1.0.0-alpha.8 (Released 2026-03-20)
* Restore quoted mapping keys after placeholder preprocessing so keys like `"@id"` and `"@type"` parse correctly instead of leaking internal `<s f=.../>` markers
* Preserve quoted-key scalar style in mapping and explicit-key reader paths, with regression coverage for quoted mapping keys

### 1.0.0-alpha.7 (Released 2026-03-18)
* Parse multiline plain scalars in mappings, sequences, and root values according to the YAML spec instead of treating them as multiple sibling values
* Re-emit multiline plain scalars in compact YAML form and keep decoder semantics aligned with YAML line folding

### 1.0.0-alpha.6 (Released 2026-03-13)
* Preprocessing now treats blank lines immediately before a more-indented block as part of that child block instead of siblings at the parent level

### 1.0.0-alpha.5 (Released 2026-02-27)
* Pin FSharp.Core to 10.0.101 to prevent mismatch with deleted FSharp.Core version

### 1.0.0-alpha.4 (Released 2026-02-24)
* Add support for YAML directives and multi-document markers in reader and writer flows
* Add block scalar and scalar-style parsing/writing with improved roundtrip preservation
* Improve escape and scalar parsing robustness, including UTF-32 handling for Python target
* Expand regression and edge-case test coverage, and replace StringBuilder with a Fable-friendly custom buffer

### 1.0.0-alpha.3 (Released 2026-01-26)
* Add support for nested flow-style YAML syntax (JSON-like with braces and brackets)
* Implement FlowStyleParser module with recursive parser for deeply nested structures
* Enhance InlineJSON, JSONKeyOpener, and InlineSequence handlers to support complex nested objects and arrays

### 1.0.0-alpha.2 (Released 2025-12-29)
* Merge 0.0.4 and 1.0.0-alpha.1

### 1.0.0-alpha.1 (Released 2025-12-18)
* Update to fable 5 and update the depending packages
    * This requires the installation of .NET 10 SDK
* Add fable-libary as python dependency
* Fix parsing of float for fable 5 in JavaScript/TypeScript (e.g. CultureInfo)

### 0.0.4 (Released 2025-12-15)
* Fix handling of multiline block literals with embedded comments/strings to prevent crashes
* Add tests for block scalar parsing and placeholder restoration

### 0.0.3 (Released 2024-10-14)
    * Add decoder for multiple optional fields with dictionaries

### 0.0.2 (Released 2024-7-11)
    * Add overflow decoding logic

### 0.0.1-alpha (Released 2024-6-20)
    * First open alpha release of YAMLicious. Rough feature set:
    * Tokenize YAML files
    * Encode & Decode YAML tokens
    * Write YAML tokens
    * Support untyped extension Data via Dictionary
