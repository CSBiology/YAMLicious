### 2.0.0 (Unreleased)

YAMLicious 2.0.0 replaces the element model of 1.x with a tree of nodes that keeps comments, blank lines, scalar styles, properties and source spans, and it replaces the reader and the writer with a strict YAML 1.2.2 parser and a writer for that tree. The decoders and the value encoders keep their names and their Thoth.Json style and work on the new nodes. The [README](README.md#how-it-reads-and-writes-yaml) describes how the library reads and writes YAML, and the sections below list what changed for callers of 1.x.

#### New API

* `Decode.int64`, `Decode.scalarText`, `Decode.nonNull`, `Decode.uniqueKeys`, `Decode.tryDecode`, `Decode.entries`, `Decode.node` and `Decode.alias`.
* `Encode.int64` and `Encode.nullValue`.
* `Aliases.expandNode`, `Aliases.expandDocument` and `Aliases.expandStream` with `AliasOptions`.
* The `YamlErrorCode` cases `recursive-alias` and `alias-limit`.

#### Behavior changes for callers of 1.x

* A missing value, as in `k:`, is an empty plain scalar. `Decode.scalarText` reads it as "", and `Decode.string` rejects it.
* `Decode.string` and `Decode.char` read strings of the core schema only. 1.x read the text of any scalar, which `Decode.scalarText` does now.
* Comments are no longer elements. The decoders ignore them, and the writer prints them from their slots.
* The writer indents by two spaces by default. `Config.init` of 1.x used four.
* A mapping inside a sequence item starts on the line of its dash. 1.x wrote the dash alone on its line.
* `Decode.object` rejects duplicate keys, and 1.x read the first one. It also rejects a key that is no string, such as a plain `1`, which 1.x read as the field "1".
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
| `Decode.string` on a scalar that is no string, such as `1` or `true` | `Decode.scalarText` |
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
* The modules of the 1.x pipeline: `Persil`, `Preprocessing`, `Syntax`, `Regex`, `RegexActivePatterns`, `Escapes` and `JsInterop`. The `TestCases` literals of `Library` are gone as well.
* The `PyInterop` module with `isString` and `fromCodePoint` is internal. So are `Located<'T>` and the `Parser` module with `Parser.parseStream`, and `Reader.tryReadDocuments` parses a text into a stream.
* `Decode.read`, `Decode.Helper`, `Decode.Interop`, `Decode.ObjectHelper` and `Decode.Getter`.
* `Encode.write`, `Encode.tryInclude`, `Encode.choose`, `Encode.withComment`, `Encode.comment`, `Encode.nil` and `Encode.Helper`.
* `Writer.StyleVerifier`, `Writer.Formatting`, `Writer.detokenize`, `Writer.detokenizeWithOptions` and `Writer.writeWithOptions`. `Writer.write` has a new signature.

#### Fable

* The sources need Fable 5.23.0 or newer. The library is built and tested with Fable 5.24.0 and Fable.Core 5.5.0.
* The NuGet package carries the F# sources in its `fable/` folder. The Python output needs `fable-library` in the range `>= 5.24.0, < 6.0.0`, the release that matches Fable 5.24.0. Versions before 5.22.0 lack names that the output imports, such as `narrow`. The JavaScript output needs `@fable-org/fable-library-js` in the range `^2.10.0`, the version that Fable 5.24.0 ships.
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
