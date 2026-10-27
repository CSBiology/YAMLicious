# YAMLicious 

![NuGet Version](https://img.shields.io/nuget/v/YAMLicious?style=for-the-badge) ![NuGet Downloads](https://img.shields.io/nuget/dt/YAMLicious?style=for-the-badge)

YAMLicious is a YAML 1.2.2 reader and writer for F# with decoders and encoders in the style of [Thoth.Json](https://github.com/thoth-org/Thoth.Json). It runs on .NET, and [Fable](https://fable.io) compiles it to JavaScript and Python.

The reader builds a tree of nodes that keeps comments, blank lines, scalar and collection styles, tags, anchors and source spans. The writer prints the tree again with every comment and blank line in place.

> [!IMPORTANT]
> If you want to use YAMLicious in Python, please install [`fable-library`](https://pypi.org/project/fable-library/#history) via python dependency manager of your choice. YAMLicious depends on fable-library for core functionality.

## Installation

```
dotnet add package YAMLicious
```

The package carries the F# sources in its `fable/` folder, so a Fable project compiles them for JavaScript or Python.

## Usage

### Read a text

`Reader.readRoot` parses a text with one document and returns the root node. `Reader.read` returns the whole `YamlDocument`, and `Reader.readDocuments` returns a `YamlStream` for a text with any number of documents. `Reader.documents` lists the documents of a stream. The `tryRead` and `tryReadDocuments` variants return a `Result` with a `YamlError`, and the functions without `try` raise `YamlParseException`.

```fsharp
open YAMLicious

type Package = { Name: string; Version: string option }

type DocValidation =
    { FormatDefinitionVersion: string
      Packages: Package list }

let yaml =
    """# validation packages of a spec
arc_specification: 2.0.0-draft
validation_packages:
  - name: package1
    version: 1.0.0
  - name: package2 # needs a review
    version: 2.0.0
  - name: package3
"""

let root: YamlNode = Reader.readRoot yaml
```

### Decode

A `Decoder<'T>` is a function from `YamlNode` to `'T`. `Decode.object` reads a mapping through getters. `Decode.list` and `Decode.option` wrap another decoder.

```fsharp
let packageDecoder: Decoder<Package> =
    Decode.object (fun get ->
        { Name = get.Required.Field "name" Decode.string
          Version = get.Optional.Field "version" Decode.string })

let docValidationDecoder: Decoder<DocValidation> =
    Decode.object (fun get ->
        { FormatDefinitionVersion = get.Required.Field "arc_specification" Decode.string
          Packages = get.Required.Field "validation_packages" (Decode.list packageDecoder) })

let docValidation: DocValidation = docValidationDecoder root

printfn "%A" (docValidation.Packages |> List.map (fun package -> package.Name, package.Version))
```

```
[("package1", Some "1.0.0"); ("package2", Some "2.0.0"); ("package3", None)]
```

The scalar decoders follow the YAML 1.2 core schema. A plain `1` is an int and a quoted `"1"` is a string. `yes` is a string too. `Decode.string` reads a string of the core schema, so it rejects a plain `1`, `true` or `null` and a missing value, and `Decode.scalarText` reads the text of any scalar. `CoreSchema.resolveKind` gives a plain scalar text its core schema kind whatever its size, and `CoreSchema.integerToFloat` converts an integer text of any size and radix to the nearest float.

The keys of a mapping are YAML values. `Decode.object` rejects two keys that are the same value in any spelling, such as `1` and `0x1` or `1.0` and `10e-1`, and so do `Decode.map` and `Decode.dict`. `Decode.object` reads string keys only. `Decode.entries` returns every entry as written, repeated keys included. `Decode.uniqueKeys decoder` checks every mapping of a node, also the ones that the decoder skips, before the decoder runs.

### Errors with positions

Every `YamlError` has a stable `Code` and the `Span` of the offending text, and its message names the line. Its `Related` spans explain the error, such as the opener of a flow collection that the text never closes. `YamlErrorCode.name` gives each of the 45 codes a stable text such as `unclosed-quote`. A decoder raises `YamlDecodeException` with a `DecodeError` that names the path to the failing node and its span. `Decode.tryDecode` returns the error as a value.

```fsharp
match Reader.tryRead "name: \"package1" with
| Ok _ -> ()
| Error error ->
    printfn "%s at %d:%d" (YamlErrorCode.name error.Code) error.Span.Start.Line error.Span.Start.Column

let withoutName = Reader.readRoot "arc_specification: 2.0.0-draft\nvalidation_packages:\n  - version: 1.0.0\n"

match Decode.tryDecode docValidationDecoder withoutName with
| Ok _ -> ()
| Error error -> printfn "%s" (DecodeError.text error)
```

```
unclosed-quote at 1:7
At validation_packages[0]: The mapping has no field name. (line 3, column 5)
```

### Aliases

The reader keeps an alias as a node, and no decoder resolves it. `Aliases.expandNode` replaces each alias with a copy of the node that its anchor names, and `Aliases.expandDocument` and `Aliases.expandStream` do the same for a document and for each document of a stream. The functions return a `Result` with a `YamlError`. An alias inside its own node is the error `recursive-alias`. Copies that pass `AliasOptions.MaxCopiedNodes` (100000 nodes by default) are the error `alias-limit`.

```fsharp
let text =
    """defaults: &defaults
  retries: 3
  timeout: 10
job: *defaults
"""

let jobDecoder: Decoder<int * int> =
    Decode.object (fun get -> get.Required.Field "retries" Decode.int, get.Required.Field "timeout" Decode.int)

let job =
    Reader.readRoot text
    |> Aliases.expandNode AliasOptions.defaults
    |> Result.map (Decode.object (fun get -> get.Required.Field "job" jobDecoder))

printfn "%A" job
```

```
Ok (3, 10)
```

### Encode and write

The encoders build nodes. `Encode.optionalField` returns `None` for a missing value, so combine the fields with `List.choose id`. `Writer.writeNode` turns a node into text.

```fsharp
let packageEncoder (package: Package) : YamlNode =
    [ Some("name", Encode.string package.Name)
      Encode.optionalField "version" Encode.string package.Version ]
    |> List.choose id
    |> Encode.object

let docValidationEncoder (doc: DocValidation) : YamlNode =
    Encode.object
        [ "arc_specification", Encode.string doc.FormatDefinitionVersion
          "validation_packages", Encode.list packageEncoder doc.Packages ]

let text: string = Writer.writeNode WriterOptions.defaults (docValidationEncoder docValidation)

printf "%s" text
```

```yaml
arc_specification: 2.0.0-draft
validation_packages:
  - name: package1
    version: 1.0.0
  - name: package2
    version: 2.0.0
  - name: package3
```

`WriterOptions.Indent` is the number of spaces for one level and is 2 by default. `NewLine` ends every line and is `"\n"` by default, and `Styles` is the style policy that the next section describes. `Writer.write` writes a `YamlDocument` and `Writer.writeDocuments` writes a `YamlStream`.

### Comments survive a round trip

A comment sits in one slot of the tree, so a document that you read and write again keeps its comments and blank lines.

```fsharp
let document: YamlDocument = Reader.read yaml

printfn "%b" (Writer.write WriterOptions.defaults document = yaml)
```

```
true
```

A scalar over several lines keeps its lines as well, because the tree holds the line breaks that folding turned into the text.

```fsharp
let wrapped = "description: >\n  Reads the input table and writes\n  one summary for each group.\n"

printfn "%b" (Writer.write WriterOptions.defaults (Reader.read wrapped) = wrapped)
```

```
true
```

A program that changes the text of such a scalar calls `YamlNode.withScalarValue`. The lines stay while each boundary still stands at the space or the line feeds that it replaced and the text next to a fold is no blank. Otherwise the writer writes the normal spelling.

To add a comment to a tree that you build, use `YamlNode.withTrailingComment` for a comment behind a value. The text of a comment is everything behind the `#`.

```fsharp
let note = YamlNode.withTrailingComment (YamlComment.create " checked") (YamlNode.plain "1.0.0")

printf "%s" (Writer.writeNode WriterOptions.defaults (Encode.object [ ("version", note) ]))
```

```yaml
version: 1.0.0 # checked
```

### Scalar styles

A node stores the style of its scalars and collections. A scalar that you construct states a preference, and the writer quotes a text that cannot stand plain. Examples are a text that starts with an indicator and a text that holds `: ` or ` #`. `Encode.string` double-quotes a text that the core schema reads as null, a bool, an int or a float.

The `Styles` option of `WriterOptions` decides what the writer does with the stored styles.

| `StylePolicy` | Behavior |
|---|---|
| `PreferStored` | Keeps a stored style that can express the value and picks the nearest valid style otherwise. This is the default. |
| `RequireStored` | Raises `YamlWriteException` where a stored style cannot express the value. |
| `Canonical` | Ignores the stored styles. Scalars are plain where that keeps the meaning and double-quoted otherwise, and collections are block collections. |

```fsharp
let literal = YamlNode.scalar (ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Clip, None)) "one\ntwo\n"
let tree = Encode.object [ ("text", literal) ]

printf "%s" (Writer.writeNode WriterOptions.defaults tree)
printf "%s" (Writer.writeNode { WriterOptions.defaults with Styles = Canonical } tree)
```

```yaml
text: |
  one
  two
text: "one\ntwo\n"
```

## How it reads and writes YAML

### The tree of nodes

* `YamlNode` is the one type for every value. Its `Kind` is a scalar, a mapping, a sequence or an alias. A mapping entry holds a key node and a value node, so a key can be a collection or an alias as well as a scalar.
* Every node has `Properties` with an optional tag and an optional anchor, and a `PropertyLayout` that records where each property stood in the text. A tag is the non-specific tag `!` or a named tag with its expanded prefix and suffix.
* A `YamlScalar` keeps the cooked `Value` and the `Style`. The style is one of plain, single-quoted, double-quoted and block. A block scalar keeps the style and the chomping mode of its header and its optional indentation indicator. A missing value, as in `k:`, is a plain scalar with the text "" and a zero-width span. A scalar that the parser read over several lines also keeps its line breaks in `Layout`, unless it is a literal block scalar. The layout is an opaque list of the line breaks that folding turned into the value. Each boundary has an index of `Value` and a kind. Plain, single-quoted and folded scalars have folds into one space. A double-quoted scalar has folds into one space or into line feeds, and escaped breaks. A constructed scalar has no layout. `YamlNode.equalIgnoringSpans` and `YamlNode.show` include the layout, as in `breaks=[f@4; e@9]`.
* Every comment and blank line has one slot in the tree. A node has a `TrailingComment` for the comment behind its last token. A block scalar keeps the comment behind its header in `HeaderComment`, and a flow collection keeps the comment behind its opener in `OpeningComment`. Entries and items have a `Before` list for the trivia in front of them, and collections have an `End` list. A line break behind an indicator holds its comment and the lines that follow. A trivia line is a comment line or a blank line. The rules R1 to R8 in the comments of `Model.fs` decide the slot when the text allows more than one.
* A `YamlDocument` has a `Root`, a `Prelude` for its directives, a `StartMarker` and an `EndMarker`, and the trivia in `BeforeRoot`, `AfterRoot` and `AfterEndMarker`. `AfterEndMarker` holds the comments and blank lines behind the `...` when no document follows them, so `Reader.read` and `Writer.write` keep a comment behind the end marker. A `YamlStream` holds the documents with the trivia and the extra end markers that belong to no document.
* Every parsed node, entry, item, comment, directive and document has a `SourceSpan` with 1-based lines and columns. A column counts Unicode code points, so an emoji is one column on every target. Constructed nodes have no span.
* `YamlNode` has constructors for trees that a program builds, such as `plain`, `scalar`, `mapping`, `sequence`, `flowSequence`, `entry`, `item`, `withTag`, `withAnchor` and the `with` functions for comments. `Validate.document` and `Validate.stream` list the rules that a tree breaks.
* The records that hold nodes have no structural equality, because the generic one recurses through the whole tree and overflows the stack on a deep one. `YamlNode.equalIgnoringSpans` compares two trees and `YamlNode.show` prints one. `YamlDocument` and `YamlStream` have the same two functions.

### Reading

* `Reader.read` and `Reader.readRoot` reject a text with several documents with the code `multiple-documents`. A text without any document reads as a document with a missing root that keeps the comments of the text.
* The parser follows the productions of YAML 1.2.2 and is strict. Examples of what it rejects are a tab as indentation, a `#` glued to a closed quoted scalar or flow collection, a flow collection that the text never closes and an alias without an anchor.
* The lexer enforces the character productions of the specification. Plain scalars, block scalars, comments, names and directives take [27] nb-char characters only. A C0 control character other than the tab, DEL, a C1 control character other than U+0085, U+FFFE, U+FFFF, the byte order mark and a surrogate without its partner are the error `invalid-character` there, and its span points at the character. U+0085, U+00A0, U+2028 and U+2029 are text, because YAML 1.2 ends a line at a line feed or a carriage return only. A quoted scalar takes every [2] nb-json character, which is the tab and everything from U+0020 up, so DEL, the C1 controls, U+FFFE, U+FFFF and the byte order mark are valid in it. It rejects the other C0 controls and a surrogate without its partner with the same code. A pair of surrogates is one character.
* A `#` starts a comment at the start of a line or behind white space. Values such as `tool.yml#main` and `C#` keep the `#`.
* The parser keeps its state on the heap, so a collection nested 10000 levels deep parses on every target. The time grows linearly with the length of the input.

### Differences from yaml@2.6.1

The tests record the result of yaml@2.6.1 above each case. The table lists the cases where the two parsers give different results.

| Case | YAMLicious | yaml@2.6.1 |
|---|---|---|
| A clip or keep block scalar whose last line ends the input, as `k: \|` with the body `  a` | "a", without a final line feed ([165]) | "a\n" |
| A line of spaces beyond an indentation indicator, as `k: \|1` with a body line of three spaces | Content: two spaces and a line feed ([167], [171]) | An empty value |
| An empty line behind an escaped line break in a double-quoted scalar, between `"a\` and `  b"` | "a\nb", one line feed per empty line ([112]) | "a b" |
| A lone carriage return | A line break | A character |
| A `?` in front of a flow indicator, as `[?]` and `{?}` | The error `reserved-indicator` ([142], [150]) | An empty key with a null value |
| An escape that spells a lone surrogate, as `"\ud800"` | The error `invalid-escape` on every target, because a Python string cannot hold it | Reads it |
| A byte order mark anywhere but at the start of a document or inside a quoted scalar | The error `invalid-character` | Keeps it in the text |
| A verbatim tag or `%TAG` prefix with other than URI characters, or a space behind `%`, as `!<{x}> a`, `%TAG !e! {bad}` and `% YAML 1.2` | Errors | Accepts all three |
| An alias without an earlier anchor | The error `undefined-alias` when the parser reads it | Fails when it builds the value |
| A `...` that no document precedes, as in example 9.3 of the specification | An extra end marker of the stream, so two documents ([205]) | Three documents |
| A line of blanks with a tab behind a block scalar at the root | The prefix of the next document | The error `BAD_INDENT` |
| Two keys that repeat each other | Kept in the tree. The object getters and the map and dict decoders reject them | Rejected while parsing |
| `.nan` and `.NaN` as keys, or two collection keys with equal content | One key, because the decoders compare keys as YAML values | Two keys, because NaN differs from itself and two collections are two objects in JavaScript |
| A collection or an alias as a key | Stays a node in the tree | A collection key becomes a string such as "[ a, b ]", and an alias key resolves to its value |

ruamel.yaml and libyaml read a block scalar at the end of the input as YAMLicious does. The YAML test suite (cases JEF9/02 and L24T/01) adds the final line feed, as yaml@2.6.1 does.

### Indentation of quoted scalars and flow collections

* The lines of a quoted scalar and the lines of a flow collection may stand at any indentation. A continuation line of `doc: "first.` may stand at the column of `doc` or at the column 0, and the items and the closer of `items: [` may stand at the column 0. YAML 1.2.2 asks for lines deeper than the node that owns the scalar or the collection ([69], [80], [81]). The parser accepts these layouts, because ruamel reads them with the folding of [73] to [81] and yaml@2.6.1 rejects them, except for the closer of an outermost flow collection at the column of the owner. A quoted scalar runs to its closing quote and a flow collection runs to its closer, so the indentation of their lines cannot change where they end or what the parser reads around them.
* Two limits remain. A document marker at the start of a line is an error inside a quoted scalar and inside a flow collection. A tab is no indentation, so a plain scalar ends in front of an empty line that holds a tab behind too few spaces, and a plain scalar in a flow collection also ends in front of a line with text that starts with a tab behind too few spaces, as it does in ruamel. A plain scalar in the block structure keeps the rule of the specification, because a line at the column of its owner starts the next entry.
* The key of a flow mapping entry may go over several lines, and its colon may stand on a later line, as in Example 9.4 of the specification (`{ matches` followed by `% : 20 }`). ruamel rejects that layout at every indentation, so no tolerance covers it, and the lines of such a key and of its colon stand deeper than the node that owns the collection. An entry with `?` and a key without a colon take their lines at any indentation.
* The tree holds the folds of the lines and no indentation, so the writer puts every line one step deeper than its parent, and the closer of a flow collection one step deeper than its key.

### Decoding

#### Getters and error paths

* `get.Required.Field` and `get.Optional.Field` read one field. `get.MultipleOptional.FieldList` returns the entries with the given names, and `get.Overflow.FieldList` returns the other entries. Both return a `MappingEntry list` in source order. `get.Entries` returns every entry.
* A `DecodeError` has a `Message` and a `Path`, and its `Span` is the range of the node that failed. `DecodeError.text` gives one line, as the example under [Errors with positions](#errors-with-positions) prints it.
* A path leads from the node that the caller passed to the node that failed. A field adds its name and a sequence item adds `[i]`. An entry of a map or dict adds `{key}`.

#### Scalars

* A plain scalar is null, a bool, an int or a float when it matches the core schema, and `yes`, `no`, `on` and `off` are strings. A quoted scalar is a string unless a tag says otherwise, so `"1"` is a string for `Decode.int` and `!!int "1"` is an int. An explicit `!!null` needs a null spelling or an empty text. Ints read decimal digits. `0o` starts octal digits and `0x` starts hexadecimal digits. Floats read `.nan` and the infinities `.inf` and `-.inf`.
* `Decode.int64` reads the same forms as `Decode.int` in 64 bits.
* `CoreSchema.resolveKind` returns a `CoreKind`, also for an integer beyond 64 bits. `CoreSchema.integerToFloat` reports an integer beyond the float range as `BeyondFloatRange`.
* `Decode.nonNull decoder` rejects a missing value and a null of the core schema, which is a plain `null`, `Null`, `NULL` or `~`, or the same under `!!null`. The error is at the node. Any other node goes to the decoder, so a quoted `"null"` and `""` pass, and so does a collection. `get.Optional.Field` turns a null into `None` before its decoder runs, so `Decode.nonNull` belongs with `get.Required.Field`, as in `get.Required.Field "name" (Decode.nonNull Decode.string)`. There it gives one message for a null field in front of any decoder.
* For `Decode.string` the tag decides before the style. `!!str` and the non-specific tag `!` give the text in any style, and any other explicit tag is an error. Without a tag a quoted or block scalar is a string, and a plain scalar is one when the core schema resolves it to a string. So a plain `1`, `true` or `null` and a missing value are errors, and the message names the tag that the plain scalar resolves to. `Decode.char` follows the same rules. `Decode.scalarText` reads the cooked text of any scalar, so `1` reads as "1" and a missing value reads as "". `Decode.option` returns `None` for a null and for a missing value. `get.Optional.Field` returns `None` for a missing field and for a null value, and `get.Entries` shows the difference.

#### Keys

* `Decode.object` checks the keys of the mapping before it decodes anything, and so do `Decode.map` and `Decode.dict`. Two keys that are the same YAML value are an error at the second key in any spelling. `1`, `+1`, `0x1` and `01` are one int, `1.0` and `10e-1` are one float, every null spelling is one null, and `.nan` and `.NaN` are one key. The int `1`, the float `1.0`, the bool `true` and the string `"1"` stay four keys. A sequence key compares its items in order and a mapping key its entries in any order. A scalar under a tag outside the core schema compares by its tag and its text. An alias as a key is an error. The message names the place of both keys, and both spellings when they differ. The check takes time that grows with the length of the keys. The one exception is an octal or hexadecimal int of more than 32 digits next to a decimal int of a similar length with the same value modulo two fixed primes, as an equal value has: it converts to decimal once, in time that grows with the square of its length.
* `Decode.object` needs keys that `Decode.string` reads, so a plain `1` as a key is an error at the key. `Decode.map` and `Decode.dict` also reject two distinct keys that the key decoder turns into the same key, as it turns `1` and `"1"` into one text under `Decode.scalarText`.

#### Dates and times

* `Decode.datetime` and `Decode.datetimeOffset` read ISO 8601 forms only: a date, or a date and a time with a `T` or a blank between them. The time needs seconds and may have a fraction of one to seven digits and a zone. A date alone is midnight, and `Decode.datetimeOffset` needs the zone. Any other text is an error on every target, also `1`, `2020`, `5` and `12:00`, which the parse of a target would read. The YAML 1.1 timestamp forms are not read.
* The kind of a `DateTime` and the offset of a `DateTimeOffset` depend on the target. .NET keeps the UTC kind of a `Z`. On Fable the kind depends on the time zone of the host. .NET and Python keep the offset of the text, and JavaScript gives the offset of the machine.

### Expanding aliases

* Every decoder except `Decode.alias` and `Decode.node` rejects an alias and names it. An alias names the most recent anchor of its name in front of it, and the anchors of a document end with the document.
* A copy has the spans of the anchored node and so has every node below it, so a decoder that fails inside a copy points at the text that holds the value. The copy keeps the properties of its node, the anchor included, and the comments inside it. The comment behind the alias replaces the comment behind the anchored node. The writer writes the anchor again on each copy, which YAML allows. A block scalar with the chomping keep and empty lines at the end of its value becomes a double-quoted scalar in the copy, because a blank line behind the copy would join those lines.
* The copies share the nodes below their top node with the anchored node, so the memory of the result grows with the input. `AliasOptions.MaxCopiedNodes` limits the nodes that the copies of one document hold (100000 in `AliasOptions.defaults`). The expansion adds up the sizes of the anchored nodes and stops at the alias that passes the limit with the error `alias-limit`, before it builds that copy. An alias inside its own anchored node, as in `&a [*a]`, is the error `recursive-alias`. An alias without an anchor in front of it is `undefined-alias`, which only a tree that a program built can hold.

### Encoding

* The value encoders build `YamlNode` values. A text that `Encode.string` double-quotes, such as `"1"` or `"true"`, reads back as a string. `Encode.float` writes `.nan`, `.inf`, `-.inf` and `-0.0` in forms that `Decode.float` reads back.
* `Encode.int64` writes a plain scalar that `Decode.int64` reads back. `Encode.nullValue` is the plain scalar `null`.
* `Encode.map` and `Encode.dict` take a key encoder that returns a node, so a key can be any node. `Encode.values` writes a block sequence of strings.
* A tree that needs comments or properties takes them from the constructors of `YamlNode`.
* `YamlNode.withScalarValue value node` gives the node another text. The properties and the comments stay, and so does the style. The span goes. The layout stays when it fits the new text as [Comments survive a round trip](#comments-survive-a-round-trip) describes, and the rules of the style have to hold as well. A boundary never moves, so an edit that shifts the text behind a boundary drops the layout, and the writer writes the normal spelling. A plain scalar that the core schema reads as a string gets the style of `Encode.string` when the new text reads as null, a bool, an int or a float, and it loses its layout then. A plain scalar that is typed already stays plain, so `80` becomes `81`. A node with an explicit tag keeps its style. A program that edits a decoded string and writes it again keeps the decoded node next to the string and encodes the edited string with this function. It fails on a node that is no scalar.

### Writing

#### Options and style policies

* `Writer.writeNode` writes a node as the root of a document, and every write function takes the `WriterOptions` value first.
* `Canonical` writes a scalar plain where that keeps its meaning under the core schema, and it writes every non-empty collection as a block collection. No policy drops a comment or a blank line.

#### Round trip and lines

* The writer puts every comment and blank line back in its slot. The output of a tree that the parser produced parses to the same tree except for the spans, and writing that tree again gives the same text. One kind of tree is the exception: an empty root block scalar without an indentation indicator that has a comment line behind it, or a blank line behind it when its chomping is keep. The first line behind the empty body would read as content. Only a text with a byte order mark line behind the scalar produces such a tree, for example `|+` followed by a line that holds a byte order mark. The writer writes the indicator 1, so the tree that comes back has it, and `RequireStored` raises with the message that a comment line or a tab line follows the scalar. A tree that a program built can differ in the styles that the writer had to change, and `RequireStored` raises there instead.
* The writer writes the lines of a scalar where the author broke them. A scalar that the parser read over several lines comes back over the same lines, so `doc: >` with three wrapped lines comes back as those three lines. The writer writes the lines of a layout again only while the layout fits the value and the style and the scalar stands where more than one line is possible. A layout fits when each boundary still stands at the space or at the line feeds that it replaced and a line of a plain or single-quoted scalar still starts and ends with text. The rules of double-quoted and folded scalars apply as well. An implicit key never goes over several lines, and neither does a flow collection that the writer keeps on one line. `PreferStored` and `RequireStored` write the normal spelling where the layout does not fit, and `RequireStored` raises only where the stored style cannot express the value. `Canonical` ignores the layout. The continuation lines stand one indentation step deeper than the parent, and the lines of a folded block scalar stand at the indentation of its body. The writer does not keep the indentation of the author.

#### Validation

* The writer checks the tree with `Validate` first and raises `YamlWriteException` with the messages of the validator when the tree breaks a rule.
* `Validate` checks more than the comment slots in a constructed tree. The text of a comment holds nb-char characters only, so it has no line break, control character, byte order mark or surrogate without its partner. An anchor or alias name has one or more ns-anchor-char characters. An entry that has a value has a colon, and so has an entry without a question mark in a block mapping. A document has at most one `%YAML` directive, and its version is 1.x. A tag handle is a word between two `!`, or one of the two short forms `!` and `!!`, and a document declares it once. A tag prefix holds URI characters. A reserved directive has a name that is neither `YAML` nor `TAG`, and its parameters are words that do not start with a `#`. A named tag has a suffix of one or more characters. The trivia behind the last node of a document stand in the `End` of the block collections along that node as the parser splits them: a blank line goes with the comment that follows it, so the `End` of a nested collection ends with a comment unless it holds the last comment, no `End` behind the one with the last comment holds anything, and trivia without a comment go to the `End` of the root collection.

#### Characters and escapes

* A character stands raw where the productions allow it, except in double quotes. Plain scalars, single quotes, block scalars, keys and comments write U+0085, U+00A0, U+2028 and U+2029 raw. Single quotes write every nb-json character raw, which is the tab and everything from U+0020 up, so DEL, the C1 controls, U+FFFE, U+FFFF and the byte order mark stand raw there. Double quotes write the tab raw and escape the line breaks, the other C0 controls, DEL, the C1 controls, U+FFFE, U+FFFF and the byte order mark. They spell U+0085, U+00A0, U+2028 and U+2029 with the named escapes `\N`, `\_`, `\L` and `\P`. The tree keeps no spelling of an escape, so a consumer that double-quotes a string gets escapes that readers of YAML 1.1 keep, while those readers take a raw U+0085, U+2028 or U+2029 for a line break. A key that would pass the limit of 1024 characters with these escapes writes the characters raw. A scalar whose style cannot hold one of its characters raw is written double-quoted under `PreferStored`, and `RequireStored` raises.

#### Keys and document markers

* A root plain scalar and the key of an entry of a root mapping start at the first column. When such a text begins with `---` or `...` and a blank or the end of the text follows, the writer puts the whole root one column in, where the text is a scalar and no document marker.
* An implicit key is limited to 1024 characters, and the writer measures the key as it writes it. It writes the normal spelling first and the shortest spelling the productions allow only when the normal one passes the limit. A flow collection as a key then separates its entries with `,` and no blank and writes a one-entry mapping in a flow sequence as a flow pair, and a double-quoted scalar of the key writes U+0085, U+00A0, U+2028, U+2029, DEL, the C1 controls, U+FFFE, U+FFFF and the byte order mark raw. A key that is still too long becomes an explicit key with `?`.
* The writer adds a `---` where the text would otherwise change the document count or lose the directives, and a `...` where a document with directives follows one without it.

#### Block scalars with the chomping keep

* A block scalar with the chomping keep owns the empty lines behind it ([165] to [169]), so a blank trivia line directly behind a stored keep scalar would read as part of the value. `Validate` rejects such a tree when the stored keep is the chomping written for the value, which is a value that is empty or ends in a line feed. A stored keep on a value without a final line feed is a style mismatch like any other: `PreferStored` writes strip, `RequireStored` raises, and the blank line behind the scalar is trivia. A blank line behind a fitting keep scalar is valid only at the end of a document, where only trivia and markers follow. It stands in `AfterRoot` behind a root scalar, in the `End` of the last collection, or in the prelude of the next document. The writer ends the scalar with a line that holds a tab when it is the root and with a line that holds a byte order mark when it stands in a collection, and the reader reads that line as the blank line again. A comment line behind the scalar ends the empty lines, so a blank line behind that comment line is trivia. A value that needs the chomping keep and has a blank line behind it is written double-quoted under `PreferStored` and `Canonical`, and `RequireStored` raises.
* `Writer.endsWithKeptLines options node` is true when the text of the node ends in a block scalar that the writer writes with the chomping keep. A caller that joins the text of several nodes leaves out the blank line behind such a node, because the empty lines behind a keep scalar belong to its value. The function follows the last entry of a block mapping and the last item of a block sequence, one level after the other. It stops at a collection that has trivia at its end, because those lines end the empty lines of the scalar. It also stops at a flow collection and at a scalar that is no block scalar. It uses the chomping that the writer writes when more text follows the node. A stored keep on a value without a final line feed does not fit, so the writer writes strip and the answer is false. `Canonical` writes no block scalar, so the answer is true there only for a stored keep that fits the value, because `Validate` rejects the blank line behind it under every policy.

---
## Local development

### Requirements

Because this library targets multiple programming languages we need to support all of them:

- [nodejs and npm](https://nodejs.org/en/download)
    - verify with `node --version` (tested with v22.19.0)
    - verify with `npm --version` (tested with 11.6.1)
- [.NET SDK](https://dotnet.microsoft.com/en-us/download)
    - verify with `dotnet --version` (`global.json` asks for 10.0.101 and accepts a newer 10.x SDK, tested with 10.0.303)
- [uv](https://docs.astral.sh/uv/getting-started/installation/)
    - verify with `uv --version` (tested with 0.9.26)


### Setup

Run these commands once in a fresh clone.

1. `dotnet tool restore` restores the .NET tools (Fable).
2. `npm ci` installs the JavaScript dependencies from `package-lock.json`.
3. `uv python install` installs the Python version of `.python-version` when it is missing.
4. `uv sync --frozen` installs the Python dependencies from `uv.lock`.

### Testing

`.\build.cmd test` runs the tests of every target. Name a target to run one of them:

`.\build.cmd test [f#, js, py, fable]`

`f#` runs the .NET tests, and `js` and `py` run the Fable output on Node and on Python. `fable` runs all three. The Python run goes through `uv run`, so the virtual environment needs no activation.

### Publish

Requires API keys for Nuget and PyPi. 

The following command will run all tests, bundle and then start publishing!

`.\build.cmd publish pipeline`

*or only publish specific targets, without test and bundle*

`.\build.cmd publish [npm, pypi, nuget]`
