### 2.0.0 (Unreleased)

YAMLicious 2.0.0 replaces the element model and the reader and writer around it. The new parser reads strict YAML 1.2.2 into a tree of nodes that keeps comments, blank lines, scalar styles, properties and source spans. The decoders and encoders keep their Thoth.Json style and work on the new nodes. The release breaks the 1.x API, and the migration table below maps each old call to its replacement.

#### Model

* `YamlNode` is the one type for every value. Its `Kind` is a scalar, a mapping, a sequence or an alias. A mapping entry holds a key node and a value node, so a key can be a collection or an alias as well as a scalar.
* Every node has `Properties` with an optional tag and an optional anchor, and a `PropertyLayout` that records where each property stood in the text. A tag is the non-specific tag `!` or a named tag with its expanded prefix and suffix.
* A `YamlScalar` keeps the cooked `Value` and the `Style`. The style is one of plain, single-quoted, double-quoted and block. A block scalar keeps the style and the chomping mode of its header and its optional indentation indicator. A missing value, as in `k:`, is a plain scalar with the text "" and a zero-width span. A scalar that the parser read over several lines also keeps its line breaks in `Layout`, unless it is a literal block scalar. The layout is an opaque list of the line breaks that folding turned into the value. Each boundary has an index of `Value` and a kind. Plain, single-quoted and folded scalars have folds into one space. A double-quoted scalar has folds into one space or into line feeds, and escaped breaks. A constructed scalar has no layout. `YamlNode.equalIgnoringSpans` and `YamlNode.show` include the layout, as in `breaks=[f@4; e@9]`.
* Every comment and blank line has one slot in the tree. A node has a `TrailingComment` for the comment behind its last token. A block scalar keeps the comment behind its header in `HeaderComment`, and a flow collection keeps the comment behind its opener in `OpeningComment`. Entries and items have a `Before` list for the trivia in front of them, and collections have an `End` list. A line break behind an indicator holds its comment and the lines that follow. A trivia line is a comment line or a blank line. The rules R1 to R8 in the comments of `Model.fs` decide the slot when the text allows more than one.
* A `YamlDocument` has a `Root`, a `Prelude` for its directives, a `StartMarker` and an `EndMarker`, and the trivia in `BeforeRoot` and `AfterRoot`. A `YamlStream` holds the documents with the trivia and the extra end markers that belong to no document.
* Every parsed node, entry, item, comment, directive and document has a `SourceSpan` with 1-based lines and columns. A column counts Unicode code points, so an emoji is one column on every target. Constructed nodes have no span.
* `YamlNode` has constructors for trees that a program builds, such as `plain`, `scalar`, `mapping`, `sequence`, `flowSequence`, `entry`, `item`, `withTag`, `withAnchor` and the `with` functions for comments. `Validate.document` and `Validate.stream` list the rules that a tree breaks.
* The records that hold nodes have no structural equality, because the generic one recurses through the whole tree and overflows the stack on a deep one. `YamlNode.equalIgnoringSpans` compares two trees and `YamlNode.show` prints one. `YamlDocument` and `YamlStream` have the same two functions.

#### Reading

* `Reader.readRoot` reads a text with one document and returns its root node. `Reader.read` returns the `YamlDocument`. `Reader.readDocuments` returns the `YamlStream` of a text with any number of documents, and `Reader.documents` lists the documents of a stream. `Reader.tryRead` and `Reader.tryReadDocuments` return a `Result` with a `YamlError`. The functions without `try` raise `YamlParseException`.
* `Reader.read` and `Reader.readRoot` reject a text with several documents with the code `multiple-documents`. A text without any document reads as a document with a missing root that keeps the comments of the text.
* The parser follows the productions of YAML 1.2.2 and is strict. Examples of what it rejects are a tab as indentation, a `#` glued to a closed quoted scalar or flow collection, a flow collection that the text never closes and an alias without an anchor.
* The lexer enforces the character productions of the specification. Plain scalars, block scalars, comments, names and directives take [27] nb-char characters only. A C0 control character other than the tab, DEL, a C1 control character other than U+0085, U+FFFE, U+FFFF, the byte order mark and a surrogate without its partner are the error `invalid-character` there, and its span points at the character. U+0085, U+00A0, U+2028 and U+2029 are text, because YAML 1.2 ends a line at a line feed or a carriage return only. A quoted scalar takes every [2] nb-json character, which is the tab and everything from U+0020 up, so DEL, the C1 controls, U+FFFE, U+FFFF and the byte order mark are valid in it. It rejects the other C0 controls and a surrogate without its partner with the same code. A pair of surrogates is one character.
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
* The parser keeps duplicate keys in the tree, and the object getters and the map and dict decoders reject them. yaml@2.6.1 rejects them while it parses. The decoders compare keys as YAML values, so `.nan` and `.NaN` are one key and two collection keys with equal content are one key. yaml@2.6.1 compares the values of JavaScript, where NaN differs from itself and two collection keys are two objects.
* A collection or an alias as a key stays a node in the tree. yaml@2.6.1 turns a collection key into a string such as "[ a, b ]" and resolves an alias key to its value.

#### Indentation of quoted scalars and flow collections

* The lines of a quoted scalar and the lines of a flow collection may stand at any indentation. A continuation line of `doc: "first.` may stand at the column of `doc` or at the column 0, and the items and the closer of `items: [` may stand at the column 0. YAML 1.2.2 asks for lines deeper than the node that owns the scalar or the collection ([69], [80], [81]). The parser accepts these layouts on purpose, because ruamel reads them with the folding of [73] to [81] and yaml@2.6.1 rejects them, except for the closer of an outermost flow collection at the column of the owner. A quoted scalar runs to its closing quote and a flow collection runs to its closer, so the indentation of their lines cannot change where they end or what the parser reads around them.
* Two limits remain. A document marker at the start of a line is an error inside a quoted scalar and inside a flow collection. A tab is no indentation, so a plain scalar ends in front of an empty line that holds a tab behind too few spaces, and a plain scalar in a flow collection also ends in front of a line with text that starts with a tab behind too few spaces, as it does in ruamel. A plain scalar in the block structure keeps the rule of the specification, because a line at the column of its owner starts the next entry.
* The key of a flow mapping entry may go over several lines, and its colon may stand on a later line, as in Example 9.4 of the specification (`{ matches` followed by `% : 20 }`). ruamel rejects that layout at every indentation, so no tolerance covers it, and the lines of such a key and of its colon stand deeper than the node that owns the collection. An entry with `?` and a key without a colon take their lines at any indentation.
* The tree holds the folds of the lines and no indentation, so the writer puts every line one step deeper than its parent, and the closer of a flow collection one step deeper than its key.

#### Decoding

* A `Decoder<'T>` is a function from `YamlNode` to `'T`. `Decode.object` reads a mapping through getters. `get.Required.Field` and `get.Optional.Field` read one field. `get.MultipleOptional.FieldList` returns the entries with the given names, and `get.Overflow.FieldList` returns the other entries. Both return a `MappingEntry list` in source order. `get.Entries` returns every entry.
* A decoder raises `YamlDecodeException` with a `DecodeError`, and `Decode.tryDecode` returns the error as a result. The error has a `Message` and a `Path`, and its `Span` is the range of the node that failed. `DecodeError.text` gives one line, such as `At inspection_packages[0]: The mapping has no field name. (line 3, column 5)`.
* A path leads from the node that the caller passed to the node that failed. A field adds its name and a sequence item adds `[i]`. An entry of a map or dict adds `{key}`.
* The scalar decoders follow the YAML 1.2 core schema. A plain scalar is null, a bool, an int or a float when it matches the schema, and `yes`, `no`, `on` and `off` are strings. A quoted scalar is a string unless a tag says otherwise, so `"1"` is a string for `Decode.int` and `!!int "1"` is an int. An explicit `!!null` needs a null spelling or an empty text. Ints read decimal digits. `0o` starts octal digits and `0x` starts hexadecimal digits. Floats read `.nan` and the infinities `.inf` and `-.inf`.
* `Decode.int64`, `Decode.scalarText`, `Decode.nonNull`, `Decode.uniqueKeys`, `Decode.tryDecode`, `Decode.entries`, `Decode.node` and `Decode.alias` are new. `Decode.int64` reads the same forms as `Decode.int` in 64 bits.
* `CoreSchema.resolveKind` gives the text of a plain scalar its core schema kind (`CoreKind`), also for an integer beyond 64 bits. `CoreSchema.integerToFloat` converts an integer text of any size and radix to the nearest float and reports an integer beyond the float range as `BeyondFloatRange`.
* `Decode.nonNull decoder` rejects a missing value and a null of the core schema, which is a plain `null`, `Null`, `NULL` or `~`, or the same under `!!null`. The error is at the node. Any other node goes to the decoder, so a quoted `"null"` and `""` pass, and so does a collection. `get.Optional.Field` turns a null into `None` before its decoder runs, so `Decode.nonNull` belongs with `get.Required.Field`, as in `get.Required.Field "name" (Decode.nonNull Decode.string)`. There it gives one message for a null field in front of any decoder.
* `Decode.string` reads a string of the core schema, and the tag decides before the style. `!!str` and the non-specific tag `!` give the text in any style, and any other explicit tag is an error. Without a tag a quoted or block scalar is a string, and a plain scalar is one when the core schema resolves it to a string. So a plain `1`, `true` or `null` and a missing value are errors, and the message names the tag that the plain scalar resolves to. `Decode.char` follows the same rules. `Decode.scalarText` reads the cooked text of any scalar, so `1` reads as "1" and a missing value reads as "". `Decode.option` returns `None` for a null and for a missing value. `get.Optional.Field` returns `None` for a missing field and for a null value, and `get.Entries` shows the difference.
* `Decode.object` checks the keys of the mapping before it decodes anything, and so do `Decode.map` and `Decode.dict`. Two keys that are the same YAML value are an error at the second key in any spelling. `1`, `+1`, `0x1` and `01` are one int, `1.0` and `10e-1` are one float, every null spelling is one null, and `.nan` and `.NaN` are one key. The int `1`, the float `1.0`, the bool `true` and the string `"1"` stay four keys. A sequence key compares its items in order and a mapping key its entries in any order. A scalar under a tag outside the core schema compares by its tag and its text. An alias as a key is an error. The message names the place of both keys, and both spellings when they differ. `Decode.entries` returns every entry as written, repeated keys included, and `Decode.uniqueKeys` checks the keys.
* `Decode.object` needs keys that `Decode.string` reads, so a plain `1` as a key is an error at the key. `Decode.map` and `Decode.dict` also reject two distinct keys that the key decoder turns into the same key, as it turns `1` and `"1"` into one text under `Decode.scalarText`. `Decode.uniqueKeys decoder` checks the keys of every mapping of a node, the ones that the decoder skips included, and then runs the decoder.
* Aliases stay symbolic and no decoder resolves an anchor. Every decoder except `Decode.alias` and `Decode.node` rejects an alias and names it. `Aliases.expandNode` replaces the aliases of a tree before the decoders run.
* `Decode.datetime` and `Decode.datetimeOffset` read ISO 8601 forms only: a date, or a date and a time with a `T` or a blank between them. The time needs seconds and may have a fraction of one to seven digits and a zone. A date alone is midnight, and `Decode.datetimeOffset` needs the zone. Any other text is an error on every target, also `1`, `2020`, `5` and `12:00`, which the parse of a target would read. The YAML 1.1 timestamp forms are not read.
* The kind of a `DateTime` and the offset of a `DateTimeOffset` depend on the target. .NET keeps the UTC kind of a `Z`, while Fable gives the kind Local on Python and Unspecified on JavaScript. .NET and Python keep the offset of the text, and JavaScript gives the offset of the machine.

#### Aliases

* `Aliases.expandNode options node` replaces each alias below the node with a copy of the node that its anchor names. `Aliases.expandDocument` and `Aliases.expandStream` do the same for a document and for each document of a stream, and each returns a `Result` with a `YamlError`. An alias names the most recent anchor of its name in front of it, and the anchors of a document end with the document. The parser still keeps an alias as a node, so a caller expands the aliases before it decodes.
* A copy has the spans of the anchored node and so has every node below it, so a decoder that fails inside a copy points at the text that holds the value. The copy keeps the properties of its node, the anchor included, and the comments inside it. The comment behind the alias replaces the comment behind the anchored node. The writer writes the anchor again on each copy, which YAML allows. A block scalar with the chomping keep and empty lines at the end of its value becomes a double-quoted scalar in the copy, because a blank line behind the copy would join those lines.
* The copies share the nodes below their top node with the anchored node, so the memory of the result grows with the input. `AliasOptions.MaxCopiedNodes` limits the nodes that the copies of one document hold (100000 in `AliasOptions.defaults`). The expansion adds up the sizes of the anchored nodes and stops at the alias that passes the limit with the error `alias-limit`, before it builds that copy. An alias inside its own anchored node, as in `&a [*a]`, is the error `recursive-alias`. An alias without an anchor in front of it is `undefined-alias`, which only a tree that a program built can hold.
* `YamlErrorCode` has two new cases, `recursive-alias` and `alias-limit`.

#### Encoding

* The value encoders keep their names and build `YamlNode` values. `Encode.string` double-quotes a text that the core schema reads as null, a bool, an int or a float, so `"1"` and `"true"` read back as strings. `Encode.float` writes `.nan`, `.inf`, `-.inf` and `-0.0` in forms that `Decode.float` reads back.
* `Encode.int64` and `Encode.nullValue` are new. `Encode.int64` writes a plain scalar that `Decode.int64` reads back.
* `Encode.optionalField name encoder value` returns `None` for a missing value. Combine the fields with `List.choose id` before they go to `Encode.object`.
* `Encode.map` and `Encode.dict` take a key encoder that returns a node, so a key can be any node. `Encode.values` writes a block sequence of strings.
* A tree that needs comments or properties takes them from the constructors of `YamlNode`.
* `YamlNode.withScalarValue value node` gives the node another text. The properties and the comments stay, and so does the style. The span goes. The layout stays when it fits the new text. It fits when each boundary still stands at the space or at the line feeds that it replaced and the text next to a fold is no blank. The rules of the style have to hold as well. A boundary never moves, so an edit that shifts the text behind a boundary drops the layout, and the writer writes the normal spelling. A plain scalar that the core schema reads as a string gets the style of `Encode.string` when the new text reads as null, a bool, an int or a float, and it loses its layout then. A plain scalar that is typed already stays plain, so `80` becomes `81`. A node with an explicit tag keeps its style. A program that edits a decoded string and writes it again keeps the decoded node next to the string and encodes the edited string with this function. It fails on a node that is no scalar.

#### Writing

* `Writer.writeNode` writes a node as the root of a document. `Writer.write` writes a `YamlDocument`, and `Writer.writeDocuments` writes a `YamlStream`. A `WriterOptions` value goes first. `WriterOptions.defaults` indents by two spaces and ends lines with `"\n"`. Its policy is `PreferStored`.
* `StylePolicy` decides what the writer does with the styles in the tree. `PreferStored` keeps a stored style that can express the value and picks the nearest valid style otherwise. `RequireStored` raises `YamlWriteException` where a stored style cannot express the value. `Canonical` ignores the stored styles. It writes a scalar plain where that keeps its meaning under the core schema and double-quoted otherwise, and it writes every non-empty collection as a block collection. No policy drops a comment or a blank line.
* The writer puts every comment and blank line back in its slot. The output of a tree that the parser produced parses to the same tree except for the spans, and writing that tree again gives the same text. One kind of tree is the exception: an empty root block scalar without an indentation indicator that has a comment line behind it, or a blank line behind it when its chomping is keep. The first line behind the empty body would read as content. Only a text with a byte order mark line behind the scalar produces such a tree, for example `|+` followed by a line that holds a byte order mark. The writer writes the indicator 1, so the tree that comes back has it, and `RequireStored` raises with the message that a comment line or a tab line follows the scalar. A tree that a program built can differ in the styles that the writer had to change, and `RequireStored` raises there instead.
* The writer writes the lines of a scalar where the author broke them. A scalar that the parser read over several lines comes back over the same lines, so `doc: >` with three wrapped lines comes back as those three lines. The writer writes the lines of a layout again only while the layout fits the value and the style and the scalar stands where more than one line is possible. A layout fits when each boundary still stands at the space or at the line feeds that it replaced and a line of a plain or single-quoted scalar still starts and ends with text. The rules of double-quoted and folded scalars apply as well. An implicit key never goes over several lines, and neither does a flow collection that the writer keeps on one line. `PreferStored` and `RequireStored` write the normal spelling where the layout does not fit, and `RequireStored` raises only where the stored style cannot express the value. `Canonical` ignores the layout. The continuation lines stand one indentation step deeper than the parent, and the lines of a folded block scalar stand at the indentation of its body. The writer does not keep the indentation of the author.
* The writer checks the tree with `Validate` first and raises `YamlWriteException` with the messages of the validator when the tree breaks a rule.
* `Validate` checks more than the comment slots in a constructed tree. The text of a comment holds nb-char characters only, so it has no line break, control character, byte order mark or surrogate without its partner. An anchor or alias name has one or more ns-anchor-char characters. An entry that has a value has a colon, and so has an entry without a question mark in a block mapping. A document has at most one `%YAML` directive, and its version is 1.x. A tag handle is a word between two `!`, or one of the two short forms `!` and `!!`, and a document declares it once. A tag prefix holds URI characters. A reserved directive has a name that is neither `YAML` nor `TAG`, and its parameters are words that do not start with a `#`. A named tag has a suffix of one or more characters. The trivia behind the last node of a document stand in the `End` of the block collections along that node as the parser splits them: a blank line goes with the comment that follows it, so the `End` of a nested collection ends with a comment unless it holds the last comment, no `End` behind the one with the last comment holds anything, and trivia without a comment go to the `End` of the root collection.
* A constructed plain scalar states a preference. The writer quotes a text that cannot stand plain. Examples are a text that starts with an indicator and a text that holds `: ` or ` #`.
* A character stands raw where the productions allow it, except in double quotes. Plain scalars, single quotes, block scalars, keys and comments write U+0085, U+00A0, U+2028 and U+2029 raw. Single quotes write every nb-json character raw, which is the tab and everything from U+0020 up, so DEL, the C1 controls, U+FFFE, U+FFFF and the byte order mark stand raw there. Double quotes write the tab raw and escape the line breaks, the other C0 controls, DEL, the C1 controls, U+FFFE, U+FFFF and the byte order mark. They spell U+0085, U+00A0, U+2028 and U+2029 with the named escapes `\N`, `\_`, `\L` and `\P`. The tree keeps no spelling of an escape, so a consumer that double-quotes a string gets escapes that readers of YAML 1.1 keep, while those readers take a raw U+0085, U+2028 or U+2029 for a line break. A key that would pass the limit of 1024 characters with these escapes writes the characters raw. A scalar whose style cannot hold one of its characters raw is written double-quoted under `PreferStored`, and `RequireStored` raises.
* A root plain scalar and the key of an entry of a root mapping start at the first column. When such a text begins with `---` or `...` and a blank or the end of the text follows, the writer puts the whole root one column in, where the text is a scalar and no document marker.
* An implicit key is limited to 1024 characters, and the writer measures the key as it writes it. It writes the normal spelling first and the shortest spelling the productions allow only when the normal one passes the limit. A flow collection as a key then separates its entries with `,` and no blank and writes a one-entry mapping in a flow sequence as a flow pair, and a double-quoted scalar of the key writes U+0085, U+00A0, U+2028, U+2029, DEL, the C1 controls, U+FFFE, U+FFFF and the byte order mark raw. A key that is still too long becomes an explicit key with `?`.
* The writer adds a `---` where the text would otherwise change the document count or lose the directives, and a `...` where a document with directives follows one without it.
* A block scalar with the chomping keep owns the empty lines behind it ([165] to [169]), so a blank trivia line directly behind a stored keep scalar would read as part of the value. `Validate` rejects such a tree when the stored keep is the chomping written for the value, which is a value that is empty or ends in a line feed. A stored keep on a value without a final line feed is a style mismatch like any other: `PreferStored` writes strip, `RequireStored` raises, and the blank line behind the scalar is trivia. A blank line behind a fitting keep scalar is valid only at the end of a document, where only trivia and markers follow. It stands in `AfterRoot` behind a root scalar, in the `End` of the last collection, or in the prelude of the next document. The writer ends the scalar with a line that holds a tab when it is the root and with a line that holds a byte order mark when it stands in a collection, and the reader reads that line as the blank line again. A comment line behind the scalar ends the empty lines, so a blank line behind that comment line is trivia. A value that needs the chomping keep and has a blank line behind it is written double-quoted under `PreferStored` and `Canonical`, and `RequireStored` raises.
* `Writer.endsWithKeptLines options node` is true when the text of the node ends in a block scalar that the writer writes with the chomping keep. A caller that joins the text of several nodes leaves out the blank line behind such a node, because the empty lines behind a keep scalar belong to its value. The function follows the last entry of a block mapping and the last item of a block sequence, one level after the other. It stops at a collection that has trivia at its end, because those lines end the empty lines of the scalar. It also stops at a flow collection and at a scalar that is no block scalar. It uses the chomping that the writer writes when more text follows the node. A stored keep on a value without a final line feed does not fit, so the writer writes strip and the answer is false. `Canonical` writes no block scalar, so the answer is true there only for a stored keep that fits the value, because `Validate` rejects the blank line behind it under every policy.

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
