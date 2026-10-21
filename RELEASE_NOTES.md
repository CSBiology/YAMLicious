### 2.0.0 (Unreleased)
* A `#` starts a comment only at the start of a line or after whitespace, as in YAML 1.2.2. Values such as `tool.cwl#main` or `C#` are no longer cut at the `#`.
* A `#` directly behind a closed quoted scalar or flow collection is an error, as in YAML 1.2.2. `k: "b"#c`, `k: 'b'#c`, `k: [a]#c` and `k: {a: 1}#c` throw with a message that names the line. Earlier versions read a comment. `C#`, `tool.cwl#main` and `a#b` are plain text.
* Inside block scalar content a `#` is always text.
* A comment line that is indented less than the content of a block scalar ends the scalar.
* Preprocessing reports a comment that ends a block scalar before content deeper than the node of the scalar, with the source lines, as in `A comment line (# c) ends a block scalar, so the more indented content after it is invalid. (Scalar line 1; comment line 3; content line 4.)`.
* Preprocessing reports more indented content right behind a block scalar, without a comment, as an indentation error with the source lines of the scalar and the content.
* A full-line comment belongs to the node that follows it. Comments directly under a nested key no longer fail with "Unknown pattern".
* A comment with no following node in its block stays at the end of that block. After the last item of a sequence it stays in the sequence.
* A blank line between a key with a trailing comment and its indented block belongs to that block.
* A comment at the end of the input takes the deepest open level that is at most as indented as the comment. A comment between two levels no longer fails with "Unknown pattern".
* Comments between sequence items are `YAMLElement.Comment` entries of the sequence. Earlier versions wrapped them in `Object [Comment ...]`, which looked like an extra item.
* All `Decode` functions skip comments.
* The writer emits comment entries of a sequence as comment lines.
* The writer writes plain scalars like `a#b` without quotes.
* The writer double-quotes an item of a flow sequence that contains `,`, `[`, `]`, `{` or `}`, as YAML 1.2.2 requires inside a flow collection. Earlier versions wrote `[f(a, b)]`, which reads as two items.
* `StyleVerifier.isPlainSafe` takes a `StyleVerifier.PlainContext` (`Block` or `Flow`) as its first argument.
* The writer puts the comment of a multi-line plain scalar behind its last line. Behind the first line the comment ended the scalar for other YAML readers.
* The writer picks the chomping of a block scalar for a value without a style from the trailing line breaks of the value, so the value reads back unchanged. Earlier versions wrote `a\n` as `|-` by default and lost the line break. `WriterOptions.MultilineFallbackChomping` is removed.
* `Encode.string` double-quotes text that the YAML 1.2 core schema reads as null, a bool, an int or a float, such as `true`, `1`, `1.5` or `null`, so that it reads back as a string. `Encode.CoreSchema.resolvesToNonString` exposes the rule.
* `Encode.bool` writes `true` and `false`. Earlier versions wrote `True` and `False`. `Encode.float` no longer throws on .NET.
* `Encode.char` and the keys of `Encode.map`, `Encode.dict` and `Encode.object` are double-quoted when the core schema would read them as null, a bool, an int or a float.
* `Decode.option` reads only a plain `null`, `Null`, `NULL`, `~`, an empty scalar, an empty node or a `!!null` scalar as `None`. A quoted or block scalar with that text is a string, so `Encode.option Encode.string (Some "null")` reads back as `Some "null"`. `!!str null` is the string `null`.
* `Decode.option` passes every other node to its decoder, as Thoth.Json does. `Decode.option (Decode.list Decode.string)` reads a sequence, which threw "Expected an option" in earlier versions, and `{}` gives `Some` of the decoded empty mapping.
* `get.Optional.Field` returns `None` for a field whose value is null, as for a missing field. It uses the null rule of `Decode.option`, so `label:` and `label: ~` give `None` and `label: "null"` gives `Some "null"`.
* `Encode.float` writes NaN and the infinities as `.nan`, `.inf` and `-.inf`, negative zero as `-0.0`, and a whole number with `.0`, as in `1.0`, so a core schema reader reads a float and not an int. On Python it writes `1e+21` and `5e-324` instead of every digit.
* `Decode.bool`, `Decode.int` and `Decode.float` read only plain scalars, or scalars under the matching tag, in the core schema forms. `Decode.bool` accepts `true`, `True`, `TRUE`, `false`, `False` and `FALSE`. `Decode.int` reads decimal, `0o` and `0x` ints. `Decode.float` reads forms such as `.NaN`, `+.Inf` or `1e3` and rejects `NaN` and `Infinity`, which YAML 1.2 reads as strings. A quoted `"true"`, `"5"` or `"1.5"` is a string and fails these decoders.
* `Encode.values` returns a sequence of string scalars, so `Decode.values` reads back what it wrote. Earlier versions returned sibling scalars that read back as one string.
* `Encode.CoreSchema` gains `resolvesToNull`, `isNull`, `tryParseBool`, `tryParseInt`, `tryParseInt64`, `tryParseFloat`, `floatText` and `stringContent`.
* `Decode.int64` and `Encode.int64` read and write 64-bit ints in the core schema forms. `CoreSchema.tryParseInt` is the 32-bit view of `CoreSchema.tryParseInt64`. A plain int beyond the int64 range still resolves as a non-string, and `Decode.int64` throws on it.
* Block scalar content keeps the exact indentation of lines that are indented deeper than the first content line. Earlier versions added two spaces per extra level.
* The content of a block scalar is indented as deep as its first nonblank line, or as deep as the indentation indicator says counted from the header.
* An explicit indentation indicator (`|2`) keeps the spaces beyond the indicated indentation as content.
* An explicit indentation indicator counts from the node that owns the scalar, as in YAML 1.2.2. In `- - |2` that is the column of the last dash.
* A block scalar header after `?`, as in `? |`, reads as a block scalar, also when the body is empty. So does a header behind nested indicators, as in `- - |` or `- ? |`.
* The writer puts the body of a block scalar with an explicit indentation indicator at the indicated indentation.
* The writer adds an indentation indicator (`|2`) when the first nonblank line of a block scalar starts with a space. YAML requires it, and other parsers reject the output without it.
* Keep chomping (`|+`) retains all trailing blank lines of a block scalar.
* A line of spaces in a block scalar that is longer than the content indentation is content, and the spaces beyond the indentation stay. A line with a tab is content too.
* A folded block scalar (`>`) turns one empty line between two lines into one line break. Earlier versions produced two. Decode folds the empty lines of a multiline plain scalar the same way.
* A line of spaces in front of the first content line of a block scalar that is longer than that line is an error, as in YAML 1.2.2.
* The writer adds an indentation indicator (`|2`) when the first line of a block scalar that is not empty holds only spaces.
* An empty block scalar such as `a: |` reads as the empty string. So does a body of blank lines under clip or strip chomping.
* A block scalar item such as `- |` in a sequence that is not indented under its key reads correctly.
* A quoted `#` before a block scalar header, as in `"a #b": |`, no longer breaks the block scalar.
* A flow collection can span several lines in every layout. The scanner merges its lines into the line where the collection opens, so `k: [a,` followed by `  b]`, `- [a,` followed by `   b]`, `data:` followed by an indented `[a, b,` and `c]`, `k: !!seq [` and a `[` with its `]` at the column of the key all read like the same collection on one line. Inside it a quote opens a quoted scalar and a `#` behind a blank starts a comment, so in `k: {` followed by `"a": 1, "b #c": 2` the second key is a string. Earlier versions threw "A flow collection over several lines in this layout is not supported yet" for most layouts, or returned a wrong structure.
* The lines of a flow collection must be indented deeper than the node that owns it. A line that starts with a closer may sit at the column of that node, as in the CWL layout `expressionLib: [` with `]` at column 0. A `#` comment inside the collection is a `YAMLElement.Comment` entry of its sequence or mapping. A plain scalar that goes on in the next line is one value, folded like a plain scalar in block context. A flow collection behind `!!seq` or `!!map` reads as that collection, and an anchor or another tag on it is an error, as on a block collection.
* A flow mapping entry without a value, such as `{ 5 }`, `{k:}` or `{a, b: c}`, maps to `YAMLElement.Nil`, like a block key without a value. A pair in a flow sequence, such as `[a: b]`, is a mapping with one entry. A colon separates a key only behind a blank or a flow indicator, or directly behind a quoted scalar or a flow collection, so `{a:b}` is the key `a:b` and `[12:45]` is one item. Earlier versions read `{a:"b"}` as the entry `a: b`.
* A flow collection as the key of an entry, as in `[a]: b`, `{? [a] : b}` or `[? {a: 1}]`, is an error that names the line, because YAMLicious keeps only scalar keys. Text, a bracket or a second `:` where a `,` or a closer belongs is an error too, as in `k: [a] b`, `[a [b]]` and `[a: b: c]`. A comment inside a flow collection ends the node in front of it, so `[a # c` followed by `b]` is an error.
* Tags and anchors inside a flow collection belong to the node behind them, as in block context. `[!!int "2", &a "x"]`, `{&a "q": !t "v"}` and `[!!str a` followed by `b]` keep the tag and the anchor on the scalar or the key. A verbatim tag such as `!<tag:yaml.org,2002:str>` is one tag, also in front of an empty item. Properties with no node behind them, as in `[!!str , a]` and `{a: &x}`, make an empty scalar that carries them. On a nested collection the core tag of its kind is dropped, and an anchor or any other tag is an error, as in block context. Earlier versions lost the properties or read them as text.
* An alias inside a flow collection reads as `YAMLElement.Alias`, as in block context. Earlier versions read the text `*a`.
* Text in front of a placeholder-shaped text inside a flow collection stays, so `[x <s f=0/> y]` is one item with that text. Earlier versions kept only the placeholder.
* An explicit key inside a flow collection, as in `{? a: b}`, `[? a : b]`, `{? a}` and `{ ? foo :, : bar }`, reads as the key. A `?` followed by a blank, the end or a flow indicator is the indicator, so `{?a: b}` has the key `?a`. Earlier versions read the `?` as part of the key.
* The key of an entry in a flow mapping and a key behind `?` may span several lines and fold like a plain scalar, as in `{ matches` followed by `% : 20 }`. The key of a pair in a flow sequence, as in `[a` followed by `b : c]`, has to stay on one line, and the error says so.
* A `,` directly behind a `[`, a `{` or another `,`, as in `[a,,b]` and `{,a}`, is an error that names the line. A trailing `,` stays valid. An anchor without a name inside a flow collection, as in `[&]`, is an error too.
* A block sequence or mapping cannot start on the line of its key. `k: - a`, `- k: - a` and `k: ? a` are errors, where earlier versions read a nested collection. The compact forms `- - a`, `- ? a`, `? - a` and `: - a` stay valid.
* A closer of the other kind, as in `k: [a}`, is an error. So is a flow collection that is still open at the end of the input or at a document marker. Both errors name the line of the opener.
* A `[` or `{` at the start of a plain scalar continuation line no longer hides the block scalars after it.
* A multiline quoted scalar before a block scalar no longer corrupts the double-quoted strings that follow it. Earlier versions turned `c: "z"` into the value `"z"` with its quotes.
* `''` reads as the empty string.
* A quote inside a plain scalar is text, as in YAML 1.2.2. `o["k"]`, `a, "b"` and `$(inputs["my-input"].path)` keep their quotes. Earlier versions read them as `o[k]`, `a, b` and `$(inputs[my-input].path)`. A quote opens a quoted scalar only where a node can start, and a `,`, `[` or `{` in a plain scalar outside a flow collection is text.
* A quote at a node start has to open a valid quoted scalar. `k: "abc` without a closing quote, `k: "a" b`, `k: 'it''s' x` and `k: "a", b` are errors that name the line. Earlier versions read them as plain text.
* A second `: ` behind a quoted part of a line, as in `k: "x": 1` and `"a": "b": c`, is an error. Earlier versions read the key `k: x`.
* A quote is part of an anchor name, and a single quote is part of a tag name, as in `&a"b` and `!t'y`.
* A line indented deeper than the node of a plain scalar continues that scalar, as in YAML 1.2.2. A quote, `- `, `? |` or a bracket on such a line is text and opens no quoted scalar, flow collection or block scalar. The scanner turns a plain scalar that spans several lines into one token, like a quoted scalar, so no later stage builds a node from its lines. A comment behind its last line is the comment of the value.
* The node of a plain scalar is the block node that contains it. A value on a line of its own, as in `valueFrom:` followed by `$(f(inputs.a,` and `"x", 1))` on indented lines, belongs to its parent, so the lines below it continue the scalar and their quotes stay text. The same holds behind a lone `-`, at the root and behind `---`. A line that holds only a tag or an anchor leaves the value to the node in front of it.
* A tag or an anchor on a line of its own belongs to the node behind it. `k: !!str` followed by `a` and `b` on indented lines reads as `a b`. Earlier versions read `\na b`. A block mapping or sequence behind `!!map` or `!!seq` reads without a stray empty scalar, and the core tag is dropped because it only names the kind of collection.
* An anchor or any other tag on a mapping or a sequence is an error, because YAMLElement has no field for properties on a collection and they would be lost. This covers `base: &b` followed by an indented mapping, `k: !custom` followed by a sequence, `k: &x [a, b]` and a core tag of the other kind, as in `!!map [a, b]`. Earlier versions dropped them silently. Files that use anchors on collections with aliases, such as CI configurations, now throw.
* A key whose value holds only properties owns the sequence that follows at the indentation of the key. `sequence: !!seq` followed by `- entry` reads as `{"sequence": ["entry"]}`, as in YAML 1.2.2 example 8.22. Earlier versions read an empty tagged scalar and a sibling sequence.
* A node with two tags or two anchors is an error also when they stand on one line, as in `k: !!str !!int 1` or `k: &a &b 1`. Earlier versions kept the last one.
* A comment behind an explicit key, as in `? a # c` followed by `: b`, is the comment of the key. Earlier versions put the internal placeholder `<c f=1/>` into the key text.
* A key with no value, such as `k:` or `k: # c`, reads as a mapping entry with an empty value (`YAMLElement.Nil`), and the writer writes it back as `k:`. Earlier versions read the text `k:` as a plain scalar.
* An empty sequence item `-` and an explicit key with an empty value (`? a` followed by `:`) read as `YAMLElement.Nil`, and the writer writes an empty item as `-`. Earlier versions read `Object []`, the same as the empty flow mapping `{}`, and wrote `- {}`. `Object []` now stands for `{}` only. In `- # c` the comment stays behind the empty item.
* The writer has no legacy path for sibling plain scalars any more. A multi-line plain value is one `YAMLElement.Value` with the plain style.
* The writer writes an empty plain scalar that carries a tag or an anchor as its properties alone, as in `k: &a` and `- !!null`, so it reads back as the same node. Earlier versions wrote `""`, which turned the null of `k: &a` into the empty string. An empty scalar with a quoted style keeps its quotes.
* A comment ends a plain scalar, so a deeper line behind it, as in `k: a # c` followed by `  b`, is an error. A colon followed by a blank on a line that continues a plain scalar is an error too, because a plain scalar cannot hold a mapping key.
* A plain scalar cannot start with `%`, `@` or a backquote, wherever a node starts. This covers `k: %x`, `- %x`, `[%bad]`, `{@a: 1}` and a `%YAML` line inside the content of a document. The error names the line.
* A comma or a `#` that starts no comment cannot start a node either, and a `]` or `}` that closes no flow collection is an error. Inside a flow collection a `-` needs a plain character behind it, as in `[-x]`.
* A `|` or `>` at the start of a node has to begin a valid block scalar header. `a: | extra` and `valueFrom: > 5` read as plain text in earlier versions and are errors now. A block scalar indicator inside a flow collection is an error too.
* A tab in the indentation of a line is an error, and so is a tab between `-`, `?` or `:` and a block collection behind it, as in `-\tk: v`. A tab as separation before a value, as in `k:\tv`, and a tab inside a scalar or a block scalar body stay valid.
* A tab behind the document start marker in front of a block mapping, as in `---\tk: v`, is an error.
* A plain scalar value cannot contain `: `. `k: a: b` and a CWL expression written as a plain scalar such as `$(inputs.a ? 'x: y' : "z")` read as a mapping key in earlier versions and are errors now. Quote such a value or write it as a block scalar.
* The Reader ends a document only at a `...` at column 0, which the scanner finds. An indented `...`, as in `k:` followed by `  - ...`, is text.
* A line break in a double-quoted scalar folds as in YAML 1.2.2. The break and the blanks around it become one space, and each empty line becomes a line feed. Earlier versions kept the raw line breaks and the indentation, so `"one` and `  two"` on two lines read as `one\n  two` instead of `one two`.
* An escaped line break (a `\` at the end of a line) in a double-quoted scalar joins the lines without a space and keeps the blanks in front of the backslash. A blank that an escape such as `\t` produces is content and stays in front of a fold.
* Text in the shape of a placeholder is plain text. This holds for `x <c f=0/>` in a plain scalar and for `<c f=0/>` in a block scalar body.
* Only ASCII digits make the shape of a placeholder, so text such as `<s f=١/>` with another kind of digit is plain text too.
* A value that consists of such a text, as in `a: <c f=0/>`, reads with the style of a plain scalar.
* Directives are allowed at the start of the input and after a `...`. Every `---` ends the prelude, so a `%` line behind a `---` is content, which is an error because a plain scalar cannot start with `%`. `Reader.read` and `Reader.readDocuments` both reject `---\na: 1\n---\n%TAG ! t!\n---\nb: 2`.
* `Reader.readDocuments` keeps the comments of a prelude in the document that its `---` starts. This includes a comment behind a directive and a comment behind the `---`.
* `Reader.readDocuments` drops a document that holds only directives.
* `Reader.read` and `Reader.readDocuments` read the prelude of a document with the same code in `Persil`.
* An explicit mapping entry (`? key` and `: value`) reads its value like the value behind any other key. Earlier versions read the value text a second time, so `k: "q"\n? a\n: "x"` gave `a` the value `q`, and a comment behind the value threw a missing key error. The body of a block scalar value stays as written.
* A plain key may contain `{`, `[`, `}` and `]` after its first character, as in the OpenAPI path `/participants/{id}.json:` or `a[0]: 1`. Earlier versions threw "Unknown pattern" for the first and read the second as one scalar `a[0]: 1`.
* A key that starts with `$`, such as `$namespaces:` or `$schemas:` in CWL, reads as a key of its mapping. Earlier versions turned it into a sequence item and dropped the `$`.
* The lines behind a compact sequence entry nest under the node that the entry starts. In `- - class: File` that node is the inner sequence, and in `- key:` it is the mapping that starts at the key. Earlier versions attached such lines to the wrong node, so lists of lists of mappings such as CWL `fileCollectionSet` and entries like `- a:` with an indented block read wrongly or threw.
* `Persil.tokenize` replaces `Persil.stringCleanUp` and `Persil.singleQuotedStringCleanUp`. One scanner finds quoted scalars and comments and copies block scalar bodies as they are. It returns a `Persil.TokenizedYaml`.
* `Persil.commentCleanUp` is removed. `Persil.tokenize` finds the comments.
* `Persil.StringMatchPattern` and `Persil.SingleQuotedStringPattern` are removed.
* `Persil.CommentMatchPattern` and `Persil.cut` are removed.
* `Persil.pipeline` returns a `Persil.PreparedYaml` instead of an anonymous record, and its lines are `Persil.TokenLine` values instead of strings.
* `Persil.pipeline` prepares the whole input as one document. `Persil.prepareDocuments` splits a `Persil.TokenizedYaml` into one `Persil.PreparedYaml` for each document.
* `Persil` gains the functions `tokenize` and `prepareDocuments` and the constants `NoBlock` and `NoComment`.
* `Persil` gains the types `TokenizedYaml`, `TokenLine`, `LineKind` and `PreparedYaml`.
* `Persil.TokenizedYaml` has the fields `Lines`, `StringMap` and `CommentMap`.
* `Persil.TokenLine` has the fields `Text`, `Kind`, `BlockId`, `SourceLine`, `CommentId` and `CodeLength`.
* `Persil.LineKind` has the cases `Code`, `CommentOnly`, `Blank`, `Directive`, `DocumentStart`, `DocumentEnd`, `BlockHeader` and `BlockBody`.
* `Persil.PreparedYaml` has the fields `Lines`, `StringMap`, `CommentMap`, `YAMLVersion` and `TagHandles`.
* `Syntax.Document` is removed, with `isStart`, `isEnd`, `isMarker`, `isTopLevelMarker`, `tryInlineContentAfterStartMarker`, `isDirectivePreludeLine` and `isDirectivePreludeOnly`. The active pattern `RegexActivePatterns.DocumentEnd` is removed too.
* `Syntax.BlockScalar.tryDetectHeaderIndent` and `Syntax.BlockScalar.isHeaderLine` are replaced by `Syntax.BlockScalar.tryParseHeaderCode`. It takes the code of a line without its comment and returns the parsed header together with its baseline, which is the indentation of the node that owns the scalar. It finds no header in `a: | extra`.
* `Syntax.BlockScalar.headerTokenFromLine` is removed.
* `Syntax.BlockScalar` gains `tryParseHeaderCode`, the type `HeaderLine`, `isHeaderValue`, `contentIndent` and `isBodyLine`.
* `Syntax` gains `CommentMarker.startsAt`, which tells whether a `#` starts a comment, and `Property.endIndex`, which finds the end of a tag or an anchor.
* `Syntax` gains the module `LineValue`. `LineValue.find` returns where the value of a line starts, the indentation of the node that owns it, and whether the line holds a bare value without an indicator or a key (`Bare`). The block scalar header parse, the scanner and Preprocessing share it.
* `Syntax.Line.isBlank` is replaced by `Syntax.Line.isSpacesOnly`. A line with a tab is no longer blank.
* `Regex.SchemaNamespacePattern` and the active pattern `RegexActivePatterns.SchemaNamespace` are removed.
* The aliases `Regex.InlineSequencePattern`, `Regex.InlineJSONPattern`, `RegexActivePatterns.(|InlineSequence|_|)` and `RegexActivePatterns.(|InlineJSON|_|)` are removed. Use `FlowStyleArrayPattern`, `FlowStyleObjectPattern`, `(|FlowStyleArray|_|)` and `(|FlowStyleObject|_|)`.
* The active patterns `RegexActivePatterns.SequenceSquareOpener`, `SequenceSquareCloser`, `FlowStyleObjectOpener`, `JSONKeyOpener`, `FlowStyleObjectCloser` and `JSONCloser` are removed, with their patterns in `Regex`. The scanner merges a flow collection into one line, so the Reader no longer matches its opener and closer lines.
* `Syntax.LineValue.dashColumns` returns the columns of the `-` entry indicators at the start of a line.
* `Preprocessing.read` gives every `-` entry indicator of a compact entry its own `Line "-"` element, and the node behind it nests in the following `Intendation`.
* `Decode.Helper.withoutComments` removes the comments from a `YAMLElement.Object` or a `YAMLElement.Sequence`.
* `YAMLiciousTypes.QuotedStringKind` gains the case `PlainScalar`. It holds the text of a plain scalar that spans several lines and plain text in the shape of a placeholder. A match over `QuotedStringKind` must handle it.

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
