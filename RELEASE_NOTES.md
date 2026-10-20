### 2.0.0 (Unreleased)
* A `#` starts a comment only at the start of a line or after whitespace, as in YAML 1.2.2. Values such as `tool.cwl#main` or `C#` are no longer cut at the `#`.
* Invalid input such as `[a]#c` (no space before `#`) is now read as the plain scalar `[a]#c`. Earlier versions read a comment.
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
* A `|` or `>` behind a `[` or `{` on the same line, as in `a: {b: |`, opens no block scalar. The scanner does not track flow collections across lines.
* A `[` or `{` at the start of a plain scalar continuation line no longer hides the block scalars after it.
* A multiline quoted scalar before a block scalar no longer corrupts the double-quoted strings that follow it. Earlier versions turned `c: "z"` into the value `"z"` with its quotes.
* `''` reads as the empty string.
* A line break in a double-quoted scalar folds as in YAML 1.2.2. The break and the blanks around it become one space, and each empty line becomes a line feed. Earlier versions kept the raw line breaks and the indentation, so `"one` and `  two"` on two lines read as `one\n  two` instead of `one two`.
* An escaped line break (a `\` at the end of a line) in a double-quoted scalar joins the lines without a space and keeps the blanks in front of the backslash. A blank that an escape such as `\t` produces is content and stays in front of a fold.
* Text in the shape of a placeholder is plain text. This holds for `x <c f=0/>` in a plain scalar and for `<c f=0/>` in a block scalar body.
* Only ASCII digits make the shape of a placeholder, so text such as `<s f=١/>` with another kind of digit is plain text too.
* A value that consists of such a text, as in `a: <c f=0/>`, reads with the style of a plain scalar.
* Directives are allowed at the start of the input and after a `...`. Every `---` ends the prelude, so a `%` line behind a `---` is content.
* The content of a document cannot start with `%`, as in YAML 1.2.2. `Reader.readDocuments` rejects `---\na: 1\n---\n%TAG ! t!\n---\nb: 2`. `Reader.read` checks only the first document, because it keeps every later marker as a line.
* `Reader.readDocuments` keeps the comments of a prelude in the document that its `---` starts. This includes a comment behind a directive and a comment behind the `---`.
* `Reader.readDocuments` drops a document that holds only directives.
* `Reader.read` and `Reader.readDocuments` read the prelude of a document with the same code in `Persil`.
* `Persil.tokenize` replaces `Persil.stringCleanUp` and `Persil.singleQuotedStringCleanUp`. One scanner finds quoted scalars and comments and copies block scalar bodies as they are. It returns a `Persil.TokenizedYaml`.
* `Persil.commentCleanUp` is removed. `Persil.tokenize` finds the comments.
* `Persil.StringMatchPattern` and `Persil.SingleQuotedStringPattern` are removed.
* `Persil.CommentMatchPattern` and `Persil.cut` are removed.
* `Persil.pipeline` returns a `Persil.PreparedYaml` instead of an anonymous record, and its lines are `Persil.TokenLine` values instead of strings.
* `Persil.pipeline` prepares the whole input as one document. `Persil.prepareDocuments` splits a `Persil.TokenizedYaml` into one `Persil.PreparedYaml` for each document.
* `Persil` gains the functions `tokenize` and `prepareDocuments` and the constants `NoBlock` and `NoComment`.
* `Persil` gains the types `TokenizedYaml`, `TokenLine`, `LineKind` and `PreparedYaml`.
* `Persil.TokenizedYaml` has the fields `Text`, `Lines`, `StringMap` and `CommentMap`.
* `Persil.TokenLine` has the fields `Text`, `Kind`, `BlockId`, `SourceLine`, `CommentId` and `CodeLength`.
* `Persil.LineKind` has the cases `Code`, `CommentOnly`, `Blank`, `Directive`, `DocumentStart`, `DocumentEnd`, `BlockHeader` and `BlockBody`.
* `Persil.PreparedYaml` has the fields `Lines`, `StringMap`, `CommentMap`, `YAMLVersion` and `TagHandles`.
* `Syntax.Document` loses `isStart`, `isTopLevelMarker`, `tryInlineContentAfterStartMarker`, `isDirectivePreludeLine` and `isDirectivePreludeOnly`.
* `Syntax.BlockScalar.tryDetectHeaderIndent` and `Syntax.BlockScalar.isHeaderLine` are replaced by `Syntax.BlockScalar.tryParseHeaderCode`. It takes the code of a line without its comment and returns the parsed header together with its baseline, which is the indentation of the node that owns the scalar. It finds no header in `a: | extra`.
* `Syntax.BlockScalar.headerTokenFromLine` is removed.
* `Syntax.BlockScalar` gains `tryParseHeaderCode`, the type `HeaderLine`, `isHeaderValue`, `contentIndent` and `isBodyLine`.
* `Syntax` gains `CommentMarker.startsAt`, which tells whether a `#` starts a comment, and `Property.endIndex`, which finds the end of a tag or an anchor.
* `Decode.Helper.withoutComments` removes the comments from a `YAMLElement.Object` or a `YAMLElement.Sequence`.
* `YAMLiciousTypes.QuotedStringKind` gains the case `EscapedPlainString` for plain text in the shape of a placeholder. A match over `QuotedStringKind` must handle it.

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
