### 2.0.0 (Unreleased)
* A `#` starts a comment only at the start of a line or after whitespace, as in YAML 1.2.2. Values such as `tool.cwl#main` or `C#` are no longer cut at the `#`. Inside block scalar content a `#` is always text. A comment line that is indented less than the content ends the scalar.
* Invalid input such as `[a]#c` (no space before `#`) is now read as the plain scalar `[a]#c`. Earlier versions read a comment.
* A full-line comment belongs to the node that follows it. Comments directly under a nested key no longer fail with "Unknown pattern".
* A comment with no following node in its block stays at the end of that block. After the last item of a sequence it stays in the sequence.
* Comments between sequence items are `YAMLElement.Comment` entries of the sequence. Earlier versions wrapped them in `Object [Comment ...]`, which looked like an extra item.
* All `Decode` functions skip comments.
* The writer emits comment entries of a sequence as comment lines and writes plain scalars like `a#b` without quotes.
* `Persil.CommentMatchPattern` is removed.
* Block scalar content keeps the exact indentation of lines that are indented deeper than the first content line. Earlier versions added two spaces per extra level.
* The writer puts the body of a block scalar with an explicit indentation indicator (`|2`) at the indicated indentation.
* The content of a block scalar is indented as deep as its first nonblank line, or as deep as the indentation indicator says counted from the header.
* An explicit indentation indicator (`|2`) keeps the spaces beyond the indicated indentation as content.
* Keep chomping (`|+`) retains all trailing blank lines of a block scalar.
* A block scalar item such as `- |` in a sequence that is not indented under its key reads correctly.
* A quoted `#` before a block scalar header, as in `"a #b": |`, no longer breaks the block scalar.
* A comment line that ends a block scalar and is followed by more indented content is reported as an error that names the comment.
* The writer adds an indentation indicator (`|2`) when the first nonblank line of a block scalar starts with a space. YAML requires it, and without it other parsers reject the output.
* New public API: `Syntax.CommentMarker.startsAt` tells whether a `#` starts a comment, and `Decode.Helper.withoutComments` removes the comments from a `YAMLElement.Object` or `YAMLElement.Sequence`.
* `Syntax.BlockScalar.tryDetectHeaderIndent` and `Syntax.BlockScalar.isHeaderLine` are replaced by `Syntax.BlockScalar.tryParseHeaderLine`, which returns the parsed header.
* `Persil.tokenize` replaces `Persil.stringCleanUp`, `Persil.singleQuotedStringCleanUp` and `Persil.commentCleanUp`. One scanner finds the quoted scalars, the comments and the block scalar bodies and returns the emitted text, both maps and a kind for each line. `Persil.pipeline` returns these kinds with its lines. `Syntax.BlockScalar.tryParseHeaderLine` returns the header together with its baseline and no longer takes `a: | extra` or a header inside a flow collection for a block scalar. A multiline quoted scalar before a block scalar no longer corrupts the double-quoted strings that follow it, which used to turn `c: "z"` into the value `"z"` with its quotes. `''` reads as the empty string.
* `Preprocessing` handles a block scalar header and its body as one unit and takes the end of the body from the scanner. A comment between a scalar and more indented content is reported with the source lines, as in `A comment line (# c) ends a block scalar, so the more indented content after it is invalid. (Scalar line 1; comment line 3; content line 4.)`. More indented content right behind a scalar, without a comment, is a separate indentation error. An empty block scalar such as `a: |` reads as the empty string, and so does a body of blank lines under clip or strip chomping. A body line such as `<c f=0/>` is text. Text of the shape `x <c f=0/>` in a plain scalar is plain text. A `[` or `{` at the start of a plain scalar continuation line no longer hides the block scalars after it, because the scanner no longer tracks flow collections across lines. `Persil.TokenLine` carries `SourceLine`, `CommentId` and `CodeLength`.

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
