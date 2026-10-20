module Tests.ScalarStyles

open Fable.Pyxpecto
open YAMLicious
open YAMLicious.YAMLiciousTypes
open Util

let private expectSingleMappingValue (ast: YAMLElement) =
    match ast with
    | YAMLElement.Object [YAMLElement.Mapping(_, YAMLElement.Value value)] -> value
    | _ -> failwithf "Unexpected AST: %A" ast

let private expectSingleSequenceItemValue (ast: YAMLElement) =
    match ast with
    | YAMLElement.Object [YAMLElement.Sequence [YAMLElement.Object [YAMLElement.Value value]]] -> value
    | _ -> failwithf "Unexpected AST: %A" ast

let private expectSingleRootValue (ast: YAMLElement) =
    match ast with
    | YAMLElement.Object [YAMLElement.Value value] -> value
    | _ -> failwithf "Unexpected AST: %A" ast

let private expectSingleMappingSequenceValues (ast: YAMLElement) =
    match ast with
    | YAMLElement.Object [YAMLElement.Mapping(_, YAMLElement.Object [YAMLElement.Sequence items])] ->
        items
        |> List.map (fun item ->
            match item with
            | YAMLElement.Object [YAMLElement.Value value] -> value.Value
            | _ -> failwithf "Unexpected sequence item: %A" item
        )
    | _ -> failwithf "Unexpected AST: %A" ast

/// A style-less value, as Encode.string creates it. Its first line starts with a space.
let private leadingSpaceValue = "  leading spaces first line\nnormal second line\n"

/// The written text with "\n" line breaks on every platform. Tests that use it compare the whole text,
/// so a blank line at its end counts.
let private writeLf (options: WriterOptions) (ast: YAMLElement) =
    (Writer.writeWithOptions ast options None).Replace("\r\n", "\n")

let private textMapping (key: string) (value: string) =
    YAMLElement.Mapping(YAMLContent.create(key), YAMLElement.Value(YAMLContent.create(value)))

let private textItem (value: string) =
    YAMLElement.Object [ YAMLElement.Value(YAMLContent.create(value)) ]

/// The scalars of the mapping values or of the sequence items in a document.
let private readScalars (text: string) =
    match Reader.read text with
    | YAMLElement.Object [ YAMLElement.Sequence items ] -> items |> List.map Decode.string
    | YAMLElement.Object entries ->
        entries
        |> List.map (fun entry ->
            match entry with
            | YAMLElement.Mapping(_, value) -> Decode.string value
            | _ -> failwithf "Unexpected entry: %A" entry
        )
    | other -> failwithf "Unexpected AST: %A" other

/// Style-less values with the text that Writer.write gives for them under the key k when nothing follows.
/// The header is the literal one. A kept blank line is the last line of that text, so the texts end in it.
let private chompingCases = [
    "a", "k: a\n"
    "a\n", "k: |\n    a\n"
    "a\n\n", "k: |+\n    a\n    \n"
    "a\n\n\n", "k: |+\n    a\n    \n    \n"
    "\n", "k: |+\n    \n"
]

let private expectFallbackChomping (options: WriterOptions) (headerChar: string) =
    for value, literalText in chompingCases do
        let mappingText = literalText.Replace("|", headerChar)
        let itemText = "- " + mappingText.Substring(3)
        let cases = [
            "mapping value at the end", YAMLElement.Object [ textMapping "k" value ], mappingText, [ value ]
            "mapping value before a key", YAMLElement.Object [ textMapping "k" value; textMapping "j" "x" ], mappingText + "j: x\n", [ value; "x" ]
            "sequence item at the end", YAMLElement.Object [ YAMLElement.Sequence [ textItem value ] ], itemText, [ value ]
            "sequence item before an item", YAMLElement.Object [ YAMLElement.Sequence [ textItem value; textItem "b" ] ], itemText + "- b\n", [ value; "b" ]
        ]
        for name, ast, expectedText, expectedValues in cases do
            let written = writeLf options ast
            Expect.equal written expectedText (sprintf "%s %A: the text" name value)
            Expect.equal (readScalars written) expectedValues (sprintf "%s %A: the values read back" name value)

/// The parts of a plain multiline scalar as the Reader returns them.
let private plainSegments (element: YAMLElement) =
    match element with
    | YAMLElement.Value content -> [ content ]
    | YAMLElement.Object items ->
        items
        |> List.map (fun item ->
            match item with
            | YAMLElement.Value content -> content
            | _ -> failwithf "Unexpected element: %A" item
        )
    | other -> failwithf "Unexpected element: %A" other

/// The comment sits on the last part, and the parts joined by line breaks are the value.
let private expectCommentOnLastSegment (segments: YAMLContent list) (value: string) =
    Expect.equal (segments |> List.map (fun segment -> segment.Value) |> String.concat "\n") value "The parts hold the value"
    let last = List.last segments
    Expect.equal last.Comment (Some " note") "The comment is on the last part"
    Expect.isTrue (segments |> List.take (segments.Length - 1) |> List.forall (fun segment -> segment.Comment.IsNone)) "No other part has a comment"

let Main =
  testList "Scalar Styles" [
    testCase "Normalization helper ignores scalar style metadata" <| fun _ ->
        let actual = YAMLElement.Value (YAMLContent.create("x", style=ScalarStyle.DoubleQuoted))
        let expected = YAMLElement.Value (YAMLContent.create("x"))
        Expect.yamlSemanticEqual actual expected "Semantic comparison should ignore style metadata"

    testCase "Writer emits styled inline sequence scalars with quotes" <| fun _ ->
        let ast =
            YAMLElement.Object [
                YAMLElement.Mapping(
                    YAMLContent.create("items"),
                    YAMLElement.Sequence [
                        YAMLElement.Value (YAMLContent.create("a", style=ScalarStyle.SingleQuoted))
                        YAMLElement.Value (YAMLContent.create("b", style=ScalarStyle.DoubleQuoted))
                    ]
                )
            ]

        let actual = Writer.write ast None
        let expected = "items: ['a', \"b\"]"
        Expect.trimEqual actual expected "Writer should preserve quote style in inline sequence values"

        let roundTripped = Reader.read actual
        let expectedRoundTripped =
            YAMLElement.Object [
                YAMLElement.Mapping(
                    YAMLContent.create("items"),
                    YAMLElement.Object [
                        YAMLElement.Sequence [
                            YAMLElement.Object [YAMLElement.Value (YAMLContent.create("a", style=ScalarStyle.SingleQuoted))]
                            YAMLElement.Object [YAMLElement.Value (YAMLContent.create("b", style=ScalarStyle.DoubleQuoted))]
                        ]
                    ]
                )
            ]
        Expect.equal roundTripped expectedRoundTripped "Reader should restore quote styles from emitted inline sequence"

    testCase "Writer fallback for a style-less multiline value without a trailing line break uses |-" <| fun _ ->
        let ast =
            YAMLElement.Object [
                YAMLElement.Mapping(
                    YAMLContent.create("note"),
                    YAMLElement.Value (YAMLContent.create("line1\nline2"))
                )
            ]

        let actual = Writer.write ast None
        let expected = """note: |-
    line1
    line2"""
        Expect.trimEqual actual expected "Style-less multiline values without a trailing line break should use |-"

        let parsed = Reader.read actual
        match parsed with
        | YAMLElement.Object [YAMLElement.Mapping(_, YAMLElement.Value value)] ->
            Expect.equal value.Value "line1\nline2" "The value should come back unchanged"
            Expect.equal value.Style (Some (ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Strip, None))) "Fallback style should be literal strip"
        | _ ->
            failwithf "Unexpected AST after fallback round-trip: %A" parsed

    testCase "Reader preserves plain multiline sequence style and Decode folds semantics" <| fun _ ->
        let yaml = """- My Value 1
  My Value 2
"""

        let parsed = Reader.read yaml
        match parsed with
        | YAMLElement.Object [YAMLElement.Sequence [firstItem]] ->
            Expect.equal (Decode.string firstItem) "My Value 1 My Value 2" "Decode should fold plain multiline sequence content"
            match firstItem with
            | YAMLElement.Object [YAMLElement.Value value] ->
                Expect.equal value.Value "My Value 1\nMy Value 2" "Reader should retain physical line breaks for write-back"
                Expect.equal value.Style (Some ScalarStyle.Plain) "Reader should preserve plain multiline style"
            | _ ->
                failwithf "Unexpected sequence item shape: %A" firstItem
        | _ ->
            failwithf "Unexpected AST: %A" parsed

    testCase "Writer preserves plain multiline sequence style on read-write-read" <| fun _ ->
        let yaml = """- My Value 1
  My Value 2
- My Value 3
"""

        let parsed = Reader.read yaml
        let written = Writer.write parsed None
        Expect.trimEqual written yaml "Writer should re-emit multiline plain sequence scalars in compact form"

        let reparsed = Reader.read written
        Expect.equal reparsed parsed "Plain multiline sequence read-write-read should preserve AST"

    testCase "Writer preserves plain multiline mapping style on read-write-read" <| fun _ ->
        let yaml = """note: line1
  line2
"""

        let parsed = Reader.read yaml
        let written = Writer.write parsed None
        Expect.trimEqual written yaml "Writer should keep mapping plain multiline continuation form"

        let reparsed = Reader.read written
        Expect.equal reparsed parsed "Plain multiline mapping read-write-read should preserve AST"

    testCase "Indented mapping values remain a plain multiline scalar" <| fun _ ->
        let yaml = """MyKey:
  test1
  test2
  test3
"""

        let parsed = Reader.read yaml
        match parsed with
        | YAMLElement.Object [YAMLElement.Mapping(key, YAMLElement.Object [YAMLElement.Value value])] ->
            Expect.equal key.Value "MyKey" "Key should parse normally"
            Expect.equal value.Value "test1\ntest2\ntest3" "Reader should preserve physical line breaks"
            Expect.equal value.Style (Some ScalarStyle.Plain) "Reader should mark the scalar as multiline plain style"
            Expect.equal (Decode.string (YAMLElement.Object [YAMLElement.Value value])) "test1 test2 test3" "Decode.string should return the folded YAML scalar value"
        | _ ->
            failwithf "Unexpected AST: %A" parsed

    testCase "Indented boolean-looking mapping values remain a plain multiline scalar" <| fun _ ->
        let yaml = """MyKey:
  true
  true
  false
"""

        let parsed = Reader.read yaml
        match parsed with
        | YAMLElement.Object [YAMLElement.Mapping(_, YAMLElement.Object [YAMLElement.Value value])] ->
            Expect.equal value.Value "true\ntrue\nfalse" "Reader should preserve physical line breaks"
            Expect.equal value.Style (Some ScalarStyle.Plain) "Reader should mark the scalar as multiline plain style"
            Expect.equal (Decode.string (YAMLElement.Object [YAMLElement.Value value])) "true true false" "Decode.string should fold the plain scalar"
        | _ ->
            failwithf "Unexpected AST: %A" parsed

    testCase "Chomping matrix round-trip preserves style and semantic value" <| fun _ ->
        let cases = [
            ("|-", """k: |-
  a
  b
""", "a\nb", ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Strip, None))
            ("|", """k: |
  a
  b
""", "a\nb\n", ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Clip, None))
            ("|+", """k: |+
  a
  b

""", "a\nb\n\n", ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Keep, None))
            (">-", """k: >-
  a
  b
""", "a b", ScalarStyle.Block(BlockScalarStyle.Folded, ChompingMode.Strip, None))
            (">", """k: >
  a
  b
""", "a b\n", ScalarStyle.Block(BlockScalarStyle.Folded, ChompingMode.Clip, None))
            (">+", """k: >+
  a
  b

""", "a b\n\n", ScalarStyle.Block(BlockScalarStyle.Folded, ChompingMode.Keep, None))
        ]

        for (label, yaml, expectedValue, expectedStyle) in cases do
            let ast1 = Reader.read yaml
            let value1 = expectSingleMappingValue ast1
            Expect.equal value1.Value expectedValue $"Parsed value mismatch for {label}"
            Expect.equal value1.Style (Some expectedStyle) $"Parsed style mismatch for {label}"

            let written = Writer.write ast1 None
            let ast2 = Reader.read written
            Expect.equal ast2 ast1 $"Round-trip mismatch for {label}"

    testCase "Explicit indentation indicator round-trip preserves semantic value" <| fun _ ->
        let yaml = """doc: |2
  explicit
"""
        let ast1 = Reader.read yaml
        let value1 = expectSingleMappingValue ast1
        Expect.equal value1.Value "explicit\n" "Initial explicit-indent parse should produce expected value"
        Expect.equal value1.Style (Some (ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Clip, Some 2))) "Style should preserve explicit indent metadata"

        let written = Writer.write ast1 None
        let ast2 = Reader.read written
        let value2 = expectSingleMappingValue ast2
        Expect.equal value2.Value value1.Value "Explicit-indent round-trip should not add leading spaces"
        Expect.equal ast2 ast1 "Explicit-indent round-trip should keep metadata and value"

    testCase "Explicit indentation indicator round-trip keeps a sequence item scalar" <| fun _ ->
        let yaml = "- |1\n  explicit\n"
        let ast1 = Reader.read yaml
        let value1 = expectSingleSequenceItemValue ast1
        Expect.equal value1.Value " explicit\n" "Initial parse should keep the space beyond the indicator"
        Expect.equal value1.Style (Some (ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Clip, Some 1))) "Style should preserve explicit indent metadata"

        let written = Writer.write ast1 None
        Expect.trimEqual written yaml "The body should sit one space beyond the dash, as the indicator says"
        let ast2 = Reader.read written
        let value2 = expectSingleSequenceItemValue ast2
        Expect.equal value2.Value value1.Value "Sequence item round-trip should not change the value"
        Expect.equal ast2 ast1 "Sequence item round-trip should keep metadata and value"

    testCase "Explicit indentation indicator round-trip keeps a root scalar" <| fun _ ->
        let yaml = "|2\n    x\n"
        let ast1 = Reader.read yaml
        let value1 = expectSingleRootValue ast1
        Expect.equal value1.Value "  x\n" "Initial parse should keep the spaces beyond the indicator"
        Expect.equal value1.Style (Some (ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Clip, Some 2))) "Style should preserve explicit indent metadata"

        let written = Writer.write ast1 None
        Expect.trimEqual written yaml "The body should sit two spaces in, as the indicator says, plus the content spaces"
        let ast2 = Reader.read written
        let value2 = expectSingleRootValue ast2
        Expect.equal value2.Value value1.Value "Root round-trip should not change the value"
        Expect.equal ast2 ast1 "Root round-trip should keep metadata and value"

    testCase "Writer adds an indentation indicator when the first line of a mapping value starts with a space" <| fun _ ->
        // yaml@2.6.1 reads the written text back as the original string
        let ast = YAMLElement.Object [
            YAMLElement.Mapping(YAMLContent.create("k"), YAMLElement.Value(YAMLContent.create(leadingSpaceValue)))
        ]
        let written = Writer.write ast None
        Expect.trimEqual written "k: |2\n    leading spaces first line\n  normal second line" "The indicator keeps the first line's spaces out of the indentation"
        let value = expectSingleMappingValue (Reader.read written)
        Expect.equal value.Value leadingSpaceValue "YAMLicious should read the value back unchanged"

    testCase "Writer adds an indentation indicator to the default strip chomping" <| fun _ ->
        let value = "  leading spaces first line\nnormal second line"
        let ast = YAMLElement.Object [
            YAMLElement.Mapping(YAMLContent.create("k"), YAMLElement.Value(YAMLContent.create(value)))
        ]
        let written = Writer.write ast None
        Expect.trimEqual written "k: |2-\n    leading spaces first line\n  normal second line" "The indicator and the chomping indicator go into one header"
        let reread = expectSingleMappingValue (Reader.read written)
        Expect.equal reread.Value value "YAMLicious should read the value back unchanged"

    testCase "Writer counts the indentation indicator from the header of a nested mapping value" <| fun _ ->
        let ast = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("a"),
                YAMLElement.Object [
                    YAMLElement.Mapping(YAMLContent.create("k"), YAMLElement.Value(YAMLContent.create(leadingSpaceValue)))
                ]
            )
        ]
        let written = Writer.write ast (Some (fun config -> { config with Whitespace = 2 }))
        Expect.trimEqual written "a:\n  k: |2\n      leading spaces first line\n    normal second line" "The body sits two spaces beyond the header plus the content spaces"
        Expect.yamlSemanticEqual (Reader.read written) ast "YAMLicious should read the nested value back unchanged"

    testCase "Writer adds an indentation indicator when the first line of a sequence item starts with a space" <| fun _ ->
        // yaml@2.6.1 reads the written text back as the original string
        let ast = YAMLElement.Object [
            YAMLElement.Sequence [
                YAMLElement.Object [YAMLElement.Value(YAMLContent.create(leadingSpaceValue))]
            ]
        ]
        let written = Writer.write ast None
        Expect.trimEqual written "- |2\n    leading spaces first line\n  normal second line" "The indicator keeps the first line's spaces out of the indentation"
        let value = expectSingleSequenceItemValue (Reader.read written)
        Expect.equal value.Value leadingSpaceValue "YAMLicious should read the item back unchanged"

    testCase "Writer adds an indentation indicator when the first line of a root value starts with a space" <| fun _ ->
        // yaml@2.6.1 reads the written text back as the original string
        let ast = YAMLElement.Value(YAMLContent.create(leadingSpaceValue))
        let written = Writer.write ast None
        Expect.trimEqual written "|2\n    leading spaces first line\n  normal second line" "The indicator keeps the first line's spaces out of the indentation"
        let value = expectSingleRootValue (Reader.read written)
        Expect.equal value.Value leadingSpaceValue "YAMLicious should read the root value back unchanged"

    testCase "Writer adds no indentation indicator when the first nonblank line starts without a space" <| fun _ ->
        let ast = YAMLElement.Object [
            YAMLElement.Mapping(YAMLContent.create("k"), YAMLElement.Value(YAMLContent.create("first\n  second\n")))
        ]
        let written = Writer.write ast None
        Expect.trimEqual written "k: |\n    first\n      second" "Deeper later lines need no indicator"

    testCase "Expression block scalar keeps quote delimiters and stays block style on write" <| fun _ ->
        let yaml = """expression: >
  ${ return (function() {
    function sanitize(entry) {
      var allowedFields = ['class', 'basename'];
      if (entry.basename === "arc" || entry.basename === ".git") return null;
      if (entry.class === "Directory") return null;
      return entry;
    }
    return sanitize(inputs.source);
  })(); }
"""

        let parsed = Reader.read yaml
        let parsedValue = expectSingleMappingValue parsed
        Expect.equal (parsedValue.Value.Contains("'class'")) true "Single-quoted literals should remain intact"
        Expect.equal (parsedValue.Value.Contains("\"Directory\"")) true "Double-quoted literals should remain intact"
        Expect.equal parsedValue.Style (Some (ScalarStyle.Block(BlockScalarStyle.Folded, ChompingMode.Clip, None))) "Expression should remain folded block style"

        let written = Writer.write parsed None
        Expect.equal (written.Contains("expression: >")) true "Writer should emit folded block header"
        Expect.equal (written.Contains("expression: ${")) false "Writer should not collapse expression to inline scalar"

        let reparsed = Reader.read written
        Expect.equal reparsed parsed "Expression read-write-read should preserve style metadata and semantics"

    testCase "Root block scalar preserves tag/anchor/comment on write" <| fun _ ->
        let yaml = """!<tag:foo> &a1 | # c
  hi
"""
        let parsed = Reader.read yaml
        let written = Writer.write parsed None
        Expect.equal (written.Contains("!<tag:foo> &a1 | # c")) true "Writer should emit root block-scalar metadata prefix and comment"
        let reparsed = Reader.read written
        Expect.equal reparsed parsed "Root block scalar metadata should survive read-write-read"

    testCase "Resolved tag round-trip stays semantically stable" <| fun _ ->
        let yaml = "foo: !!str bar"
        let parsed = Reader.read yaml
        let written = Writer.write parsed None
        Expect.equal (written.Contains("!<tag:yaml.org,2002:str>")) true "Writer should emit resolved tag as verbatim form"
        let reparsed = Reader.read written
        Expect.equal reparsed parsed "Resolved tags should survive read-write-read without mutation"

    testCase "Writer preserves key tag and anchor metadata in mapping emission" <| fun _ ->
        let ast =
            YAMLElement.Object [
                YAMLElement.Mapping(
                    YAMLContent.create("k", tag="tag:foo", anchor="a1"),
                    YAMLElement.Value(YAMLContent.create("v"))
                )
            ]
        let written = Writer.write ast None
        Expect.equal (written.Contains("!<tag:foo> &a1 k:")) true "Mapping key metadata should be emitted in scalar-value mapping branch"
        let reparsed = Reader.read written
        match reparsed with
        | YAMLElement.Object [YAMLElement.Mapping(key, YAMLElement.Object [YAMLElement.Value value])] ->
            Expect.equal key.Tag (Some "tag:foo") "Reparsed key should preserve tag metadata"
            Expect.equal key.Anchor (Some "a1") "Reparsed key should preserve anchor metadata"
            Expect.equal value.Value "v" "Reparsed value should be preserved"
        | _ ->
            failwithf "Unexpected reparsed AST shape: %A" reparsed

    testCase "Writer preserves mapping-key comments on non-inline mappings" <| fun _ ->
        let yaml = """a: # key
  b: c
"""
        let parsed = Reader.read yaml
        let written = Writer.write parsed None
        Expect.equal (written.Contains("a: # key")) true "Writer should emit key comments on mapping header lines"
        let reparsed = Reader.read written
        Expect.equal reparsed parsed "Key comments should survive read-write-read"

    testCase "Non-specific tag round-trip remains '!'" <| fun _ ->
        let yaml = "- ! 12"
        let parsed = Reader.read yaml
        let written = Writer.write parsed None
        let reparsed = Reader.read written
        Expect.equal reparsed parsed "Non-specific tag should not be rewritten to another tag kind"

    // ── Automatic plain-vs-double-quoted selection for style-less scalars ────────

    testCase "isPlainSafe classifies structurally unsafe strings" <| fun _ ->
        // safe — note ":x" is valid: a leading ':' followed by a non-space is a legal
        // plain-scalar first character, and YAMLicious only splits mappings on ": ".
        // "a#b" is safe because a '#' only starts a comment at the start or after whitespace.
        for s in [ "hello world"; "it's fine"; "-5"; "3:30"; "http://example.com/x"; ":x"; "a#b"; "tool.cwl#main" ] do
            Expect.isTrue (Writer.StyleVerifier.isPlainSafe Writer.StyleVerifier.PlainContext.Block s) $"'{s}' should be plain-safe"
        // unsafe: "x # y" would lose " y" as a comment.
        for s in [ ""; " padded"; "trailing "; "a: b"; "@id"; "*ref"; "- item"; "x # y"; "x\t# y"; "'q"; "\"q"; "-"; "line1\nline2" ] do
            Expect.isFalse (Writer.StyleVerifier.isPlainSafe Writer.StyleVerifier.PlainContext.Block s) $"'{s}' should be plain-unsafe"

    // yaml@2.6.1 reads k: <s> as the string s for each string in the first list below and k: [<s>] as the
    // single item s for each string in the second list. For the first list it reads k: [f(a, b)] as the
    // items "f(a" and "b)", and k: [a[0]], k: [a:]] and k: [x{y}] are parse errors.
    testCase "isPlainSafe applies the flow indicator rules only in Flow context" <| fun _ ->
        let block = Writer.StyleVerifier.PlainContext.Block
        let flow = Writer.StyleVerifier.PlainContext.Flow
        // "a:]" and "a:," are fine in block context, where only ": " and a trailing ':' end the scalar.
        for s in [ "f(a, b)"; "function f(a){ return a; }"; "a[0]"; "x{y}"; "a:]"; "a:," ] do
            Expect.isTrue (Writer.StyleVerifier.isPlainSafe block s) $"'{s}' should be plain-safe in block context"
            Expect.isFalse (Writer.StyleVerifier.isPlainSafe flow s) $"'{s}' should be plain-unsafe in flow context"
        for s in [ "abc"; "hello world"; "C#"; "tool.cwl#main"; "a:b"; "3:30"; ":x" ] do
            Expect.isTrue (Writer.StyleVerifier.isPlainSafe flow s) $"'{s}' should be plain-safe in flow context"
        // The leading-character, whitespace and ": " rules are the same in both contexts.
        for s in [ ""; " padded"; "a: b"; "x # y"; "@id"; "- item"; "line1\nline2" ] do
            Expect.isFalse (Writer.StyleVerifier.isPlainSafe flow s) $"'{s}' should be plain-unsafe in flow context"

    testCase "Style-less unsafe scalar auto-selects double-quoted and round-trips" <| fun _ ->
        // Every one of these has no explicit style (mirrors Encode.string) and is not a
        // valid plain scalar, so the writer must emit a double-quoted scalar.
        let cases =
            [ "a: b"; "@id"; "*ref"; "- item"; "x # y"; ""; " padded"; "trailing "; "'q"; "\"q"; "tab\there" ]
        for value in cases do
            let ast =
                YAMLElement.Object [
                    YAMLElement.Mapping(YAMLContent.create("k"), YAMLElement.Value (YAMLContent.create(value)))
                ]
            let written = Writer.write ast None
            Expect.isTrue (written.Contains("k: \"")) $"'{value}' should be emitted as a double-quoted scalar (got: {written})"

            match Reader.read written with
            | YAMLElement.Object [YAMLElement.Mapping(_, YAMLElement.Object [YAMLElement.Value v])]
            | YAMLElement.Object [YAMLElement.Mapping(_, YAMLElement.Value v)] ->
                Expect.equal v.Value value $"Round-trip should preserve the exact value for '{value}'"
                Expect.equal v.Style (Some ScalarStyle.DoubleQuoted) $"Reader should report double-quoted style for '{value}'"
            | other ->
                failwithf "Unexpected AST for '%s': %A" value other

    testCase "Style-less safe scalar stays plain and round-trips" <| fun _ ->
        let cases = [ "hello world"; "it's fine"; "-5"; "3:30"; "http://example.com/x"; ":x"; "a#b"; "tool.cwl#main" ]
        for value in cases do
            let ast =
                YAMLElement.Object [
                    YAMLElement.Mapping(YAMLContent.create("k"), YAMLElement.Value (YAMLContent.create(value)))
                ]
            let written = Writer.write ast None
            Expect.isFalse (written.Contains("\"")) $"'{value}' should stay unquoted (got: {written})"

            match Reader.read written with
            | YAMLElement.Object [YAMLElement.Mapping(_, YAMLElement.Object [YAMLElement.Value v])]
            | YAMLElement.Object [YAMLElement.Mapping(_, YAMLElement.Value v)] ->
                Expect.equal v.Value value $"Round-trip should preserve the exact value for '{value}'"
            | other ->
                failwithf "Unexpected AST for '%s': %A" value other

    testCase "Style-less multiline scalar still prefers a block scalar over double-quoting" <| fun _ ->
        let ast =
            YAMLElement.Object [
                YAMLElement.Mapping(YAMLContent.create("k"), YAMLElement.Value (YAMLContent.create("line1\nline2")))
            ]
        let written = Writer.write ast None
        Expect.isTrue (written.Contains("k: |-")) $"Multiline style-less value should use a block scalar (got: {written})"
        Expect.isFalse (written.Contains("\\n")) "Multiline value should not be collapsed into an escaped double-quoted scalar"

        match Reader.read written with
        | YAMLElement.Object [YAMLElement.Mapping(_, YAMLElement.Value v)] ->
            Expect.equal v.Value "line1\nline2" "Block-scalar round-trip should preserve the value"
        | other ->
            failwithf "Unexpected AST: %A" other

    testCase "Explicit Plain style is respected even when structurally unsafe" <| fun _ ->
        // Setting Plain explicitly is the caller's opt-out from automatic quoting.
        let ast =
            YAMLElement.Object [
                YAMLElement.Mapping(
                    YAMLContent.create("k"),
                    YAMLElement.Value (YAMLContent.create("a: b", style = ScalarStyle.Plain)))
            ]
        let written = Writer.write ast None
        Expect.isTrue (written.Contains("k: a: b")) $"Explicit Plain style should be emitted verbatim (got: {written})"
        Expect.isFalse (written.Contains("\"")) "Explicit Plain style should not be auto-quoted"

    testCase "Style-less unsafe scalars survive read-write-read unchanged" <| fun _ ->
        // Idempotency: quoting is stable across repeated round-trips.
        let ast =
            YAMLElement.Object [
                YAMLElement.Mapping(YAMLContent.create("k"), YAMLElement.Value (YAMLContent.create("a: b")))
            ]
        let written1 = Writer.write ast None
        let reparsed = Reader.read written1
        let written2 = Writer.write reparsed None
        Expect.equal written2 written1 "Second write should equal the first (idempotent quoting)"

    // ── Flow sequence items use the flow context of plain-scalar safety ─────────

    // yaml@2.6.1 reads k: ["x.replace(/#/g, \"_\"), y"] as the single item x.replace(/#/g, "_"), y.
    // The unquoted k: [x.replace(/#/g, "_"), y] is a parse error (Missing , or : between flow sequence items).
    testCase "Writer double-quotes a flow sequence item that contains commas and quotes" <| fun _ ->
        let value = "x.replace(/#/g, \"_\"), y"
        let ast =
            YAMLElement.Object [
                YAMLElement.Mapping(
                    YAMLContent.create("k"),
                    YAMLElement.Sequence [ YAMLElement.Value (YAMLContent.create(value)) ]
                )
            ]
        let written = Writer.write ast None
        Expect.trimEqual written "k: [\"x.replace(/#/g, \\\"_\\\"), y\"]" "The item should be double-quoted with escaped inner quotes"
        Expect.equal (expectSingleMappingSequenceValues (Reader.read written)) [ value ] "Reader should restore the exact item"

    // yaml@2.6.1 reads each quoted form below as one item with the original string. Unquoted,
    // k: [f(a, b)] gives the items "f(a" and "b)", k: [x: {y}] gives a nested mapping, and
    // k: [function f(a){ return a; }] and k: [a[0]] are parse errors.
    testCase "Writer double-quotes flow sequence items that contain flow indicators" <| fun _ ->
        let cases =
            [ "f(a, b)", "k: [\"f(a, b)\"]"
              "function f(a){ return a; }", "k: [\"function f(a){ return a; }\"]"
              "a[0]", "k: [\"a[0]\"]"
              "x: {y}", "k: [\"x: {y}\"]" ]
        for value, expected in cases do
            let ast =
                YAMLElement.Object [
                    YAMLElement.Mapping(
                        YAMLContent.create("k"),
                        YAMLElement.Sequence [ YAMLElement.Value (YAMLContent.create(value)) ]
                    )
                ]
            let written = Writer.write ast None
            Expect.trimEqual written expected $"'{value}' should be double-quoted in a flow sequence"
            Expect.equal (expectSingleMappingSequenceValues (Reader.read written)) [ value ] $"Reader should restore '{value}'"

    // yaml@2.6.1 reads each expected form below as the original string. The unquoted k: x: {y} is a parse error.
    testCase "Writer keeps the block rules for flow indicators in a mapping value" <| fun _ ->
        let cases =
            [ "f(a, b)", "k: f(a, b)"
              "function f(a){ return a; }", "k: function f(a){ return a; }"
              "a[0]", "k: a[0]"
              "x: {y}", "k: \"x: {y}\"" ]
        for value, expected in cases do
            let ast =
                YAMLElement.Object [
                    YAMLElement.Mapping(YAMLContent.create("k"), YAMLElement.Value (YAMLContent.create(value)))
                ]
            Expect.trimEqual (Writer.write ast None) expected $"'{value}' should follow the block rules as a mapping value"

    // yaml@2.6.1 reads k: [abc, C#, tool.cwl#main, a:b] as the four items abc, C#, tool.cwl#main and a:b.
    testCase "Writer keeps flow-safe flow sequence items plain" <| fun _ ->
        let values = [ "abc"; "C#"; "tool.cwl#main"; "a:b" ]
        let ast =
            YAMLElement.Object [
                YAMLElement.Mapping(
                    YAMLContent.create("k"),
                    YAMLElement.Sequence [ for value in values -> YAMLElement.Value (YAMLContent.create(value)) ]
                )
            ]
        let written = Writer.write ast None
        Expect.trimEqual written "k: [abc, C#, tool.cwl#main, a:b]" "Items without flow indicators should stay unquoted"
        Expect.equal (expectSingleMappingSequenceValues (Reader.read written)) values "Reader should restore all items"

    // yaml@2.6.1 reads `k: $(f(a,\n  "x")) # note` as {"k":"$(f(a, \"x\"))"} and rejects the comment behind
    // the first line, `k: $(f(a, # note\n  "x"))` (BAD_INDENT, MISSING_CHAR).
    testCase "Writer puts the comment of a multiline plain mapping value behind its last line" <| fun _ ->
        let value = "$(f(a,\n\"x\"))"
        let content = YAMLContent.create(value, comment=" note", style=ScalarStyle.Plain)
        let ast = YAMLElement.Object [ YAMLElement.Mapping(YAMLContent.create("k"), YAMLElement.Value content); textMapping "j" "x" ]
        let written = writeLf WriterOptions.Default ast
        Expect.equal written "k: $(f(a,\n  \"x\")) # note\nj: x\n" "The comment follows the last line and the next key stays a key"
        match Reader.read written with
        | YAMLElement.Object [ YAMLElement.Mapping(_, scalar); YAMLElement.Mapping(_, next) ] ->
            expectCommentOnLastSegment (plainSegments scalar) value
            Expect.equal (Decode.string next) "x" "The next key keeps its value"
        | other -> failwithf "Unexpected AST: %A" other

    // yaml@2.6.1 reads `- $(f(a,\n  "x")) # note` as ["$(f(a, \"x\"))"] and rejects the comment behind the
    // first line, `- $(f(a, # note\n  "x"))` (MISSING_CHAR, UNEXPECTED_TOKEN).
    testCase "Writer puts the comment of a multiline plain sequence item behind its last line" <| fun _ ->
        let value = "$(f(a,\n\"x\"))"
        let content = YAMLContent.create(value, comment=" note", style=ScalarStyle.Plain)
        let ast = YAMLElement.Object [ YAMLElement.Sequence [ YAMLElement.Object [ YAMLElement.Value content ]; textItem "x" ] ]
        let written = writeLf WriterOptions.Default ast
        Expect.equal written "- $(f(a,\n  \"x\")) # note\n- x\n" "The comment follows the last line and the next item stays an item"
        match Reader.read written with
        | YAMLElement.Object [ YAMLElement.Sequence [ scalar; next ] ] ->
            expectCommentOnLastSegment (plainSegments scalar) value
            Expect.equal (Decode.string next) "x" "The next item keeps its value"
        | other -> failwithf "Unexpected AST: %A" other

    // yaml@2.6.1 reads each written text back as the value, with or without the key or item that follows it.
    // The values are "a", "a\n", "a\n\n", "a\n\n\n" and "\n". The kept blank lines are the last lines of the text.
    testCase "Writer chomps a style-less multiline value by its trailing line breaks" <| fun _ ->
        expectFallbackChomping WriterOptions.Default "|"

    // yaml@2.6.1 reads the folded texts back as the same values as the literal ones above.
    testCase "Writer chomps a folded fallback by the trailing line breaks too" <| fun _ ->
        expectFallbackChomping { WriterOptions.Default with MultilineFallbackStyle = BlockScalarStyle.Folded } ">"

    // yaml@2.6.1 reads `k: |-\n    a\n` as {"k":"a"} and `k: |+\n    a\n` as {"k":"a\n"}.
    testCase "Writer keeps the chomping of an explicit block style that the value contradicts" <| fun _ ->
        let block (chomp: ChompingMode) (value: string) =
            YAMLElement.Object [
                YAMLElement.Mapping(
                    YAMLContent.create("k"),
                    YAMLElement.Value(YAMLContent.create(value, style=ScalarStyle.Block(BlockScalarStyle.Literal, chomp, None)))
                )
            ]
        let writtenStrip = writeLf WriterOptions.Default (block ChompingMode.Strip "a\n\n")
        Expect.equal writtenStrip "k: |-\n    a\n" "Strip stays although the value ends in two line breaks"
        let readStrip = expectSingleMappingValue (Reader.read writtenStrip)
        Expect.equal readStrip.Style (Some (ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Strip, None))) "The header is read as written"
        let writtenKeep = writeLf WriterOptions.Default (block ChompingMode.Keep "a")
        Expect.equal writtenKeep "k: |+\n    a\n" "Keep stays although the value has no line break"
        let readKeep = expectSingleMappingValue (Reader.read writtenKeep)
        Expect.equal readKeep.Style (Some (ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Keep, None))) "The header is read as written"
  ]
