module Tests.ScalarStyles

open Fable.Pyxpecto
open YAMLicious
open YAMLicious.YAMLiciousTypes
open Util

let private expectSingleMappingValue (ast: YAMLElement) =
    match ast with
    | YAMLElement.Object [YAMLElement.Mapping(_, YAMLElement.Value value)] -> value
    | _ -> failwithf "Unexpected AST: %A" ast

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

    testCase "Writer default fallback for style-less multiline uses |-" <| fun _ ->
        let ast =
            YAMLElement.Object [
                YAMLElement.Mapping(
                    YAMLContent.create("note"),
                    YAMLElement.Value (YAMLContent.create("line1\nline2\n"))
                )
            ]

        let actual = Writer.write ast None
        let expected = """note: |-
    line1
    line2"""
        Expect.trimEqual actual expected "Style-less multiline values should use default |- fallback"

        let parsed = Reader.read actual
        match parsed with
        | YAMLElement.Object [YAMLElement.Mapping(_, YAMLElement.Value value)] ->
            Expect.equal value.Value "line1\nline2" "Strip fallback should remove trailing newline"
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
  ]
