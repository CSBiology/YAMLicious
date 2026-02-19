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
  ]
