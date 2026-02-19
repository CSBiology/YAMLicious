module Tests.BlockScalars

open Fable.Pyxpecto
open YAMLicious
open YAMLiciousTypes

let Main =
  testList "Block Scalars" [
    testCase "Explicit Indentation Indicator" <| fun _ ->
        let yaml = """doc: |2
  explicit
"""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("doc"),
                // Since base indent is 2, content "  explicit" indented by 2 -> "explicit"
                // But wait, the standard usually implies relative indentation.
                // In YAMLicious Preprocessing, indentation is handled by Level parsing.
                // If we use |2, we tell the parser the indentation level.
                // NOTE: Current YAMLicious preprocessing separates indentation before Reader sees it.
                // We test if Reader accepts the header "|2".
                YAMLElement.Value(YAMLContent.create("explicit\n", style=ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Clip, Some 2)))
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Should parse explicit indentation header"

    testCase "Chomping Strip" <| fun _ ->
        let yaml = """strip: |-
  text
"""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("strip"),
                YAMLElement.Value(YAMLContent.create("text", style=ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Strip, None)))
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Should strip trailing newline"

    testCase "Chomping Keep" <| fun _ ->
        let yaml = """keep: |+
  text

"""
        let expected = YAMLElement.Object [
             YAMLElement.Mapping(
                YAMLContent.create("keep"),
                // "text\n" (from text line) + "\n" (empty line) -> "text\n\n"
                YAMLElement.Value(YAMLContent.create("text\n\n", style=ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Keep, None)))
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Should keep all trailing newlines"

    testCase "Folded Style" <| fun _ ->
        let yaml = """folded: >
  line 1
  line 2
"""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("folded"),
                YAMLElement.Value(YAMLContent.create("line 1 line 2\n", style=ScalarStyle.Block(BlockScalarStyle.Folded, ChompingMode.Clip, None)))
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Should fold newlines to spaces"

    testCase "Block scalar keeps quote delimiters in placeholders" <| fun _ ->
        let yaml = """expression: >
  ${ return (function() {
    var allowedFields = ['class', 'basename'];
    if (sanitized.basename === "arc") return null;
    if (sanitized.class === "Directory") return null;
  })(); }
"""
        let actual = Reader.read yaml
        match actual with
        | YAMLElement.Object [YAMLElement.Mapping(_, YAMLElement.Value content)] ->
            Expect.equal (content.Value.Contains("'class'")) true "Should preserve single-quoted strings in block scalars"
            Expect.equal (content.Value.Contains("\"arc\"")) true "Should preserve double-quoted strings in block scalars"
            Expect.equal (content.Value.Contains("\"Directory\"")) true "Should preserve all double-quoted strings in block scalars"
        | _ ->
            failwithf "Unexpected AST: %A" actual
  ]
