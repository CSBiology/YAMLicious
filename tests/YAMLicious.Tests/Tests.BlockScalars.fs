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
                YAMLElement.Value(YAMLContent.create("explicit\n"))
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
                YAMLElement.Value(YAMLContent.create("text"))
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
                YAMLElement.Value(YAMLContent.create("text\n\n"))
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
                YAMLElement.Value(YAMLContent.create("line 1 line 2\n"))
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Should fold newlines to spaces"
  ]
