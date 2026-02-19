module Tests.BlockScalars

open Fable.Pyxpecto
open YAMLicious
open YAMLiciousTypes

let Main =
  testList "Block Scalars" [
    testCase "Explicit indentation preserves leading content spaces" <| fun _ ->
        let yaml = """- |1
  explicit
"""
        let expected = YAMLElement.Object [
            YAMLElement.Sequence [
                YAMLElement.Object [
                    YAMLElement.Value(
                        YAMLContent.create(
                            " explicit\n",
                            style=ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Clip, Some 1)
                        )
                    )
                ]
            ]
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Explicit indent indicator should preserve leading content spaces"

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

    testCase "Literal style preserves additional indentation" <| fun _ ->
        let yaml = """doc: |
  line1
    indented
  line3
"""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("doc"),
                YAMLElement.Value(
                    YAMLContent.create(
                        "line1\n  indented\nline3\n",
                        style=ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Clip, None)
                    )
                )
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Literal block should preserve relative indentation"

    testCase "Folded style preserves more-indented lines" <| fun _ ->
        let yaml = """folded: >
  Sammy Sosa completed another
  fine season with great stats.

    63 Home Runs
    0.288 Batting Average

  What a year!
"""
        let expectedValue = "Sammy Sosa completed another fine season with great stats.\n\n  63 Home Runs\n  0.288 Batting Average\n\nWhat a year!\n"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("folded"),
                YAMLElement.Value(
                    YAMLContent.create(
                        expectedValue,
                        style=ScalarStyle.Block(BlockScalarStyle.Folded, ChompingMode.Clip, None)
                    )
                )
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Folded block should preserve more-indented line breaks"

    testCase "Block scalar header supports inline comments" <| fun _ ->
        let yaml = """doc: | # comment
  hello
"""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("doc"),
                YAMLElement.Value(
                    YAMLContent.create(
                        "hello\n",
                        comment=" comment",
                        style=ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Clip, None)
                    )
                )
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Inline header comments should be parsed and preserved"

    testCase "Root-level folded scalar parses" <| fun _ ->
        let yaml = """>
 text line1
 text line2
"""
        let expected = YAMLElement.Object [
            YAMLElement.Value(
                YAMLContent.create(
                    "text line1 text line2\n",
                    style=ScalarStyle.Block(BlockScalarStyle.Folded, ChompingMode.Clip, None)
                )
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Root block scalar should parse as value node"

    testCase "Sequence item supports literal block scalar" <| fun _ ->
        let yaml = """- |
  text
"""
        let expected = YAMLElement.Object [
            YAMLElement.Sequence [
                YAMLElement.Object [
                    YAMLElement.Value(
                        YAMLContent.create(
                            "text\n",
                            style=ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Clip, None)
                        )
                    )
                ]
            ]
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Sequence block scalar should parse as a single scalar value item"

    testCase "Literal block scalar preserves trailing spaces" <| fun _ ->
        let yaml = """doc: |
  a 
  b  
"""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("doc"),
                YAMLElement.Value(
                    YAMLContent.create(
                        "a \nb  \n",
                        style=ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Clip, None)
                    )
                )
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Trailing spaces in literal block content should be preserved"

    testCase "Block scalar keeps quote delimiters exactly" <| fun _ ->
        let yaml = """doc: |
  sql: 'it''s'
  if x == "y"
"""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("doc"),
                YAMLElement.Value(
                    YAMLContent.create(
                        "sql: 'it''s'\nif x == \"y\"\n",
                        style=ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Clip, None)
                    )
                )
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Quoted delimiters inside block scalars should remain literal content"

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

    testCase "Block scalar with hash content" <| fun _ ->
        let yaml = """comment: |
  # Not a comment
  value"""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("comment"),
                YAMLElement.Value(YAMLContent.create("# Not a comment\nvalue\n", style=ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Clip, None)))
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Hash inside block scalar should be treated as content, not a comment"

    testCase "Combined explicit indent and strip chomping" <| fun _ ->
        let yaml = """data: |2-
  text
  more

"""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("data"),
                YAMLElement.Value(YAMLContent.create("text\nmore", style=ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Strip, Some 2)))
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Combined explicit indent and strip chomping should parse correctly"
  ]
