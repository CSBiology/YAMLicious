module Tests.BlockScalars

open Fable.Pyxpecto
open YAMLicious
open YAMLiciousTypes

let private blockScalar (style: BlockScalarStyle) (chomp: ChompingMode) (indent: int option) (value: string) =
    YAMLElement.Value(YAMLContent.create(value, style=ScalarStyle.Block(style, chomp, indent)))

let private literalBlock (value: string) =
    blockScalar BlockScalarStyle.Literal ChompingMode.Clip None value

let private plainNode (value: string) =
    YAMLElement.Object [YAMLElement.Value(YAMLContent.create(value))]

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

    testCase "Block scalar preserves leading blank content line" <| fun _ ->
        let yaml = """doc: |

  hello
"""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("doc"),
                YAMLElement.Value(
                    YAMLContent.create(
                        "\nhello\n",
                        style=ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Clip, None)
                    )
                )
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Leading blank lines after a block scalar header should stay as scalar content"

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

    testCase "Block scalar preserves final hash content before dedent" <| fun _ ->
        let yaml = """doc: |
  Workflow doc line.
  # This line is doc text, not a YAML comment.
next: value"""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("doc"),
                YAMLElement.Value(YAMLContent.create("Workflow doc line.\n# This line is doc text, not a YAML comment.\n", style=ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Clip, None)))
            )
            YAMLElement.Mapping(
                YAMLContent.create("next"),
                YAMLElement.Object [
                    YAMLElement.Value(YAMLContent.create("value"))
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Indented hash content should stay in the block scalar even when the following line dedents."

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

    testCase "Literal block scalar keeps the exact indentation of a more indented line" <| fun _ ->
        let yaml = "key: |\n    text\n     more\n"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(YAMLContent.create("key"), literalBlock "text\n more\n")
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "One extra space in the source should stay one extra space in the value"

    testCase "Literal block scalar keeps a more indented hash line as content" <| fun _ ->
        let yaml = "key: |\n    text\n     # more\n"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(YAMLContent.create("key"), literalBlock "text\n # more\n")
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "A hash line inside the block is content and keeps its extra space"

    testCase "Literal block scalar keeps two spaces before a deeper hash line" <| fun _ ->
        let yaml = "key: |\n    text\n      # two\n"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(YAMLContent.create("key"), literalBlock "text\n  # two\n")
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Two extra spaces in the source should stay two extra spaces in the value"

    testCase "Folded block scalar keeps the line breaks around a more indented line" <| fun _ ->
        let yaml = "key: >\n    text\n     more\n    end\n"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(YAMLContent.create("key"), blockScalar BlockScalarStyle.Folded ChompingMode.Clip None "text\n more\nend\n")
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "A more indented line is not folded and keeps its extra space"

    testCase "Literal block scalar keeps mixed indentation depths with hash lines" <| fun _ ->
        let yaml = "key: |\n    a\n     # b\n         c\n       # d\n    e\n"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(YAMLContent.create("key"), literalBlock "a\n # b\n     c\n   # d\ne\n")
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Every line should keep its indentation relative to the least indented line"

    testCase "Nested mapping block scalar keeps indentation and the next key" <| fun _ ->
        let yaml = "outer:\n  key: |\n    x\n     y\n  next: 1\n"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("outer"),
                YAMLElement.Object [
                    YAMLElement.Mapping(YAMLContent.create("key"), literalBlock "x\n y\n")
                    YAMLElement.Mapping(YAMLContent.create("next"), YAMLElement.Object [YAMLElement.Value(YAMLContent.create("1"))])
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "A nested block scalar should end at the dedented sibling key"

    testCase "Root block scalar keeps the exact indentation of a more indented line" <| fun _ ->
        let yaml = "|\n  x\n   y\n"
        let expected = YAMLElement.Object [literalBlock "x\n y\n"]
        let actual = Reader.read yaml
        Expect.equal actual expected "A root block scalar should keep one extra space"

    testCase "First sequence item block scalar keeps indentation and the next item" <| fun _ ->
        let yaml = "- |\n  x\n   y\n- z\n"
        let expected = YAMLElement.Object [
            YAMLElement.Sequence [
                YAMLElement.Object [literalBlock "x\n y\n"]
                YAMLElement.Object [YAMLElement.Value(YAMLContent.create("z"))]
            ]
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "The first item should keep one extra space and the second item should stay a plain value"

    testCase "Later sequence item block scalars keep their indentation" <| fun _ ->
        let yaml = "- a\n- |\n  x\n   y\n- b\n"
        let expected = YAMLElement.Object [
            YAMLElement.Sequence [
                YAMLElement.Object [YAMLElement.Value(YAMLContent.create("a"))]
                YAMLElement.Object [literalBlock "x\n y\n"]
                YAMLElement.Object [YAMLElement.Value(YAMLContent.create("b"))]
            ]
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "A block scalar after the first item should read as one scalar"

    testCase "Indentless sequence under a key keeps block scalar indentation" <| fun _ ->
        let yaml = "key:\n- |\n  x\n   y\n- z\n"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("key"),
                YAMLElement.Object [
                    YAMLElement.Sequence [
                        YAMLElement.Object [literalBlock "x\n y\n"]
                        YAMLElement.Object [YAMLElement.Value(YAMLContent.create("z"))]
                    ]
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "The first item of an indentless sequence should read as one scalar"

    testCase "Explicit indentation indicator keeps spaces beyond the indicated baseline" <| fun _ ->
        let yaml = "key: |2\n     x\n   y\n"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("key"),
                blockScalar BlockScalarStyle.Literal ChompingMode.Clip (Some 2) "   x\n y\n"
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "The indicator sets the baseline even when the first line is deeper than a later line"

    testCase "Keep chomping retains trailing blank lines before a dedented sibling" <| fun _ ->
        let yaml = "a: |+\n  x\n\n\nb: 1\n"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(YAMLContent.create("a"), blockScalar BlockScalarStyle.Literal ChompingMode.Keep None "x\n\n\n")
            YAMLElement.Mapping(YAMLContent.create("b"), YAMLElement.Object [YAMLElement.Value(YAMLContent.create("1"))])
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Both trailing blank lines belong to the scalar and the sibling key stays separate"

    testCase "Keep chomping retains trailing blank lines of a nested scalar" <| fun _ ->
        let yaml = "outer:\n  key: |+\n    x\n\nnext: 1\n"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("outer"),
                YAMLElement.Object [
                    YAMLElement.Mapping(YAMLContent.create("key"), blockScalar BlockScalarStyle.Literal ChompingMode.Keep None "x\n\n")
                ]
            )
            YAMLElement.Mapping(YAMLContent.create("next"), YAMLElement.Object [YAMLElement.Value(YAMLContent.create("1"))])
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "A blank line before a dedent to an outer key still belongs to the nested scalar"

    testCase "Header-looking body text stays literal content" <| fun _ ->
        let yaml = "key: |\n  a: |\n    b\n  - c\n"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(YAMLContent.create("key"), literalBlock "a: |\n  b\n- c\n")
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Text that looks like a header or a sequence item inside a body is not interpreted"

    testCase "Preprocessing keeps a block scalar body flat with only the header indentation removed" <| fun _ ->
        let yaml = "outer:\n  key: |\n    text\n     # more\n       deeper\n  next: 1\n"
        let expected =
            Level [
                Line "outer:"
                Intendation [
                    Line "key: |"
                    Intendation [Line "  text"; Line "   # more"; Line "     deeper"]
                    Line "next: 1"
                ]
            ]
        let actual = Preprocessing.read yaml
        Expect.equal actual.AST expected "The body should be one Intendation of lines without nested blocks"

    testCase "Comment less indented than the content ends a nested block scalar" <| fun _ ->
        // yaml@2.6.1: {"a":{"b":"x\n","d":1}}
        let yaml = "a:\n  b: |\n      x\n    # c\n  d: 1\n"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("a"),
                YAMLElement.Object [
                    YAMLElement.Mapping(YAMLContent.create("b"), literalBlock "x\n")
                    YAMLElement.Comment(" c")
                    YAMLElement.Mapping(YAMLContent.create("d"), plainNode "1")
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "A comment between the header and the content indentation is a comment, not content"

    testCase "Comment less indented than an explicit indicator ends the block scalar" <| fun _ ->
        // yaml@2.6.1: {"a":"  x\n","b":1}
        let yaml = "a: |2\n    x\n # c\nb: 1\n"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(YAMLContent.create("a"), blockScalar BlockScalarStyle.Literal ChompingMode.Clip (Some 2) "  x\n")
            YAMLElement.Comment(" c")
            YAMLElement.Mapping(YAMLContent.create("b"), plainNode "1")
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "The indicator sets the content indentation, so a comment at one space ends the scalar"

    testCase "Comment less indented than the content ends a sequence item block scalar" <| fun _ ->
        // yaml@2.6.1: ["x\n","y"]
        let yaml = "- |\n    x\n  # c\n- y\n"
        let expected = YAMLElement.Object [
            YAMLElement.Sequence [
                YAMLElement.Object [literalBlock "x\n"]
                YAMLElement.Comment(" c")
                plainNode "y"
            ]
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "The comment ends the scalar and stays between the sequence items"

    testCase "Comment less indented than the content ends a folded block scalar" <| fun _ ->
        // yaml@2.6.1: {"key":"folded\n","next":1}
        let yaml = "key: >\n    folded\n  # c\nnext: 1\n"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(YAMLContent.create("key"), blockScalar BlockScalarStyle.Folded ChompingMode.Clip None "folded\n")
            YAMLElement.Comment(" c")
            YAMLElement.Mapping(YAMLContent.create("next"), plainNode "1")
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "The content indentation is the one of the first line, not of the header"

    testCase "Trailing comment between the header and the content indentation ends the block scalar" <| fun _ ->
        // yaml@2.6.1: {"x":{"a":"body\n"}} and {"a":"x\n"}
        let nested = "x:\n  a: |\n      body\n    # c\n"
        let expectedNested = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("x"),
                YAMLElement.Object [
                    YAMLElement.Mapping(YAMLContent.create("a"), literalBlock "body\n")
                    YAMLElement.Comment(" c")
                ]
            )
        ]
        Expect.equal (Reader.read nested) expectedNested "A trailing comment stays in the block of the header"
        let root = "a: |\n    x\n  # c\n"
        let expectedRoot = YAMLElement.Object [
            YAMLElement.Mapping(YAMLContent.create("a"), literalBlock "x\n")
            YAMLElement.Comment(" c")
        ]
        Expect.equal (Reader.read root) expectedRoot "A trailing comment must not be read as scalar content"

    testCase "Keep chomping stops before a comment that is less indented than the content" <| fun _ ->
        // yaml@2.6.1: {"a":"x\n\n","b":1}
        let yaml = "a: |+\n    x\n\n  # c\nb: 1\n"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(YAMLContent.create("a"), blockScalar BlockScalarStyle.Literal ChompingMode.Keep None "x\n\n")
            YAMLElement.Comment(" c")
            YAMLElement.Mapping(YAMLContent.create("b"), plainNode "1")
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "The blank line before the comment belongs to the scalar and the comment does not"
  ]
