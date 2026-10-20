module Tests.YamlRead

open Fable.Pyxpecto
open YAMLicious
open YAMLiciousTypes
open Preprocessing
open YAMLiciousTypes

let private hasAnyComment (element: YAMLElement) =
    let rec loop (el: YAMLElement) =
        match el with
        | YAMLElement.Comment _ -> true
        | YAMLElement.Value content -> content.Comment.IsSome
        | YAMLElement.Mapping (key, value) -> key.Comment.IsSome || loop value
        | YAMLElement.Object items
        | YAMLElement.Sequence items -> List.exists loop items
        | _ -> false
    loop element

let private plainMapping (key: string) (value: string) =
    YAMLElement.Object [
        YAMLElement.Mapping(YAMLContent.create(key), YAMLElement.Object [YAMLElement.Value(YAMLContent.create(value))])
    ]

let private scalarNode (value: string) =
    YAMLElement.Object [ YAMLElement.Value(YAMLContent.create(value)) ]

let private sequenceNode (items: YAMLElement list) =
    YAMLElement.Object [ YAMLElement.Sequence items ]

let private entry (key: string) (value: YAMLElement) =
    YAMLElement.Mapping(YAMLContent.create(key), value)

let Main = testList "YamlRead" [
    testCase "Value" <| fun _ ->
        let yaml = "Hello World"
        let expected = YAMLElement.Object [YAMLElement.Value(YAMLContent.create("Hello World"))]
        let actual = Reader.read yaml
        Expect.equal actual expected ""

    testCase "KeyValue" <| fun _ ->
        let yaml = "Say: Hello World"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(YAMLContent.create("Say"), 
                YAMLElement.Object [
                    YAMLElement.Value(YAMLContent.create("Hello World"))
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected ""

    testCase "KeyValue + Comment" <| fun _ ->
        let yaml = "Say: Hello World # 420 blaze it"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("Say"), 
                YAMLElement.Object [
                    YAMLElement.Value(YAMLContent.create("Hello World", " 420 blaze it"))
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected ""

    testCase "KeyValue InlineSequence" <| fun _ ->
        let yaml = "Say: [Hello, World]"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("Say"),
                YAMLElement.Object [
                    YAMLElement.Sequence[
                        YAMLElement.Object [
                            YAMLElement.Value(YAMLContent.create("Hello"));
                        ]
                        YAMLElement.Object [
                            YAMLElement.Value(YAMLContent.create("World"))
                        ]
                    ]
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected ""

    testCase "KeyValue InlineSequence + Comment" <| fun _ ->
        let yaml = "Say: [Hello, World] # 420 blaze it"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("Say"),
                YAMLElement.Object [
                    YAMLElement.Comment(" 420 blaze it");
                    YAMLElement.Sequence[
                        YAMLElement.Object [
                            YAMLElement.Value(YAMLContent.create("Hello"));
                        ]
                        YAMLElement.Object [
                            YAMLElement.Value(YAMLContent.create("World"))
                        ]
                    ]
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected ""

    testCase "KeyValue empty inline sequence stays empty sequence" <| fun _ ->
        let yaml = "value: []"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("value"),
                YAMLElement.Object [
                    YAMLElement.Sequence []
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Empty inline sequence should not decode as an empty object."

    testCase "KeyValue empty inline sequence roundtrips as brackets" <| fun _ ->
        let yaml = "value: []"
        let actual =
            yaml
            |> Reader.read
            |> fun element -> Writer.write element None
        Expect.equal (actual.Trim()) yaml "Empty inline sequence should be preserved for roundtrip YAML output."

    testCase "Single-quoted string" <| fun _ ->
        let yaml = "single: 'hello world'"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("single"),
                YAMLElement.Object [
                    YAMLElement.Value(YAMLContent.create("hello world", style=ScalarStyle.SingleQuoted))
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected ""

    testCase "Single-quoted with escaped quote" <| fun _ ->
        let yaml = "single: 'here''s to quotes'"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("single"),
                YAMLElement.Object [
                    YAMLElement.Value(YAMLContent.create("here's to quotes", style=ScalarStyle.SingleQuoted))
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected ""

    testCase "Single-quoted preserves backslashes" <| fun _ ->
        let yaml = "tie-fighter: '|\\-*-/|'"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("tie-fighter"),
                YAMLElement.Object [
                    YAMLElement.Value(YAMLContent.create("|\\-*-/|", style=ScalarStyle.SingleQuoted))
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected ""

    testCase "Double-quoted escape: newline" <| fun _ ->
        let yaml = "key: \"line1\\nline2\""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("key"),
                YAMLElement.Object [
                    YAMLElement.Value(YAMLContent.create("line1\nline2", style=ScalarStyle.DoubleQuoted))
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected ""

    testCase "Double-quoted escape: tab" <| fun _ ->
        let yaml = "key: \"before\\tafter\""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("key"),
                YAMLElement.Object [
                    YAMLElement.Value(YAMLContent.create("before\tafter", style=ScalarStyle.DoubleQuoted))
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected ""

    testCase "Double-quoted escape: backslash" <| fun _ ->
        let yaml = "key: \"path\\\\to\\\\file\""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("key"),
                YAMLElement.Object [
                    YAMLElement.Value(YAMLContent.create("path\\to\\file", style=ScalarStyle.DoubleQuoted))
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected ""

    testCase "Double-quoted escape: escaped quote" <| fun _ ->
        let yaml = "key: \"a \\\"b\\\" c\""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("key"),
                YAMLElement.Object [
                    YAMLElement.Value(YAMLContent.create("a \"b\" c", style=ScalarStyle.DoubleQuoted))
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected ""

    testCase "Double-quoted escape: hex unicode" <| fun _ ->
        let yaml = "key: \"\\x41\\x42\\x43\""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("key"),
                YAMLElement.Object [
                    YAMLElement.Value(YAMLContent.create("ABC", style=ScalarStyle.DoubleQuoted))
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected ""

    testCase "Double-quoted escape: unicode 16-bit" <| fun _ ->
        let yaml = "key: \"\\u263A\""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("key"),
                YAMLElement.Object [
                    YAMLElement.Value(YAMLContent.create("☺", style=ScalarStyle.DoubleQuoted))
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected ""

    testCase "Double-quoted escape: null character" <| fun _ ->
        let yaml = "key: \"before\\0after\""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("key"),
                YAMLElement.Object [
                    YAMLElement.Value(YAMLContent.create("before\u0000after", style=ScalarStyle.DoubleQuoted))
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected ""

    testCase "Double-quoted mapping key" <| fun _ ->
        let yaml = "\"@id\": MyIdentifier"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("@id", style=ScalarStyle.DoubleQuoted),
                YAMLElement.Object [
                    YAMLElement.Value(YAMLContent.create("MyIdentifier"))
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Quoted mapping keys should restore placeholder content and preserve style"

    testCase "Double-quoted string with embedded single quotes stays intact" <| fun _ ->
        let yaml = "key: \"a 'b' c\""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("key"),
                YAMLElement.Object [
                    YAMLElement.Value(YAMLContent.create("a 'b' c", style=ScalarStyle.DoubleQuoted))
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Single quotes inside double-quoted scalar should not be placeholder-leaked"

    testCase "Plain scalar with embedded single quotes stays intact" <| fun _ ->
        let yaml = "key: rock 'n' roll"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("key"),
                YAMLElement.Object [
                    YAMLElement.Value(YAMLContent.create("rock 'n' roll"))
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Single quotes in plain scalars should remain literal content"

    testCase "Plain scalar with embedded double quotes stays intact" <| fun _ ->
        let yaml = "key: he said \"hi\""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("key"),
                YAMLElement.Object [
                    YAMLElement.Value(YAMLContent.create("he said \"hi\""))
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Double quotes in plain scalars should remain literal content"

    testCase "Sequence" <| fun _ ->
        let yaml = """
- My Value 1
- My Value 2
- My Value 3
"""
        let expected = YAMLElement.Object [
            YAMLElement.Sequence[
                YAMLElement.Object [
                    YAMLElement.Value(YAMLContent.create("My Value 1"));
                ]
                YAMLElement.Object [
                    YAMLElement.Value(YAMLContent.create("My Value 2"));
                ]
                YAMLElement.Object [
                    YAMLElement.Value(YAMLContent.create("My Value 3"))
                ]
            ]
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected ""

    testCase "SequenceObjects" <| fun _ ->
        let yaml = """
- My Value 1
  My Value 2
- My Value 3
"""
        let expected = YAMLElement.Object [
            YAMLElement.Sequence[
                YAMLElement.Object [
                    YAMLElement.Value (YAMLContent.create("My Value 1\nMy Value 2", style=ScalarStyle.Plain))
                ];
                YAMLElement.Object [
                    YAMLElement.Value(YAMLContent.create("My Value 3"))
                ]
            ]
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected ""

    testCase "SequenceImplicit" <| fun _ ->
        let yaml = """
My Key:
  My Value1
  My Value2
  My Value3
"""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("My Key"),
                YAMLElement.Object [
                    YAMLElement.Value(YAMLContent.create("My Value1\nMy Value2\nMy Value3", style=ScalarStyle.Plain))
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected ""

    testCase "KeyValue plain continuation folds into one scalar" <| fun _ ->
        let yaml = """
My Key: My Value1
  My Value2
"""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("My Key"),
                YAMLElement.Object [
                    YAMLElement.Value(YAMLContent.create("My Value1\nMy Value2", style=ScalarStyle.Plain))
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected ""

    testCase "Indented multiline plain scalar expression decodes as one string" <| fun _ ->
        let yaml = """valueFrom:
  ${
    var reads = null;
    if (self !== null) { reads = [self]; }
    return reads;
  }"""
        let decoded =
            YAMLicious.Decode.read yaml
            |> YAMLicious.Decode.object (fun get ->
                get.Required.Field "valueFrom" YAMLicious.Decode.string
            )

        Expect.isTrue (decoded.Contains("${")) "The opening expression marker should be part of the scalar string."
        Expect.isTrue (decoded.Contains("return reads;")) "The return statement should be part of the scalar string."
        Expect.isTrue (decoded.Contains("}")) "The closing brace should be part of the scalar string."

    testCase "Inline-start multiline plain scalar expression decodes as one string" <| fun _ ->
        let yaml = """valueFrom: ${
  var reads = null;
  return reads;
}"""
        let decoded =
            YAMLicious.Decode.read yaml
            |> YAMLicious.Decode.object (fun get ->
                get.Required.Field "valueFrom" YAMLicious.Decode.string
            )

        Expect.isTrue (decoded.Contains("${")) "The opening expression marker should be part of the scalar string."
        Expect.isTrue (decoded.Contains("return reads;")) "The return statement should be part of the scalar string."
        Expect.isTrue (decoded.Contains("}")) "The closing brace should be part of the scalar string."

    testCase "Root plain continuation folds into one scalar" <| fun _ ->
        let yaml = """
My Value1
  My Value2
"""
        let expected =
            YAMLElement.Object [
                YAMLElement.Value(YAMLContent.create("My Value1\nMy Value2", style=ScalarStyle.Plain))
            ]
        let actual = Reader.read yaml
        Expect.equal actual expected ""

    testCase "Nested mapping accepts structural blank line before child block" <| fun _ ->
        let yaml = """inputs:

  a:
    type: string
  b:
    type: string
"""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("inputs"),
                YAMLElement.Object [
                    YAMLElement.Mapping(
                        YAMLContent.create("a"),
                        YAMLElement.Object [
                            YAMLElement.Mapping(
                                YAMLContent.create("type"),
                                YAMLElement.Object [
                                    YAMLElement.Value(YAMLContent.create("string"))
                                ]
                            )
                        ]
                    )
                    YAMLElement.Mapping(
                        YAMLContent.create("b"),
                        YAMLElement.Object [
                            YAMLElement.Mapping(
                                YAMLContent.create("type"),
                                YAMLElement.Object [
                                    YAMLElement.Value(YAMLContent.create("string"))
                                ]
                            )
                        ]
                    )
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Blank lines before a nested mapping should stay inside the child block"

    testCase "Nested mapping accepts structural blank line and comment before child block" <| fun _ ->
        let yaml = """inputs:

  # comment
  a: 1
"""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("inputs"),
                YAMLElement.Object [
                    YAMLElement.Comment(" comment")
                    YAMLElement.Mapping(
                        YAMLContent.create("a"),
                        YAMLElement.Object [
                            YAMLElement.Value(YAMLContent.create("1"))
                        ]
                    )
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Blank lines and comments before a nested mapping should be accepted"

    testCase "Nested mapping keeps blank lines between sibling entries" <| fun _ ->
        let yaml = """inputs:
  a: 1

  b: 2
"""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("inputs"),
                YAMLElement.Object [
                    YAMLElement.Mapping(
                        YAMLContent.create("a"),
                        YAMLElement.Object [
                            YAMLElement.Value(YAMLContent.create("1"))
                        ]
                    )
                    YAMLElement.Mapping(
                        YAMLContent.create("b"),
                        YAMLElement.Object [
                            YAMLElement.Value(YAMLContent.create("2"))
                        ]
                    )
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Blank sibling separators inside an open child mapping should remain harmless"

    testCase "Lower-indented comment between nested mapping entries stays in open block" <| fun _ ->
        let yaml = """inputs:
  a: string
# group comment
  b: string"""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("inputs"),
                YAMLElement.Object [
                    YAMLElement.Mapping(
                        YAMLContent.create("a"),
                        YAMLElement.Object [
                            YAMLElement.Value(YAMLContent.create("string"))
                        ]
                    )
                    YAMLElement.Comment(" group comment")
                    YAMLElement.Mapping(
                        YAMLContent.create("b"),
                        YAMLElement.Object [
                            YAMLElement.Value(YAMLContent.create("string"))
                        ]
                    )
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "A lower-indented full-line comment should not close the inputs mapping."

    testCase "Lower-indented comment before nested sequence stays in open block" <| fun _ ->
        let yaml = """inputs:
# group comment
  - id: a
    type: string"""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("inputs"),
                YAMLElement.Object [
                    YAMLElement.Comment(" group comment")
                    YAMLElement.Sequence [
                        YAMLElement.Object [
                            YAMLElement.Mapping(
                                YAMLContent.create("id"),
                                YAMLElement.Object [
                                    YAMLElement.Value(YAMLContent.create("a"))
                                ]
                            )
                            YAMLElement.Mapping(
                                YAMLContent.create("type"),
                                YAMLElement.Object [
                                    YAMLElement.Value(YAMLContent.create("string"))
                                ]
                            )
                        ]
                    ]
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "A lower-indented full-line comment should not detach the sequence value from inputs."

    testCase "Lower-indented comment between nested workflow steps stays in open block" <| fun _ ->
        let yaml = """steps:
  first:
    run: tool.cwl
    in: []
    out: []
# separator
  second:
    run: tool.cwl
    in: []
    out: []"""
        let emptySequence = YAMLElement.Object [YAMLElement.Sequence []]
        let step name = 
            YAMLElement.Mapping(
                YAMLContent.create(name),
                YAMLElement.Object [
                    YAMLElement.Mapping(
                        YAMLContent.create("run"),
                        YAMLElement.Object [YAMLElement.Value(YAMLContent.create("tool.cwl"))]
                    )
                    YAMLElement.Mapping(YAMLContent.create("in"), emptySequence)
                    YAMLElement.Mapping(YAMLContent.create("out"), emptySequence)
                ]
            )
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("steps"),
                YAMLElement.Object [
                    step "first"
                    YAMLElement.Comment(" separator")
                    step "second"
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "A lower-indented full-line comment should not close the steps mapping."

    testCase "Blank line before lower-indented comment keeps following entry in open block" <| fun _ ->
        let yaml = """steps:
  first:
    run: tool.cwl

# separator
  second:
    run: tool.cwl"""
        let step name =
            YAMLElement.Mapping(
                YAMLContent.create(name),
                YAMLElement.Object [
                    YAMLElement.Mapping(
                        YAMLContent.create("run"),
                        YAMLElement.Object [YAMLElement.Value(YAMLContent.create("tool.cwl"))]
                    )
                ]
            )
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("steps"),
                YAMLElement.Object [
                    step "first"
                    YAMLElement.Comment(" separator")
                    step "second"
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Blank lines before lower-indented comments should not close an open block."

    testCase "Blank line before indented commented block does not create orphan indentation" <| fun _ ->
        let yaml = """steps:
  first:
    run: tool.cwl
# disabled section

  # disabled:
  #   run: disabled.cwl
# end

meta: value"""
        let actual = Reader.read yaml
        match actual with
        | YAMLElement.Object elements ->
            let hasMapping key =
                elements
                |> List.exists (function
                    | YAMLElement.Mapping(k, _) -> k.Value = key
                    | _ -> false)
            Expect.isTrue (hasMapping "steps") "steps mapping should decode."
            Expect.isTrue (hasMapping "meta") "meta mapping should decode after commented-out block."
        | other -> failwithf "Expected object, got %A" other

    testCase "A blank line behind a key with a trailing comment opens the block of the key" <| fun _ ->
        // yaml@2.6.1: {"a":{"b":1}}, {"x":1,"a":{"b":1},"z":2} and {"a":[1]}
        let keyWithComment = YAMLContent.create("a", comment = " c")
        let value (text: string) = YAMLElement.Object [YAMLElement.Value(YAMLContent.create(text))]
        let mapping (key: string) (child: YAMLElement) = YAMLElement.Mapping(YAMLContent.create(key), child)
        let nested = YAMLElement.Object [mapping "b" (value "1")]
        Expect.equal
            (Reader.read "a: # c\n\n  b: 1")
            (YAMLElement.Object [YAMLElement.Mapping(keyWithComment, nested)])
            "A mapping behind the blank line"
        Expect.equal
            (Reader.read "x: 1\na: # c\n\n  b: 1\nz: 2")
            (YAMLElement.Object [mapping "x" (value "1"); YAMLElement.Mapping(keyWithComment, nested); mapping "z" (value "2")])
            "A mapping between two siblings"
        Expect.equal
            (Reader.read "a: # c\n\n  - 1")
            (YAMLElement.Object [YAMLElement.Mapping(keyWithComment, YAMLElement.Object [YAMLElement.Sequence [value "1"]])])
            "A sequence behind the blank line"

    testCase "SequenceSameIndentAsMapping" <| fun _ ->
        let yaml = """
My Key:
- My Value1
- My Value2
- My Value3
"""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("My Key"),
                YAMLElement.Object [
                    YAMLElement.Sequence[
                        YAMLElement.Object [
                            YAMLElement.Value(YAMLContent.create("My Value1"))
                        ]
                        YAMLElement.Object [
                            YAMLElement.Value(YAMLContent.create("My Value2"))
                        ]
                        YAMLElement.Object [
                            YAMLElement.Value(YAMLContent.create("My Value3"))
                        ]
                    ]
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected ""

    testCase "SequenceSameIndentAsMappingAndComment" <| fun _ ->
        let yaml = """
My Key:
- My Value1
- My Value2
#ich hab das gerade gesehen und dachte mir "ah fuck"
- My Value3
"""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("My Key"),
                YAMLElement.Object [
                    YAMLElement.Sequence[
                        YAMLElement.Object [
                            YAMLElement.Value(YAMLContent.create("My Value1"))
                        ]
                        YAMLElement.Object [
                            YAMLElement.Value(YAMLContent.create("My Value2"))
                        ]
                        YAMLElement.Comment("ich hab das gerade gesehen und dachte mir \"ah fuck\"")
                        YAMLElement.Object [
                            YAMLElement.Value(YAMLContent.create("My Value3"))
                        ]
                    ]
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected ""

    testCase "SequenceSameIndentAsMapping2" <| fun _ ->
        let yaml = """
My Key:
- My Value1
  My Value2
- My Value3
"""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("My Key"),
                YAMLElement.Object [
                    YAMLElement.Sequence[
                        YAMLElement.Object [
                            YAMLElement.Value(YAMLContent.create("My Value1"));
                            YAMLElement.Value(YAMLContent.create("My Value2"))
                        ]
                        YAMLElement.Object [
                            YAMLElement.Value(YAMLContent.create("My Value3"))
                        ]
                    ]
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected ""

    testCase "Key followed by full-line comment and unindented sequence" <| fun _ ->
        let yaml = "inputs:\n# comment\n- id: x"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("inputs"),
                YAMLElement.Object [
                    YAMLElement.Comment(" comment")
                    YAMLElement.Sequence [
                        YAMLElement.Object [
                            YAMLElement.Mapping(
                                YAMLContent.create("id"),
                                YAMLElement.Object [
                                    YAMLElement.Value(YAMLContent.create("x"))
                                ]
                            )
                        ]
                    ]
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Comment between key and sequence should not detach the sequence from the key."

    testCase "Key followed by multiple full-line comments and unindented sequence" <| fun _ ->
        let yaml = "inputs:\n# first\n# second\n- id: x\n- id: y"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("inputs"),
                YAMLElement.Object [
                    YAMLElement.Comment(" first")
                    YAMLElement.Comment(" second")
                    YAMLElement.Sequence [
                        YAMLElement.Object [
                            YAMLElement.Mapping(
                                YAMLContent.create("id"),
                                YAMLElement.Object [YAMLElement.Value(YAMLContent.create("x"))]
                            )
                        ]
                        YAMLElement.Object [
                            YAMLElement.Mapping(
                                YAMLContent.create("id"),
                                YAMLElement.Object [YAMLElement.Value(YAMLContent.create("y"))]
                            )
                        ]
                    ]
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Multiple comments between key and sequence should be preserved and sequence should stay mapped."

    testCase "Comment between sequence items stays inside mapped sequence" <| fun _ ->
        let yaml = "inputs:\n- id: x\n# between\n- id: y"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("inputs"),
                YAMLElement.Object [
                    YAMLElement.Sequence [
                        YAMLElement.Object [
                            YAMLElement.Mapping(
                                YAMLContent.create("id"),
                                YAMLElement.Object [YAMLElement.Value(YAMLContent.create("x"))]
                            )
                        ]
                        YAMLElement.Comment(" between")
                        YAMLElement.Object [
                            YAMLElement.Mapping(
                                YAMLContent.create("id"),
                                YAMLElement.Object [YAMLElement.Value(YAMLContent.create("y"))]
                            )
                        ]
                    ]
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Comments between sequence items should not terminate the mapped sequence."

    testCase "Comment between inline sequence mapping and nested fields stays in same item" <| fun _ ->
        let yaml = """listing:
  - entryname: arc
    # disabled entry example
    entry: $(inputs.rootDir)
    writable: true"""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("listing"),
                YAMLElement.Object [
                    YAMLElement.Sequence [
                        YAMLElement.Object [
                            YAMLElement.Mapping(
                                YAMLContent.create("entryname"),
                                YAMLElement.Object [
                                    YAMLElement.Value(YAMLContent.create("arc"))
                                ]
                            )
                            YAMLElement.Comment(" disabled entry example")
                            YAMLElement.Mapping(
                                YAMLContent.create("entry"),
                                YAMLElement.Object [
                                    YAMLElement.Value(YAMLContent.create("$(inputs.rootDir)"))
                                ]
                            )
                            YAMLElement.Mapping(
                                YAMLContent.create("writable"),
                                YAMLElement.Object [
                                    YAMLElement.Value(YAMLContent.create("true"))
                                ]
                            )
                        ]
                    ]
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "A sequence item comment should not split following nested fields into another item."

    testCase "Comment under a nested key with mapping value" <| fun _ ->
        let yaml = "r:\n  a:\n    # c\n    b: 1"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("r"),
                YAMLElement.Object [
                    YAMLElement.Mapping(
                        YAMLContent.create("a"),
                        YAMLElement.Object [
                            YAMLElement.Comment(" c")
                            YAMLElement.Mapping(
                                YAMLContent.create("b"),
                                YAMLElement.Object [YAMLElement.Value(YAMLContent.create("1"))]
                            )
                        ]
                    )
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "A comment below a nested key should land in the block of the next mapping."

    testCase "Comment under a nested key with sequence value" <| fun _ ->
        let yaml = "r:\n  a:\n    # c\n    - 1"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("r"),
                YAMLElement.Object [
                    YAMLElement.Mapping(
                        YAMLContent.create("a"),
                        YAMLElement.Object [
                            YAMLElement.Comment(" c")
                            YAMLElement.Sequence [
                                YAMLElement.Object [YAMLElement.Value(YAMLContent.create("1"))]
                            ]
                        ]
                    )
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "A comment below a nested key should precede the sequence inside the same block."

    testCase "Comment under a deeply nested key" <| fun _ ->
        let yaml = "a:\n  b:\n    c:\n      # x\n      d: 1"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("a"),
                YAMLElement.Object [
                    YAMLElement.Mapping(
                        YAMLContent.create("b"),
                        YAMLElement.Object [
                            YAMLElement.Mapping(
                                YAMLContent.create("c"),
                                YAMLElement.Object [
                                    YAMLElement.Comment(" x")
                                    YAMLElement.Mapping(
                                        YAMLContent.create("d"),
                                        YAMLElement.Object [YAMLElement.Value(YAMLContent.create("1"))]
                                    )
                                ]
                            )
                        ]
                    )
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "A comment should stay with the next mapping at any nesting depth."

    testCase "Hash without preceding whitespace stays in the scalar" <| fun _ ->
        let yaml = "run: tool.cwl#main\nlang: C#\nurl: http://x.org/a#frag"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("run"),
                YAMLElement.Object [YAMLElement.Value(YAMLContent.create("tool.cwl#main"))]
            )
            YAMLElement.Mapping(
                YAMLContent.create("lang"),
                YAMLElement.Object [YAMLElement.Value(YAMLContent.create("C#"))]
            )
            YAMLElement.Mapping(
                YAMLContent.create("url"),
                YAMLElement.Object [YAMLElement.Value(YAMLContent.create("http://x.org/a#frag"))]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "A hash that does not follow whitespace is part of the scalar."

    testCase "Hash directly after a flow closer is read as a plain scalar" <| fun _ ->
        let cases = [
            "k: [a]#c", "k", "[a]#c"
            "Say: [Hello, World]# 420 blaze it", "Say", "[Hello, World]# 420 blaze it"
        ]
        for (yaml, key, scalar) in cases do
            let expected = YAMLElement.Object [
                YAMLElement.Mapping(
                    YAMLContent.create(key),
                    YAMLElement.Object [YAMLElement.Value(YAMLContent.create(scalar))]
                )
            ]
            let actual = Reader.read yaml
            Expect.equal actual expected $"A hash without leading whitespace belongs to the plain scalar of {yaml}."

    testCase "Hash inside a literal block scalar is content" <| fun _ ->
        let yaml = "s: |\n  line\n  # not a comment\n"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("s"),
                YAMLElement.Value(
                    YAMLContent.create(
                        "line\n# not a comment\n",
                        style=ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Clip, None)
                    )
                )
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "A hash line inside a literal block scalar is scalar content."
        Expect.isFalse (hasAnyComment actual) "A literal block scalar should not produce comments."

    testCase "Comment after the last item of an unindented sequence belongs to the next key" <| fun _ ->
        let yaml = "a:\n- 1\n# end\nb: 2"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("a"),
                YAMLElement.Object [
                    YAMLElement.Sequence [
                        YAMLElement.Object [YAMLElement.Value(YAMLContent.create("1"))]
                    ]
                ]
            )
            YAMLElement.Comment(" end")
            YAMLElement.Mapping(
                YAMLContent.create("b"),
                YAMLElement.Object [YAMLElement.Value(YAMLContent.create("2"))]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "A comment after the last sequence item should precede the next key and stay outside the sequence."

    testCase "Trailing comment at the end of a nested block stays in the block" <| fun _ ->
        let yaml = "a:\n  b: 1\n  # end"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("a"),
                YAMLElement.Object [
                    YAMLElement.Mapping(
                        YAMLContent.create("b"),
                        YAMLElement.Object [YAMLElement.Value(YAMLContent.create("1"))]
                    )
                    YAMLElement.Comment(" end")
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "A comment after the last content line should stay in the last block."

    testCase "Trailing comment between two levels stays in the outer one" <| fun _ ->
        // yaml@2.6.1: {"a":{"b":{"c":1}}}. The comment at 6 spaces lies between the levels at 4 and 8.
        let yaml = "a:\n    b:\n        c: 1\n      # note\n"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("a"),
                YAMLElement.Object [
                    YAMLElement.Mapping(
                        YAMLContent.create("b"),
                        YAMLElement.Object [
                            YAMLElement.Mapping(
                                YAMLContent.create("c"),
                                YAMLElement.Object [YAMLElement.Value(YAMLContent.create("1"))]
                            )
                        ]
                    )
                    YAMLElement.Comment(" note")
                ]
            )
        ]
        Expect.equal (Reader.read yaml) expected "The comment joins the mapping at 4 spaces, which holds b"

    testCase "Trailing comments never reopen a level that an earlier comment closed" <| fun _ ->
        // yaml@2.6.1: {"a":{"b":1}}. The second comment sits at the level of b but follows the first one.
        let yaml = "a:\n    b: 1\n  # x\n    # y\n"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("a"),
                YAMLElement.Object [
                    YAMLElement.Mapping(
                        YAMLContent.create("b"),
                        YAMLElement.Object [YAMLElement.Value(YAMLContent.create("1"))]
                    )
                ]
            )
            YAMLElement.Comment(" x")
            YAMLElement.Comment(" y")
        ]
        Expect.equal (Reader.read yaml) expected "Both comments end up in the root mapping, in source order"

    testCase "Comment before a dedented key belongs to that key" <| fun _ ->
        let yaml = "a:\n  b: 1\n  # about c\nc: 2"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("a"),
                YAMLElement.Object [
                    YAMLElement.Mapping(
                        YAMLContent.create("b"),
                        YAMLElement.Object [YAMLElement.Value(YAMLContent.create("1"))]
                    )
                ]
            )
            YAMLElement.Comment(" about c")
            YAMLElement.Mapping(
                YAMLContent.create("c"),
                YAMLElement.Object [YAMLElement.Value(YAMLContent.create("2"))]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "A comment before a dedented key should move to the block of that key."

    testCase "Comments in a CWL requirements block" <| fun _ ->
        let yaml = """requirements:
  EnvVarRequirement:
    envDef:
      # Runner writes logs
      TOOL_RUN_SETTINGS: >-
        -Da
        -Db
  InitialWorkDirRequirement:
    listing:
      # Make an editable copy
      - entryname: src_data
        writable: true"""
        let envDefFirst =
            match Reader.read yaml with
            | YAMLElement.Object [
                YAMLElement.Mapping(
                    _,
                    YAMLElement.Object [
                        YAMLElement.Mapping(
                            _,
                            YAMLElement.Object [YAMLElement.Mapping(_, YAMLElement.Object (first :: _))]
                        )
                        _
                    ]
                )
              ] -> Some first
            | _ -> None
        Expect.equal envDefFirst (Some (YAMLElement.Comment(" Runner writes logs"))) "The comment should be the first element of the envDef block."
        let javaOptions =
            Decode.read yaml
            |> Decode.object (fun get ->
                get.Required.Field "requirements" (Decode.object (fun get2 ->
                    get2.Required.Field "EnvVarRequirement" (Decode.object (fun get3 ->
                        get3.Required.Field "envDef" (Decode.object (fun get4 ->
                            get4.Required.Field "TOOL_RUN_SETTINGS" Decode.string
                        ))
                    ))
                ))
            )
        Expect.equal javaOptions "-Da -Db" "The folded scalar should decode despite the comment above it."
        let listing =
            Decode.read yaml
            |> Decode.object (fun get ->
                get.Required.Field "requirements" (Decode.object (fun get2 ->
                    get2.Required.Field "InitialWorkDirRequirement" (Decode.object (fun get3 ->
                        get3.Required.Field "listing" (Decode.list (Decode.object (fun get4 ->
                            get4.Required.Field "entryname" Decode.string
                        )))
                    ))
                ))
            )
        Expect.equal listing ["src_data"] "The listing should decode despite the comment above it."

    testCase "Sequence item accepts structural blank line before nested mapping" <| fun _ ->
        let yaml = """items:
  -

    a: 1
"""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("items"),
                YAMLElement.Object [
                    YAMLElement.Sequence [
                        YAMLElement.Object [
                            YAMLElement.Mapping(
                                YAMLContent.create("a"),
                                YAMLElement.Object [
                                    YAMLElement.Value(YAMLContent.create("1"))
                                ]
                            )
                        ]
                    ]
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Blank lines before nested sequence-item content should stay with that item"

    testCase "NextLineSequenceObjects" <| fun _ ->
        let yaml = """
-
  My Key1: My Value1
  My Key2: My Value2
  My Key3: My Value3
-
  My Key4: My Value4
  My Key5: My Value5
  My Key6: My Value6
"""
        let expected = YAMLElement.Object [
            YAMLElement.Sequence[
                YAMLElement.Object[
                    YAMLElement.Mapping(
                        YAMLContent.create("My Key1"),
                        YAMLElement.Object [
                            YAMLElement.Value(YAMLContent.create("My Value1"))
                        ]
                    );
                    YAMLElement.Mapping(
                        YAMLContent.create("My Key2"),
                        YAMLElement.Object [
                            YAMLElement.Value(YAMLContent.create("My Value2"))
                        ]
                    );
                    YAMLElement.Mapping(
                        YAMLContent.create("My Key3"),
                        YAMLElement.Object [
                            YAMLElement.Value(YAMLContent.create("My Value3"))
                        ]
                    )
                ]
                YAMLElement.Object[
                        YAMLElement.Mapping(
                            YAMLContent.create("My Key4"),
                            YAMLElement.Object [
                                YAMLElement.Value(YAMLContent.create("My Value4"))
                            ]
                        );
                        YAMLElement.Mapping(
                            YAMLContent.create("My Key5"),
                            YAMLElement.Object [
                                YAMLElement.Value(YAMLContent.create("My Value5"))
                            ]
                        );
                        YAMLElement.Mapping(
                            YAMLContent.create("My Key6"),
                            YAMLElement.Object [
                                YAMLElement.Value(YAMLContent.create("My Value6"))
                            ]
                        )
                    ]
            ]
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected ""

    testCase "SequenceofSequences" <| fun _ ->
        let yaml = """
- [v1, v2, v3]
- [v4, v5, v6]
- [v7, v8, v9]
"""
        let expected = YAMLElement.Object [
            YAMLElement.Sequence[
                YAMLElement.Object[
                    YAMLElement.Sequence[
                        YAMLElement.Object[
                            YAMLElement.Value(YAMLContent.create("v1"))
                        ];
                        YAMLElement.Object[
                            YAMLElement.Value(YAMLContent.create("v2"))
                        ];
                        YAMLElement.Object[
                            YAMLElement.Value(YAMLContent.create("v3"))
                        ]
                    ]
                ];
                YAMLElement.Object[
                    YAMLElement.Sequence[
                        YAMLElement.Object[
                            YAMLElement.Value(YAMLContent.create("v4"))
                        ];
                        YAMLElement.Object[
                            YAMLElement.Value(YAMLContent.create("v5"))
                        ];
                        YAMLElement.Object[
                            YAMLElement.Value(YAMLContent.create("v6"))
                        ]
                    ]
                ];
                YAMLElement.Object[
                    YAMLElement.Sequence[
                        YAMLElement.Object[
                            YAMLElement.Value(YAMLContent.create("v7"))
                        ];
                        YAMLElement.Object[
                            YAMLElement.Value(YAMLContent.create("v8"))
                        ];
                        YAMLElement.Object[
                            YAMLElement.Value(YAMLContent.create("v9"))
                        ]
                    ]
                ]
            ]
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected ""

    testCase "MultilineSequenceSquare" <| fun _ ->
        let yaml = """
[
  v1,
  v2,
  v3
]
"""
        let expected = YAMLElement.Object [
            YAMLElement.Sequence[
                YAMLElement.Object [
                    YAMLElement.Value(YAMLContent.create("v1"));
                ]
                YAMLElement.Object [
                    YAMLElement.Value(YAMLContent.create("v2"));
                ]
                YAMLElement.Object [
                    YAMLElement.Value(YAMLContent.create("v3"))
                ]
            ]
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected ""

    testCase "MultilineSequenceSquare accepts blank line after opener" <| fun _ ->
        let yaml = """arr:
  [

    a,
    b
  ]
"""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("arr"),
                YAMLElement.Object [
                    YAMLElement.Sequence [
                        YAMLElement.Object [
                            YAMLElement.Value(YAMLContent.create("a"))
                        ]
                        YAMLElement.Object [
                            YAMLElement.Value(YAMLContent.create("b"))
                        ]
                    ]
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Blank lines after a multiline flow sequence opener should be ignored structurally"

    testCase "Namespaces" <| fun _ ->
        // yaml@2.6.1: {"$namespaces":{"arc":"https://github.com/nfdi4plants/ARC_ontology","test":"https://github.com/nfdi4plants/TEST_ontology"}}
        let yaml = """
$namespaces:
  arc: https://github.com/nfdi4plants/ARC_ontology
  test: https://github.com/nfdi4plants/TEST_ontology
"""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("$namespaces"),
                YAMLElement.Object [
                    YAMLElement.Mapping(
                        YAMLContent.create("arc"),
                        YAMLElement.Object[
                            YAMLElement.Value(YAMLContent.create("https://github.com/nfdi4plants/ARC_ontology"))
                        ]
                    );
                    YAMLElement.Mapping(
                        YAMLContent.create("test"),
                        YAMLElement.Object[
                            YAMLElement.Value(YAMLContent.create("https://github.com/nfdi4plants/TEST_ontology"))
                        ]
                    )
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "A key that starts with a dollar sign is a key like any other"

    testCase "A dollar key with a block sequence" <| fun _ ->
        // yaml@2.6.1: {"$schemas":["https://x","https://y"]} for both inputs
        let expected = YAMLElement.Object [ entry "$schemas" (sequenceNode [ scalarNode "https://x"; scalarNode "https://y" ]) ]
        Expect.equal (Reader.read "$schemas:\n  - https://x\n  - https://y\n") expected "The indented sequence"
        Expect.equal (Reader.read "$schemas:\n- https://x\n- https://y\n") expected "The sequence at the column of the key"

    testCase "A CWL head with dollar keys and prefixed keys" <| fun _ ->
        // yaml@2.6.1: {"id":"x","$namespaces":{"s":"https://schema.org/"},"$schemas":["https://schema.org/version/latest/termsonly-current-https.rdf"],"cwlVersion":"v1.2","s:author":[{"class":"s:Person","s:name":"Jane"}]}
        let yaml = "id: x\n$namespaces:\n  s: https://schema.org/\n$schemas:\n  - https://schema.org/version/latest/termsonly-current-https.rdf\ncwlVersion: v1.2\ns:author:\n  - class: s:Person\n    s:name: Jane\n"
        let expected = YAMLElement.Object [
            entry "id" (scalarNode "x")
            entry "$namespaces" (YAMLElement.Object [ entry "s" (scalarNode "https://schema.org/") ])
            entry "$schemas" (sequenceNode [ scalarNode "https://schema.org/version/latest/termsonly-current-https.rdf" ])
            entry "cwlVersion" (scalarNode "v1.2")
            entry "s:author" (sequenceNode [ YAMLElement.Object [ entry "class" (scalarNode "s:Person"); entry "s:name" (scalarNode "Jane") ] ])
        ]
        Expect.equal (Reader.read yaml) expected "The dollar keys and the prefixed keys are keys"

    testCase "A dollar key with a value and a nested dollar key" <| fun _ ->
        // yaml@2.6.1: {"$a":1} and {"x":{"$a":{"b":1}}}
        Expect.equal
            (Reader.read "$a: 1\n")
            (YAMLElement.Object [ entry "$a" (scalarNode "1") ])
            "A dollar key with a value"
        Expect.equal
            (Reader.read "x:\n  $a:\n    b: 1\n")
            (YAMLElement.Object [ entry "x" (YAMLElement.Object [ entry "$a" (YAMLElement.Object [ entry "b" (scalarNode "1") ]) ]) ])
            "A nested dollar key"

    testCase "A dollar key with a space or a colon in its name and a sibling behind it" <| fun _ ->
        // yaml@2.6.1: {"$a b":{"c":1}}, {"$a:b":{"c":1}}, {"$":{"c":1}} and {"$a":{"b":1},"z":2}
        let inner (key: string) = YAMLElement.Object [ entry key (YAMLElement.Object [ entry "c" (scalarNode "1") ]) ]
        Expect.equal (Reader.read "$a b:\n  c: 1\n") (inner "$a b") "A space in the name"
        Expect.equal (Reader.read "$a:b:\n  c: 1\n") (inner "$a:b") "A colon in the name"
        Expect.equal (Reader.read "$:\n  c: 1\n") (inner "$") "A dollar sign alone"
        Expect.equal
            (Reader.read "$a:\n  b: 1\nz: 2\n")
            (YAMLElement.Object [ entry "$a" (YAMLElement.Object [ entry "b" (scalarNode "1") ]); entry "z" (scalarNode "2") ])
            "A sibling behind the dollar key"

    testCase "A compact nested sequence holds the items at the column of its dash" <| fun _ ->
        // yaml@2.6.1: [["a","b"]], [["a"],"b"], [[["a","b"]]] and [[["a","b"],"c"],"d"]
        Expect.equal
            (Reader.read "- - a\n  - b\n")
            (sequenceNode [ sequenceNode [ scalarNode "a"; scalarNode "b" ] ])
            "A sibling of the first inner item"
        Expect.equal
            (Reader.read "- - a\n- b\n")
            (sequenceNode [ sequenceNode [ scalarNode "a" ]; scalarNode "b" ])
            "An item of the outer sequence ends the inner one"
        Expect.equal
            (Reader.read "- - - a\n    - b\n")
            (sequenceNode [ sequenceNode [ sequenceNode [ scalarNode "a"; scalarNode "b" ] ] ])
            "Three dashes on one line"
        Expect.equal
            (Reader.read "- - - a\n    - b\n  - c\n- d\n")
            (sequenceNode [ sequenceNode [ sequenceNode [ scalarNode "a"; scalarNode "b" ]; scalarNode "c" ]; scalarNode "d" ])
            "Each dash closes the sequences behind it"

    testCase "The lines behind a compact nested sequence item belong to that item" <| fun _ ->
        // yaml@2.6.1: {"k":[[{"class":"File","path":"p"}]]}, [[{"k":"v","j":"w"}]], [["x\n","y"]],
        // [[{"class":"File","path":"p"},"z"],"w"] and [[{"a":"b"}]]
        let fileItem = YAMLElement.Object [ entry "class" (scalarNode "File"); entry "path" (scalarNode "p") ]
        Expect.equal
            (Reader.read "k:\n  - - class: File\n      path: p\n")
            (YAMLElement.Object [ entry "k" (sequenceNode [ sequenceNode [ fileItem ] ]) ])
            "A mapping behind a mapping key"
        Expect.equal
            (Reader.read "- - k: v\n    j: w\n")
            (sequenceNode [ sequenceNode [ YAMLElement.Object [ entry "k" (scalarNode "v"); entry "j" (scalarNode "w") ] ] ])
            "A mapping at the root"
        Expect.equal
            (Reader.read "- - |\n    x\n  - y\n")
            (sequenceNode [
                sequenceNode [
                    YAMLElement.Object [
                        YAMLElement.Value(YAMLContent.create("x\n", style=ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Clip, None)))
                    ]
                    scalarNode "y"
                ]
            ])
            "A block scalar"
        Expect.equal
            (Reader.read "- - class: File\n    path: p\n  - z\n- w\n")
            (sequenceNode [ sequenceNode [ fileItem; scalarNode "z" ]; scalarNode "w" ])
            "A sibling and an outer item behind the mapping"
        Expect.equal
            (Reader.read "- - ? a\n    : b\n")
            (sequenceNode [ sequenceNode [ YAMLElement.Object [ entry "a" (scalarNode "b") ] ] ])
            "An explicit key"

    testCase "A list of lists of CWL file records reads as nested sequences" <| fun _ ->
        // yaml@2.6.1: {"fileCollectionSet":[[{"class":"File","path":"a_1.fq.gz"},{"class":"File","path":"a_2.fq.gz"}],[{"class":"File","path":"b_1.fq.gz"}]]}
        let yaml = "fileCollectionSet:\n  - - class: File\n      path: a_1.fq.gz\n    - class: File\n      path: a_2.fq.gz\n  - - class: File\n      path: b_1.fq.gz\n"
        let file (path: string) = YAMLElement.Object [ entry "class" (scalarNode "File"); entry "path" (scalarNode path) ]
        Expect.equal
            (Reader.read yaml)
            (YAMLElement.Object [
                entry "fileCollectionSet" (sequenceNode [ sequenceNode [ file "a_1.fq.gz"; file "a_2.fq.gz" ]; sequenceNode [ file "b_1.fq.gz" ] ])
            ])
            "Two inner sequences with their records"

    testCase "A key at the start of a sequence item owns the block behind it" <| fun _ ->
        // yaml@2.6.1: [{"a":{"b":1},"c":2}], [{"a":"x\n","b":1}] and [{"key":"x\n"}]
        Expect.equal
            (Reader.read "- a:\n    b: 1\n  c: 2\n")
            (sequenceNode [ YAMLElement.Object [ entry "a" (YAMLElement.Object [ entry "b" (scalarNode "1") ]); entry "c" (scalarNode "2") ] ])
            "A mapping behind the key"
        Expect.equal
            (Reader.read "- a: |\n    x\n  b: 1\n")
            (sequenceNode [
                YAMLElement.Object [
                    entry "a" (YAMLElement.Value(YAMLContent.create("x\n", style=ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Clip, None))))
                    entry "b" (scalarNode "1")
                ]
            ])
            "A block scalar behind the key"
        Expect.equal
            (Reader.read "- key: |2\n    x\n")
            (sequenceNode [
                YAMLElement.Object [
                    entry "key" (YAMLElement.Value(YAMLContent.create("x\n", style=ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Clip, Some 2))))
                ]
            ])
            "A block scalar with an indentation indicator"

    testCase "Comments around the items of a compact nested sequence stay with their items" <| fun _ ->
        // yaml@2.6.1: [["a","b"]]. It drops the comments.
        Expect.equal
            (Reader.read "- - a # c1\n  # c2\n  - b # c3\n")
            (sequenceNode [
                sequenceNode [
                    YAMLElement.Object [ YAMLElement.Value(YAMLContent.create("a", comment=" c1")) ]
                    YAMLElement.Comment " c2"
                    YAMLElement.Object [ YAMLElement.Value(YAMLContent.create("b", comment=" c3")) ]
                ]
            ])
            "The comments"

    testCase "Nested sequences of mappings and dollar keys survive write and read" <| fun _ ->
        // yaml@2.6.1 reads the written text as {"fileCollectionSet":[[{"class":"File","path":"a_1"},{"class":"File","path":"a_2"}],[{"class":"File","path":"b_1"}]]}
        // and as {"$namespaces":{"s":"https://schema.org/"},"$schemas":["https://x"]}
        let file (path: string) = YAMLElement.Object [ entry "class" (scalarNode "File"); entry "path" (scalarNode path) ]
        let records =
            YAMLElement.Object [
                entry "fileCollectionSet" (sequenceNode [ sequenceNode [ file "a_1"; file "a_2" ]; sequenceNode [ file "b_1" ] ])
            ]
        let dollarKeys =
            YAMLElement.Object [
                entry "$namespaces" (YAMLElement.Object [ entry "s" (scalarNode "https://schema.org/") ])
                entry "$schemas" (sequenceNode [ scalarNode "https://x" ])
            ]
        Expect.equal (Reader.read (Writer.write records None)) records "Nested sequences of mappings"
        Expect.equal (Reader.read (Writer.write dollarKeys None)) dollarKeys "Dollar keys"

    testCase "Block scalar literal" <| fun _ ->
        let yaml = """doc: |
  DESeq2 example workflow for **differential gene expression analysis**
  
  This workflow runs DESeq2 on the output of the kallisto workflow
  and the metadata file.
  It runs an R script, deseq2.R, which ideally should be split into three sub scripts and accordingly three workflow steps
    1. Read kallsito data
    2. Prep / run deseq2
    3. Plot results

  ## DESeq2 docs:
    https://bioconductor.org/packages/release/bioc/html/DESeq2.html

  ## Importing kallisto output with tximport
    https://bioconductor.org/packages/release/bioc/vignettes/tximport/inst/doc/tximport.html#kallisto

  ## Multi-package containers
  - R and combinations of library dependencies are available as multi-package containers from [BioContainers](https://github.com/BioContainers/multi-package-containers)
  - Searched for `repo:BioContainers/multi-package-containers deseq2 tximport rhdf5`
  - and found `quay.io/biocontainers/mulled-v2-05fd88b9ac812a9149da2f2d881d62f01cc49835:a10f0e3a7a70fc45494f8781d33901086d2214d0-0` :tada:"""
        let blockLines = [|
            "DESeq2 example workflow for **differential gene expression analysis**";
            "";
            "This workflow runs DESeq2 on the output of the kallisto workflow";
            "and the metadata file.";
            "It runs an R script, deseq2.R, which ideally should be split into three sub scripts and accordingly three workflow steps";
            "  1. Read kallsito data";
            "  2. Prep / run deseq2";
            "  3. Plot results";
            "";
            "## DESeq2 docs:";
            "  https://bioconductor.org/packages/release/bioc/html/DESeq2.html";
            "";
            "## Importing kallisto output with tximport";
            "  https://bioconductor.org/packages/release/bioc/vignettes/tximport/inst/doc/tximport.html#kallisto";
            "";
            "## Multi-package containers";
            "- R and combinations of library dependencies are available as multi-package containers from [BioContainers](https://github.com/BioContainers/multi-package-containers)";
            "- Searched for `repo:BioContainers/multi-package-containers deseq2 tximport rhdf5`";
            "- and found `quay.io/biocontainers/mulled-v2-05fd88b9ac812a9149da2f2d881d62f01cc49835:a10f0e3a7a70fc45494f8781d33901086d2214d0-0` :tada:";
        |]
        let block = System.String.Join("\n", blockLines) + "\n"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("doc"),
                YAMLElement.Value (YAMLContent.create(block, style=ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Clip, None)))
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected ""
    testCase "JSONMappingsInline" <| fun _ ->
        let yaml = """
Mark McGwire: {hr: 65, avg: 0.278}
"""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("Mark McGwire"),
                YAMLElement.Object [
                    YAMLElement.Mapping(
                        YAMLContent.create("hr"),
                        YAMLElement.Object [
                            YAMLElement.Value(YAMLContent.create("65"))
                        ]
                    );
                    YAMLElement.Mapping(
                        YAMLContent.create("avg"),
                        YAMLElement.Object [
                            YAMLElement.Value(YAMLContent.create("0.278"))
                        ]
                    )
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected ""
    testCase "JSONMappingsMultiline" <| fun _ ->
        let yaml = """
Sammy Sosa: {
    hr: 63,
    avg: 0.288,
}"""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("Sammy Sosa"),
                YAMLElement.Object [
                    YAMLElement.Mapping(
                        YAMLContent.create("hr"),
                        YAMLElement.Object [
                            YAMLElement.Value(YAMLContent.create("63"))
                        ]
                    );
                    YAMLElement.Mapping(
                        YAMLContent.create("avg"),
                        YAMLElement.Object [
                            YAMLElement.Value(YAMLContent.create("0.288"))
                        ]
                    )
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected ""

    testCase "JSONMappingsMultiline accepts blank line after opener" <| fun _ ->
        let yaml = """obj:
  k: {

    a: 1
  }
"""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("obj"),
                YAMLElement.Object [
                    YAMLElement.Mapping(
                        YAMLContent.create("k"),
                        YAMLElement.Object [
                            YAMLElement.Mapping(
                                YAMLContent.create("a"),
                                YAMLElement.Object [
                                    YAMLElement.Value(YAMLContent.create("1"))
                                ]
                            )
                        ]
                    )
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Blank lines after a multiline flow object opener should be ignored structurally"

    testCase "NestedFlowStyleComplex" <| fun _ ->
        let yamlFlowstyle = """requirements: {
  DockerRequirement: {
    dockerImageId: "devcontainer",
    dockerFile: { $include: "FSharpArcCapsule/Dockerfile" }
  },
  SubworkflowFeatureRequirement: {},
  NetworkAccess: { networkAccess: true }
}"""
        let yaml = """requirements:
  DockerRequirement:
    dockerImageId: "devcontainer"
    dockerFile:
      $include: "FSharpArcCapsule/Dockerfile"
  SubworkflowFeatureRequirement: {}
  NetworkAccess:
    networkAccess: true
"""
        let actualFlowstyle = Reader.read yamlFlowstyle
        let actual = Reader.read yaml
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("requirements"),
                YAMLElement.Object [
                    YAMLElement.Mapping(
                        YAMLContent.create("DockerRequirement"),
                        YAMLElement.Object [
                            YAMLElement.Mapping(
                                YAMLContent.create("dockerImageId"),
                                YAMLElement.Object [ YAMLElement.Value(YAMLContent.create("devcontainer", style=ScalarStyle.DoubleQuoted)) ]
                            );
                            YAMLElement.Mapping(
                                YAMLContent.create("dockerFile"),
                                YAMLElement.Object [
                                    YAMLElement.Mapping(
                                        YAMLContent.create("$include"),
                                        YAMLElement.Object [ YAMLElement.Value(YAMLContent.create("FSharpArcCapsule/Dockerfile", style=ScalarStyle.DoubleQuoted)) ]
                                    )
                                ]
                            )
                        ]
                    );
                    YAMLElement.Mapping(
                        YAMLContent.create("SubworkflowFeatureRequirement"),
                        YAMLElement.Object []
                    );
                    YAMLElement.Mapping(
                        YAMLContent.create("NetworkAccess"),
                        YAMLElement.Object [
                            YAMLElement.Mapping(
                                YAMLContent.create("networkAccess"),
                                YAMLElement.Object [ YAMLElement.Value(YAMLContent.create("true")) ]
                            )
                        ]
                    )
                ]
            )
        ]
        Expect.equal actualFlowstyle expected "Flowstyle"
        Expect.equal actual expected "Blockstyle"

    testCase "NestedFlowStyleCompact" <| fun _ ->
        let yamlFlowstyle = """requirements: {
  DockerRequirement: { dockerImageId: "devcontainer", dockerFile: { $include: "FSharpArcCapsule/Dockerfile" } },
  SubworkflowFeatureRequirement: {},
  NetworkAccess: { networkAccess: true }
}"""
        let yaml = """requirements:
  DockerRequirement:
    dockerImageId: "devcontainer"
    dockerFile:
      $include: "FSharpArcCapsule/Dockerfile"
  SubworkflowFeatureRequirement: {}
  NetworkAccess:
    networkAccess: true
"""
        let actualFlowstyle = Reader.read yamlFlowstyle
        let actual = Reader.read yaml
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("requirements"),
                YAMLElement.Object [
                    YAMLElement.Mapping(
                        YAMLContent.create("DockerRequirement"),
                        YAMLElement.Object [
                            YAMLElement.Mapping(
                                YAMLContent.create("dockerImageId"),
                                YAMLElement.Object [ YAMLElement.Value(YAMLContent.create("devcontainer", style=ScalarStyle.DoubleQuoted)) ]
                            );
                            YAMLElement.Mapping(
                                YAMLContent.create("dockerFile"),
                                YAMLElement.Object [
                                    YAMLElement.Mapping(
                                        YAMLContent.create("$include"),
                                        YAMLElement.Object [ YAMLElement.Value(YAMLContent.create("FSharpArcCapsule/Dockerfile", style=ScalarStyle.DoubleQuoted)) ]
                                    )
                                ]
                            )
                        ]
                    );
                    YAMLElement.Mapping(
                        YAMLContent.create("SubworkflowFeatureRequirement"),
                        YAMLElement.Object []
                    );
                    YAMLElement.Mapping(
                        YAMLContent.create("NetworkAccess"),
                        YAMLElement.Object [
                            YAMLElement.Mapping(
                                YAMLContent.create("networkAccess"),
                                YAMLElement.Object [ YAMLElement.Value(YAMLContent.create("true")) ]
                            )
                        ]
                    )
                ]
            )
        ]
        Expect.equal actualFlowstyle expected "Flowstyle"
        Expect.equal actual expected "Blockstyle"

    testCase "NestedFlowStyleInlineArrays" <| fun _ ->
        let yamlFlowstyle = """requirements: {
  InitialWorkDirRequirement: { listing: [{entryname:"arc",entry:"$(inputs.arcDirectory)", writable: true},{ entry: "$(inputs.outputDirectory)", writable: true }] },
  EnvVarRequirement: { envDef: [{ envName: "DOTNET_NOLOGO", envValue: "true" },{ envName: "TEST", envValue: "false" }] },
  SubworkflowFeatureRequirement: {}
}"""
        let yaml = """requirements:
  InitialWorkDirRequirement:
    listing:
      - entryname: "arc"
        entry: "$(inputs.arcDirectory)"
        writable: true
      - entry: "$(inputs.outputDirectory)"
        writable: true
  EnvVarRequirement:
    envDef:
      - envName: "DOTNET_NOLOGO"
        envValue: "true"
      - envName: "TEST"
        envValue: "false"
  SubworkflowFeatureRequirement: {}
"""
        let actualFlowstyle = Reader.read yamlFlowstyle
        let actual = Reader.read yaml
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("requirements"),
                YAMLElement.Object [
                    YAMLElement.Mapping(
                        YAMLContent.create("InitialWorkDirRequirement"),
                        YAMLElement.Object [
                            YAMLElement.Mapping(
                                YAMLContent.create("listing"),
                                YAMLElement.Object [
                                    YAMLElement.Sequence [
                                        YAMLElement.Object [
                                            YAMLElement.Mapping(YAMLContent.create("entryname"), YAMLElement.Object [ YAMLElement.Value(YAMLContent.create("arc", style=ScalarStyle.DoubleQuoted)) ]);
                                            YAMLElement.Mapping(YAMLContent.create("entry"), YAMLElement.Object [ YAMLElement.Value(YAMLContent.create("$(inputs.arcDirectory)", style=ScalarStyle.DoubleQuoted)) ]);
                                            YAMLElement.Mapping(YAMLContent.create("writable"), YAMLElement.Object [ YAMLElement.Value(YAMLContent.create("true")) ])
                                        ];
                                        YAMLElement.Object [
                                            YAMLElement.Mapping(YAMLContent.create("entry"), YAMLElement.Object [ YAMLElement.Value(YAMLContent.create("$(inputs.outputDirectory)", style=ScalarStyle.DoubleQuoted)) ]);
                                            YAMLElement.Mapping(YAMLContent.create("writable"), YAMLElement.Object [ YAMLElement.Value(YAMLContent.create("true")) ])
                                        ]
                                    ]
                                ]
                            )
                        ]
                    );
                    YAMLElement.Mapping(
                        YAMLContent.create("EnvVarRequirement"),
                        YAMLElement.Object [
                            YAMLElement.Mapping(
                                YAMLContent.create("envDef"),
                                YAMLElement.Object [
                                    YAMLElement.Sequence [
                                        YAMLElement.Object [
                                            YAMLElement.Mapping(YAMLContent.create("envName"), YAMLElement.Object [ YAMLElement.Value(YAMLContent.create("DOTNET_NOLOGO", style=ScalarStyle.DoubleQuoted)) ]);
                                            YAMLElement.Mapping(YAMLContent.create("envValue"), YAMLElement.Object [ YAMLElement.Value(YAMLContent.create("true", style=ScalarStyle.DoubleQuoted)) ])
                                        ];
                                        YAMLElement.Object [
                                            YAMLElement.Mapping(YAMLContent.create("envName"), YAMLElement.Object [ YAMLElement.Value(YAMLContent.create("TEST", style=ScalarStyle.DoubleQuoted)) ]);
                                            YAMLElement.Mapping(YAMLContent.create("envValue"), YAMLElement.Object [ YAMLElement.Value(YAMLContent.create("false", style=ScalarStyle.DoubleQuoted)) ])
                                        ]
                                    ]
                                ]
                            )
                        ]
                    );
                    YAMLElement.Mapping(
                        YAMLContent.create("SubworkflowFeatureRequirement"),
                        YAMLElement.Object []
                    )
                ]
            )
        ]
        Expect.equal actualFlowstyle expected "Flowstyle"
        Expect.equal actual expected "Blockstyle"

    testCase "Multi-document: Two documents separated by ---" <| fun _ ->
        let yaml = """---
document1: value1
---
document2: value2"""
        let actual = Reader.readDocuments yaml
        Expect.equal (List.length actual) 2 "Should parse two documents"
        
        // Check first document
        let doc1 = actual.[0]
        match doc1 with
        | YAMLElement.Object elems ->
            Expect.equal (List.length elems) 1 "First document should have one mapping"
            match elems.[0] with
            | YAMLElement.Mapping(key, value) ->
                Expect.equal key.Value "document1" "First document key should be document1"
            | _ -> failwith "Expected mapping in first document"
        | _ -> failwith "Expected object in first document"
        
        // Check second document
        let doc2 = actual.[1]
        match doc2 with
        | YAMLElement.Object elems ->
            Expect.equal (List.length elems) 1 "Second document should have one mapping"
            match elems.[0] with
            | YAMLElement.Mapping(key, value) ->
                Expect.equal key.Value "document2" "Second document key should be document2"
            | _ -> failwith "Expected mapping in second document"
        | _ -> failwith "Expected object in second document"

    testCase "Multi-document: Document with end marker" <| fun _ ->
        let yaml = """---
key: value
..."""
        let actual = Reader.readDocuments yaml
        Expect.equal (List.length actual) 1 "Should parse one document"
        
        let doc = actual.[0]
        match doc with
        | YAMLElement.Object elems ->
            Expect.equal (List.length elems) 1 "Document should have one mapping"
            match elems.[0] with
            | YAMLElement.Mapping(key, value) ->
                Expect.equal key.Value "key" "Document key should be key"
            | _ -> failwith "Expected mapping in document"
        | _ -> failwith "Expected object in document"

    testCase "Reader.read stops at document end marker" <| fun _ ->
        let yaml = """---
key: value
...
trailing: ignored"""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("key"),
                YAMLElement.Object [YAMLElement.Value(YAMLContent.create("value"))]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Reader.read should ignore content after explicit document end"

    testCase "Reader.read keeps inline root after document start marker" <| fun _ ->
        let yaml = "--- foo"
        let expected = YAMLElement.Object [
            YAMLElement.Value(YAMLContent.create("foo"))
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Reader.read should parse inline content after --- as the document root"

    testCase "Bare dash sequence element" <| fun _ ->
        let yaml = """- 
- value"""
        let expected = YAMLElement.Object [
            YAMLElement.Sequence [
                YAMLElement.Object []
                YAMLElement.Object [YAMLElement.Value(YAMLContent.create("value"))]
            ]
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Bare dash with no value should not crash"

    testCase "Multiple bare dash sequence elements" <| fun _ ->
        let yaml = """- 
- 
- end"""
        let expected = YAMLElement.Object [
            YAMLElement.Sequence [
                YAMLElement.Object []
                YAMLElement.Object []
                YAMLElement.Object [YAMLElement.Value(YAMLContent.create("end"))]
            ]
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Multiple bare dashes should be parsed as empty sequence elements"

    testCase "Key followed by blank lines and unindented sequence" <| fun _ ->
        let yaml = """root:


- a
- b"""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("root"),
                YAMLElement.Object [
                    YAMLElement.Sequence [
                        YAMLElement.Object [YAMLElement.Value(YAMLContent.create("a"))]
                        YAMLElement.Object [YAMLElement.Value(YAMLContent.create("b"))]
                    ]
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Blank separation lines between a key and an indentless sequence should not split the mapping."

    testCase "Key followed by blank lines, comments, and unindented sequence" <| fun _ ->
        let yaml = """root:

# keep

- a
- b"""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("root"),
                YAMLElement.Object [
                    YAMLElement.Comment(" keep")
                    YAMLElement.Sequence [
                        YAMLElement.Object [YAMLElement.Value(YAMLContent.create("a"))]
                        YAMLElement.Object [YAMLElement.Value(YAMLContent.create("b"))]
                    ]
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Blank separation lines around comments should not prevent a following indentless sequence from staying mapped."

    testCase "Blank lines between sequence items do not split the sequence" <| fun _ ->
        let yaml = """root:
- a

- b"""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("root"),
                YAMLElement.Object [
                    YAMLElement.Sequence [
                        YAMLElement.Object [YAMLElement.Value(YAMLContent.create("a"))]
                        YAMLElement.Object [YAMLElement.Value(YAMLContent.create("b"))]
                    ]
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Blank separation lines between sequence items should not split one sequence into sibling sequences."

    testCase "Empty inline flow object" <| fun _ ->
        let yaml = "key: {}"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("key"),
                YAMLElement.Object []
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Empty inline flow object should parse as empty Object"

    testCase "Multiline flow sequence with trailing comment on value line" <| fun _ ->
        let yaml = "data: [a, b] # after"
        let actual = Reader.read yaml
        match actual with
        | YAMLElement.Object [YAMLElement.Mapping(k, v)] when k.Value = "data" ->
            match v with
            | YAMLElement.Object children ->
                let hasSeq = children |> List.exists (function YAMLElement.Sequence _ -> true | _ -> false)
                let hasComment = children |> List.exists (function YAMLElement.Comment c -> c.Contains("after") | _ -> false)
                Expect.isTrue hasSeq "Should contain a sequence"
                Expect.isTrue hasComment "Should contain the trailing comment"
            | _ -> Expect.isTrue false "Expected Object wrapping sequence and comment"
        | _ -> Expect.isTrue false "Expected mapping for data"

    testCase "Multiline flow sequence in mapping context" <| fun _ ->
        let yaml = """data:
  [
    a,
    b
  ]"""
        let actual = Reader.read yaml
        match actual with
        | YAMLElement.Object [YAMLElement.Mapping(k, v)] when k.Value = "data" ->
            match v with
            | YAMLElement.Object [YAMLElement.Sequence items] ->
                Expect.equal (List.length items) 2 "Should have 2 items in multiline sequence"
            | _ -> Expect.isTrue false "Expected Object wrapping sequence"
        | _ -> Expect.isTrue false "Expected mapping for data"

    testCase "Multiline flow sequence as block sequence item parses structurally" <| fun _ ->
        let yaml = "items:\n  - [\n      a,\n      b\n    ]"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("items"),
                YAMLElement.Object [
                    YAMLElement.Sequence [
                        YAMLElement.Object [
                            YAMLElement.Sequence [
                                YAMLElement.Object [YAMLElement.Value(YAMLContent.create("a"))]
                                YAMLElement.Object [YAMLElement.Value(YAMLContent.create("b"))]
                            ]
                        ]
                    ]
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "A multiline flow sequence used as a block sequence item should go through the flow parser"

    testCase "Multiline flow object as block sequence item parses structurally" <| fun _ ->
        let yaml = "items:\n  - {\n      a: b\n    }"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("items"),
                YAMLElement.Object [
                    YAMLElement.Sequence [
                        YAMLElement.Object [
                            YAMLElement.Mapping(
                                YAMLContent.create("a"),
                                YAMLElement.Object [YAMLElement.Value(YAMLContent.create("b"))]
                            )
                        ]
                    ]
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "A multiline flow object used as a block sequence item should go through the flow parser"

    testCase "Root multiline flow object parses structurally" <| fun _ ->
        let yaml = "{\n  a: b\n}"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("a"),
                YAMLElement.Object [YAMLElement.Value(YAMLContent.create("b"))]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "A root multiline flow object should parse like an inline root flow object"

    testCase "Flow plain scalar value may contain colon" <| fun _ ->
        let yaml = "data: {url: http://example.com, time: 12:45}"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("data"),
                YAMLElement.Object [
                    YAMLElement.Mapping(
                        YAMLContent.create("url"),
                        YAMLElement.Object [YAMLElement.Value(YAMLContent.create("http://example.com"))]
                    )
                    YAMLElement.Mapping(
                        YAMLContent.create("time"),
                        YAMLElement.Object [YAMLElement.Value(YAMLContent.create("12:45"))]
                    )
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Colon characters inside plain flow scalar values should remain scalar content"

    testCase "Comment inside multiline flow sequence stays a comment node" <| fun _ ->
        let yaml = "data:\n  [\n    a, # one\n    b\n  ]"
        let actual = Reader.read yaml
        match actual with
        | YAMLElement.Object [YAMLElement.Mapping(k, YAMLElement.Object [YAMLElement.Sequence items])] when k.Value = "data" ->
            Expect.equal (List.length items) 3 "The comment should be preserved as a sequence entry between values"
            match items.[1] with
            | YAMLElement.Comment c -> Expect.equal c " one" "Flow comment placeholder should restore to a comment"
            | other -> failwithf "Expected flow comment sequence entry, got: %A" other
        | other -> failwithf "Unexpected structure: %A" other

    testCase "Multiline flow object with closer inside mapping" <| fun _ ->
        let yaml = """data:
  k: {
    a: 1
  }"""
        let actual = Reader.read yaml
        match actual with
        | YAMLElement.Object [YAMLElement.Mapping(k, v)] when k.Value = "data" ->
            match v with
            | YAMLElement.Object children ->
                let hasInnerKey = children |> List.exists (function
                    | YAMLElement.Mapping(ik, _) when ik.Value = "k" -> true
                    | _ -> false)
                Expect.isTrue hasInnerKey "Should have inner mapping for key 'k'"
            | _ -> Expect.isTrue false "Expected Object with inner mappings"
        | _ -> Expect.isTrue false "Expected mapping for data"

    testCase "Inline flow sequence with double-quoted value containing comma" <| fun _ ->
        let yaml = "items: [\"hello, world\", c]"
        let actual = Reader.read yaml
        match actual with
        | YAMLElement.Object [YAMLElement.Mapping(k, v)] when k.Value = "items" ->
            match v with
            | YAMLElement.Object [YAMLElement.Sequence items] ->
                Expect.equal (List.length items) 2 "Should have 2 items in flow sequence with comma in quoted string"
                let firstValue =
                    match items.[0] with
                    | YAMLElement.Object [YAMLElement.Value content] -> content.Value
                    | _ -> ""
                Expect.isTrue (firstValue.Contains("hello") && firstValue.Contains("world")) "First item should preserve comma-separated value"
            | _ -> Expect.isTrue false "Expected Sequence inside Object"
        | _ -> Expect.isTrue false "Expected mapping for items"

    testCase "Inline flow object with double-quoted value containing comma" <| fun _ ->
        let yaml = "data: {key: \"a, b, c\"}"
        let actual = Reader.read yaml
        match actual with
        | YAMLElement.Object [YAMLElement.Mapping(k, v)] when k.Value = "data" ->
            match v with
            | YAMLElement.Object children ->
                let hasKey = children |> List.exists (function
                    | YAMLElement.Mapping(ck, _) -> ck.Value = "key"
                    | _ -> false)
                Expect.isTrue hasKey "Should contain key mapping"
                let keyValue =
                    children |> List.tryPick (function
                        | YAMLElement.Mapping(ck, cv) when ck.Value = "key" ->
                            match cv with
                            | YAMLElement.Object [YAMLElement.Value content] -> Some content.Value
                            | _ -> None
                        | _ -> None)
                match keyValue with
                | Some v ->
                    Expect.isTrue (v.Contains("a") && v.Contains("b") && v.Contains("c")) "Double-quoted value should preserve commas"
                | None -> Expect.isTrue false "Expected value for key"
            | _ -> Expect.isTrue false "Expected Object"
        | _ -> Expect.isTrue false "Expected mapping for data"

    testCase "Inline flow sequence nested in block sequence with double-quoted comma" <| fun _ ->
        let yaml = "steps:\n  - [\"a,b\", c]\n  - [d, e]"
        let actual = Reader.read yaml
        match actual with
        | YAMLElement.Object [YAMLElement.Mapping(k, v)] when k.Value = "steps" ->
            match v with
            | YAMLElement.Object [YAMLElement.Sequence items] ->
                Expect.equal (List.length items) 2 "Should have 2 sequence items"
            | _ -> Expect.isTrue false "Expected Sequence inside Object"
        | _ -> Expect.isTrue false "Expected mapping for steps"

    testCase "Multiline flow sequence accepts trailing comma and blank line" <| fun _ ->
        let yaml = "arr:\n  [\n\n    a,\n    b,\n  ]"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("arr"),
                YAMLElement.Object [
                    YAMLElement.Sequence [
                        YAMLElement.Object [
                            YAMLElement.Value(YAMLContent.create("a"))
                        ];
                        YAMLElement.Object [
                            YAMLElement.Value(YAMLContent.create("b"))
                        ]
                    ]
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Multiline flow sequence with trailing comma and blank line should parse correctly"

    testCase "CWL-style inline flow object with nested flow value" <| fun _ ->
        let yaml = """requirements: { LoadListingRequirement: { loadListing: "no_listing" } }"""
        let actual = Reader.read yaml
        match actual with
        | YAMLElement.Object [YAMLElement.Mapping(key, value)] ->
            Expect.equal key.Value "requirements" "Key should be 'requirements'"
            match value with
            | YAMLElement.Object [YAMLElement.Mapping(innerKey, innerValue)] ->
                Expect.equal innerKey.Value "LoadListingRequirement" "Inner key should be 'LoadListingRequirement'"
                match innerValue with
                | YAMLElement.Object _ ->
                    Expect.isTrue true "Parsed nested flow object successfully"
                | _ -> failwithf "Expected Object for inner value, got: %A" innerValue
            | _ -> failwithf "Expected Object with Mapping for value, got: %A" value
        | _ -> failwithf "Unexpected structure: %A" actual

    testCase "Inline flow object as value with space before brace" <| fun _ ->
        let yaml = "key: {a: 1, b: 2}"
        let actual = Reader.read yaml
        match actual with
        | YAMLElement.Object [YAMLElement.Mapping(key, value)] ->
            Expect.equal key.Value "key" "Key should be 'key'"
        | _ -> failwithf "Unexpected structure: %A" actual

    testCase "Inline flow array as value" <| fun _ ->
        let yaml = "key: [1, 2, 3]"
        let actual = Reader.read yaml
        match actual with
        | YAMLElement.Object [YAMLElement.Mapping(key, value)] ->
            Expect.equal key.Value "key" "Key should be 'key'"
        | _ -> failwithf "Unexpected structure: %A" actual

    testCase "Empty flow object as mapping value (CWL pattern)" <| fun _ ->
        let yaml = "requirements:\n  InlineJavascriptRequirement: {}"
        let actual = Reader.read yaml
        match actual with
        | YAMLElement.Object [YAMLElement.Mapping(key, value)] ->
            Expect.equal key.Value "requirements" "Key should be 'requirements'"
        | _ -> failwithf "Unexpected structure: %A" actual

    testCase "Empty flow array as mapping value" <| fun _ ->
        let yaml = "outputs: {}"
        let actual = Reader.read yaml
        match actual with
        | YAMLElement.Object [YAMLElement.Mapping(key, value)] ->
            Expect.equal key.Value "outputs" "Key should be 'outputs'"
        | _ -> failwithf "Unexpected structure: %A" actual

    testCase "CWL ExpressionTool pool output roundtrip YAML" <| fun _ ->
        let yaml = """#!/usr/bin/env cwl-runner

cwlVersion: v1.2
class: ExpressionTool
id: "V_pool_out"
label: Returns the output directory named after "analysis", containing all input files and directories.
requirements:
  InlineJavascriptRequirement: {}
inputs:
  mount_dir:
    type: Directory
  file_single:
    type: File?
  file_array:
    type: File[]?
  directory_single:
    type: Directory?
  directory_array:
    type: Directory[]?
  newname:
    type: string?
outputs:
  pool_DIR:
    type: Directory
    doc: "Final analysis output folder"
expression: >
  ${ return (function() {
    function sanitize(entry) {
      var allowedFields = ['class', 'basename', 'location', 'listing'];
      var sanitized = {};
      for (var i = 0; i < allowedFields.length; i++) {
        var key = allowedFields[i];
        if (entry[key] !== undefined) sanitized[key] = entry[key];
      }
      return sanitized.class && sanitized.basename ? sanitized : null;
      return name.replace(/\.tiff$/, "").replace(/\.tif$/, "");
    }

    var outputList = [];
    if (inputs.directory_single) outputList.push(sanitize(inputs.directory_single));
    if (inputs.file_single) outputList.push(sanitize(inputs.file_single));

    return {
      pool_DIR: { class: "Directory", basename: inputs.newname || "analysis", listing: outputList }
    };
  })(); }"""
        let actual = Reader.read yaml
        match actual with
        | YAMLElement.Object _ ->
            Expect.isTrue true "Parsed CWL ExpressionTool YAML"
        | _ -> failwithf "Unexpected structure: %A" actual

    testCase "CWL ExpressionTool roundtrip preserves block scalar" <| fun _ ->
        let yaml = "expression: >\n  ${ return { \"out\": name }; }"
        let parsed = Reader.read yaml
        let written = Writer.write (YAMLElement.Object [parsed]) None
        Expect.stringContains written "out" "Block scalar content should survive roundtrip"

    testCase "InlineJavascriptRequirement empty flow object roundtrip" <| fun _ ->
        let yaml = "InlineJavascriptRequirement: {}"
        let parsed = Reader.read yaml
        let written = Writer.write (YAMLElement.Object [parsed]) None
        Expect.stringContains written "{}" "Empty flow object should survive roundtrip"

    testCase "CWL ExpressionTool full roundtrip preserves expression and empty object" <| fun _ ->
        let yaml = """#!/usr/bin/env cwl-runner

cwlVersion: v1.2
class: ExpressionTool
id: "V_pool_out"
label: Returns the output directory named after "analysis", containing all input files and directories.
requirements:
  InlineJavascriptRequirement: {}
inputs:
  mount_dir:
    type: Directory
  file_single:
    type: File?
  file_array:
    type: File[]?
  directory_single:
    type: Directory?
  directory_array:
    type: Directory[]?
  newname:
    type: string?
outputs:
  pool_DIR:
    type: Directory
    doc: "Final analysis output folder"
expression: >
  ${ return (function() {
    function sanitize(entry) {
      var allowedFields = ['class', 'basename', 'location', 'listing'];
      var sanitized = {};
      for (var i = 0; i < allowedFields.length; i++) {
        var key = allowedFields[i];
        if (entry[key] !== undefined) sanitized[key] = entry[key];
      }
      return sanitized.class && sanitized.basename ? sanitized : null;
      return name.replace(/\.tiff$/, "").replace(/\.tif$/, "");
    }

    var outputList = [];
    if (inputs.directory_single) outputList.push(sanitize(inputs.directory_single));
    if (inputs.file_single) outputList.push(sanitize(inputs.file_single));

    return {
      pool_DIR: { class: "Directory", basename: inputs.newname || "analysis", listing: outputList }
    };
  })(); }"""
        let parsed = Reader.read yaml
        let written = Writer.write parsed None
        let reparsed = Reader.read written
        let rewritten = Writer.write reparsed None
        Expect.stringContains written "{}" "Empty flow object {} should appear in first write"
        Expect.stringContains rewritten "{}" "Empty flow object {} should survive roundtrip"
        Expect.stringContains rewritten "sanitize(entry)" "sanitize helper should survive roundtrip"
        Expect.stringContains rewritten "allowedFields" "allowedFields array should survive roundtrip"
        Expect.equal written rewritten "Write output should be stable across roundtrips"

    testCase "Commented-out block scalar header does not protect the following lines" <| fun _ ->
        let yaml = "a:\n# old: |\n  b: 1 # trailing\n  d: 2"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("a"),
                YAMLElement.Object [
                    YAMLElement.Comment(" old: |")
                    YAMLElement.Mapping(
                        YAMLContent.create("b"),
                        YAMLElement.Object [YAMLElement.Value(YAMLContent.create("1", " trailing"))]
                    )
                    YAMLElement.Mapping(
                        YAMLContent.create("d"),
                        YAMLElement.Object [YAMLElement.Value(YAMLContent.create("2"))]
                    )
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "A header inside a comment should not open a block scalar."

    testCase "Commented-out block scalar header inside a CWL requirements block" <| fun _ ->
        let yaml = "requirements:\n#  InitialWorkDirRequirement: |\n  DockerRequirement:\n    dockerPull: x\n    # pinned version\n    b: 2"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("requirements"),
                YAMLElement.Object [
                    YAMLElement.Comment("  InitialWorkDirRequirement: |")
                    YAMLElement.Mapping(
                        YAMLContent.create("DockerRequirement"),
                        YAMLElement.Object [
                            YAMLElement.Mapping(
                                YAMLContent.create("dockerPull"),
                                YAMLElement.Object [YAMLElement.Value(YAMLContent.create("x"))]
                            )
                            YAMLElement.Comment(" pinned version")
                            YAMLElement.Mapping(
                                YAMLContent.create("b"),
                                YAMLElement.Object [YAMLElement.Value(YAMLContent.create("2"))]
                            )
                        ]
                    )
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "A commented-out block scalar header should leave the following comments and entries alone."

    testCase "Comment before a dedented flow closer stays in the flow collection" <| fun _ ->
        let yaml = "data:\n  [\n    a,\n    b\n# last\n  ]\nz: 1"
        Expect.isTrue (try (Reader.read yaml |> ignore; true) with _ -> false) "Reading should not throw."
        let data, z =
            yaml
            |> Decode.read
            |> Decode.object (fun get ->
                let data = get.Required.Field "data" (Decode.list Decode.string)
                let z = get.Required.Field "z" Decode.int
                data, z
            )
        Expect.equal data ["a"; "b"] "The comment should not add or drop flow items."
        Expect.equal z 1 "The key after the flow sequence should keep its value."
        let tree = Reader.read yaml
        let keepsComment =
            match tree with
            | YAMLElement.Object (YAMLElement.Mapping(_, dataValue) :: _) ->
                match dataValue with
                | YAMLElement.Sequence items
                | YAMLElement.Object [YAMLElement.Sequence items] ->
                    items |> List.contains (YAMLElement.Comment " last")
                | _ -> false
            | _ -> false
        Expect.isTrue keepsComment (sprintf "The comment should be a Comment entry of the sequence under data. Actual tree: %A" tree)

    testCase "Commented-out last entry of a multi-line flow sequence" <| fun _ ->
        let yaml = "baseCommand:\n  [\n    python,\n#   old,\n  ]"
        let actual =
            yaml
            |> Decode.read
            |> Decode.object (fun get -> get.Required.Field "baseCommand" (Decode.list Decode.string))
        Expect.equal actual ["python"] "The commented-out entry should not appear in the list."

    testCase "Comment before a flow sequence closer that is not dedented stays in the flow collection" <| fun _ ->
        // yaml@2.6.1 reads both inputs as {"a":[["1"]]}
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("a"),
                YAMLElement.Object [
                    YAMLElement.Sequence [
                        YAMLElement.Object [
                            YAMLElement.Sequence [
                                YAMLElement.Object [YAMLElement.Value(YAMLContent.create("1"))]
                                YAMLElement.Comment(" c")
                            ]
                        ]
                    ]
                ]
            )
        ]
        for yaml in [ "a:\n  - [\n    1,\n# c\n    ]\n"; "a:\n  - [\n    1,\n  # c\n    ]\n" ] do
            Expect.equal (Reader.read yaml) expected $"The comment should stay a Comment entry of the flow sequence in {yaml}."

    testCase "Comment before a flow mapping closer that is not dedented stays in the flow collection" <| fun _ ->
        // yaml@2.6.1 reads the input as [{"x":1}]
        let yaml = "- {\n    x: 1,\n# c\n    }\n"
        let expected = YAMLElement.Object [
            YAMLElement.Sequence [
                YAMLElement.Object [
                    YAMLElement.Mapping(
                        YAMLContent.create("x"),
                        YAMLElement.Object [YAMLElement.Value(YAMLContent.create("1"))]
                    )
                    YAMLElement.Comment(" c")
                ]
            ]
        ]
        Expect.equal (Reader.read yaml) expected "The comment should stay a Comment entry of the flow mapping."

    testCase "Comment after the last item at the end of a block stays in the sequence" <| fun _ ->
        let yaml = "files:\n  - a\n  # c2"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("files"),
                YAMLElement.Object [
                    YAMLElement.Sequence [
                        YAMLElement.Object [YAMLElement.Value(YAMLContent.create("a"))]
                        YAMLElement.Comment(" c2")
                    ]
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "A comment with nothing after it in the block should stay in the sequence."

    testCase "Comment after the last item of a root sequence stays in the sequence" <| fun _ ->
        let yaml = "- 1\n# end"
        let expected = YAMLElement.Object [
            YAMLElement.Sequence [
                YAMLElement.Object [YAMLElement.Value(YAMLContent.create("1"))]
                YAMLElement.Comment(" end")
            ]
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "A trailing comment of a root sequence should stay in the sequence."

    testCase "Quoted key with a hash keeps its block scalar content" <| fun _ ->
        let yaml = "\"a #b\": |\n  'it''s'\n"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("a #b", style=ScalarStyle.DoubleQuoted),
                YAMLElement.Value(
                    YAMLContent.create(
                        "'it''s'\n",
                        style=ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Clip, None)
                    )
                )
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "The hash inside the quoted key should not hide the block scalar header."

    testCase "Single-quoted key with a hash keeps its block scalar content" <| fun _ ->
        let yaml = "'x #y': |\n  'a\n  b'\n"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("x #y", style=ScalarStyle.SingleQuoted),
                YAMLElement.Value(
                    YAMLContent.create(
                        "'a\nb'\n",
                        style=ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Clip, None)
                    )
                )
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "The hash inside the single-quoted key should not hide the block scalar header."

    testCase "Commented-out header after a sequence dash opens no block scalar" <| fun _ ->
        let yaml = "- # note: |\n  k: 'it''s'\n"
        let expected = YAMLElement.Object [
            YAMLElement.Sequence [
                YAMLElement.Object [
                    YAMLElement.Comment(" note: |")
                    YAMLElement.Mapping(
                        YAMLContent.create("k"),
                        YAMLElement.Object [YAMLElement.Value(YAMLContent.create("it's", style=ScalarStyle.SingleQuoted))]
                    )
                ]
            ]
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "The quote pass should run on the line after a commented-out header."

    testCase "Block scalar error names the comment and the lines" <| fun _ ->
        // yaml@2.6.1 rejects the input with BAD_INDENT
        let yaml = "a: >-\n  text\n# c\n  more\n"
        Expect.throws (fun () -> Reader.read yaml |> ignore) "A comment line ends a block scalar, so the more indented text after it is invalid."
        Expect.throws (fun () -> Preprocessing.read yaml |> ignore) "Preprocessing finds the boundary before the Reader runs."
        let message =
            try
                Reader.read yaml |> ignore
                ""
            with e ->
                e.Message
        Expect.stringContains message "(# c)" "The error should print the comment text."
        Expect.stringContains message "(Scalar line 1; comment line 3; content line 4.)" "The error should print the source lines."

    testCase "Quotes behind a bracket in a plain value are text" <| fun _ ->
        // yaml@2.6.1: {"k":"o[\"k\"]"}, {"k":"$(o['k'] || [1, 2])"} and {"k":"$(inputs[\"my-input\"].path)"}
        for (yaml, value) in [ ("k: o[\"k\"]", "o[\"k\"]"); ("k: $(o['k'] || [1, 2])", "$(o['k'] || [1, 2])"); ("k: $(inputs[\"my-input\"].path)", "$(inputs[\"my-input\"].path)") ] do
            Expect.equal (Reader.read yaml) (plainMapping "k" value) $"The quotes in {yaml} are part of the value."

    testCase "Quotes behind a comma or a brace in a plain value are text" <| fun _ ->
        // yaml@2.6.1: {"k":"a, \"b\""} and {"k":"x {\"a\", 'b'}"}. The input `k: x {"a": 1}` is invalid
        // there (BLOCK_AS_IMPLICIT_KEY), so a valid input with a brace stands in for it.
        Expect.equal (Reader.read "k: a, \"b\"") (plainMapping "k" "a, \"b\"") "comma"
        Expect.equal (Reader.read "k: x {\"a\", 'b'}") (plainMapping "k" "x {\"a\", 'b'}") "brace"

    testCase "Quotes in single-line flow collections stay quoted scalars" <| fun _ ->
        // yaml@2.6.1: {"k":["a","b"]}, {"k":{"a":"b"}}, {"k":["x","y, z"]} and [["a","b"]]
        let quoted (style: ScalarStyle) (value: string) = YAMLElement.Object [YAMLElement.Value(YAMLContent.create(value, style=style))]
        let inMapping (value: YAMLElement) = YAMLElement.Object [YAMLElement.Mapping(YAMLContent.create("k"), YAMLElement.Object [value])]
        let doubled = ScalarStyle.DoubleQuoted
        Expect.equal
            (Reader.read "k: [\"a\", 'b']")
            (inMapping (YAMLElement.Sequence [quoted doubled "a"; quoted ScalarStyle.SingleQuoted "b"]))
            "sequence with both quote kinds"
        Expect.equal
            (Reader.read "k: {\"a\": \"b\"}")
            (inMapping (YAMLElement.Mapping(YAMLContent.create("a", style=doubled), quoted doubled "b")))
            "mapping"
        Expect.equal
            (Reader.read "k: [x, \"y, z\"]")
            (inMapping (YAMLElement.Sequence [YAMLElement.Object [YAMLElement.Value(YAMLContent.create("x"))]; quoted doubled "y, z"]))
            "a comma inside a quoted item"
        Expect.equal
            (Reader.read "- [\"a\", \"b\"]")
            (YAMLElement.Object [YAMLElement.Sequence [YAMLElement.Object [YAMLElement.Sequence [quoted doubled "a"; quoted doubled "b"]]]])
            "sequence in a sequence"

    testCase "A quote behind a property at a node start is a quoted scalar" <| fun _ ->
        // yaml@2.6.1: {"k":"x"}
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("k"),
                YAMLElement.Object [YAMLElement.Value(YAMLContent.create("x", tag="tag:yaml.org,2002:str", style=ScalarStyle.DoubleQuoted))]
            )
        ]
        Expect.equal (Reader.read "k: !!str \"x\"") expected "The tag and the quotes are kept."

    testCase "Decode.string keeps the quotes of a CWL expression" <| fun _ ->
        // yaml@2.6.1: {"valueFrom":"$(inputs[\"my-input\"].path)"}
        let yaml = "valueFrom: $(inputs[\"my-input\"].path)"
        let actual = Decode.read yaml |> Decode.object (fun get -> get.Required.Field "valueFrom" Decode.string)
        Expect.equal actual "$(inputs[\"my-input\"].path)" "The index key keeps its quotes."

    testCase "Decode.string folds a plain CWL expression over two lines" <| fun _ ->
        // yaml@2.6.1: {"valueFrom":"$(inputs.a + \"x\")"} and {"valueFrom":"$(f(inputs.a, \"x\", 1))"}.
        // The closing quote of the first input is not at a token boundary, so only the second one
        // needs the continuation rule.
        let decode (yaml: string) = Decode.read yaml |> Decode.object (fun get -> get.Required.Field "valueFrom" Decode.string)
        Expect.equal (decode "valueFrom: $(inputs.a +\n  \"x\")") "$(inputs.a + \"x\")" "The line break folds into a space and the quotes stay."
        Expect.equal (decode "valueFrom: $(f(inputs.a,\n  \"x\", 1))") "$(f(inputs.a, \"x\", 1))" "A quote behind a comma on the second line stays."
]

