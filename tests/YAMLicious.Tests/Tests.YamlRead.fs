module Tests.YamlRead

open Fable.Pyxpecto
open YAMLicious
open YAMLiciousTypes
open Preprocessing
open YAMLiciousTypes

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
        let yaml = "Say: [Hello, World]# 420 blaze it"
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
                        YAMLElement.Object[
                            YAMLElement.Comment("ich hab das gerade gesehen und dachte mir \"ah fuck\"")
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
                        YAMLElement.Object [
                            YAMLElement.Comment(" between")
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
        let yaml = """
$namespaces:
  arc: https://github.com/nfdi4plants/ARC_ontology
  test: https://github.com/nfdi4plants/TEST_ontology
"""
        let expected = YAMLElement.Object [
            YAMLElement.Sequence[
                YAMLElement.Object [
                    YAMLElement.Value(YAMLContent.create("namespaces"));
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
            ]
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected ""

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
            | YAMLElement.Object [YAMLElement.Comment c] -> Expect.equal c " one" "Flow comment placeholder should restore to a comment"
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
]

