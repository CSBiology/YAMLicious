module Tests.YamlWrite

open Fable.Pyxpecto
open YAMLicious
open YAMLicious.YAMLiciousTypes
open YAMLicious.Writer

open Util

let Main = testList "YamlWrite" [
    testCase "Value" <| fun _ ->
        let ele = YAMLElement.Object [YAMLElement.Value(YAMLContent.create("Hello World"))]
        let actual = write ele None
        let expected = "Hello World"
        Expect.trimEqual actual expected ""

    testCase "KeyValue" <| fun _ ->
        let ele = YAMLElement.Object [
            YAMLElement.Mapping(YAMLContent.create("Say"), YAMLElement.Value(YAMLContent.create("Hello World")))
        ]
        let actual = write ele None
        let expected = "Say: Hello World"
        Expect.trimEqual actual expected ""

    testCase "KeyValue + Comment" <| fun _ ->
        let ele = YAMLElement.Object [
            YAMLElement.Mapping(YAMLContent.create("Say"), YAMLElement.Value(YAMLContent.create("Hello World", " 420 blaze it")))
        ]
        let actual = write ele None
        let expected = "Say: Hello World # 420 blaze it"
        Expect.trimEqual actual expected ""

    testCase "KeyValue InlineSequence" <| fun _ ->
        let ele = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("Say"),
                YAMLElement.Sequence[
                    YAMLElement.Value(YAMLContent.create("Hello"));
                    YAMLElement.Value(YAMLContent.create("World"))
                ]
            )
        ]
        let actual = write ele None
        let expected = "Say: [Hello, World]"
        Expect.trimEqual actual expected ""

    testCase "KeyValue InlineSequence + Comment" <| fun _ ->
        let ele = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("Say"),
                YAMLElement.Object [
                    YAMLElement.Comment(" 420 blaze it");
                    YAMLElement.Sequence[
                        YAMLElement.Value(YAMLContent.create("Hello"));
                        YAMLElement.Value(YAMLContent.create("World"))
                    ]
                ]
            )
        ]
        let actual = write ele None
        let expected = "Say:
    # 420 blaze it
    - Hello
    - World"
        Expect.trimEqual actual expected ""

    testCase "Sequence" <| fun _ ->
        let ele = YAMLElement.Object [
            YAMLElement.Sequence[
                YAMLElement.Value(YAMLContent.create("My Value 1"));
                YAMLElement.Value(YAMLContent.create("My Value 2"));
                YAMLElement.Value(YAMLContent.create("My Value 3"))
            ]
        ]
        let actual = write ele None
        let expected = "- My Value 1
- My Value 2
- My Value 3"
        Expect.trimEqual actual expected ""

    // yaml@2.6.1 reads the expected text as ["My Value 1 My Value 2", "My Value 3"].
    testCase "SequenceObjects" <| fun _ ->
        let ele = YAMLElement.Object [
            YAMLElement.Sequence[
                YAMLElement.Object [
                    YAMLElement.Value (YAMLContent.create("My Value 1\nMy Value 2", style=ScalarStyle.Plain))
                ];
                YAMLElement.Value(YAMLContent.create("My Value 3"))
            ]
        ]
        let actual = write ele None
        let expected = "- My Value 1
  My Value 2
- My Value 3"
        Expect.trimEqual actual expected ""

    // yaml@2.6.1 reads the expected text as {"My Key": "My Value1 My Value2 My Value3"}.
    testCase "SequenceImplicit" <| fun _ ->
        let ele = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("My Key"),
                YAMLElement.Object [
                    YAMLElement.Value(YAMLContent.create("My Value1\nMy Value2\nMy Value3", style=ScalarStyle.Plain))
                ]
            )
        ]
        let actual = write ele None
        let expected = "My Key: My Value1
  My Value2
  My Value3"
        Expect.trimEqual actual expected ""

    testCase "NextLineSequenceObjects" <| fun _ ->
        let ele = YAMLElement.Object [
            YAMLElement.Sequence[
                YAMLElement.Object[
                    YAMLElement.Mapping(
                        YAMLContent.create("My Key1"),
                        YAMLElement.Value(YAMLContent.create("My Value1"))
                    );
                    YAMLElement.Mapping(
                        YAMLContent.create("My Key2"),
                        YAMLElement.Value(YAMLContent.create("My Value2"))
                    );
                    YAMLElement.Mapping(
                        YAMLContent.create("My Key3"),
                        YAMLElement.Value(YAMLContent.create("My Value3"))
                    )
                ]
                YAMLElement.Object[
                        YAMLElement.Mapping(
                            YAMLContent.create("My Key4"),
                            YAMLElement.Value(YAMLContent.create("My Value4"))
                        );
                        YAMLElement.Mapping(
                            YAMLContent.create("My Key5"),
                            YAMLElement.Value(YAMLContent.create("My Value5"))
                        );
                        YAMLElement.Mapping(
                            YAMLContent.create("My Key6"),
                            YAMLElement.Value(YAMLContent.create("My Value6"))
                        )
                    ]
            ]
        ]
        let actual = write ele None
        let expected = "-
    My Key1: My Value1
    My Key2: My Value2
    My Key3: My Value3
-
    My Key4: My Value4
    My Key5: My Value5
    My Key6: My Value6"
        Expect.trimEqual actual expected ""

    testCase "SequenceofSequences" <| fun _ ->
        let ele = YAMLElement.Object [
            YAMLElement.Sequence[
                YAMLElement.Sequence[
                    YAMLElement.Value(YAMLContent.create("v1"));
                    YAMLElement.Value(YAMLContent.create("v2"));
                    YAMLElement.Value(YAMLContent.create("v3"))
                ]
                YAMLElement.Sequence[
                    YAMLElement.Value(YAMLContent.create("v4"));
                    YAMLElement.Value(YAMLContent.create("v5"));
                    YAMLElement.Value(YAMLContent.create("v6"))
                ]
                YAMLElement.Sequence[
                    YAMLElement.Value(YAMLContent.create("v7"));
                    YAMLElement.Value(YAMLContent.create("v8"));
                    YAMLElement.Value(YAMLContent.create("v9"))
                ]
            ]
        ]
        let actual = write ele None
        let expected = "- [v1, v2, v3]
- [v4, v5, v6]
- [v7, v8, v9]"
        Expect.trimEqual actual expected ""

    testCase "MultilineSequenceSquare" <| fun _ ->
        let ele = YAMLElement.Object [
            YAMLElement.Sequence[
                YAMLElement.Value(YAMLContent.create("v1"));
                YAMLElement.Value(YAMLContent.create("v2"));
                YAMLElement.Value(YAMLContent.create("v3"))
            ]
        ]
        let actual = write ele None
        let expected = "- v1
- v2
- v3"
        Expect.trimEqual actual expected ""

    testCase "DocumentStart writes ---" <| fun _ ->
        let ele = YAMLElement.Object [
            YAMLElement.DocumentStart
            YAMLElement.Mapping(YAMLContent.create("key"), YAMLElement.Value(YAMLContent.create("value")))
        ]
        let actual = write ele None
        let expected = "---\nkey: value"
        Expect.trimEqual actual expected "DocumentStart should be written as ---"

    testCase "DocumentEnd writes ..." <| fun _ ->
        let ele = YAMLElement.Object [
            YAMLElement.Mapping(YAMLContent.create("key"), YAMLElement.Value(YAMLContent.create("value")))
            YAMLElement.DocumentEnd
        ]
        let actual = write ele None
        let expected = "key: value\n..."
        Expect.trimEqual actual expected "DocumentEnd should be written as ..."

    testCase "Alias round-trip write" <| fun _ ->
        let ele = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("first"),
                YAMLElement.Object [YAMLElement.Value(YAMLContent.create("val", anchor="a1"))]
            )
            YAMLElement.Mapping(
                YAMLContent.create("second"),
                YAMLElement.Object [YAMLElement.Alias("a1")]
            )
        ]
        let actual = write ele None
        let expected = "first: &a1 val\nsecond: *a1"
        Expect.trimEqual actual expected "Alias should be written as *name"

    testCase "Double-quoted escapes control characters" <| fun _ ->
        let ele = YAMLElement.Object [
            YAMLElement.Value(YAMLContent.create("null\u0000bell\u0007back\u0008", style=ScalarStyle.DoubleQuoted))
        ]
        let actual = write ele None
        Expect.equal (actual.Contains("\\0")) true "Null byte should be escaped"
        Expect.equal (actual.Contains("\\a")) true "Bell should be escaped"
        Expect.equal (actual.Contains("\\b")) true "Backspace should be escaped"

    testCase "Comment element inside a sequence is written as a comment line" <| fun _ ->
        let ele = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("a"),
                YAMLElement.Object [
                    YAMLElement.Sequence [
                        YAMLElement.Object [YAMLElement.Value(YAMLContent.create("1"))]
                        YAMLElement.Comment(" x")
                        YAMLElement.Object [YAMLElement.Value(YAMLContent.create("2"))]
                    ]
                ]
            )
        ]
        let actual = write ele None
        let expected = "a:
    - 1
    # x
    - 2"
        Expect.trimEqual actual expected ""
        Expect.equal (Reader.read actual) (Reader.read "a:\n  - 1\n  # x\n  - 2") "The written comment should read back at the same position."

    testCase "Comments round-trip through write and read" <| fun _ ->
        let inputs = [
            "r:\n  a:\n    # c\n    b: 1"
            "a:\n- 1\n# end\nb: 2"
            "lang: C#\nrun: tool.cwl#main\nnote: a #b"
            "a:\n  - 1\n  # x\n  - 2"
        ]
        for input in inputs do
            let parsed = Reader.read input
            let reparsed = Reader.read (write parsed None)
            let label = input.Replace("\n", "\\n")
            Expect.equal reparsed parsed $"Round trip should keep the tree for: {label}"

    testCase "Trailing comment after a nested sequence round-trips" <| fun _ ->
        let s = "r:\n  a:\n    - 1\n    # end"
        Expect.equal (Reader.read (Writer.write (Reader.read s) None)) (Reader.read s) "The trailing comment should survive write and read."

    // yaml@2.6.1 reads `k: &a` as {"k":null}, `k: !!str` as {"k":""}, `- &a` as [null], `- !!null` as [null], the
    // root `&a` as null and `k: &a # c` as {"k":null}. The writer spells a core tag in its verbatim form.
    testCase "An empty plain scalar with properties is written as its properties alone" <| fun _ ->
        let cases =
            [ "k: &a", "k: &a"
              "k: !!str", "k: !<tag:yaml.org,2002:str>"
              "- &a", "- &a"
              "- !!null", "- !<tag:yaml.org,2002:null>"
              "&a", "&a"
              "k: &a # c", "k: &a # c" ]
        for (source, expected) in cases do
            let parsed = Reader.read source
            let written = write parsed None
            Expect.trimEqual written expected $"The properties of {source} stand alone"
            Expect.equal (Reader.read written) parsed $"The text of {source} reads back as the same element"

    // yaml@2.6.1 reads `k: !!str ""` as {"k":""}, `k: &a ''` as {"k":""} and `- &a ""` as [""].
    testCase "An empty quoted scalar with properties keeps its quotes" <| fun _ ->
        let cases =
            [ "k: !!str \"\"", "k: !<tag:yaml.org,2002:str> \"\""
              "k: &a ''", "k: &a ''"
              "- &a \"\"", "- &a \"\"" ]
        for (source, expected) in cases do
            let parsed = Reader.read source
            let written = write parsed None
            Expect.trimEqual written expected $"The quotes of {source} stay"
            Expect.equal (Reader.read written) parsed $"The text of {source} reads back as the same element"

    // yaml@2.6.1 reads `k: [&a, b]` as {"k":[null,"b"]} and `k: [b, &a]` as {"k":["b",null]}.
    testCase "An empty plain item of a flow sequence is written as its properties alone" <| fun _ ->
        let anchored = YAMLElement.Value(YAMLContent.create("", anchor="a"))
        let b = YAMLElement.Value(YAMLContent.create "b")
        let inMapping (items: YAMLElement list) =
            YAMLElement.Object [ YAMLElement.Mapping(YAMLContent.create "k", YAMLElement.Sequence items) ]
        let readBack (items: YAMLElement list) =
            YAMLElement.Object [ YAMLElement.Mapping(YAMLContent.create "k", YAMLElement.Object [ YAMLElement.Sequence (items |> List.map (fun item -> YAMLElement.Object [ item ])) ]) ]
        let first = write (inMapping [ anchored; b ]) None
        Expect.trimEqual first "k: [&a, b]" "The anchor stands in front of the comma"
        Expect.equal (Reader.read first) (readBack [ anchored; b ]) "The first item reads back as an empty scalar with the anchor"
        let last = write (inMapping [ b; anchored ]) None
        Expect.trimEqual last "k: [b, &a]" "The anchor stands in front of the bracket"
        Expect.equal (Reader.read last) (readBack [ b; anchored ]) "The last item reads back as an empty scalar with the anchor"
]
