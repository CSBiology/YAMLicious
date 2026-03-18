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

    testCase "SequenceObjects" <| fun _ ->
        let ele = YAMLElement.Object [
            YAMLElement.Sequence[
                YAMLElement.Object [
                    YAMLElement.Value (YAMLContent.create("My Value 1"))
                    YAMLElement.Value (YAMLContent.create("My Value 2"))
                ];
                YAMLElement.Value(YAMLContent.create("My Value 3"))
            ]
        ]
        let actual = write ele None
        let expected = "- My Value 1
  My Value 2
- My Value 3"
        Expect.trimEqual actual expected ""

    testCase "SequenceImplicit" <| fun _ ->
        let ele = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("My Key"),
                YAMLElement.Object [
                    YAMLElement.Value(YAMLContent.create("My Value1"));
                    YAMLElement.Value(YAMLContent.create("My Value2"));
                    YAMLElement.Value(YAMLContent.create("My Value3"))
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
]
