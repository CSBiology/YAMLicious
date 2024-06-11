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
            YAMLElement.Mapping(YAMLContent.create("Say"), YAMLElement.Value(YAMLContent.create("Hello World")))
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected ""

    testCase "KeyValue + Comment" <| fun _ ->
        let yaml = "Say: Hello World # 420 blaze it"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(YAMLContent.create("Say"), YAMLElement.Value(YAMLContent.create("Hello World", " 420 blaze it")))
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected ""

    testCase "KeyValue InlineSequence" <| fun _ ->
        let yaml = "Say: [Hello, World]"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("Say"),
                YAMLElement.Sequence[
                    YAMLElement.Value(YAMLContent.create("Hello"));
                    YAMLElement.Value(YAMLContent.create("World"))
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
                        YAMLElement.Value(YAMLContent.create("Hello"));
                        YAMLElement.Value(YAMLContent.create("World"))
                    ]
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected ""

    testCase "Sequence" <| fun _ ->
        let yaml = """
- My Value 1
- My Value 2
- My Value 3
"""
        let expected = YAMLElement.Object [
            YAMLElement.Sequence[
                YAMLElement.Value(YAMLContent.create("My Value 1"));
                YAMLElement.Value(YAMLContent.create("My Value 2"));
                YAMLElement.Value(YAMLContent.create("My Value 3"))
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
                    YAMLElement.Value (YAMLContent.create("My Value 1"))
                    YAMLElement.Value (YAMLContent.create("My Value 2"))
                ];
                YAMLElement.Value(YAMLContent.create("My Value 3"))
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
                    YAMLElement.Value(YAMLContent.create("My Value1"));
                    YAMLElement.Value(YAMLContent.create("My Value2"));
                    YAMLElement.Value(YAMLContent.create("My Value3"))
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected ""

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
                YAMLElement.Value(YAMLContent.create("v1"));
                YAMLElement.Value(YAMLContent.create("v2"));
                YAMLElement.Value(YAMLContent.create("v3"))
            ]
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected ""
]