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
]

