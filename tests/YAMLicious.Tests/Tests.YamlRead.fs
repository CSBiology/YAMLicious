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
                    YAMLElement.Value (YAMLContent.create("My Value 1"))
                    YAMLElement.Value (YAMLContent.create("My Value 2"))
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
                    YAMLElement.Value(YAMLContent.create("My Value1"));
                    YAMLElement.Value(YAMLContent.create("My Value2"));
                    YAMLElement.Value(YAMLContent.create("My Value3"))
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected ""

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
        let block = System.String.Join("\n", [|
            "DESeq2 example workflow for **differential gene expression analysis**";
            "";
            "This workflow runs DESeq2 on the output of the kallisto workflow";
            "and the metadata file.";
            "It runs an R script, deseq2.R, which ideally should be split into three sub scripts and accordingly three workflow steps";
            "1. Read kallsito data";
            "2. Prep / run deseq2";
            "3. Plot results";
            "## DESeq2 docs:";
            "https://bioconductor.org/packages/release/bioc/html/DESeq2.html";
            "## Importing kallisto output with tximport";
            "https://bioconductor.org/packages/release/bioc/vignettes/tximport/inst/doc/tximport.html#kallisto";
            "## Multi-package containers";
            "- R and combinations of library dependencies are available as multi-package containers from [BioContainers](https://github.com/BioContainers/multi-package-containers)";
            "- Searched for `repo:BioContainers/multi-package-containers deseq2 tximport rhdf5`";
            "- and found `quay.io/biocontainers/mulled-v2-05fd88b9ac812a9149da2f2d881d62f01cc49835:a10f0e3a7a70fc45494f8781d33901086d2214d0-0` :tada:";
        |])
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("doc"),
                YAMLElement.Value (YAMLContent.create(block))
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
]

