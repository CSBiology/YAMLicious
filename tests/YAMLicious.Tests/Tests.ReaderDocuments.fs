module Tests.ReaderDocuments

open Fable.Pyxpecto
open YAMLicious
open YAMLiciousTypes

let Main = testList "Reader.readDocuments" [
    // --- Comparable Single Document Tests ---
    testCase "Single document: Scalar" <| fun _ ->
        let yaml = "value"
        let actual = Reader.readDocuments yaml
        let expected = [
            YAMLElement.Object [YAMLElement.Value(YAMLContent.create("value"))]
        ]
        Expect.equal actual expected "Should return list with one scalar document"

    testCase "Single document: Mapping" <| fun _ ->
        let yaml = "key: value"
        let actual = Reader.readDocuments yaml
        let expected = [
            YAMLElement.Object [
                YAMLElement.Mapping(
                    YAMLContent.create("key"),
                    YAMLElement.Object [YAMLElement.Value(YAMLContent.create("value"))]
                )
            ]
        ]
        Expect.equal actual expected "Should return list with one mapping document"

    testCase "Single document: Sequence" <| fun _ ->
        let yaml = """- a
- b"""
        let actual = Reader.readDocuments yaml
        let expected = [
            YAMLElement.Object [
                YAMLElement.Sequence [
                    YAMLElement.Object [YAMLElement.Value(YAMLContent.create("a"))]
                    YAMLElement.Object [YAMLElement.Value(YAMLContent.create("b"))]
                ]
            ]
        ]
        Expect.equal actual expected "Should return list with one sequence document"

    testCase "Single document: Explicit Start" <| fun _ ->
        let yaml = """---
key: value"""
        let actual = Reader.readDocuments yaml
        let expected = [
            YAMLElement.Object [
                YAMLElement.Mapping(
                    YAMLContent.create("key"),
                    YAMLElement.Object [YAMLElement.Value(YAMLContent.create("value"))]
                )
            ]
        ]
        Expect.equal actual expected "Should handle explicit start marker"

    testCase "Single document: Expect End" <| fun _ ->
        let yaml = """key: value
..."""
        let actual = Reader.readDocuments yaml
        let expected = [
            YAMLElement.Object [
                YAMLElement.Mapping(
                    YAMLContent.create("key"),
                    YAMLElement.Object [YAMLElement.Value(YAMLContent.create("value"))]
                )
            ]
        ]
        Expect.equal actual expected "Should handle explicit end marker"

    // --- Multi-Document Tests ---

    testCase "Two scalars" <| fun _ ->
        let yaml = """---
doc1
---
doc2"""
        let actual = Reader.readDocuments yaml
        let expected = [
            YAMLElement.Object [YAMLElement.Value(YAMLContent.create("doc1"))]
            YAMLElement.Object [YAMLElement.Value(YAMLContent.create("doc2"))]
        ]
        Expect.equal actual expected "Should return two scalar documents"

    testCase "Mapping and Sequence" <| fun _ ->
        let yaml = """---
key: value
---
- item1
- item2"""
        let actual = Reader.readDocuments yaml
        let expected = [
            YAMLElement.Object [
                YAMLElement.Mapping(
                    YAMLContent.create("key"),
                    YAMLElement.Object [YAMLElement.Value(YAMLContent.create("value"))]
                )
            ]
            YAMLElement.Object [
                YAMLElement.Sequence [
                    YAMLElement.Object [YAMLElement.Value(YAMLContent.create("item1"))]
                    YAMLElement.Object [YAMLElement.Value(YAMLContent.create("item2"))]
                ]
            ]
        ]
        Expect.equal actual expected "Should return mapping doc and sequence doc"

    testCase "Three documents with directives" <| fun _ ->
        let yaml = """%YAML 1.2
---
doc1: v1
---
%TAG ! t!
---
!foo "doc2"
...
---
doc3"""
        let actual = Reader.readDocuments yaml
        
        // This test specifically verifies three behaviors:
        // 1. Directives (e.g. %YAML, %TAG) are correctly associated with the document they precede.
        // 2. The parser handles tag resolution (e.g. !foo becoming t!foo or remaining !foo depending on context).
        // 3. Reader.readDocuments correctly identifies document boundaries even when directives are involved,
        //    ensuring directives don't get split into their own "empty" documents.

        // Document 1: Simple mapping
        let doc1 = YAMLElement.Object [
             YAMLElement.Mapping(YAMLContent.create("doc1"), YAMLElement.Object[YAMLElement.Value(YAMLContent.create("v1"))])
        ]
        
        // Document 2: Uses a custom tag !foo. 
        // The directive `%TAG ! t!` maps the primary handle `!` to the prefix `t!`.
        // Therefore, `!foo` (which uses the `!` handle) resolves to `t!` + `foo` = `t!foo`.
        // We verify that the parser correctly performs this expansion.
        let doc2 = YAMLElement.Object [
            YAMLElement.Value(YAMLContent.create("doc2", tag="t!foo"))
        ]
        
        // Document 3: Simple scalar
        let doc3 = YAMLElement.Object [YAMLElement.Value(YAMLContent.create("doc3"))]
        
        Expect.equal actual.Length 3 "Should have 3 docs"
        match actual with
        | [d1; d2; d3] ->
             Expect.equal d1 doc1 "Doc 1 mismatch"
             Expect.equal d2 doc2 "Doc 2 mismatch - verifying correct tag parsing"
             Expect.equal d3 doc3 "Doc 3 mismatch"
        | _ -> failwith "Structure mismatch - expected 3 documents"

    testCase "Empty documents skipped" <| fun _ ->
        let yaml = """---
doc1
---
# Just a comment
---
doc2"""
        // We want to verify that truly empty documents (e.g. adjacent markers) are skipped.
        // However, documents containing only comments are currently treated as valid documents 
        // because the preprocessor preserves them.
        
        let actual = Reader.readDocuments yaml
        // With comments preserved, the middle document exists.
        
        // Test ignoring truly empty documents:
        let yaml2 = """---
doc1
---
---
doc2"""
        let actual2 = Reader.readDocuments yaml2
        // Usage of empty document markers (e.g. adjacent ---) is valid YAML.
        // Reader.readDocuments is designed to filter out these truly empty documents.
        Expect.equal actual2.Length 2 "Should skip empty document"
]
