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

    testCase "Single document: Explicit Start with comment" <| fun _ ->
        // yaml@2.6.1: {"key":"value"}. The comment is no data and stays a comment of the document.
        let yaml = """--- # start
key: value"""
        let actual = Reader.readDocuments yaml
        let expected = [
            YAMLElement.Object [
                YAMLElement.Comment(" start")
                YAMLElement.Mapping(
                    YAMLContent.create("key"),
                    YAMLElement.Object [YAMLElement.Value(YAMLContent.create("value"))]
                )
            ]
        ]
        Expect.equal actual expected "Start marker with inline comment should be recognized"

    testCase "Single document: Explicit Start with inline root node" <| fun _ ->
        let yaml = "--- foo"
        let actual = Reader.readDocuments yaml
        let expected = [
            YAMLElement.Object [YAMLElement.Value(YAMLContent.create("foo"))]
        ]
        Expect.equal actual expected "Inline content after start marker should become document content"

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
        // yaml@2.6.1: {"doc1":"v1"}, "doc2" and "doc3". A directive needs the `...` of the document before it.
        let yaml = """%YAML 1.2
---
doc1: v1
...
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
            YAMLElement.Value(YAMLContent.create("doc2", tag="t!foo", style=ScalarStyle.DoubleQuoted))
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
        let actual = Reader.readDocuments yaml
        let expected = [
            YAMLElement.Object [YAMLElement.Value(YAMLContent.create("doc1"))]
            YAMLElement.Object [YAMLElement.Comment(" Just a comment"); YAMLElement.Nil]
            YAMLElement.Object [YAMLElement.Value(YAMLContent.create("doc2"))]
        ]
        Expect.equal actual expected "Comment-only documents should be preserved as document content, with the null node behind the comments"
        
        // yaml@2.6.1 reads this as ["doc1", null, "doc2"]. A start marker makes a document without a body.
        let yaml2 = """---
doc1
---
---
doc2"""
        let actual2 = Reader.readDocuments yaml2
        Expect.equal
            actual2
            [
                YAMLElement.Object [YAMLElement.Value(YAMLContent.create("doc1"))]
                YAMLElement.Nil
                YAMLElement.Object [YAMLElement.Value(YAMLContent.create("doc2"))]
            ]
            "The empty document between the markers reads as Nil"

    testCase "Directive prelude comments are kept with the same document" <| fun _ ->
        let yaml = """%YAML 1.2
# prelude
---
foo: bar
---
baz: qux"""
        let actual = Reader.readDocuments yaml
        let expected = [
            YAMLElement.Object [
                YAMLElement.Comment(" prelude")
                YAMLElement.Mapping(
                    YAMLContent.create("foo"),
                    YAMLElement.Object [YAMLElement.Value(YAMLContent.create("bar"))]
                )
            ]
            YAMLElement.Object [
                YAMLElement.Mapping(
                    YAMLContent.create("baz"),
                    YAMLElement.Object [YAMLElement.Value(YAMLContent.create("qux"))]
                )
            ]
        ]
        Expect.equal actual expected "The prelude comment belongs to the first document and creates no document of its own"

    testCase "Document markers inside block scalar content do not split documents" <| fun _ ->
        let yaml = """---
text: |
  line1
  ---
  ...
  line2
---
next: ok"""
        let actual = Reader.readDocuments yaml
        let expected = [
            YAMLElement.Object [
                YAMLElement.Mapping(
                    YAMLContent.create("text"),
                    YAMLElement.Value(
                        YAMLContent.create(
                            "line1\n---\n...\nline2\n",
                            style=ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Clip, None)
                        )
                    )
                )
            ]
            YAMLElement.Object [
                YAMLElement.Mapping(
                    YAMLContent.create("next"),
                    YAMLElement.Object [YAMLElement.Value(YAMLContent.create("ok"))]
                )
            ]
        ]
        Expect.equal actual expected "Indented marker-like content in block scalars must remain scalar content"

    testCase "End-marker prefix is not treated as document end" <| fun _ ->
        let yaml = """...foo: bar
next: v"""
        let actual = Reader.readDocuments yaml
        let expected = [
            YAMLElement.Object [
                YAMLElement.Mapping(
                    YAMLContent.create("...foo"),
                    YAMLElement.Object [YAMLElement.Value(YAMLContent.create("bar"))]
                )
                YAMLElement.Mapping(
                    YAMLContent.create("next"),
                    YAMLElement.Object [YAMLElement.Value(YAMLContent.create("v"))]
                )
            ]
        ]
        Expect.equal actual expected "Only standalone ... markers should terminate a document"

    testCase "Leading comments and directives belong to the document they introduce" <| fun _ ->
        // yaml@2.6.1 reads each of the three inputs as one document, {"a":1}
        let expected = [
            YAMLElement.Object [
                YAMLElement.Comment(" lead")
                YAMLElement.Mapping(
                    YAMLContent.create("a"),
                    YAMLElement.Object [YAMLElement.Value(YAMLContent.create("1"))]
                )
            ]
        ]
        for yaml in [ "# lead\n%YAML 1.2\n---\na: 1\n"; "%YAML 1.2\n# lead\n---\na: 1\n"; "# lead\n---\na: 1\n" ] do
            Expect.equal (Reader.readDocuments yaml) expected $"The prelude of {yaml} should stay in the document and create no document of its own."

    testCase "A comment between an end marker and the next start marker is no document" <| fun _ ->
        // yaml@2.6.1: two documents, {"a":1} and {"b":2}
        let yaml = "a: 1\n...\n# c\n---\nb: 2\n"
        let expected = [
            YAMLElement.Object [
                YAMLElement.Mapping(
                    YAMLContent.create("a"),
                    YAMLElement.Object [YAMLElement.Value(YAMLContent.create("1"))]
                )
            ]
            YAMLElement.Object [
                YAMLElement.Comment(" c")
                YAMLElement.Mapping(
                    YAMLContent.create("b"),
                    YAMLElement.Object [YAMLElement.Value(YAMLContent.create("2"))]
                )
            ]
        ]
        Expect.equal (Reader.readDocuments yaml) expected "The comment is the prelude of the second document"

    testCase "A comment behind a directive stays a comment of the document" <| fun _ ->
        // yaml@2.6.1: {"a":"x"}
        let yaml = "%YAML 1.2 # c\n---\na: 'x'\n"
        let expected = [
            YAMLElement.Object [
                YAMLElement.Comment(" c")
                YAMLElement.Mapping(
                    YAMLContent.create("a"),
                    YAMLElement.Object [YAMLElement.Value(YAMLContent.create("x", style=ScalarStyle.SingleQuoted))]
                )
            ]
        ]
        Expect.equal (Reader.readDocuments yaml) expected "The directive applies to the document and the comment stays in it"

    testCase "A prelude comment stays with the document it introduces" <| fun _ ->
        // yaml@2.6.1 reads both inputs as the two documents {"a":1} and {"b":2}
        let first = YAMLElement.Mapping(YAMLContent.create("a"), YAMLElement.Object [YAMLElement.Value(YAMLContent.create("1"))])
        let second = YAMLElement.Mapping(YAMLContent.create("b"), YAMLElement.Object [YAMLElement.Value(YAMLContent.create("2"))])
        Expect.equal
            (Reader.readDocuments "---\na: 1\n...\n# intro\n---\nb: 2\n")
            [ YAMLElement.Object [first]; YAMLElement.Object [YAMLElement.Comment(" intro"); second] ]
            "A comment behind an end marker belongs to the next document"
        Expect.equal
            (Reader.readDocuments "---\na: 1\n# tail\n---\nb: 2\n")
            [ YAMLElement.Object [first; YAMLElement.Comment(" tail")]; YAMLElement.Object [second] ]
            "A comment behind content belongs to the document of that content"

    testCase "A directive behind a start marker is rejected" <| fun _ ->
        // yaml@2.6.1 rejects all three inputs with BAD_SCALAR_START, because the marker ended the prelude
        // and a plain scalar cannot start with a percent sign, also not as the key of a block scalar header
        Expect.throws
            (fun () -> Reader.readDocuments "---\na: 1\n---\n%TAG ! t!\n---\nb: 2\n" |> ignore)
            "A directive after a bare start marker is no directive"
        Expect.throws
            (fun () -> Reader.readDocuments "---\n# c\n%YAML 1.2\n" |> ignore)
            "A comment between the marker and the directive changes nothing"
        Expect.throws
            (fun () -> Reader.readDocuments "---\n%a: |\n  x\n" |> ignore)
            "A block scalar header behind the marker cannot start with a percent sign"
        Expect.throws
            (fun () -> Reader.read "---\n%a: |\n  x\n" |> ignore)
            "Reader.read rejects the same input"

    testCase "A directive behind an end marker applies to the next document" <| fun _ ->
        // yaml@2.6.1: two documents, {"a":1} and {"b":2}
        let yaml = "%YAML 1.2\n---\na: 1\n...\n%YAML 1.2\n---\nb: 2\n"
        let expected = [
            YAMLElement.Object [
                YAMLElement.Mapping(YAMLContent.create("a"), YAMLElement.Object [YAMLElement.Value(YAMLContent.create("1"))])
            ]
            YAMLElement.Object [
                YAMLElement.Mapping(YAMLContent.create("b"), YAMLElement.Object [YAMLElement.Value(YAMLContent.create("2"))])
            ]
        ]
        Expect.equal (Reader.readDocuments yaml) expected "Each document reads its own directive"

    testCase "Multiline quotes and block scalars stay inside their own documents" <| fun _ ->
        // yaml@2.6.1: {"a":"x y"}, {"b":"\"hi\"\n"} and {"c":"z"}
        let yaml = "a: 'x\n  y'\n---\nb: |\n  \"hi\"\n---\nc: \"z\"\n"
        let expected = [
            YAMLElement.Object [
                YAMLElement.Mapping(
                    YAMLContent.create("a"),
                    YAMLElement.Object [YAMLElement.Value(YAMLContent.create("x y", style=ScalarStyle.SingleQuoted))]
                )
            ]
            YAMLElement.Object [
                YAMLElement.Mapping(
                    YAMLContent.create("b"),
                    YAMLElement.Value(
                        YAMLContent.create(
                            "\"hi\"\n",
                            style=ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Clip, None)
                        )
                    )
                )
            ]
            YAMLElement.Object [
                YAMLElement.Mapping(
                    YAMLContent.create("c"),
                    YAMLElement.Object [YAMLElement.Value(YAMLContent.create("z", style=ScalarStyle.DoubleQuoted))]
                )
            ]
        ]
        Expect.equal (Reader.readDocuments yaml) expected "Each document should keep its own strings"

    testCase "Block scalars behind start markers" <| fun _ ->
        // yaml@2.6.1: "x\n" and "y\n"
        let yaml = "--- !!str |\n  x\n--- &a |\n  y\n"
        let expected = [
            YAMLElement.Object [
                YAMLElement.Value(
                    YAMLContent.create(
                        "x\n",
                        tag="tag:yaml.org,2002:str",
                        style=ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Clip, None)
                    )
                )
            ]
            YAMLElement.Object [
                YAMLElement.Value(
                    YAMLContent.create(
                        "y\n",
                        anchor="a",
                        style=ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Clip, None)
                    )
                )
            ]
        ]
        Expect.equal (Reader.readDocuments yaml) expected "The header after a start marker should open a block scalar"

    testCase "A document marker inside a quoted scalar is an error" <| fun _ ->
        // yaml@2.6.1 rejects the input with MISSING_CHAR. A quote never crosses a marker, so the
        // scalar that starts on line 1 never closes.
        Expect.stringContains
            (errorOf (fun () -> Reader.readDocuments "a: 'x\n---\nb: 1\n" |> ignore))
            "Line 1: the quoted scalar that starts here is not closed."
            "The error names the line of the quote"

    // yaml@2.6.1 gives null for an empty input, a blank one, a lone comment, `---` followed by `...` and `---` followed by a
    // comment. The middle document of spec Example 9.6 is null too: ["Document",null,{"matches %":20}]. The comments of a
    // document without a node stay in front of the null node, so that Decode.option can tell the document from `{}`.
    testCase "A document without a node reads as Nil and keeps its comments" <| fun _ ->
        Expect.equal (Reader.read "") YAMLElement.Nil "An empty input"
        Expect.equal (Reader.read "\n\n") YAMLElement.Nil "Blank lines"
        Expect.equal (Reader.read "---\n...") YAMLElement.Nil "A start marker and an end marker"
        Expect.equal (Reader.read "# c") (YAMLElement.Object [ YAMLElement.Comment " c"; YAMLElement.Nil ]) "A comment"
        Expect.equal
            (Reader.read "# c1\n# c2")
            (YAMLElement.Object [ YAMLElement.Comment " c1"; YAMLElement.Comment " c2"; YAMLElement.Nil ])
            "Two comments"
        Expect.equal
            (Reader.readDocuments "---\n# c\n...")
            [ YAMLElement.Object [ YAMLElement.Comment " c"; YAMLElement.Nil ] ]
            "A comment between the markers"
        Expect.equal
            (Reader.readDocuments "Document\n---\n# Empty\n...\n%YAML 1.2\n---\nmatches %: 20")
            [ YAMLElement.Object [ YAMLElement.Value(YAMLContent.create "Document") ]
              YAMLElement.Object [ YAMLElement.Comment " Empty"; YAMLElement.Nil ]
              YAMLElement.Object [ YAMLElement.Mapping(YAMLContent.create "matches %", YAMLElement.Object [ YAMLElement.Value(YAMLContent.create "20") ]) ] ]
            "The middle document of Example 9.6"
        Expect.equal (Reader.read "{}") (YAMLElement.Object []) "The empty flow mapping is no empty document"

    // yaml@2.6.1 parse rejects each of these inputs with "Source contains multiple documents; please use
    // YAML.parseAllDocuments()". It reads `a: 1` followed by `...`, with or without a comment behind it, as {"a":1}.
    testCase "Reader.read rejects input with more than one document" <| fun _ ->
        let inputs = [ "a: 1\n---\nb: 2"; "---\na: 1\n---\nb: 2"; "a: 1\n...\n---\nb: 2"; "---\n---\na: 1"; "a: 1\n---\n"; "# c\n...\na: 1" ]
        for yaml in inputs do
            let message = try (Reader.read yaml |> ignore; "") with e -> e.Message
            let label = yaml.Replace("\n", "|")
            Expect.stringContains message "Use Reader.readDocuments to read all of them" $"{label} holds two documents"
            let decodeMessage = try (Decode.read yaml |> ignore; "") with e -> e.Message
            Expect.stringContains decodeMessage "Use Reader.readDocuments" "Decode.read reads one document too"
        let expected = YAMLElement.Object [ YAMLElement.Mapping(YAMLContent.create "a", YAMLElement.Object [ YAMLElement.Value(YAMLContent.create "1") ]) ]
        Expect.equal (Reader.read "a: 1\n...\n") expected "An end marker closes the only document"
        Expect.equal (Reader.read "a: 1\n...\n# c\n") expected "A comment behind the end marker is no document"

    // yaml@2.6.1 reads `---` followed by `...` and `%YAML 1.2` followed by `---` as [null], `---` twice followed by `a: 1` as
    // [null,{"a":1}], and `%YAML 1.2` alone as no document.
    testCase "A start marker makes a document without a body" <| fun _ ->
        Expect.equal (Reader.readDocuments "---\n...\n") [ YAMLElement.Nil ] "A start and an end marker"
        Expect.equal (Reader.readDocuments "%YAML 1.2\n---\n") [ YAMLElement.Nil ] "A directive and a start marker"
        Expect.equal
            (Reader.readDocuments "---\n---\na: 1")
            [ YAMLElement.Nil
              YAMLElement.Object [ YAMLElement.Mapping(YAMLContent.create "a", YAMLElement.Object [ YAMLElement.Value(YAMLContent.create "1") ]) ] ]
            "An empty document in front of another"
        Expect.equal (Reader.readDocuments "%YAML 1.2\n") [] "A directive alone makes no document"
        Expect.equal (Reader.readDocuments "") [] "An empty input makes no document"

    // yaml@2.6.1 reads {} as {} and the documents above as null.
    testCase "Decode.option reads a document without a node as None" <| fun _ ->
        let decode (yaml: string) = Decode.read yaml |> Decode.option (Decode.map id Decode.string)
        for yaml in [ ""; "\n\n"; "# c"; "# c1\n# c2"; "---\n# c" ] do
            let label = yaml.Replace("\n", "|")
            Expect.equal (decode yaml) None $"The document {label} is null"
        Expect.equal (decode "{}") (Some Map.empty) "The empty flow mapping is no null"

    testCase "A document without a node round trips through the writer" <| fun _ ->
        let written (element: YAMLElement) = (Writer.write element None).Replace("\r\n", "\n")
        Expect.equal (written YAMLElement.Nil) "" "Nil writes nothing"
        Expect.equal (written (YAMLElement.Object [ YAMLElement.Comment " c"; YAMLElement.Nil ])) "# c\n" "The comment stays"
        for yaml in [ ""; "# c"; "# c1\n# c2" ] do
            let parsed = Reader.read yaml
            let label = yaml.Replace("\n", "|")
            Expect.equal (Reader.read (written parsed)) parsed $"The text of the document {label} reads back as the same element"
]
