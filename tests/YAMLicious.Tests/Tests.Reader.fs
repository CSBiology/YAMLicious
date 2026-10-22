module Tests.Reader

// The reader: one document, several documents, no document and the errors. The parser tests cover the documents themselves (Tests.Documents), so these tests
// check what the reader adds: the choice of the single document, the document that stands for a text without any, the error for a second document and the
// exception that read raises.
//
// Old tests that are ported: Tests.ReaderDocuments "Reader.read rejects input with more than one document", "A document without a node reads as Nil and keeps
// its comments", "A start marker makes a document without a body" and "Decode.option reads a document without a node as None". The first six cases of
// the old rejection list keep their input. One of them changed: `# c`, `...` and `a: 1` is one document in the model, because a `...` that no document
// precedes ends no document (Tests.Documents, "trivia in front of an end marker that ends no document stay in the stream").
//
// Old tests that changed:
// - Reader.read returns a YamlDocument and Reader.readRoot returns its root. The old Nil is a document with a missing root.
// - Reader.readDocuments returns a YamlStream, and Reader.documents lists its documents. A directive alone is an error now, as the parser reports it. The old
//   reader returned no document for `%YAML 1.2`.
// - Decode.read is gone.
//
// Old tests that move: "A document without a node round trips through the writer" needs the writer, so it is in Tests.Writer.

open Fable.Pyxpecto
open YAMLicious

let private expectDocument (actual: YamlDocument) (expected: YamlDocument) (message: string) =
    if not (YamlDocument.equalIgnoringSpans actual expected) then
        failwith (message + "\nActual:\n" + YamlDocument.show actual + "Expected:\n" + YamlDocument.show expected)

let private readOk (text: string) : YamlDocument =
    match Reader.tryRead text with
    | Ok document -> document
    | Error error -> failwith (sprintf "The reader rejects %A: %s" text error.Message)

let private streamOf (text: string) : YamlStream =
    match Reader.tryReadDocuments text with
    | Ok stream -> stream
    | Error error -> failwith (sprintf "The reader rejects %A: %s" text error.Message)

/// The byte order mark, spelled out so that the source file shows it.
let private bom = string (char 0xFEFF)

let private documentsOf (text: string) : YamlDocument list = Reader.documents (Reader.readDocuments text)

let private marker (commentText: string option) : DocumentMarker =
    { TrailingComment = commentText |> Option.map YamlComment.create
      Span = None }

/// A document with a missing root and the given trivia in front of it.
let private missingDocument (trivia: Trivia list) : YamlDocument =
    { YamlDocument.create YamlNode.empty with BeforeRoot = trivia }

let private span (line1: int) (column1: int) (line2: int) (column2: int) : SourceSpan =
    { Start = { Line = line1; Column = column1 }
      Finish = { Line = line2; Column = column2 } }

let private singleTests = testList "A single document" [
    // Tests.ReaderDocuments "Single document: Scalar", "Mapping", "Sequence", "Explicit Start", "Explicit Start with comment", "Explicit Start with inline root node"
    // and "Expect End". yaml@2.6.1: "value", {"key":"value"}, ["a","b"], {"key":"value"} three times, "foo" and {"key":"value"}.
    testCase "read returns the document that the parser makes" <| fun _ ->
        let plain (root: YamlNode) = YamlDocument.create root
        expectDocument (Reader.read "value") (plain (sc "value")) "A scalar"
        expectDocument (Reader.read "key: value") (plain (mapOf [ "key", sc "value" ])) "A mapping"
        expectDocument (Reader.read "- a\n- b") (plain (seqOf [ sc "a"; sc "b" ])) "A sequence"
        let started = { plain (mapOf [ "key", sc "value" ]) with StartMarker = Some(marker None) }
        expectDocument (Reader.read "---\nkey: value") started "An explicit start"
        expectDocument (Reader.read "--- # start\nkey: value") { started with StartMarker = Some(marker (Some " start")) } "A start marker with a comment"
        expectDocument (Reader.read "--- foo") { plain (sc "foo") with StartMarker = Some(marker None) } "An inline root"
        expectDocument (Reader.read "key: value\n...") { plain (mapOf [ "key", sc "value" ]) with EndMarker = Some(marker None) } "An end marker"

    testCase "tryRead, read and readRoot give the same document" <| fun _ ->
        let text = "%YAML 1.2\n---\n# c\na: 1 # t\n"
        let expected = parseDocument text
        expectDocument (readOk text) expected "tryRead"
        expectDocument (Reader.read text) expected "read"
        Expect.nodeEqual (Reader.readRoot text) expected.Root "readRoot"

    // yaml@2.6.1 reads `a: 1` followed by `...`, with or without a comment behind it, as {"a":1}. A comment behind the end marker belongs to no document, so
    // read drops it and readDocuments keeps it.
    testCase "an end marker closes the only document and a comment behind it is no document" <| fun _ ->
        let closed = { YamlDocument.create (mapOf [ "a", sc "1" ]) with EndMarker = Some(marker None) }
        expectDocument (Reader.read "a: 1\n...\n") closed "An end marker"
        expectDocument (Reader.read "a: 1\n...\n# c\n") closed "A comment behind the end marker"
        match (Reader.readDocuments "a: 1\n...\n# c\n").Parts with
        | [ StreamPart.Document _; StreamPart.Trivia _ ] -> ()
        | parts -> failwith (sprintf "The stream holds %d parts, not a document and a comment" parts.Length)

    // `# c`, `...` and `a: 1`: yaml@2.6.1 reads the documents null and {"a":1}. YAML 1.2.2 (9.2) has a document suffix without a document, so the
    // model has one document, the comment and an extra end marker. read returns the only document.
    testCase "an end marker that ends no document makes no second document" <| fun _ ->
        expectDocument (Reader.read "# c\n...\na: 1") (YamlDocument.create (mapOf [ "a", sc "1" ])) "The extra end marker is not part of the document"
        Expect.equal (List.length (documentsOf "# c\n...\na: 1")) 1 "One document"
]

let private emptyTests = testList "A text without a document" [
    // yaml@2.6.1 gives null for an empty input, a blank one, a lone comment, `---` followed by `...` and `---` followed by a comment. A text without any
    // document gives a document with a missing root, and the comments of the stream stay in BeforeRoot in their order.
    testCase "an empty text, blank lines and comments give a document with a missing root" <| fun _ ->
        expectDocument (Reader.read "") (missingDocument []) "An empty input"
        expectDocument (Reader.read "\n\n") (missingDocument [ blank; blank ]) "Blank lines"
        expectDocument (Reader.read "# c") (missingDocument [ note " c" ]) "A comment"
        expectDocument (Reader.read "# c1\n# c2") (missingDocument [ note " c1"; note " c2" ]) "Two comments"
        expectDocument (Reader.read "# c\n\n# d\n") (missingDocument [ note " c"; blank; note " d" ]) "Comments with a blank line between"
        expectDocument (Reader.read bom) (missingDocument []) "A byte order mark alone"
        expectDocument (Reader.read (bom + "# c")) (missingDocument [ note " c" ]) "A byte order mark in front of a comment"

    testCase "the document for a text without any is valid and its root is missing" <| fun _ ->
        for text in [ ""; "\n"; "# c"; "# c1\n\n# c2"; "..."; "# c\n..." ] do
            let document = Reader.read text
            let problems = Validate.document document
            Expect.isTrue problems.IsEmpty (sprintf "%A breaks the slot rules: %s" text (String.concat "; " problems))
            Expect.isTrue (YamlNode.isMissingValue document.Root) (sprintf "The root of %A is missing" text)

    // A `...` with no document in front of it is an extra end marker. The document for the text keeps the first one as its end marker, and every comment and
    // blank line of the stream goes to BeforeRoot, also the ones behind that marker. A single document cannot hold the shape `# a`, `...`, `# b`, `...`, so
    // `# b` moves in front of the first marker and the second marker is lost with its comment. readDocuments keeps the shape.
    testCase "a text without a document keeps its first end marker and moves every comment in front of it" <| fun _ ->
        expectDocument (Reader.read "...") { missingDocument [] with EndMarker = Some(marker None) } "An end marker"
        expectDocument (Reader.read "# c\n... # e") { missingDocument [ note " c" ] with EndMarker = Some(marker (Some " e")) } "A comment and an end marker"
        expectDocument (Reader.read "...\n...") { missingDocument [] with EndMarker = Some(marker None) } "The second end marker is lost"
        expectDocument
            (Reader.read "# a\n...\n# b\n...\n")
            { missingDocument [ note " a"; note " b" ] with EndMarker = Some(marker None) }
            "The comment behind the first marker moves in front of it and the second marker is lost"
        expectDocument
            (Reader.read "# a\n... # e1\n# b\n... # e2\n")
            { missingDocument [ note " a"; note " b" ] with EndMarker = Some(marker (Some " e1")) }
            "The comment of the second marker is lost with it"
        match (Reader.readDocuments "# a\n...\n# b\n...\n").Parts with
        | [ StreamPart.Trivia _; StreamPart.ExtraEndMarker _; StreamPart.Trivia _; StreamPart.ExtraEndMarker _ ] -> ()
        | parts -> failwith (sprintf "readDocuments holds %d parts, not the four of the text" parts.Length)

    // yaml@2.6.1 reads `---` followed by `...` as [null], `---` followed by a comment as [null] and `%YAML 1.2` followed by `---` as [null]. A start marker
    // makes a document, so the parser gives it and the reader does not synthesize one.
    testCase "a start marker makes a document without a body" <| fun _ ->
        expectDocument (Reader.read "---\n...\n") { YamlDocument.create YamlNode.empty with StartMarker = Some(marker None); EndMarker = Some(marker None) } "Start and end marker"
        expectDocument (Reader.read "---\n# c") { missingDocument [ note " c" ] with StartMarker = Some(marker None) } "A start marker and a comment"
        Expect.isTrue (YamlNode.isMissingValue (Reader.readRoot "---")) "The root behind a lone start marker"
]

let private multipleTests = testList "More than one document" [
    // yaml@2.6.1 parse rejects each of these inputs with an error that asks for YAML.parseAllDocuments().
    // The span is the second document and the related span is the first.
    testCase "read rejects a text with two documents and names both" <| fun _ ->
        let cases =
            [ "a: 1\n---\nb: 2", span 2 1 3 5, span 1 1 1 5
              "---\na: 1\n---\nb: 2", span 3 1 4 5, span 1 1 2 5
              "a: 1\n...\n---\nb: 2", span 3 1 4 5, span 1 1 2 4
              "---\n---\na: 1", span 2 1 3 5, span 1 1 1 4
              "a: 1\n---\n", span 2 1 2 4, span 1 1 1 5 ]
        for (text, second, first) in cases do
            let label = text.Replace("\n", "|")
            match Reader.tryRead text with
            | Ok _ -> failwith (sprintf "%s holds two documents" label)
            | Error error ->
                Expect.equal (YamlErrorCode.name error.Code) "multiple-documents" (sprintf "The code of %s" label)
                Expect.equal error.Span second (sprintf "The span of %s is the second document" label)
                Expect.equal error.Related [ first ] (sprintf "The related span of %s is the first document" label)
                Expect.stringContains error.Message "Use Reader.readDocuments to read all of them" (sprintf "The message of %s" label)
                Expect.stringContains error.Message (sprintf "Line %d:" second.Start.Line) (sprintf "The line in the message of %s" label)

    // Earlier test "a second document is an error". The old test listed these texts for the scanner of the reader before 2.0.0. yaml@2.6.1 parse throws "Source contains multiple documents" for each of the first six.
    // A text with one document and an end marker, with or without a comment behind it, is one document.
    testCase "the texts of the old second-document test are rejected by read and the others are read" <| fun _ ->
        for text in [ "a: 1\n---\nb: 2\n"; "a: 1\n...\nb: 2\n"; "a: 1\n...\n---\nb: 2\n"; "a: 1\n---\nb: 2\n...\nc: 3\n"; "---\n---\na: 1\n"; "a: 1\n---\n" ] do
            match Reader.tryRead text with
            | Ok _ -> failwith (sprintf "%A holds two documents" text)
            | Error error -> Expect.equal (YamlErrorCode.name error.Code) "multiple-documents" (sprintf "The code of %A" text)
        for text in [ "a: 1\n...\n"; "a: 1\n...\n# c\n" ] do
            Expect.isTrue (Result.isOk (Reader.tryRead text)) (sprintf "%A is one document" text)

    testCase "read and readRoot raise YamlParseException with the error" <| fun _ ->
        let text = "a: 1\n---\nb: 2"
        let tried = match Reader.tryRead text with Error error -> error | Ok _ -> failwith "Two documents"
        let fromRead =
            try
                Reader.read text |> ignore
                None
            with YamlParseException error ->
                Some error
        Expect.equal fromRead (Some tried) "read raises the error of tryRead"
        let fromRoot =
            try
                Reader.readRoot text |> ignore
                None
            with YamlParseException error ->
                Some error.Code
        Expect.equal fromRoot (Some YamlErrorCode.MultipleDocuments) "readRoot raises it too"
        // The Message override of an F# exception reaches e.Message on .NET only. Fable compiles it to a method that JavaScript and Python never call.
#if !FABLE_COMPILER
        let message =
            try
                Reader.read text |> ignore
                ""
            with e ->
                e.Message
        Expect.stringContains message "Line 2:" "The message of the exception"
#endif

    testCase "readDocuments reads every document" <| fun _ ->
        let stream = Reader.readDocuments "a: 1\n---\nb: 2"
        let documents = Reader.documents stream
        Expect.equal (List.length documents) 2 "Two documents"
        Expect.nodeEqual documents.Head.Root (mapOf [ "a", sc "1" ]) "The first root"
        Expect.nodeEqual documents.Tail.Head.Root (mapOf [ "b", sc "2" ]) "The second root"
]

let private streamTests = testList "Streams" [
    // Tests.ReaderDocuments "Single document: Scalar" and "Two scalars": the stream of the reader is the stream of the parser.
    testCase "readDocuments returns the stream of the parser" <| fun _ ->
        for text in [ ""; "a"; "---\na: 1\n...\n---\nb: 2\n"; "# c\n...\na: 1"; "%YAML 1.2\n---\nx\n...\n%TAG ! t!\n---\n!y z" ] do
            Expect.streamEqual (Reader.readDocuments text) (parseOk text) (sprintf "The stream of %A" text)
            Expect.streamEqual (streamOf text) (parseOk text) (sprintf "The stream of %A by tryReadDocuments" text)

    // yaml@2.6.1 reads `---` `doc1` `---` `---` `doc2` as ["doc1",null,"doc2"] and `%YAML 1.2` alone as no document. The parser rejects a directive without a
    // start marker, as it does for a directive anywhere.
    testCase "readDocuments gives zero documents for an empty text and comments" <| fun _ ->
        Expect.isTrue (List.isEmpty (Reader.readDocuments "").Parts) "An empty text has no part"
        Expect.isTrue (List.isEmpty (documentsOf "")) "An empty text has no document"
        Expect.isTrue (List.isEmpty (documentsOf "# c\n\n# d")) "Comments make no document"
        Expect.equal (List.length (Reader.readDocuments "# c\n\n# d").Parts) 3 "The comments and the blank line are parts of the stream"
        Expect.equal (List.length (documentsOf "---\ndoc1\n---\n---\ndoc2")) 3 "A start marker makes a document"
        match Reader.tryReadDocuments "%YAML 1.2\n" with
        | Error error -> Expect.equal (YamlErrorCode.name error.Code) "directive-without-document-start" "A directive alone"
        | Ok _ -> failwith "A directive alone is accepted"

    // Spec example 9.3: two documents with a comment and an extra end marker between them. Reader.documents lists the documents only.
    testCase "documents leaves out the trivia and the extra end markers" <| fun _ ->
        let stream = Reader.readDocuments "Bare\ndocument\n...\n# No document\n...\n|\n%!PS-Adobe-2.0 # Not the first line\n"
        Expect.equal (List.length stream.Parts) 4 "A document, a comment, an extra end marker and a document"
        let documents = Reader.documents stream
        Expect.equal (List.length documents) 2 "Two documents"
        Expect.nodeEqual documents.Head.Root (scLines [ "Bare"; "document" ]) "The first root"
        Expect.nodeEqual documents.Tail.Head.Root (lit "%!PS-Adobe-2.0 # Not the first line\n") "The second root"

    testCase "a byte order mark stands in front of the first document and of a later one" <| fun _ ->
        Expect.nodeEqual (Reader.readRoot (bom + "key: value")) (mapOf [ "key", sc "value" ]) "At the start"
        let documents = documentsOf ("a\n...\n" + bom + "b")
        Expect.equal (List.length documents) 2 "Two documents"
        Expect.nodeEqual documents.Tail.Head.Root (sc "b") "The second root"

    // The parser rejects a quote that crosses a marker. The reader returns the error of the parser.
    testCase "a parse error comes back as the error of the parser" <| fun _ ->
        let text = "a: 'x\n---\nb: 1\n"
        match Reader.tryReadDocuments text, Reader.tryRead text with
        | Error streamError, Error documentError ->
            Expect.equal (YamlErrorCode.name streamError.Code) "marker-in-quote" "The code"
            Expect.equal streamError.Span.Start.Line 2 "The marker stands on line 2"
            Expect.equal (streamError.Related |> List.map (fun related -> related.Start.Line)) [ 1 ] "The quote stands on line 1"
            Expect.equal documentError.Code streamError.Code "tryRead gives the same code"
        | _ -> failwith "The parser accepts a quote that crosses a marker"
        let raised =
            try
                Reader.readDocuments text |> ignore
                None
            with YamlParseException error ->
                Some (YamlErrorCode.name error.Code)
        Expect.equal raised (Some "marker-in-quote") "readDocuments raises it"
]

let private rootTests = testList "Reader.readRoot and the decoders" [
    testCase "readRoot drops the markers, the directives and the trivia around the root" <| fun _ ->
        Expect.nodeEqual (Reader.readRoot "%YAML 1.2\n# c\n---\n# d\na") (sc "a") "A scalar root"
        Expect.nodeEqual (Reader.readRoot "# c\nk: v\n# t\n...\n") (withEnd [ note " t" ] (YamlNode.mapping [ entryBefore [ note " c" ] (sc "k") (sc "v") ])) "A block root keeps its trivia inside"
        Expect.nodeEqual (Reader.readRoot "a # t") (withNote " t" (sc "a")) "The comment of the root stays in the root"

    // yaml@2.6.1 reads {} as {} and the documents above as null. A decoder reads the missing root as a null, and a collection is never null.
    testCase "Decode.option reads a document without a node as None" <| fun _ ->
        let decode (text: string) = Decode.option (Decode.map Decode.string Decode.string) (Reader.readRoot text)
        for text in [ ""; "\n\n"; "# c"; "# c1\n# c2"; "---\n# c"; "---"; "..."; "---\n..." ] do
            Expect.equal (decode text) None (sprintf "The document %A is null" text)
        Expect.equal (decode "a: b") (Some (Map [ "a", "b" ])) "A mapping is not null"

    testCase "the root of a document without a node is the empty text for the string decoder" <| fun _ ->
        Expect.equal (Decode.string (Reader.readRoot "")) "" "The text"
        Expect.equal (Decode.string (Reader.readRoot "# c")) "" "The text of a comment-only document"
]

let Main =
    testList "Reader" [
        singleTests
        emptyTests
        multipleTests
        streamTests
        rootTests
    ]
