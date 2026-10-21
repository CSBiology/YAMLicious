module Tests.Documents

// The documents of a stream: markers, directives, the trivia around the root and between the documents, and the parts that belong to
// no document. The tests port Tests.ReaderDocuments, Tests.Directives and the multi-document tests of Tests.YamlRead. Their expectations
// are streams built with the constructors and compared with YamlStream.equalIgnoringSpans. The yaml@2.6.1 value stands above each test.
//
// Ruling 1c changes where some trivia go. The old reader put a comment in front of a `---` into the document before it or into a document of its own.
// The tree puts it into the Prelude of the document that the marker opens. The trivia split of rule R4 applies only in front of a `...` and at the
// end of the stream.

open Fable.Pyxpecto
open YAMLicious

let private marker (commentText: string option) : DocumentMarker =
    { TrailingComment = commentText |> Option.map YamlComment.create
      Span = None }

let private plainDocument (root: YamlNode) : YamlDocument = YamlDocument.create root

/// A document that starts with `---`.
let private explicitDocument (root: YamlNode) : YamlDocument =
    { plainDocument root with StartMarker = Some(marker None) }

let private closed (document: YamlDocument) : YamlDocument = { document with EndMarker = Some(marker None) }

let private withPrelude (items: PreludeItem list) (document: YamlDocument) : YamlDocument = { document with Prelude = items }

let private versionDirective (major: int) (minor: int) : PreludeItem =
    PreludeItem.Directive
        { Kind = DirectiveKind.YamlVersion(major, minor)
          TrailingComment = None
          Span = None }

let private tagDirective (handle: string) (prefix: string) : PreludeItem =
    PreludeItem.Directive
        { Kind = DirectiveKind.TagDirective(handle, prefix)
          TrailingComment = None
          Span = None }

let private preludeNote (text: string) : PreludeItem = PreludeItem.PreludeTrivia(note text)

let private document (value: YamlDocument) : StreamPart = StreamPart.Document value

let private expectStream (input: string) (parts: StreamPart list) =
    Expect.streamEqual (parseOk input) { Parts = parts } (sprintf "The stream of %A" input)

let private singleTests = testList "Single documents" [
    // Tests.ReaderDocuments "Single document: Scalar", "Mapping", "Sequence", "Explicit Start", "Explicit Start with comment", "Explicit Start with inline root node"
    // and "Expect End". yaml@2.6.1: "value", {"key":"value"}, ["a","b"], {"key":"value"} three times, "foo" and {"key":"value"}.
    testCase "a document with and without markers" <| fun _ ->
        expectStream "value" [ document (plainDocument (sc "value")) ]
        expectStream "key: value" [ document (plainDocument (mapOf [ "key", sc "value" ])) ]
        expectStream "- a\n- b" [ document (plainDocument (seqOf [ sc "a"; sc "b" ])) ]
        expectStream "---\nkey: value" [ document (explicitDocument (mapOf [ "key", sc "value" ])) ]
        expectStream
            "--- # start\nkey: value"
            [ document { plainDocument (mapOf [ "key", sc "value" ]) with StartMarker = Some(marker (Some " start")) } ]
        expectStream "--- foo" [ document (explicitDocument (sc "foo")) ]
        expectStream "key: value\n..." [ document (closed (plainDocument (mapOf [ "key", sc "value" ]))) ]
        expectStream "key: value\n... # end" [ document { plainDocument (mapOf [ "key", sc "value" ]) with EndMarker = Some(marker (Some " end")) } ]

    // Tests.YamlRead "Reader.read stops at document end marker". yaml@2.6.1 reads both inputs as {"key":"value"}. A comment behind the end marker is trivia that
    // belongs to no document.
    testCase "a comment behind an end marker belongs to no document" <| fun _ ->
        expectStream "---\nkey: value\n...\n" [ document (closed (explicitDocument (mapOf [ "key", sc "value" ]))) ]
        expectStream
            "---\nkey: value\n...\n# c\n"
            [ document (closed (explicitDocument (mapOf [ "key", sc "value" ]))); StreamPart.Trivia(note " c") ]
        // Text behind the end marker is a second document.
        expectStream
            "---\nkey: value\n...\ntrailing: ignored"
            [ document (closed (explicitDocument (mapOf [ "key", sc "value" ]))); document (plainDocument (mapOf [ "trailing", sc "ignored" ])) ]

    // Tests.YamlRead "Reader.read keeps inline root after document start marker", the block scalar and property forms. yaml@2.6.1: "foo", "x\n" and "y\n".
    testCase "the root can start on the line of the start marker" <| fun _ ->
        expectStream
            "--- !!str |\n  x\n--- &a |\n  y\n"
            [ document (explicitDocument (YamlNode.withTag (YamlTag.core "str") (lit "x\n")))
              document (explicitDocument (YamlNode.withAnchor "a" (lit "y\n"))) ]
        expectStream "--- >\n x\n y\n" [ document (explicitDocument (fold "x y\n")) ]
        expectStream "--- 'a\n  b'" [ document (explicitDocument (sq "a b")) ]

    // Tests.YamlRead "KeyValue plain continuation..." in a document: `---` followed by a block collection on the next line.
    testCase "a block collection starts on the line behind the start marker" <| fun _ ->
        expectStream "---\n- a\n- b\n" [ document (explicitDocument (seqOf [ sc "a"; sc "b" ])) ]
        expectStream "---\n? a\n: b\n" [ document (explicitDocument (YamlNode.mapping [ explicitEntry (sc "a") (sc "b") ])) ]
]

let private multipleTests = testList "Several documents" [
    // Tests.ReaderDocuments "Two scalars", "Mapping and Sequence" and Tests.YamlRead "Multi-document: Two documents separated by ---". yaml@2.6.1: ["doc1","doc2"],
    // [{"key":"value"},["item1","item2"]] and [{"document1":"value1"},{"document2":"value2"}].
    testCase "documents separated by start markers" <| fun _ ->
        expectStream "---\ndoc1\n---\ndoc2" [ document (explicitDocument (sc "doc1")); document (explicitDocument (sc "doc2")) ]
        expectStream
            "---\nkey: value\n---\n- item1\n- item2"
            [ document (explicitDocument (mapOf [ "key", sc "value" ])); document (explicitDocument (seqOf [ sc "item1"; sc "item2" ])) ]
        expectStream
            "---\ndocument1: value1\n---\ndocument2: value2"
            [ document (explicitDocument (mapOf [ "document1", sc "value1" ])); document (explicitDocument (mapOf [ "document2", sc "value2" ])) ]
        // The first document may have no marker.
        expectStream "a: 1\n---\nb: 2" [ document (plainDocument (mapOf [ "a", sc "1" ])); document (explicitDocument (mapOf [ "b", sc "2" ])) ]

    // Tests.ReaderDocuments "Three documents with directives". yaml@2.6.1: {"doc1":"v1"}, "doc2" and "doc3". A directive needs the `...` of the document before it. The directive
    // %TAG ! t! maps the primary handle to the prefix t!, so !foo is the tag t!foo.
    testCase "directives belong to the document they precede" <| fun _ ->
        let doc2 = YamlNode.withTag (YamlTag.Named { Prefix = "t!"; Suffix = "foo" }) (dq "doc2")
        expectStream
            "%YAML 1.2\n---\ndoc1: v1\n...\n%TAG ! t!\n---\n!foo \"doc2\"\n...\n---\ndoc3"
            [ document (closed (withPrelude [ versionDirective 1 2 ] (explicitDocument (mapOf [ "doc1", sc "v1" ]))))
              document (closed (withPrelude [ tagDirective "!" "t!" ] (explicitDocument doc2)))
              document (explicitDocument (sc "doc3")) ]

    // Tests.ReaderDocuments "A directive behind an end marker applies to the next document". yaml@2.6.1: {"a":1} and {"b":2}.
    testCase "a directive behind an end marker applies to the next document" <| fun _ ->
        expectStream
            "%YAML 1.2\n---\na: 1\n...\n%YAML 1.2\n---\nb: 2\n"
            [ document (closed (withPrelude [ versionDirective 1 2 ] (explicitDocument (mapOf [ "a", sc "1" ]))))
              document (withPrelude [ versionDirective 1 2 ] (explicitDocument (mapOf [ "b", sc "2" ]))) ]

    // Tests.ReaderDocuments "Document markers inside block scalar content do not split documents" and "End-marker prefix is not treated as document end".
    // yaml@2.6.1: [{"text":"line1\n---\n...\nline2\n"},{"next":"ok"}] and {"...foo":"bar","next":"v"}.
    testCase "a marker-like text inside a scalar or a key splits no document" <| fun _ ->
        expectStream
            "---\ntext: |\n  line1\n  ---\n  ...\n  line2\n---\nnext: ok"
            [ document (explicitDocument (mapOf [ "text", lit "line1\n---\n...\nline2\n" ])); document (explicitDocument (mapOf [ "next", sc "ok" ])) ]
        expectStream "...foo: bar\nnext: v" [ document (plainDocument (mapOf [ "...foo", sc "bar"; "next", sc "v" ])) ]
        expectStream "---foo: bar" [ document (plainDocument (mapOf [ "---foo", sc "bar" ])) ]

    // Tests.ReaderDocuments "Multiline quotes and block scalars stay inside their own documents". yaml@2.6.1: {"a":"x y"}, {"b":"\"hi\"\n"} and {"c":"z"}.
    testCase "multiline quotes and block scalars stay inside their own documents" <| fun _ ->
        expectStream
            "a: 'x\n  y'\n---\nb: |\n  \"hi\"\n---\nc: \"z\"\n"
            [ document (plainDocument (mapOf [ "a", sq "x y" ]))
              document (explicitDocument (mapOf [ "b", lit "\"hi\"\n" ]))
              document (explicitDocument (mapOf [ "c", dq "z" ])) ]

    // Tests.ReaderDocuments "A document marker inside a quoted scalar is an error". yaml@2.6.1 rejects the input with MISSING_CHAR on line 1. A quote never crosses a marker.
    testCase "a document marker inside a quoted scalar is an error" <| fun _ ->
        let error = parseError "a: 'x\n---\nb: 1\n"
        Expect.equal (YamlErrorCode.name error.Code) "marker-in-quote" "The code"
        Expect.equal error.Span.Start.Line 2 "The marker stands on line 2"
        Expect.equal (error.Related |> List.map (fun span -> span.Start.Line)) [ 1 ] "The quote stands on line 1"
]

let private emptyTests = testList "Documents without a node" [
    // Tests.ReaderDocuments "Empty documents skipped" and "A start marker makes a document without a body". yaml@2.6.1 reads `---` `doc1` `---` `# Just a comment` `---` `doc2` as
    // ["doc1",null,"doc2"], and `---` `...` and `%YAML 1.2` followed by `---` as [null]. A start marker makes a document with a missing root. The comment in front
    // of the third marker is the prelude of the third document (ruling 1c). The old reader kept it in the document in front.
    testCase "a start marker makes a document with a missing root" <| fun _ ->
        let missing = explicitDocument nothing
        expectStream
            "---\ndoc1\n---\n# Just a comment\n---\ndoc2"
            [ document (explicitDocument (sc "doc1")); document missing; document (withPrelude [ preludeNote " Just a comment" ] (explicitDocument (sc "doc2"))) ]
        expectStream "---\ndoc1\n---\n---\ndoc2" [ document (explicitDocument (sc "doc1")); document missing; document (explicitDocument (sc "doc2")) ]
        expectStream "---\n---\na: 1" [ document missing; document (explicitDocument (mapOf [ "a", sc "1" ])) ]
        expectStream "---\n...\n" [ document (closed missing) ]
        expectStream "%YAML 1.2\n---\n" [ document (withPrelude [ versionDirective 1 2 ] missing) ]
        expectStream "---" [ document missing ]

    // Tests.ReaderDocuments "A document without a node reads as Nil and keeps its comments". yaml@2.6.1 gives no document for an empty input, blank lines or a lone comment, and
    // [null] for `---` with a comment. A document without a node keeps its comments in BeforeRoot (rule R3). An input without any document is a stream of trivia.
    testCase "an input without a node is trivia, and a start marker keeps its comments in BeforeRoot" <| fun _ ->
        expectStream "" []
        expectStream "\n\n" [ StreamPart.Trivia blank; StreamPart.Trivia blank ]
        expectStream "# c" [ StreamPart.Trivia(note " c") ]
        expectStream "# c1\n# c2" [ StreamPart.Trivia(note " c1"); StreamPart.Trivia(note " c2") ]
        expectStream "---\n# c" [ document { explicitDocument nothing with BeforeRoot = [ note " c" ] } ]
        expectStream "---\n# c\n..." [ document { closed (explicitDocument nothing) with BeforeRoot = [ note " c" ] } ]
        expectStream "---\n\n# c\n\n" [ document { explicitDocument nothing with BeforeRoot = [ blank; note " c"; blank ] } ]

    // Tests.ReaderDocuments "A document without a node reads as Nil and keeps its comments", the middle document of spec Example 9.6. yaml@2.6.1: ["Document",null,{"matches %":20}].
    testCase "the middle document of example 9.6 has a missing root and keeps its comment" <| fun _ ->
        expectStream
            "Document\n---\n# Empty\n...\n%YAML 1.2\n---\nmatches %: 20"
            [ document (plainDocument (sc "Document"))
              document { closed (explicitDocument nothing) with BeforeRoot = [ note " Empty" ] }
              document (withPrelude [ versionDirective 1 2 ] (explicitDocument (mapOf [ "matches %", sc "20" ]))) ]

    // Tests.Directives "YAML directive 1.2" and "TAG directive". yaml@2.6.1 reads each of these as {"foo":"bar"}.
    testCase "the directives of a document" <| fun _ ->
        expectStream "%YAML 1.2\n---\nfoo: bar" [ document (withPrelude [ versionDirective 1 2 ] (explicitDocument (mapOf [ "foo", sc "bar" ]))) ]
        expectStream "%TAG !m! !mytag:\n---\nfoo: bar" [ document (withPrelude [ tagDirective "!m!" "!mytag:" ] (explicitDocument (mapOf [ "foo", sc "bar" ]))) ]
        let reserved =
            PreludeItem.Directive
                { Kind = DirectiveKind.ReservedDirective("FOO", [ "bar"; "baz" ])
                  TrailingComment = None
                  Span = None }
        expectStream "%FOO bar baz\n---\nx" [ document (withPrelude [ reserved ] (explicitDocument (sc "x"))) ]
        // Several directives keep their order, and a %YAML directive may stand after a %TAG directive.
        expectStream
            "%TAG !a! tag:a,2000:\n%YAML 1.2\n%TAG !b! tag:b,2000:\n---\nx"
            [ document (withPrelude [ tagDirective "!a!" "tag:a,2000:"; versionDirective 1 2; tagDirective "!b!" "tag:b,2000:" ] (explicitDocument (sc "x"))) ]
]

let private preludeTests = testList "Trivia in front of a document" [
    // Tests.ReaderDocuments "Leading comments and directives belong to the document they introduce", "Directive prelude comments are kept with the same document" and
    // "A comment behind a directive stays a comment of the document". yaml@2.6.1 reads each of the inputs as the document {"a":1} or {"foo":"bar"}. The trivia in front of a `---`
    // and between the directives go to the Prelude in the order of the text (rule R3).
    testCase "comments and directives in front of a start marker make the prelude of the document" <| fun _ ->
        let root = mapOf [ "a", sc "1" ]
        expectStream "# lead\n%YAML 1.2\n---\na: 1\n" [ document (withPrelude [ preludeNote " lead"; versionDirective 1 2 ] (explicitDocument root)) ]
        expectStream "%YAML 1.2\n# lead\n---\na: 1\n" [ document (withPrelude [ versionDirective 1 2; preludeNote " lead" ] (explicitDocument root)) ]
        expectStream "# lead\n---\na: 1\n" [ document (withPrelude [ preludeNote " lead" ] (explicitDocument root)) ]
        expectStream
            "%YAML 1.2\n# prelude\n---\nfoo: bar\n---\nbaz: qux"
            [ document (withPrelude [ versionDirective 1 2; preludeNote " prelude" ] (explicitDocument (mapOf [ "foo", sc "bar" ])))
              document (explicitDocument (mapOf [ "baz", sc "qux" ])) ]
        let commented =
            PreludeItem.Directive
                { Kind = DirectiveKind.YamlVersion(1, 2)
                  TrailingComment = Some(YamlComment.create " c")
                  Span = None }
        expectStream "%YAML 1.2 # c\n---\na: 'x'\n" [ document (withPrelude [ commented ] (explicitDocument (mapOf [ "a", sq "x" ]))) ]

    // Tests.ReaderDocuments "A comment between an end marker and the next start marker is no document" and "A prelude comment stays with the document it introduces". yaml@2.6.1:
    // {"a":1} and {"b":2}, twice. A comment behind a `...` and in front of a `---`, and a comment behind the root and in front of a `---` (ruling 1c), go to the Prelude of the next document.
    testCase "a comment in front of a start marker goes to the prelude of the next document" <| fun _ ->
        let first = mapOf [ "a", sc "1" ]
        let second = mapOf [ "b", sc "2" ]
        expectStream
            "a: 1\n...\n# c\n---\nb: 2\n"
            [ document (closed (plainDocument first)); document (withPrelude [ preludeNote " c" ] (explicitDocument second)) ]
        expectStream
            "---\na: 1\n...\n# intro\n---\nb: 2\n"
            [ document (closed (explicitDocument first)); document (withPrelude [ preludeNote " intro" ] (explicitDocument second)) ]
        expectStream
            "---\na: 1\n# tail\n---\nb: 2\n"
            [ document (explicitDocument first); document (withPrelude [ preludeNote " tail" ] (explicitDocument second)) ]
        // A blank line counts, and so do several comments.
        expectStream
            "a: 1\n\n# c\n# d\n---\nb: 2\n"
            [ document (plainDocument first); document (withPrelude [ PreludeItem.PreludeTrivia blank; preludeNote " c"; preludeNote " d" ] (explicitDocument second)) ]

    // Tests.Directives "2.1.2 Invalid duplicate YAML directive" and "Tests.ReaderDocuments A directive behind a start marker is rejected". yaml@2.6.1 rejects the second group with
    // BAD_SCALAR_START, because the marker ended the prelude. It reads the first input with a warning. YAML 1.2.2 (6.8.1) makes a second %YAML directive an error.
    testCase "a duplicate directive and a directive behind a start marker are errors" <| fun _ ->
        Expect.equal (parseErrorOf "%YAML 1.2\n%YAML 1.1\n---\nfoo: bar") "duplicate-yaml-directive@2" "A second %YAML directive"
        Expect.equal (parseErrorOf "---\na: 1\n---\n%TAG ! t!\n---\nb: 2\n") "reserved-indicator@4" "A directive after a bare start marker"
        Expect.equal (parseErrorOf "---\n# c\n%YAML 1.2\n") "reserved-indicator@3" "A comment between the marker and the directive"
        Expect.equal (parseErrorOf "---\n%a: |\n  x\n") "reserved-indicator@2" "A block scalar header behind the marker"
        Expect.equal (parseErrorOf "%YAML 1.2\n") "directive-without-document-start@2" "A directive alone makes no document"
        Expect.equal (parseErrorOf "%YAML 1.2\na: 1") "directive-without-document-start@2" "A directive without a start marker"
]

let private rootTriviaTests = testList "Trivia around the root (rule R3)" [
    // The trivia in front of a root that is no block collection go to BeforeRoot. In front of a block collection without a property they go to the Before of its first entry
    // or item. Behind a root that is no block collection they go to AfterRoot, and behind a block collection to its End.
    testCase "the trivia in front of the root go to BeforeRoot or to the first entry" <| fun _ ->
        expectStream "# c\na" [ document { plainDocument (sc "a") with BeforeRoot = [ note " c" ] } ]
        expectStream "# c\nk: v" [ document (plainDocument (YamlNode.mapping [ entryBefore [ note " c" ] (sc "k") (sc "v") ])) ]
        expectStream "---\n# c\na" [ document { explicitDocument (sc "a") with BeforeRoot = [ note " c" ] } ]
        expectStream "---\n# c\n- a" [ document (explicitDocument (YamlNode.sequence [ itemBefore [ note " c" ] (sc "a") ])) ]
        expectStream "\n\nk: v" [ document (plainDocument (YamlNode.mapping [ entryBefore [ blank; blank ] (sc "k") (sc "v") ])) ]
        // A root with a property has a token of its own, so the trivia in front of the property go to BeforeRoot and the ones behind it to the first entry.
        let anchored = anchorOnOwnLine "a" (YamlNode.mapping [ entryBefore [ note " d" ] (sc "k") (sc "v") ])
        expectStream "# c\n&a\n# d\nk: v" [ document { plainDocument anchored with BeforeRoot = [ note " c" ] } ]
        expectStream "# c\n&a x" [ document { plainDocument (YamlNode.withAnchor "a" (sc "x")) with BeforeRoot = [ note " c" ] } ]

    testCase "the trivia behind the root go to AfterRoot or to the End of the root collection" <| fun _ ->
        expectStream "a # t\n# u\n" [ document { plainDocument (withNote " t" (sc "a")) with AfterRoot = [ note " u" ] } ]
        expectStream "a\n# t\n...\n" [ document { closed (plainDocument (sc "a")) with AfterRoot = [ note " t" ] } ]
        expectStream "k: v\n# t\n...\n" [ document (closed (plainDocument (withEnd [ note " t" ] (mapOf [ "k", sc "v" ])))) ]
        expectStream "k: v\n# t\n" [ document (plainDocument (withEnd [ note " t" ] (mapOf [ "k", sc "v" ]))) ]
        expectStream "- a\n\n" [ document (plainDocument (withEnd [ blank ] (seqOf [ sc "a" ]))) ]

    // A marker on the line behind blank or comment lines does not close the collections in front of the trivia. The trivia before an end marker go by the columns of
    // the open collections, and the trivia before a start marker go to the next document.
    testCase "the trivia in front of a marker are placed before the collections close" <| fun _ ->
        expectStream "    k: v\n      \n...\n" [ document (closed (plainDocument (withEnd [ blank ] (mapOf [ "k", sc "v" ])))) ]
        expectStream "a:\n  b: 1\n  # c\n...\n" [ document (closed (plainDocument (mapOf [ "a", withEnd [ note " c" ] (mapOf [ "b", sc "1" ]) ]))) ]
        expectStream "a:\n  b: 1\n# c\n...\n" [ document (closed (plainDocument (withEnd [ note " c" ] (mapOf [ "a", mapOf [ "b", sc "1" ] ])))) ]
        expectStream "k: v\n\n---\nx" [ document (plainDocument (mapOf [ "k", sc "v" ])); document (withPrelude [ PreludeItem.PreludeTrivia blank ] (explicitDocument (sc "x"))) ]
]

let private suffixTests = testList "End markers without a document" [
    // Spec example 9.3 and the rulings of this milestone. yaml@2.6.1 reads `a`, `...`, `# c`, `...` as the two documents "a" and null. YAML 1.2.2 (9.2) has a document suffix
    // without a document: l-yaml-stream is l-document-prefix* l-any-document? (l-document-suffix+ l-document-prefix* l-any-document? | l-document-prefix* l-explicit-document?)*,
    // and only a `---` or a bare node makes a document. So the second `...` is a suffix with no document in front of it, and the stream has one document, the comment and an extra end marker.
    testCase "a second end marker with a comment between is an extra end marker and no document" <| fun _ ->
        expectStream
            "a\n...\n# c\n...\n"
            [ document (closed (plainDocument (sc "a"))); StreamPart.Trivia(note " c"); StreamPart.ExtraEndMarker(marker None) ]

    // Spec example 9.3, "Bare documents". yaml@2.6.1 reads three documents: "Bare document", null and "%!PS-Adobe-2.0 # Not the first line\n". The specification has two.
    testCase "example 9.3 has two documents, a comment and an extra end marker" <| fun _ ->
        expectStream
            "Bare\ndocument\n...\n# No document\n...\n|\n%!PS-Adobe-2.0 # Not the first line\n"
            [ document (closed (plainDocument (sc "Bare document")))
              StreamPart.Trivia(note " No document")
              StreamPart.ExtraEndMarker(marker None)
              document (plainDocument (lit "%!PS-Adobe-2.0 # Not the first line\n")) ]

    // yaml@2.6.1 reads `...` followed by `"a #b": !!str |` as the two documents null and {"a #b":""}. The specification has one document.
    testCase "an end marker without a document in front of it is an extra end marker" <| fun _ ->
        expectStream "..." [ StreamPart.ExtraEndMarker(marker None) ]
        expectStream "...\n..." [ StreamPart.ExtraEndMarker(marker None); StreamPart.ExtraEndMarker(marker None) ]
        expectStream "...\na: 1" [ StreamPart.ExtraEndMarker(marker None); document (plainDocument (mapOf [ "a", sc "1" ])) ]
        expectStream "# c\n...\n" [ StreamPart.Trivia(note " c"); StreamPart.ExtraEndMarker(marker None) ]
        expectStream "a\n...\n\n# c\n" [ document (closed (plainDocument (sc "a"))); StreamPart.Trivia blank; StreamPart.Trivia(note " c") ]
        expectStream "a\n...\n... # e\n" [ document (closed (plainDocument (sc "a"))); StreamPart.ExtraEndMarker(marker (Some " e")) ]

    // By [211] `# c`, `...` and a bare document are l-document-prefix* (no document), l-document-suffix and a bare document, so the comment in front of a `...` that ends no document belongs to
    // no document, whatever follows. yaml@2.6.1 reads `# c`, `...` and `a: 1` as the documents null and {"a":1}. The trivia stay in the stream, in front of the ExtraEndMarker, in the order of the text.
    testCase "trivia in front of an end marker that ends no document stay in the stream" <| fun _ ->
        expectStream "# c\n...\na: 1" [ StreamPart.Trivia(note " c"); StreamPart.ExtraEndMarker(marker None); document (plainDocument (mapOf [ "a", sc "1" ])) ]
        expectStream "\n...\nk2: v" [ StreamPart.Trivia blank; StreamPart.ExtraEndMarker(marker None); document (plainDocument (mapOf [ "k2", sc "v" ])) ]
        expectStream
            "# c\n\n...\n# d\n...\nb"
            [ StreamPart.Trivia(note " c")
              StreamPart.Trivia blank
              StreamPart.ExtraEndMarker(marker None)
              StreamPart.Trivia(note " d")
              StreamPart.ExtraEndMarker(marker None)
              document (plainDocument (sc "b")) ]
        // The trivia behind the extra end marker and in front of the document belong to that document.
        expectStream
            "# c\n...\n# d\na: 1"
            [ StreamPart.Trivia(note " c")
              StreamPart.ExtraEndMarker(marker None)
              document (plainDocument (YamlNode.mapping [ entryBefore [ note " d" ] (sc "a") (sc "1") ])) ]

    // Text on the line of an end marker, other than a comment, is an error (l-document-suffix is c-document-end s-l-comments).
    testCase "text behind an end marker on its line is an error" <| fun _ ->
        Expect.equal (parseErrorOf "a\n... b") "unexpected-content@2" "Text behind the marker"
        // Each kind of lexeme that is no comment: a scalar, an alias, a tag, an anchor, a dash, a question mark and a colon.
        for text in [ "a\n... b"; "a\n... *x"; "a\n... !t"; "a\n... &x"; "a\n... -"; "a\n... ?"; "a\n... :" ] do
            let error = parseError text
            Expect.equal (YamlErrorCode.name error.Code) "unexpected-content" (sprintf "The code of %A" text)
            Expect.stringContains error.Message "Only a comment can follow" (sprintf "The message of %A" text)
        expectStream "a\n... # c\n" [ document { closed (plainDocument (sc "a")) with EndMarker = Some(marker (Some " c")) } ]

    // A bare document needs no directive and no marker, and text behind the root of a document that no marker ended is an error.
    testCase "text behind the root of a document is an error" <| fun _ ->
        Expect.equal (parseErrorOf "\"a\"\nb") "unexpected-content@2" "A second node behind a quoted scalar"
        Expect.equal (parseErrorOf "--- \"a\"\n\"b\"") "unexpected-content@2" "A second node behind the root"
]

let Main =
    testList "Documents" [
        singleTests
        multipleTests
        emptyTests
        preludeTests
        rootTriviaTests
        suffixTests
    ]
