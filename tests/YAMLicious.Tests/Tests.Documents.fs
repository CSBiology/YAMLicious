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

let private flowRootTests = testList "Documents with a flow collection as the root" [
    // yaml@2.6.1 reads `--- [a]` and `--- {b: 1}` as ["a"] and {"b":1}, and `[a]`, `...`, `{b: 1}` as the same two documents.
    testCase "a flow collection is the root of a document with and without markers" <| fun _ ->
        let first = flowSeq [ sc "a" ]
        let second = flowMap [ "b", sc "1" ]
        expectStream "[a]" [ document (plainDocument first) ]
        expectStream "--- [a]\n--- {b: 1}\n" [ document (explicitDocument first); document (explicitDocument second) ]
        expectStream "[a]\n...\n{b: 1}" [ document (closed (plainDocument first)); document (plainDocument second) ]
        expectStream "---\n[a]\n---\n{b: 1}" [ document (explicitDocument first); document (explicitDocument second) ]
        expectStream "--- !!seq [a]" [ document (explicitDocument (YamlNode.withTag (YamlTag.core "seq") first)) ]

    // Rule R3 and ruling 1c for a root that is no block collection: the trivia in front of the root go to BeforeRoot and the ones behind it to AfterRoot, except in front of a `---`, where they go to
    // the Prelude of the next document. A comment behind the closer is the trailing comment of the root (rule R8).
    testCase "the trivia around a flow root follow rules R3 and 1c" <| fun _ ->
        expectStream "# c\n[a]" [ document { plainDocument (flowSeq [ sc "a" ]) with BeforeRoot = [ note " c" ] } ]
        expectStream "---\n# c\n[a]" [ document { explicitDocument (flowSeq [ sc "a" ]) with BeforeRoot = [ note " c" ] } ]
        expectStream "[a] # t\n# u\n" [ document { plainDocument (withNote " t" (flowSeq [ sc "a" ])) with AfterRoot = [ note " u" ] } ]
        expectStream "[a]\n# t\n...\n" [ document { closed (plainDocument (flowSeq [ sc "a" ])) with AfterRoot = [ note " t" ] } ]
        expectStream
            "[a]\n# t\n---\n[b]"
            [ document (plainDocument (flowSeq [ sc "a" ])); document (withPrelude [ preludeNote " t" ] (explicitDocument (flowSeq [ sc "b" ]))) ]
        expectStream
            "[a]\n...\n# c\n---\n[b]"
            [ document (closed (plainDocument (flowSeq [ sc "a" ]))); document (withPrelude [ preludeNote " c" ] (explicitDocument (flowSeq [ sc "b" ]))) ]
        expectStream "{a: 1}: b\n# t\n" [ document (plainDocument (withEnd [ note " t" ] (YamlNode.mapping [ YamlNode.entry (flowMap [ "a", sc "1" ]) (sc "b") ]))) ]

    // A %TAG directive binds a handle for the document that it precedes, and the anchors of a document end with it.
    testCase "directives and anchors belong to the document of a flow root" <| fun _ ->
        let tag = YamlTag.Named { Prefix = "tag:e,2000:"; Suffix = "t" }
        expectStream
            "%TAG !e! tag:e,2000:\n---\n!e!t [a]\n...\n"
            [ document (closed (withPrelude [ tagDirective "!e!" "tag:e,2000:" ] (explicitDocument (YamlNode.withTag tag (flowSeq [ sc "a" ]))))) ]
        Expect.equal (parseErrorOf "&a [x]\n---\n*a") "undefined-alias@3" "The anchor ends with its document"
        Expect.equal (parseErrorOf "%TAG !e! tag:e,2000:\n---\n[a]\n---\n!e!t [b]") "undeclared-tag-handle@5" "The handle ends with its document"
]

let private byteOrderMarkTests = testList "Byte order marks between documents" [
    // yaml@2.6.1 reads `--- a\n...\n\uFEFFb` as "a" and "b". [202] l-document-prefix allows a mark in front of every document, behind a `...` as well as at the start of the stream.
    testCase "a mark at the start of a document prefix is ignored" <| fun _ ->
        expectStream "\uFEFFa" [ document (plainDocument (sc "a")) ]
        expectStream "\uFEFF\uFEFFa" [ document (plainDocument (sc "a")) ]
        expectStream "--- a\n...\n\uFEFFb" [ document (closed (explicitDocument (sc "a"))); document (plainDocument (sc "b")) ]
        expectStream "a\n...\n\uFEFF---\nb" [ document (closed (plainDocument (sc "a"))); document (explicitDocument (sc "b")) ]
        expectStream
            "a\n...\n\uFEFF%YAML 1.2\n---\nb"
            [ document (closed (plainDocument (sc "a"))); document (withPrelude [ versionDirective 1 2 ] (explicitDocument (sc "b"))) ]
        expectStream "a\n...\n\uFEFF# c\nb" [ document (closed (plainDocument (sc "a"))); document { plainDocument (sc "b") with BeforeRoot = [ note " c" ] } ]
        expectStream "a\n...\n\uFEFF\uFEFF\uFEFFb" [ document (closed (plainDocument (sc "a"))); document (plainDocument (sc "b")) ]
        expectStream "\uFEFF%YAML 1.2\n---\na" [ document (withPrelude [ versionDirective 1 2 ] (explicitDocument (sc "a"))) ]

    // 1.2.2 [211] l-yaml-stream repeats `l-document-suffix+ l-document-prefix* l-any-document? | c-byte-order-mark | l-comment | l-explicit-document` behind a document. So a mark at the start
    // of a line ends a bare document without a `...`. Only comment lines, blank lines, more marks, a `---` and a `...` may follow it. The mark has no place in the tree, and the
    // trivia in front of a `---` go to the Prelude of the next document as ever (rule R3).
    testCase "a mark ends the open document, and a start marker, an end marker or the end of the text may follow" <| fun _ ->
        expectStream "--- a\n\uFEFF--- b" [ document (explicitDocument (sc "a")); document (explicitDocument (sc "b")) ]
        expectStream "a\n\uFEFF---\nb" [ document (plainDocument (sc "a")); document (explicitDocument (sc "b")) ]
        expectStream "a\n\uFEFF...\n" [ document (closed (plainDocument (sc "a"))) ]
        expectStream "a\n\uFEFF...\nb" [ document (closed (plainDocument (sc "a"))); document (plainDocument (sc "b")) ]
        expectStream "a\n\uFEFF# c\n--- b" [ document (plainDocument (sc "a")); document (withPrelude [ preludeNote " c" ] (explicitDocument (sc "b"))) ]
        expectStream "a\n\uFEFF\uFEFF\n---\nb" [ document (plainDocument (sc "a")); document (withPrelude [ PreludeItem.PreludeTrivia blank ] (explicitDocument (sc "b"))) ]
        expectStream "a: 1\n\uFEFF" [ document (plainDocument (mapOf [ "a", sc "1" ])) ]
        expectStream "a\n\uFEFF# c" [ document { plainDocument (sc "a") with AfterRoot = [ note " c" ] } ]
        expectStream "a: 1\n\uFEFF# c\n" [ document (plainDocument (withEnd [ note " c" ] (mapOf [ "a", sc "1" ]))) ]
        expectStream "k: |\n  a\n\uFEFF--- b" [ document (plainDocument (mapOf [ "k", lit "a\n" ])); document (explicitDocument (sc "b")) ]
        expectStream "[a]\n\uFEFF--- {b: 1}" [ document (plainDocument (flowSeq [ sc "a" ])); document (explicitDocument (flowMap [ "b", sc "1" ])) ]
        expectStream "- a\n- b\n\uFEFF# c\n\uFEFF\n# d\n---\n" [ document (plainDocument (seqOf [ sc "a"; sc "b" ])); document (withPrelude [ preludeNote " c"; PreludeItem.PreludeTrivia blank; preludeNote " d" ] (explicitDocument nothing)) ]

    // Content behind a mark that ended a document is an error. The error names the content and has the mark as the related span.
    testCase "content behind a mark that ended a document is an error" <| fun _ ->
        Expect.equal (parseErrorOf "a: 1\n\uFEFF# c\nb: 2") "content-after-byte-order-mark@3" "A comment line and a mapping"
        Expect.equal (parseErrorOf "a\n\uFEFFb") "content-after-byte-order-mark@2" "A scalar"
        Expect.equal (parseErrorOf "k: 1\n\uFEFFj: 2") "content-after-byte-order-mark@2" "A mapping entry"
        Expect.equal (parseErrorOf "- a\n\uFEFF- b") "content-after-byte-order-mark@2" "A sequence item"
        Expect.equal (parseErrorOf "--- # c\n\uFEFFa") "content-after-byte-order-mark@2" "The root behind a start marker"
        Expect.equal (parseErrorOf "k: |\n  a\n\uFEFFb") "content-after-byte-order-mark@3" "Behind a block scalar"
        Expect.equal (parseErrorOf "[a]\n\uFEFFb") "content-after-byte-order-mark@2" "Behind a flow root"
        let error = parseError "a\n\uFEFFb"
        Expect.equal (error.Related |> List.map showSpan) [ "2:1-2:2" ] "The mark is the related span"
        Expect.equal (showSpan error.Span) "2:1-2:2" "The content is the span"

    // A mark between a directive and its `---` is no prefix and no continuation, and a mark inside a line or a flow collection is no mark of the stream. The lexer reports the second kind.
    testCase "a mark between a directive and the start marker or inside a line is an error" <| fun _ ->
        Expect.equal (parseErrorOf "%YAML 1.2\n\uFEFF---\na") "invalid-character@2" "Behind a directive"
        Expect.equal (parseErrorOf "a\n...\n%TAG ! tag:x,2000:\n\uFEFF---\nb") "invalid-character@4" "Behind a directive of a later document"
        Expect.equal (parseErrorOf "[a,\n\uFEFFb]") "invalid-character@2" "Inside a flow collection"
        Expect.equal (parseErrorOf "a\uFEFFb") "invalid-character@1" "Inside a line"

    // [107] nb-double-char and [118] nb-single-char take every character of [2] nb-json, so a mark inside a quoted scalar is part of its value. It does not end the document there.
    testCase "a mark inside a quoted scalar is part of the value" <| fun _ ->
        expectRoot "\"a\uFEFFb\"" (dq "a\uFEFFb")
        expectRoot "'a\uFEFFb'" (sq "a\uFEFFb")
        expectRoot "k: \"\uFEFF\"" (mapOf [ "k", dq "\uFEFF" ])
        expectRoot "[\"a\uFEFFb\", 'c\uFEFF']" (flowSeq [ dq "a\uFEFFb"; sq "c\uFEFF" ])
        expectRoot "{\"\uFEFF\": 1}" (YamlNode.flowMapping [ YamlNode.entry (dq "\uFEFF") (sc "1") ])
        expectRoot "\"a\n\uFEFFb\"" (dq "a \uFEFFb")
        expectRoot "- \"a\n  \uFEFFb\"" (seqOf [ dq "a \uFEFFb" ])
        expectStream "\"a\uFEFF\"\n--- b" [ document (plainDocument (dq "a\uFEFF")); document (explicitDocument (sc "b")) ]
        // A mark that starts a continuation line is content, so it is no indentation.
        Expect.equal (parseErrorOf "k: \"a\n\uFEFFb\"") "quoted-indent@2" "The mark does not indent the line"
        // Outside the quotes the mark is invalid again.
        Expect.equal (parseErrorOf "\"a\"\uFEFF") "invalid-character@1" "Behind the closing quote"

    // [211]: a mark ends the document that a block scalar ends, as it ends a document behind any other node. At the root the body could take a line at the column 0, so the mark has to end it.
    testCase "a mark behind the body of a block scalar ends the scalar and the document" <| fun _ ->
        expectStream "|\n  x\n\uFEFF# c\n" [ document { plainDocument (lit "x\n") with AfterRoot = [ note " c" ] } ]
        expectStream "|+\n  x\n\n\uFEFF---\ny" [ document (plainDocument (blockScalar BlockScalarStyle.Literal ChompingMode.Keep None "x\n\n")); document (explicitDocument (sc "y")) ]
        expectStream ">\n  x\n\uFEFF" [ document (plainDocument (fold "x\n")) ]
        expectStream "--- |\n  x\n\uFEFF--- y" [ document (explicitDocument (lit "x\n")); document (explicitDocument (sc "y")) ]
        Expect.equal (parseErrorOf "|\n  x\n\uFEFFy") "content-after-byte-order-mark@3" "Content behind the mark"
        Expect.equal (parseErrorOf "|\nx\n\uFEFFy") "content-after-byte-order-mark@3" "Content behind the mark at the indentation 0"

    // [211] l-yaml-stream lets marks, comment lines and explicit documents follow a document that has no suffix. A directive belongs to an l-directive-document, which only a suffix, the `...`, leads to.
    // So a directive behind a mark that ended a document is content behind the mark. Without the mark the line continues a plain scalar, and behind a `...` it is a directive.
    testCase "a directive behind a mark that ended a document is content behind the mark" <| fun _ ->
        Expect.equal (parseErrorOf "a\n\uFEFF%YAML 1.2\n---\nb") "content-after-byte-order-mark@2" "A directive on the line of the mark"
        Expect.equal (parseErrorOf "a\n\uFEFF\n# c\n%YAML 1.2\n---\nb") "content-after-byte-order-mark@4" "A directive behind a comment line"
        Expect.equal (parseErrorOf "k: v\n\uFEFF%TAG ! tag:x,2000:\n---\nb") "content-after-byte-order-mark@2" "A tag directive behind a mapping"
        Expect.equal (parseErrorOf "--- a\n\uFEFF%YAML 1.2\n---\nb") "content-after-byte-order-mark@2" "A directive behind an explicit document"
        let error = parseError "a\n\uFEFF%YAML 1.2\n---\nb"
        Expect.equal (error.Related |> List.map showSpan) [ "2:1-2:2" ] "The mark is the related span"
        expectStream "a\n%YAML 1.2\n---\nb" [ document (plainDocument (sc "a %YAML 1.2")); document (explicitDocument (sc "b")) ]
        expectStream
            "a\n...\n\uFEFF%YAML 1.2\n---\nb"
            [ document (closed (plainDocument (sc "a"))); document (withPrelude [ versionDirective 1 2 ] (explicitDocument (sc "b"))) ]
]

// The inputs of the old tests that asserted the lines of the scanner or the elements of the old reader for markers, directives and documents. The first column names
// the old test. The assertion moves to the value that yaml@2.6.1 reads, and ERROR marks a text that yaml@2.6.1 rejects.
let private oldDocumentRows : (string * string * string) list = [
    "Tokenizer a document marker ends a block", "a: |\n  x\n---\nb: 'q'\n", "[{\"a\":\"x\\n\"},{\"b\":\"q\"}]"
    "Tokenizer a body keeps text that looks like markers and directives", "a: |\n  ---\n  ...\n  %YAML 1.2\nb: 1\n", "{\"a\":\"---\\n...\\n%YAML 1.2\\n\",\"b\":1}"
    "Tokenizer a comment behind the start marker", "--- # c\na: 1\n", "{\"a\":1}"
    "Tokenizer a block scalar behind the start marker", "--- |\n  x 'q'\n", "\"x 'q'\\n\""
    "Tokenizer quoted content behind the start marker", "--- 'x' # c\n--- !!str \"y\"\n", "[\"x\",\"y\"]"
    "Tokenizer a percent sign in content is no directive", "a: 1\n%YAML 1.2\n", "ERROR"
    "Tokenizer a percent sign in content is no directive", "a: |\n  b\n%TAG\n", "ERROR"
    "Tokenizer a percent sign in content is no directive", "a: |\n  b\n  %TAG\n", "{\"a\":\"b\\n%TAG\\n\"}"
    "Tokenizer a directive follows an end marker and every start marker ends the prelude", "a: 1\n...\n%YAML 1.2\n---\nb: 2\n", "[{\"a\":1},{\"b\":2}]"
    "Tokenizer a directive follows an end marker and every start marker ends the prelude", "---\na: 1\n---\n%TAG ! t!\n---\nb: 2\n", "ERROR"
    "Tokenizer a directive follows an end marker and every start marker ends the prelude", "---\n%TAG ! t!\n", "ERROR"
    "Tokenizer a marker needs a separator behind it and sits at column 0", "---a: 1\n...foo\n --- x\n--- a\n...", "ERROR"
    "Tokenizer a marker needs a separator behind it and sits at column 0", "---\t\n---", "[null,null]"
    "Tokenizer a quote does not cross a marker", "a: 'x\n---\ny'\n", "ERROR"
    "Tokenizer a quote does not cross a marker", "a: \"x\n...\ny\"\n---\nb: 'z'\n", "ERROR"
    "Tokenizer a quote does not cross a marker", "a: \"x\\\n---\ny\"\n", "ERROR"
    "Tokenizer a marker inside a quote that closes before it is no marker", "a: 'x\n  ---\n  y'\n", "{\"a\":\"x --- y\"}"
    "Tokenizer directives and the start marker are removed and prelude comments stay", "# lead\n%YAML 1.2\n---\na: 1\n", "{\"a\":1}"
    "Tokenizer a comment behind a directive stays as a comment line", "%YAML 1.2 # c\n---\na: 1\n", "{\"a\":1}"
    "Tokenizer content behind the start marker becomes the first line", "--- 'q'\n", "\"q\""
    "Tokenizer content behind the start marker becomes the first line", "--- |\n  x\n", "\"x\\n\""
    "Tokenizer tag directives", "%TAG !e! tag:example.com,2000:app/ # c\n%FOO bar\n---\n!e!foo bar\n", "\"bar\""
    "Tokenizer the body ends at the first end marker the scanner found", "k:\n  - ...\n  - y\n", "{\"k\":[\"...\",\"y\"]}"
    "Tokenizer the empty first and last line are cut and body blanks stay", "\n# c\na: 1\n", "{\"a\":1}"
    "Tokenizer the empty first and last line are cut and body blanks stay", "a: |+\n  x\n\n\n", "{\"a\":\"x\\n\\n\\n\"}"
    "Tokenizer every document keeps its own directives", "%YAML 1.2\n---\na: 1\n...\n%TAG !e! tag:e,2000:\n---\nb: 2\n", "[{\"a\":1},{\"b\":2}]"
    "Tokenizer a start marker does not pass its directives on", "%YAML 1.2\n---\n---\nb: 2\n", "[null,{\"b\":2}]"
    "Tokenizer a marker makes a document without a body", "---\n...\n", "null"
    "Tokenizer a marker makes a document without a body", "%YAML 1.2\n---\n", "null"
    "Tokenizer a marker makes a document without a body", "---\n---\n", "[null,null]"
    "Tokenizer a marker makes a document without a body", "a: 1\n---\n", "[{\"a\":1},null]"
    "Tokenizer a prelude comment is the first line of the document behind it", "a: 1\n...\n# c\n---\nb: 2\n", "[{\"a\":1},{\"b\":2}]"
    "Tokenizer leading comments and directives are read as the start of the document", "# lead\n%YAML 1.2\n---\na: 1\n", "{\"a\":1}"
    "Tokenizer leading comments and directives are read as the start of the document", "%YAML 1.2\n# lead\n---\na: 1\n", "{\"a\":1}"
    "Tokenizer leading comments and directives are read as the start of the document", "# lead\n---\na: 1\n", "{\"a\":1}"
    "Tokenizer leading comments and directives are read as the start of the document", "--- # c\na: 1\n", "{\"a\":1}"
    "Tokenizer a block scalar behind the start marker", "--- |\n  x\n", "\"x\\n\""
    "Tokenizer a block scalar behind the start marker", "--- |\n  x \"q\"\n  y 'r'\n", "\"x \\\"q\\\"\\ny 'r'\\n\""
    "ReaderDocuments Block scalars behind start markers", "--- !!str |\n  x\n--- &a |\n  y\n", "[\"x\\n\",\"y\\n\"]"
    "YamlRead Multi-document: Document with end marker", "---\nkey: value\n...", "{\"key\":\"value\"}"
]

let private oldInputTests = testList "Inputs of the old tests" [
    testCase "the document inputs of the old scanner and reader tests read the value that yaml@2.6.1 reads" <| fun _ -> expectYamlValues oldDocumentRows

    // Tests.Tokenizer "a marker makes a document without a body" and "a document that holds only directives is dropped". yaml@2.6.1 reads `a: 1`, `...`, `...` as the documents {"a":1} and null,
    // `%YAML 1.2` alone as no document and `a: 1`, `...`, `%YAML 1.2` as {"a":1}. YAML 1.2.2 lets an end marker follow an end marker without a document ([205] l-document-suffix)
    // and asks for the start marker of a document with directives ([209] l-directive-document). The candidate follows the spec, so it reads one document for the first text and rejects the others.
    testCase "the old document counts where the spec and yaml@2.6.1 differ" <| fun _ ->
        Expect.equal (yamlValueOf "a: 1\n...\n...\n") "{\"a\":1}" "An end marker behind an end marker ends no document"
        Expect.equal (yamlValueOf "%YAML 1.2\n") "ERROR" "A directive alone makes no document"
        Expect.equal (yamlValueOf "a: 1\n...\n%YAML 1.2\n") "ERROR" "A directive behind an end marker needs a start marker"
]

let Main =
    testList "Documents" [
        singleTests
        multipleTests
        emptyTests
        preludeTests
        rootTriviaTests
        suffixTests
        flowRootTests
        byteOrderMarkTests
        oldInputTests
    ]
