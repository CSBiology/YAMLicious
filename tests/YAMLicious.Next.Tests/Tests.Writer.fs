module Tests.Writer

// The tests of the writer. A test builds a tree with the constructors, or parses a text, and compares the text that the
// writer gives with the expected text. A tree that the parser produced has to come back as the same text, and the text has to
// read as the same tree, so each slot of the model is tested with a text that goes through the parser and the writer.
// Flow collections cannot be parsed before the flow parser lands, so their tests build the tree and compare the text only.
//
// The tests port the expectations of the old Tests.YamlWrite. The groups say which test each one ports. The differences:
// - The old tests wrote with an indentation of four spaces and the new default is two.
// - The old writer made an inline sequence of every sequence of plain scalars. The tree stores the style now, so a block
//   sequence stays a block sequence and a flow sequence is built with `YamlNode.flowSequence`.
// - The old trees kept the physical lines of a plain scalar over several lines. The tree holds the folded text, so a value with
//   a line feed is written with an empty line for each line feed.
// - The old writer spelled a core tag in its verbatim form. The new writer writes `!!str`.
// - The old trees held a comment as an element between two items. The new trees hold it in the `Before` of the next item.
// Dropped: the old tests of the options `PreserveScalarStyle` and `MultilineFallbackStyle` have no counterpart, because the
// model stores the style and the policy of the writer replaces the options.

open Fable.Pyxpecto
open YAMLicious

let private defaults = WriterOptions.defaults

let private withStyles (policy: StylePolicy) : WriterOptions = { defaults with Styles = policy }

let private writeNode (node: YamlNode) : string = Writer.writeNode defaults node

let private expectNode (node: YamlNode) (expected: string) =
    Expect.equal (writeNode node) expected "The text of the tree"

/// The error of the writer for the action, or a failure when the action succeeds.
let private writeError (action: unit -> string) : WriteError =
    try
        let text = action ()
        failwithf "The writer accepted the tree and wrote %A" text
    with YamlWriteException error ->
        error

/// The text parses to a stream, the writer gives the same text back, and that text reads as the same stream.
let private expectStable (input: string) =
    let stream = parseOk input
    let text = Writer.writeDocuments defaults stream
    Expect.equal text input (sprintf "The text written for %A" input)
    Expect.streamEqual (parseOk text) stream (sprintf "The tree read from the text written for %A" input)

/// The text parses to a stream and the writer gives the expected text, which reads as the same stream and is written again unchanged.
let private expectWritten (input: string) (expected: string) =
    let stream = parseOk input
    let text = Writer.writeDocuments defaults stream
    Expect.equal text expected (sprintf "The text written for %A" input)
    let reparsed = parseOk text
    Expect.streamEqual reparsed stream (sprintf "The tree read from the text written for %A" input)
    Expect.equal (Writer.writeDocuments defaults reparsed) text (sprintf "The second write of %A" input)

let private flowSeq (nodes: YamlNode list) : YamlNode = YamlNode.flowSequence (List.map YamlNode.item nodes)

let private flowMap (pairs: (string * YamlNode) list) : YamlNode =
    YamlNode.flowMapping [ for (key, value) in pairs -> YamlNode.entry (sc key) value ]

let private entryOf (key: string) (value: YamlNode) : MappingEntry = YamlNode.entry (sc key) value

/// A block mapping with plain keys, built with the plain constructors only.
let private mapOf (pairs: (string * YamlNode) list) : YamlNode =
    YamlNode.mapping [ for (key, value) in pairs -> entryOf key value ]

let private marker (commentText: string option) : DocumentMarker =
    { TrailingComment = commentText |> Option.map YamlComment.create
      Span = None }

let private document (root: YamlNode) : YamlDocument = YamlDocument.create root

let private writeStream (parts: StreamPart list) : string = Writer.writeDocuments defaults { Parts = parts }

let private withOpening (text: string) (node: YamlNode) : YamlNode = YamlNode.withOpeningComment (comment text) node

let private withHeader (text: string) (node: YamlNode) : YamlNode = YamlNode.withHeaderComment (comment text) node

let private directive (kind: DirectiveKind) : PreludeItem =
    PreludeItem.Directive
        { Kind = kind
          TrailingComment = None
          Span = None }

let private localTag (name: string) : YamlTag = YamlTag.local name

let private portedTests = testList "Ported from YamlWrite" [
    // Tests.YamlWrite "Value", "KeyValue" and "KeyValue + Comment". yaml@2.6.1 reads the texts as "Hello World", {"Say":"Hello World"} and
    // {"Say":"Hello World"}. The comment goes behind the value (rule R8).
    testCase "a scalar, an entry and an entry with a comment" <| fun _ ->
        expectNode (sc "Hello World") "Hello World\n"
        expectNode (mapOf [ "Say", sc "Hello World" ]) "Say: Hello World\n"
        expectNode (mapOf [ "Say", withNote " 420 blaze it" (sc "Hello World") ]) "Say: Hello World # 420 blaze it\n"

    // Tests.YamlWrite "KeyValue InlineSequence". yaml@2.6.1 reads the text as {"Say":["Hello","World"]}. The old writer chose the inline form
    // for every sequence of plain scalars. The tree stores the style, so the test builds a flow sequence.
    testCase "an entry with a flow sequence" <| fun _ ->
        expectNode (mapOf [ "Say", flowSeq [ sc "Hello"; sc "World" ] ]) "Say: [Hello, World]\n"

    // Tests.YamlWrite "KeyValue InlineSequence + Comment". yaml@2.6.1 reads the text as {"Say":["Hello","World"]}. The comment line in front of the
    // sequence is the Before of its first item (rule R2). The old test wrote four spaces and the new default is two.
    testCase "a comment line in front of the first item of a sequence in an entry" <| fun _ ->
        let items = [ itemBefore [ note " 420 blaze it" ] (sc "Hello"); YamlNode.item (sc "World") ]
        expectNode (mapOf [ "Say", YamlNode.sequence items ]) "Say:\n  # 420 blaze it\n  - Hello\n  - World\n"

    // Tests.YamlWrite "Sequence" and "MultilineSequenceSquare". yaml@2.6.1 reads the text as ["My Value 1","My Value 2","My Value 3"].
    testCase "a block sequence" <| fun _ ->
        expectNode (seqOf [ sc "My Value 1"; sc "My Value 2"; sc "My Value 3" ]) "- My Value 1\n- My Value 2\n- My Value 3\n"

    // Tests.YamlWrite "SequenceObjects" and "SequenceImplicit". yaml@2.6.1 reads the texts as ["My Value 1 My Value 2","My Value 3"] and
    // {"My Key":"My Value1 My Value2 My Value3"} for the old inputs, and as the values with line feeds for the texts below. The old trees kept the
    // lines. The new tree holds the text with its line feeds, and each line feed is an empty line of the plain scalar.
    testCase "a plain scalar over several lines" <| fun _ ->
        expectNode (seqOf [ sc "My Value 1\nMy Value 2"; sc "My Value 3" ]) "- My Value 1\n\n  My Value 2\n- My Value 3\n"
        expectNode (mapOf [ "My Key", sc "My Value1\nMy Value2\nMy Value3" ]) "My Key: My Value1\n\n  My Value2\n\n  My Value3\n"

    // Tests.YamlWrite "NextLineSequenceObjects". yaml@2.6.1 reads both texts as two objects of three keys. The old writer put the mappings
    // on lines below a lone dash. A mapping in an item continues the line of the dash, and a dash with a line break gives the old form.
    testCase "mappings in the items of a sequence" <| fun _ ->
        let first = mapOf [ "My Key1", sc "My Value1"; "My Key2", sc "My Value2"; "My Key3", sc "My Value3" ]
        let second = mapOf [ "My Key4", sc "My Value4"; "My Key5", sc "My Value5"; "My Key6", sc "My Value6" ]
        expectNode
            (YamlNode.sequence [ YamlNode.item first; YamlNode.item second ])
            "- My Key1: My Value1\n  My Key2: My Value2\n  My Key3: My Value3\n- My Key4: My Value4\n  My Key5: My Value5\n  My Key6: My Value6\n"
        expectNode
            (YamlNode.sequence [ itemOnNewLine first; itemOnNewLine second ])
            "-\n  My Key1: My Value1\n  My Key2: My Value2\n  My Key3: My Value3\n-\n  My Key4: My Value4\n  My Key5: My Value5\n  My Key6: My Value6\n"

    // Tests.YamlWrite "SequenceofSequences". yaml@2.6.1 reads the text as [["v1","v2","v3"],["v4","v5","v6"],["v7","v8","v9"]].
    testCase "flow sequences in a block sequence" <| fun _ ->
        let row (a: string) (b: string) (c: string) = flowSeq [ sc a; sc b; sc c ]
        expectNode (seqOf [ row "v1" "v2" "v3"; row "v4" "v5" "v6"; row "v7" "v8" "v9" ]) "- [v1, v2, v3]\n- [v4, v5, v6]\n- [v7, v8, v9]\n"

    // Tests.YamlWrite "DocumentStart writes ---" and "DocumentEnd writes ...". yaml@2.6.1 reads both texts as {"key":"value"}.
    testCase "a document with a start marker and a document with an end marker" <| fun _ ->
        let root = mapOf [ "key", sc "value" ]
        Expect.equal (Writer.write defaults { document root with StartMarker = Some(marker None) }) "---\nkey: value\n" "start marker"
        Expect.equal (Writer.write defaults { document root with EndMarker = Some(marker None) }) "key: value\n...\n" "end marker"

    // Tests.YamlWrite "Alias round-trip write". yaml@2.6.1 reads the text as {"first":"val","second":"val"}.
    testCase "an anchor and an alias" <| fun _ ->
        let anchored = YamlNode.withAnchor "a1" (sc "val")
        expectNode (mapOf [ "first", anchored; "second", YamlNode.alias "a1" ]) "first: &a1 val\nsecond: *a1\n"

    // Tests.YamlWrite "Double-quoted escapes control characters". yaml@2.6.1 reads the text as "null\u0000bell\u0007back\b".
    testCase "a double-quoted scalar escapes control characters" <| fun _ ->
        expectNode (dq "null\u0000bell\u0007back\u0008") "\"null\\0bell\\aback\\b\"\n"

    // Tests.YamlWrite "Comment element inside a sequence is written as a comment line", "Comments round-trip through write and read" and
    // "Trailing comment after a nested sequence round-trips". yaml@2.6.1 reads the texts without the comments. The comment of the
    // old input `a:\n- 1\n# end\nb: 2` stands in front of `b` and so it goes to the Before of that entry (rule R4).
    testCase "comments come back in the slot that holds them" <| fun _ ->
        expectStable "a:\n  - 1\n  # x\n  - 2\n"
        expectStable "r:\n  a:\n    # c\n    b: 1\n"
        expectWritten "a:\n- 1\n# end\nb: 2\n" "a:\n  - 1\n# end\nb: 2\n"
        expectStable "lang: C#\nrun: tool.cwl#main\nnote: a #b\n"
        expectStable "r:\n  a:\n    - 1\n    # end\n"

    // Tests.YamlWrite "An empty plain scalar with properties is written as its properties alone". yaml@2.6.1 reads `k: &a` as {"k":null}, `k: !!str` as {"k":""}, `- &a` as [null],
    // `- !!null` as [null], the root `&a` as null and `k: &a # c` as {"k":null}. The old writer spelled the core tag verbatim, the new writer writes `!!str`.
    testCase "an empty plain scalar with properties is written as its properties alone" <| fun _ ->
        for text in [ "k: &a\n"; "k: !!str\n"; "- &a\n"; "- !!null\n"; "&a\n"; "k: &a # c\n" ] do
            expectStable text

    // Tests.YamlWrite "An empty quoted scalar with properties keeps its quotes". yaml@2.6.1 reads `k: !!str ""` as {"k":""}, `k: &a ''` as {"k":""} and `- &a ""` as [""].
    testCase "an empty quoted scalar with properties keeps its quotes" <| fun _ ->
        for text in [ "k: !!str \"\"\n"; "k: &a ''\n"; "- &a \"\"\n" ] do
            expectStable text

    // Tests.YamlWrite "An empty plain item of a flow sequence is written as its properties alone". yaml@2.6.1 reads `k: [&a, b]` as {"k":[null,"b"]} and `k: [b, &a]` as {"k":["b",null]}.
    testCase "an empty plain item of a flow sequence is written as its properties alone" <| fun _ ->
        let anchored = YamlNode.withAnchor "a" YamlNode.empty
        expectNode (mapOf [ "k", flowSeq [ anchored; sc "b" ] ]) "k: [&a, b]\n"
        expectNode (mapOf [ "k", flowSeq [ sc "b"; anchored ] ]) "k: [b, &a]\n"

    // Tests.YamlWrite "An empty key with properties is written as its properties and a colon". yaml@2.6.1 reads `&a : v`, `!!str : v`, `&a !!str : v` as {"":"v"},
    // `&a :` as {"":null} and `&a :` followed by `  x: 1` as {"":{"x":1}}. The old writer put the tag first, the new writer keeps the order of the layout.
    testCase "an empty key with properties is written as its properties and a colon" <| fun _ ->
        for text in [ "&a : v\n"; "!!str : v\n"; "&a !!str : v\n"; "&a :\n"; "&a :\n  x: 1\n" ] do
            expectStable text

    // Tests.YamlWrite "An empty quoted key with properties keeps its quotes". yaml@2.6.1 reads `&a "": v` and `&a '': v` as {"":"v"}.
    testCase "an empty quoted key with properties keeps its quotes" <| fun _ ->
        expectStable "&a \"\": v\n"
        expectStable "&a '': v\n"
]

let private slotTests = testList "Slots" [
    // Rule R4: a run of trivia in front of an entry or an item is the Before of that entry or item.
    // yaml@2.6.1 reads the expected texts as {"a":1,"b":2}, ["a","b"], {"a":["x","y"],"b":1}.
    testCase "the trivia in front of entries and items" <| fun _ ->
        expectStable "# c1\n\na: 1\n# c2\nb: 2\n"
        expectStable "- a\n# c\n\n- b\n"
        expectStable "a:\n  # c\n\n  - x\n\n  # d\n  - y\nb: 1\n"

    // Rule R8: a comment on the line of the last token is the trailing comment of the node.
    // yaml@2.6.1 reads the expected texts as {"a":"v","b":"q","c":"w","d":"w"}, ["a","b"].
    testCase "the trailing comment of a scalar, a quoted scalar, an alias and an item" <| fun _ ->
        expectStable "a: v # c\nb: 'q' # d\nc: &x w # e\nd: *x # f\n"
        expectStable "- a # c\n- \"b\" # d\n"

    // Rules R1 and R2: a comment behind an indicator whose content starts on a later line is part of the separation. The lines between that
    // comment and the content are the Following of the separation.
    // yaml@2.6.1 reads the expected texts as {"k":"v"}, {"k":"v"}, {"k":{"a":1}}, {"k":["a"]}, ["a",["x"]], [{"a":1}] and the other texts in the same way.
    testCase "the separation behind a colon, a dash and a question mark" <| fun _ ->
        expectStable "k: # c\n  v\n"
        expectStable "k: # c\n  # f\n\n  v\n"
        expectStable "k: # c\n  a: 1\n"
        expectStable "k: # c\n  - a\n"
        expectStable "- # c\n  a\n- # d\n  - x\n"
        expectStable "- # c\n  # f\n  a: 1\n"
        expectStable "? # c\n  k\n: v\n"
        expectStable "? a\n: # c\n  v\n"

    // Rule R7: the lines between an explicit key and its colon are the BeforeColon of the entry.
    // yaml@2.6.1 reads the expected texts as {"a":"b"}, {"[\"x\",\"y\"]":"z"}.
    testCase "the gap between an explicit key and its colon" <| fun _ ->
        expectStable "? a # k\n# between\n\n: b # v\n"
        expectStable "? - x\n  - y\n# c\n: z\n"

    // Rule R4: the trivia behind the last node of a document go to the End of the deepest open block collection whose column fits.
    // yaml@2.6.1 reads the expected texts as {"a":{"b":1}}, [1], {"a":{"b":1}}, {"a":{"b":{"c":1}}}.
    testCase "the end of a block collection" <| fun _ ->
        expectStable "a:\n  b: 1\n  # inner\n# outer\n"
        expectStable "- 1\n# end\n"
        expectStable "a:\n  b: 1\n\n  # c\n\n"
        // The comment at column 6 belongs to the mapping of `b`, whose entries stand at column 4 (rule R4).
        expectWritten "a:\n    b:\n        c: 1\n      # note\n" "a:\n  b:\n    c: 1\n  # note\n"

    // The comment behind the header of a block scalar.
    // yaml@2.6.1 reads the expected texts as {"k":"text\n"}, {"k":"text"}, "text\n".
    testCase "the header comment of a block scalar" <| fun _ ->
        expectStable "k: | # h\n  text\n"
        expectStable "k: >- # h\n  text\n"
        expectStable "--- | # h\n  text\n"

    // Rules R1 and R2: the comment behind a property, and the properties on lines of their own.
    // yaml@2.6.1 reads the expected texts as {"k":""}, {"k":{"b":1}}, {"k":{"b":1}}, {"k":{"b":1}}, {"k":"v"}, [["x"]].
    testCase "the comments and line breaks between the properties of a node" <| fun _ ->
        expectStable "k: !!str # c\n"
        expectStable "k: &a # c\n  b: 1\n"
        expectStable "k: !!map # c1\n  &a\n  b: 1\n"
        expectStable "k: &a\n  # f\n  b: 1\n"
        expectStable "k: !!str\n  v\n"
        expectStable "- &a # c\n  - x\n"

    // Rule R3: the directives with their comments and the trivia between them stay in the prelude. The root of a document with a start marker
    // continues the line of the marker.
    // yaml@2.6.1 reads the expected texts as "a", {"a":1}, {"a":1}.
    testCase "the prelude of a document" <| fun _ ->
        expectWritten "%YAML 1.2 # v\n# note\n\n%TAG !e! tag:example.com,2000:\n---\n!e!x a\n" "%YAML 1.2 # v\n# note\n\n%TAG !e! tag:example.com,2000:\n--- !e!x a\n"
        expectStable "# c\n---\na: 1\n"
        expectStable "%FOO bar baz\n---\na: 1\n"

    // Rule R3: the comments of the markers.
    // yaml@2.6.1 reads the expected texts as {"a":1}, "value".
    testCase "the comments behind the markers" <| fun _ ->
        expectStable "--- # s\na: 1\n... # e\n"
        expectStable "--- # s\nvalue\n"

    // Rule R3: the trivia between the marker and the root, and behind the root.
    // yaml@2.6.1 reads the expected texts as "value", "value", ["a"].
    testCase "the trivia around the root" <| fun _ ->
        expectStable "---\n# c\n\nvalue\n# d\n"
        expectStable "# top\nvalue # tail\n# after\n"
        expectStable "---\n# c\n- a\n"

    // Rule R3 and ruling 1c: the trivia that belong to no document, and an end marker without a document.
    // yaml@2.6.1 reads the expected texts as [null,"a"], ["a",null,"b"], {"a":1}, nothing.
    testCase "the trivia of a stream and the extra end markers" <| fun _ ->
        expectStable "# c\n...\na\n"
        expectStable "a\n...\n# t\n...\nb\n"
        expectStable "a: 1\n...\n# t\n"
        expectStable "# only a comment\n"

    // Several documents.
    // yaml@2.6.1 reads the expected texts as [{"a":1},{"b":2}], ["a","b"], [{"a":1},{"b":2}], [["a"],["b"]].
    testCase "documents behind each other" <| fun _ ->
        expectStable "a: 1\n---\nb: 2\n"
        expectWritten "---\na\n---\nb\n" "--- a\n--- b\n"
        expectStable "%YAML 1.2\n---\na: 1\n...\n%YAML 1.2\n---\nb: 2\n"
        expectStable "# one\n---\n- a\n\n# two\n---\n- b\n"

    // Rule R6: the slots of a flow collection. The tests build the trees, because the flow parser is not there yet.
    // yaml@2.6.1 reads each expected text as the data of the tree, and the comments stay out of the data.
    testCase "the slots of a flow sequence" <| fun _ ->
        let items = [ itemBefore [ note " before" ] (withNote " trailing" (sc "a")); YamlNode.item (sc "b") ]
        let node = withEnd [ note " end" ] (withOpening " open" (YamlNode.flowSequence items))
        expectNode node "[ # open\n  # before\n  a, # trailing\n  b\n  # end\n]\n"

    // yaml@2.6.1 reads the expected texts as ["a","b"], [], {}, {"a":1,"b":2}, [{"a":"b"}], [["a"],[]] and the other texts in the same way.
    testCase "a flow collection without a comment stays on one line" <| fun _ ->
        expectNode (flowSeq [ sc "a"; sc "b" ]) "[a, b]\n"
        expectNode (flowSeq []) "[]\n"
        expectNode (flowMap []) "{}\n"
        expectNode (flowMap [ "a", sc "1"; "b", sc "2" ]) "{a: 1, b: 2}\n"
        expectNode (flowSeq [ flowMap [ "a", sc "b" ] ]) "[{a: b}]\n"
        expectNode (flowSeq [ flowSeq [ sc "a" ]; flowSeq [] ]) "[[a], []]\n"
        expectNode (YamlNode.flowMapping [ explicitEntry (sc "a") (sc "b") ]) "{? a : b}\n"
        expectNode (YamlNode.flowMapping [ explicitKeyOnly (sc "a"); entryOf "b" nothing ]) "{? a, b:}\n"

    // yaml@2.6.1 reads the expected texts as ["a","b"], {"k":["a","b"]}.
    testCase "the comma stands in front of the trailing comment" <| fun _ ->
        let node = flowSeq [ withNote " one" (sc "a"); withNote " two" (sc "b") ]
        expectNode node "[\n  a, # one\n  b # two\n]\n"
        expectNode (mapOf [ "k", node ]) "k: [\n  a, # one\n  b # two\n]\n"

    // yaml@2.6.1 reads the expected texts as {"a":1,"b":2}.
    testCase "a flow mapping with comments puts one entry on each line" <| fun _ ->
        let entries = [ { entryOf "a" (sc "1") with Before = [ note " first" ] }; entryOf "b" (withNote " second" (sc "2")) ]
        expectNode (withEnd [ blank ] (YamlNode.flowMapping entries)) "{\n  # first\n  a: 1,\n  b: 2 # second\n\n}\n"

    // yaml@2.6.1 reads the expected texts as {"a":"b"}, {"a":"b"}.
    testCase "the gap between a key and its colon in a flow mapping" <| fun _ ->
        let gap = { entryOf "a" (sc "b") with BeforeColon = [ note " gap" ] }
        expectNode (YamlNode.flowMapping [ gap ]) "{\n  a\n  # gap\n  : b\n}\n"
        let commentedKey = YamlNode.entry (withNote " key" (sc "a")) (sc "b")
        expectNode (YamlNode.flowMapping [ commentedKey ]) "{\n  a # key\n  : b\n}\n"

    // yaml@2.6.1 reads the expected texts as [["a"],"b"].
    testCase "a comment behind the closer of a flow collection and a nested flow collection" <| fun _ ->
        let inner = withNote " closer" (flowSeq [ withNote " x" (sc "a") ])
        expectNode (flowSeq [ inner; sc "b" ]) "[\n  [\n    a # x\n  ], # closer\n  b\n]\n"

    // yaml@2.6.1 reads the expected texts as {"k":["a"]}, {"[\"a\",\"b\"]":"v"}.
    testCase "a flow collection with properties and a flow collection as a key" <| fun _ ->
        let tagged = YamlNode.withAnchor "x" (YamlNode.withTag (YamlTag.core "seq") (flowSeq [ sc "a" ]))
        expectNode (mapOf [ "k", tagged ]) "k: !!seq &x [a]\n"
        let keyed = YamlNode.mapping [ YamlNode.entry (flowSeq [ sc "a"; sc "b" ]) (sc "v") ]
        expectNode keyed "[a, b]: v\n"

    // yaml@2.6.1 reads the expected texts as ["","b"].
    testCase "an item that holds properties only and has a comment in a flow sequence" <| fun _ ->
        let propsOnly = withNote " c" (YamlNode.withTag (YamlTag.core "str") YamlNode.empty)
        expectNode (flowSeq [ propsOnly; sc "b" ]) "[\n  !!str, # c\n  b\n]\n"
]

let private plainIndicatorTexts =
    [ "@id"; "`x"; "%x"; "&a"; "*a"; "!t"; "|x"; ">x"; "#x"; "[x"; "]x"; "{x"; "}x"; ",x"; "'x"; "\"x"; "- a"; "-"; "? a"; "?"; ": a"; ":"; "a: b"; "a #b"; "a:"; "\tx"; " x"; "x "; "x\t" ]

let private scalarTests = testList "Scalar spellings" [
    // yaml@2.6.1 reads the expected texts as nothing.
    testCase "a plain scalar stays plain where it is safe" <| fun _ ->
        for text in [ "abc"; "a b"; "a:b"; "a#b"; "C#"; "-x"; "?x"; ":x"; "a\tb"; "https://x.y/z?q=1#f"; "1.5"; "true"; "null"; "~"; "a - b"; "\u00E9"; "\u65E5\u672C\u8A9E"; "a'b"; "a\"b"; "a,b"; "a[0]"; "a{b}" ] do
            expectNode (mapOf [ "k", sc text ]) ("k: " + text + "\n")

    // The old writer made a double-quoted scalar of every plain text that cannot stand plain. Plain is the stored style of a constructed scalar and a preference.
    testCase "a plain scalar that cannot stand plain becomes double-quoted" <| fun _ ->
        for text in plainIndicatorTexts do
            let written = writeNode (mapOf [ "k", sc text ])
            let escaped = text.Replace("\\", "\\\\").Replace("\"", "\\\"").Replace("\t", "\\t")
            Expect.equal written ("k: \"" + escaped + "\"\n") (sprintf "The text for %A" text)
            Expect.equal (valueJson (parseRoot written)) (valueJson (mapOf [ "k", dq text ])) (sprintf "The value for %A" text)

    // yaml@2.6.1 reads the expected texts as {"k":"a\u0001b"}, {"k":"\u007F"}, {"k":"\u0085"}, {"k":"\uFFFE"}, {"k":"a\r\nb"}, {"k":"\uFEFFx"}.
    testCase "a plain scalar with a control character or a surrogate is escaped" <| fun _ ->
        expectNode (mapOf [ "k", sc "a\u0001b" ]) "k: \"a\\x01b\"\n"
        expectNode (mapOf [ "k", sc "\u007F" ]) "k: \"\\x7F\"\n"
        expectNode (mapOf [ "k", sc "\u0085" ]) "k: \"\\N\"\n"
        expectNode (mapOf [ "k", sc "\uFFFE" ]) "k: \"\\uFFFE\"\n"
        expectNode (mapOf [ "k", sc "a\r\nb" ]) "k: \"a\\r\\nb\"\n"
        expectNode (mapOf [ "k", sc "\uFEFFx" ]) "k: \"\\uFEFFx\"\n"

    // yaml@2.6.1 reads the expected texts as {"k":"a\nb"}, {"k":"a\n\nb"}, "a\nb", ["a\nb c\nd"], {"k":"a\n b"}, {"k":"a\n#b"} and the other texts in the same way.
    testCase "a plain scalar over several lines folds each line feed into an empty line" <| fun _ ->
        expectNode (mapOf [ "k", sc "a\nb" ]) "k: a\n\n  b\n"
        expectNode (mapOf [ "k", sc "a\n\nb" ]) "k: a\n\n\n  b\n"
        expectNode (sc "a\nb") "a\n\n  b\n"
        expectNode (seqOf [ sc "a\nb c\nd" ]) "- a\n\n  b c\n\n  d\n"
        // A line that starts with a blank or a comment sign, and a leading or trailing line feed, need quotes.
        expectNode (mapOf [ "k", sc "a\n b" ]) "k: \"a\\n b\"\n"
        expectNode (mapOf [ "k", sc "a\n#b" ]) "k: \"a\\n#b\"\n"
        expectNode (mapOf [ "k", sc "\na" ]) "k: \"\\na\"\n"
        expectNode (mapOf [ "k", sc "a\n" ]) "k: \"a\\n\"\n"

    // yaml@2.6.1 reads the expected texts as "--- a", "...", {"k":"--- a"}, {"--- a":"v"}, ["--- a"].
    testCase "a plain scalar that starts with a document marker is quoted at the first column" <| fun _ ->
        expectNode (sc "--- a") "\"--- a\"\n"
        expectNode (sc "...") "\"...\"\n"
        expectNode (mapOf [ "k", sc "--- a" ]) "k: --- a\n"
        expectNode (mapOf [ "--- a", sc "v" ]) "\"--- a\": v\n"
        expectNode (seqOf [ sc "--- a" ]) "- --- a\n"

    // yaml@2.6.1 reads the expected texts as {"k":"it's"}, {"k":" x "}, {"k":""}, {"k":"a\nb"}, {"k":"a\n\nb"}, {"k":"\na"} and the other texts in the same way.
    testCase "a single-quoted scalar doubles its quote and folds its line feeds" <| fun _ ->
        expectNode (mapOf [ "k", sq "it's" ]) "k: 'it''s'\n"
        expectNode (mapOf [ "k", sq " x " ]) "k: ' x '\n"
        expectNode (mapOf [ "k", sq "" ]) "k: ''\n"
        expectNode (mapOf [ "k", sq "a\nb" ]) "k: 'a\n\n  b'\n"
        expectNode (mapOf [ "k", sq "a\n\nb" ]) "k: 'a\n\n\n  b'\n"
        expectNode (mapOf [ "k", sq "\na" ]) "k: '\n\n  a'\n"
        expectNode (mapOf [ "k", sq "a\n" ]) "k: 'a\n\n  '\n"
        expectNode (mapOf [ "k", sq "a'\nb" ]) "k: 'a''\n\n  b'\n"
        // A control character and a line that starts or ends with a blank cannot stand in single quotes.
        expectNode (mapOf [ "k", sq "a\u0001" ]) "k: \"a\\x01\"\n"
        expectNode (mapOf [ "k", sq "a \nb" ]) "k: \"a \\nb\"\n"
        expectNode (mapOf [ "k", sq "a\n b" ]) "k: \"a\\n b\"\n"

    // yaml@2.6.1 reads each double-quoted text as the value on its left.
    testCase "a double-quoted scalar writes the shortest escape of each character" <| fun _ ->
        let cases =
            [ "\"", "\\\""; "\\", "\\\\"; "\n", "\\n"; "\t", "\\t"; "\r", "\\r"; "\u0000", "\\0"; "\u0007", "\\a"; "\u0008", "\\b"
              "\u000B", "\\v"; "\u000C", "\\f"; "\u001B", "\\e"; "\u0085", "\\N"; "\u00A0", "\\_"; "\u2028", "\\L"; "\u2029", "\\P"
              "\u0001", "\\x01"; "\u001F", "\\x1F"; "\u007F", "\\x7F"; "\u0080", "\\x80"; "\u009F", "\\x9F"; "\uFFFE", "\\uFFFE"; "\uFEFF", "\\uFEFF"
              "/", "/"; "\u00E9", "\u00E9"; "\u20AC", "\u20AC" ]
        for (value, escape) in cases do
            expectNode (mapOf [ "k", dq value ]) ("k: \"" + escape + "\"\n")

    // yaml@2.6.1 reads the expected texts as {"k":"\U0001F600"}.
    testCase "a double-quoted scalar keeps a surrogate pair and a lone surrogate is an error" <| fun _ ->
        expectNode (mapOf [ "k", dq "\U0001F600" ]) "k: \"\U0001F600\"\n"
        // The compiler turns a lone surrogate in a literal into U+FFFD, so the test builds the text at run time.
        let lone = "a" + string (char 0xD800) + "b"
        let error = writeError (fun () -> writeNode (mapOf [ "k", dq lone ]))
        Expect.stringContains error.Message "lone surrogate" "The message"
        Expect.equal error.Path [ "k" ] "The path"

    // yaml@2.6.1 reads the expected texts as {"k":"a\nb\n"}, {"k":"a\nb"}, {"k":"a\n\n\n"}, {"k":"a\n"}, {"k":"\n\n"}, {"k":""} and the other texts in the same way.
    testCase "a literal scalar writes a header from the value" <| fun _ ->
        expectNode (mapOf [ "k", lit "a\nb\n" ]) "k: |\n  a\n  b\n"
        expectNode (mapOf [ "k", blockScalar BlockScalarStyle.Literal ChompingMode.Strip None "a\nb" ]) "k: |-\n  a\n  b\n"
        expectNode (mapOf [ "k", blockScalar BlockScalarStyle.Literal ChompingMode.Keep None "a\n\n\n" ]) "k: |+\n  a\n\n\n"
        expectNode (mapOf [ "k", blockScalar BlockScalarStyle.Literal ChompingMode.Keep None "a\n" ]) "k: |+\n  a\n"
        expectNode (mapOf [ "k", blockScalar BlockScalarStyle.Literal ChompingMode.Keep None "\n\n" ]) "k: |+\n\n\n"
        expectNode (mapOf [ "k", blockScalar BlockScalarStyle.Literal ChompingMode.Strip None "" ]) "k: |-\n"
        expectNode (mapOf [ "k", blockScalar BlockScalarStyle.Literal ChompingMode.Clip None "" ]) "k: |\n"
        expectNode (mapOf [ "k", blockScalar BlockScalarStyle.Literal ChompingMode.Keep None "" ]) "k: |+\n"

    // yaml@2.6.1 reads the expected texts as {"k":" a\nb\n"}, {"k":"  a"}, {"k":"\n a\n"}, {"k":"\ta\n"}, {"k":"\na\n"}, {"k":"a\n"} and the other texts in the same way.
    testCase "a block scalar gets an indentation indicator when its first line starts with a blank" <| fun _ ->
        expectNode (mapOf [ "k", lit " a\nb\n" ]) "k: |2\n   a\n  b\n"
        expectNode (mapOf [ "k", blockScalar BlockScalarStyle.Literal ChompingMode.Strip None "  a" ]) "k: |2-\n    a\n"
        expectNode (mapOf [ "k", lit "\n a\n" ]) "k: |2\n\n   a\n"
        // A tab is no indentation, and a leading empty line needs no indicator.
        expectNode (mapOf [ "k", lit "\ta\n" ]) "k: |\n  \ta\n"
        expectNode (mapOf [ "k", lit "\na\n" ]) "k: |\n\n  a\n"
        // A stored indicator stays, and it sets the indentation of the body.
        expectNode (mapOf [ "k", blockScalar BlockScalarStyle.Literal ChompingMode.Clip (Some 4) "a\n" ]) "k: |4\n    a\n"
        expectNode (mapOf [ "a", mapOf [ "k", blockScalar BlockScalarStyle.Literal ChompingMode.Clip (Some 1) "a\n" ] ]) "a:\n  k: |1\n   a\n"

    // yaml@2.6.1 reads the expected texts as {"k":"a b\n"}, {"k":"a\nb\n"}, {"k":"a\n\nb\n"}, {"k":"a\n  b\nc\n"}, {"k":"a\n\n  b\n\nc\n"}, {"k":"\n\na\n"} and the other texts in the same way.
    testCase "a folded scalar folds its text back from the line feeds" <| fun _ ->
        expectNode (mapOf [ "k", fold "a b\n" ]) "k: >\n  a b\n"
        expectNode (mapOf [ "k", fold "a\nb\n" ]) "k: >\n  a\n\n  b\n"
        expectNode (mapOf [ "k", fold "a\n\nb\n" ]) "k: >\n  a\n\n\n  b\n"
        expectNode (mapOf [ "k", fold "a\n  b\nc\n" ]) "k: >\n  a\n    b\n  c\n"
        expectNode (mapOf [ "k", fold "a\n\n  b\n\nc\n" ]) "k: >\n  a\n\n    b\n\n  c\n"
        expectNode (mapOf [ "k", fold "\n\na\n" ]) "k: >\n\n\n  a\n"
        expectNode (mapOf [ "k", blockScalar BlockScalarStyle.Folded ChompingMode.Keep None "a\n\n" ]) "k: >+\n  a\n\n"

    // yaml@2.6.1 reads the expected texts as {"k":"a","j":1}, {"k":"a\n","j":1}, {"k":"a\n\n","j":1}, {"k":"\n","j":1}.
    testCase "a block scalar adjusts a stored chomping that disagrees with the value" <| fun _ ->
        let next = "j", sc "1"
        expectNode (mapOf [ "k", blockScalar BlockScalarStyle.Literal ChompingMode.Clip None "a"; next ]) "k: |-\n  a\nj: 1\n"
        expectNode (mapOf [ "k", blockScalar BlockScalarStyle.Literal ChompingMode.Strip None "a\n"; next ]) "k: |\n  a\nj: 1\n"
        expectNode (mapOf [ "k", blockScalar BlockScalarStyle.Literal ChompingMode.Clip None "a\n\n"; next ]) "k: |+\n  a\n\nj: 1\n"
        expectNode (mapOf [ "k", blockScalar BlockScalarStyle.Literal ChompingMode.Clip None "\n"; next ]) "k: |+\n\nj: 1\n"

    // [165] b-chomped-last takes the end of the input in place of a line feed. A block scalar with the chomping Clip or Keep and no final line
    // feed in its value can only be the last node of the text, where the text ends without a line break. The output depends on the tree alone,
    // so a parsed scalar and a built one are written alike. Anywhere else the chomping conflicts with the value.
    // yaml@2.6.1 reads the expected texts as {"k":"a","j":1}.
    testCase "a block scalar at the end of the text may lack its last line break" <| fun _ ->
        let stream = parseOk "k: |\n  a"
        let text = Writer.writeDocuments defaults stream
        Expect.equal text "k: |\n  a" "The text ends in the scalar"
        Expect.streamEqual (parseOk text) stream "The same tree"
        // Another entry behind the scalar needs the line break, so the chomping changes.
        let node = mapOf [ "k", blockScalar BlockScalarStyle.Literal ChompingMode.Clip None "a"; "j", sc "1" ]
        expectNode node "k: |-\n  a\nj: 1\n"
        Expect.stringContains (writeError (fun () -> Writer.writeNode (withStyles RequireStored) node)).Message "chomping" "RequireStored"
        Expect.equal (writeError (fun () -> Writer.writeNode (withStyles RequireStored) node)).Path [ "k" ] "The path"

    // yaml@2.6.1 reads the expected texts as {"k":"a\n"}, {"k":"a\n"}, ["x","a\n"], {"a":{"k":"a\n"}}, "a\n", {"k":"a","j":1} and the other texts in the same way.
    testCase "a built block scalar is written like a parsed one" <| fun _ ->
        let clip (style: BlockScalarStyle) (value: string) = blockScalar style ChompingMode.Clip None value
        let keep = blockScalar BlockScalarStyle.Literal ChompingMode.Keep None "a"
        // The last node of the text: no final line break, under both policies.
        let last = mapOf [ "k", clip BlockScalarStyle.Literal "a" ]
        expectNode last "k: |\n  a"
        Expect.equal (Writer.writeNode (withStyles RequireStored) last) "k: |\n  a" "RequireStored"
        expectNode (mapOf [ "k", keep ]) "k: |+\n  a"
        expectNode (seqOf [ sc "x"; clip BlockScalarStyle.Folded "a" ]) "- x\n- >\n  a"
        expectNode (mapOf [ "a", mapOf [ "k", clip BlockScalarStyle.Literal "a" ] ]) "a:\n  k: |\n    a"
        expectNode (clip BlockScalarStyle.Literal "a") "|\n  a"
        Expect.nodeEqual (parseRoot "k: |\n  a") last "The text reads as the built tree"
        // Not the last node: Strip under PreferStored and a conflict under RequireStored.
        let notLast = mapOf [ "k", clip BlockScalarStyle.Literal "a"; "j", sc "1" ]
        expectNode notLast "k: |-\n  a\nj: 1\n"
        Expect.stringContains (writeError (fun () -> Writer.writeNode (withStyles RequireStored) notLast)).Message "chomping" "RequireStored"
        Expect.equal (writeError (fun () -> Writer.writeNode (withStyles RequireStored) notLast)).Path [ "k" ] "The path"
        // A document behind the scalar, a comment behind the root and an end marker also end the text later.
        let open1 = document (mapOf [ "k", clip BlockScalarStyle.Literal "a" ])
        Expect.equal (writeStream [ StreamPart.Document open1; StreamPart.Document(document (sc "b")) ]) "k: |-\n  a\n--- b\n" "A document follows"
        Expect.equal (Writer.write defaults { open1 with AfterRoot = [] }) "k: |\n  a" "A single document"
        Expect.equal (Writer.write defaults { document (clip BlockScalarStyle.Literal "a") with AfterRoot = [ note " t" ] }) "|-\n  a\n# t\n" "A comment follows"
        Expect.equal (Writer.write defaults { open1 with EndMarker = Some(marker None) }) "k: |-\n  a\n...\n" "An end marker follows"
        let withEnd1 = mapOf [ "k", clip BlockScalarStyle.Literal "a" ] |> withEnd [ note " e" ]
        Expect.equal (writeNode withEnd1) "k: |-\n  a\n# e\n" "End trivia follow"

    // yaml@2.6.1 reads the expected texts as {"k":"a\u0001\n"}, {"k":"a\rb\n"}.
    testCase "a block scalar that cannot stand as a block scalar becomes double-quoted" <| fun _ ->
        expectNode (mapOf [ "k", lit "a\u0001\n" ]) "k: \"a\\x01\\n\"\n"
        expectNode (mapOf [ "k", lit "a\rb\n" ]) "k: \"a\\rb\\n\"\n"

    // yaml@2.6.1 reads the expected texts as ["a\n"].
    testCase "the header comment of a block scalar that loses its block form moves behind the text" <| fun _ ->
        let node = withHeader " h" (lit "a\n")
        expectNode (flowSeq [ node ]) "[\n  \"a\\n\" # h\n]\n"

    // yaml@2.6.1 reads the expected texts as {"a b":"v"}, {"a: b":"v"}, {"k:":"v"}, {"-":"v"}, {"?":"v"}, {":":"v"} and the other texts in the same way.
    testCase "a plain scalar in a key position" <| fun _ ->
        expectNode (YamlNode.mapping [ YamlNode.entry (sc "a b") (sc "v") ]) "a b: v\n"
        expectNode (YamlNode.mapping [ YamlNode.entry (sc "a: b") (sc "v") ]) "\"a: b\": v\n"
        expectNode (YamlNode.mapping [ YamlNode.entry (sc "k:") (sc "v") ]) "k:: v\n"
        expectNode (YamlNode.mapping [ YamlNode.entry (sc "-") (sc "v") ]) "-: v\n"
        expectNode (YamlNode.mapping [ YamlNode.entry (sc "?") (sc "v") ]) "?: v\n"
        expectNode (YamlNode.mapping [ YamlNode.entry (sc ":") (sc "v") ]) ":: v\n"
        expectNode (YamlNode.mapping [ YamlNode.entry (sc "a\nb") (sc "v") ]) "\"a\\nb\": v\n"
        expectNode (YamlNode.mapping [ YamlNode.entry (dq "") (sc "v") ]) "\"\": v\n"
]

let private tagTests = testList "Tags and anchors" [
    // yaml@2.6.1 reads the expected texts as "x", "x", "x", {"k":5}.
    testCase "a core tag, a local tag and the non-specific tag" <| fun _ ->
        expectNode (YamlNode.withTag (YamlTag.core "str") (sc "x")) "!!str x\n"
        expectNode (YamlNode.withTag (localTag "local") (sc "x")) "!local x\n"
        expectNode (YamlNode.withTag YamlTag.NonSpecific (dq "x")) "! \"x\"\n"
        expectNode (mapOf [ "k", YamlNode.withTag (YamlTag.core "int") (sc "5") ]) "k: !!int 5\n"

    // yaml@2.6.1 reads the expected texts as "v", "v".
    testCase "a tag of another namespace is written in the verbatim form" <| fun _ ->
        let other = YamlTag.Named { Prefix = "tag:example.com,2000:"; Suffix = "x" }
        expectNode (YamlNode.withTag other (sc "v")) "!<tag:example.com,2000:x> v\n"
        let verbatim = YamlTag.Named { Prefix = ""; Suffix = "tag:yaml.org,2002:str" }
        expectNode (YamlNode.withTag verbatim (sc "v")) "!<tag:yaml.org,2002:str> v\n"

    // yaml@2.6.1 reads the expected texts as "v", "v", "v".
    testCase "a tag handle that a directive declares is used for its prefix" <| fun _ ->
        let other = YamlTag.Named { Prefix = "tag:example.com,2000:"; Suffix = "x" }
        let declared = { document (YamlNode.withTag other (sc "v")) with Prelude = [ directive (DirectiveKind.TagDirective("!e!", "tag:example.com,2000:")) ] }
        Expect.equal (Writer.write defaults declared) "%TAG !e! tag:example.com,2000:\n--- !e!x v\n" "The handle"
        // A directive that gives `!!` another prefix takes the handle from the core tags, and the core tag turns verbatim.
        let redeclared = { document (YamlNode.withTag (YamlTag.core "str") (sc "v")) with Prelude = [ directive (DirectiveKind.TagDirective("!!", "tag:example.com,2000:")) ] }
        Expect.equal (Writer.write defaults redeclared) "%TAG !! tag:example.com,2000:\n--- !<tag:yaml.org,2002:str> v\n" "The redeclared handle"
        let local = { document (YamlNode.withTag (localTag "x") (sc "v")) with Prelude = [ directive (DirectiveKind.TagDirective("!", "tag:example.com,2000:")) ] }
        Expect.equal (Writer.write defaults local) "%TAG ! tag:example.com,2000:\n--- !<!x> v\n" "The redeclared primary handle"

    // A shorthand tag holds ns-tag-char only. The suffix is read with its %XX escapes decoded as UTF-8.
    // yaml@2.6.1 reads the expected texts as "v", "v", "v", "v", "v", "v" and the other texts in the same way.
    testCase "the suffix of a shorthand tag is escaped" <| fun _ ->
        expectNode (YamlNode.withTag (localTag "a!b") (sc "v")) "!a%21b v\n"
        expectNode (YamlNode.withTag (localTag "a b") (sc "v")) "!a%20b v\n"
        expectNode (YamlNode.withTag (localTag "a,b[c]{d}") (sc "v")) "!a%2Cb%5Bc%5D%7Bd%7D v\n"
        expectNode (YamlNode.withTag (localTag "100%") (sc "v")) "!100%25 v\n"
        expectNode (YamlNode.withTag (localTag "\u00E9") (sc "v")) "!%C3%A9 v\n"
        expectNode (YamlNode.withTag (localTag "\u20AC") (sc "v")) "!%E2%82%AC v\n"
        expectNode (YamlNode.withTag (localTag "\U0001F600") (sc "v")) "!%F0%9F%98%80 v\n"
        expectNode (YamlNode.withTag (localTag "a/b:c@d") (sc "v")) "!a/b:c@d v\n"
        let escaped = parseRoot "!a%21b%20c%C3%A9 v"
        Expect.nodeEqual escaped (YamlNode.withTag (localTag "a!b c\u00E9") (sc "v")) "The parser decodes the escapes"
        expectNode escaped "!a%21b%20c%C3%A9 v\n"

    testCase "a verbatim tag with a character it cannot hold is an error" <| fun _ ->
        let bad = YamlTag.Named { Prefix = "tag:example.com,2000:"; Suffix = "a b" }
        let error = writeError (fun () -> writeNode (YamlNode.withTag bad (sc "v")))
        Expect.stringContains error.Message "verbatim" "The message"

    // yaml@2.6.1 reads the expected texts as "v", "v".
    testCase "the order of the properties follows the layout" <| fun _ ->
        let both = YamlNode.withAnchor "a" (YamlNode.withTag (YamlTag.core "str") (sc "v"))
        expectNode both "!!str &a v\n"
        let swapped = withLayout [ placement PropertyName.Anchor Separation.Inline; placement PropertyName.Tag Separation.Inline ] both
        expectNode swapped "&a !!str v\n"

    // yaml@2.6.1 reads the expected texts as {"1":"v","a":1}, [1,1].
    testCase "an alias, an alias as a key and an alias with a comment" <| fun _ ->
        let both = YamlNode.mapping [ entryOf "a" (YamlNode.withAnchor "x" (sc "1")); YamlNode.entry (YamlNode.alias "x") (sc "v") ]
        expectNode both "a: &x 1\n*x : v\n"
        expectNode (seqOf [ YamlNode.withAnchor "x" (sc "1"); withNote " c" (YamlNode.alias "x") ]) "- &x 1\n- *x # c\n"
        Expect.nodeEqual (parseRoot "a: &x 1\n*x : v\n") both "The text reads as the same tree"
]

let private documentTests = testList "Documents" [
    // yaml@2.6.1 reads the expected texts as null.
    testCase "a missing root keeps its document with a start marker" <| fun _ ->
        expectNode YamlNode.empty "---\n"
        Expect.equal (writeStream [ StreamPart.Document(document YamlNode.empty); StreamPart.Document(document (sc "a")) ]) "---\n--- a\n" "Two documents"
        Expect.equal (writeStream [ StreamPart.Document { document YamlNode.empty with EndMarker = Some(marker None) } ]) "---\n...\n" "The end marker"
        Expect.equal (writeStream []) "" "An empty stream"

    testCase "a document behind a document without an end marker gets a start marker" <| fun _ ->
        Expect.equal (writeStream [ StreamPart.Document(document (sc "a")); StreamPart.Document(document (sc "b")) ]) "a\n--- b\n" "Two scalars"
        Expect.equal
            (writeStream [ StreamPart.Document { document (sc "a") with EndMarker = Some(marker None) }; StreamPart.Document(document (sc "b")) ])
            "a\n...\nb\n"
            "The end marker closes the first document"

    // yaml@2.6.1 reads the expected texts as "b", "b".
    testCase "a document with a directive gets a start marker, and the document before it gets an end marker" <| fun _ ->
        let withDirective = { document (sc "b") with Prelude = [ directive (DirectiveKind.YamlVersion(1, 2)) ] }
        Expect.equal (Writer.write defaults withDirective) "%YAML 1.2\n--- b\n" "The directive"
        Expect.equal (writeStream [ StreamPart.Document(document (sc "a")); StreamPart.Document withDirective ]) "a\n...\n%YAML 1.2\n--- b\n" "The end marker"
        let reserved = { document (sc "b") with Prelude = [ directive (DirectiveKind.ReservedDirective("FOO", [ "x"; "y" ])) ] }
        Expect.equal (Writer.write defaults reserved) "%FOO x y\n--- b\n" "The reserved directive"

    // yaml@2.6.1 reads the expected texts as "a".
    testCase "trivia in the prelude without a directive keep the start marker" <| fun _ ->
        let trivia = { document (sc "a") with Prelude = [ PreludeItem.PreludeTrivia(note " c") ] }
        Expect.equal (Writer.write defaults trivia) "# c\n--- a\n" "The trivia"

    // yaml@2.6.1 reads the expected texts as "a", "a", "a\n", ["a"], ["a"], ["a"] and the other texts in the same way.
    testCase "the root stays on the line of the start marker or moves to a later line" <| fun _ ->
        let explicit (root: YamlNode) = { document root with StartMarker = Some(marker None) }
        Expect.equal (Writer.write defaults (explicit (sc "a"))) "--- a\n" "A scalar"
        Expect.equal (Writer.write defaults (explicit (YamlNode.withTag (YamlTag.core "str") (sc "a")))) "--- !!str a\n" "A tagged scalar"
        Expect.equal (Writer.write defaults (explicit (lit "a\n"))) "--- |\n  a\n" "A block scalar"
        Expect.equal (Writer.write defaults (explicit (flowSeq [ sc "a" ]))) "--- [a]\n" "A flow sequence"
        Expect.equal (Writer.write defaults (explicit (seqOf [ sc "a" ]))) "---\n- a\n" "A block sequence"
        Expect.equal (Writer.write defaults (explicit (YamlNode.withTag (YamlTag.core "seq") (seqOf [ sc "a" ])))) "--- !!seq\n- a\n" "A tagged block sequence"
        Expect.equal (Writer.write defaults { explicit (sc "a") with StartMarker = Some(marker (Some " s")) }) "--- # s\na\n" "A comment behind the marker"
        Expect.equal (Writer.write defaults { explicit (sc "a") with BeforeRoot = [ note " c" ] }) "---\n# c\na\n" "Trivia in front of the root"

    testCase "an extra end marker and the trivia of a stream" <| fun _ ->
        let extra = StreamPart.ExtraEndMarker(marker (Some " e"))
        Expect.equal (writeStream [ StreamPart.Trivia(note " c"); extra; StreamPart.Document(document (sc "a")) ]) "# c\n... # e\na\n" "Trivia and a marker in front of a document"
        Expect.equal (writeStream [ StreamPart.Document { document (sc "a") with EndMarker = Some(marker None) }; StreamPart.Trivia blank; StreamPart.Trivia(note " t") ]) "a\n...\n\n# t\n" "Trivia behind a document"

    // yaml@2.6.1 reads the expected texts as "a", "a".
    testCase "the trivia behind the root of a scalar document stay behind it" <| fun _ ->
        Expect.equal (Writer.write defaults { document (sc "a") with AfterRoot = [ note " t" ] }) "a\n# t\n" "AfterRoot"
        Expect.equal (Writer.write defaults { document (sc "a") with AfterRoot = [ note " t" ]; EndMarker = Some(marker None) }) "a\n# t\n...\n" "AfterRoot and an end marker"
]

let private blockLayoutTests = testList "Block layout" [
    // yaml@2.6.1 reads the expected texts as {"k":["a","b"],"j":1}.
    testCase "a block sequence in a mapping value is indented, not indentless" <| fun _ ->
        expectNode (mapOf [ "k", seqOf [ sc "a"; sc "b" ]; "j", sc "1" ]) "k:\n  - a\n  - b\nj: 1\n"

    // yaml@2.6.1 reads the expected texts as [["a","b"],"c"], [{"a":1,"b":2}], [[{"a":1}]], {"[\"x\"]":{"a":1}}.
    testCase "a compact collection continues the line of its indicator" <| fun _ ->
        expectNode (seqOf [ seqOf [ sc "a"; sc "b" ]; sc "c" ]) "- - a\n  - b\n- c\n"
        expectNode (seqOf [ mapOf [ "a", sc "1"; "b", sc "2" ] ]) "- a: 1\n  b: 2\n"
        expectNode (seqOf [ seqOf [ mapOf [ "a", sc "1" ] ] ]) "- - a: 1\n"
        expectNode (YamlNode.mapping [ explicitEntry (seqOf [ sc "x" ]) (mapOf [ "a", sc "1" ]) ]) "? - x\n: a: 1\n"

    // yaml@2.6.1 reads the expected texts as {"a":{"b":{"c":1}}}.
    testCase "a nested mapping goes to the next line" <| fun _ ->
        expectNode (mapOf [ "a", mapOf [ "b", mapOf [ "c", sc "1" ] ] ]) "a:\n  b:\n    c: 1\n"

    // yaml@2.6.1 reads the expected texts as ["a"], [["a"]], {"k":"v"}.
    testCase "an item and an entry whose content stands on a later line" <| fun _ ->
        expectNode (YamlNode.sequence [ itemOnNewLine (sc "a") ]) "-\n  a\n"
        expectNode (YamlNode.sequence [ itemOnNewLine (seqOf [ sc "a" ]) ]) "-\n  - a\n"
        expectNode (YamlNode.mapping [ entryOnNewLine "k" (sc "v") ]) "k:\n  v\n"

    // yaml@2.6.1 reads the expected texts as {"k":{"a":1}}, [{"a":1}], {"a":1}.
    testCase "the properties of a block collection stand on the line of the indicator" <| fun _ ->
        expectNode (mapOf [ "k", YamlNode.withTag (YamlTag.core "map") (mapOf [ "a", sc "1" ]) ]) "k: !!map\n  a: 1\n"
        expectNode (seqOf [ YamlNode.withAnchor "x" (mapOf [ "a", sc "1" ]) ]) "- &x\n  a: 1\n"
        expectNode (YamlNode.withTag (YamlTag.core "map") (mapOf [ "a", sc "1" ])) "!!map\na: 1\n"

    // The plain constructors give a block collection behind a colon a line break, and a block collection with properties a line break behind its
    // last property, as the parser does. The tree comes back equal from its text, and the text is stable. yaml@2.6.1 reads the first text as
    // {"map":{"a":1},"seq":["x",{"b":2}],"tagged":{"c":3},"list":["y"]} and the second text as [{"c":3},["q"]].
    testCase "a tree built with the plain constructors comes back equal" <| fun _ ->
        let tagged = YamlNode.mapping [ entryOf "c" (sc "3") ] |> YamlNode.withTag (YamlTag.core "map") |> YamlNode.withAnchor "m"
        let tree =
            YamlNode.mapping [
                entryOf "map" (YamlNode.mapping [ entryOf "a" (sc "1") ])
                entryOf "seq" (YamlNode.sequence [ YamlNode.item (sc "x"); YamlNode.item (YamlNode.mapping [ entryOf "b" (sc "2") ]) ])
                entryOf "tagged" tagged
                entryOf "list" (YamlNode.withAnchor "s" (YamlNode.sequence [ YamlNode.item (sc "y") ]))
            ]
        let text = writeNode tree
        Expect.equal text "map:\n  a: 1\nseq:\n  - x\n  - b: 2\ntagged: !!map &m\n  c: 3\nlist: &s\n  - y\n" "The text"
        Expect.nodeEqual (parseRoot text) tree "The tree read from the text"
        Expect.equal (Writer.writeNode defaults (parseRoot text)) text "The second write"
        let root = YamlNode.sequence [ YamlNode.item tagged; YamlNode.item (YamlNode.sequence [ YamlNode.item (sc "q") ]) ]
        let rootText = writeNode root
        Expect.equal rootText "- !!map &m\n  c: 3\n- - q\n" "The text of a sequence"
        Expect.nodeEqual (parseRoot rootText) root "The sequence read from the text"

    // yaml@2.6.1 reads the expected texts as {"a":"b"}, {"a":null,"b":"c"}, {"a":null}, {"a\nb":"c"}, {"a\n":"b\n"}.
    testCase "an explicit key and an explicit value" <| fun _ ->
        expectNode (YamlNode.mapping [ explicitEntry (sc "a") (sc "b") ]) "? a\n: b\n"
        expectNode (YamlNode.mapping [ explicitKeyOnly (sc "a"); entryOf "b" (sc "c") ]) "? a\nb: c\n"
        expectNode (YamlNode.mapping [ explicitEntry (sc "a") nothing ]) "? a\n:\n"
        expectNode (YamlNode.mapping [ explicitEntry (sc "a\nb") (sc "c") ]) "? a\n\n  b\n: c\n"
        expectNode (YamlNode.mapping [ explicitEntry (lit "a\n") (lit "b\n") ]) "? |\n  a\n: |\n  b\n"

    // yaml@2.6.1 reads the expected texts as {"[\"a\"]":"b"}, {"{\"a\":1}":"b"}, {"a":"b"}, {"a\n":"b"}, {"":"b"}, {"":"b"}.
    testCase "a key that cannot stand without a question mark gets one" <| fun _ ->
        expectNode (YamlNode.mapping [ YamlNode.entry (seqOf [ sc "a" ]) (sc "b") ]) "? - a\n: b\n"
        expectNode (YamlNode.mapping [ YamlNode.entry (mapOf [ "a", sc "1" ]) (sc "b") ]) "? a: 1\n: b\n"
        expectNode (YamlNode.mapping [ YamlNode.entry (withNote " c" (sc "a")) (sc "b") ]) "? a # c\n: b\n"
        // A block scalar as an implicit key is double-quoted, because it cannot stand on one line.
        expectNode (YamlNode.mapping [ YamlNode.entry (lit "a\n") (sc "b") ]) "\"a\\n\": b\n"
        let long = String.replicate 1100 "a"
        expectNode (YamlNode.mapping [ YamlNode.entry (sc long) (sc "b") ]) ("? " + long + "\n: b\n")
        let atLimit = String.replicate 1024 "a"
        expectNode (YamlNode.mapping [ YamlNode.entry (sc atLimit) (sc "b") ]) (atLimit + ": b\n")

    // yaml@2.6.1 reads the expected texts as {"":"v"}, {"a":null,"":"v"}, {"a":"b","":"c"}.
    testCase "an empty key is implicit unless the entry before it is an explicit key without a value" <| fun _ ->
        expectNode (YamlNode.mapping [ YamlNode.entry nothing (sc "v") ]) ": v\n"
        expectNode (YamlNode.mapping [ explicitKeyOnly (sc "a"); YamlNode.entry nothing (sc "v") ]) "? a\n?\n: v\n"
        expectStable ": v\n"
        expectStable "? a\n: b\n: c\n"

    // yaml@2.6.1 reads the expected texts as {"k":null,"j":1}, [null,"a"], {"k":null}.
    testCase "a missing value and a comment behind the colon" <| fun _ ->
        expectNode (mapOf [ "k", nothing; "j", sc "1" ]) "k:\nj: 1\n"
        expectNode (seqOf [ nothing; sc "a" ]) "-\n- a\n"
        expectNode (YamlNode.mapping [ { entryOf "k" nothing with Colon = Some(newLine (Some " c") []) } ]) "k: # c\n"

    // yaml@2.6.1 reads the expected texts as {"a":{"b":["x",{"c":1,"d":"t\n"}]}}, {"a":{"b":["x",{"c":1,"d":"t\n"}]}}, {"a":1,"b":["x"],"c":"t\nu\n"}, {"k":"a\nb"}.
    testCase "an indentation of four spaces, an indentation of one space and the line break CRLF" <| fun _ ->
        let node = mapOf [ "a", mapOf [ "b", seqOf [ sc "x"; mapOf [ "c", sc "1"; "d", lit "t\n" ] ] ] ]
        Expect.equal (Writer.writeNode { defaults with Indent = 4 } node) "a:\n    b:\n        - x\n        - c: 1\n          d: |\n              t\n" "Indent 4"
        Expect.equal (Writer.writeNode { defaults with Indent = 1 } node) "a:\n b:\n  - x\n  - c: 1\n    d: |\n     t\n" "Indent 1"
        let crlf = { defaults with NewLine = "\r\n" }
        Expect.equal (Writer.writeNode crlf (mapOf [ "a", sc "1"; "b", seqOf [ sc "x" ]; "c", lit "t\nu\n" ])) "a: 1\r\nb:\r\n  - x\r\nc: |\r\n  t\r\n  u\r\n" "CRLF"
        Expect.equal (Writer.writeNode crlf (mapOf [ "k", sc "a\nb" ])) "k: a\r\n\r\n  b\r\n" "CRLF in a scalar"

    // yaml@2.6.1 reads the expected texts as [{"k":" a\n"}], [{"k":" a\n"}], " a\n", "a\n".
    testCase "the indicator of a block scalar counts from the indentation of its parent" <| fun _ ->
        let node = seqOf [ mapOf [ "k", lit " a\n" ] ]
        expectNode node "- k: |2\n     a\n"
        Expect.equal (Writer.writeNode { defaults with Indent = 4 } node) "- k: |4\n       a\n" "Indent 4"
        expectNode (lit " a\n") "|2\n   a\n"
        expectNode (lit "a\n") "|\n  a\n"
]

let private policyTests = testList "Style policies" [
    let requireStored (node: YamlNode) = Writer.writeNode (withStyles RequireStored) node
    let canonical (node: YamlNode) = Writer.writeNode (withStyles Canonical) node

    testCase "RequireStored raises when a plain scalar cannot stand plain" <| fun _ ->
        let error = writeError (fun () -> requireStored (mapOf [ "a", mapOf [ "k", sc "x: y" ] ]))
        Expect.stringContains error.Message "plain" "The message"
        Expect.equal error.Path [ "a"; "k" ] "The path names the key"
        let inSequence = writeError (fun () -> requireStored (seqOf [ sc "a"; sc "- b" ]))
        Expect.equal inSequence.Path [ "1" ] "The path names the item"

    testCase "RequireStored raises for a single-quoted scalar, a block scalar and a block header that disagree with the value" <| fun _ ->
        let next = "j", sc "1"
        Expect.stringContains (writeError (fun () -> requireStored (mapOf [ "k", sq "a\u0001" ]))).Message "single quotes" "single"
        Expect.stringContains (writeError (fun () -> requireStored (mapOf [ "k", lit "a\u0001\n" ]))).Message "block scalar" "block"
        Expect.stringContains (writeError (fun () -> requireStored (mapOf [ "k", blockScalar BlockScalarStyle.Literal ChompingMode.Clip None "a"; next ]))).Message "chomping" "chomping"
        Expect.stringContains (writeError (fun () -> requireStored (mapOf [ "k", lit " a\n" ]))).Message "indentation indicator" "indicator"

    testCase "RequireStored accepts what the stored styles express" <| fun _ ->
        let node = mapOf [ "a", sc "x"; "b", sq "it's"; "c", dq "\u0001"; "d", lit "t\n"; "e", flowSeq [ sc "1" ]; "f", blockScalar BlockScalarStyle.Literal ChompingMode.Clip (Some 2) " t\n" ]
        Expect.equal (requireStored node) (writeNode node) "The same text"

    testCase "RequireStored raises when a key needs a question mark or a block collection stands in a flow collection" <| fun _ ->
        let key = YamlNode.mapping [ YamlNode.entry (seqOf [ sc "a" ]) (sc "b") ]
        Expect.stringContains (writeError (fun () -> requireStored key)).Message "key" "key"
        let error = writeError (fun () -> requireStored (mapOf [ "k", flowSeq [ seqOf [ sc "a" ] ] ]))
        Expect.stringContains error.Message "block collection" "block in flow"
        Expect.equal error.Path [ "k"; "0" ] "The path"
        let emptyKey = YamlNode.mapping [ explicitKeyOnly (sc "a"); YamlNode.entry nothing (sc "v") ]
        Expect.stringContains (writeError (fun () -> requireStored emptyKey)).Message "empty key" "empty key"

    // yaml@2.6.1 reads the expected texts as {"k":[["a","b"],{"c":1}]}, [["a"]].
    testCase "PreferStored turns a block collection in a flow collection into a flow collection" <| fun _ ->
        expectNode (mapOf [ "k", flowSeq [ seqOf [ sc "a"; sc "b" ]; mapOf [ "c", sc "1" ] ] ]) "k: [[a, b], {c: 1}]\n"
        // The comment behind the dash has no slot in a flow sequence, so it goes in front of the item.
        let commented = flowSeq [ YamlNode.sequence [ { YamlNode.item (sc "a") with Dash = Some(newLine (Some " d") [ note " f" ]) } ] ]
        expectNode commented "[\n  [\n    # d\n    # f\n    a\n  ]\n]\n"

    testCase "Canonical writes plain where that keeps the meaning and double quotes otherwise" <| fun _ ->
        Expect.equal (canonical (mapOf [ "a", sq "text"; "b", dq "two words"; "c", lit "line\n" ])) "a: text\nb: two words\nc: \"line\\n\"\n" "Styles are ignored"
        Expect.equal
            (canonical (mapOf [ "a", dq "true"; "b", dq "5"; "c", sq "null"; "d", sq "~"; "e", dq "1.5"; "f", dq "" ]))
            "a: \"true\"\nb: \"5\"\nc: \"null\"\nd: \"~\"\ne: \"1.5\"\nf: \"\"\n"
            "Text that a core reader takes for another type"
        Expect.equal (canonical (mapOf [ "a", sc "true"; "b", sc "5"; "c", sc "null"; "d", sc "" ])) "a: true\nb: 5\nc: null\nd:\n" "A plain scalar keeps its meaning"
        Expect.equal (canonical (mapOf [ "a", sc "x: y"; "b", sq "a\nb"; "c", sq "it's" ])) "a: \"x: y\"\nb: \"a\\nb\"\nc: it's\n" "Unsafe text"
        Expect.equal (canonical (mapOf [ "a", YamlNode.withTag (YamlTag.core "str") (sq "5") ])) "a: !!str 5\n" "A tag fixes the type"

    testCase "Canonical writes block collections and keeps flow collections that are empty" <| fun _ ->
        Expect.equal
            (canonical (mapOf [ "a", flowSeq [ sc "x"; sc "y" ]; "b", flowMap [ "k", sc "v" ]; "c", flowSeq []; "d", flowMap [] ]))
            "a:\n  - x\n  - y\nb:\n  k: v\nc: []\nd: {}\n"
            "Collections"
        Expect.equal (canonical (flowSeq [ flowSeq [ sc "a" ] ])) "- - a\n" "Nested"

    // The opening comment of a flow collection has no slot in a block collection. It stands on a line in front of the first item, and the
    // comment behind the closer stands at the end of the block.
    testCase "Canonical keeps every comment and blank line, including the ones of a flow collection" <| fun _ ->
        let flow = withNote " after" (withOpening " open" (flowSeq [ withNote " item" (sc "a"); sc "b" ]))
        Expect.equal (canonical (mapOf [ "k", flow ])) "k:\n  # open\n  - a # item\n  - b\n  # after\n" "The comments of a flow collection"
        Expect.equal (canonical (mapOf [ "a", withHeader " h" (lit "x\n"); "b", sc "1" ])) "a: \"x\\n\" # h\nb: 1\n" "The header comment of a block scalar"
        Expect.equal (canonical (seqOf [ sc "a" ] |> withFirstBefore [ note " c"; blank ])) "# c\n\n- a\n" "Trivia"
        // A flow collection with trivia in front of its first item is a valid tree. As a block collection it cannot continue the line of the dash.
        let flow = YamlNode.flowSequence [ itemBefore [ note " c" ] (sc "a") ]
        Expect.equal (canonical (seqOf [ flow ])) "-\n  # c\n  - a\n" "Trivia in front of the first item"
        Expect.equal (canonical (seqOf [ withOpening " o" (flowSeq [ sc "a" ]) ])) "-\n  # o\n  - a\n" "Opening comment"
]

let private invalidTreeTests = testList "Invalid trees" [
    testCase "a tree that breaks a slot rule raises with the messages of Validate" <| fun _ ->
        let broken = { mapOf [ "k", sc "v" ] with TrailingComment = Some(comment " c") }
        let error = writeError (fun () -> writeNode broken)
        Expect.stringContains error.Message "root.TrailingComment" "The slot"
        Expect.stringContains error.Message "R8" "The rule"
        Expect.equal error.Path [] "No path"
        let inStream = writeError (fun () -> writeStream [ StreamPart.Document(document broken) ])
        Expect.stringContains inStream.Message "Parts[0].root.TrailingComment" "The slot in a stream"

    // yaml@2.6.1 reads the expected texts as {"c":2,"a":{"b":1}}.
    testCase "the trivia at the end of a collection that is not the last node of its document break rule R4" <| fun _ ->
        let inner = withEnd [ note " end" ] (mapOf [ "b", sc "1" ])
        let error = writeError (fun () -> writeNode (mapOf [ "a", inner; "c", sc "2" ]))
        Expect.stringContains error.Message ".End" "The slot"
        Expect.stringContains error.Message "R4" "The rule"
        // The same End is valid in the last entry.
        expectNode (mapOf [ "c", sc "2"; "a", inner ]) "c: 2\na:\n  b: 1\n  # end\n"

    // yaml@2.6.1 rejects `k: - a` and `k: !!seq - a`, because a block collection cannot start on the line of an implicit key or of a property.
    testCase "a block collection on the line of a colon or a property breaks rule R1" <| fun _ ->
        let onColonLine = { entryOf "k" YamlNode.empty with Value = seqOf [ sc "a" ] }
        let error = writeError (fun () -> writeNode (YamlNode.mapping [ onColonLine ]))
        Expect.stringContains error.Message "root.entries[0].Colon" "The slot of the colon"
        Expect.stringContains error.Message "R1" "The rule"
        let tagged = { YamlNode.withTag (YamlTag.core "seq") (seqOf [ sc "a" ]) with PropertyLayout = [ placement PropertyName.Tag Separation.Inline ] }
        let behindProperty = writeError (fun () -> writeNode (mapOf [ "k", tagged ]))
        Expect.stringContains behindProperty.Message "root.entries[0].Value.PropertyLayout[0].After" "The slot of the property"

    // yaml@2.6.1 reads the expected texts as [["a"]].
    testCase "trivia in front of the first item of a collection that continues the line of its dash are an error" <| fun _ ->
        let inner = YamlNode.sequence [ { YamlNode.item (sc "a") with Before = [ note " c" ] } ]
        let error = writeError (fun () -> writeNode (YamlNode.sequence [ YamlNode.item inner ]))
        Expect.stringContains error.Message "root.items[0].Node.items[0].Before" "The slot"
        // The dash with a line break holds the trivia.
        expectNode (YamlNode.sequence [ itemOnNewLine inner ]) "-\n  # c\n  - a\n"

    testCase "a comment with a line break is an error" <| fun _ ->
        let error = writeError (fun () -> writeNode (withNote " a\nb" (sc "v")))
        Expect.stringContains error.Message "line break" "The message"

    testCase "bad options are an error" <| fun _ ->
        Expect.stringContains (writeError (fun () -> Writer.writeNode { defaults with Indent = 0 } (sc "a"))).Message "indentation" "indent"
        Expect.stringContains (writeError (fun () -> Writer.writeNode { defaults with NewLine = "\n\n" } (sc "a"))).Message "line break" "newline"
]

let private depth = 10000

/// A block sequence nested `levels` deep around one scalar.
let private nestedSequence (levels: int) : YamlNode =
    let mutable node = sc "x"
    for _ in 1 .. levels do
        node <- YamlNode.sequence [ YamlNode.item node ]
    node

/// A block mapping nested `levels` deep as the value of its only entry.
let private nestedMapping (levels: int) : YamlNode =
    let mutable node = sc "x"
    for _ in 1 .. levels do
        node <- mapOf [ "k", node ]
    node

/// A flow sequence nested `levels` deep around one scalar.
let private nestedFlow (levels: int) : YamlNode =
    let mutable node = sc "x"
    for _ in 1 .. levels do
        node <- flowSeq [ node ]
    node

let private repeated (text: string) (count: int) : string =
    let sb = System.Text.StringBuilder()
    for _ in 1 .. count do
        sb.Append(text) |> ignore
    sb.ToString()

/// A parse of the text that the operation reuses, so the timing measures the write.
let private cachedStreams = System.Collections.Generic.Dictionary<string, YamlStream>()

let private streamOfText (text: string) : YamlStream =
    match cachedStreams.TryGetValue text with
    | true, stream -> stream
    | _ ->
        let stream = parseOk text
        cachedStreams.[text] <- stream
        stream

let private writeText (text: string) = Writer.writeDocuments defaults (streamOfText text) |> ignore

/// Writes a flow sequence with one scalar for each two characters of the text, which is one line of the input `x`.
let private writeFlowOfText (text: string) =
    let items = [ for _ in 1 .. text.Length / 2 -> YamlNode.item (sc "x") ]
    Writer.writeNode defaults (YamlNode.flowSequence items) |> ignore

/// A block sequence nested 200 levels deep that holds one mapping for each eight characters of the text. Each mapping has a
/// literal scalar with the chomping Clip and no final line feed in its value, and another entry behind it. The writer asks for
/// each scalar whether it ends the text, and the answer has to take no walk through the 200 open collections. The indentation
/// of a line grows with the depth, so the depth stays fixed and the number of scalars grows with the size of the text.
let private writeClipScalarsAtDepth (text: string) =
    let clip = blockScalar BlockScalarStyle.Literal ChompingMode.Clip None "a"
    let items = [ for _ in 1 .. text.Length / 8 -> YamlNode.item (mapOf [ "k", clip; "j", sc "1" ]) ]
    let mutable node = YamlNode.sequence items
    for _ in 1 .. 200 do
        node <- YamlNode.sequence [ YamlNode.item node ]
    Writer.writeNode defaults node |> ignore

let private depthTests = testList "Deep trees and linear time" [
    testCase "a block sequence nested 10000 levels deep" <| fun _ ->
        Expect.equal (writeNode (nestedSequence depth)) (repeated "- " depth + "x\n") "One dash on the line for each level"

    // The line of level i has i spaces of indentation and the key `k:`, and the last line holds the scalar behind its colon.
    testCase "a block mapping nested 10000 levels deep" <| fun _ ->
        let text = Writer.writeNode { defaults with Indent = 1 } (nestedMapping depth)
        Expect.isTrue (text.StartsWith "k:\n k:\n  k:\n   k:\n") "The first lines"
        Expect.isTrue (text.EndsWith "k: x\n") "The last line"
        Expect.equal text.Length (depth * (depth - 1) / 2 + 3 * depth + 2) "The size follows from the indentation"

    testCase "a flow sequence nested 10000 levels deep" <| fun _ ->
        Expect.equal (writeNode (nestedFlow depth)) (repeated "[" depth + "x" + repeated "]" depth + "\n") "Brackets around the scalar"

    testCase "a block sequence and a flow sequence nested in turn 10000 levels deep" <| fun _ ->
        let mutable node = sc "x"
        for level in 1 .. depth do
            node <- if level % 2 = 0 then YamlNode.sequence [ YamlNode.item node ] else flowSeq [ node ]
        Expect.isTrue ((writeNode node).Length > depth) "The text is written"

    testCase "writing block mappings takes time linear in the number of entries" <| fun _ ->
        expectLinear "write of block mappings" writeText countLineBreaks (fun lines -> repeated "key: value\n" lines)

    testCase "writing block sequences takes time linear in the number of items" <| fun _ ->
        expectLinear "write of block sequences" writeText countLineBreaks (fun lines -> repeated "- item\n" lines)

    testCase "writing comments and blank lines takes time linear in their number" <| fun _ ->
        expectLinear "write of comments" writeText countLineBreaks (fun lines -> repeated "# c\n\na: 1 # t\n" (lines / 3))

    testCase "writing block scalars takes time linear in their size" <| fun _ ->
        expectLinear "write of block scalars" writeText countLineBreaks (fun lines -> repeated "k: |\n  text\n  more\n" (lines / 3))

    testCase "writing flow collections takes time linear in their size" <| fun _ ->
        expectLinear "write of a flow sequence" writeFlowOfText countLineBreaks (fun lines -> repeated "x\n" lines)

    testCase "writing literal scalars without a final line feed at depth takes time linear in their number" <| fun _ ->
        expectLinear "write of Clip scalars at depth" writeClipScalarsAtDepth countLineBreaks (fun lines -> repeated "x\n" lines)

    // The last node of the text is found through 10000 open collections, and a scalar that has another entry behind it is not the last node.
    testCase "a literal scalar without a final line feed at the end of a text nested 10000 levels deep" <| fun _ ->
        let clip = blockScalar BlockScalarStyle.Literal ChompingMode.Clip None "a"
        let mutable last = clip
        for _ in 1 .. depth do
            last <- YamlNode.sequence [ YamlNode.item last ]
        let text = writeNode last
        Expect.equal text (repeated "- " depth + "|\n" + String.replicate (2 * depth) " " + "a") "The text ends in the scalar"
        let mutable notLast = YamlNode.sequence [ YamlNode.item clip; YamlNode.item (sc "b") ]
        for _ in 2 .. depth do
            notLast <- YamlNode.sequence [ YamlNode.item notLast ]
        let strip = writeNode notLast
        Expect.isTrue (strip.StartsWith(repeated "- " (depth - 1) + "- |-\n")) "The chomping is Strip"
        Expect.isTrue (strip.EndsWith "- b\n") "The text ends with a line break"

    testCase "writing nested block collections takes time linear in their number" <| fun _ ->
        expectLinear "write of nested collections" writeText countLineBreaks (fun lines -> repeated "a:\n  - b: 1\n    c: d\n" (lines / 3))
]

let Main =
    testList "Writer" [ portedTests; slotTests; scalarTests; tagTests; documentTests; blockLayoutTests; policyTests; invalidTreeTests; depthTests ]
