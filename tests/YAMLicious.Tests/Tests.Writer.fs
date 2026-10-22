module Tests.Writer

// The tests of the writer. A test builds a tree with the constructors, or parses a text, and compares the text that the
// writer gives with the expected text. A tree that the parser produced has to come back as the same text, and the text has to
// read as the same tree, so each slot of the model is tested with a text that goes through the parser and the writer.
// The tests of the flow slots build the tree and compare the text. The flow inputs of Tests.RoundTrip go through the flow parser.
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

// Characters that a test needs and a source file cannot show: built at run time, so that no editor or compiler changes them.
let private bom = string (char 0xFEFF)
let private nel = string (char 0x85)
let private nbsp = string (char 0xA0)
let private lineSeparator = string (char 0x2028)
let private paragraphSeparator = string (char 0x2029)
let private del = string (char 0x7F)
let private c1Control = string (char 0x81)

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

/// An entry without a colon and without a value, as `{a}` in a flow mapping.
let private keyOnly (key: YamlNode) : MappingEntry = { YamlNode.entry key nothing with Colon = None }

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

    // Rule R6: the slots of a flow collection. The tests build the trees and compare the text.
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
        expectNode (YamlNode.flowMapping [ explicitEntry (sc "a") (sc "b") ]) "{? a: b}\n"
        expectNode (YamlNode.flowMapping [ explicitKeyOnly (sc "a"); entryOf "b" nothing ]) "{? a, b:}\n"

    // yaml@2.6.1 reads the expected texts as ["a","b"], {"k":["a","b"]}.
    testCase "the comma stands in front of the trailing comment" <| fun _ ->
        let node = flowSeq [ withNote " one" (sc "a"); withNote " two" (sc "b") ]
        expectNode node "[\n  a, # one\n  b # two\n]\n"
        expectNode (mapOf [ "k", node ]) "k: [\n  a, # one\n  b # two\n  ]\n"

    // yaml@2.6.1 reads the expected texts as [["a","b"]], {"k":{"j":["a","b"]}}, [{"k":["a","b"]}], {"k":[["a"],"b"]} and {"k":{"a":"b"}}. A flow collection that a block node owns
    // has its lines at least one level in from the block node ([69] and [80]), and the closer is such a line. So the closer stands at the column of the items.
    testCase "the closer of a flow collection in a block collection stands at the items column" <| fun _ ->
        let node = flowSeq [ withNote " one" (sc "a"); sc "b" ]
        expectNode (seqOf [ node ]) "- [\n  a, # one\n  b\n  ]\n"
        expectNode (mapOf [ "k", mapOf [ "j", node ] ]) "k:\n  j: [\n    a, # one\n    b\n    ]\n"
        expectNode (seqOf [ mapOf [ "k", node ] ]) "- k: [\n    a, # one\n    b\n    ]\n"
        let nested = flowSeq [ flowSeq [ withNote " x" (sc "a") ]; sc "b" ]
        expectNode (mapOf [ "k", nested ]) "k: [\n  [\n    a # x\n  ],\n  b\n  ]\n"
        let commented = withEnd [ note " end" ] (flowMap [ "a", sc "b" ])
        expectNode (mapOf [ "k", commented ]) "k: {\n  a: b\n  # end\n  }\n"

    // yaml@2.6.1 reads the expected texts as {"a":1,"b":2}.
    testCase "a flow mapping with comments puts one entry on each line" <| fun _ ->
        let entries = [ { entryOf "a" (sc "1") with Before = [ note " first" ] }; entryOf "b" (withNote " second" (sc "2")) ]
        expectNode (withEnd [ blank ] (YamlNode.flowMapping entries)) "{\n  # first\n  a: 1,\n  b: 2 # second\n\n}\n"

    // yaml@2.6.1 reads the expected texts as {"a":"b"} for the first three, {"":"b"} for the next three, {"a":null} for the next two and {"a":null,"j":1} for the last one.
    // The comment behind a key stands on the line of the key, the lines of BeforeColon follow it, and the colon stands on the line behind them ([147]).
    // The key of an entry that has a colon and no value keeps its comment behind it, because the comment behind the colon belongs to the colon.
    testCase "the gap between a key and its colon in a flow mapping" <| fun _ ->
        let gap = { entryOf "a" (sc "b") with BeforeColon = [ note " gap" ] }
        expectNode (YamlNode.flowMapping [ gap ]) "{\n  a\n  # gap\n  : b\n}\n"
        let commentedKey = YamlNode.entry (withNote " key" (sc "a")) (sc "b")
        expectNode (YamlNode.flowMapping [ commentedKey ]) "{\n  a # key\n  : b\n}\n"
        expectNode (YamlNode.flowMapping [ { commentedKey with BeforeColon = [ note " gap" ] } ]) "{\n  a # key\n  # gap\n  : b\n}\n"
        // A key that holds properties only has the comment in the After of its last property or in its TrailingComment. The text is the same.
        let anchored = YamlNode.withAnchor "a" nothing
        let afterProperty = withLayout [ placement PropertyName.Anchor (newLine (Some " key") []) ] anchored
        expectNode (YamlNode.flowMapping [ YamlNode.entry afterProperty (sc "b") ]) "{\n  &a # key\n  : b\n}\n"
        expectNode (YamlNode.flowMapping [ YamlNode.entry (withNote " key" anchored) (sc "b") ]) "{\n  &a # key\n  : b\n}\n"
        // Both comments of such a key stay, the second one on a line of its own.
        expectNode (YamlNode.flowMapping [ YamlNode.entry (withNote " second" afterProperty) (sc "b") ]) "{\n  &a # key\n  # second\n  : b\n}\n"
        // An entry with a colon and no value keeps the comment of the key behind the key, and the colon keeps its own comment.
        let noValue = { commentedKey with Value = nothing }
        expectNode (YamlNode.flowMapping [ noValue ]) "{\n  a # key\n  :\n}\n"
        expectNode (YamlNode.flowMapping [ { noValue with Colon = Some(newLine (Some " colon") []) } ]) "{\n  a # key\n  : # colon\n}\n"
        expectNode (YamlNode.flowMapping [ noValue; entryOf "j" (sc "1") ]) "{\n  a # key\n  :,\n  j: 1\n}\n"

    // yaml@2.6.1 reads the expected texts as [["a"],"b"].
    testCase "a comment behind the closer of a flow collection and a nested flow collection" <| fun _ ->
        let inner = withNote " closer" (flowSeq [ withNote " x" (sc "a") ])
        expectNode (flowSeq [ inner; sc "b" ]) "[\n  [\n    a # x\n  ], # closer\n  b\n]\n"

    // yaml@2.6.1 reads the expected texts as {"k":["a"]}, {"[\"a\",\"b\"]":"v"}. A flow collection as a key has the normal spelling, with a blank behind the comma, and the shortest spelling only
    // when the normal one passes the limit of 1024 characters.
    testCase "a flow collection with properties and a flow collection as a key" <| fun _ ->
        let tagged = YamlNode.withAnchor "x" (YamlNode.withTag (YamlTag.core "seq") (flowSeq [ sc "a" ]))
        expectNode (mapOf [ "k", tagged ]) "k: !!seq &x [a]\n"
        let keyed = YamlNode.mapping [ YamlNode.entry (flowSeq [ sc "a"; sc "b" ]) (sc "v") ]
        expectNode keyed "[a, b]: v\n"

    // yaml@2.6.1 reads the expected texts as ["","b"].
    // yaml@2.6.1 reads `{? }` as {"":null}, `{? , a}` as {"":null,"a":null} and `{? : v}` as {"":"v"}. The question mark needs a blank behind it,
    // even when the key is empty ([142] and [150]).
    testCase "an empty explicit key in a flow mapping has a blank behind the question mark" <| fun _ ->
        let emptyKey = explicitKeyOnly nothing
        expectNode (YamlNode.flowMapping [ emptyKey ]) "{? }\n"
        expectNode (YamlNode.flowMapping [ emptyKey; keyOnly (sc "a") ]) "{? , a}\n"
        expectNode (YamlNode.flowMapping [ explicitEntry nothing (sc "v") ]) "{? : v}\n"
        expectNode (flowSeq [ YamlNode.flowMapping [ emptyKey ] ]) "[{? }]\n"

    // yaml@2.6.1 reads `[ &a, # c5` followed by `]` as [null] and `{a: &a, # c5` followed by `}` as {"a":null}. The comma stands between the property and
    // the comment, so the comment is the trailing comment of the item and not the comment of the line break behind the property (rules R1 and R6).
    testCase "the last item that holds properties only and has a comment gets its comma" <| fun _ ->
        let anchored = withNote " c5" (YamlNode.withAnchor "a" YamlNode.empty)
        expectNode (flowSeq [ anchored ]) "[\n  &a, # c5\n]\n"
        expectNode (YamlNode.flowMapping [ YamlNode.entry (sc "a") anchored ]) "{\n  a: &a, # c5\n}\n"
        expectNode (flowSeq [ sc "x"; anchored ]) "[\n  x,\n  &a, # c5\n]\n"
        // A comment behind another last item needs no comma.
        expectNode (flowSeq [ withNote " c5" (sc "x") ]) "[\n  x # c5\n]\n"

    // yaml@2.6.1 reads `{a`, an empty line and `b}` as a key with a line feed and the value null, and `{[a, # c`, `b]: d}` as the key ["a","b"] with the value "d".
    // A key of a flow mapping runs over several lines when it is a plain scalar, a quoted scalar or a flow collection ([143] to [147]). Only the key of a block mapping and of a flow pair stands on one line.
    testCase "the key of a flow mapping may run over several lines and stays an implicit key" <| fun _ ->

        expectNode (YamlNode.flowMapping [ keyOnly (sc "a\nb") ]) "{\n  a\n\n  b\n}\n"
        expectNode (YamlNode.flowMapping [ YamlNode.entry (sc "a\nb") (sc "1") ]) "{\n  a\n\n  b: 1\n}\n"
        expectNode (YamlNode.flowMapping [ YamlNode.entry (sq "a\nb") (sc "1") ]) "{\n  'a\n\n  b': 1\n}\n"
        let key = flowSeq [ withNote " c" (sc "a"); sc "b" ]
        expectNode (YamlNode.flowMapping [ YamlNode.entry key (sc "d") ]) "{\n  [\n    a, # c\n    b\n  ]: d\n}\n"
        expectNode (YamlNode.flowMapping [ keyOnly key ]) "{\n  [\n    a, # c\n    b\n  ]\n}\n"
        // RequireStored keeps the stored styles of these keys.
        let required = withStyles RequireStored
        Expect.equal (Writer.writeNode required (YamlNode.flowMapping [ YamlNode.entry (sc "a\nb") (sc "1") ])) "{\n  a\n\n  b: 1\n}\n" "plain key"
        Expect.equal (Writer.writeNode required (YamlNode.flowMapping [ YamlNode.entry key (sc "d") ])) "{\n  [\n    a, # c\n    b\n  ]: d\n}\n" "flow key"
        // The key of a block mapping still stands on one line.
        expectNode (YamlNode.mapping [ YamlNode.entry (sc "a\nb") (sc "1") ]) "\"a\\nb\": 1\n"

    // yaml@2.6.1 reads `{? x:: v}` as {"x:":"v"} and `{? k1 :: foo}` as {"k1 :":"foo"}. The key of an explicit entry is the key of an implicit entry, so a plain key may
    // end with a colon, and the colon of the entry follows it without a blank.
    testCase "a plain key that ends with a colon stays plain in a flow mapping" <| fun _ ->
        expectNode (YamlNode.flowMapping [ explicitEntry (sc "x:") (sc "v") ]) "{? x:: v}\n"
        expectNode (YamlNode.flowMapping [ explicitEntry (sc "k1 :") (sc "foo") ]) "{? k1 :: foo}\n"
        expectNode (YamlNode.flowMapping [ YamlNode.entry (sc "x:") (sc "v") ]) "{x:: v}\n"
        // Without a colon behind it the key cannot end with one.
        expectNode (YamlNode.flowMapping [ keyOnly (sc "x:") ]) "{\"x:\"}\n"

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
            let escaped = text.Replace("\\", "\\\\").Replace("\"", "\\\"")
            Expect.equal written ("k: \"" + escaped + "\"\n") (sprintf "The text for %A" text)
            Expect.equal (valueJson (parseRoot written)) (valueJson (mapOf [ "k", dq text ])) (sprintf "The value for %A" text)

    // yaml@2.6.1 reads the expected texts as {"k":"a\u0001b"}, {"k":"\u007F"}, {"k":"\u0085"}, {"k":"\uFFFE"}, {"k":"a\r\nb"}, {"k":"\uFEFFx"}.
    testCase "a plain scalar with a control character or a surrogate is escaped" <| fun _ ->
        expectNode (mapOf [ "k", sc "a\u0001b" ]) "k: \"a\\x01b\"\n"
        expectNode (mapOf [ "k", sc "\u007F" ]) "k: \"\\x7F\"\n"
        expectNode (mapOf [ "k", sc "\u0085" ]) ("k: " + nel + "\n")
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

    // yaml@2.6.1 reads the expected texts as "--- a", "...", {"k":"--- a"}, {"--- a":"v"}, {"a":"1","... b":"v"}, ["--- a"] and the other texts in the same way.
    // A root plain scalar and the key of a root mapping start at the first column, where `---` and `...` mark a document. The writer puts the root one column in,
    // and the same text is a plain scalar there. A double-quoted scalar and a scalar behind an indicator need nothing.
    testCase "a root plain scalar that starts with a document marker stands one column in" <| fun _ ->
        expectNode (sc "--- a") " --- a\n"
        expectNode (sc "...") " ...\n"
        expectNode (sc "--- a\nb") " --- a\n\n  b\n"
        expectNode (mapOf [ "k", sc "--- a" ]) "k: --- a\n"
        expectNode (mapOf [ "--- a", sc "v" ]) " --- a: v\n"
        expectNode (mapOf [ "a", sc "1"; "... b", sc "v"; "c", mapOf [ "--- d", sc "w" ] ]) " a: 1\n ... b: v\n c:\n   --- d: w\n"
        expectNode (seqOf [ sc "--- a" ]) "- --- a\n"
        expectNode (dq "--- a") "\"--- a\"\n"
        expectNode (YamlNode.mapping [ YamlNode.entry (dq "--- a") (sc "v") ]) "\"--- a\": v\n"
        // Behind the marker or a property on its line the scalar is not at the first column.
        Expect.equal (Writer.write defaults { document (sc "--- a") with StartMarker = Some(marker None) }) "--- --- a\n" "Behind the start marker"
        Expect.equal (writeNode (YamlNode.withAnchor "x" (sc "--- a"))) "&x --- a\n" "Behind an anchor"
        // A property with a line break behind it leaves the scalar at the first column.
        let onNextLine = withLayout [ placement PropertyName.Anchor (newLine None []) ] (YamlNode.withAnchor "x" (sc "--- a"))
        expectNode onNextLine "&x\n --- a\n"
        // RequireStored keeps the stored style, and Canonical writes the same text.
        Expect.equal (Writer.writeNode (withStyles RequireStored) (sc "--- a")) " --- a\n" "RequireStored"
        Expect.equal (Writer.writeNode (withStyles Canonical) (sc "--- a")) " --- a\n" "Canonical"
        Expect.equal (Writer.writeNode (withStyles Canonical) (mapOf [ "--- a", sc "v" ])) " --- a: v\n" "Canonical key"

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

    // yaml@2.6.1 reads the expected texts as {"k":"a\uFEFFb"}, {"k":"x\uFEFF\ny"}, ["a\uFEFFb"], {"k":"a\uFEFFb"} (the double-quoted text), {"k":"a\uFEFFb\n"} and {"k":"x\uFEFF y"}.
    // A quoted scalar holds nb-json characters ([107] and [118]), which include U+FEFF, and single quotes have no escape that could spell it, so a single-quoted scalar holds it raw.
    // A plain and a block scalar hold nb-char ([27]), which excludes it, so they fall back to double quotes with the escape.
    testCase "a single-quoted scalar holds U+FEFF raw and a plain or block scalar with it falls back to the escape" <| fun _ ->
        expectNode (mapOf [ "k", sq "a\uFEFFb" ]) "k: 'a\uFEFFb'\n"
        expectNode (mapOf [ "k", sq "x\uFEFF\ny" ]) "k: 'x\uFEFF\n\n  y'\n"
        expectNode (flowSeq [ sq "a\uFEFFb" ]) "['a\uFEFFb']\n"
        expectNode (mapOf [ "k", sc "a\uFEFFb" ]) "k: \"a\\uFEFFb\"\n"
        expectNode (mapOf [ "k", lit "a\uFEFFb\n" ]) "k: \"a\\uFEFFb\\n\"\n"
        expectStable "k: 'a\uFEFFb'\n"
        expectWritten "k: 'x\uFEFF\n y'\n" "k: 'x\uFEFF y'\n"

    // yaml@2.6.1 reads each double-quoted text as the value on its left.
    testCase "a double-quoted scalar writes the shortest escape of each character" <| fun _ ->
        let cases =
            [ "\"", "\\\""; "\\", "\\\\"; "\n", "\\n"; "\t", "\t"; "\r", "\\r"; "\u0000", "\\0"; "\u0007", "\\a"; "\u0008", "\\b"
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
        // Tests.ScalarStyles "Writer keeps the chomping of an explicit block style that the value contradicts". The old writer kept the chomping that the node stored, so a stripped value that ended in two line
        // feeds lost them. The writer adjusts the chomping so that the text reads back as the value.
        expectNode (mapOf [ "k", blockScalar BlockScalarStyle.Literal ChompingMode.Strip None "a\n\n"; next ]) "k: |+\n  a\n\nj: 1\n"
        expectNode (mapOf [ "k", blockScalar BlockScalarStyle.Literal ChompingMode.Keep None "a"; next ]) "k: |-\n  a\nj: 1\n"

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

    // yaml@2.6.1 reads `|1` and a comment line as "", `>1` and a comment line as "", `|1-` and a comment line as "", `|1+`, a blank line and a comment line as "\n", `|2` and a comment line
    // as "", `|`, two spaces, `a` and a comment line as "a\n", `|` and two blank lines as "", `|1`, a blank line and a comment line as "", and `|` and a comment line as the content "# c\n".
    // The content of a root block scalar starts at the first column unless an indentation indicator says otherwise, so a comment line behind a body without content lines is content
    // and needs the indicator. The indicator 1 puts the content at the column 1, and the comment at the first column ends the scalar. Blank lines need no indicator.
    testCase "a root block scalar without content gets an indentation indicator when a comment follows it" <| fun _ ->
        let withComment (node: YamlNode) = { document node with AfterRoot = [ note " c" ] }
        Expect.equal (Writer.write defaults (withComment (lit ""))) "|1\n# c\n" "Literal, Clip"
        Expect.equal (Writer.write defaults (withComment (fold ""))) ">1\n# c\n" "Folded, Clip"
        Expect.equal (Writer.write defaults (withComment (blockScalar BlockScalarStyle.Literal ChompingMode.Keep None "\n"))) "|1+\n\n# c\n" "Literal, Keep, a blank line"
        Expect.equal (Writer.write defaults (withComment (blockScalar BlockScalarStyle.Literal ChompingMode.Strip None ""))) "|1-\n# c\n" "Literal, Strip"
        Expect.equal (Writer.write defaults { withComment (lit "") with AfterRoot = [ blank; note " c" ] }) "|1\n\n# c\n" "A blank line in front of the comment"
        // A stored indicator stays, and a scalar with content, without a comment behind it or inside a collection needs none.
        Expect.equal (Writer.write defaults (withComment (blockScalar BlockScalarStyle.Literal ChompingMode.Clip (Some 2) ""))) "|2\n# c\n" "Stored indicator"
        Expect.equal (Writer.write defaults (withComment (lit "a\n"))) "|\n  a\n# c\n" "Content"
        Expect.equal (Writer.write defaults (document (lit ""))) "|\n" "No trivia"
        Expect.equal (Writer.write defaults { document (lit "") with AfterRoot = [ blank; blank ] }) "|\n\n\n" "Blank lines"
        Expect.equal (Writer.write defaults (document (withEnd [ note " c" ] (mapOf [ "k", lit "" ])))) "k: |\n# c\n" "Inside a mapping"
        // The policy that requires the stored style cannot add the indicator.
        let error = writeError (fun () -> Writer.write (withStyles RequireStored) (withComment (lit "")))
        Expect.stringContains error.Message "indentation indicator" "RequireStored"
        Expect.equal (Writer.write (withStyles RequireStored) (withComment (blockScalar BlockScalarStyle.Literal ChompingMode.Clip (Some 1) ""))) "|1\n# c\n" "RequireStored with the stored indicator"
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
    let writeCanonical (text: string) = Writer.writeDocuments (withStyles Canonical) (parseOk text)
    // The data of a text with the flow collections counted as block collections, which is what Canonical makes of them.
    let blockData (text: string) = (Projection.data (parseOk text)).Replace("\"style\":\"flow\"", "\"style\":\"block\"")
    /// The Canonical text of the input is the expected text, it keeps the data, and a second write of the text that it reads as gives the same text.
    let expectTwice (input: string) (expected: string) =
        let once = writeCanonical input
        Expect.equal once expected (sprintf "The Canonical text of %A" input)
        Expect.equal (writeCanonical once) once (sprintf "The second Canonical write of %A" input)
        Expect.equal (blockData once) (blockData input) (sprintf "The data of the Canonical text of %A" input)

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

    // The byte order mark stands raw in single quotes only (see "a single-quoted scalar holds U+FEFF raw" in the scalar spellings).
    testCase "RequireStored keeps a single-quoted scalar with U+FEFF and raises for a plain or a block scalar with it" <| fun _ ->
        let single = mapOf [ "k", sq "a\uFEFFb" ]
        Expect.equal (requireStored single) "k: 'a\uFEFFb'\n" "Single quotes"
        Expect.equal (requireStored (mapOf [ "k", sq "x\uFEFF\ny" ])) "k: 'x\uFEFF\n\n  y'\n" "Single quotes over several lines"
        Expect.stringContains (writeError (fun () -> requireStored (mapOf [ "k", sc "a\uFEFFb" ]))).Message "plain" "plain"
        Expect.stringContains (writeError (fun () -> requireStored (mapOf [ "k", lit "a\uFEFFb\n" ]))).Message "block scalar" "block"
        Expect.equal (canonical single) "k: \"a\\uFEFFb\"\n" "Canonical ignores the stored single quotes"

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
    // comment behind the closer stands at the end of the block when the block ends the text. The test below this one shows where the comment goes otherwise.
    testCase "Canonical keeps every comment and blank line, including the ones of a flow collection" <| fun _ ->
        let flow = withNote " after" (withOpening " open" (flowSeq [ withNote " item" (sc "a"); sc "b" ]))
        Expect.equal (canonical (mapOf [ "k", flow ])) "k:\n  # open\n  - a # item\n  - b\n  # after\n" "The comments of a flow collection"
        Expect.equal (canonical (mapOf [ "a", withHeader " h" (lit "x\n"); "b", sc "1" ])) "a: \"x\\n\" # h\nb: 1\n" "The header comment of a block scalar"
        Expect.equal (canonical (seqOf [ sc "a" ] |> withFirstBefore [ note " c"; blank ])) "# c\n\n- a\n" "Trivia"
        // A flow collection with trivia in front of its first item is a valid tree. As a block collection it cannot continue the line of the dash.
        let flow = YamlNode.flowSequence [ itemBefore [ note " c" ] (sc "a") ]
        Expect.equal (canonical (seqOf [ flow ])) "-\n  # c\n  - a\n" "Trivia in front of the first item"
        Expect.equal (canonical (seqOf [ withOpening " o" (flowSeq [ sc "a" ]) ])) "-\n  # o\n  - a\n" "Opening comment"

    // yaml@2.6.1 reads each expected text as the data of its input: [["a"],"b"], {"k":["a"],"j":1}, {"[ a ]":"v"} (it spells the sequence key as a string), {"k":["a"]},
    // {"a":["x"],"b":1}, [[["a"]],"b"], [["a"]], [["a"],"b"] and {"k":[["a"]],"j":1}.
    // The comment behind the closer of a flow collection and the lines in front of the closer move to the trivia of the next entry or item when something follows
    // the block that replaces the collection, and to the end of the block when the block ends the text. The parser reads them from there (R4), so the second
    // write gives the first text again.
    testCase "Canonical gives the same text twice when the trivia at the closer of a flow collection move" <| fun _ ->
        expectTwice "[[a] # c\n , b]\n" "- - a\n# c\n- b\n"
        expectTwice "k: [a] # c\nj: 1\n" "k:\n  - a\n# c\nj: 1\n"
        expectTwice "? [a] # c\n: v\n" "? - a\n# c\n: v\n"
        expectTwice "k: [a] # c\n" "k:\n  - a\n  # c\n"
        expectTwice "{a: [x] # c\n, b: 1}\n" "a:\n  - x\n# c\nb: 1\n"
        expectTwice "[[[a] # c\n], b]\n" "- - - a\n# c\n- b\n"
        expectTwice "[[a] # c\n # end\n]\n" "- - a\n# c\n# end\n"
        expectTwice "[[a] # c\n]\n" "- - a\n  # c\n"
        // The lines in front of the closer are in the End of the flow collection, and the block that replaces it is not the last node.
        expectTwice "[[a\n # e\n ] , b]\n" "- - a\n# e\n- b\n"
        // Two collections close in a row, and the comment of the inner one stands in front of the comment of the outer one.
        expectTwice "k: [ [a] # c\n ] # o\nj: 1\n" "k:\n  - - a\n# c\n# o\nj: 1\n"

    // yaml@2.6.1 reads the first two inputs as [null,"b"] and {"k":"","j":1}, and the expected texts in the same way. It rejects a comma that stands on a line behind a
    // property, so the third input, which this parser reads, has no reference. A node that holds properties only keeps the comment behind it in its TrailingComment in a
    // flow collection. In a block collection the comment stands behind the properties. When the line holds the comment of the line break behind the property
    // already, the second comment stands in front of what follows.
    testCase "Canonical keeps the comment behind a node that holds properties only" <| fun _ ->
        expectTwice "[&a , # c\n b]\n" "- &a # c\n- b\n"
        expectTwice "{k: !!str , # c\n j: 1}\n" "k: !!str # c\nj: 1\n"
        expectTwice "[&a # c1\n , # c2\n b]\n" "- &a # c1\n# c2\n- b\n"
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

/// A block sequence nested 50 levels deep that holds one mapping for each eight characters of the text. Each mapping has a
/// literal scalar with the chomping Clip and no final line feed in its value, and another entry behind it. The writer asks for
/// each scalar whether it ends the text, and the answer has to take no walk through the 50 open collections. The indentation
/// of a line grows with the depth, so the depth stays fixed and the number of scalars grows with the size of the text.
let private writeClipScalarsAtDepth (text: string) =
    let clip = blockScalar BlockScalarStyle.Literal ChompingMode.Clip None "a"
    let items = [ for _ in 1 .. text.Length / 8 -> YamlNode.item (mapOf [ "k", clip; "j", sc "1" ]) ]
    let mutable node = YamlNode.sequence items
    for _ in 1 .. 50 do
        node <- YamlNode.sequence [ YamlNode.item node ]
    Writer.writeNode defaults node |> ignore

/// A block sequence nested 50 levels deep that holds one mapping for each eight characters of the text. Each mapping has a
/// literal scalar with the chomping Clip and two line feeds in its value, which gets the chomping Keep, and a blank line stands in
/// the Before of the next item. The writer asks for each scalar whether a blank line follows, and the answer takes a walk up
/// from the scalar to the first open collection that has more to write, so the cost of a scalar does not grow with the text.
let private writeKeepScalarsAtDepth (text: string) =
    let clip = blockScalar BlockScalarStyle.Literal ChompingMode.Clip None "x\n\n"
    let items = [ for i in 1 .. text.Length / 8 -> (if i = 1 then YamlNode.item else itemBefore [ blank ]) (mapOf [ "k", clip ]) ]
    let mutable node = YamlNode.sequence items
    for _ in 1 .. 50 do
        node <- YamlNode.sequence [ YamlNode.item node ]
    Writer.writeNode defaults node |> ignore

let private styleTests = testList "Ported from ScalarStyles, BlockScalars and YamlRead" [
    // Tests.ScalarStyles "Writer emits styled inline sequence scalars with quotes". yaml@2.6.1 reads the text as {"items":["a","b"]}. The old writer chose the inline form for
    // every sequence of scalars. The tree stores the style, so the test builds a flow sequence.
    testCase "a flow sequence keeps the quotes of its scalars" <| fun _ ->
        expectNode (mapOf [ "items", flowSeq [ sq "a"; dq "b" ] ]) "items: ['a', \"b\"]\n"

    // Tests.ScalarStyles "Writer preserves plain multiline sequence style on read-write-read" and "Writer preserves plain multiline mapping style on read-write-read".
    // yaml@2.6.1 reads the inputs as ["My Value 1 My Value 2","My Value 3"] and {"note":"line1 line2"}. The old tree kept the lines of the source and the writer wrote them
    // again. The new tree holds the folded text, so the text comes back on one line.
    testCase "a plain scalar that the parser folded is written on one line" <| fun _ ->
        expectWritten "- My Value 1\n  My Value 2\n- My Value 3\n" "- My Value 1 My Value 2\n- My Value 3\n"
        expectWritten "note: line1\n  line2\n" "note: line1 line2\n"

    // Tests.ScalarStyles "Explicit indentation indicator round-trip preserves semantic value" and "Explicit indentation indicator round-trip keeps a sequence item scalar".
    // yaml@2.6.1 reads the texts as {"doc":"explicit\n"} and [" explicit\n"].
    testCase "a stored indentation indicator comes back in the same text" <| fun _ ->
        expectStable "doc: |2\n  explicit\n"
        expectStable "- |1\n  explicit\n"

    // Tests.ScalarStyles "Writer adds an indentation indicator when the first line of a mapping value starts with a space", "Writer adds an indentation indicator to the default strip
    // chomping", "Writer counts the indentation indicator from the header of a nested mapping value", "Writer adds an indentation indicator when the first line of a sequence item
    // starts with a space", "Writer adds an indentation indicator when the first line of a root value starts with a space" and "Writer adds no indentation indicator when the first
    // nonblank line starts without a space". yaml@2.6.1 reads each expected text back as the original value. The old tests built values without a style and the writer chose the
    // block form. The tree stores the style now, so the tests build literal scalars.
    testCase "a literal scalar with a space at the start of its first line gets an indentation indicator in every position" <| fun _ ->
        let value = "  leading spaces first line\nnormal second line\n"
        expectNode (mapOf [ "k", lit value ]) "k: |2\n    leading spaces first line\n  normal second line\n"
        expectNode
            (mapOf [ "k", blockScalar BlockScalarStyle.Literal ChompingMode.Strip None "  leading spaces first line\nnormal second line" ])
            "k: |2-\n    leading spaces first line\n  normal second line\n"
        expectNode (mapOf [ "a", mapOf [ "k", lit value ] ]) "a:\n  k: |2\n      leading spaces first line\n    normal second line\n"
        expectNode (seqOf [ lit value ]) "- |2\n    leading spaces first line\n  normal second line\n"
        expectNode (lit value) "|2\n    leading spaces first line\n  normal second line\n"
        expectNode (mapOf [ "k", lit "first\n  second\n" ]) "k: |\n  first\n    second\n"

    // Tests.ScalarStyles "Expression block scalar keeps quote delimiters and stays block style on write". yaml@2.6.1 reads the folded text with its quotes and its more indented
    // lines as one string.
    testCase "a folded expression keeps its quotes and its block style" <| fun _ ->
        expectStable "expression: >\n  ${ return (function() {\n    function trimItem(datum) {\n      var permittedKeys = ['class', 'basename'];\n      if (datum.basename === \"own\" || datum.basename === \".tmp\") return null;\n      if (datum.class === \"Directory\") return null;\n      return entry;\n    }\n    return trimItem(inputs.source);\n  })(); }\n"

    // Tests.ScalarStyles "Writer preserves key tag and anchor metadata in mapping emission". yaml@2.6.1 reads the text as {"k":"v"}. The tag comes first, as in the layout that the
    // tree stores.
    testCase "the tag and the anchor of a key stand in front of the key" <| fun _ ->
        let key = YamlNode.withAnchor "a1" (YamlNode.withTag (YamlTag.Named { Prefix = ""; Suffix = "tag:foo" }) (sc "k"))
        let node = YamlNode.mapping [ YamlNode.entry key (sc "v") ]
        expectNode node "!<tag:foo> &a1 k: v\n"
        Expect.nodeEqual (parseRoot "!<tag:foo> &a1 k: v\n") node "The text reads as the same tree"

    // Tests.ScalarStyles "Writer preserves mapping-key comments on non-inline mappings" and "Non-specific tag round-trip remains '!'". yaml@2.6.1 reads the texts as {"a":{"b":"c"}}
    // and ["12"].
    testCase "a comment behind a key and the non-specific tag come back in the same text" <| fun _ ->
        expectStable "a: # key\n  b: c\n"
        expectStable "- ! 12\n"

    // Tests.ScalarStyles "isPlainSafe classifies structurally unsafe strings" and "Style-less safe scalar stays plain and round-trips". The function is gone, because the writer
    // decides the spelling. yaml@2.6.1 reads each text as the string that the node holds. The old lists had the empty string and a string with a line feed among the unsafe strings.
    // A plain scalar without text is a missing value now, and a plain scalar with a line feed is written over several lines, so both leave the lists.
    testCase "a plain scalar that can stand plain stays plain in a mapping value" <| fun _ ->
        for text in [ "hello world"; "it's fine"; "-5"; "3:30"; "http://example.com/x"; ":x"; "a#b"; "tool.cwl#main" ] do
            expectNode (mapOf [ "k", sc text ]) (sprintf "k: %s\n" text)
            Expect.nodeEqual (parseRoot (sprintf "k: %s\n" text)) (mapOf [ "k", sc text ]) (sprintf "%s reads back as the same tree" text)

    // Tests.ScalarStyles "isPlainSafe classifies structurally unsafe strings", "Style-less unsafe scalar auto-selects double-quoted and round-trips" and "Style-less unsafe
    // scalars survive read-write-read unchanged". yaml@2.6.1 reads the written texts as the strings that the nodes hold. The tab inside "tab\there" is no problem for a plain scalar,
    // so the test only reads that text back.
    testCase "a plain scalar that cannot stand plain is written in double quotes and reads back" <| fun _ ->
        for text in [ "a: b"; "@id"; "*ref"; "- item"; "x # y"; "x\t# y"; " padded"; "trailing "; "-"; "'q"; "\"q" ] do
            let written = writeNode (mapOf [ "k", sc text ])
            Expect.isTrue (written.StartsWith "k: \"") (sprintf "%A is written in double quotes: %s" text written)
            Expect.nodeEqual (parseRoot written) (mapOf [ "k", dq text ]) (sprintf "%A reads back" text)
            Expect.equal (writeNode (parseRoot written)) written (sprintf "The text of %A is stable" text)
        let tab = writeNode (mapOf [ "k", sc "tab\there" ])
        Expect.equal (Decode.string (YamlNode.entries (parseRoot tab)).Head.Value) "tab\there" "The tab reads back"

    // Tests.ScalarStyles "isPlainSafe applies the flow indicator rules only in Flow context", "Writer double-quotes a flow sequence item that contains commas and quotes", "Writer
    // double-quotes flow sequence items that contain flow indicators", "Writer keeps the block rules for flow indicators in a mapping value" and "Writer keeps flow-safe flow
    // sequence items plain". yaml@2.6.1 reads k: [f(a, b)] as the two items "f(a" and "b)", and it reads each expected text as the single value of the node.
    testCase "a flow indicator in a plain scalar is text in a block mapping and a reason for double quotes in a flow sequence" <| fun _ ->
        for text in [ "f(a, b)"; "function f(a){ return a; }"; "a[0]"; "x{y}"; "a:]"; "a:," ] do
            expectNode (mapOf [ "k", sc text ]) (sprintf "k: %s\n" text)
            expectNode (mapOf [ "k", flowSeq [ sc text ] ]) (sprintf "k: [\"%s\"]\n" text)
        expectNode (mapOf [ "k", sc "x: {y}" ]) "k: \"x: {y}\"\n"
        expectNode (mapOf [ "k", flowSeq [ sc "x: {y}" ] ]) "k: [\"x: {y}\"]\n"
        expectNode (mapOf [ "k", flowSeq [ sc "x.replace(/#/g, \"_\"), y" ] ]) "k: [\"x.replace(/#/g, \\\"_\\\"), y\"]\n"
        let safe = [ "abc"; "hello world"; "C#"; "tool.cwl#main"; "a:b"; "3:30"; ":x" ]
        expectNode (mapOf [ "k", flowSeq [ for text in safe -> sc text ] ]) "k: [abc, hello world, C#, tool.cwl#main, a:b, 3:30, :x]\n"

    // Tests.ScalarStyles "Writer puts the comment of a multiline plain mapping value behind its last line" and "Writer puts the comment of a multiline plain sequence item behind its
    // last line". yaml@2.6.1 reads the texts as {"k":"$(f(a, \"x\"))"...} with the line feed of the empty line, and it rejects a comment behind the first line. The tree holds the
    // line feed, and the writer writes an empty line for it.
    testCase "the comment of a plain scalar over several lines stands behind its last line" <| fun _ ->
        let withComment = YamlNode.withTrailingComment (comment " note") (sc "$(f(a,\n\"x\"))")
        let mapping = YamlNode.mapping [ entryOf "k" withComment; entryOf "j" (sc "x") ]
        expectNode mapping "k: $(f(a,\n\n  \"x\")) # note\nj: x\n"
        Expect.nodeEqual (parseRoot "k: $(f(a,\n\n  \"x\")) # note\nj: x\n") mapping "The mapping reads back"
        let sequence = seqOf [ withComment; sc "x" ]
        expectNode sequence "- $(f(a,\n\n  \"x\")) # note\n- x\n"
        Expect.nodeEqual (parseRoot "- $(f(a,\n\n  \"x\")) # note\n- x\n") sequence "The sequence reads back"

    // Tests.BlockScalars "A line of spaces survives writing and reading" and "Writer adds an indentation indicator when the first line is a line of spaces". yaml@2.6.1 reads
    // the written texts back as "a\n \nb\n", "a\n\n  \nb" and " \na\n". A line of spaces keeps its spaces behind the indentation.
    testCase "a line of spaces in a block scalar keeps its spaces" <| fun _ ->
        expectNode (mapOf [ "k", lit "a\n \nb\n" ]) "k: |\n  a\n   \n  b\n"
        // The old writer put the indentation on the empty line. The new writer leaves the empty line empty, which [70] l-empty allows (s-indent(<n) and a line feed), and the line of two spaces
        // keeps its spaces behind the indentation.
        let two = mapOf [ "k", blockScalar BlockScalarStyle.Literal ChompingMode.Strip None "a\n\n  \nb" ]
        expectNode two "k: |-\n  a\n\n    \n  b\n"
        Expect.nodeEqual (parseRoot "k: |-\n  a\n\n    \n  b\n") two "The text reads back as the same value"
        expectNode (mapOf [ "k", lit " \na\n" ]) "k: |2\n   \n  a\n"
        Expect.nodeEqual
            (parseRoot "k: |2\n   \n  a\n")
            (mapOf [ "k", blockScalar BlockScalarStyle.Literal ChompingMode.Clip (Some 2) " \na\n" ])
            "The indicator is part of the tree"

    // Tests.ScalarStyles "Writer chomps a style-less multiline value by its trailing line breaks" and "Writer chomps a folded fallback by the trailing line breaks too". yaml@2.6.1 reads each written text
    // back as the value, with or without the key or the item that follows it. The values are "a", "a\n", "a\n\n", "a\n\n\n" and "\n". The old writer chose the block form for a value without a style.
    // The tree stores the style now, so the test builds a literal and a folded scalar with the chomping Clip, and the writer sets the chomping from the trailing line feeds. A kept blank line stays in
    // the text and is empty.
    testCase "the chomping of a literal and a folded scalar follows its trailing line feeds in a mapping value and in a sequence item" <| fun _ ->
        let cases = [ "a", "-", "  a\n"; "a\n", "", "  a\n"; "a\n\n", "+", "  a\n\n"; "a\n\n\n", "+", "  a\n\n\n"; "\n", "+", "\n" ]
        for (style, indicator) in [ (BlockScalarStyle.Literal, "|"); (BlockScalarStyle.Folded, ">") ] do
            for (value, chomp, body) in cases do
                let node = blockScalar style ChompingMode.Clip None value
                let header = indicator + chomp
                // The value "a" has no final line feed. As the last node of the text it keeps the chomping Clip and the text ends without a line break ([165]).
                if value = "a" then
                    expectNode (mapOf [ "k", node ]) ("k: " + indicator + "\n  a")
                    expectNode (seqOf [ node ]) ("- " + indicator + "\n  a")
                else
                    expectNode (mapOf [ "k", node ]) ("k: " + header + "\n" + body)
                    expectNode (seqOf [ node ]) ("- " + header + "\n" + body)
                expectNode (mapOf [ "k", node; "j", sc "x" ]) ("k: " + header + "\n" + body + "j: x\n")
                expectNode (seqOf [ node; sc "b" ]) ("- " + header + "\n" + body + "- b\n")

    // Tests.ScalarStyles "Root block scalar preserves tag/anchor/comment on write". yaml@2.6.1 reads the text as "hi\n". The tag, the anchor and the header comment of a root block scalar stand
    // in front of its header, and the text comes back unchanged.
    testCase "a root block scalar keeps its tag, its anchor and its header comment" <| fun _ ->
        expectStable "!<tag:foo> &a1 | # c\n  hi\n"

    // Tests.Escapes "A double-quoted scalar with a line break round-trips through the writer". yaml@2.6.1 reads the text as {"one \n\n  two \t":"v"}. The writer escapes the line breaks
    // and the tab, so the output never relies on folding.
    testCase "a double-quoted key with line breaks and a tab is written with escapes for the line breaks only" <| fun _ ->
        let node = YamlNode.mapping [ YamlNode.entry (dq "one \n\n  two \t") (sc "v") ]
        expectNode node "\"one \\n\\n  two \t\": v\n"
        Expect.nodeEqual (parseRoot "\"one \\n\\n  two \\t\": v\n") node "The escaped text reads as the same tree"
        Expect.nodeEqual (parseRoot "\"one \\n\\n  two \t\": v\n") node "The text with the raw tab reads as the same tree"

    // Tests.AdvancedFeatures "7.7 A writer round trip keeps the tag and the anchor of the scalars". yaml@2.6.1 reads the texts as {"k":"a b"}, {"k":"a"}, ["a b"], {"k":"x\n"} and ["y"].
    testCase "the tag and the anchor of a scalar with its value on a later line survive writing" <| fun _ ->
        for text in [ "k: !!str\n  a\n  b"; "k: &x\n  a"; "- !!str\n  a\n  b"; "k: !t\n  |\n    x"; "- &a\n  >-\n    y" ] do
            let stream = parseOk text
            let written = Writer.writeDocuments defaults stream
            Expect.streamEqual (parseOk written) stream (sprintf "The tree of %A after writing" text)
            Expect.equal (Writer.writeDocuments defaults (parseOk written)) written (sprintf "The text of %A is stable" text)

    // Earlier test "a placeholder-shaped value reads like a plain scalar and survives the writer", Tests.YamlRead "KeyValue empty inline sequence roundtrips as brackets", "An empty
    // sequence item writes as a bare dash", "A key without a value reads as None and writes as the key" and "Nested sequences of mappings and dollar keys survive write and read",
    // Tests.YamlGenericRead "Preprocessing read/write keeps stable structural indentation". yaml@2.6.1 reads the texts as {"a":"<c f=0/>"}, {"value":[]}, ["a",null,{}], {"k":[null,"x"]}
    // and {"k":null} for the keys without a value. The old tests built the elements by hand or compared a text with a part of it, so the tests here compare the whole text.
    testCase "the texts of the old round trip tests come back as the same text" <| fun _ ->
        for text in [ "a: <c f=0/>\n"; "a: <s f=1/>\n"; "value: []\n"; "- a\n-\n- {}\n"; "k:\n  -\n  - x\n"; "k:\n"; "k:\nj: 1\n"; "a:\n  k:\nb: 1\n"; "k: # c\n"; "- k:\n- b\n"; "x: &a 1\nk: *a # c\n"; "- &a 1\n- *a # c\n" ] do
            expectStable text
        expectWritten "a:\n      b:\n        c: d" "a:\n  b:\n    c: d\n"
        // Tests.ReaderDocuments "A document without a node round trips through the writer". The old Nil is a missing root. A text without a node writes nothing, and the comments stay.
        expectWritten "" ""
        expectWritten "# c" "# c\n"
        expectWritten "# c1\n# c2" "# c1\n# c2\n"
        for text in [ "fileCollectionSet:\n- - class: File\n    path: a_1\n  - class: File\n    path: a_2\n- - class: File\n    path: b_1\n"; "$namespaces:\n  s: https://schema.org/\n$schemas:\n- https://x\n" ] do
            let stream = parseOk text
            Expect.streamEqual (parseOk (Writer.writeDocuments defaults stream)) stream (sprintf "The tree of %A after writing" text)

    // Tests.YamlRead "CWL ExpressionTool full roundtrip preserves expression and empty object". yaml@2.6.1 reads the text as the tool with its expression in a folded scalar and
    // `InlineJavascriptRequirement: {}`. The empty flow mapping stays a flow mapping, the expression keeps its helper functions and the second write gives the first text again.
    testCase "a CWL ExpressionTool keeps its expression and its empty flow mapping through the writer" <| fun _ ->
        let text =
            "#!/usr/bin/env cwl-runner\n\ncwlVersion: v1.2\nclass: ExpressionTool\nid: \"T_join_out\"\nlabel: Gives back the joined folder called from \"analysis\", holding all given files and the subfolders.\nrequirements:\n  InlineJavascriptRequirement: {}\ninputs:\n  base_path:\n    type: Directory\n  item_single:\n    type: File?\n  item_array:\n    type: File[]?\n  container_single:\n    type: Directory?\n  container_array:\n    type: Directory[]?\n  relabel:\n    type: string?\noutputs:\n  joined_D:\n    type: Directory\n    doc: \"Merged results output folder\"\nexpression: >\n  ${ return (function() {\n    function trimItem(datum) {\n      var permittedKeys = ['class', 'basename', 'checksum', 'listing'];\n      var shortened = {};\n      for (var i = 0; i < permittedKeys.length; i++) {\n        var key = permittedKeys[i];\n        if (datum[key] !== undefined) shortened[key] = datum[key];\n      }\n      return shortened.class && shortened.basename ? shortened : null;\n      return name.replace(/\\.webp$/, \"\").replace(/\\.gif$/, \"\");\n    }\n\n    var collection = [];\n    if (inputs.container_single) collection.push(trimItem(inputs.container_single));\n    if (inputs.item_single) collection.push(trimItem(inputs.item_single));\n\n    return {\n      joined_D: { class: \"Directory\", basename: inputs.relabel || \"analysis\", listing: collection }\n    };\n  })(); }"
        let stream = parseOk text
        let written = Writer.writeDocuments defaults stream
        let reparsed = parseOk written
        Expect.streamEqual reparsed stream "The tree after the round trip"
        Expect.isTrue (written.Contains "{}") "The empty flow mapping is written"
        Expect.isTrue (written.Contains "trimItem(datum)") "The trimItem helper survives"
        Expect.isTrue (written.Contains "permittedKeys") "The array of allowed fields survives"
        Expect.equal (Writer.writeDocuments defaults reparsed) written "The second write gives the first text"
]

/// A place where a blank line can follow a scalar directly.
type KeepPlace =
    { Name: string
      /// The tree around the scalar.
      Build: YamlNode -> YamlStream
      /// The text when the scalar is the value "x" with two line feeds and gets Keep from its value: the blank line behind it
      /// makes the writer double-quote the scalar.
      Quoted: string
      /// The text when the scalar is a stored Keep with the value "x" and one line feed. It is None where more content follows
      /// the blank line, which makes the tree invalid. Where only trivia follow, the blank line is written as a line that ends
      /// the body: a byte order mark line behind a scalar in a collection and a tab line behind a root scalar.
      Kept: string option }

/// The places where a blank line can follow a scalar directly.
let keepPlaces: KeepPlace list =
    let one (root: YamlNode) : YamlStream = { Parts = [ StreamPart.Document(document root) ] }
    let blankBefore (entry: MappingEntry) : MappingEntry = { entry with Before = [ blank ] }
    let secondDocument (prelude: PreludeItem list) : StreamPart =
        StreamPart.Document { document (sc "y") with Prelude = prelude; StartMarker = Some(marker None) }
    [ { Name = "the Before of the next entry"
        Build = fun s -> one (YamlNode.mapping [ entryOf "a" s; blankBefore (entryOf "b" (sc "1")) ])
        Quoted = "a: \"x\\n\\n\"\n\nb: 1\n"
        Kept = None }
      { Name = "the Before of the next item"
        Build = fun s -> one (YamlNode.sequence [ YamlNode.item s; itemBefore [ blank ] (sc "y") ])
        Quoted = "- \"x\\n\\n\"\n\n- y\n"
        Kept = None }
      { Name = "the End of a mapping"
        Build = fun s -> one (withEnd [ blank ] (mapOf [ "a", s ]))
        Quoted = "a: \"x\\n\\n\"\n\n"
        Kept = Some("a: |+\n  x\n" + bom + "\n") }
      { Name = "the End of a sequence"
        Build = fun s -> one (withEnd [ blank ] (seqOf [ s ]))
        Quoted = "- \"x\\n\\n\"\n\n"
        Kept = Some("- |+\n  x\n" + bom + "\n") }
      { Name = "the Before of an entry of the outer mapping, three levels deep"
        Build = fun s -> one (YamlNode.mapping [ entryOf "a" (mapOf [ "b", mapOf [ "c", s ] ]); blankBefore (entryOf "d" (sc "1")) ])
        Quoted = "a:\n  b:\n    c: \"x\\n\\n\"\n\nd: 1\n"
        Kept = None }
      { Name = "the Before of an entry behind a sequence in a mapping"
        Build = fun s -> one (YamlNode.mapping [ entryOf "a" (seqOf [ s ]); blankBefore (entryOf "d" (sc "1")) ])
        Quoted = "a:\n  - \"x\\n\\n\"\n\nd: 1\n"
        Kept = None }
      { Name = "the End of the root, two levels deep"
        Build = fun s -> one (withEnd [ blank ] (mapOf [ "a", mapOf [ "b", s ] ]))
        Quoted = "a:\n  b: \"x\\n\\n\"\n\n"
        Kept = Some("a:\n  b: |+\n    x\n" + bom + "\n") }
      { Name = "the gap in front of the colon of an explicit key"
        Build = fun s -> one (YamlNode.mapping [ { explicitEntry s (sc "w") with BeforeColon = [ blank ] } ])
        Quoted = "? \"x\\n\\n\"\n\n: w\n"
        Kept = None }
      { Name = "the Before of the entry behind an explicit key without a colon"
        Build = fun s -> one (YamlNode.mapping [ explicitKeyOnly s; blankBefore (entryOf "d" (sc "1")) ])
        Quoted = "? \"x\\n\\n\"\n\nd: 1\n"
        Kept = None }
      { Name = "the End of the collection behind an explicit key without a colon"
        Build = fun s -> one (withEnd [ blank ] (YamlNode.mapping [ explicitKeyOnly s ]))
        Quoted = "? \"x\\n\\n\"\n\n"
        Kept = Some("? |+\n  x\n" + bom + "\n") }
      { Name = "AfterRoot"
        Build = fun s -> { Parts = [ StreamPart.Document { document s with AfterRoot = [ blank ] } ] }
        Quoted = "\"x\\n\\n\"\n\n"
        Kept = Some("|+\n  x\n\t\n") }
      { Name = "the prelude of the next document"
        Build = fun s -> { Parts = [ StreamPart.Document(document s); secondDocument [ PreludeItem.PreludeTrivia blank ] ] }
        Quoted = "\"x\\n\\n\"\n\n--- y\n"
        Kept = Some("|+\n  x\n\t\n--- y\n") }
      { Name = "the prelude of the next document behind a collection"
        Build = fun s -> { Parts = [ StreamPart.Document(document (mapOf [ "a", s ])); secondDocument [ PreludeItem.PreludeTrivia blank ] ] }
        Quoted = "a: \"x\\n\\n\"\n\n--- y\n"
        Kept = Some("a: |+\n  x\n" + bom + "\n--- y\n") } ]

let private keepBlankTests = testList "A blank line behind a Keep block scalar" [
    // yaml@2.6.1 reads the texts of each place with the block scalar `|+` as the value with the empty line, for example {"a":"x\n\n","b":1} for the first place, and
    // reads the double-quoted texts of the next test as the same value. The empty lines behind a Keep scalar are part of its value ([165] to [169]).
    testCase "a stored Keep scalar with a blank line in front of more content is an invalid tree" <| fun _ ->
        let keep = blockScalar BlockScalarStyle.Literal ChompingMode.Keep None "x\n"
        for place in keepPlaces do
            if place.Kept.IsNone then
                for options in [ defaults; withStyles RequireStored; withStyles Canonical ] do
                    let error = writeError (fun () -> Writer.writeDocuments options (place.Build keep))
                    Expect.stringContains error.Message "Keep" (sprintf "The chomping in %s" place.Name)
                    Expect.stringContains error.Message "[165]" (sprintf "The production in %s" place.Name)
                    Expect.stringContains error.Message ".Style" (sprintf "The slot in %s" place.Name)

    // The parser reads `|+`, `  x` and a line that holds a byte order mark as the value "x\n" and a blank line behind it. The same holds for a line with a tab behind a root scalar.
    // yaml@2.6.1 reads the byte order mark line in the middle of a document as the end of the document and the tab line as the end of the body. Only trivia and markers follow in these places.
    testCase "a stored Keep scalar with a blank line where only trivia follow gets a line that ends its body" <| fun _ ->
        let keep = blockScalar BlockScalarStyle.Literal ChompingMode.Keep None "x\n"
        for place in keepPlaces do
            match place.Kept with
            | Some expected ->
                for options in [ defaults; withStyles RequireStored ] do
                    let written = Writer.writeDocuments options (place.Build keep)
                    Expect.equal written expected (sprintf "The text in %s" place.Name)
                    Expect.streamEqual (parseOk written) (place.Build keep) (sprintf "The tree read in %s" place.Name)
                    Expect.equal (Writer.writeDocuments options (parseOk written)) written (sprintf "The second write in %s" place.Name)
            | None -> ()

    // yaml@2.6.1 reads each expected text as the value "x\n\n" in the place of the scalar. A stored Clip or Strip cannot express the value, so the writer
    // picks Keep, and a blank line behind a Keep scalar reads as part of its value. No other chomping gives the value, so the scalar is double-quoted.
    testCase "a scalar that gets the chomping Keep is double-quoted when a blank line directly follows" <| fun _ ->
        for place in keepPlaces do
            for stored in [ ChompingMode.Clip; ChompingMode.Strip ] do
                let stream = place.Build(blockScalar BlockScalarStyle.Literal stored None "x\n\n")
                let written = Writer.writeDocuments defaults stream
                Expect.equal written place.Quoted (sprintf "The text in %s with the stored chomping %A" place.Name stored)
                Expect.equal (Writer.writeDocuments defaults (parseOk written)) written (sprintf "The second write in %s" place.Name)
            Expect.equal (Writer.writeDocuments (withStyles Canonical) (place.Build(blockScalar BlockScalarStyle.Literal ChompingMode.Clip None "x\n\n"))) place.Quoted (sprintf "Canonical in %s" place.Name)
            let error = writeError (fun () -> Writer.writeDocuments (withStyles RequireStored) (place.Build(blockScalar BlockScalarStyle.Literal ChompingMode.Clip None "x\n\n")))
            Expect.stringContains error.Message "chomping" (sprintf "RequireStored in %s" place.Name)

    // yaml@2.6.1 reads the expected texts as {"a":"\n","b":1}, {"a":"x\n\n\n","b":1}, {"a":"x\n\n","b":1}, {"a":"x\n","b":1} and {"a":"x","b":1}. A value of line feeds only, more line feeds
    // and a folded scalar need the same rule, and a value that the stored chomping expresses needs none.
    testCase "the rule holds for a value of line feeds only, more line feeds and a folded scalar" <| fun _ ->
        let next = { entryOf "b" (sc "1") with Before = [ blank ] }
        let write (scalar: YamlNode) = Writer.writeNode defaults (YamlNode.mapping [ entryOf "a" scalar; next ])
        Expect.equal (write (blockScalar BlockScalarStyle.Literal ChompingMode.Clip None "\n")) "a: \"\\n\"\n\nb: 1\n" "Line feeds only"
        Expect.equal (write (blockScalar BlockScalarStyle.Literal ChompingMode.Clip None "x\n\n\n")) "a: \"x\\n\\n\\n\"\n\nb: 1\n" "Three line feeds"
        Expect.equal (write (blockScalar BlockScalarStyle.Folded ChompingMode.Clip None "x\n\n")) "a: \"x\\n\\n\"\n\nb: 1\n" "Folded"
        // A value that the stored chomping expresses has no Keep, so a blank line behind it is trivia.
        Expect.equal (write (blockScalar BlockScalarStyle.Literal ChompingMode.Clip None "x\n")) "a: |\n  x\n\nb: 1\n" "Clip with one line feed"
        Expect.equal (write (blockScalar BlockScalarStyle.Literal ChompingMode.Strip None "x")) "a: |-\n  x\n\nb: 1\n" "Strip"

    // yaml@2.6.1 reads `a: |+`, `  x`, `# c`, an empty line and `b: 1` as {"a":"x\n","b":1}. A comment line ends the empty lines of the scalar (l-trail-comments), so the blank line
    // behind the comment line is trivia and the scalar stays a block scalar. The Keep scalar keeps its empty lines, and a stored Clip with two line feeds is written as Keep.
    testCase "a comment line between the Keep scalar and the blank line keeps the block scalar" <| fun _ ->
        let keep = blockScalar BlockScalarStyle.Literal ChompingMode.Keep None "x\n"
        let behindComment = { entryOf "b" (sc "1") with Before = [ note " c"; blank ] }
        Expect.equal (Writer.writeNode defaults (YamlNode.mapping [ entryOf "a" keep; behindComment ])) "a: |+\n  x\n# c\n\nb: 1\n" "Before of the next entry"
        let clipped = blockScalar BlockScalarStyle.Literal ChompingMode.Clip None "x\n\n"
        Expect.equal (Writer.writeNode defaults (YamlNode.mapping [ entryOf "a" clipped; behindComment ])) "a: |+\n  x\n\n# c\n\nb: 1\n" "Before of the next entry, two line feeds"
        Expect.equal (Writer.writeNode defaults (withEnd [ note " c"; blank ] (mapOf [ "a", keep ]))) "a: |+\n  x\n# c\n\n" "End"
        Expect.equal (Writer.writeNode defaults (seqOf [ keep ] |> withEnd [ note " c"; blank ])) "- |+\n  x\n# c\n\n" "End of a sequence"
        let secondDocument = StreamPart.Document { document (sc "y") with Prelude = [ PreludeItem.PreludeTrivia(note " c"); PreludeItem.PreludeTrivia blank ]; StartMarker = Some(marker None) }
        Expect.equal (writeStream [ StreamPart.Document(document keep); secondDocument ]) "|+\n  x\n# c\n\n--- y\n" "Prelude of the next document"
        Expect.equal (Writer.write defaults { document keep with AfterRoot = [ note " c"; blank ] }) "|+\n  x\n# c\n\n" "AfterRoot"

    // yaml@2.6.1 reads `|+`, `  x`, an empty line, `...`, an empty line, `%YAML 1.2` and `--- y` as ["x\n\n","y"]. The writer puts `...` in front of a prelude with a directive, so no blank line
    // follows the scalar directly. A `...` behind the scalar and a following entry without a blank line need no change either.
    testCase "a prelude with a directive, an end marker and no trivia leave the block scalar alone" <| fun _ ->
        let clipped = blockScalar BlockScalarStyle.Literal ChompingMode.Clip None "x\n\n"
        let directive = PreludeItem.Directive { Kind = DirectiveKind.YamlVersion(1, 2); TrailingComment = None; Span = None }
        let secondDocument = StreamPart.Document { document (sc "y") with Prelude = [ PreludeItem.PreludeTrivia blank; directive ] }
        Expect.equal (writeStream [ StreamPart.Document(document clipped); secondDocument ]) "|+\n  x\n\n...\n\n%YAML 1.2\n--- y\n" "A directive in the prelude"
        Expect.equal (Writer.write defaults { document clipped with EndMarker = Some(marker None) }) "|+\n  x\n\n...\n" "An end marker"
        Expect.equal (Writer.writeNode defaults (YamlNode.mapping [ entryOf "a" clipped; entryOf "b" (sc "1") ])) "a: |+\n  x\n\nb: 1\n" "No trivia"
]

/// Writes the text with the policy Canonical, which has to give the expected text, and writes the tree of that text again, which has to give the same text.
let private canonicalTwice (input: string) (expected: string) =
    let write (text: string) = Writer.writeDocuments (withStyles Canonical) (parseOk text)
    let once = write input
    Expect.equal once expected (sprintf "The Canonical text of %A" input)
    Expect.equal (write once) once (sprintf "The second Canonical write of %A" input)

let private finalReviewTests = testList "Characters, root markers, key length and trivia of the final review" [
    // yaml@2.6.1 reads the expected texts of this test as the value with the character in it, as a plain scalar, a single-quoted, a double-quoted and a block scalar, as a key,
    // as an item of a flow sequence and as a character of a comment line. YAML 1.2 ends a line at a line feed or a carriage return only (b-char, [27]), so NEL, NBSP and the two
    // Unicode line separators are nb-char characters and stand raw wherever a plain scalar, a single-quoted scalar, a block scalar or a comment may hold them. Double quotes keep the
    // named escapes, because the tree keeps no spelling of an escape and readers of YAML 1.1 keep the escapes where they read a raw NEL or separator as a line break.
    testCase "NEL, NBSP and the line and paragraph separators stand raw except in double quotes" <| fun _ ->
        for (c, escape) in [ nel, "\\N"; nbsp, "\\_"; lineSeparator, "\\L"; paragraphSeparator, "\\P" ] do
            let word = "b" + c + "c"
            expectNode (mapOf [ "a", sc word ]) ("a: " + word + "\n")
            expectNode (mapOf [ "a", sq word ]) ("a: '" + word + "'\n")
            expectNode (mapOf [ "a", dq word ]) ("a: \"b" + escape + "c\"\n")
            expectNode (mapOf [ "a", lit (word + "\n") ]) ("a: |\n  " + word + "\n")
            expectNode (mapOf [ ("b" + c), sc "v" ]) ("b" + c + ": v\n")
            expectNode (mapOf [ "a", withNote (" x" + c + "y") (sc "v") ]) ("a: v # x" + c + "y\n")
            expectNode (flowSeq [ sc word ]) ("[" + word + "]\n")
            expectStable ("a: " + word + "\n")
            expectStable ("a: '" + word + "'\n")
            expectStable ("a: |\n  " + word + "\n")
            expectStable ("a: \"b" + escape + "c\"\n")
            Expect.equal (Writer.writeNode (withStyles RequireStored) (mapOf [ "a", sq word ])) ("a: '" + word + "'\n") "RequireStored keeps single quotes"
            Expect.equal (Writer.writeNode (withStyles Canonical) (mapOf [ "a", sq word ])) ("a: " + word + "\n") "Canonical writes plain"

    // yaml@2.6.1 reads `'b` DEL `c'`, `'b` C1 `c'` and the other single-quoted texts as the values with the characters. Single quotes hold every nb-json character ([2], [118])
    // and have no escape, so they keep DEL, the C1 controls and U+FFFE raw. The other styles have no place for them and escape them.
    testCase "single quotes hold DEL, the C1 controls and U+FFFE raw, and the other styles escape them" <| fun _ ->
        for (c, escape) in [ del, "\\x7F"; c1Control, "\\x81"; string (char 0x9F), "\\x9F"; string (char 0xFFFE), "\\uFFFE" ] do
            let word = "b" + c + "c"
            expectNode (mapOf [ "a", sq word ]) ("a: 'b" + c + "c'\n")
            expectNode (mapOf [ "a", sc word ]) ("a: \"b" + escape + "c\"\n")
            expectNode (mapOf [ "a", dq word ]) ("a: \"b" + escape + "c\"\n")
            expectNode (mapOf [ "a", lit (word + "\n") ]) ("a: \"b" + escape + "c\\n\"\n")
            Expect.equal (Writer.writeNode (withStyles RequireStored) (mapOf [ "a", sq word ])) ("a: 'b" + c + "c'\n") "RequireStored keeps single quotes"
            Expect.stringContains (writeError (fun () -> Writer.writeNode (withStyles RequireStored) (mapOf [ "a", sc word ]))).Message "plain" "RequireStored refuses plain"
            expectStable ("a: 'b" + c + "c'\n")

    // The fuzzer found a flow sequence with a single-quoted scalar over several lines and a NEL that came back on one line. yaml@2.6.1 reads both texts as ["\n\u0085"].
    testCase "a single-quoted scalar over several lines with NEL in a flow sequence is written the same way twice" <| fun _ ->
        expectWritten ("['\n\n" + nel + "']\n") ("[\n  '\n\n  " + nel + "'\n]\n")

    // yaml@2.6.1 reads the expected texts as {"a":"b\tc"} and the like. A raw tab is an nb-json character, so a double-quoted scalar writes it raw.
    testCase "a double-quoted scalar writes the tab raw and escapes the line breaks and the C0 controls" <| fun _ ->
        expectNode (mapOf [ "a", dq "b\tc" ]) "a: \"b\tc\"\n"
        expectNode (mapOf [ "a", dq "b\r\nc\001" ]) "a: \"b\\r\\nc\\x01\"\n"

    // yaml@2.6.1 reads the expected texts as the trees of the inputs. Canonical used to write the comment of the last property and drop the trivia lines between that property and the
    // flow collection that it writes as a block collection. The lines go to the Before of the first entry or item, in front of the opening comment.
    testCase "Canonical keeps the trivia lines between the last property and a flow collection" <| fun _ ->
        canonicalTwice "- !a\n  # lost?\n  {a: b}\n" "- !a\n  # lost?\n  a: b\n"
        canonicalTwice "k: &x\n  # lost too?\n  [1, 2]\n" "k: &x\n  # lost too?\n  - 1\n  - 2\n"
        canonicalTwice "- !a\n\n  {!!str &a2 a: b}\n" "- !a\n\n  !!str &a2 a: b\n"
        canonicalTwice "- !a # p\n  # f\n\n  [ # o\n  x]\n" "- !a # p\n  # f\n\n  # o\n  - x\n"
        canonicalTwice "k: !a\n  # f\n  [x] # c\nj: 1\n" "k: !a\n  # f\n  - x\n# c\nj: 1\n"
        // The comment lines and blank lines of a flow collection are not lost with PreferStored and RequireStored either.
        expectStable "- !a\n  # f\n\n  {a: b}\n"
        Expect.equal (Writer.writeDocuments (withStyles RequireStored) (parseOk "- !a\n  # f\n\n  {a: b}\n")) "- !a\n  # f\n\n  {a: b}\n" "RequireStored"

    // yaml@2.6.1 reads both expected texts as {"a":1,"--- b":2}. The comment line in front of a root block mapping is the Before of its first entry (R2) and stands in the column of the
    // entry. The first Canonical write put it in the first column in front of the root that stands one column in, and the second write moved it.
    testCase "Canonical writes the lines in front of a root that stands one column in at its column" <| fun _ ->
        canonicalTwice "# c1\n{a: 1, --- b: 2}\n" " # c1\n a: 1\n --- b: 2\n"
        canonicalTwice "# c1\n\n[--- b, c]\n" "# c1\n\n- --- b\n- c\n"
        expectWritten "# c1\na: 1\n" "# c1\na: 1\n"

    // yaml@2.6.1 reads the expected texts of this test as the mappings of the inputs. A root mapping stands one column in when a key that starts a line would read as a document marker. A key that
    // the writer writes with a question mark does not start the line, because the question mark does. The reasons for the question mark are a comment behind the key, a key over the limit of
    // 1024 characters and a key in an explicit entry. The first Canonical write of the flow inputs put the root one column in for a key that then got a question mark, and the second write did not.
    testCase "a root mapping stands one column in only for a key that the writer writes without a question mark" <| fun _ ->
        let inputs =
            [ "{ ---\n? \"x\", # c5\n: [] }\n", "? --- ? \"x\" # c5\n?\n: []\n"
              "--- { ---\n? \"#\P\", # c5\n: [  ] }\n", "---\n? --- ? \"#\P\" # c5\n?\n: []\n"
              "{ a,\n? \"x\", # c5\n: [] }\n", "a:\n? x # c5\n?\n: []\n"
              "{ ? \"x\", # c5\n: b }\n", "? x # c5\n?\n: b\n" ]
        for (input, expected) in inputs do
            canonicalTwice input expected
        for policy in [ PreferStored; RequireStored; Canonical ] do
            for (input, _) in inputs do
                let write (stream: YamlStream) = Writer.writeDocuments (withStyles policy) stream
                let once = write (parseOk input)
                Expect.equal (write (parseOk once)) once (sprintf "The second write of %A under %A" input policy)
        // Trees that a program built. The key has no question mark, so RequireStored raises where the writer needs one.
        let commented = withNote " c" (sc "--- a")
        let longKey = sc ("--- " + String.replicate 1100 "a")
        let trees =
            [ "a comment behind the key", YamlNode.mapping [ YamlNode.entry commented (sc "v") ], "? --- a # c\n: v\n"
              "a key over the limit", YamlNode.mapping [ YamlNode.entry longKey (sc "v") ], "? " + ("--- " + String.replicate 1100 "a") + "\n: v\n"
              "a comment and a key that starts the line", YamlNode.mapping [ YamlNode.entry commented (sc "v"); YamlNode.entry (sc "--- b") (sc "w") ], " ? --- a # c\n : v\n --- b: w\n"
              "a key over the limit and a key that starts the line", YamlNode.mapping [ YamlNode.entry longKey (sc "v"); YamlNode.entry (sc "... b") (sc "w") ], " ? " + ("--- " + String.replicate 1100 "a") + "\n : v\n ... b: w\n"
              "a key that starts the line", YamlNode.mapping [ YamlNode.entry (sc "--- a") (sc "v") ], " --- a: v\n" ]
        for (label, tree, expected) in trees do
            for policy in [ PreferStored; Canonical ] do
                let once = Writer.writeNode (withStyles policy) tree
                Expect.equal once expected (sprintf "%s under %A" label policy)
                Expect.equal (Writer.writeNode (withStyles policy) (parseRoot once)) once (sprintf "The second write for %s under %A" label policy)
            if expected.StartsWith " ---" then
                Expect.equal (Writer.writeNode (withStyles RequireStored) tree) expected (sprintf "RequireStored for %s" label)
            else
                writeError (fun () -> Writer.writeNode (withStyles RequireStored) tree) |> ignore

    // The first line behind the root decides whether an empty root block scalar needs the indicator, and the line may stand in the prelude of the next document. The parser produces such a
    // tree from a text with a byte order mark line, which yaml@2.6.1 does not read, so the expected texts have no reference.
    testCase "an empty root block scalar gets the indicator when the comment line is in the prelude of the next document" <| fun _ ->
        let prelude (items: PreludeItem list) : StreamPart = StreamPart.Document { document nothing with Prelude = items; StartMarker = Some(marker None) }
        let rootStream (scalar: YamlNode) (following: StreamPart) : YamlStream =
            { Parts = [ StreamPart.Document { document scalar with StartMarker = Some(marker None) }; following ] }
        let strip = blockScalar BlockScalarStyle.Literal ChompingMode.Strip None ""
        let stream = rootStream strip (prelude [ PreludeItem.PreludeTrivia blank; PreludeItem.PreludeTrivia blank; PreludeItem.PreludeTrivia(note " k") ])
        Expect.equal (Writer.writeDocuments defaults stream) "--- |1-\n\n\n# k\n---\n" "A comment behind two blank lines"
        Expect.stringContains (writeError (fun () -> Writer.writeDocuments (withStyles RequireStored) stream)).Message "indentation indicator" "RequireStored"
        let stored = blockScalar BlockScalarStyle.Literal ChompingMode.Strip (Some 1) ""
        Expect.equal (Writer.writeDocuments (withStyles RequireStored) (rootStream stored (prelude [ PreludeItem.PreludeTrivia(note " k") ]))) "--- |1-\n# k\n---\n" "A stored indicator"
        // Blank lines alone need nothing, and a directive puts a `...` in front of the prelude.
        Expect.equal (Writer.writeDocuments defaults (rootStream strip (prelude [ PreludeItem.PreludeTrivia blank ]))) "--- |-\n\n---\n" "Blank lines only"
        let withDirective = prelude [ PreludeItem.PreludeTrivia(note " k"); directive (DirectiveKind.YamlVersion(1, 2)) ]
        Expect.equal (Writer.writeDocuments defaults (rootStream strip withDirective)) "--- |-\n...\n# k\n%YAML 1.2\n---\n" "A directive in the prelude"
        // AfterRoot comes first, and a blank line in front of the comment does not matter.
        Expect.equal (Writer.write defaults { document strip with AfterRoot = [ blank; note " c" ] }) "|1-\n\n# c\n" "AfterRoot"
        // A text from the fuzzer.
        let input = "|\n" + bom + "#\n---\n"
        let written = Writer.writeDocuments defaults (parseOk input)
        Expect.equal written "|1\n#\n---\n" "The text of the fuzzer"
        Expect.equal (Writer.writeDocuments defaults (parseOk written)) written "The second write"

    // A stored Keep scalar without content has a blank line behind it as a tab line or a byte order mark line, and the tab line would be content without an indicator.
    testCase "an empty root Keep scalar with a blank line behind it gets the indicator" <| fun _ ->
        let keep (value: string) = blockScalar BlockScalarStyle.Literal ChompingMode.Keep None value
        Expect.equal (Writer.write defaults { document (keep "") with AfterRoot = [ blank ] }) "|1+\n\t\n" "No value"
        Expect.equal (Writer.write defaults { document (keep "\n") with AfterRoot = [ blank; blank ] }) "|1+\n\n\t\n\n" "A line feed"
        Expect.stringContains (writeError (fun () -> Writer.write (withStyles RequireStored) { document (keep "") with AfterRoot = [ blank ] })).Message "tab line" "RequireStored"
        let stored = blockScalar BlockScalarStyle.Literal ChompingMode.Keep (Some 1) ""
        Expect.equal (Writer.write (withStyles RequireStored) { document stored with AfterRoot = [ blank ] }) "|1+\n\t\n" "A stored indicator"
        // A scalar with content needs none.
        Expect.equal (Writer.write defaults { document (keep "a\n") with AfterRoot = [ blank ] }) "|+\n  a\n\t\n" "Content"

    // The parser reads a line that holds a tab behind a root scalar, and a line that holds a byte order mark behind any scalar, as a blank line that ends the body of a Keep scalar. yaml@2.6.1
    // rejects these texts, so they have no reference. The writer writes the lines back, and the second write gives the same text.
    testCase "the lines that end the body of a Keep scalar are written back" <| fun _ ->
        let cases =
            [ "|+\n  a\n\t\n", "|+\n  a\n\t\n"
              "|1+\n" + bom + " \n\n", "|1+\n\t\n\n"
              "|+\n  a\n" + bom + "\n", "|+\n  a\n\t\n"
              "|+\n  a\n\t\n\n# c\n", "|+\n  a\n\t\n\n# c\n"
              "|+\n  a\n\t\n---\ny\n", "|+\n  a\n\t\n--- y\n"
              "a: |+\n  x\n" + bom + "\n", "a: |+\n  x\n" + bom + "\n"
              "a:\n  b: |+\n    x\n" + bom + "\n# c\n", "a:\n  b: |+\n    x\n" + bom + "\n# c\n"
              "- |+\n  x\n\n" + bom + "\n", "- |+\n  x\n\n" + bom + "\n"
              "a: |+\n  x\n" + bom + "\n---\ny\n", "a: |+\n  x\n" + bom + "\n--- y\n" ]
        for (input, expected) in cases do
            expectWritten input expected
            Expect.equal (Writer.writeDocuments (withStyles RequireStored) (parseOk input)) expected (sprintf "RequireStored for %A" input)

    // yaml@2.6.1 reads the first three expected texts and the flow pairs as keys, and it rejects a double-quoted key of 1025 characters without a question mark. The key with the tabs has
    // 1020 characters with the quotes, and the flow key with the commas has 1001. A parsed implicit key of up to 1024 characters stays implicit, so the writer measures the spelling that it
    // writes and writes the shortest one: a raw tab in double quotes, a comma without a blank in a flow collection, a flow pair in a flow sequence, and the raw characters when the escapes
    // pass the limit.
    testCase "an implicit key that is within 1024 characters in its shortest spelling stays implicit" <| fun _ ->
        let key = String.replicate 1010 "a" + String.replicate 8 "\t"
        Expect.equal (writeNode (YamlNode.mapping [ YamlNode.entry (dq key) (sc "v") ])) ("\"" + key + "\": v\n") "Tabs in a double-quoted key"
        let items = [ for _ in 1 .. 500 -> sc "a" ]
        Expect.equal (writeNode (YamlNode.mapping [ YamlNode.entry (flowSeq items) (sc "v") ])) ("[" + String.concat "," [ for _ in 1 .. 500 -> "a" ] + "]: v\n") "Commas in a flow key"
        let keyed (key: YamlNode) = writeNode (YamlNode.mapping [ YamlNode.entry key (sc "v") ])
        let joined (parts: string list) = String.concat "," parts
        // Below the limit a flow key has the normal spelling, with a blank behind the comma and with braces around a pair.
        Expect.equal (keyed (flowSeq [ sc "a"; sc "b" ])) "[a, b]: v\n" "A short flow sequence"
        Expect.equal (keyed (flowSeq [ flowMap [ "a", sc "b" ] ])) "[{a: b}]: v\n" "A short flow pair"
        Expect.equal (keyed (flowMap [ "c", sc "d"; "e", sc "f" ])) "{c: d, e: f}: v\n" "A short flow mapping"
        Expect.equal (keyed (flowSeq [ dq ("a" + del + "b") ])) ("[\"a\\x7Fb\"]: v\n") "A short flow key escapes DEL"
        // Past the limit the key has the shortest spelling: a flow pair in a flow sequence without braces, and the commas without blanks.
        Expect.equal (keyed (flowSeq [ for _ in 1 .. 150 -> flowMap [ "a", sc "b" ] ])) ("[" + joined [ for _ in 1 .. 150 -> "a: b" ] + "]: v\n") "Flow pairs in a flow key"
        Expect.equal (keyed (flowSeq [ for _ in 1 .. 250 -> YamlNode.flowMapping [ YamlNode.entry (sc "a") nothing ] ])) ("[" + joined [ for _ in 1 .. 250 -> "a:" ] + "]: v\n") "Flow pairs without a value"
        Expect.equal (keyed (flowSeq [ for _ in 1 .. 85 -> flowMap [ "a", sc "b"; "c", sc "d" ] ])) ("[" + joined [ for _ in 1 .. 85 -> "{a: b,c: d}" ] + "]: v\n") "A mapping with two entries keeps its braces"
        let dels = String.replicate 300 del
        Expect.equal (keyed (flowSeq [ dq dels ])) ("[\"" + dels + "\"]: v\n") "Raw DEL in a flow key past the limit"
        let marks = String.replicate 200 bom
        Expect.equal (writeNode (YamlNode.mapping [ YamlNode.entry (dq marks) (sc "v") ])) ("\"" + marks + "\": v\n") "Raw marks when the escapes pass the limit"
        Expect.equal (writeNode (YamlNode.mapping [ YamlNode.entry (dq (String.replicate 10 bom)) (sc "v") ])) ("\"" + String.replicate 10 "\\uFEFF" + "\": v\n") "Escapes below the limit"
        let nels = String.replicate 600 nel
        Expect.equal (keyed (dq nels)) ("\"" + nels + "\": v\n") "Raw NEL when the named escapes pass the limit"
        Expect.equal (keyed (dq (String.replicate 10 nel))) ("\"" + String.replicate 10 "\\N" + "\": v\n") "Named escapes below the limit"
        // A value keeps the spelling with the blank and the braces, and a key over the limit needs a question mark.
        Expect.equal (writeNode (mapOf [ "k", flowSeq [ sc "a"; sc "b" ] ])) "k: [a, b]\n" "A flow value"
        Expect.equal (writeNode (mapOf [ "k", flowSeq [ flowMap [ "a", sc "b" ] ] ])) "k: [{a: b}]\n" "A flow value with a mapping"
        Expect.isTrue ((writeNode (YamlNode.mapping [ YamlNode.entry (dq (String.replicate 1025 "a")) (sc "v") ])).StartsWith "? ") "A key over the limit"

    // The parser reads the trees of these texts, and the writer gives the texts back.
    testCase "the keys of the 1024 limit round trip" <| fun _ ->
        let key = String.replicate 1010 "a" + String.replicate 8 "\t"
        expectStable ("\"" + key + "\": v\n")
        expectStable ("[" + String.concat "," [ for _ in 1 .. 500 -> "a" ] + "]: v\n")
        expectWritten ("[ a : b ]: v\n") "[{a: b}]: v\n"
        expectStable "[{a: b}, {c: d}]: v\n"
        expectStable ("[" + String.concat "," [ for _ in 1 .. 150 -> "a: b" ] + "]: v\n")

    // The final review wrote these constructed trees with every policy and got text that the parser rejects or reads as another tree. Validate rejects them now, and the writer raises
    // with its messages. yaml@2.6.1 rejects the texts of the comment and of the names and reads the others as other data.
    testCase "a constructed tree that the productions do not allow is an error under every policy" <| fun _ ->
        // The constructors reject a bad name, so the trees are built from the records.
        let aliasOf (name: string) = { nothing with Kind = YamlNodeKind.Alias name }
        let taggedWith (tag: YamlTag) =
            { sc "v" with
                Properties = { Tag = Some tag; Anchor = None }
                PropertyLayout = [ { Property = PropertyName.Tag; After = Separation.Inline; Span = None } ] }
        let anchoredAs (name: string) (node: YamlNode) =
            { node with
                Properties = { node.Properties with Anchor = Some name }
                PropertyLayout = [ { Property = PropertyName.Anchor; After = Separation.Inline; Span = None } ] }
        let withPrelude (items: PreludeItem list) = StreamPart.Document { document (sc "a") with Prelude = items; StartMarker = Some(marker None) }
        let cases =
            [ "nb-char", (fun (options: WriterOptions) -> Writer.writeNode options (withNote (" a" + bom) (sc "v")))
              "nb-char", (fun options -> Writer.writeNode options (withNote " a\001" (sc "v")))
              "nb-char", (fun options -> Writer.writeNode options (mapOf [ "k", withHeader (" a" + bom) (lit "x\n") ]))
              "ns-anchor-char", (fun options -> Writer.writeNode options (anchoredAs ("a" + bom + "b") (sc "v")))
              "ns-anchor-char", (fun options -> Writer.writeNode options (seqOf [ YamlNode.withAnchor "a" (sc "v"); aliasOf ("a" + string (char 1)) ]))
              "colon", (fun options -> Writer.writeNode options (YamlNode.mapping [ { entryOf "k" (sc "v") with Colon = None } ]))
              "colon", (fun options -> Writer.writeNode options (YamlNode.flowMapping [ { entryOf "k" (sc "v") with Colon = None } ]))
              "%YAML", (fun options -> Writer.writeDocuments options { Parts = [ withPrelude [ directive (DirectiveKind.YamlVersion(2, 0)) ] ] })
              "%YAML", (fun options -> Writer.writeDocuments options { Parts = [ withPrelude [ directive (DirectiveKind.YamlVersion(1, 2)); directive (DirectiveKind.YamlVersion(1, 2)) ] ] })
              "tag handle", (fun options -> Writer.writeDocuments options { Parts = [ withPrelude [ directive (DirectiveKind.TagDirective("e", "tag:example.com,2000:")) ] ] })
              "reserved directive", (fun options -> Writer.writeDocuments options { Parts = [ withPrelude [ directive (DirectiveKind.ReservedDirective("FOO BAR", [])) ] ] })
              "reserved directive", (fun options -> Writer.writeDocuments options { Parts = [ withPrelude [ directive (DirectiveKind.ReservedDirective("FOO", [ "#c" ])) ] ] })
              "(R4)", (fun options -> Writer.writeNode options (mapOf [ "a", withEnd [ blank ] (mapOf [ "b", sc "c" ]) ]))
              "(R4)", (fun options -> Writer.writeNode options (withEnd [ note " d" ] (mapOf [ "a", withEnd [ blank ] (mapOf [ "b", sc "c" ]) ])))
              "(R4)", (fun options -> Writer.writeNode options (withEnd [ blank ] (mapOf [ "a", withEnd [ note " d" ] (mapOf [ "b", sc "c" ]) ])))
              "suffix", (fun options -> Writer.writeNode options (taggedWith (YamlTag.Named { Prefix = YamlTag.coreNamespace; Suffix = "" }))) ]
        for (fragment, write) in cases do
            for options in [ defaults; withStyles RequireStored; withStyles Canonical ] do
                Expect.stringContains (writeError (fun () -> write options)).Message fragment (sprintf "The message for the fragment %s" fragment)
        // The same trees without the fault are written.
        Expect.equal (writeNode (withNote (" a\tb" + nel + lineSeparator) (sc "v"))) ("v # a\tb" + nel + lineSeparator + "\n") "A comment with a tab, NEL and a separator"
        Expect.equal (writeStream [ withPrelude [ directive (DirectiveKind.YamlVersion(1, 1)); directive (DirectiveKind.TagDirective("!e!", "tag:example.com,2000:")) ] ]) "%YAML 1.1\n%TAG !e! tag:example.com,2000:\n--- a\n" "Directives"

    // yaml@2.6.1 reads the answers of the function as the chomping of the scalar that the writer writes. The function follows the last node of the text.
    testCase "endsWithKeptLines follows the last node of the text to a block scalar with the chomping Keep" <| fun _ ->
        let keepOf (value: string) = blockScalar BlockScalarStyle.Literal ChompingMode.Keep None value
        let ends (node: YamlNode) = Writer.endsWithKeptLines defaults node
        Expect.isTrue (ends (keepOf "x\n")) "A root scalar"
        Expect.isTrue (ends (mapOf [ "a", sc "1"; "b", keepOf "x\n" ])) "The last value of a mapping"
        Expect.isTrue (ends (seqOf [ sc "1"; keepOf "x\n" ])) "The last item of a sequence"
        Expect.isTrue (ends (mapOf [ "a", seqOf [ mapOf [ "b", keepOf "x\n" ] ] ])) "Collections one in the other"
        Expect.isTrue (ends (YamlNode.mapping [ explicitEntry (sc "k") (keepOf "x\n") ])) "The value of an explicit entry"
        Expect.isTrue (ends (YamlNode.mapping [ explicitKeyOnly (keepOf "x\n") ])) "A key without a colon"
        Expect.isFalse (ends (mapOf [ "a", keepOf "x\n"; "b", sc "1" ])) "A plain value behind the scalar"
        Expect.isFalse (ends (YamlNode.mapping [ explicitEntry (keepOf "x\n") (sc "v") ])) "A Keep key with a value behind it"
        Expect.isFalse (ends (withEnd [ note " c" ] (mapOf [ "a", keepOf "x\n" ]))) "Trivia at the end of the collection"
        Expect.isFalse (ends (mapOf [ "a", flowSeq [ keepOf "x\n" ] ])) "A flow collection"
        Expect.isFalse (ends (mapOf [ "a", sq "x" ])) "A quoted scalar"
        Expect.isFalse (ends (mapOf [ "a", YamlNode.empty ])) "A missing value"
        Expect.isFalse (ends (YamlNode.sequence [ YamlNode.item (keepOf ("a" + string (char 1) + "\n")) ])) "A value that no block scalar can hold"
        // The chomping comes from the value when the stored one does not fit.
        let blockOf (chomping: ChompingMode) (value: string) = blockScalar BlockScalarStyle.Literal chomping None value
        Expect.isTrue (ends (blockOf ChompingMode.Clip "x\n\n")) "Clip with two line feeds"
        Expect.isTrue (ends (blockOf ChompingMode.Strip "\n")) "Strip with a line feed only"
        Expect.isFalse (ends (blockOf ChompingMode.Clip "x\n")) "Clip with one line feed"
        Expect.isFalse (ends (blockOf ChompingMode.Strip "x")) "Strip"
        Expect.isFalse (ends (keepOf "x")) "Keep without a final line feed, which the writer writes as Strip when text follows"
        let strict = withStyles RequireStored
        Expect.isFalse (Writer.endsWithKeptLines strict (keepOf "x")) "RequireStored raises for a stored Keep that does not fit, so it is no kept line"
        Expect.isTrue (Writer.endsWithKeptLines strict (keepOf "x\n")) "RequireStored with a stored Keep that fits"
        Expect.isFalse (Writer.endsWithKeptLines strict (blockOf ChompingMode.Clip "x\n")) "RequireStored with Clip"
        // Canonical writes no block scalar. Validate rejects a blank line behind a stored Keep that fits under every policy, so only that Keep counts there.
        let canonical = withStyles Canonical
        Expect.isTrue (Writer.endsWithKeptLines canonical (keepOf "x\n")) "Canonical with a stored Keep that fits"
        Expect.isTrue (Writer.endsWithKeptLines canonical (keepOf "")) "Canonical with a stored Keep on an empty value"
        Expect.isFalse (Writer.endsWithKeptLines canonical (keepOf "x")) "Canonical with a stored Keep that does not fit"
        Expect.isFalse (Writer.endsWithKeptLines canonical (blockOf ChompingMode.Clip "x\n\n")) "Canonical with a value that only needs Keep"
        Expect.isFalse (Writer.endsWithKeptLines canonical (keepOf ("a" + string (char 1) + "\n"))) "Canonical with a value that no block scalar can hold"

    // The answer is the chomping of the scalar that the writer writes when more text follows the node. The test writes the node in front of another entry and looks at the header.
    testCase "endsWithKeptLines agrees with the header that the writer writes" <| fun _ ->
        for style in [ BlockScalarStyle.Literal; BlockScalarStyle.Folded ] do
            for stored in [ ChompingMode.Strip; ChompingMode.Clip; ChompingMode.Keep ] do
                for value in [ ""; "x"; "x\n"; "x\n\n"; "\n"; "\n\n"; "x\ny\n\n\n" ] do
                    let node = blockScalar style stored None value
                    let text = Writer.writeNode defaults (YamlNode.mapping [ entryOf "a" node; entryOf "b" (sc "1") ])
                    let header = text.Substring(3, text.IndexOf '\n' - 3)
                    Expect.equal (Writer.endsWithKeptLines defaults node) (header.EndsWith "+") (sprintf "The header %s of %A with the stored chomping %A" header value stored)

    // A tree nested 10000 levels deep ends in a Keep scalar, which the function finds without a recursion.
    testCase "endsWithKeptLines walks 10000 levels without a recursion" <| fun _ ->
        let mutable node = blockScalar BlockScalarStyle.Literal ChompingMode.Keep None "x\n"
        for _ in 1 .. depth do
            node <- YamlNode.sequence [ YamlNode.item (sc "a"); YamlNode.item node ]
        Expect.isTrue (Writer.endsWithKeptLines defaults node) "The last item of every sequence"

    // ARCtrl reads a text whose last node is `a: |+` with the body `  abc` and no final line feed. The parser keeps the Keep and the value "abc". The text cannot end the scalar once more follows
    // it, so the stored Keep does not fit, the chomping of the value is Strip and the blank line behind the scalar is trivia. Validate, the writer and endsWithKeptLines use one rule.
    testCase "a stored Keep on a value without a final line feed leaves the blank line behind it as trivia" <| fun _ ->
        let keep = blockScalar BlockScalarStyle.Literal ChompingMode.Keep None "abc"
        let next = { entryOf "b" (sc "x") with Before = [ blank ] }
        let tree = YamlNode.mapping [ entryOf "a" keep; next ]
        Expect.isEmpty (Validate.stream { Parts = [ StreamPart.Document(document tree) ] }) "Validate accepts the tree"
        Expect.equal (Writer.writeNode defaults tree) "a: |-\n  abc\n\nb: x\n" "PreferStored writes Strip"
        Expect.equal (Writer.writeNode (withStyles Canonical) tree) "a: abc\n\nb: x\n" "Canonical"
        Expect.stringContains (writeError (fun () -> Writer.writeNode (withStyles RequireStored) tree)).Message "does not match the line feeds" "RequireStored raises the mismatch of the style"
        for policy in [ PreferStored; RequireStored; Canonical ] do
            Expect.isFalse (Writer.endsWithKeptLines (withStyles policy) keep) (sprintf "endsWithKeptLines under %A" policy)
        // The node as the parser reads it from the text.
        let parsedKeep =
            match (parseOk "a: |+\n  abc").Parts with
            | [ StreamPart.Document parsed ] ->
                match parsed.Root.Kind with
                | YamlNodeKind.Mapping collection -> collection.Items.Head.Value
                | _ -> failwith "A mapping was expected"
            | _ -> failwith "One document was expected"
        Expect.equal (Writer.writeNode defaults (YamlNode.mapping [ entryOf "a" parsedKeep; next ])) "a: |-\n  abc\n\nb: x\n" "The parsed node"
        Expect.isFalse (Writer.endsWithKeptLines defaults parsedKeep) "The parsed node ends no kept lines"
        // The same scalar is a stored Keep that fits when the value ends in a line feed, and then the blank line cannot stand behind it.
        let fitting = YamlNode.mapping [ entryOf "a" (blockScalar BlockScalarStyle.Literal ChompingMode.Keep None "abc\n"); next ]
        for policy in [ PreferStored; RequireStored; Canonical ] do
            let message = (writeError (fun () -> Writer.writeNode (withStyles policy) fitting)).Message
            Expect.stringContains message "stored chomping Keep" (sprintf "The rule under %A" policy)
            Expect.isTrue (Writer.endsWithKeptLines (withStyles policy) (blockScalar BlockScalarStyle.Literal ChompingMode.Keep None "abc\n")) (sprintf "endsWithKeptLines under %A" policy)

    // For each value and stored chomping, a blank line behind the scalar is rejected by Validate exactly when the stored Keep fits the value. endsWithKeptLines is true then under every policy,
    // and it is false for every tree that Validate accepts, except for a value that only needs Keep, where PreferStored and RequireStored report it because the writer double-quotes the scalar.
    // Where the write succeeds, the text holds the blank line as trivia in front of the next entry and the scalar has no Keep header.
    testCase "Validate, the written text and endsWithKeptLines agree for every value and stored chomping" <| fun _ ->
        let beforeSecond (text: string) : Trivia list =
            match (parseOk text).Parts with
            | [ StreamPart.Document parsed ] ->
                match parsed.Root.Kind with
                | YamlNodeKind.Mapping collection -> collection.Items.[1].Before
                | _ -> failwith "A mapping was expected"
            | _ -> failwith "One document was expected"
        let next = { entryOf "b" (sc "x") with Before = [ blank ] }
        for style in [ BlockScalarStyle.Literal; BlockScalarStyle.Folded ] do
            for stored in [ ChompingMode.Strip; ChompingMode.Clip; ChompingMode.Keep ] do
                for value in [ "abc"; "abc\n"; ""; "abc\n\n"; "\n"; "abc\n\n\n" ] do
                    let node = blockScalar style stored None value
                    let tree = YamlNode.mapping [ entryOf "a" node; next ]
                    let fits = stored = ChompingMode.Keep && (value = "" || value.EndsWith "\n")
                    let problems = Validate.stream { Parts = [ StreamPart.Document(document tree) ] }
                    Expect.equal problems.IsEmpty (not fits) (sprintf "Validate for %A with the stored chomping %A" value stored)
                    for policy in [ PreferStored; RequireStored; Canonical ] do
                        let options = withStyles policy
                        let label = sprintf "%A with the stored chomping %A under %A" value stored policy
                        // A value of several line feeds, or of line feeds only, gets the chomping Keep when the stored one is another.
                        let needsKeep = stored <> ChompingMode.Keep && (value = "\n" || value.EndsWith "\n\n")
                        Expect.equal (Writer.endsWithKeptLines options node) (fits || (needsKeep && policy <> Canonical)) (sprintf "endsWithKeptLines for %s" label)
                        if fits then
                            Expect.stringContains (writeError (fun () -> Writer.writeNode options tree)).Message "stored chomping Keep" (sprintf "The write for %s" label)
                        else
                            let written = try Some(Writer.writeNode options tree) with YamlWriteException _ -> None
                            match written with
                            | Some text ->
                                Expect.isTrue (Validate.startsWithBlankLine (beforeSecond text)) (sprintf "The blank line is trivia for %s" label)
                                let header = text.Substring(3, text.IndexOf '\n' - 3)
                                Expect.isFalse (header.EndsWith "+") (sprintf "The header %s for %s" header label)
                            | None ->
                                Expect.equal policy RequireStored (sprintf "Only RequireStored raises for %s" label)
]

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

    testCase "writing Keep scalars that a blank line follows at depth takes time linear in their number" <| fun _ ->
        expectLinear "write of Keep scalars with a blank line behind them at depth" writeKeepScalarsAtDepth countLineBreaks (fun lines -> repeated "x\n" lines)

    // The blank line behind the scalar is found through 10000 open collections, which have nothing more to write.
    testCase "a blank line behind a scalar with the chomping Keep nested 10000 levels deep" <| fun _ ->
        let clipped = blockScalar BlockScalarStyle.Literal ChompingMode.Clip None "x\n\n"
        let mutable chain = clipped
        for _ in 2 .. depth do
            chain <- YamlNode.sequence [ YamlNode.item chain ]
        let withBlank = YamlNode.sequence [ YamlNode.item chain; itemBefore [ blank ] (sc "y") ]
        Expect.equal (writeNode withBlank) (repeated "- " depth + "\"x\\n\\n\"\n\n- y\n") "The scalar is double-quoted"
        let withoutBlank = YamlNode.sequence [ YamlNode.item chain; YamlNode.item (sc "y") ]
        Expect.isTrue ((writeNode withoutBlank).StartsWith(repeated "- " depth + "|+\n")) "The scalar stays a block scalar"

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
    testList "Writer" [ portedTests; slotTests; scalarTests; tagTests; documentTests; blockLayoutTests; policyTests; styleTests; invalidTreeTests; keepBlankTests; finalReviewTests; depthTests ]
