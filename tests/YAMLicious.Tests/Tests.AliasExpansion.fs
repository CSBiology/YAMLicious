module Tests.AliasExpansion

// The tests of the functions in `Aliases`. The parser keeps an alias as a node, and the functions replace it by a copy of the node that its anchor names.
//
// The reference results are the values that yaml@2.6.1 and ruamel read. ruamel is the round trip loader of ruamel.yaml 0.19.1. Both give an alias the
// value of the most recent anchor of its name in front of it, and the tests of the copies check the value of the expanded tree against that. The tests of
// the expansion show the expanded tree through its value and through the text that the writer writes, because the copies share their nodes with the
// anchored node.

open System
open System.Text
open Fable.Pyxpecto
open YAMLicious

/// The only document of the text with its aliases expanded. A tree that breaks the slot rules fails the test.
let private expandedDocument (text: string) : YamlDocument =
    match Aliases.expandDocument AliasOptions.defaults (parseDocument text) with
    | Ok document ->
        let problems = Validate.document document
        if not problems.IsEmpty then
            failwithf "The expansion of %A breaks the slot rules: %s" text (String.concat "; " problems)
        document
    | Error error -> failwithf "The expansion of %A fails: %s" text error.Message

let private expandedRoot (text: string) : YamlNode = (expandedDocument text).Root

/// The value of the expanded tree as JSON, from `valueJson`.
let private expandedValue (text: string) : string = valueJson (expandedRoot text)

/// The text that the writer writes for the expanded document.
let private expandedText (text: string) : string = Writer.write WriterOptions.defaults (expandedDocument text)

/// The error of the expansion with the default limit.
let private expansionError (text: string) : YamlError =
    match Aliases.expandNode AliasOptions.defaults (parseRoot text) with
    | Error error -> error
    | Ok _ -> failwithf "The expansion of %A succeeds" text

/// The error as its code, the span of the alias and the related spans, as in `recursive-alias 1:5-1:7 [1:1-1:8]`.
let private describe (error: YamlError) : string =
    YamlErrorCode.name error.Code + " " + showSpan error.Span + " [" + String.concat ", " (List.map showSpan error.Related) + "]"

/// Writes the expanded document, reads the text again and compares the two trees.
let private expectRoundTrip (text: string) =
    let document = expandedDocument text
    let written = Writer.write WriterOptions.defaults document
    let again = Reader.read written
    if not (YamlDocument.equalIgnoringSpans document again) then
        failwithf
            "The expanded tree of %A differs from the tree of its written text %A\nActual:\n%s\nExpected:\n%s"
            text
            written
            (YamlDocument.show again)
            (YamlDocument.show document)

let private expectValues (rows: (string * string) list) =
    for (text, expected) in rows do
        Expect.equal (expandedValue text) expected (sprintf "The value of %A" text)

let private valueTests = testList "The value of a copy" [
    // yaml@2.6.1 and ruamel read each alias with the value of its anchor. An explicit tag keeps `!!str 1` a string in yaml@2.6.1, and ruamel keeps the tag as an
    // object of its own.
    testCase "an alias of a scalar becomes a copy of the scalar" <| fun _ ->
        expectValues [
            "a: &x 1\nb: *x", "{\"a\":1,\"b\":1}"
            "a: &x plain text\nb: *x", "{\"a\":\"plain text\",\"b\":\"plain text\"}"
            "a: &x \"dq\\tx\"\nb: *x", "{\"a\":\"dq\\tx\",\"b\":\"dq\\tx\"}"
            "a: &x 'sq'\nb: *x", "{\"a\":\"sq\",\"b\":\"sq\"}"
            "a: &x |\n  lit\n  eral\nb: *x", "{\"a\":\"lit\\neral\\n\",\"b\":\"lit\\neral\\n\"}"
            "a: &x >\n  fol\n  ded\nb: *x", "{\"a\":\"fol ded\\n\",\"b\":\"fol ded\\n\"}"
            "a: &x \"multi\n  line\"\nb: *x", "{\"a\":\"multi line\",\"b\":\"multi line\"}"
            "a: &x\nb: *x", "{\"a\":null,\"b\":null}"
            "a: &x !!str 1\nb: *x", "{\"a\":\"1\",\"b\":\"1\"}"
            "a: !!int &x \"5\"\nb: *x", "{\"a\":5,\"b\":5}"
            "- &x 1\n- *x\n- *x", "[1,1,1]"
            "[&x 1, *x]", "[1,1]"
            "{a: &x 1, b: *x}", "{\"a\":1,\"b\":1}"
        ]

    // yaml@2.6.1 and ruamel build one object for the anchored node and for its aliases and read the values below.
    testCase "an alias of a mapping or a sequence becomes a copy of the collection" <| fun _ ->
        expectValues [
            "a: &x {p: 1}\nb: *x", "{\"a\":{\"p\":1},\"b\":{\"p\":1}}"
            "a: &x\n  p: 1\n  q: [2, 3]\nb: *x", "{\"a\":{\"p\":1,\"q\":[2,3]},\"b\":{\"p\":1,\"q\":[2,3]}}"
            "a: &x\n  p: 1\nb:\n  - *x\n  - *x", "{\"a\":{\"p\":1},\"b\":[{\"p\":1},{\"p\":1}]}"
            "a: &x {}\nb: *x", "{\"a\":{},\"b\":{}}"
            "- &x\n  p: 1\n- *x", "[{\"p\":1},{\"p\":1}]"
            "a: &x [1, 2]\nb: *x\nc: *x", "{\"a\":[1,2],\"b\":[1,2],\"c\":[1,2]}"
            "a: &x\n  - 1\n  - 2\nb: *x", "{\"a\":[1,2],\"b\":[1,2]}"
            "a: &x\n- 1\n- 2\nb: *x", "{\"a\":[1,2],\"b\":[1,2]}"
            "a: &x []\nb: *x", "{\"a\":[],\"b\":[]}"
            "- &x [1]\n- *x\n- [*x]", "[[1],[1],[[1]]]"
            "a: &x {p: 1}\nb: {j: *x}\nc: [*x]", "{\"a\":{\"p\":1},\"b\":{\"j\":{\"p\":1}},\"c\":[{\"p\":1}]}"
        ]

    // yaml@2.6.1 and ruamel: {"a":[1],"b":[[1],[1]],"c":[[1],[1]]}. The aliases inside b name a, and c names b with those aliases resolved.
    testCase "a copy holds the copies of the aliases inside its node" <| fun _ ->
        expectValues [
            "a: &x [1]\nb: &y [*x, *x]\nc: *y", "{\"a\":[1],\"b\":[[1],[1]],\"c\":[[1],[1]]}"
            "&r [x, &s y, *s]", "[\"x\",\"y\",\"y\"]"
        ]

    // yaml@2.6.1 and ruamel read the alias of a key as the key. `*k:` without a blank is an error in both, because the name of an alias may hold a colon.
    // For a collection as key, yaml@2.6.1 builds a string and ruamel a key object.
    testCase "an alias can be a key" <| fun _ ->
        expectValues [
            "a: &k key\n*k : v", "{\"a\":\"key\",\"key\":\"v\"}"
            "a: &k key\n? *k\n: v", "{\"a\":\"key\",\"key\":\"v\"}"
            "{a: &k key, *k : v}", "{\"a\":\"key\",\"key\":\"v\"}"
            "[ &k key, {*k : v} ]", "[\"key\",{\"key\":\"v\"}]"
            "&k a: 1\nb: *k", "{\"a\":1,\"b\":\"a\"}"
            "a: &x \"q\"\nb: [*x, {*x : *x}]", "{\"a\":\"q\",\"b\":[\"q\",{\"q\":\"q\"}]}"
        ]
        let entries = YamlNode.entries (expandedRoot "a: &m {p: 1}\n? *m\n: v")
        Expect.nodeEqual entries.[1].Key entries.[0].Value "The key is a copy of the mapping"

    // The most recent anchor of a name in front of the alias serves it, in the order of the text. yaml@2.6.1 and ruamel read these values. ruamel warns about the
    // repeated anchor and goes on.
    testCase "an anchor that is defined again serves the aliases behind it" <| fun _ ->
        expectValues [
            "a: &x 1\nb: *x\nc: &x 2\nd: *x", "{\"a\":1,\"b\":1,\"c\":2,\"d\":2}"
            "p: &a [ &a 1 ]\nq: *a", "{\"p\":[1],\"q\":1}"
            "&a [ &a 1, *a ]", "[1,1]"
            "a: &x 1\nb: [*x, &x 2, *x]", "{\"a\":1,\"b\":[1,2,2]}"
            "p: &a [ x, &b 1 ]\nq: &b 2\nr: *b", "{\"p\":[\"x\",1],\"q\":2,\"r\":2}"
            "p: &a {k: &a v, j: *a}\nq: *a", "{\"p\":{\"k\":\"v\",\"j\":\"v\"},\"q\":\"v\"}"
        ]

    // yaml@2.6.1 and ruamel read an alias of a tagged node with the tag. The copy has the tag and the anchor of its node.
    testCase "a copy keeps the tag and the anchor of its node" <| fun _ ->
        let entries = YamlNode.entries (expandedRoot "a: !!int &x \"5\"\nb: *x")
        Expect.equal entries.[1].Value.Properties entries.[0].Value.Properties "The properties of the copy"
        Expect.equal entries.[1].Value.Properties.Anchor (Some "x") "The anchor"
        Expect.equal entries.[1].Value.Properties.Tag (Some (YamlTag.core "int")) "The tag"
        Expect.equal entries.[1].Value.PropertyLayout.Length 2 "Both properties have a placement"

    testCase "a copy has the span of its node, and so has every node below it" <| fun _ ->
        let entries = YamlNode.entries (expandedRoot "a: &x [1, {k: v}]\nb: *x")
        let spanOfNode (node: YamlNode) = node.Span |> Option.map showSpan |> Option.defaultValue "none"
        Expect.equal (spanOfNode entries.[1].Value) "1:4-1:18" "The span of the copy is the span of the anchored node"
        Expect.equal (spanOfNode entries.[0].Value) "1:4-1:18" "The anchored node"
        let below (node: YamlNode) = YamlNode.items node |> List.map (fun item -> spanOfNode item.Node)
        Expect.equal (below entries.[1].Value) (below entries.[0].Value) "The nodes below the copy"

    testCase "a tree without aliases comes back unchanged" <| fun _ ->
        for text in [ "a: 1"; "a: &x 1"; "- [a, b]\n- {k: v}"; "a: &x\n  p: 1 # c\n# end"; "k: |\n  lit\n" ] do
            let before = parseDocument text
            match Aliases.expandDocument AliasOptions.defaults before with
            | Ok after -> Expect.isTrue (YamlDocument.equalIgnoringSpans before after) (sprintf "The document of %A" text)
            | Error error -> failwithf "The expansion of %A fails: %s" text error.Message

    // yaml@2.6.1 and ruamel read whole documents. The function on a node serves the aliases from the anchors that stand inside that node.
    testCase "the node functions work on any node whose anchors stand inside it" <| fun _ ->
        let entries = YamlNode.entries (parseRoot "a: &x 1\nb: {k: &y 2, j: *y}")
        match Aliases.expandNode AliasOptions.defaults entries.[1].Value with
        | Ok node -> Expect.equal (valueJson node) "{\"k\":2,\"j\":2}" "The value of the inner mapping"
        | Error error -> failwithf "The expansion fails: %s" error.Message
]

let private commentTests = testList "The comments of a copy" [
    // The comment behind the alias goes to the slot where the parser puts a comment behind the copy, and the comment behind the anchored node stays with that
    // node. Neither yaml@2.6.1 nor ruamel gives a comment to the value.
    testCase "the comment behind an alias replaces the comment behind the node of its anchor" <| fun _ ->
        Expect.equal (expandedText "a: &x [1] # c\nb: *x # d") "a: &x [1] # c\nb: &x [1] # d\n" "A flow collection"
        Expect.equal (expandedText "a: &x [1] # c\nb: *x") "a: &x [1] # c\nb: &x [1]\n" "No comment behind the alias"
        Expect.equal (expandedText "a: &x 1 # c\nb: *x # d") "a: &x 1 # c\nb: &x 1 # d\n" "A scalar"
        Expect.equal (expandedText "k:\n  - &x 1\n  - *x # c\n  - *x") "k:\n  - &x 1\n  - &x 1 # c\n  - &x 1\n" "In a sequence"
        Expect.equal (expandedText "a: &x | # h\n  lit\nb: *x # d") "a: &x | # h\n  lit\nb: &x | # d\n  lit\n" "The header of a block scalar"
        Expect.equal (expandedText "a: &x # c\n  p: 1 # t\nb: *x # d") "a: &x # c\n  p: 1 # t\nb: &x # d\n  p: 1 # t\n" "Behind the last property of a block collection"
        Expect.equal (expandedText "a: &x !!str\nb: *x # d") "a: &x !!str\nb: &x !!str # d\n" "Behind the last property of a node without content"
        Expect.equal (expandedText "a: &x\nb: *x # d") "a: &x\nb: &x # d\n" "Behind the anchor of a missing value"

    testCase "the comments inside the node of the anchor stay in the copy" <| fun _ ->
        Expect.equal
            (expandedText "a: &x\n  # inner\n  p: 1 # t\nb: *x")
            "a: &x\n  # inner\n  p: 1 # t\nb: &x\n  # inner\n  p: 1 # t\n"
            "A block mapping"
        Expect.equal (expandedText "a: &x [ # o\n  1 ]\nb: *x") "a: &x [ # o\n  1\n  ]\nb: &x [ # o\n  1\n  ]\n" "A flow sequence with an opening comment"

    testCase "the comment lines in front of an alias stay where they stand" <| fun _ ->
        Expect.equal (expandedText "a: &x\n  p: 1\nb:\n  # lead\n  *x # t\n# last") "a: &x\n  p: 1\nb:\n  # lead\n  &x # t\n  p: 1\n# last\n" "A block mapping on its own line"

    // yaml@2.6.1 and ruamel: {"a":"lit\n\n","b":"lit\n\n","c":1}. A blank line behind the copy would join the empty lines of a block scalar with the chomping Keep, so the
    // copy is a double-quoted scalar with the same value.
    testCase "a block scalar with the chomping keep is copied as a double-quoted scalar" <| fun _ ->
        Expect.equal (expandedText "a: &x |+\n  lit\n\nb: *x # d\n\nc: 1") "a: &x |+\n  lit\n\nb: &x \"lit\\n\\n\" # d\n\nc: 1\n" "In front of a blank line"
        Expect.equal (expandedValue "a: &x |+\n  lit\n\nb: *x\n\nc: 1") "{\"a\":\"lit\\n\\n\",\"b\":\"lit\\n\\n\",\"c\":1}" "The value"
]

let private styleTests = testList "The style of a copy" [
    // yaml@2.6.1 and ruamel: {"a":{"k":"v"},"b":[{"k":"v"}]}. A block collection stays a block collection in the tree. A flow collection cannot hold it, so the writer writes it as
    // a flow collection, and the text reads back as a tree with another style and the same value.
    testCase "a block collection in a flow collection keeps its style and writes in the style that the flow collection allows" <| fun _ ->
        let text = "a: &x\n  k: v\nb: [*x]"
        let copy = (YamlNode.items (YamlNode.entries (expandedRoot text)).[1].Value).Head.Node
        Expect.equal (YamlNode.tryMapping copy |> Option.map (fun collection -> collection.Style)) (Some CollectionStyle.Block) "The style in the tree"
        Expect.equal (valueJson (Reader.readRoot (expandedText text))) (expandedValue text) "The value of the written text"
        Expect.equal (expandedValue text) "{\"a\":{\"k\":\"v\"},\"b\":[{\"k\":\"v\"}]}" "The value"
        Expect.equal (expandedValue "a: &x\n  k: v\nb: {j: *x}") "{\"a\":{\"k\":\"v\"},\"b\":{\"j\":{\"k\":\"v\"}}}" "In a flow mapping"
        Expect.equal (expandedValue "a: &x |\n  lit\nb: [*x]") "{\"a\":\"lit\\n\",\"b\":[\"lit\\n\"]}" "A block scalar"
        Expect.equal (valueJson (Reader.readRoot (expandedText "a: &x |\n  lit\nb: [*x]"))) (expandedValue "a: &x |\n  lit\nb: [*x]") "The value of the written block scalar"

    // The writer writes an implicit key on one line, so the lines of a quoted key vanish from the written text.
    testCase "a multi-line quoted scalar as an implicit key keeps its value" <| fun _ ->
        let text = "a: &k \"x\n  y\"\n*k : v"
        Expect.equal (expandedValue text) "{\"a\":\"x y\",\"x y\":\"v\"}" "The value"
        Expect.equal (valueJson (Reader.readRoot (expandedText text))) (expandedValue text) "The value of the written text"
]

let private roundTripTests = testList "The written text of an expanded tree" [
    // The written text reads as an equal tree. The anchor of each copy is written again, as the doc comment of Aliases says.
    testCase "an expanded tree writes text that reads as the same tree" <| fun _ ->
        for text in [
            "a: &x 1\nb: *x"
            "a: &x [1, 2]\nb: *x"
            "a: &x\n  p: 1\n  q: 2\nb: *x"
            "a: &x\n  p: 1\nb:\n  - *x\n  - *x"
            "- &x\n  p: 1\n- *x"
            "a: &x |\n  lit\nb: *x"
            "a: &x\nb: *x"
            "a: &x !!str\nb: *x"
            "a: &x [1] # c\nb: *x # d"
            "a: &x # c\n  p: 1\nb: *x # d"
            "a: &x | # h\n  lit\nb: *x # d"
            "a: &x 1\nb: *x\nc: &x 2\nd: *x"
            "&a [ &a 1, *a ]"
            "a: &x\n  # inner\n  p: 1\nb:\n  # before alias\n  *x"
            "a: &k key\n*k : v"
            "a: &k key\n? *k\n: v"
            "{a: &k key, *k : v}"
            "a: &m {p: 1}\n*m : v"
            "a: &m\n  p: 1\n? *m\n: v"
            "- &x\n  - 1\n  - 2\n- *x\n- k: *x"
            "a: !!map &x\n  p: 1\nb: *x # d"
            "a: &x !!map\n  p: 1\nb: *x"
            "a: &x |+\n  lit\n\nb: *x\n\nc: 1"
            "a: &x \"multi\n  line\"\nb: *x # d"
            "a: &x\n  - 1\nb:\n  # lead\n  *x # t\n# last"
            "a: &a x\nb: [*a, # c\n  y]"
            "? &k [a]\n: v\n? *k\n: w"
        ] do
            expectRoundTrip text
]

let private streamTests = testList "Documents and streams" [
    // yaml@2.6.1 and ruamel: [{"a":1,"b":1},{"a":2,"b":2}]. The anchors of a document end with it.
    testCase "each document of a stream has its own anchors" <| fun _ ->
        let stream = parseOk "a: &x 1\nb: *x\n---\na: &x 2\nb: *x"
        match Aliases.expandStream AliasOptions.defaults stream with
        | Ok expanded ->
            Expect.isEmpty (Validate.stream expanded) "The slot rules"
            let values = Reader.documents expanded |> List.map (fun document -> valueJson document.Root)
            Expect.equal values [ "{\"a\":1,\"b\":1}"; "{\"a\":2,\"b\":2}" ] "The values"
        | Error error -> failwithf "The expansion fails: %s" error.Message

    // yaml@2.6.1 reads the alias of the second document and fails when it builds the value (Unresolved alias). ruamel fails with a ComposerError
    // (found undefined alias). The parser rejects the text, so the tree is built by hand.
    testCase "an alias in a document does not reach the anchors of an earlier document" <| fun _ ->
        let first = YamlDocument.create (YamlNode.mapping [ YamlNode.entry (sc "a") (YamlNode.withAnchor "x" (sc "1")) ])
        let second = YamlDocument.create (YamlNode.mapping [ YamlNode.entry (sc "b") (YamlNode.alias "x") ])
        match Aliases.expandStream AliasOptions.defaults { Parts = [ StreamPart.Document first; StreamPart.Document second ] } with
        | Error error -> Expect.equal (YamlErrorCode.name error.Code) "undefined-alias" "The code"
        | Ok _ -> failwith "The second document has no anchor x"

    testCase "the document and the stream keep everything around the root" <| fun _ ->
        let text = "# top\n%YAML 1.2\n---\n# in\na: &x 1\nb: *x # d\n...\n# tail\n---\nc: 1\n"
        let stream = parseOk text
        match Aliases.expandStream AliasOptions.defaults stream with
        | Ok expanded ->
            let first = (Reader.documents stream).Head
            match Aliases.expandNode AliasOptions.defaults first.Root with
            | Ok root -> Expect.isTrue (YamlDocument.equalIgnoringSpans (Reader.documents expanded).Head { first with Root = root }) "The first document"
            | Error error -> failwithf "The expansion of the root fails: %s" error.Message
            Expect.isTrue (YamlDocument.equalIgnoringSpans (List.item 1 (Reader.documents expanded)) (List.item 1 (Reader.documents stream))) "The second document"
            Expect.equal expanded.Parts.Length stream.Parts.Length "The parts"
        | Error error -> failwithf "The expansion fails: %s" error.Message

    testCase "expandDocument returns the document of the node function" <| fun _ ->
        let document = parseDocument "---\na: &x 1\nb: *x\n"
        match Aliases.expandDocument AliasOptions.defaults document, Aliases.expandNode AliasOptions.defaults document.Root with
        | Ok expanded, Ok root ->
            Expect.nodeEqual expanded.Root root "The root"
            Expect.equal expanded.StartMarker.IsSome true "The start marker"
        | _ -> failwith "The expansion fails"
]

let private errorTests = testList "Alias errors" [
    // yaml@2.6.1 reads an alias inside its own anchored node and builds a circular value. ruamel builds a circular value for a root collection and a null for
    // the same alias inside a mapping (`a: &a [ *a ]` is {"a": [None]}). The expansion has no circular tree to return, so it reports the alias.
    testCase "an alias inside its own anchored node is an error with the alias and the node" <| fun _ ->
        let rows = [
            "&a [*a]", "recursive-alias 1:5-1:7 [1:1-1:8]"
            "a: &a\n  k: *a", "recursive-alias 2:6-2:8 [1:4-2:8]"
            "&a {k: *a}", "recursive-alias 1:8-1:10 [1:1-1:11]"
            "&a [1, [*a]]", "recursive-alias 1:9-1:11 [1:1-1:13]"
            "x: &a\n  k: &b\n    j: *a", "recursive-alias 3:8-3:10 [1:4-3:10]"
            "x: &a\n  k: &b\n    j: *b", "recursive-alias 3:8-3:10 [2:6-3:10]"
            "a: &a [ b: *a ]", "recursive-alias 1:12-1:14 [1:4-1:16]"
            "&a\nk: [*a]", "recursive-alias 2:5-2:7 [1:1-2:8]"
            "a: &x 1\nb: &x [*x]\nc: *x", "recursive-alias 2:8-2:10 [2:4-2:11]"
        ]
        for (text, expected) in rows do
            Expect.equal (describe (expansionError text)) expected (sprintf "The error of %A" text)

    testCase "the message of a recursive alias names the line" <| fun _ ->
        let error = expansionError "x: 1\ny: &a [*a]"
        Expect.equal error.Message "Line 2: The alias *a stands inside the node that its anchor names, so a copy of that node would contain the alias again." "The message"

    // yaml@2.6.1 parses an alias in front of its anchor and fails when it builds the value (Unresolved alias, the anchor must be set before the alias). ruamel fails
    // with a ComposerError (found undefined alias). The parser rejects such a text, so the tree is built by hand, or the node is a part of a tree.
    testCase "an alias without an anchor in front of it is an error" <| fun _ ->
        let built = YamlNode.mapping [ YamlNode.entry (sc "k") (YamlNode.alias "x") ]
        match Aliases.expandNode AliasOptions.defaults built with
        | Error error ->
            Expect.equal (describe error) "undefined-alias 1:1-1:1 []" "A built tree has no span, so the error points at the start of the text"
            Expect.equal error.Message "The alias *x names no anchor that stands earlier in the document." "The message names no line"
        | Ok _ -> failwith "The alias has no anchor"
        let part = (YamlNode.entries (parseRoot "a: &x 1\nb: [*x]")).[1].Value
        match Aliases.expandNode AliasOptions.defaults part with
        | Error error ->
            Expect.equal (describe error) "undefined-alias 2:5-2:7 []" "The anchor stands outside the node"
            Expect.equal (error.Message.StartsWith "Line 2: ") true "The message names the line"
        | Ok _ -> failwith "The anchor stands outside the node"
]

/// The alias lines of the chain that doubles the size of a node `fan` times in each of `depth` links, like the entity chain of the billion laughs attack.
let private laughs (depth: int) (fan: int) : string =
    let sb = StringBuilder()
    sb.Append("a0: &a0 [lol, lol, lol, lol, lol, lol, lol, lol, lol]\n") |> ignore
    for i in 1 .. depth - 1 do
        let aliases = [ for _ in 1 .. fan -> sprintf "*a%d" (i - 1) ]
        sb.Append(sprintf "a%d: &a%d [%s]\n" i i (String.concat ", " aliases)) |> ignore
    sb.ToString()

let private limitTests = testList "The limit of the copies" [
    // ruamel loads the chain of nine links with nine aliases each in a few milliseconds, because it shares one object for each anchor (10 collections for
    // 490329055 nodes). yaml@2.6.1 throws a ReferenceError (Excessive alias count indicates a resource exhaustion attack) when it builds the value. The
    // expansion adds up the sizes of the anchored nodes before it builds anything.
    testCase "a chain of anchors that multiply the nodes fails at the first alias that passes the limit" <| fun _ ->
        let error = expansionError (laughs 9 9)
        Expect.equal (YamlErrorCode.name error.Code) "alias-limit" "The code"
        Expect.equal error.Span.Start.Line 6 "The first alias of the line of a5 passes 100000 nodes"
        Expect.equal error.Related.Length 1 "The anchored node"
        Expect.equal error.Related.Head.Start.Line 5 "The node of a4"
        Expect.equal error.Message ("Line 6: The copies of the aliases would pass the limit of 100000 nodes at the alias *a4.") "The message"

    testCase "the sizes of a longer chain pass every int and the limit still holds" <| fun _ ->
        Expect.equal (YamlErrorCode.name (expansionError (laughs 40 9)).Code) "alias-limit" "The default limit"
        match Aliases.expandNode { AliasOptions.defaults with MaxCopiedNodes = Int32.MaxValue } (parseRoot (laughs 40 9)) with
        | Error error -> Expect.equal (YamlErrorCode.name error.Code) "alias-limit" "The largest limit"
        | Ok _ -> failwith "The chain holds more nodes than any limit"

    // The aliases count the nodes of the expansion of the anchored node: a collection and each node below it. `a: &x [1, 2]` holds 3 nodes.
    testCase "the limit counts the nodes that the copies hold" <| fun _ ->
        let limited (limit: int) (text: string) = Aliases.expandNode { AliasOptions.defaults with MaxCopiedNodes = limit } (parseRoot text)
        let code (result: Result<YamlNode, YamlError>) =
            match result with
            | Ok _ -> "ok"
            | Error error -> describe error
        let twice = "a: &x [1, 2]\nb: *x\nc: *x"
        Expect.equal (code (limited 6 twice)) "ok" "Two copies of 3 nodes fit 6"
        Expect.equal (code (limited 5 twice)) "alias-limit 3:4-3:6 [1:4-1:13]" "The second copy passes 5"
        Expect.equal (code (limited 3 twice)) "alias-limit 3:4-3:6 [1:4-1:13]" "The second copy passes 3"
        Expect.equal (code (limited 2 twice)) "alias-limit 2:4-2:6 [1:4-1:13]" "The first copy passes 2"
        Expect.equal (code (limited 0 twice)) "alias-limit 2:4-2:6 [1:4-1:13]" "No copy passes 0"
        Expect.equal (code (limited 0 "a: &x [1, 2]\nb: [3]")) "ok" "No alias, no copy"
        let nested = "a: &x [1]\nb: &y [*x, *x]\nc: *y"
        Expect.equal (code (limited 9 nested)) "ok" "The aliases inside y copy 4 nodes, and the copy of y holds 5"
        Expect.equal (code (limited 8 nested)) "alias-limit 3:4-3:6 [2:4-2:15]" "The copy of y passes 8"

    testCase "a limit below 0 is refused" <| fun _ ->
        Expect.notEqual (errorOf (fun () -> Aliases.expandNode { AliasOptions.defaults with MaxCopiedNodes = -1 } (sc "x") |> ignore)) "" "The exception"

    testCase "the default limit is 100000 nodes" <| fun _ ->
        Expect.equal AliasOptions.defaults.MaxCopiedNodes 100000 "The default"

    testCase "the limit applies to each document of a stream" <| fun _ ->
        let stream = parseOk "a: &x [1, 2]\nb: *x\n---\na: &x [1, 2]\nb: *x"
        match Aliases.expandStream { AliasOptions.defaults with MaxCopiedNodes = 3 } stream with
        | Ok _ -> ()
        | Error error -> failwithf "Each document copies 3 nodes: %s" error.Message
        match Aliases.expandStream { AliasOptions.defaults with MaxCopiedNodes = 2 } stream with
        | Error error -> Expect.equal (YamlErrorCode.name error.Code) "alias-limit" "The first document passes 2"
        | Ok _ -> failwith "Each document copies 3 nodes"
]

let private depthTests = testList "Deep trees" [
    // The walk keeps its stack on the heap, so a tree nested 10000 levels deep expands on every target.
    testCase "a tree nested 10000 levels deep expands" <| fun _ ->
        let depth = 10000
        let text = "- &a x\n- " + String.replicate depth "[" + "*a" + String.replicate depth "]"
        let root = expandedRoot text
        let mutable current = (YamlNode.items root).[1].Node
        for _ in 1 .. depth - 1 do
            current <- (YamlNode.items current).Head.Node
        let copy = (YamlNode.items current).Head.Node
        Expect.equal (YamlNode.tryScalar copy |> Option.map (fun scalar -> scalar.Value)) (Some "x") "The copy at the bottom"
        Expect.equal copy.Properties.Anchor (Some "a") "The anchor of the copy"
]

// The input of the scaling test.

/// An anchored scalar with aliases to it as values and in flow sequences.
let private scalarAliasInput (count: int) =
    "s: &s text\nn: &n 12\n"
    + units count (fun i ->
        match i % 3 with
        | 0 -> sprintf "k%d: *s\n" i
        | 1 -> sprintf "m%d: *n\n" i
        | _ -> sprintf "l%d: [*s, *n, *s]\n" i)

/// Anchored collections with aliases to them as values and as items, some with a comment behind the alias.
let private collectionAliasInput (count: int) =
    "m: &m\n  a: 1\n  b: [x, y]\nq: &q\n- 1\n- two\nr: &r {u: 1, v: [2, 3]}\n"
    + units count (fun i ->
        match i % 6 with
        | 0 -> sprintf "k%d: *m\n" i
        | 1 -> sprintf "l%d: *q # note %d\n" i i
        | 2 -> sprintf "n%d: *r\n" i
        | 3 -> sprintf "o%d: *m # note %d\n" i i
        | 4 -> sprintf "p%d: *q\n" i
        | _ -> sprintf "u%d:\n  - *m\n  - *r\n" i)

/// Anchors that hold aliases, and aliases to those anchors, in three links.
let private chainAliasInput (count: int) =
    "base: &base [a, b]\n"
    + units count (fun i ->
        sprintf "c%d: &c%d [*base, *base]\nd%d: &d%d {x: *c%d, y: [*c%d, *base]}\ne%d: [*d%d, *d%d]\n" i i i i i i i i i)

/// A flow sequence with an alias for each item on one line, and a block sequence with an alias for each line.
let private aliasRunInput (items: int) =
    "base: &base x\nall: [" + String.concat ", " [ for _ in 1 .. items -> "*base" ] + "]\nlist:\n" + String.replicate items "- *base\n"

/// Documents with an anchor and the aliases to it.
let private aliasDocumentsInput (count: int) =
    String.concat "---\n" [ for i in 0 .. count - 1 -> sprintf "a: &x [%d, 2]\nb: *x\nc: {d: *x}\n" i ]

/// Anchored block scalars with aliases to them: a scalar that keeps its empty lines, which the copy writes in double quotes, and scalars with a comment behind the alias or an
/// alias in a flow sequence.
let private blockScalarAliasInput (count: int) =
    units count (fun i ->
        match i % 3 with
        | 0 -> sprintf "s%d: &s%d |+\n  text %d\n\nt%d: *s%d # note\n" i i i i i
        | 1 -> sprintf "u%d: &u%d |\n  x\nv%d: *u%d # note\n" i i i i
        | _ -> sprintf "w%d: &w%d >-\n  folded %d\nz%d: [*w%d]\n" i i i i i)

/// The stream that the parser reads from an input of the scaling test, made outside the timed runs so that the timing measures the expansion.
let private expandedStreams = System.Collections.Generic.Dictionary<string, YamlStream>()

/// The input of the scaling test, one document for each part. The share of a part is in ten-thousandths of the lines. A part that is cheap for the expansion has a large share,
/// so that each of the eight parts takes about an eighth of the time, and a quadratic cost in one kind of alias moves the ratio of the whole run. The last part holds no alias,
/// and the expansion only walks its nodes.
let private mixedInput (lines: int) =
    let part = partOf lines
    let text =
        documentsOf [
            // an anchored scalar
            scalarAliasInput (part 1450)
            // anchored collections
            collectionAliasInput (part 1740)
            // anchors in flow collections and anchors that a later anchor replaces
            aliasEntriesInput (part 779)
            // chains of anchors that hold aliases
            chainAliasInput (part 213)
            // runs of aliases
            aliasRunInput (part 1860)
            // documents with an anchor
            aliasDocumentsInput (part 378)
            // anchored block scalars
            blockScalarAliasInput (part 899)
            // no alias
            entriesInput (part 1870)
        ]
    expandedStreams.[text] <-
        match Parser.parseStream text with
        | Ok stream -> stream
        | Error error -> failwithf "The parser fails: %s" error.Message
    text

let private expandOnly (text: string) =
    match Aliases.expandStream { AliasOptions.defaults with MaxCopiedNodes = Int32.MaxValue } expandedStreams.[text] with
    | Ok _ -> ()
    | Error error -> failwithf "The expansion fails: %s" error.Message

let private performanceTests = testList "Alias expansion performance" [
    testCase "the expansion of a mixed input takes time linear in its length" <| fun _ ->
        expectLinear "The expansion on a mixed input" expandOnly mixedInput
        expandedStreams.Clear()
]

let Main =
    testList "Alias expansion" [
        valueTests
        commentTests
        styleTests
        roundTripTests
        streamTests
        errorTests
        limitTests
        depthTests
        performanceTests
    ]
