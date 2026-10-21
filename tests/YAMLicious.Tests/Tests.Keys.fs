module Tests.Keys

// The tests of keys that are full nodes: flow collections, quoted scalars, aliases, nodes with properties, and explicit keys that hold block collections or
// block scalars. The reader before 2.0.0 kept scalar keys only and rejected the others. The model keeps every key as a node, so the old tests that expected a
// rejection expect a tree here. yaml@2.6.1 builds a string from a collection key such as "[ a ]" and reads an alias key as its value, so its value stands in
// the comments only where it helps. The expected trees come from the model and from the productions [142] to [154] (flow entries) and [189] to [194]
// (block entries).

open Fable.Pyxpecto
open YAMLicious

let private expectError (input: string) (expected: string) =
    Expect.equal (parseErrorOf input) expected (sprintf "The error of %A" input)

let private strTag = YamlTag.core "str"

let private flowKeyTests = testList "Flow collections as keys" [
    // Earlier test "a flow collection as the key of an entry is an error that names the line". The reader before 2.0.0 rejected `[a]: b` and `{a: 1}: b`. A flow collection
    // that stands on one line is an implicit key now, with the one-lexeme peek of a quoted scalar and the limits of [154].
    testCase "a flow collection on one line is an implicit key" <| fun _ ->
        expectRoot "[a, b]: c" (YamlNode.mapping [ YamlNode.entry (flowSeq [ sc "a"; sc "b" ]) (sc "c") ])
        expectRoot "{a: 1}: c" (YamlNode.mapping [ YamlNode.entry (flowMap [ "a", sc "1" ]) (sc "c") ])
        expectRoot "[a]: b\n{c: d}: e" (YamlNode.mapping [ YamlNode.entry (flowSeq [ sc "a" ]) (sc "b"); YamlNode.entry (flowMap [ "c", sc "d" ]) (sc "e") ])
        expectRoot "- [a]: b" (seqOf [ mapOfNodes [ flowSeq [ sc "a" ], sc "b" ] ])
        expectRoot "k:\n  [a]: b" (mapOf [ "k", mapOfNodes [ flowSeq [ sc "a" ], sc "b" ] ])
        expectRoot "[]: b" (YamlNode.mapping [ YamlNode.entry (YamlNode.flowSequence []) (sc "b") ])
        expectRoot "[a]:\n  - x" (YamlNode.mapping [ { YamlNode.entry (flowSeq [ sc "a" ]) (seqOf [ sc "x" ]) with Colon = Some(newLine None []) } ])
        expectRoot "[a]: # c\n  b" (YamlNode.mapping [ { YamlNode.entry (flowSeq [ sc "a" ]) (sc "b") with Colon = Some(newLine (Some " c") []) } ])
        // The data projection names the key node.
        let data = Projection.data (parseOk "[a, b]: c")
        Expect.stringContains data "\"c\"" "The value"

    // A comment behind the key goes to the Before of the next entry, and the key itself has no trailing comment, because a comment behind the colon belongs
    // to the colon (rule R1).
    testCase "a key that is a flow collection has no comment of its own" <| fun _ ->
        let root = parseRoot "[a]: b # c"
        Expect.nodeEqual root (YamlNode.mapping [ YamlNode.entry (flowSeq [ sc "a" ]) (withNote " c" (sc "b")) ]) "The comment belongs to the value"

    // Earlier test "a flow collection as the key of an entry is an error" (the cases `{? [a] : b}` and `k: [\n  ? {a: 1}\n]`) and Tests.YamlRead "Flow collections over
    // several lines as explicit keys and values". yaml@2.6.1 builds the key "[ a ]" from the collection.
    testCase "a flow collection is an explicit key" <| fun _ ->
        expectRoot "{? [a] : b}" (YamlNode.flowMapping [ explicitFlowEntry (flowSeq [ sc "a" ]) (sc "b") ])
        expectRoot "k: [\n  ? {a: 1}\n]" (mapOf [ "k", flowSeq [ YamlNode.flowMapping [ { explicitFlowEntry (flowMap [ "a", sc "1" ]) nothing with Colon = None } ] ] ])
        expectRoot "? [\n  a\n]\n: b" (YamlNode.mapping [ explicitEntry (flowSeq [ sc "a" ]) (sc "b") ])
        expectRoot "? a\n: [\n  b\n]" (YamlNode.mapping [ explicitEntry (sc "a") (flowSeq [ sc "b" ]) ])
        expectRoot "? [\n  a\n]\n: [\n  b\n]" (YamlNode.mapping [ explicitEntry (flowSeq [ sc "a" ]) (flowSeq [ sc "b" ]) ])

    // The cases N1-F06 to N1-F10. yaml@2.6.1 reads the key as the text of the collection, such as "[ a ]". The reader before 2.0.0 rejected them, and the
    // model keeps the collection as the key node.
    testCase "the node end cases with a flow collection as an explicit key are trees" <| fun _ ->
        expectRoot "? [a]\n: v" (YamlNode.mapping [ explicitEntry (flowSeq [ sc "a" ]) (sc "v") ])
        expectRoot "? {a: 1}\n: v" (YamlNode.mapping [ explicitEntry (flowMap [ "a", sc "1" ]) (sc "v") ])
        expectRoot "?\n  [a]\n: v" (YamlNode.mapping [ { explicitEntry (flowSeq [ sc "a" ]) (sc "v") with Question = Some(newLine None []) } ])
        expectRoot "- ? [a]\n  : b" (seqOf [ YamlNode.mapping [ explicitEntry (flowSeq [ sc "a" ]) (sc "b") ] ])
        expectRoot "? !!seq [a]\n: v" (YamlNode.mapping [ explicitEntry (YamlNode.withTag (YamlTag.core "seq") (flowSeq [ sc "a" ])) (sc "v") ])

    // Tests.AdvancedFeatures "5.1.3 Nested mapping as key". yaml@2.6.1: {"[ New York Yankees, Atlanta Braves ]":["2001-07-02","2001-08-12","2001-08-14"]}.
    testCase "spec example 5.1.3 reads a flow sequence as the key and a flow sequence as the value" <| fun _ ->
        let expected =
            YamlNode.mapping [
                explicitEntry
                    (flowSeq [ sc "New York Yankees"; sc "Atlanta Braves" ])
                    (flowSeq [ sc "2001-07-02"; sc "2001-08-12"; sc "2001-08-14" ])
            ]
        expectRoot "? [ New York Yankees,\n    Atlanta Braves ]\n: [ 2001-07-02, 2001-08-12,\n    2001-08-14 ]" expected

    // The limits of an implicit key: one line, and 1024 characters from its first character to its colon.
    testCase "a collection key has the limits of an implicit key" <| fun _ ->
        let bracketed (length: int) = "[" + String.replicate (length - 2) "k" + "]"
        Expect.equal (YamlNode.entries (parseRoot (bracketed 1024 + ": v"))).Length 1 "1024 characters"
        expectError (bracketed 1025 + ": v") "implicit-key-length@1"
        expectError "[a,\n b]: c" "implicit-key-multiline@1"
        expectError "k: [a]: v" "block-collection-on-key-line@1"
        expectError "a: 1\n{[a]: b}" "missing-colon@2"
        expectError "[a]\n: b" "unexpected-content@2"
        expectError "[a]:b" "unexpected-content@1"
        // yaml@2.6.1 reads `[a]:b` as a key behind a colon without a blank. [c-l-block-map-implicit-value] needs a separator, so the production rejects it.
        expectError "{a: 1}:b" "unexpected-content@1"
]

let private quotedKeyTests = testList "Quoted scalars as keys" [
    // yaml@2.6.1: {"a":1}, {"a b":1}, {"a: b":1}, {"it's":1}.
    testCase "a quoted scalar on one line is an implicit key" <| fun _ ->
        expectRoot "\"a\": 1" (YamlNode.mapping [ YamlNode.entry (dq "a") (sc "1") ])
        expectRoot "'a b': 1" (YamlNode.mapping [ YamlNode.entry (sq "a b") (sc "1") ])
        expectRoot "\"a: b\": 1" (YamlNode.mapping [ YamlNode.entry (dq "a: b") (sc "1") ])
        expectRoot "'it''s': 1" (YamlNode.mapping [ YamlNode.entry (sq "it's") (sc "1") ])
        expectRoot "- \"a\": 1" (seqOf [ YamlNode.mapping [ YamlNode.entry (dq "a") (sc "1") ] ])
        expectRoot "{\"a\": 1}" (YamlNode.flowMapping [ YamlNode.entry (dq "a") (sc "1") ])

    testCase "a quoted key over several lines or beyond 1024 characters is an error" <| fun _ ->
        expectError "\"a\n b\": 1" "implicit-key-multiline@1"
        expectError "'a\n b': 1" "implicit-key-multiline@1"
        Expect.equal (YamlNode.entries (parseRoot ("\"" + String.replicate 1022 "k" + "\": 1"))).Length 1 "1024 characters"
        expectError ("\"" + String.replicate 1023 "k" + "\": 1") "implicit-key-length@1"
        // The key of a flow mapping entry has no limit.
        Expect.equal (YamlNode.entries (parseRoot ("{\"" + String.replicate 2000 "k" + "\": 1}"))).Length 1 "A long key in a flow mapping"
]

let private aliasKeyTests = testList "Aliases as keys" [
    // Tests.YamlRead "An alias as the key of a flow entry is an error" and the earlier test "an alias in the place of a key inside a flow collection is an error that names the line". yaml@2.6.1 reads all of these with the alias resolved,
    // for example `{*a : v}` as {"x":"v"}. The reader before 2.0.0 rejected them, because it kept scalar keys only. An alias is a node, so it is a key like any other.
    testCase "an alias is a key in a flow collection" <| fun _ ->
        let alias = YamlNode.alias "a"
        let anchored = YamlNode.withAnchor "a" (sc "x")
        let after (text: string) = (parseDocument ("- &a x\n- " + text)).Root
        Expect.nodeEqual (after "{*a : v}") (seqOf [ anchored; YamlNode.flowMapping [ YamlNode.entry alias (sc "v") ] ]) "{*a : v}"
        Expect.nodeEqual (after "[*a : v]") (seqOf [ anchored; flowSeq [ flowPair alias (sc "v") ] ]) "[*a : v]"
        Expect.nodeEqual (after "{? *a : v}") (seqOf [ anchored; YamlNode.flowMapping [ explicitFlowEntry alias (sc "v") ] ]) "{? *a : v}"
        Expect.nodeEqual
            (after "{? *a}")
            (seqOf [ anchored; YamlNode.flowMapping [ { explicitFlowEntry alias nothing with Colon = None } ] ])
            "{? *a}"
        Expect.nodeEqual
            (after "[? *a]")
            (seqOf [ anchored; flowSeq [ YamlNode.flowMapping [ { explicitFlowEntry alias nothing with Colon = None } ] ] ])
            "[? *a]"
        Expect.nodeEqual (after "{k: 1, *a}") (seqOf [ anchored; YamlNode.flowMapping [ YamlNode.entry (sc "k") (sc "1"); keyOnly alias ] ]) "{k: 1, *a}"

    // The name of an alias runs to a blank, a line break or a flow indicator ([103] and [104] ns-anchor-char), so `:` is part of it. `*a: c` names the alias "a:",
    // and the alias key of an entry needs a blank in front of its colon.
    testCase "a colon is part of the name of an alias" <| fun _ ->
        expectError "- &a x\n- *a: c" "undefined-alias@2"
        let colon = parseRoot "- &a: x\n- *a: "
        Expect.nodeEqual colon (seqOf [ YamlNode.withAnchor "a:" (sc "x"); YamlNode.alias "a:" ]) "The anchor and the alias are named a:"
        let key = parseRoot "- &a x\n- *a : c"
        Expect.nodeEqual key (seqOf [ YamlNode.withAnchor "a" (sc "x"); mapOfNodes [ YamlNode.alias "a", sc "c" ] ]) "A blank in front of the colon ends the name"
        let flow = parseRoot "- &a x\n- [*a, *a ]"
        Expect.nodeEqual flow (seqOf [ YamlNode.withAnchor "a" (sc "x"); flowSeq [ YamlNode.alias "a"; YamlNode.alias "a" ] ]) "A flow indicator ends the name"
]

let private propertyKeyTests = testList "Keys with properties" [
    // yaml@2.6.1: {"a":"c"}. The anchor and the tag belong to the key. The layout names the properties in the order of the text.
    testCase "a key can carry a tag and an anchor" <| fun _ ->
        let key = YamlNode.withTag strTag (YamlNode.withAnchor "k" (sc "a"))
        let keyLayout = withLayout [ placement PropertyName.Anchor Separation.Inline; placement PropertyName.Tag Separation.Inline ] key
        expectRoot "&k !!str a: c" (YamlNode.mapping [ YamlNode.entry keyLayout (sc "c") ])
        expectRoot "!!str a: c" (YamlNode.mapping [ YamlNode.entry (YamlNode.withTag strTag (sc "a")) (sc "c") ])
        expectRoot "&k \"a\": c" (YamlNode.mapping [ YamlNode.entry (YamlNode.withAnchor "k" (dq "a")) (sc "c") ])
        expectRoot "- !!str a: c" (seqOf [ YamlNode.mapping [ YamlNode.entry (YamlNode.withTag strTag (sc "a")) (sc "c") ] ])
        expectRoot "{&k !!str a: c}" (YamlNode.flowMapping [ YamlNode.entry keyLayout (sc "c") ])
        expectRoot "[!!str a: c]" (flowSeq [ flowPair (YamlNode.withTag strTag (sc "a")) (sc "c") ])

    // Properties of a key that has no content: yaml@2.6.1 reads `&a : v` as {"":"v"} (Tests.Parser N1-E04, N1-E05).
    testCase "a key that holds properties only is an empty node with properties" <| fun _ ->
        expectRoot "&a : v" (YamlNode.mapping [ YamlNode.entry (YamlNode.withAnchor "a" nothing) (sc "v") ])
        expectRoot "!!str : v" (YamlNode.mapping [ YamlNode.entry (YamlNode.withTag strTag nothing) (sc "v") ])
        expectRoot "{&a : v}" (YamlNode.flowMapping [ YamlNode.entry (YamlNode.withAnchor "a" nothing) (sc "v") ])

    // The properties of a flow collection in front of a colon belong to the collection, which is the key.
    testCase "properties of a collection key belong to the key" <| fun _ ->
        expectRoot "&a [x]: 1" (YamlNode.mapping [ YamlNode.entry (YamlNode.withAnchor "a" (flowSeq [ sc "x" ])) (sc "1") ])
        expectRoot "!!seq [x]: 1" (YamlNode.mapping [ YamlNode.entry (YamlNode.withTag (YamlTag.core "seq") (flowSeq [ sc "x" ])) (sc "1") ])
        expectRoot "k: &a {x: 1}" (mapOf [ "k", YamlNode.withAnchor "a" (flowMap [ "x", sc "1" ]) ])

    // Properties on a line of their own belong to the block collection behind them, in every place where a node can stand: behind a question mark, a colon, a dash and a start marker,
    // as the root and in the entries of a key. yaml@2.6.1 reads each of these with the tag or the anchor on the collection.
    testCase "properties on a block collection in every position" <| fun _ ->
        let map = YamlTag.core "map"
        let a = mapOf [ "a", sc "1" ]
        expectRoot "? !!map\n  a: 1\n: &v\n  - 1" (YamlNode.mapping [ explicitEntry (tagOnOwnLine map a) (anchorOnOwnLine "v" (seqOf [ sc "1" ])) ])
        expectRoot "- &a\n  k: v" (seqOf [ anchorOnOwnLine "a" (mapOf [ "k", sc "v" ]) ])
        expectRoot "k: &a\n- x" (mapOf [ "k", anchorOnOwnLine "a" (seqOf [ sc "x" ]) ])
        expectRoot "? &a\n- x\n: v" (YamlNode.mapping [ explicitEntry (anchorOnOwnLine "a" (seqOf [ sc "x" ])) (sc "v") ])
        expectRoot "!!seq\n- a" (tagOnOwnLine (YamlTag.core "seq") (seqOf [ sc "a" ]))
        let both = YamlNode.withAnchor "a" (YamlNode.withTag map (mapOf [ "k", sc "v" ]))
        expectRoot "&a !!map\nk: v" (withLayout [ placement PropertyName.Anchor Separation.Inline; placement PropertyName.Tag (newLine None []) ] both)
        Expect.nodeEqual (parseDocument "--- !!map\na: 1").Root (tagOnOwnLine map a) "Behind a start marker"
]

let private explicitKeyTests = testList "Explicit keys with block content" [
    // Tests.AdvancedFeatures "5.1.2 Sequence as mapping key" (spec example 5.1.2, 6.? "mapping between sequences"). yaml@2.6.1: {"[ Detroit Tigers, Chicago cubs ]":["2001-07-23"]}.
    testCase "a block sequence is an explicit key" <| fun _ ->
        let expected =
            YamlNode.mapping [ { explicitEntry (seqOf [ sc "Detroit Tigers"; sc "Chicago cubs" ]) (seqOf [ sc "2001-07-23" ]) with Colon = Some Separation.Inline } ]
        expectRoot "? - Detroit Tigers\n  - Chicago cubs\n: - 2001-07-23" expected
        expectRoot "? - a\n  - b\n: c" (YamlNode.mapping [ explicitEntry (seqOf [ sc "a"; sc "b" ]) (sc "c") ])

    testCase "a block mapping is an explicit key" <| fun _ ->
        expectRoot "? a: 1\n  b: 2\n: c" (YamlNode.mapping [ explicitEntry (mapOf [ "a", sc "1"; "b", sc "2" ]) (sc "c") ])
        expectRoot "?\n  a: 1\n  b: 2\n: v" (YamlNode.mapping [ { explicitEntry (mapOf [ "a", sc "1"; "b", sc "2" ]) (sc "v") with Question = Some(newLine None []) } ])

    // yaml@2.6.1: {"t\n":"c"}. The body of a block scalar belongs to the key, and the colon stands on its own line.
    testCase "a block scalar is an explicit key" <| fun _ ->
        expectRoot "? |\n  t\n: c" (YamlNode.mapping [ explicitEntry (lit "t\n") (sc "c") ])
        expectRoot "? >-\n  a\n  b\n: c" (YamlNode.mapping [ explicitEntry (blockScalar BlockScalarStyle.Folded ChompingMode.Strip None "a b") (sc "c") ])

    // Tests.AdvancedFeatures "An explicit value reads a flow sequence with a quoted item", "An explicit value that starts a block scalar keeps its body as written" and
    // "An explicit key on several lines and a multiline explicit value read from the document". yaml@2.6.1: {"k":"q","a":["x","y"]}, {"k":"q","a":"<c f=0/>\n"}, {"k":"q","a b":"x"}
    // and {"k":"q","a":"c d"}.
    testCase "an explicit value can be a flow collection or a block scalar" <| fun _ ->
        Expect.equal (valueOf "k: \"q\"\n? a\n: [x, \"y\"]") "{\"k\":\"q\",\"a\":[\"x\",\"y\"]}" "A flow sequence on the colon line"
        Expect.equal (valueOf "k: \"q\"\n? a\n: |\n  <c f=0/>\n") "{\"k\":\"q\",\"a\":\"<c f=0/>\\n\"}" "A block scalar"
        Expect.equal (valueOf "k: \"q\"\n? a\n  b\n: \"x\"") "{\"k\":\"q\",\"a b\":\"x\"}" "The key continues on the next line"
        Expect.equal (valueOf "k: \"q\"\n?\n  a\n  b\n: \"x\"") "{\"k\":\"q\",\"a b\":\"x\"}" "The key starts on the next line"
        Expect.equal (valueOf "k: \"q\"\n? a\n: c\n  d") "{\"k\":\"q\",\"a\":\"c d\"}" "The value continues on the next line"

    // yaml@2.6.1 rejects a block collection behind `?` on the line of a key only when it is not compact. The compact forms are valid.
    testCase "a compact collection after the question mark and a collection on the next line read the same tree apart from the separation" <| fun _ ->
        let compact = parseRoot "? - a\n: v"
        let spread = parseRoot "?\n- a\n: v"
        Expect.isTrue (YamlNode.trySequence (YamlNode.entries compact).Head.Key).IsSome "A sequence key"
        Expect.nodeNotEqual compact spread "The separation behind the question mark differs"
]

/// True when a mapping inside the node has a key that is no scalar, a collection or an alias.
let rec private hasNonScalarKey (node: YamlNode) : bool =
    let isNode (entry: MappingEntry) =
        match entry.Key.Kind with
        | YamlNodeKind.Scalar _ -> false
        | _ -> true
    match node.Kind with
    | YamlNodeKind.Mapping collection -> collection.Items |> List.exists (fun entry -> isNode entry || hasNonScalarKey entry.Key || hasNonScalarKey entry.Value)
    | YamlNodeKind.Sequence collection -> collection.Items |> List.exists (fun item -> hasNonScalarKey item.Node)
    | _ -> false

/// The inputs of the old tests that the reader before 2.0.0 rejected because it kept scalar keys only. yaml@2.6.1 reads each of them with a string for the collection or the value of the alias as the
/// key. The first column names the old test.
let private oldKeyRows : (string * string) list = [
    "Tokenizer a colon inside a word, a quoted scalar or a comment, and a compact mapping are no error", "? a: b"
    "Tokenizer a colon inside a word, a quoted scalar or a comment, and a compact mapping are no error", "? 'a': b"
    "Tokenizer an alias in the place of a key inside a flow collection is an error that names the line", "- &a x\n- [? *a : v]"
    "Tokenizer an alias in the place of a key inside a flow collection is an error that names the line", "- &a x\n- {k: 1,\n  *a : v}"
    "Tokenizer an alias in the place of a key inside a flow collection is an error that names the line", "- &a x\n- { # c\n  *a : v}"
    "Tokenizer a flow collection as the key of an entry is an error that names the line", "[a]: b"
    "Tokenizer a flow collection as the key of an entry is an error that names the line", "{a: 1}: b"
    "Tokenizer a flow collection as the key of an entry is an error that names the line", "k: [[a]: b]"
    "Tokenizer a flow collection as the key of an entry is an error that names the line", "{? [a] : b}"
    "Tokenizer a flow collection as the key of an entry is an error that names the line", "k: [\n  ? {a: 1}\n]"
    "Tokenizer a block collection cannot start on the line of its key", "? - a"
    "YamlRead An alias as the key of a flow entry is an error", "x: &a:b 1\nk: {*a:b : v}"
]

let private oldInputTests = testList "Inputs of the old tests" [
    testCase "the collection and alias keys of the old rejection lists are accepted" <| fun _ ->
        for (reference, input) in oldKeyRows do
            Expect.isTrue (hasNonScalarKey (parseRoot input)) (sprintf "%s: %A has a key that is no scalar" reference input)

    // Tests.AdvancedFeatures "An explicit value reads a flow sequence with a quoted item". The old test compared the tree with the styles of the scalars. yaml@2.6.1: {"k":"q","a":["x","y"]}.
    testCase "an explicit value that is a flow sequence keeps the quoted style of its items" <| fun _ ->
        expectRoot
            "k: \"q\"\n? a\n: [x, \"y\"]"
            (YamlNode.mapping [ YamlNode.entry (sc "k") (dq "q"); explicitEntry (sc "a") (flowSeq [ sc "x"; dq "y" ]) ])
]

let Main =
    testList "Keys" [
        flowKeyTests
        quotedKeyTests
        aliasKeyTests
        propertyKeyTests
        explicitKeyTests
        oldInputTests
    ]
