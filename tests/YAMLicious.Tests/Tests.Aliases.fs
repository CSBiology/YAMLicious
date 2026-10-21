module Tests.Aliases

// The tests of anchors and aliases: where an alias may stand, which anchor it names, the names, the rules for redefinition and self reference, and the text
// that may not follow an alias. The tree keeps an alias as a node with the name of its anchor and never resolves it. An alias names an anchor that stands
// earlier in the same document. yaml@2.6.1 reads an alias to an unknown anchor without an error and fails when it builds the value, so its value of such an
// input is an exception, and the tests of the errors cite the production instead.

open Fable.Pyxpecto
open YAMLicious

let private expectError (input: string) (expected: string) =
    Expect.equal (parseErrorOf input) expected (sprintf "The error of %A" input)

/// The text that stands in front of the aliases of the tests: an anchored scalar on the first line.
let private anchored = "s: &a x\n"

let private placeTests = testList "Where an alias stands" [
    // Tests.YamlRead "An alias inside a flow collection is an alias". yaml@2.6.1: {"k":["x","x",{"k":"x"}]} with the anchors resolved.
    testCase "an alias is a node in a flow collection" <| fun _ ->
        expectRoot
            "k: [&a x, *a, {k: *a}]"
            (mapOf [ "k", flowSeq [ YamlNode.withAnchor "a" (sc "x"); YamlNode.alias "a"; flowMap [ "k", YamlNode.alias "a" ] ] ])
        Expect.equal (valueOf "k: [&a x, *a, {k: *a}]") "{\"k\":[\"x\",\"x\",{\"k\":\"x\"}]}" "The value with the alias resolved"

    // Earlier test "an alias may be followed by blanks, a comment, a comma, a closer or the end". yaml@2.6.1 reads these inputs after the anchor `s: &a x` with
    // the alias resolved.
    testCase "an alias can be followed by blanks, a comment, a comma, a closer or the end" <| fun _ ->
        let alias = YamlNode.alias "a"
        let anchoredEntry = YamlNode.entry (sc "s") (YamlNode.withAnchor "a" (sc "x"))
        let after (text: string) = parseRoot (anchored + text)
        Expect.nodeEqual (after "k: *a") (YamlNode.mapping [ anchoredEntry; YamlNode.entry (sc "k") alias ]) "At the end"
        Expect.nodeEqual (after "k: *a # c") (YamlNode.mapping [ anchoredEntry; YamlNode.entry (sc "k") (withNote " c" alias) ]) "A comment"
        Expect.nodeEqual (after "k: *a   ") (YamlNode.mapping [ anchoredEntry; YamlNode.entry (sc "k") alias ]) "Blanks"
        Expect.nodeEqual (after "k: *a\t") (YamlNode.mapping [ anchoredEntry; YamlNode.entry (sc "k") alias ]) "A tab"
        Expect.nodeEqual (after "l: [*a]") (YamlNode.mapping [ anchoredEntry; YamlNode.entry (sc "l") (flowSeq [ alias ]) ]) "A closer"
        Expect.nodeEqual (after "l: [*a,b]") (YamlNode.mapping [ anchoredEntry; YamlNode.entry (sc "l") (flowSeq [ alias; sc "b" ]) ]) "A comma"
        Expect.nodeEqual (after "l: [*a ,b]") (YamlNode.mapping [ anchoredEntry; YamlNode.entry (sc "l") (flowSeq [ alias; sc "b" ]) ]) "A blank and a comma"
        Expect.nodeEqual (after "l: [*a # c\n ]") (YamlNode.mapping [ anchoredEntry; YamlNode.entry (sc "l") (flowSeq [ withNote " c" alias ]) ]) "A comment and a closer"
        Expect.nodeEqual (after "l: {k: *a, j: *a }") (YamlNode.mapping [ anchoredEntry; YamlNode.entry (sc "l") (flowMap [ "k", alias; "j", alias ]) ]) "In a mapping"

    testCase "an alias is an item, a value and a key of the block structure" <| fun _ ->
        let alias = YamlNode.alias "a"
        let anchoredNode = YamlNode.withAnchor "a" (sc "x")
        expectRoot "- &a x\n- *a" (seqOf [ anchoredNode; alias ])
        expectRoot "k: &a x\nj: *a" (mapOf [ "k", anchoredNode; "j", alias ])
        expectRoot "k:\n  &a x\nj: *a" (YamlNode.mapping [ entryOnNewLine "k" anchoredNode; YamlNode.entry (sc "j") alias ])
        expectRoot "- &a x\n- *a : v" (seqOf [ anchoredNode; mapOfNodes [ alias, sc "v" ] ])
        expectRoot "- &a x\n- ? *a\n  : *a" (seqOf [ anchoredNode; YamlNode.mapping [ explicitEntry alias alias ] ])

    // Earlier test "an alias may be followed by...": the name of an alias ends at a blank, a line break or a flow indicator and may hold quotes, hashes and
    // colons, so `k: *a'q'` and `k: *a:b` name other anchors. yaml@2.6.1 reads the anchor `a'q'` as unresolved in the first of them.
    testCase "the name of an alias ends at a blank or a flow indicator and may hold other characters" <| fun _ ->
        let definitions = "defs: [&a one v, &b two v, &a:b three v, &a'q' four v, &a#c five v, &c1 six v]\n"
        Expect.nodeEqual (parseRoot (definitions + "k: *a:b") |> YamlNode.entries |> List.last).Value (YamlNode.alias "a:b") "A colon in the name"
        Expect.nodeEqual (parseRoot (definitions + "k: *a'q'") |> YamlNode.entries |> List.last).Value (YamlNode.alias "a'q'") "Quotes in the name"
        Expect.nodeEqual (parseRoot (definitions + "k: *a#c") |> YamlNode.entries |> List.last).Value (YamlNode.alias "a#c") "A hash in the name"
        Expect.nodeEqual (parseRoot (definitions + "l: [*a:b]") |> YamlNode.entries |> List.last).Value (flowSeq [ YamlNode.alias "a:b" ]) "A colon in a flow collection"
        expectError (anchored + "k: *a:b") "undefined-alias@2"
        expectError (anchored + "k: *a'q'") "undefined-alias@2"
]

let private definitionTests = testList "Anchors and the names that they define" [
    // An anchor can be defined again. The later definition wins for the aliases behind it. yaml@2.6.1: ["x","x","y","y"].
    testCase "a second definition of an anchor serves the aliases behind it" <| fun _ ->
        let expected = seqOf [ YamlNode.withAnchor "a" (sc "x"); YamlNode.alias "a"; YamlNode.withAnchor "a" (sc "y"); YamlNode.alias "a" ]
        expectRoot "- &a x\n- *a\n- &a y\n- *a" expected
        Expect.equal (valueOf "- &a x\n- *a\n- &a y\n- *a") "[\"x\",\"x\",\"y\",\"y\"]" "The value"

    // The anchor of a collection is defined before its children are read, so a collection may contain an alias of itself (the invariant on YamlNode). yaml@2.6.1
    // builds a circular value from such an input, which JSON cannot print.
    testCase "a collection can contain an alias of itself" <| fun _ ->
        expectRoot "&a [*a]" (YamlNode.withAnchor "a" (flowSeq [ YamlNode.alias "a" ]))
        expectRoot "&m {k: *m}" (YamlNode.withAnchor "m" (flowMap [ "k", YamlNode.alias "m" ]))
        expectRoot "&a [x, *a]" (YamlNode.withAnchor "a" (flowSeq [ sc "x"; YamlNode.alias "a" ]))
        expectRoot "- &a\n  - *a" (seqOf [ anchorOnOwnLine "a" (seqOf [ YamlNode.alias "a" ]) ])
        expectRoot "k: &m\n  j: *m" (mapOf [ "k", anchorOnOwnLine "m" (mapOf [ "j", YamlNode.alias "m" ]) ])
        expectRoot "&a [&a x, *a]" (YamlNode.withAnchor "a" (flowSeq [ YamlNode.withAnchor "a" (sc "x"); YamlNode.alias "a" ]))

    // An anchor belongs to its document. A `---` ends the definitions of the document in front of it.
    testCase "the anchors of a document end with the document" <| fun _ ->
        expectError "- &a x\n---\n- *a" "undefined-alias@3"
        expectError "- &a x\n...\n- *a" "undefined-alias@3"
        let stream = parseOk "- &a x\n- *a\n---\n- &a y\n- *a"
        Expect.equal (stream.Parts |> List.length) 2 "Two documents"
]

let private errorTests = testList "Alias errors" [
    // YAML 1.2.2 [104] c-ns-alias-node and 7.1: an alias names a node that was anchored earlier. yaml@2.6.1 reads the alias and fails when it builds the value.
    testCase "an alias to an unknown anchor is an error with the alias span and no related span" <| fun _ ->
        expectError "*a" "undefined-alias@1"
        expectError "- *a" "undefined-alias@1"
        expectError "- *a\n- &a x" "undefined-alias@1"
        expectError "k: [*a]" "undefined-alias@1"
        expectError "{*a : v}" "undefined-alias@1"
        expectError "k: *a:" "undefined-alias@1"
        expectError "- &a x\n- *a: c" "undefined-alias@2"
        let error = parseError "- *b"
        Expect.equal (showSpan error.Span) "1:3-1:5" "The span of the alias"
        Expect.isEmpty error.Related "No related span"
        Expect.equal (YamlErrorCode.name error.Code) "undefined-alias" "The code"

    // Tests.YamlRead "An alias inside a flow collection is an alias" (the last check) and "Text behind an alias and an alias as the key of a flow entry are errors".
    // An alias has no properties.
    testCase "an alias takes no properties" <| fun _ ->
        expectError (anchored + "k: !!str *a") "alias-with-properties@2"
        expectError (anchored + "k: &b *a") "alias-with-properties@2"
        expectError (anchored + "k: [&b *a]") "alias-with-properties@2"
        expectError (anchored + "&b *a : v") "alias-with-properties@2"
        expectError (anchored + "k: {&y *a}") "alias-with-properties@2"
        let error = parseError (anchored + "k: !!str *a")
        Expect.equal (error.Related |> List.map showSpan) [ "2:10-2:12" ] "The alias is the related span of the properties"

    // Earlier test "text behind an alias is an error that names the line". yaml@2.6.1 rejects all nine inputs with UNEXPECTED_TOKEN or MISSING_CHAR when the
    // anchor `s: &a x` stands in front of them. An alias ends its node. Outside a flow collection a comma cannot start a plain scalar, so `k: *a,b` is an error of the lexer.
    testCase "text behind an alias is an error" <| fun _ ->
        expectError (anchored + "k: *a b") "unexpected-content@2"
        expectError (anchored + "k: *a,b") "reserved-indicator@2"
        expectError (anchored + "l:\n- *a b") "unexpected-content@3"
        expectError (anchored + "*a b: c") "missing-colon@2"
        expectError (anchored + "l: [*a b]") "missing-flow-comma@2"
        expectError (anchored + "l: {k: *a b}") "missing-flow-comma@2"
        expectError (anchored + "l: [*a\n b]") "missing-flow-comma@3"
        expectError (anchored + "l: [*a \"q\"]") "missing-flow-comma@2"
        expectError (anchored + "l: [*a [b]]") "missing-flow-comma@2"
]

// The inputs of the scaling tests. The reference is a single pass over the characters.

/// A line for each entry, with an anchor, an alias to an earlier anchor and an alias to the anchor of the same line.
let private aliasLinesInput (lines: int) =
    let sb = System.Text.StringBuilder()
    sb.Append("base: &base {a: 1}\n") |> ignore
    for i in 0 .. lines - 1 do
        sb.Append(sprintf "k%d: [*base, &x%d v%d, *x%d]\n" i i i i) |> ignore
    sb.ToString()

/// One flow sequence with an alias for each item.
let private aliasItemsInput (lines: int) =
    let sb = System.Text.StringBuilder()
    sb.Append("base: &base x\nall: [") |> ignore
    for i in 0 .. lines - 1 do
        if i > 0 then sb.Append(", ") |> ignore
        sb.Append("*base") |> ignore
    sb.Append("]\n") |> ignore
    sb.ToString()

/// Many redefinitions of one anchor, each followed by an alias.
let private redefinitionInput (lines: int) =
    let sb = System.Text.StringBuilder()
    for i in 0 .. lines - 1 do
        sb.Append(sprintf "- &a v%d\n- *a\n" i) |> ignore
    sb.ToString()

let private parseOnly (input: string) = Parser.parseStream input |> ignore

let private performanceTests = testList "Alias performance" [
    testCase "anchors and aliases in many flow collections take time linear in the length of the input" <| fun _ ->
        expectLinear "The parser on anchors and aliases in flow collections" parseOnly countLineBreaks aliasLinesInput

    testCase "a flow sequence of many aliases takes time linear in the number of items" <| fun _ ->
        expectLinear "The parser on a flow sequence of aliases" parseOnly countLineBreaks aliasItemsInput

    testCase "many redefinitions of one anchor take time linear in their number" <| fun _ ->
        expectLinear "The parser on redefined anchors" parseOnly countLineBreaks redefinitionInput
]

let Main =
    testList "Aliases" [
        placeTests
        definitionTests
        errorTests
        performanceTests
    ]
