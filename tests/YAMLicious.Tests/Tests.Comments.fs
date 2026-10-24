module Tests.Comments

// The slots of comments and blank lines in a parsed block tree. One test for each rule of the model: R1 to R5, R7 and R8
// as Model.fs words them, and the three rulings 1a to 1c that complete them. Rule R6, the flow parts of R1, R2, R7 and R8 and the rules 20
// to 22 are about flow collections and stand at the end of the file. Rule R3 and ruling 1c are about documents and stand in Tests.Documents. The tests of this file give the input
// text, the expected tree and, through parseOk, the check of Validate.stream on the parsed tree.
//
// The comments of the tests that come from the old tests say which one they port. The old tree put a comment between the
// entries of a list. The new tree puts it in the slot that the text decides: the Before of the entry that follows it, the
// End of the collection that it closes, or the separation of the indicator that it follows.

open Fable.Pyxpecto
open YAMLicious

let private r1Tests = testList "R1 a comment behind an indicator without content" [
    // yaml@2.6.1 reads these as {"k":"v"}, ["v"], {"a":"b"} and {"k":{"a":1}}. The comment stands on the line of the indicator and the content
    // starts on a later line, so the separation of the indicator is a line break that holds the comment.
    testCase "a comment behind a colon, a dash or a question mark goes to the separation of the indicator" <| fun _ ->
        expectRoot "k: # c\n  v" (YamlNode.mapping [ { YamlNode.entry (sc "k") (sc "v") with Colon = Some(newLine (Some " c") []) } ])
        expectRoot "- # c\n  v" (YamlNode.sequence [ { YamlNode.item (sc "v") with Dash = Some(newLine (Some " c") []) } ])
        expectRoot
            "? # c\n  a\n: b"
            (YamlNode.mapping [ { explicitEntry (sc "a") (sc "b") with Question = Some(newLine (Some " c") []) } ])
        expectRoot "k: # c\n  a: 1" (YamlNode.mapping [ { YamlNode.entry (sc "k") (mapOf [ "a", sc "1" ]) with Colon = Some(newLine (Some " c") []) } ])
        expectRoot
            "? a\n: # c\n  b"
            (YamlNode.mapping [ { explicitEntry (sc "a") (sc "b") with Colon = Some(newLine (Some " c") []) } ])

    // yaml@2.6.1 reads `k: # c` as {"k":null}. A missing value has no trailing comment, so the comment stays with the colon, the dash or the question mark.
    testCase "a comment behind an indicator with a missing value goes to the separation too" <| fun _ ->
        expectRoot "k: # c" (YamlNode.mapping [ { YamlNode.entry (sc "k") nothing with Colon = Some(newLine (Some " c") []) } ])
        expectRoot "- # c" (YamlNode.sequence [ { YamlNode.item nothing with Dash = Some(newLine (Some " c") []) } ])
        expectRoot "? a\n: # c" (YamlNode.mapping [ { explicitEntry (sc "a") nothing with Colon = Some(newLine (Some " c") []) } ])
        expectRoot "? # c" (YamlNode.mapping [ { explicitKeyOnly nothing with Question = Some(newLine (Some " c") []) } ])

    // yaml@2.6.1 reads these as {"k":"v"} and {"k":{"a":1}}. The comment behind a property puts the next token on a later line, so the separation of the
    // property holds it and the separation behind the colon stays on the line.
    testCase "a comment behind a property goes to the separation of that property" <| fun _ ->
        let anchored = withLayout [ placement PropertyName.Anchor (newLine (Some " c") []) ] (YamlNode.withAnchor "a" (sc "v"))
        expectRoot "k: &a # c\n  v" (mapOf [ "k", anchored ])
        let tagged = withLayout [ placement PropertyName.Tag (newLine (Some " c") []) ] (YamlNode.withTag (YamlTag.core "map") (mapOf [ "a", sc "1" ]))
        expectRoot "k: !!map # c\n  a: 1" (mapOf [ "k", tagged ])
        expectRoot "- &a # c\n  v" (seqOf [ anchored ])

    // Tests.YamlRead "Commented-out header after a sequence dash opens no block scalar". yaml@2.6.1: [{"k":"it's"}]. The comment behind the dash is the comment
    // of the dash, and the mapping starts on the next line.
    testCase "a comment behind a dash that looks like a block scalar header is a comment" <| fun _ ->
        expectRoot
            "- # note: |\n  k: 'it''s'\n"
            (YamlNode.sequence [ { YamlNode.item (mapOf [ "k", sq "it's" ]) with Dash = Some(newLine (Some " note: |") []) } ])
]

let private r2Tests = testList "R2 trivia behind the line break of an indicator" [
    // yaml@2.6.1 reads this as {"k":"v"}. The trivia between the line of the colon and the scalar belong to the line break of the colon.
    testCase "trivia between an indicator and a scalar are the Following of the separation" <| fun _ ->
        expectRoot "k: # c\n  # d\n\n  v" (YamlNode.mapping [ { YamlNode.entry (sc "k") (sc "v") with Colon = Some(newLine (Some " c") [ note " d"; blank ]) } ])
        expectRoot "k:\n  # d\n  v" (YamlNode.mapping [ { YamlNode.entry (sc "k") (sc "v") with Colon = Some(newLine None [ note " d" ]) } ])
        expectRoot "-\n\n  v" (YamlNode.sequence [ { YamlNode.item (sc "v") with Dash = Some(newLine None [ blank ]) } ])
        expectRoot
            "?\n  # d\n  a\n: b"
            (YamlNode.mapping [ { explicitEntry (sc "a") (sc "b") with Question = Some(newLine None [ note " d" ]) } ])

    // yaml@2.6.1 reads each of these as {"k":{"a":1}} or {"k":[1]}. A block collection without a property has no token of its own on the line of the
    // indicator, so the trivia in front of it go to the Before of its first entry or item and Following stays empty.
    testCase "trivia in front of a block collection without a property go to its first entry" <| fun _ ->
        expectRoot
            "k:\n  # d\n  a: 1"
            (mapOf [ "k", YamlNode.mapping [ entryBefore [ note " d" ] (sc "a") (sc "1") ] ])
        expectRoot
            "k: # c\n  # d\n  - 1"
            (YamlNode.mapping [ { YamlNode.entry (sc "k") (YamlNode.sequence [ itemBefore [ note " d" ] (sc "1") ]) with Colon = Some(newLine (Some " c") []) } ])
        expectRoot
            "-\n  # d\n  a: 1"
            (YamlNode.sequence [ itemOnNewLine (YamlNode.mapping [ entryBefore [ note " d" ] (sc "a") (sc "1") ]) ])
        expectRoot
            "k:\n  # d\n  &a j: 1"
            (mapOf [ "k", YamlNode.mapping [ entryBefore [ note " d" ] (YamlNode.withAnchor "a" (sc "j")) (sc "1") ] ])

    // yaml@2.6.1 reads these as {"k":"v"}, {"k":{"b":1}} and "v". The trivia between a property and the content belong to the property when the content is a
    // scalar. They go to the first entry when the content is a block collection (the same holds behind the last property of a block collection).
    testCase "trivia between properties and the content" <| fun _ ->
        let scalar = withLayout [ placement PropertyName.Anchor (newLine None [ note " d" ]) ] (YamlNode.withAnchor "a" (sc "v"))
        expectRoot "k: &a\n  # d\n  v" (mapOf [ "k", scalar ])
        let collection =
            anchorOnOwnLine "a" (YamlNode.mapping [ entryBefore [ note " d" ] (sc "b") (sc "1") ])
        expectRoot "k: &a\n  # d\n  b: 1" (YamlNode.mapping [ YamlNode.entry (sc "k") collection ])
        // Between two properties the trivia belong to the first of them.
        let both = withLayout [ placement PropertyName.Anchor (newLine None [ note " d" ]); placement PropertyName.Tag Separation.Inline ] (YamlNode.withTag (YamlTag.core "str") (YamlNode.withAnchor "a" (sc "v")))
        expectRoot "&a\n# d\n!!str v" both

    // The trivia in front of the first property of a node belong to the separation of the indicator, as the trivia in front of a scalar do.
    testCase "trivia in front of the first property are the Following of the indicator" <| fun _ ->
        let anchored = YamlNode.withAnchor "a" (sc "v")
        expectRoot "k:\n  # d\n  &a v" (YamlNode.mapping [ { YamlNode.entry (sc "k") anchored with Colon = Some(newLine None [ note " d" ]) } ])
]

let private r4Tests = testList "R4 trivia in a block collection" [
    // Tests.YamlRead "Lower-indented comment between nested mapping entries stays in open block". yaml@2.6.1: {"inputs":{"a":"string","b":"string"}}.
    // A run of trivia between two nodes belongs to the Before of the entry that follows it, whatever the columns of the comments are.
    testCase "a comment between two entries goes to the Before of the second entry whatever its column" <| fun _ ->
        let expected = mapOf [ "inputs", YamlNode.mapping [ YamlNode.entry (sc "a") (sc "string"); entryBefore [ note " group comment" ] (sc "b") (sc "string") ] ]
        expectRoot "inputs:\n  a: string\n# group comment\n  b: string" expected
        expectRoot "inputs:\n  a: string\n  # group comment\n  b: string" expected
        expectRoot "inputs:\n  a: string\n      # group comment\n  b: string" expected

    // yaml@2.6.1 reads `a: 1`, a comment line, a blank line and `&x b: 2` as {"a":1,"b":2}, the same with `- a: 1` and `!!str b: 2` as [{"a":1,"b":2}], and `a: 1`, a comment
    // line and `!!str : 2` as {"a":1,"":2}. A key with properties keeps the trivia in front of its line in the Before of its entry, as a key without properties does. The
    // parseRoot behind expectRoot runs Validate.stream on each tree.
    testCase "the trivia in front of a key with properties go to the Before of its entry" <| fun _ ->
        let tagged = YamlNode.withTag (YamlTag.core "str") (sc "b")
        expectRoot
            "a: 1\n# c\n\n&x b: 2"
            (YamlNode.mapping [ YamlNode.entry (sc "a") (sc "1"); entryBefore [ note " c"; blank ] (YamlNode.withAnchor "x" (sc "b")) (sc "2") ])
        expectRoot
            "- a: 1\n  # c\n  !!str b: 2"
            (seqOf [ YamlNode.mapping [ YamlNode.entry (sc "a") (sc "1"); entryBefore [ note " c" ] tagged (sc "2") ] ])
        expectRoot
            "a: 1\n# c\n!!str : 2"
            (YamlNode.mapping [ YamlNode.entry (sc "a") (sc "1"); entryBefore [ note " c" ] (YamlNode.withTag (YamlTag.core "str") (sc "")) (sc "2") ])

    // Tests.YamlRead "Lower-indented comment before nested sequence stays in open block", "Key followed by full-line comment and unindented sequence" and
    // "Key followed by multiple full-line comments and unindented sequence". yaml@2.6.1 reads each of these as {"inputs":[{"id":"a","type":"string"}]} or
    // {"inputs":[{"id":"x"},{"id":"y"}]}.
    testCase "a comment in front of the first item of a sequence goes to that item" <| fun _ ->
        expectRoot
            "inputs:\n# group comment\n  - id: a\n    type: string"
            (mapOf [ "inputs", YamlNode.sequence [ itemBefore [ note " group comment" ] (mapOf [ "id", sc "a"; "type", sc "string" ]) ] ])
        expectRoot
            "inputs:\n# comment\n- id: x"
            (mapOf [ "inputs", YamlNode.sequence [ itemBefore [ note " comment" ] (mapOf [ "id", sc "x" ]) ] ])
        expectRoot
            "inputs:\n# first\n# second\n- id: x\n- id: y"
            (mapOf [ "inputs", YamlNode.sequence [ itemBefore [ note " first"; note " second" ] (mapOf [ "id", sc "x" ]); YamlNode.item (mapOf [ "id", sc "y" ]) ] ])

    // Tests.YamlRead "Comment between sequence items stays inside mapped sequence", "Comment between inline sequence mapping and nested fields stays in same item" and
    // "Comment under a nested key with mapping value", "with sequence value" and "Comment under a deeply nested key". yaml@2.6.1 reads them without the comments.
    testCase "a comment between two items or in front of a nested entry goes to the Before of the node behind it" <| fun _ ->
        expectRoot
            "inputs:\n- id: x\n# between\n- id: y"
            (mapOf [ "inputs", YamlNode.sequence [ YamlNode.item (mapOf [ "id", sc "x" ]); itemBefore [ note " between" ] (mapOf [ "id", sc "y" ]) ] ])
        expectRoot
            "listing:\n  - entryname: own\n    # disabled entry example\n    entry: $(inputs.rootDir)\n    writable: true"
            (mapOf [
                "listing",
                seqOf [
                    YamlNode.mapping [
                        YamlNode.entry (sc "entryname") (sc "own")
                        entryBefore [ note " disabled entry example" ] (sc "entry") (sc "$(inputs.rootDir)")
                        YamlNode.entry (sc "writable") (sc "true")
                    ]
                ]
            ])
        expectRoot "r:\n  a:\n    # c\n    b: 1" (mapOf [ "r", mapOf [ "a", YamlNode.mapping [ entryBefore [ note " c" ] (sc "b") (sc "1") ] ] ])
        expectRoot "r:\n  a:\n    # c\n    - 1" (mapOf [ "r", mapOf [ "a", YamlNode.sequence [ itemBefore [ note " c" ] (sc "1") ] ] ])
        expectRoot
            "a:\n  b:\n    c:\n      # x\n      d: 1"
            (mapOf [ "a", mapOf [ "b", mapOf [ "c", YamlNode.mapping [ entryBefore [ note " x" ] (sc "d") (sc "1") ] ] ] ])

    // Tests.YamlRead "Blank line before lower-indented comment keeps following entry in open block", "Comment before a dedented key belongs to that key",
    // "Comment after the last item of an unindented sequence belongs to the next key" and "Blank line before indented commented block does not create orphan
    // indentation". yaml@2.6.1 reads {"steps":{"first":{"run":"tool.cwl"},"second":{"run":"tool.cwl"}}}, {"a":{"b":1},"c":2}, {"a":[1],"b":2} and
    // {"steps":{"first":{"run":"tool.cwl"}},"meta":"value"}.
    testCase "the trivia before a dedented key belong to that key" <| fun _ ->
        let step = mapOf [ "run", sc "tool.cwl" ]
        expectRoot
            "steps:\n  first:\n    run: tool.cwl\n\n# separator\n  second:\n    run: tool.cwl"
            (mapOf [ "steps", YamlNode.mapping [ blockEntry (sc "first") step; entryBefore [ blank; note " separator" ] (sc "second") step ] ])
        expectRoot
            "a:\n  b: 1\n  # about c\nc: 2"
            (YamlNode.mapping [ blockEntry (sc "a") (mapOf [ "b", sc "1" ]); entryBefore [ note " about c" ] (sc "c") (sc "2") ])
        expectRoot
            "a:\n- 1\n# end\nb: 2"
            (YamlNode.mapping [ blockEntry (sc "a") (seqOf [ sc "1" ]); entryBefore [ note " end" ] (sc "b") (sc "2") ])
        expectRoot
            "steps:\n  first:\n    run: tool.cwl\n# disabled section\n\n  # disabled:\n  #   run: disabled.cwl\n# end\n\nmeta: value"
            (YamlNode.mapping [
                blockEntry (sc "steps") (mapOf [ "first", step ])
                entryBefore [ note " disabled section"; blank; note " disabled:"; note "   run: disabled.cwl"; note " end"; blank ] (sc "meta") (sc "value")
            ])

    // Tests.YamlRead "A blank line behind a key with a trailing comment opens the block of the key". yaml@2.6.1: {"a":{"b":1}}, {"x":1,"a":{"b":1},"z":2} and {"a":[1]}.
    // The comment goes to the colon (rule R1), and the blank line goes to the first entry of the block (rule R2).
    testCase "a blank line behind a key with a comment goes to the first entry of its block" <| fun _ ->
        let commented (value: YamlNode) = { YamlNode.entry (sc "a") value with Colon = Some(newLine (Some " c") []) }
        let nested = YamlNode.mapping [ entryBefore [ blank ] (sc "b") (sc "1") ]
        expectRoot "a: # c\n\n  b: 1" (YamlNode.mapping [ commented nested ])
        expectRoot "x: 1\na: # c\n\n  b: 1\nz: 2" (YamlNode.mapping [ YamlNode.entry (sc "x") (sc "1"); commented nested; YamlNode.entry (sc "z") (sc "2") ])
        expectRoot "a: # c\n\n  - 1" (YamlNode.mapping [ commented (YamlNode.sequence [ itemBefore [ blank ] (sc "1") ]) ])

    // Tests.YamlRead "Commented-out block scalar header does not protect the following lines" and "Commented-out block scalar header inside a CWL requirements block".
    // yaml@2.6.1: {"a":{"b":1,"d":2}} and {"requirements":{"DockerRequirement":{"dockerPull":"x","b":2}}}.
    testCase "a commented-out block scalar header is a comment in front of the next entry" <| fun _ ->
        expectRoot
            "a:\n# old: |\n  b: 1 # trailing\n  d: 2"
            (mapOf [ "a", YamlNode.mapping [ entryBefore [ note " old: |" ] (sc "b") (withNote " trailing" (sc "1")); YamlNode.entry (sc "d") (sc "2") ] ])
        expectRoot
            "requirements:\n#  InitialWorkDirRequirement: |\n  DockerRequirement:\n    dockerPull: x\n    # pinned version\n    b: 2"
            (mapOf [
                "requirements",
                YamlNode.mapping [
                    entryBefore [ note "  InitialWorkDirRequirement: |" ] (sc "DockerRequirement") (YamlNode.mapping [ YamlNode.entry (sc "dockerPull") (sc "x"); entryBefore [ note " pinned version" ] (sc "b") (sc "2") ])
                ]
            ])
]

let private r4EndTests = testList "R4 trivia behind the last node of a document" [
    // Tests.YamlRead "Trailing comment at the end of a nested block stays in the block", "Comment after the last item at the end of a block stays in the sequence" and
    // "Comment after the last item of a root sequence stays in the sequence". yaml@2.6.1: {"a":{"b":1}}, {"files":["a"]} and [1]. Each comment goes to the End of the
    // deepest open collection whose entries stand at a column at most the column of the comment.
    testCase "a comment at the end goes to the End of the deepest open collection that fits its column" <| fun _ ->
        expectRoot "a:\n  b: 1\n  # end" (mapOf [ "a", withEnd [ note " end" ] (mapOf [ "b", sc "1" ]) ])
        expectRoot "files:\n  - a\n  # c2" (mapOf [ "files", withEnd [ note " c2" ] (seqOf [ sc "a" ]) ])
        expectRoot "- 1\n# end" (withEnd [ note " end" ] (seqOf [ sc "1" ]))
        expectRoot "a:\n  b: 1\n  # c1\n# c2" (withEnd [ note " c2" ] (mapOf [ "a", withEnd [ note " c1" ] (mapOf [ "b", sc "1" ]) ]))
        // A comment less indented than every collection goes to the End of the root collection.
        expectRoot "  a: 1\n# c" (withEnd [ note " c" ] (mapOf [ "a", sc "1" ]))

    // Tests.YamlRead "Trailing comment between two levels stays in the outer one". yaml@2.6.1: {"a":{"b":{"c":1}}}. The comment at 6 spaces lies between the
    // levels at 4 and 8, so it goes to the End of the mapping at 4, which holds b.
    testCase "a comment between two levels goes to the outer one" <| fun _ ->
        let inner = mapOf [ "c", sc "1" ]
        expectRoot "a:\n    b:\n        c: 1\n      # note\n" (mapOf [ "a", withEnd [ note " note" ] (mapOf [ "b", inner ]) ])

    // Tests.YamlRead "Trailing comments never reopen a level that an earlier comment closed". yaml@2.6.1: {"a":{"b":1}}. The comment at 2 spaces goes to the root, and the comment
    // behind it, at 4 spaces, fits the mapping that holds b. A comment goes to no deeper End than the comment before it in the same run (rule R4), so the second comment goes
    // to the root as well, and the writer prints the comments in the order of the text. The reader before 2.0.0 put both comments in the root mapping.
    testCase "a comment at the end goes to no deeper End than the comment before it" <| fun _ ->
        expectRoot "a:\n    b: 1\n  # x\n    # y\n" (withEnd [ note " x"; note " y" ] (mapOf [ "a", mapOf [ "b", sc "1" ] ]))
        // Comments in the order of falling columns keep their own Ends, and a comment that rises again stays at the depth of the one before it.
        expectRoot "a:\n    b: 1\n    # x\n  # y\n    # z\n" (withEnd [ note " y"; note " z" ] (mapOf [ "a", withEnd [ note " x" ] (mapOf [ "b", sc "1" ]) ]))
        // A blank line goes with the comment that follows it, so it takes the target of that comment, and the blank lines at the end take the target of the last comment.
        expectRoot
            "a:\n    b: 1\n\n  # x\n\n    # y\n\n"
            (withEnd [ blank; note " x"; blank; note " y"; blank ] (mapOf [ "a", mapOf [ "b", sc "1" ] ]))

    // A blank line goes with the comment that follows it. A blank line behind the last comment goes to the same End as that comment, and a blank line without any
    // comment goes to the End of the root collection.
    testCase "a blank line at the end goes with the comment next to it" <| fun _ ->
        expectRoot "a:\n  b: 1\n\n  # c\n" (mapOf [ "a", withEnd [ blank; note " c" ] (mapOf [ "b", sc "1" ]) ])
        expectRoot "a:\n  b: 1\n  # c\n\n" (mapOf [ "a", withEnd [ note " c"; blank ] (mapOf [ "b", sc "1" ]) ])
        expectRoot "a:\n  b: 1\n\n" (withEnd [ blank ] (mapOf [ "a", mapOf [ "b", sc "1" ] ]))
        // The comment at 0 spaces goes to the root, so the comment behind it, at 2 spaces, goes there too (rule R4).
        expectRoot "a:\n  b: 1\n\n# c\n  # d\n" (withEnd [ blank; note " c"; note " d" ] (mapOf [ "a", mapOf [ "b", sc "1" ] ]))
        expectRoot "a:\n  b: 1\n  # d\n\n# c\n" (withEnd [ blank; note " c" ] (mapOf [ "a", withEnd [ note " d" ] (mapOf [ "b", sc "1" ]) ]))

    // An indentless sequence is an open collection at the column of its key, so a comment at that column goes to its End.
    testCase "a comment behind an indentless sequence goes to the End of the sequence" <| fun _ ->
        expectRoot "k:\n- a\n# c\n" (mapOf [ "k", withEnd [ note " c" ] (seqOf [ sc "a" ]) ])
]

let private r5Tests = testList "R5 blank lines behind a block scalar" [
    // yaml@2.6.1 reads these as {"k":"x\n","j":1}, {"k":"x","j":1} and {"k":"x\n\n","j":1}. A blank line behind the body of a clipped or stripped block scalar is
    // trivia in front of the next entry. Behind the body of a kept block scalar it is content.
    testCase "a blank line behind a clipped or stripped body is trivia and behind a kept body content" <| fun _ ->
        expectRoot "k: |\n  x\n\nj: 1" (YamlNode.mapping [ YamlNode.entry (sc "k") (lit "x\n"); entryBefore [ blank ] (sc "j") (sc "1") ])
        expectRoot "k: >-\n  x\n\nj: 1" (YamlNode.mapping [ YamlNode.entry (sc "k") (blockScalar BlockScalarStyle.Folded ChompingMode.Strip None "x"); entryBefore [ blank ] (sc "j") (sc "1") ])
        expectRoot "k: |+\n  x\n\nj: 1" (YamlNode.mapping [ YamlNode.entry (sc "k") (blockScalar BlockScalarStyle.Literal ChompingMode.Keep None "x\n\n"); YamlNode.entry (sc "j") (sc "1") ])
        // At the end of the text the blank line goes to the End of the collection.
        expectRoot "k: |\n  x\n\n" (withEnd [ blank ] (mapOf [ "k", lit "x\n" ]))
        // A block scalar without content keeps no line break, and the empty lines behind its header are trivia.
        expectRoot "k: |\n\n\nj: 1" (YamlNode.mapping [ YamlNode.entry (sc "k") (lit ""); entryBefore [ blank; blank ] (sc "j") (sc "1") ])
]

let private r7Tests = testList "R7 the gap before the colon" [
    // yaml@2.6.1 reads `? a`, a blank line, a comment line and `: v` as {"a":"v"}. The trivia between an explicit key and its colon go to BeforeColon.
    testCase "trivia between an explicit key and its colon go to BeforeColon" <| fun _ ->
        expectRoot "? a\n\n# c\n: v" (YamlNode.mapping [ { explicitEntry (sc "a") (sc "v") with BeforeColon = [ blank; note " c" ] } ])
        expectRoot "? a\n# c\n# d\n: v\n" (YamlNode.mapping [ { explicitEntry (sc "a") (sc "v") with BeforeColon = [ note " c"; note " d" ] } ])
        // Behind an explicit key without a colon the trivia belong to the next entry.
        expectRoot "? a\n# c\n? b" (YamlNode.mapping [ explicitKeyOnly (sc "a"); { explicitKeyOnly (sc "b") with Before = [ note " c" ] } ])
        expectRoot "? a\n# c\nb: 1" (YamlNode.mapping [ explicitKeyOnly (sc "a"); entryBefore [ note " c" ] (sc "b") (sc "1") ])

    // The trivia behind the question mark of a missing key go to BeforeColon too, because no node of the key stands between them and the colon.
    testCase "trivia behind the question mark of a missing key go to BeforeColon" <| fun _ ->
        expectRoot "?\n# c\n: v" (YamlNode.mapping [ { explicitEntry nothing (sc "v") with BeforeColon = [ note " c" ] } ])
]

let private r8Tests = testList "R8 the trailing comment of a node" [
    // yaml@2.6.1 reads `k: v # c` as {"k":"v"}, and `? key # c` followed by `: v` as {"key":"v"}. A comment on the line of the last token of a node is its
    // trailing comment: a plain, quoted or alias scalar, or the key of an explicit entry.
    testCase "a comment behind a scalar, a quoted scalar, an alias or a key is the trailing comment of that node" <| fun _ ->
        expectRoot "k: v # c" (mapOf [ "k", withNote " c" (sc "v") ])
        expectRoot "k: \"v\" # c" (mapOf [ "k", withNote " c" (dq "v") ])
        expectRoot "k: 'v' # c" (mapOf [ "k", withNote " c" (sq "v") ])
        expectRoot "- v # c" (seqOf [ withNote " c" (sc "v") ])
        expectRoot "? key # c\n: v" (YamlNode.mapping [ explicitEntry (withNote " c" (sc "key")) (sc "v") ])
        expectRoot "k: &a v\nj: *a # c" (mapOf [ "k", YamlNode.withAnchor "a" (sc "v"); "j", withNote " c" (YamlNode.alias "a") ])
        // The comment behind the last line of a plain scalar over several lines.
        expectRoot "k: a\n  b # c" (mapOf [ "k", withNote " c" (scLines [ "a"; "b" ]) ])

    // yaml@2.6.1 reads `k: | # c` followed by `  x` as {"k":"x\n"}. A block scalar keeps the comment behind its header as the header comment and never has a trailing comment.
    testCase "a comment behind a block scalar header is the header comment" <| fun _ ->
        let withHeader = YamlNode.withHeaderComment (YamlComment.create " c") (lit "x\n")
        expectRoot "k: | # c\n  x\n" (mapOf [ "k", withHeader ])
        expectRoot "- >- # c\n  x\n" (seqOf [ YamlNode.withHeaderComment (YamlComment.create " c") (blockScalar BlockScalarStyle.Folded ChompingMode.Strip None "x") ])

    // A block collection has no closing token, so a comment behind its last entry belongs to that entry: it is the trailing comment of the last scalar.
    testCase "a comment behind the last entry of a block collection is the trailing comment of its value" <| fun _ ->
        expectRoot "a:\n  b: 1 # c\n" (mapOf [ "a", mapOf [ "b", withNote " c" (sc "1") ] ])
]

let private rulingTests = testList "Rulings 1a and 1b" [
    // 1a. yaml@2.6.1 reads `k: !!str # c` followed by `j: 1` as {"k":"","j":1}. A node with properties and no content gets the comment on the separation of its last property,
    // and has no trailing comment.
    testCase "1a: a comment behind the properties of a node without content goes to the last property" <| fun _ ->
        let tagged = withLayout [ placement PropertyName.Tag (newLine (Some " c") []) ] (YamlNode.withTag (YamlTag.core "str") (sc ""))
        expectRoot "k: !!str # c\nj: 1" (YamlNode.mapping [ YamlNode.entry (sc "k") tagged; YamlNode.entry (sc "j") (sc "1") ])
        let both = withLayout [ placement PropertyName.Anchor Separation.Inline; placement PropertyName.Tag (newLine (Some " c") []) ] (YamlNode.withTag (YamlTag.core "str") (YamlNode.withAnchor "a" (sc "")))
        expectRoot "- &a !!str # c\n- x" (YamlNode.sequence [ YamlNode.item both; YamlNode.item (sc "x") ])
        // The property alone, without a comment, keeps an inline separation.
        expectRoot "k: !!str" (mapOf [ "k", YamlNode.withTag (YamlTag.core "str") (sc "") ])

    // 1b. yaml@2.6.1 reads these as {"k":null,"j":1}, [null,"x"] and {"k":"","j":1}. When the content of an indicator is missing, Following stays empty and the trivia go to the
    // Before of the next entry or item.
    testCase "1b: the trivia behind an indicator without content go to the next entry or item" <| fun _ ->
        expectRoot "k:\n# d\nj: 1" (YamlNode.mapping [ YamlNode.entry (sc "k") nothing; entryBefore [ note " d" ] (sc "j") (sc "1") ])
        expectRoot "k:\n\n# d\nj: 1" (YamlNode.mapping [ YamlNode.entry (sc "k") nothing; entryBefore [ blank; note " d" ] (sc "j") (sc "1") ])
        expectRoot "-\n# d\n- x" (YamlNode.sequence [ YamlNode.item nothing; itemBefore [ note " d" ] (sc "x") ])
        // A comment behind the indicator stays with the indicator, and the trivia lines after it go to the next entry.
        expectRoot
            "k: # c\n# d\nj: 1"
            (YamlNode.mapping [ { YamlNode.entry (sc "k") nothing with Colon = Some(newLine (Some " c") []) }; entryBefore [ note " d" ] (sc "j") (sc "1") ])
        // The same behind the properties of a node without content.
        expectRoot
            "k: !!str\n# d\nj: 1"
            (YamlNode.mapping [ YamlNode.entry (sc "k") (YamlNode.withTag (YamlTag.core "str") (sc "")); entryBefore [ note " d" ] (sc "j") (sc "1") ])
]

// The rules of the flow collections. A flow collection has tokens at both ends and between its items, so a comment finds its owner by the line: the node that
// ends on the line of the comment, the colon that stands before it, or the entry that follows it.

let private strTag = YamlTag.core "str"

let private r6Tests = testList "R6 comments in a flow collection" [
    // yaml@2.6.1 reads these as ["a"] and {"a":1}. A comment behind the opener on its line is the OpeningComment of the collection.
    testCase "a comment behind the opener is the opening comment" <| fun _ ->
        expectRoot "[ # c\n a ]" (YamlNode.withOpeningComment (comment " c") (flowSeq [ sc "a" ]))
        expectRoot "{ # c\n a: 1 }" (YamlNode.withOpeningComment (comment " c") (flowMap [ "a", sc "1" ]))
        expectRoot "k: [ # c\n  a\n]" (mapOf [ "k", YamlNode.withOpeningComment (comment " c") (flowSeq [ sc "a" ]) ])
        expectRoot "[ # c\n]" (YamlNode.withOpeningComment (comment " c") (YamlNode.flowSequence []))

    // Tests.YamlRead "Comments at the start, in the middle and at the end of a flow sequence over several lines" and "Comments in a flow mapping over several lines stay entries of
    // the mapping". yaml@2.6.1 reads all of these as ["a","b"] or {"a":1,"b":2}. A comment on the same line behind an item and its comma is the trailing comment of the
    // node of the item, of the value of the entry, or of the key of an entry whose value is missing. A comment behind the item and in front of the comma is the same comment.
    testCase "a comment behind an item and its comma is the trailing comment of the node" <| fun _ ->
        expectRoot "[a, # c\n b]" (flowSeq [ withNote " c" (sc "a"); sc "b" ])
        expectRoot "[a # c\n, b]" (flowSeq [ withNote " c" (sc "a"); sc "b" ])
        expectRoot "[a, b # c\n]" (flowSeq [ sc "a"; withNote " c" (sc "b") ])
        expectRoot "k: [\n  a, # c2\n  b\n]" (mapOf [ "k", flowSeq [ withNote " c2" (sc "a"); sc "b" ] ])
        expectRoot "{a: 1, # c\n b: 2}" (flowMap [ "a", withNote " c" (sc "1"); "b", sc "2" ])
        expectRoot "{a: 1 # c\n, b: 2}" (flowMap [ "a", withNote " c" (sc "1"); "b", sc "2" ])
        expectRoot "{a, # c\n b}" (YamlNode.flowMapping [ keyOnly (withNote " c" (sc "a")); keyOnly (sc "b") ])
        expectRoot "[a: b # c\n]" (flowSeq [ flowPair (sc "a") (withNote " c" (sc "b")) ])
        expectRoot "[a: b, # c\n d]" (flowSeq [ flowPair (sc "a") (withNote " c" (sc "b")); sc "d" ])
        expectRoot "[\"a\", # c\n \"b\" # d\n]" (flowSeq [ withNote " c" (dq "a"); withNote " d" (dq "b") ])

    // yaml@2.6.1 reads `[!!str , # c` and `b]` as ["","b"]. The rule holds for a node that has properties and no content (rule 21 below).
    testCase "a comment behind a node with properties and no content is its trailing comment" <| fun _ ->
        expectRoot "[!!str , # c\n b]" (flowSeq [ withNote " c" (YamlNode.withTag strTag nothing); sc "b" ])

    // Tests.YamlRead "Comments at the start, in the middle and at the end of a flow sequence over several lines". Trivia lines between items go to the Before of the next item or entry.
    testCase "trivia lines between items go to the Before of the next item or entry" <| fun _ ->
        expectRoot "[a,\n # c\n\n b]" (YamlNode.flowSequence [ YamlNode.item (sc "a"); itemBefore [ note " c"; blank ] (sc "b") ])
        expectRoot "[a\n # c\n , b]" (YamlNode.flowSequence [ YamlNode.item (sc "a"); itemBefore [ note " c" ] (sc "b") ])
        expectRoot "[\n # c\n a]" (YamlNode.flowSequence [ itemBefore [ note " c" ] (sc "a") ])
        expectRoot "[\n\n a]" (YamlNode.flowSequence [ itemBefore [ blank ] (sc "a") ])
        expectRoot
            "{a: 1,\n # c\n b: 2}"
            (YamlNode.flowMapping [ YamlNode.entry (sc "a") (sc "1"); flowEntryBefore [ note " c" ] (sc "b") (sc "2") ])
        expectRoot
            "{\n # c\n a: 1}"
            (YamlNode.flowMapping [ flowEntryBefore [ note " c" ] (sc "a") (sc "1") ])
        // The trivia in front of a pair belong to the item of the sequence and not to the entry of its mapping.
        expectRoot
            "[\n # c\n a: b\n]"
            (YamlNode.flowSequence [ itemBefore [ note " c" ] (flowPair (sc "a") (sc "b")) ])

    // yaml@2.6.1 reads the first two as ["a","b"] and the others as [] and {"a":1}. Trivia lines in front of the closer go to End.
    testCase "trivia lines in front of the closer go to End" <| fun _ ->
        expectRoot "[a,\n b\n # c\n]" (withEnd [ note " c" ] (flowSeq [ sc "a"; sc "b" ]))
        expectRoot "[a,\n b,\n\n # c\n]" (withEnd [ blank; note " c" ] (flowSeq [ sc "a"; sc "b" ]))
        expectRoot "[\n # c\n]" (withEnd [ note " c" ] (YamlNode.flowSequence []))
        expectRoot "{a: 1\n # c\n}" (withEnd [ note " c" ] (flowMap [ "a", sc "1" ]))
        expectRoot "k: [\n  # c1\n  a,\n  # c2\n  b # c3\n  # c4\n]" (mapOf [ "k", withEnd [ note " c4" ] (YamlNode.flowSequence [ itemBefore [ note " c1" ] (sc "a"); itemBefore [ note " c2" ] (withNote " c3" (sc "b")) ]) ])
        // The closer may stand at a column left of the comments.
        expectRoot "a:\n  - [\n    1,\n# c\n    ]\n" (mapOf [ "a", seqOf [ withEnd [ note " c" ] (flowSeq [ sc "1" ]) ] ])

    // Tests.YamlRead "KeyValue InlineSequence + Comment" and "Comments at the start...": yaml@2.6.1 reads `Say: [Hello, World] # 420 blaze it` as {"Say":["Hello","World"]}. A comment behind
    // the closer is the trailing comment of the collection node (rule R8).
    testCase "a comment behind the closer is the trailing comment of the collection" <| fun _ ->
        expectRoot "Say: [Hello, World] # 420 blaze it" (mapOf [ "Say", withNote " 420 blaze it" (flowSeq [ sc "Hello"; sc "World" ]) ])
        expectRoot "{a: 1} # c" (withNote " c" (flowMap [ "a", sc "1" ]))
        expectRoot "k: [\n  a\n] # c4" (mapOf [ "k", withNote " c4" (flowSeq [ sc "a" ]) ])
        expectRoot "[[a] # c\n, b]" (flowSeq [ withNote " c" (flowSeq [ sc "a" ]); sc "b" ])
        expectRoot "- [a] # c\n- b" (seqOf [ withNote " c" (flowSeq [ sc "a" ]); sc "b" ])
        expectRoot "[a]: b # c" (YamlNode.mapping [ YamlNode.entry (flowSeq [ sc "a" ]) (withNote " c" (sc "b")) ])
]

let private r1FlowTests = testList "R1 and R2 in a flow collection" [
    // yaml@2.6.1 reads these as {"a":"b"}, {"a":"b"}. The comment behind a colon or a question mark goes to the separation of the indicator when the content stands on a later line,
    // and the trivia lines behind it are the Following of that separation (R1 and R2).
    testCase "a comment behind a colon or a question mark in a flow collection goes to the separation" <| fun _ ->
        expectRoot "{a: # c\n  b}" (YamlNode.flowMapping [ { YamlNode.entry (sc "a") (sc "b") with Colon = Some(newLine (Some " c") []) } ])
        expectRoot "{a:\n # d\n b}" (YamlNode.flowMapping [ { YamlNode.entry (sc "a") (sc "b") with Colon = Some(newLine None [ note " d" ]) } ])
        expectRoot "{? # q\n a : b}" (YamlNode.flowMapping [ { explicitFlowEntry (sc "a") (sc "b") with Question = Some(newLine (Some " q") []) } ])
        expectRoot "[a: # c\n b]" (flowSeq [ YamlNode.flowMapping [ { YamlNode.entry (sc "a") (sc "b") with Colon = Some(newLine (Some " c") []) } ] ])
        // A comment behind a colon whose value is missing, or behind the comma that follows the colon on its line, stays with the colon, because a missing value has no trailing comment.
        expectRoot "{a: # c\n}" (YamlNode.flowMapping [ { YamlNode.entry (sc "a") nothing with Colon = Some(newLine (Some " c") []) } ])
        expectRoot "{a: , # c\n b: 2}" (YamlNode.flowMapping [ { YamlNode.entry (sc "a") nothing with Colon = Some(newLine (Some " c") []) }; YamlNode.entry (sc "b") (sc "2") ])
        expectRoot "{? , # c\n b}" (YamlNode.flowMapping [ { explicitFlowEntry nothing nothing with Colon = None; Question = Some(newLine (Some " c") []) }; keyOnly (sc "b") ])

    // A comment behind the properties of a node, and the trivia lines behind them, go to the separation of the last property (rules R1 and R2).
    testCase "a comment behind a property in a flow collection goes to the separation of the property" <| fun _ ->
        let tagged = withLayout [ placement PropertyName.Tag (newLine (Some " c") []) ] (YamlNode.withTag strTag (sc "a"))
        expectRoot "[!!str # c\n a]" (flowSeq [ tagged ])
        let following = withLayout [ placement PropertyName.Tag (newLine None [ note " d" ]) ] (YamlNode.withTag strTag (sc "a"))
        expectRoot "[!!str\n # d\n a]" (flowSeq [ following ])
        let both = withLayout [ placement PropertyName.Anchor (newLine (Some " c") []); placement PropertyName.Tag Separation.Inline ] (YamlNode.withTag strTag (YamlNode.withAnchor "x" (sc "a")))
        expectRoot "[&x # c\n !!str a]" (flowSeq [ both ])
]

let private r7FlowTests = testList "R7 in a flow mapping and rule 20" [
    // Earlier test "the key of an entry in a flow mapping or behind a question mark may span lines". yaml@2.6.1 reads these as {"a":"b"}. The trivia lines between a key and its colon
    // go to BeforeColon, for an implicit key in a flow mapping too. A comment on the line of the key is no line of that gap (see the next test).
    testCase "the trivia lines between a key and its colon go to BeforeColon" <| fun _ ->
        expectRoot "{ a\n # c\n : b }" (YamlNode.flowMapping [ { YamlNode.entry (sc "a") (sc "b") with BeforeColon = [ note " c" ] } ])
        expectRoot "{ a\n\n : b }" (YamlNode.flowMapping [ { YamlNode.entry (sc "a") (sc "b") with BeforeColon = [ blank ] } ])
        expectRoot "{ ? a\n # c\n : b }" (YamlNode.flowMapping [ { explicitFlowEntry (sc "a") (sc "b") with BeforeColon = [ note " c" ] } ])
        expectRoot "{ ? a\n\n # c\n : b }" (YamlNode.flowMapping [ { explicitFlowEntry (sc "a") (sc "b") with BeforeColon = [ blank; note " c" ] } ])
        expectRoot "[ ? a\n # c\n : b ]" (flowSeq [ YamlNode.flowMapping [ { explicitFlowEntry (sc "a") (sc "b") with BeforeColon = [ note " c" ] } ] ])

    // Rules R7 and R8, the same in a block mapping and in a flow collection: a comment on the line where the key ends is the trailing comment of the key when the colon stands on a later line, and
    // the After of the last property when the key holds properties only (rule R1). Only the comment lines and the blank lines behind it go to BeforeColon.
    testCase "a comment on the line of a key whose colon stands on a later line is the trailing comment of the key" <| fun _ ->
        expectRoot "? \"a\" # c\n: v" (YamlNode.mapping [ explicitEntry (withNote " c" (dq "a")) (sc "v") ])
        expectRoot "{ ? \"a\" # c\n : v }" (YamlNode.flowMapping [ explicitFlowEntry (withNote " c" (dq "a")) (sc "v") ])
        expectRoot "{ a # c\n : b }" (YamlNode.flowMapping [ YamlNode.entry (withNote " c" (sc "a")) (sc "b") ])
        expectRoot "{ \"a\" # c\n : b }" (YamlNode.flowMapping [ YamlNode.entry (withNote " c" (dq "a")) (sc "b") ])
        expectRoot "{ [a] # c\n : v }" (YamlNode.flowMapping [ YamlNode.entry (withNote " c" (flowSeq [ sc "a" ])) (sc "v") ])
        expectRoot "{ ? [a] # c\n : v }" (YamlNode.flowMapping [ explicitFlowEntry (withNote " c" (flowSeq [ sc "a" ])) (sc "v") ])
        expectRoot "[ ? a # c\n : b ]" (flowSeq [ YamlNode.flowMapping [ explicitFlowEntry (withNote " c" (sc "a")) (sc "b") ] ])
        expectRoot "{ x: 1, a # c\n : b }" (YamlNode.flowMapping [ YamlNode.entry (sc "x") (sc "1"); YamlNode.entry (withNote " c" (sc "a")) (sc "b") ])
        // A comment on the line of the key and comment lines behind it.
        expectRoot
            "{ a # c\n # d\n\n # e\n : b }"
            (YamlNode.flowMapping [ { YamlNode.entry (withNote " c" (sc "a")) (sc "b") with BeforeColon = [ note " d"; blank; note " e" ] } ])
        expectRoot
            "{ ? a # c\n # d\n : b }"
            (YamlNode.flowMapping [ { explicitFlowEntry (withNote " c" (sc "a")) (sc "b") with BeforeColon = [ note " d" ] } ])
        // A key of properties only has no text to carry the comment, so it goes to the After of the last property.
        let anchored = withLayout [ placement PropertyName.Anchor (newLine (Some " c") []) ] (YamlNode.withAnchor "x" nothing)
        expectRoot "? &x # c\n: v" (YamlNode.mapping [ explicitEntry anchored (sc "v") ])
        expectRoot "{ &x # c\n : v }" (YamlNode.flowMapping [ YamlNode.entry anchored (sc "v") ])
        expectRoot "{ ? &x # c\n : v }" (YamlNode.flowMapping [ explicitFlowEntry anchored (sc "v") ])
        let tagged = withLayout [ placement PropertyName.Tag (newLine (Some " c") []) ] (YamlNode.withTag strTag nothing)
        expectRoot "{ !!str # c\n : b }" (YamlNode.flowMapping [ YamlNode.entry tagged (sc "b") ])
        expectRoot
            "{ &x # c\n # d\n : v }"
            (YamlNode.flowMapping [ { YamlNode.entry anchored (sc "v") with BeforeColon = [ note " d" ] } ])
        // A flow pair in a sequence keeps its key on one line with the colon, so an implicit key with a comment is an error as before.
        Expect.equal (parseErrorOf "[ a # c\n : b ]") "implicit-key-multiline@1" "A comment behind an implicit key of a flow pair"
        Expect.equal (parseErrorOf "[ &x # c\n : b ]") "implicit-key-multiline@1" "A comment behind the properties of an implicit key of a flow pair"

    // Rule 20: an explicit key without a colon has no gap before a colon. A comment on its line is its trailing comment, and the trivia lines behind it go to the Before of the next entry.
    testCase "rule 20: an explicit key without a colon passes its trivia to the next entry" <| fun _ ->
        expectRoot
            "{ ? a\n # c\n , b }"
            (YamlNode.flowMapping [ { explicitFlowEntry (sc "a") nothing with Colon = None }; keyOnly (sc "b") |> fun entry -> { entry with Before = [ note " c" ] } ])
        expectRoot "{ ? a # c\n , b }" (YamlNode.flowMapping [ { explicitFlowEntry (withNote " c" (sc "a")) nothing with Colon = None }; keyOnly (sc "b") ])
        expectRoot
            "{ ?\n # c\n , b }"
            (YamlNode.flowMapping [ { explicitFlowEntry nothing nothing with Colon = None }; keyOnly (sc "b") |> fun entry -> { entry with Before = [ note " c" ] } ])
        expectRoot
            "{ a\n # c\n , b }"
            (YamlNode.flowMapping [ keyOnly (sc "a"); keyOnly (sc "b") |> fun entry -> { entry with Before = [ note " c" ] } ])
]

let private rule21Tests = testList "Rules 21 and 22 on a node with properties and no content" [
    // Rule 21: in a flow collection a node that has properties and no content takes the comment behind the comma as its trailing comment. yaml@2.6.1 reads these as ["","b"] and {"a":"","b":1}.
    testCase "rule 21: the comment behind the comma is the trailing comment of a property-only node" <| fun _ ->
        expectRoot "[!!str , # c\n b]" (flowSeq [ withNote " c" (YamlNode.withTag strTag nothing); sc "b" ])
        expectRoot "[&a , # c\n b]" (flowSeq [ withNote " c" (YamlNode.withAnchor "a" nothing); sc "b" ])
        expectRoot "{a: !!str , # c\n b: 1}" (flowMap [ "a", withNote " c" (YamlNode.withTag strTag nothing); "b", sc "1" ])
        expectRoot "{!!str , # c\n b: 1}" (YamlNode.flowMapping [ keyOnly (withNote " c" (YamlNode.withTag strTag nothing)); YamlNode.entry (sc "b") (sc "1") ])

    // Rule 22: a comment behind a property with the comma on a later line goes to the After of the last property. The node has no trailing comment, because its last token is the property.
    testCase "rule 22: a comment behind a property with the comma on a later line goes to the last property" <| fun _ ->
        let tagged = withLayout [ placement PropertyName.Tag (newLine (Some " c") []) ] (YamlNode.withTag strTag nothing)
        expectRoot "[!!str # c\n , b]" (flowSeq [ tagged; sc "b" ])
        expectRoot "[!!str # c\n]" (flowSeq [ tagged ])
        expectRoot "{a: !!str # c\n , b: 1}" (flowMap [ "a", tagged; "b", sc "1" ])
        let anchored = withLayout [ placement PropertyName.Anchor (newLine (Some " c") []) ] (YamlNode.withAnchor "a" nothing)
        expectRoot "[&a # c\n , b]" (flowSeq [ anchored; sc "b" ])
        // The trivia lines behind the property go to the Before of the next item (for a property-only node).
        expectRoot "[!!str\n # d\n , b]" (YamlNode.flowSequence [ YamlNode.item (YamlNode.withTag strTag nothing); itemBefore [ note " d" ] (sc "b") ])
]

let private r8FlowTests = testList "R8 the closer of a flow collection" [
    // yaml@2.6.1 reads these as {"k":["a"]}, [["a"],"b"] and [{"a":"b"}]. The closer is the last token of a flow collection, so the comment on its line is the trailing comment of the collection.
    testCase "a comment on the line of the closer belongs to the collection" <| fun _ ->
        expectRoot "k: [\n  a\n] # c" (mapOf [ "k", withNote " c" (flowSeq [ sc "a" ]) ])
        expectRoot "[[a], # c\n b]" (flowSeq [ withNote " c" (flowSeq [ sc "a" ]); sc "b" ])
        expectRoot "[{a: b} # c\n]" (flowSeq [ withNote " c" (flowMap [ "a", sc "b" ]) ])
        expectRoot "[a, b] # c" (withNote " c" (flowSeq [ sc "a"; sc "b" ]))
        // A comment after the closer on a line of its own is not part of the collection.
        let document = parseDocument "[a]\n# c"
        Expect.nodeEqual document.Root (flowSeq [ sc "a" ]) "The root"
        Expect.equal (List.length document.AfterRoot) 1 "The comment goes behind the root"
]

let private cleanUpTests = testList "Comments of the old CommentCleanUp tests" [
    // Tests.CommentCleanUp "comments in values and behind values", "Text in the shape of a comment placeholder is text and not a comment", "Hash without preceding whitespace is not a comment",
    // "C# and a hash behind a word are text" and "a comment keeps its payload as written". The old tests checked the text that the scanner stage emitted, with its placeholders. The
    // placeholders are gone. The tests check the tree instead: where the comment stands and what the text of the scalar is. yaml@2.6.1 reads the scalars in the same way.
    testCase "a hash behind a blank starts a comment, a hash inside a word is text, and the comment keeps its payload" <| fun _ ->
        expectRoot
            "My Key: # This is a comment\n  My Value1\n  <s f=0/>\n  My Value3 # :::: \"This is also a comment\""
            (YamlNode.mapping [ { YamlNode.entry (sc "My Key") (withNote " :::: \"This is also a comment\"" (scLines [ "My Value1"; "<s f=0/>"; "My Value3" ])) with Colon = Some(newLine (Some " This is a comment") []) } ])
        expectRoot "a: x <c f=0/>\nb: 1 # real\n" (mapOf [ "a", sc "x <c f=0/>"; "b", withNote " real" (sc "1") ])
        expectRoot "a: tool.cwl#main # real" (mapOf [ "a", withNote " real" (sc "tool.cwl#main") ])
        expectRoot
            "lang: C# # real\nother: C#\nc: a#b#c\n"
            (mapOf [ "lang", withNote " real" (sc "C#"); "other", sc "C#"; "c", sc "a#b#c" ])
        expectRoot
            "a: 1 #   spaced  \nb: 2 ### more # hashes\nc: 3 #\nd: 4 #end"
            (mapOf [ "a", withNote "   spaced  " (sc "1"); "b", withNote "## more # hashes" (sc "2"); "c", withNote "" (sc "3"); "d", withNote "end" (sc "4") ])

    // Tests.CommentCleanUp "a tab before the hash starts a comment", "a comment at the end of the input without a line break", "quotes inside a comment are comment text" and
    // "a hash inside a quoted scalar is not a comment".
    testCase "a tab before the hash, the end of the input, quotes and hashes inside quoted scalars" <| fun _ ->
        expectRoot "a: 1\t# c\n" (mapOf [ "a", withNote " c" (sc "1") ])
        expectRoot "a: 1 # end" (mapOf [ "a", withNote " end" (sc "1") ])
        expectRoot "a: 1 # it's \"x\" 'y'\nb: 'z'\n" (mapOf [ "a", withNote " it's \"x\" 'y'" (sc "1"); "b", sq "z" ])
        expectRoot "a: \"x # y\" # c\nb: 'p # q' # d\n" (mapOf [ "a", withNote " c" (dq "x # y"); "b", withNote " d" (sq "p # q") ])
        expectRoot "a:\n  # one\n    # two\nb: 1\n#three\n" (withEnd [ note "three" ] (YamlNode.mapping [ YamlNode.entry (sc "a") nothing; entryBefore [ note " one"; note " two" ] (sc "b") (sc "1") ]))

    // Tests.CommentCleanUp "a quote glued to a word is not a string but a separated comment still is one", "a hash glued to a closing quote is an error" and "a hash behind a quote that never
    // closes is no comment". yaml@2.6.1 rejects the last two inputs with MISSING_CHAR.
    testCase "a quote inside a word opens no scalar and a hash behind a quote or inside an open quote is an error" <| fun _ ->
        expectRoot "x\"#suffix" (sc "x\"#suffix")
        expectRoot "\"x\" # comment" (withNote " comment" (dq "x"))
        Expect.equal (parseErrorOf "\"x\"#suffix") "glued-comment@1" "A hash glued to the closing quote"
        Expect.equal (parseErrorOf "a: 'open # c") "unclosed-quote@1" "The hash is inside the open quote"
        // The quoted scalar goes on in the next line, which stands at the column of the key, and no quote closes it.
        Expect.equal (parseErrorOf "a: 'open # c\nb: 1 # d\n") "unclosed-quote@1" "The scalar runs on to the end of the input"

    // Tests.CommentCleanUp "comments inside a multiline flow collection stay in the merged line". yaml@2.6.1: {"a":["x","y"]}. The reader before 2.0.0 merged the lines and kept only the comment behind
    // the closer as the comment of the line. The tree keeps every comment in its slot.
    testCase "comments inside a flow collection over several lines each have a slot" <| fun _ ->
        expectRoot
            "a: [ # open\n  'x', # one\n  \"y\" # two\n] # close\n"
            (mapOf [ "a", withNote " close" (YamlNode.withOpeningComment (comment " open") (flowSeq [ withNote " one" (sq "x"); withNote " two" (dq "y") ])) ])
]

let Main =
    testList "Comments" [
        r1Tests
        r2Tests
        r4Tests
        r4EndTests
        r5Tests
        r7Tests
        r8Tests
        rulingTests
        r6Tests
        r1FlowTests
        r7FlowTests
        rule21Tests
        r8FlowTests
        cleanUpTests
    ]
