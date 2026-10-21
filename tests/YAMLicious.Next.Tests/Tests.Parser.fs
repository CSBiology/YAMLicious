module Tests.Parser

// The tests of the block grammar of the parser. The expected trees are built with the constructors and compared
// with YamlNode.equalIgnoringSpans, so comments, styles, properties and the separations behind the indicators
// count. The yaml@2.6.1 value stands in a comment above the test. The inputs and the yaml values come from the
// old tests, which are named in the comment of each test. A test that differs from its old version says why: the
// new tree keeps the comments of a block in the slots of the model (rules R1 to R8), keeps collections and
// aliases as keys, and holds the folded text of a plain scalar over several lines.
//
// The tests with a flow collection, an alias key or a collection key in their input stand in Tests.Flow, Tests.Keys, Tests.Aliases and Tests.Comments.
// Old tests that are left for the next milestone:
// - M5: every test that writes, encodes or decodes (Decode.object, Encode.write, Writer.write, round trips).

open Fable.Pyxpecto
open YAMLicious

let private mappingTests = testList "Block mappings" [
    // Tests.YamlRead "Value", "KeyValue" and "KeyValue + Comment". yaml@2.6.1: "Hello World", {"Say":"Hello World"} twice.
    // The comment goes to the trailing comment of the value (rule R8).
    testCase "a scalar, an entry and an entry with a comment" <| fun _ ->
        expectRoot "Hello World" (sc "Hello World")
        expectRoot "Say: Hello World" (mapOf [ "Say", sc "Hello World" ])
        expectRoot "Say: Hello World # 420 blaze it" (mapOf [ "Say", withNote " 420 blaze it" (sc "Hello World") ])

    // Tests.YamlRead "SequenceImplicit", "KeyValue plain continuation folds into one scalar" and "Root plain continuation
    // folds into one scalar". yaml@2.6.1: {"My Key":"My Value1 My Value2 My Value3"}, {"My Key":"My Value1 My Value2"} and
    // "My Value1 My Value2". The old tree kept the physical lines, the new tree holds the folded text.
    testCase "a plain scalar over several lines is one scalar with the folded text" <| fun _ ->
        // The blank line at the top of the old inputs is the Before of the first entry (rules R3 and R4).
        // A value on a later line has a line break behind the colon (rule R2).
        expectRoot
            "\nMy Key:\n  My Value1\n  My Value2\n  My Value3\n"
            (withFirstBefore [ blank ] (YamlNode.mapping [ entryOnNewLine "My Key" (sc "My Value1 My Value2 My Value3") ]))
        expectRoot "\nMy Key: My Value1\n  My Value2\n" (withFirstBefore [ blank ] (mapOf [ "My Key", sc "My Value1 My Value2" ]))
        expectRoot "\nMy Value1\n  My Value2\n" (sc "My Value1 My Value2")

    // Tests.YamlRead "Nested mapping accepts structural blank line before child block". yaml@2.6.1: {"inputs":{"a":{"type":"string"},"b":{"type":"string"}}}.
    // The blank line belongs to the Before of the first entry of the child mapping (rules R2 and R4).
    testCase "a blank line in front of a nested mapping goes to its first entry" <| fun _ ->
        let expected =
            mapOf [
                "inputs",
                YamlNode.mapping [
                    entryBefore [ blank ] (sc "a") (mapOf [ "type", sc "string" ])
                    blockEntry (sc "b") (mapOf [ "type", sc "string" ])
                ]
            ]
        expectRoot "inputs:\n\n  a:\n    type: string\n  b:\n    type: string\n" expected

    // Tests.YamlRead "Nested mapping keeps blank lines between sibling entries". yaml@2.6.1: {"inputs":{"a":1,"b":2}}.
    testCase "a blank line between two entries goes to the second entry" <| fun _ ->
        let expected =
            mapOf [ "inputs", YamlNode.mapping [ YamlNode.entry (sc "a") (sc "1"); entryBefore [ blank ] (sc "b") (sc "2") ] ]
        expectRoot "inputs:\n  a: 1\n\n  b: 2\n" expected

    // Tests.YamlRead "A key without a value maps to nothing". yaml@2.6.1: {"k":null}, {"k":null}, {"k":null,"j":1}, {"k":null},
    // {"k":null,"j":1} and {"a":{"k":null},"b":1}. A missing value is an empty plain scalar, and a comment behind
    // the colon goes to the separation of the colon (rule R1).
    testCase "a key without a value has a missing value" <| fun _ ->
        expectRoot "k:" (mapOf [ "k", nothing ])
        expectRoot "k: " (mapOf [ "k", nothing ])
        expectRoot "k:\nj: 1" (mapOf [ "k", nothing; "j", sc "1" ])
        expectRoot "a:\n  k:\nb: 1" (mapOf [ "a", mapOf [ "k", nothing ]; "b", sc "1" ])
        let withComment = { YamlNode.entry (sc "k") nothing with Colon = Some(newLine (Some " c") []) }
        expectRoot "k: # c" (YamlNode.mapping [ withComment ])
        expectRoot "k: # c\nj: 1" (YamlNode.mapping [ withComment; YamlNode.entry (sc "j") (sc "1") ])
        // The span of a missing value has no width and stands behind the colon.
        let value = (YamlNode.entries (parseRoot "k:")).Head.Value
        Expect.equal (value.Span |> Option.map (fun span -> span.Start = span.Finish)) (Some true) "A span of no width"

    // Tests.YamlRead "Namespaces", "A dollar key with a block sequence", "A CWL head with dollar keys and prefixed keys",
    // "A dollar key with a value and a nested dollar key" and "A dollar key with a space or a colon in its name and a sibling
    // behind it". yaml@2.6.1 reads the dollar keys and the prefixed keys as ordinary keys.
    testCase "a key that starts with a dollar sign or holds a colon is a key like any other" <| fun _ ->
        let namespaces =
            mapOf [ "$namespaces", mapOf [ "own", sc "https://github.com/example-org/OWN_ontology"; "test", sc "https://github.com/example-org/ORIG_ontology" ] ]
        expectRoot "\n$namespaces:\n  own: https://github.com/example-org/OWN_ontology\n  test: https://github.com/example-org/ORIG_ontology\n" (withFirstBefore [ blank ] namespaces)
        let schemas = mapOf [ "$schemas", seqOf [ sc "https://x"; sc "https://y" ] ]
        expectRoot "$schemas:\n  - https://x\n  - https://y\n" schemas
        expectRoot "$schemas:\n- https://x\n- https://y\n" schemas
        expectRoot
            "id: x\n$namespaces:\n  s: https://schema.org/\n$schemas:\n  - https://schema.org/version/latest/termsonly-current-https.rdf\ncwlVersion: v1.2\ns:author:\n  - class: s:Person\n    s:name: Jane\n"
            (mapOf [
                "id", sc "x"
                "$namespaces", mapOf [ "s", sc "https://schema.org/" ]
                "$schemas", seqOf [ sc "https://schema.org/version/latest/termsonly-current-https.rdf" ]
                "cwlVersion", sc "v1.2"
                "s:author", seqOf [ mapOf [ "class", sc "s:Person"; "s:name", sc "Jane" ] ]
            ])
        expectRoot "$a: 1\n" (mapOf [ "$a", sc "1" ])
        expectRoot "x:\n  $a:\n    b: 1\n" (mapOf [ "x", mapOf [ "$a", mapOf [ "b", sc "1" ] ] ])
        expectRoot "$a b:\n  c: 1\n" (mapOf [ "$a b", mapOf [ "c", sc "1" ] ])
        expectRoot "$a:b:\n  c: 1\n" (mapOf [ "$a:b", mapOf [ "c", sc "1" ] ])
        expectRoot "$:\n  c: 1\n" (mapOf [ "$", mapOf [ "c", sc "1" ] ])
        expectRoot "$a:\n  b: 1\nz: 2\n" (mapOf [ "$a", mapOf [ "b", sc "1" ]; "z", sc "2" ])

    // Tests.YamlRead "A question mark without a blank behind it begins a plain key" and "A colon without a blank behind it is no
    // explicit value". yaml@2.6.1: {"?a":"b","?c":"d"}, {"?own:x":{"?class":"y"}}, ":a", {":a":"b"}, [":a"], {"k":":a"} and
    // {"k":"v",":a":"w"}. It rejects `? k` followed by `:a`.
    testCase "an indicator character without a blank behind it begins a plain scalar" <| fun _ ->
        expectRoot "?a: b\n?c: d" (mapOf [ "?a", sc "b"; "?c", sc "d" ])
        expectRoot "?own:x:\n    ?class: y" (mapOf [ "?own:x", mapOf [ "?class", sc "y" ] ])
        expectRoot ":a" (sc ":a")
        expectRoot ":a: b" (mapOf [ ":a", sc "b" ])
        expectRoot "- :a" (seqOf [ sc ":a" ])
        expectRoot "k:\n  :a" (YamlNode.mapping [ entryOnNewLine "k" (sc ":a") ])
        expectRoot "? k\n: v\n:a: w" (YamlNode.mapping [ explicitEntry (sc "k") (sc "v"); YamlNode.entry (sc ":a") (sc "w") ])
        Expect.equal (parseErrorOf "? k\n:a") "missing-colon@2" "The scalar :a has no colon of its own"

    // Tests.YamlRead "Deeper lines and comment lines stay valid among the entries of a block mapping". yaml@2.6.1: {"a":"1 b"},
    // {"a":1,"b":2}. The comment line goes to the Before of the next entry.
    testCase "a deeper line continues the value and a comment line stands in front of the next entry" <| fun _ ->
        expectRoot "a: 1\n  b" (mapOf [ "a", sc "1 b" ])
        expectRoot "a: 1\n# c\nb: 2" (YamlNode.mapping [ YamlNode.entry (sc "a") (sc "1"); entryBefore [ note " c" ] (sc "b") (sc "2") ])

    // Tests.YamlRead "A plain key may hold braces and brackets" (the block part). yaml@2.6.1: {"/participants/{id}.json":{"get":1}},
    // {"key with [brackets] inside":"v"}.
    testCase "a plain key may hold braces and brackets" <| fun _ ->
        expectRoot "/participants/{id}.json:\n  get: 1" (mapOf [ "/participants/{id}.json", mapOf [ "get", sc "1" ] ])
        expectRoot "key with [brackets] inside: v" (mapOf [ "key with [brackets] inside", sc "v" ])
        expectRoot "a[0]: 1\nb{x}: 2" (mapOf [ "a[0]", sc "1"; "b{x}", sc "2" ])

    // A line ends at LF, CRLF or a lone CR (YAML 1.2.2, b-break), and a byte order mark at the start is no text. The tree and the lines of the errors are the same for each.
    testCase "LF, CRLF and CR line breaks and a byte order mark give the same tree and the same error lines" <| fun _ ->
        let text = "a: 1\nb:\n  - x # c\n  - y\n# end\n"
        let expected = parseRoot text
        Expect.nodeEqual (parseRoot (text.Replace("\n", "\r\n"))) expected "CRLF"
        Expect.nodeEqual (parseRoot (text.Replace("\n", "\r"))) expected "CR"
        Expect.nodeEqual (parseRoot ("\uFEFF" + text)) expected "A byte order mark"
        Expect.equal (parseErrorOf "a: 1\r\nb") "missing-colon@2" "CRLF"
        Expect.equal (parseErrorOf "a: 1\rb") "missing-colon@2" "CR"
        Expect.equal (parseErrorOf "\uFEFFa: 1\nb") "missing-colon@2" "A byte order mark"

    // Tests.YamlRead "Hash without preceding whitespace stays in the scalar". yaml@2.6.1: {"run":"tool.cwl#main","lang":"C#","url":"http://x.org/a#frag"}.
    testCase "a hash without a blank in front of it stays in the scalar" <| fun _ ->
        expectRoot
            "run: tool.cwl#main\nlang: C#\nurl: http://x.org/a#frag"
            (mapOf [ "run", sc "tool.cwl#main"; "lang", sc "C#"; "url", sc "http://x.org/a#frag" ])

    // Tests.YamlRead "Comments in a CWL requirements block" (the structure). yaml@2.6.1: {"requirements":{"EnvVarRequirement":{"envDef":{"TOOL_RUN_SETTINGS":"-Da -Db"}},
    // "InitialWorkDirRequirement":{"listing":[{"entryname":"src_data","writable":true}]}}}. The comment in front of a key goes to the Before of its entry.
    testCase "a CWL requirements block keeps a comment in front of the first entry of a block" <| fun _ ->
        let yaml =
            "requirements:\n  EnvVarRequirement:\n    envDef:\n      # Runner writes logs\n      TOOL_RUN_SETTINGS: >-\n        -Da\n        -Db\n  InitialWorkDirRequirement:\n    listing:\n      # Make an editable copy\n      - entryname: src_data\n        writable: true"
        let envDef =
            YamlNode.mapping [ entryBefore [ note " Runner writes logs" ] (sc "TOOL_RUN_SETTINGS") (blockScalar BlockScalarStyle.Folded ChompingMode.Strip None "-Da -Db") ]
        let listing =
            YamlNode.sequence [ itemBefore [ note " Make an editable copy" ] (mapOf [ "entryname", sc "src_data"; "writable", sc "true" ]) ]
        expectRoot
            yaml
            (mapOf [ "requirements", mapOf [ "EnvVarRequirement", mapOf [ "envDef", envDef ]; "InitialWorkDirRequirement", mapOf [ "listing", listing ] ] ])
]

let private sequenceTests = testList "Block sequences" [
    // Tests.YamlRead "Sequence", "SequenceObjects" and "SequenceSameIndentAsMapping2". yaml@2.6.1: ["My Value 1","My Value 2","My Value 3"],
    // ["My Value 1 My Value 2","My Value 3"] and {"My Key":["My Value1 My Value2","My Value3"]}.
    testCase "a sequence with a scalar over several lines" <| fun _ ->
        // The blank line at the top of the old inputs is the Before of the first entry or item (rules R3 and R4).
        expectRoot "\n- My Value 1\n- My Value 2\n- My Value 3\n" (withFirstBefore [ blank ] (seqOf [ sc "My Value 1"; sc "My Value 2"; sc "My Value 3" ]))
        expectRoot "\n- My Value 1\n  My Value 2\n- My Value 3\n" (withFirstBefore [ blank ] (seqOf [ sc "My Value 1 My Value 2"; sc "My Value 3" ]))
        expectRoot "\nMy Key:\n- My Value1\n  My Value2\n- My Value3\n" (withFirstBefore [ blank ] (mapOf [ "My Key", seqOf [ sc "My Value1 My Value2"; sc "My Value3" ] ]))

    // Tests.YamlRead "SequenceSameIndentAsMapping" and "SequenceSameIndentAsMappingAndComment". yaml@2.6.1: {"My Key":["My Value1","My Value2","My Value3"]}
    // twice. The sequence stands at the column of its key, and the comment goes to the Before of the item behind it.
    testCase "an indentless sequence is the value of its key" <| fun _ ->
        let plain = mapOf [ "My Key", seqOf [ sc "My Value1"; sc "My Value2"; sc "My Value3" ] ]
        expectRoot "\nMy Key:\n- My Value1\n- My Value2\n- My Value3\n" (withFirstBefore [ blank ] plain)
        let withComment =
            mapOf [
                "My Key",
                YamlNode.sequence [
                    YamlNode.item (sc "My Value1")
                    YamlNode.item (sc "My Value2")
                    itemBefore [ note "ich hab das gerade gesehen und dachte mir \"ah fuck\"" ] (sc "My Value3")
                ]
            ]
        expectRoot "\nMy Key:\n- My Value1\n- My Value2\n#ich hab das gerade gesehen und dachte mir \"ah fuck\"\n- My Value3\n" (withFirstBefore [ blank ] withComment)

    // Tests.YamlRead "An indentless sequence between sibling keys reads the same at depth 1, 2 and 3". yaml@2.6.1 reads each of these as
    // {"before":1,"tags":["x","y"],"after":{"c":1},"last":2}, nested in {"k0":...} and {"k0":{"k1":...}} for the depths 2 and 3, and the key up at the root.
    testCase "an indentless sequence between sibling keys reads the same at every depth" <| fun _ ->
        let body = [ "before: 1"; "tags:"; "- x"; "- y"; "after:"; "  c: 1"; "last: 2" ]
        let members =
            [ "before", sc "1"
              "tags", seqOf [ sc "x"; sc "y" ]
              "after", mapOf [ "c", sc "1" ]
              "last", sc "2" ]
        for depth in [ 1; 2; 3 ] do
            let keys = [ for level in 0 .. depth - 2 -> String.replicate level "  " + sprintf "k%d:" level ]
            let lines = body |> List.map (fun line -> String.replicate (depth - 1) "  " + line)
            let up = if depth > 1 then [ "up: 9" ] else []
            let rec wrap (level: int) : (string * YamlNode) list =
                if level = depth - 1 then members else [ sprintf "k%d" level, mapOf (wrap (level + 1)) ]
            let expectedUp = if depth > 1 then [ "up", sc "9" ] else []
            expectRoot (String.concat "\n" (keys @ lines @ up)) (mapOf (wrap 0 @ expectedUp))

    // Tests.YamlRead "An indentless sequence of mappings keeps its items and the key behind it". yaml@2.6.1:
    // {"p":{"parameters":[{"name":"id","in":"path"},{"name":"b","in":"q"}],"responses":{"x":1}}}.
    testCase "an indentless sequence of mappings ends at the next key of its mapping" <| fun _ ->
        let item (name: string) (place: string) = mapOf [ "name", sc name; "in", sc place ]
        expectRoot
            "p:\n  parameters:\n  - name: id\n    in: path\n  - name: b\n    in: q\n  responses:\n    x: 1"
            (mapOf [ "p", mapOf [ "parameters", seqOf [ item "id" "path"; item "b" "q" ]; "responses", mapOf [ "x", sc "1" ] ] ])

    // Tests.YamlRead "A block sequence and a block mapping do not share an indentation" (the valid part). yaml@2.6.1 reads `k:` followed by `- x`
    // and `k: !!seq` followed by `- x` as {"k":["x"]}, and `- a`, a comment and `- b` as ["a","b"].
    testCase "a sequence at the column of its key, behind a core tag and with a comment line between its items" <| fun _ ->
        expectRoot "k:\n- x" (mapOf [ "k", seqOf [ sc "x" ] ])
        let tagged = tagOnOwnLine (YamlTag.core "seq") (seqOf [ sc "x" ])
        expectRoot "k: !!seq\n- x" (YamlNode.mapping [ YamlNode.entry (sc "k") tagged ])
        expectRoot "- a\n# c\n- b" (YamlNode.sequence [ YamlNode.item (sc "a"); itemBefore [ note " c" ] (sc "b") ])

    // Tests.YamlRead "A compact nested sequence holds the items at the column of its dash". yaml@2.6.1: [["a","b"]], [["a"],"b"], [[["a","b"]]]
    // and [[["a","b"],"c"],"d"].
    testCase "a compact nested sequence holds its items at the column of its dash" <| fun _ ->
        expectRoot "- - a\n  - b\n" (seqOf [ seqOf [ sc "a"; sc "b" ] ])
        expectRoot "- - a\n- b\n" (seqOf [ seqOf [ sc "a" ]; sc "b" ])
        expectRoot "- - - a\n    - b\n" (seqOf [ seqOf [ seqOf [ sc "a"; sc "b" ] ] ])
        expectRoot "- - - a\n    - b\n  - c\n- d\n" (seqOf [ seqOf [ seqOf [ sc "a"; sc "b" ]; sc "c" ]; sc "d" ])

    // Tests.YamlRead "The lines behind a compact nested sequence item belong to that item". yaml@2.6.1: {"k":[[{"class":"File","path":"p"}]]},
    // [[{"k":"v","j":"w"}]], [["x\n","y"]], [[{"class":"File","path":"p"},"z"],"w"] and [[{"a":"b"}]].
    testCase "the lines behind a compact nested sequence item belong to that item" <| fun _ ->
        let fileItem = mapOf [ "class", sc "File"; "path", sc "p" ]
        expectRoot "k:\n  - - class: File\n      path: p\n" (mapOf [ "k", seqOf [ seqOf [ fileItem ] ] ])
        expectRoot "- - k: v\n    j: w\n" (seqOf [ seqOf [ mapOf [ "k", sc "v"; "j", sc "w" ] ] ])
        expectRoot "- - |\n    x\n  - y\n" (seqOf [ seqOf [ lit "x\n"; sc "y" ] ])
        expectRoot "- - class: File\n    path: p\n  - z\n- w\n" (seqOf [ seqOf [ fileItem; sc "z" ]; sc "w" ])
        expectRoot "- - ? a\n    : b\n" (seqOf [ seqOf [ YamlNode.mapping [ explicitEntry (sc "a") (sc "b") ] ] ])

    // Tests.YamlRead "A list of lists of CWL file records reads as nested sequences". yaml@2.6.1:
    // {"fileCollectionSet":[[{"class":"File","path":"a_1.fq.gz"},{"class":"File","path":"a_2.fq.gz"}],[{"class":"File","path":"b_1.fq.gz"}]]}.
    testCase "a list of lists of records reads as nested sequences" <| fun _ ->
        let file (path: string) = mapOf [ "class", sc "File"; "path", sc path ]
        expectRoot
            "fileCollectionSet:\n  - - class: File\n      path: a_1.fq.gz\n    - class: File\n      path: a_2.fq.gz\n  - - class: File\n      path: b_1.fq.gz\n"
            (mapOf [ "fileCollectionSet", seqOf [ seqOf [ file "a_1.fq.gz"; file "a_2.fq.gz" ]; seqOf [ file "b_1.fq.gz" ] ] ])

    // Tests.YamlRead "A key at the start of a sequence item owns the block behind it". yaml@2.6.1: [{"a":{"b":1},"c":2}], [{"a":"x\n","b":1}] and [{"key":"x\n"}].
    testCase "a key at the start of a sequence item owns the block behind it" <| fun _ ->
        expectRoot "- a:\n    b: 1\n  c: 2\n" (seqOf [ mapOf [ "a", mapOf [ "b", sc "1" ]; "c", sc "2" ] ])
        expectRoot "- a: |\n    x\n  b: 1\n" (seqOf [ mapOf [ "a", lit "x\n"; "b", sc "1" ] ])
        expectRoot "- key: |2\n    x\n" (seqOf [ mapOf [ "key", blockScalar BlockScalarStyle.Literal ChompingMode.Clip (Some 2) "x\n" ] ])

    // Tests.YamlRead "Sequence item accepts structural blank line before nested mapping", "NextLineSequenceObjects". yaml@2.6.1:
    // {"items":[{"a":1}]} and [{"My Key1":"My Value1",...},{"My Key4":"My Value4",...}]. The content of a dash on a later line gives the
    // dash a line break (rule R2).
    testCase "a mapping on the line behind its dash" <| fun _ ->
        expectRoot "items:\n  -\n    a: 1\n" (mapOf [ "items", YamlNode.sequence [ itemOnNewLine (mapOf [ "a", sc "1" ]) ] ])
        expectRoot
            "\n-\n  My Key1: My Value1\n  My Key2: My Value2\n  My Key3: My Value3\n-\n  My Key4: My Value4\n  My Key5: My Value5\n  My Key6: My Value6\n"
            (withFirstBefore
                [ blank ]
                (YamlNode.sequence [
                    itemOnNewLine (mapOf [ "My Key1", sc "My Value1"; "My Key2", sc "My Value2"; "My Key3", sc "My Value3" ])
                    itemOnNewLine (mapOf [ "My Key4", sc "My Value4"; "My Key5", sc "My Value5"; "My Key6", sc "My Value6" ])
                ]))

    // Tests.YamlRead "An empty sequence item reads as Nil" (the block part), "Bare dash sequence element" and "Multiple bare dash sequence elements".
    // yaml@2.6.1: [null,"x"], ["x",null], [null], {"k":[null,"x",null],"j":1} and [[null,"x"],[null]]. An empty item is a missing value.
    testCase "an empty sequence item is a missing value" <| fun _ ->
        expectRoot "-\n- x" (seqOf [ nothing; sc "x" ])
        expectRoot "- x\n-" (seqOf [ sc "x"; nothing ])
        expectRoot "-" (seqOf [ nothing ])
        expectRoot "k:\n-\n- x\n-\nj: 1" (mapOf [ "k", seqOf [ nothing; sc "x"; nothing ]; "j", sc "1" ])
        expectRoot "- -\n  - x\n- -" (seqOf [ seqOf [ nothing; sc "x" ]; seqOf [ nothing ] ])

    // Tests.YamlRead "Comments around the items of a compact nested sequence stay with their items". yaml@2.6.1: [["a","b"]], which drops the comments.
    testCase "comments around the items of a compact nested sequence stay with their items" <| fun _ ->
        expectRoot
            "- - a # c1\n  # c2\n  - b # c3\n"
            (seqOf [ YamlNode.sequence [ YamlNode.item (withNote " c1" (sc "a")); itemBefore [ note " c2" ] (withNote " c3" (sc "b")) ] ])
]

let private explicitKeyTests = testList "Explicit keys" [
    // Tests.YamlRead "A comment behind an explicit key belongs to the key". yaml@2.6.1 reads `? a # c` followed by `: b` as {"a":"b"}, and
    // `? # c` followed by `a` and `: b` as {"a":"b"}. It reads spec Example 8.17 as {"explicit key":null,"block key\n":["one","two"]}.
    // The comment behind a scalar is its trailing comment (rule R8). The comment behind the question mark goes to the separation of the
    // question mark (rule R1).
    testCase "a comment behind an explicit key stands where the text puts it" <| fun _ ->
        expectRoot "? a # c\n: b" (YamlNode.mapping [ explicitEntry (withNote " c" (sc "a")) (sc "b") ])
        expectRoot
            "? # c\n  a\n: b"
            (YamlNode.mapping [ { explicitEntry (sc "a") (sc "b") with Question = Some(newLine (Some " c") []) } ])
        expectRoot "? \"q\" # c\n: v" (YamlNode.mapping [ explicitEntry (withNote " c" (dq "q")) (sc "v") ])
        let items = YamlNode.sequence [ YamlNode.item (withNote " Explicit compact" (sc "one")); YamlNode.item (withNote " block value" (sc "two")) ]
        expectRoot
            "? explicit key # Empty value\n? |\n  block key\n: - one # Explicit compact\n  - two # block value\n"
            (YamlNode.mapping [
                { explicitKeyOnly (withNote " Empty value" (sc "explicit key")) with Value = nothing }
                explicitEntry (lit "block key\n") items
            ])

    // Tests.YamlRead "A comment behind a key...": `? a`, a comment line and `: v`. yaml@2.6.1: {"a":"v"}. The comment line between the key and the colon
    // goes to the BeforeColon of the entry (rule R7), and so does a comment line behind a key that stands on its own line.
    testCase "a comment line between an explicit key and its colon goes to the entry" <| fun _ ->
        let commented = { explicitEntry (sc "a") (sc "v") with BeforeColon = [ note " c" ] }
        expectRoot "? a\n# c\n: v" (YamlNode.mapping [ commented ])
        expectRoot
            "?\n  a\n  # c\n: v"
            (YamlNode.mapping [ { commented with Question = Some(newLine None []) } ])
        // A blank line counts as well.
        expectRoot "? a\n\n: v" (YamlNode.mapping [ { explicitEntry (sc "a") (sc "v") with BeforeColon = [ blank ] } ])

    // Tests.YamlRead "An explicit key with an empty value maps to Nil". yaml@2.6.1: {"a":null} twice and {"a":{}}. A colon alone is an
    // entry with a missing value, and a comment behind it goes to the separation of the colon (rule R1).
    testCase "an explicit key with a colon and no value has a missing value" <| fun _ ->
        expectRoot "? a\n:" (YamlNode.mapping [ explicitEntry (sc "a") nothing ])
        expectRoot "? a\n: # c" (YamlNode.mapping [ { explicitEntry (sc "a") nothing with Colon = Some(newLine (Some " c") []) } ])
        expectRoot "? a" (YamlNode.mapping [ explicitKeyOnly (sc "a") ])
        expectRoot "? a\n? b" (YamlNode.mapping [ explicitKeyOnly (sc "a"); explicitKeyOnly (sc "b") ])

    // Tests.YamlRead "A collection behind a question mark is an error". The old project rejected a collection as a key, because it kept only scalar keys.
    // yaml@2.6.1 accepts each of these and makes a string key from the collection. The new model keeps the collection as the key node.
    testCase "a collection behind a question mark is the key node" <| fun _ ->
        expectRoot "? - a\n: b" (YamlNode.mapping [ explicitEntry (seqOf [ sc "a" ]) (sc "b") ])
        expectRoot "? a: b\n: c" (YamlNode.mapping [ explicitEntry (mapOf [ "a", sc "b" ]) (sc "c") ])
        expectRoot
            "?\n  - a\n  - b\n: v"
            (YamlNode.mapping [ { explicitEntry (seqOf [ sc "a"; sc "b" ]) (sc "v") with Question = Some(newLine None []) } ])
        expectRoot
            "?\n  a: 1\n  b: 2\n: v"
            (YamlNode.mapping [ { explicitEntry (mapOf [ "a", sc "1"; "b", sc "2" ]) (sc "v") with Question = Some(newLine None []) } ])
        expectRoot "- ? - a\n    - b\n  : c" (seqOf [ YamlNode.mapping [ explicitEntry (seqOf [ sc "a"; sc "b" ]) (sc "c") ] ])

    // Tests.YamlRead "A scalar behind a question mark stays a key although it looks like an indicator". yaml@2.6.1 reads the keys as the strings
    // "-a", "?a", ":a", "a:b", "a: b" and "a - b".
    testCase "a scalar behind a question mark is a key although it looks like an indicator" <| fun _ ->
        for text in [ "-a"; "?a"; ":a"; "a:b"; "a - b" ] do
            expectRoot (sprintf "? %s\n: v" text) (YamlNode.mapping [ explicitEntry (sc text) (sc "v") ])
        expectRoot "? \"a: b\"\n: v" (YamlNode.mapping [ explicitEntry (dq "a: b") (sc "v") ])

    // Tests.YamlRead "A scalar over several lines folds as an explicit key". yaml@2.6.1: {"a b":"v"} for the first two inputs, {"a b\nc":"v"},
    // {"a b":"v"} for the double-quoted key, {"a\nb":"v"}, {"a\nb\n":"v"} and {"a b\n":"v"}.
    testCase "a scalar over several lines folds as an explicit key" <| fun _ ->
        let single (key: YamlNode) = YamlNode.mapping [ explicitEntry key (sc "v") ]
        expectRoot "? a\n  b\n: v" (single (sc "a b"))
        expectRoot "?\n  a\n  b\n: v" (YamlNode.mapping [ { explicitEntry (sc "a b") (sc "v") with Question = Some(newLine None []) } ])
        expectRoot "? a\n  b\n\n  c\n: v" (single (sc "a b\nc"))
        expectRoot "? \"a\n  b\"\n: v" (single (dq "a b"))
        expectRoot "? 'a\n\n  b'\n: v" (single (sq "a\nb"))
        expectRoot "? |\n  a\n  b\n: v" (single (lit "a\nb\n"))
        expectRoot "? >\n  a\n  b\n: v" (single (fold "a b\n"))

    // Tests.YamlRead "A block collection on the line of its key is an error that the Reader reports", the valid part. yaml@2.6.1 reads `- - a` as [["a"]].
    // A compact collection behind a dash, a question mark and a colon of an explicit entry is valid.
    testCase "a compact collection behind a dash, a question mark or an explicit colon is valid" <| fun _ ->
        expectRoot "- - a" (seqOf [ seqOf [ sc "a" ] ])
        expectRoot "- a: 1\n  b: 2" (seqOf [ mapOf [ "a", sc "1"; "b", sc "2" ] ])
        expectRoot "- ? a\n  : b" (seqOf [ YamlNode.mapping [ explicitEntry (sc "a") (sc "b") ] ])
        expectRoot "? a\n: - b\n  - c" (YamlNode.mapping [ explicitEntry (sc "a") (seqOf [ sc "b"; sc "c" ]) ])
        expectRoot "? a\n: b: 1\n  c: 2" (YamlNode.mapping [ explicitEntry (sc "a") (mapOf [ "b", sc "1"; "c", sc "2" ]) ])
        expectRoot "? - a\n  - b\n: c" (YamlNode.mapping [ explicitEntry (seqOf [ sc "a"; sc "b" ]) (sc "c") ])

    // Tests.YamlRead "An empty key with properties is a mapping entry". yaml@2.6.1 reads `&a : v`, `!!str : v` and `&a !!str : v` as {"":"v"},
    // `&a :` as {"":null}, `- &a : v` as [{"":"v"}], `&a :` followed by `  x: 1` as {"":{"x":1}} and `? &a` followed by `: v` as {"":"v"}.
    testCase "an empty key with properties is a mapping entry" <| fun _ ->
        let anchored = YamlNode.withAnchor "a" (sc "")
        let tagged = YamlNode.withTag (YamlTag.core "str") (sc "")
        let both = { YamlNode.withAnchor "a" tagged with PropertyLayout = List.rev (YamlNode.withAnchor "a" tagged).PropertyLayout }
        expectRoot "&a : v" (YamlNode.mapping [ YamlNode.entry anchored (sc "v") ])
        expectRoot "!!str : v" (YamlNode.mapping [ YamlNode.entry tagged (sc "v") ])
        expectRoot "&a !!str : v" (YamlNode.mapping [ YamlNode.entry both (sc "v") ])
        expectRoot "&a :" (YamlNode.mapping [ YamlNode.entry anchored nothing ])
        expectRoot "&a :\n  x: 1" (YamlNode.mapping [ { YamlNode.entry anchored (mapOf [ "x", sc "1" ]) with Colon = Some(newLine None []) } ])
        expectRoot "- &a : v" (seqOf [ YamlNode.mapping [ YamlNode.entry anchored (sc "v") ] ])
        expectRoot "? &a\n: v" (YamlNode.mapping [ explicitEntry anchored (sc "v") ])
        // An empty key without properties is a missing value.
        expectRoot ": v" (YamlNode.mapping [ YamlNode.entry nothing (sc "v") ])
        expectRoot "a: 1\n: v" (YamlNode.mapping [ YamlNode.entry (sc "a") (sc "1"); YamlNode.entry nothing (sc "v") ])
]

let private propertyTests = testList "Properties and aliases" [
    // Tests.YamlRead "An anchor name may hold colons". yaml@2.6.1 reads `k: &a:b x` as {"k":"x"}, `&a: b` as "b", `k: &a:` as {"k":null} and
    // `&a:b : v` as {"":"v"}. The anchors are a:b, a:, a: and a:b.
    testCase "an anchor name may hold colons" <| fun _ ->
        expectRoot "k: &a:b x" (mapOf [ "k", YamlNode.withAnchor "a:b" (sc "x") ])
        expectRoot "&a: b" (YamlNode.withAnchor "a:" (sc "b"))
        expectRoot "k: &a:" (mapOf [ "k", YamlNode.withAnchor "a:" (sc "") ])
        expectRoot "&a:b : v" (YamlNode.mapping [ YamlNode.entry (YamlNode.withAnchor "a:b" (sc "")) (sc "v") ])

    // Tests.YamlRead "An alias name may hold colons" (the block part). yaml@2.6.1 reads `x: &a:b 1` followed by `k: *a:b` as {"x":1,"k":1} and the
    // sequence `- &a:b 1`, `- *a:b` as [1,1]. The tree keeps the alias symbolic.
    testCase "an alias name may hold colons and the alias stays symbolic" <| fun _ ->
        expectRoot
            "x: &a:b 1\nk: *a:b"
            (mapOf [ "x", YamlNode.withAnchor "a:b" (sc "1"); "k", YamlNode.alias "a:b" ])
        expectRoot "- &a:b 1\n- *a:b" (seqOf [ YamlNode.withAnchor "a:b" (sc "1"); YamlNode.alias "a:b" ])

    // Tests.YamlRead "An alias as a block key is an error". yaml@2.6.1 reads `x: &a 1` followed by `*a : v` as {"1":"v","x":1}. It resolves the alias to
    // make the key. The old project rejected the alias, because it kept only scalar keys. The new model keeps the alias as the key node.
    testCase "an alias is a valid key and stays symbolic" <| fun _ ->
        let anchored = YamlNode.withAnchor "a" (sc "1")
        expectRoot "x: &a 1\n*a : v" (YamlNode.mapping [ YamlNode.entry (sc "x") anchored; YamlNode.entry (YamlNode.alias "a") (sc "v") ])
        expectRoot "x: &a 1\n? *a\n: v" (YamlNode.mapping [ YamlNode.entry (sc "x") anchored; explicitEntry (YamlNode.alias "a") (sc "v") ])
        expectRoot "x: &a:b 1\n*a:b : v" (YamlNode.mapping [ YamlNode.entry (sc "x") (YamlNode.withAnchor "a:b" (sc "1")); YamlNode.entry (YamlNode.alias "a:b") (sc "v") ])
        expectRoot "x: &a 1\nk:\n  - *a : v" (YamlNode.mapping [ YamlNode.entry (sc "x") anchored; blockEntry (sc "k") (seqOf [ YamlNode.mapping [ YamlNode.entry (YamlNode.alias "a") (sc "v") ] ]) ])

    // Tests.YamlRead "An alias followed by a comment is an alias". yaml@2.6.1 reads `x: &a 1` followed by `k: *a # c` as {"x":1,"k":1}, and `- &a 1`
    // followed by `- *a # c` as [1,1]. The comment is the trailing comment of the alias (rule R8). The old tree put it in front of the alias.
    testCase "an alias followed by a comment keeps the comment as its trailing comment" <| fun _ ->
        let anchored = YamlNode.withAnchor "a" (sc "1")
        let alias = withNote " c" (YamlNode.alias "a")
        expectRoot "x: &a 1\nk: *a # c" (mapOf [ "x", anchored; "k", alias ])
        expectRoot "- &a 1\n- *a # c" (seqOf [ anchored; alias ])
        expectRoot "&a x\n" (YamlNode.withAnchor "a" (sc "x"))

    // Tests.YamlRead "A named tag handle needs a TAG directive in its document" (the valid part). yaml@2.6.1 reads `%TAG !e! tag:example.com,2000:`, `---` and
    // `k: !e!b c` as {"k":"c"}. The handles ! and !! need no directive. A verbatim tag keeps its URI as the suffix of an empty prefix.
    testCase "a tag handle resolves to the prefix of its directive" <| fun _ ->
        let tagged = YamlNode.withTag (YamlTag.Named { Prefix = "tag:example.com,2000:"; Suffix = "b" }) (sc "c")
        expectRoot "%TAG !e! tag:example.com,2000:\n---\nk: !e!b c" (mapOf [ "k", tagged ])
        expectRoot "%TAG ! tag:example.com,2000:\n---\nk: !b c" (mapOf [ "k", tagged ])
        expectRoot "k: !<tag:example.com,2000:b> c" (mapOf [ "k", YamlNode.withTag (YamlTag.Named { Prefix = ""; Suffix = "tag:example.com,2000:b" }) (sc "c") ])
        expectRoot "k: !local c" (mapOf [ "k", YamlNode.withTag (YamlTag.local "local") (sc "c") ])
        expectRoot "k: !!str c" (mapOf [ "k", YamlNode.withTag (YamlTag.core "str") (sc "c") ])
        expectRoot "k: ! c" (mapOf [ "k", YamlNode.withTag YamlTag.NonSpecific (sc "c") ])
        // The % escapes of a suffix are decoded (YAML 1.2.2, example 6.26). yaml@2.6.1 reads the tag of the last item as tag:example.com,2000:app/tag!.
        expectRoot
            "%TAG !e! tag:example.com,2000:app/\n---\n- !local foo\n- !!str bar\n- !e!tag%21 baz\n"
            (seqOf [
                YamlNode.withTag (YamlTag.local "local") (sc "foo")
                YamlNode.withTag (YamlTag.core "str") (sc "bar")
                YamlNode.withTag (YamlTag.Named { Prefix = "tag:example.com,2000:app/"; Suffix = "tag!" }) (sc "baz")
            ])

    // Tests.Directives "TAG directive" and the second document of the old test in YamlRead. A directive holds for its own document only.
    testCase "a tag directive holds for its own document only" <| fun _ ->
        Expect.equal (parseErrorOf "%TAG !e! tag:example.com,2000:\n---\na: !e!b c\n...\n---\nb: !e!b c") "undeclared-tag-handle@6" "The second document has no directive of its own"

    // Tests.YamlRead "A block scalar header...": properties in any order, on any node. yaml@2.6.1 reads each of these as the plain value "x" or
    // as {"k":"x"} with the tag and the anchor on the scalar.
    testCase "a tag and an anchor in either order stand on the same node" <| fun _ ->
        let both = YamlNode.withAnchor "a" (YamlNode.withTag (YamlTag.core "str") (sc "x"))
        expectRoot "!!str &a x" both
        let reversed = { both with PropertyLayout = List.rev both.PropertyLayout }
        expectRoot "&a !!str x" reversed
        expectRoot "k: &a !!str x" (mapOf [ "k", reversed ])

    // Tests.YamlRead "An alias with properties is an error". yaml@2.6.1 rejects each of these with "An alias node must not specify any properties".
    testCase "an alias with properties is an error" <| fun _ ->
        for (input, line) in [ "k: !!str *a", 1; "k: &b *a", 1; "- !t *a", 1; "k: !!str\n  *a", 1; "- &b\n  *a", 1; "!!str *a", 1 ] do
            Expect.equal (parseErrorOf input) (sprintf "alias-with-properties@%d" line) (sprintf "The properties of %A" input)

    // Two tags or two anchors on one node are an error. yaml@2.6.1 rejects them with MULTIPLE_TAGS and MULTIPLE_ANCHORS ("A node can have at most one tag").
    testCase "two tags or two anchors on one node are an error" <| fun _ ->
        Expect.equal (parseErrorOf "!!str !!int x") "duplicate-tag@1" "Two tags"
        Expect.equal (parseErrorOf "&a &b x") "duplicate-anchor@1" "Two anchors"
        Expect.equal (parseErrorOf "k: &a &b x") "duplicate-anchor@1" "Two anchors behind a key"
        Expect.equal (parseErrorOf "&a\n&b x") "duplicate-anchor@2" "Two anchors on two lines"

    // Tests.YamlRead "A named tag handle needs a TAG directive in its document" (the rejections). yaml@2.6.1 rejects each of these with "Could not resolve tag".
    testCase "a named tag handle without a directive is an error" <| fun _ ->
        for (input, line) in [ "k: !e!b c", 1; "- !e!b c", 1; "k: &a !e!b c", 1; "? !e!b k\n: v", 1; "!e!b c", 1; "%TAG !e! tag:example.com,2000:\n---\nk: !f!b c", 3 ] do
            Expect.equal (parseErrorOf input) (sprintf "undeclared-tag-handle@%d" line) (sprintf "The handle in %A" input)

    // The tag and the anchor of a mapping or a sequence stand on a line of their own in front of its first entry.
    testCase "properties on a line of their own belong to the collection that follows" <| fun _ ->
        let tagged = tagOnOwnLine (YamlTag.core "map") (mapOf [ "a", sc "1" ])
        expectRoot "k: !!map\n  a: 1" (mapOf [ "k", tagged ])
        expectRoot "!!map\na: 1" tagged
        let anchoredSeq =
            { YamlNode.withAnchor "s" (seqOf [ sc "x" ]) with PropertyLayout = [ { Property = PropertyName.Anchor; After = newLine None []; Span = None } ] }
        expectRoot "k: &s\n- x" (mapOf [ "k", anchoredSeq ])
        expectRoot "- &s\n  - x" (seqOf [ anchoredSeq ])
        // The properties on the line of a key belong to the key, and the ones on the line before to the mapping.
        let key = YamlNode.withTag (YamlTag.core "str") (sc "k")
        let mapping = tagOnOwnLine (YamlTag.core "map") (YamlNode.mapping [ YamlNode.entry key (sc "v") ])
        expectRoot "!!map\n!!str k: v" mapping
        // Two anchors on two lines are two nodes when the second stands on the line of a key.
        let outer = { YamlNode.withAnchor "a" (YamlNode.mapping [ YamlNode.entry (YamlNode.withAnchor "b" (sc "k")) (sc "v") ]) with PropertyLayout = [ { Property = PropertyName.Anchor; After = newLine None []; Span = None } ] }
        expectRoot "&a\n&b k: v" outer
]

let private expectError (input: string) (expected: string) =
    Expect.equal (parseErrorOf input) expected (sprintf "The error of %A" input)

let private errorTests = testList "Parser errors" [
    // Tests.YamlRead "A block collection on the line of its key is an error that the Reader reports". yaml@2.6.1 rejects `k: - a` and `k: ? a` ("Block collections are
    // not allowed within flow collections" and "A block sequence may not be used as an implicit map key"). Compact forms behind a dash, a question mark and an
    // explicit colon stay valid.
    testCase "a block collection on the line of its key is an error" <| fun _ ->
        expectError "k: - a" "block-collection-on-key-line@1"
        expectError "a: 1\nk: ? a" "block-collection-on-key-line@2"
        expectError "k:\n  j: - a" "block-collection-on-key-line@2"
        expectError "- k: - a" "block-collection-on-key-line@1"

    // Tests.YamlRead "A colon followed by a blank inside a plain scalar value is an error" (Tokenizer) and "A second colon behind a quoted part of the line is an error".
    // yaml@2.6.1 rejects all of these with BLOCK_AS_IMPLICIT_KEY. A mapping cannot start on the line of its key, and a `: ` inside a plain value starts one.
    testCase "a colon followed by a blank behind a value is an error" <| fun _ ->
        for input in [ "valueFrom: $(inputs.a ? 'x: y' : \"z\")"; "valueFrom: $(inputs.a ? 'x' : 'y')"; "k: a: b"; "k: a:"; "k: a: b: c"; "- k: a: b" ] do
            expectError input "block-collection-on-key-line@1"
        for input in [ "k: \"x\": 1"; "\"a\": \"b\": c"; "k: \"x\" : 1"; "k: 'x': 1"; "- \"a\": \"b\": c" ] do
            expectError input "block-collection-on-key-line@1"

    // Tests.YamlRead "A colon followed by a blank inside a plain scalar value is an error". A plain scalar over several lines cannot be an implicit key.
    // yaml@2.6.1 rejects `a: 1` followed by an indented `b: 2` with "Implicit keys need to be on a single line".
    testCase "a plain scalar over several lines cannot be an implicit key" <| fun _ ->
        expectError "a: 1\n  b: 2" "implicit-key-multiline@1"
        expectError "k:\n  a\n  b: 1" "implicit-key-multiline@2"
        expectError "\"a\nb\": 1" "implicit-key-multiline@1"
        expectError (sprintf "%s: 1" (String.replicate 1100 "k")) "implicit-key-length@1"

    // YAML 1.2.2 limits an implicit key to 1024 characters (ns-s-implicit-yaml-key, [154] and [155]). The characters count from the first character of the key, its properties included,
    // up to the colon, so the blanks in front of the colon count too. A key behind a dash or in a nested mapping counts from its own start.
    testCase "an implicit key has at most 1024 characters from its first property to its colon" <| fun _ ->
        let keys (length: int) = String.replicate length "k"
        parseOk (keys 1024 + ": 1") |> ignore
        expectError (keys 1025 + ": 1") "implicit-key-length@1"
        // The properties and their blanks.
        parseOk ("&a " + keys 1021 + ": 1") |> ignore
        expectError ("&a " + keys 1022 + ": 1") "implicit-key-length@1"
        parseOk ("!!str &a " + keys 1015 + ": 1") |> ignore
        expectError ("!!str &a " + keys 1016 + ": 1") "implicit-key-length@1"
        // The blanks in front of the colon.
        parseOk (keys 1023 + " : 1") |> ignore
        expectError (keys 1024 + " : 1") "implicit-key-length@1"
        parseOk (keys 1020 + "    : 1") |> ignore
        expectError (keys 1021 + "    : 1") "implicit-key-length@1"
        // A quoted key counts its quotes.
        parseOk ("\"" + keys 1022 + "\": 1") |> ignore
        expectError ("\"" + keys 1023 + "\": 1") "implicit-key-length@1"
        // The start of the key, not the start of the line.
        parseOk ("- " + keys 1024 + ": 1") |> ignore
        expectError ("- " + keys 1025 + ": 1") "implicit-key-length@1"
        parseOk ("a:\n  " + keys 1024 + ": 1") |> ignore
        expectError ("a:\n  " + keys 1025 + ": 1") "implicit-key-length@2"
        // An explicit key has no such limit.
        parseOk ("? " + keys 2000 + "\n: 1") |> ignore
        let error = parseError (keys 1025 + ": 1")
        Expect.equal (showSpan error.Span) "1:1-1:1026" "The error names the key up to the colon"

    // Tests.YamlRead "A scalar among the entries of a block mapping is an error". yaml@2.6.1 rejects each of these with "Implicit map keys need to be followed
    // by map values", and `a: 1` followed by `|` and ` x` with "Implicit keys need to be on a single line".
    testCase "a scalar among the entries of a block mapping is an error" <| fun _ ->
        expectError "? a\n:b" "missing-colon@2"
        expectError "a: 1\nb" "missing-colon@2"
        expectError "- a: 1\n  b" "missing-colon@2"
        expectError "a:\n  b: 1\n  c" "missing-colon@3"
        expectError "a: 1\n'q'" "missing-colon@2"
        expectError "? a\n: 1\nb" "missing-colon@3"
        expectError "a: !!str\nb" "missing-colon@2"
        expectError "a: 1\n|\n x" "missing-colon@2"
        expectError "a: 1\n&b" "misplaced-properties@2"

    // Tests.YamlRead "A block sequence and a block mapping do not share an indentation". yaml@2.6.1 rejects `a: 1` followed by `- x`, `k: v`, `- x` and `j: 2`, and
    // `- a: 1` followed by `  - b` ("A block sequence may not be used as an implicit map key"), and `- a` followed by `b: 1` or `b` ("Unexpected scalar at node end").
    testCase "a block sequence and a block mapping do not share an indentation" <| fun _ ->
        expectError "a: 1\n- x" "mixed-collections@2"
        expectError "- a\nb: 1" "mixed-collections@2"
        expectError "k: v\n- x\nj: 2" "mixed-collections@2"
        expectError "- a: 1\n  - b" "mixed-collections@2"
        expectError "- a\n#c\nb: 1" "mixed-collections@3"
        expectError "- a\nb" "mixed-collections@2"
        expectError "- a\n? b\n: c" "mixed-collections@2"

    // N1-A and N1-B groups, here with their codes. A line that is deeper than the node in front of it, behind a node that ended, is an error.
    testCase "a deeper line behind a node that ended is an error" <| fun _ ->
        expectError "k: \"q\"\n  x" "bad-indentation@2"
        expectError "- \"q\"\n  x" "bad-indentation@2"
        expectError "s: &a x\nk: *a\n  x" "bad-indentation@3"
        expectError "s: &a x\nk: *a # c\n  b" "bad-indentation@3"
        expectError "k: a # c\n  b" "bad-indentation@2"
        expectError "k:\n  a # c\n  b" "bad-indentation@3"
        expectError "k:\n  \"a\"\n  j: 1" "bad-indentation@3"
        expectError "a:\n  b: foo\n c: 1" "bad-indentation@3"
        expectError "- k: 3.5\n j: 1" "bad-indentation@2"

    // Tests.Tokenizer "an alias with properties...". yaml@2.6.1 rejects "a: 1\n&x\nj: 2" and "k: v\n!!str\nj: w" with MULTILINE_IMPLICIT_KEY. Properties on lines of their own cannot
    // stand in front of an entry of a mapping that has started already.
    testCase "properties on lines of their own in front of an entry of an open mapping are an error" <| fun _ ->
        expectError "a: 1\n&x\nj: 2" "misplaced-properties@3"
        expectError "k: v\n!!str\nj: w" "misplaced-properties@3"
        expectError "k: v\n&x" "misplaced-properties@2"

    // yaml@2.6.1 rejects every one of these inputs, with BAD_PROP_ORDER in front of a `?` and with errors about an implicit key or an unexpected token in front of a dash. A dash starts
    // its sequence entry right behind the indentation ([183], [184]) and a `?` starts its mapping entry right behind it ([187] to [190]), so properties on the line of an entry of an
    // open mapping cannot stand in front of either of them.
    testCase "properties in front of a dash or a question mark on the line of an entry are an error" <| fun _ ->
        for (input, expected) in
            [ "a: 1\n&x - b", "mixed-collections@2"
              "k:\n- a\n&x - b", "mixed-collections@3"
              "- a: 1\n  !!str - b\n  c: 2", "mixed-collections@2"
              "a: 1\n&x - b\nc: *x", "mixed-collections@2"
              "- a\n&x - b", "mixed-collections@2"
              "a: 1\n&x ? b", "misplaced-properties@2"
              "a: 1\n&x ? b\n: *x", "misplaced-properties@2"
              "a: 1\n!!str ? b", "misplaced-properties@2"
              "- a: 1\n  !!str &y ? b", "misplaced-properties@2" ] do
            expectError input expected

    // Tests.Tokenizer "an undefined alias": the alias has to name an anchor that stands earlier in its document (YAML 1.2.2, 7.1). yaml@2.6.1 accepts these
    // inputs and fails later, when it resolves the alias.
    testCase "an alias needs an anchor earlier in its document" <| fun _ ->
        expectError "a: *b" "undefined-alias@1"
        expectError "*a" "undefined-alias@1"
        expectError "a: &b 1\n---\nc: *b" "undefined-alias@3"
        // An alias may name the anchor of the collection that holds it, and an anchor name may be used again.
        parseOk "&a\n- *a" |> ignore
        parseOk "a: &x 1\nb: &x 2\nc: *x" |> ignore

    // yaml@2.6.1 rejects `a: !tag"x"` with MISSING_CHAR ("Tags and anchors must be separated from the next token by white space").
    testCase "a tag or an anchor needs a blank behind it" <| fun _ ->
        expectError "a: !tag\"x\"" "missing-separation@1"
        expectError "a: !t|\n  x" "missing-separation@1"

    // yaml@2.6.1 rejects `!<!> foo` ("Verbatim tags aren't resolved, so !<!> is invalid.", spec example 6.25).
    testCase "a verbatim tag needs a name" <| fun _ ->
        expectError "- !<!> foo" "invalid-tag@1"
        expectError "- !<> foo" "invalid-tag@1"

    // The tabs: Tests.Tokenizer "a tab in the indentation or between an indicator and a block collection is an error". yaml@2.6.1 rejects the first group with TAB_AS_INDENT
    // and reads the second group.
    testCase "a tab in the indentation or between an indicator and a block collection is an error" <| fun _ ->
        expectError "\tk: v" "tab-indentation@1"
        expectError "-\tk: v" "tab-indentation@1"
        expectError "- \tk: v" "tab-indentation@1"
        expectError "?\tk: v" "tab-indentation@1"
        expectError "k:\n  -\tk: v" "tab-indentation@2"
        expectError "- - \ta: b" "tab-indentation@1"
        expectError "k:\n  \ta: b" "tab-indentation@2"
        expectError "a: b\n# c\n\tc: d" "tab-indentation@3"
        expectError "---\tk: v" "tab-indentation@1"
        expectError "--- k: v" "block-collection-on-marker-line@1"
        expectError "--- - a" "block-collection-on-marker-line@1"
        expectError "--- ? a" "block-collection-on-marker-line@1"
        expectError "\t- a" "tab-indentation@1"
        expectError "-\t- a" "tab-indentation@1"
        expectError "?\t- a" "tab-indentation@1"
        expectError "a:\t- b" "tab-indentation@1"
        // The line is no continuation of the scalar and no key, so it is a node of its own.
        expectError "k: a\n\tb" "missing-colon@2"
        expectError "k:\n\tv" "missing-colon@2"
        expectError "- a\n\tb" "mixed-collections@2"
        expectError "k: a\n\n\tb" "missing-colon@3"

    // yaml@2.6.1 reads each of these: {"k":"v"} for the first two, {"k":"a\tb"} three times, ["v"] twice. A tab is separation between an indicator and a scalar,
    // inside a scalar and behind the indentation of a continuation line. YAML 1.2.2 (s-separate-in-line) also allows a tab in front of a root scalar, which
    // yaml@2.6.1 rejects with TAB_AS_INDENT.
    testCase "a tab as separation or inside a scalar is no error" <| fun _ ->
        expectRoot "k:\tv" (mapOf [ "k", sc "v" ])
        expectRoot "k:\n  \tv" (YamlNode.mapping [ entryOnNewLine "k" (sc "v") ])
        expectRoot "k: a\tb" (mapOf [ "k", sc "a\tb" ])
        expectRoot "k: 'a\tb'" (mapOf [ "k", sq "a\tb" ])
        expectRoot "k: \"a\tb\"" (mapOf [ "k", dq "a\tb" ])
        expectRoot "k: |\n  a\tb\n  \tc\n" (mapOf [ "k", lit "a\tb\n\tc\n" ])
        expectRoot "-\tv" (seqOf [ sc "v" ])
        expectRoot "k: a\n  \tb" (mapOf [ "k", sc "a b" ])
        expectRoot "?\tk" (YamlNode.mapping [ explicitKeyOnly (sc "k") ])
        expectRoot "\tv" (sc "v")
        expectRoot "---\tv" (sc "v")

    // The directives. Tests.Directives "2.1.2 Invalid duplicate YAML directive". yaml@2.6.1 reads a second %YAML directive with a warning. YAML 1.2.2 (6.8.1) makes
    // it an error. A directive needs the --- of its document (l-directive-document), which yaml@2.6.1 does not insist on.
    testCase "the directives of a document are checked" <| fun _ ->
        expectError "%YAML 1.2\n%YAML 1.1\n---\nfoo: bar" "duplicate-yaml-directive@2"
        expectError "%TAG !e! tag:a,2000:\n%TAG !e! tag:b,2000:\n---\nx" "duplicate-tag-directive@2"
        expectError "%YAML 1.2\nfoo: bar" "directive-without-document-start@2"
        expectError "%YAML 1.2\n" "directive-without-document-start@2"
        expectError "a\n...\n%TAG ! t!" "directive-without-document-start@3"
        expectError "%YAML 2.0\n---\nx" "unsupported-yaml-version@1"

    // The message names the line, and Related names the opener where the error has one.
    testCase "the message of an error names its line" <| fun _ ->
        let error = parseError "a: 1\nb"
        Expect.stringContains error.Message "Line 2" "The message names the line"
        let alias = parseError "k: !!str *a"
        Expect.equal (showSpan alias.Span) "1:4-1:9" "The error names the properties"
        Expect.equal (alias.Related |> List.map showSpan) [ "1:10-1:12" ] "The alias is the related span of the properties"
        let failure = Projection.failureOf error
        Expect.equal failure "{\"ok\":false,\"errors\":[{\"code\":\"missing-colon\",\"line\":2}]}" "The failure projection"

    // yaml@2.6.1 reads the two inputs with duplicate directives as {"foo":"bar"} and "x" and `? a` followed by `- x` as {"a":null}. It rejects the other inputs. Related names the
    // text that explains the error: the first of two directives, the first entry or item of the collection, the node that ended, the root, the first key and the first property.
    // A line that is shallower than the collection it ends and deeper than the one around it matches the indentation of no open collection.
    testCase "an error names a related span where one explains it" <| fun _ ->
        for (input, code, span, related) in
            [ "%YAML 1.2\n%YAML 1.1\n---\nfoo: bar", "duplicate-yaml-directive", "2:1-2:10", [ "1:1-1:10" ]
              "%TAG !e! tag:a,2000:\n%TAG !e! tag:b,2000:\n---\nx", "duplicate-tag-directive", "2:1-2:21", [ "1:1-1:21" ]
              "k: \"q\"\n  x", "bad-indentation", "2:3-2:4", [ "1:1-1:7" ]
              "a:\n    b: 1\n  c: 2", "bad-indentation", "3:3-3:4", [ "2:5-2:9" ]
              "a: 1\n- x", "mixed-collections", "2:1-2:2", [ "1:1-1:5" ]
              "- a\nb: 1", "mixed-collections", "2:1-2:2", [ "1:1-1:4" ]
              "? a\n- x", "mixed-collections", "2:1-2:2", [ "1:1-1:4" ]
              "a: 1\n&x - b", "mixed-collections", "2:4-2:5", [ "1:1-1:5" ]
              "\"a\"\nb", "unexpected-content", "2:1-2:2", [ "1:1-1:4" ]
              "a: 1\nb", "missing-colon", "2:1-2:2", [ "1:1-1:2" ]
              "a: 1\n&x ? b", "misplaced-properties", "2:4-2:5", [ "2:1-2:3" ] ] do
            let error = parseError input
            Expect.equal (YamlErrorCode.name error.Code) code (sprintf "The code of %A" input)
            Expect.equal (showSpan error.Span) span (sprintf "The span of %A" input)
            Expect.equal (error.Related |> List.map showSpan) related (sprintf "The related spans of %A" input)
        Expect.stringContains (parseError "a:\n    b: 1\n  c: 2").Message "matches the indentation of no open collection" "A line between two indentations"
        Expect.stringContains (parseError "k: \"q\"\n  x").Message "deeper than the node in front of it" "A line deeper than the node that ended"
]

let private spanText (span: SourceSpan option) : string =
    match span with
    | Some value -> showSpan value
    | None -> "none"

// The tests above compare trees without their spans. These compare the spans exactly. Lines and columns count from 1, and a column counts a code point.
let private spanTests = testList "Spans" [
    // yaml@2.6.1: {"k":"v","j":"w"} and {"k":{"a":1}}. A mapping starts at its first property when the property stands on a line of its own, and an entry ends where its value ends.
    testCase "a mapping with properties on a line of its own spans from the first property to its last entry" <| fun _ ->
        let root = parseRoot "&m\nk: v\nj: w"
        Expect.equal (spanText root.Span) "1:1-3:5" "The mapping"
        Expect.equal (root.PropertyLayout |> List.map (fun placement -> spanText placement.Span)) [ "1:1-1:3" ] "The anchor"
        Expect.equal (YamlNode.entries root |> List.map (fun entry -> spanText entry.Span)) [ "2:1-2:5"; "3:1-3:5" ] "The entries"
        let entry = (YamlNode.entries (parseRoot "k: !!map\n  a: 1")).Head
        Expect.equal (spanText entry.Span) "1:1-2:7" "The entry"
        Expect.equal (spanText entry.Value.Span) "1:4-2:7" "The value mapping starts at its tag"
        Expect.equal (entry.Value.PropertyLayout |> List.map (fun placement -> spanText placement.Span)) [ "1:4-1:9" ] "The tag"

    // yaml@2.6.1: {"a":null,"bb":null}. An explicit entry without a value ends at its key, and the missing value is a span of no width there.
    testCase "an explicit entry without a value ends at its key" <| fun _ ->
        let root = parseRoot "? a\n? bb"
        Expect.equal (spanText root.Span) "1:1-2:5" "The mapping"
        let entries = YamlNode.entries root
        Expect.equal (entries |> List.map (fun entry -> spanText entry.Span)) [ "1:1-1:4"; "2:1-2:5" ] "The entries"
        Expect.equal (entries |> List.map (fun entry -> spanText entry.Key.Span)) [ "1:3-1:4"; "2:3-2:5" ] "The keys"
        Expect.equal (entries |> List.map (fun entry -> spanText entry.Value.Span)) [ "1:4-1:4"; "2:5-2:5" ] "The missing values"

    // yaml@2.6.1: {"k":"","j":1} and [null,"x"]. A node that has properties and no content spans its properties, and its entry or item ends there.
    testCase "a missing value behind a property spans the properties" <| fun _ ->
        let entry = (YamlNode.entries (parseRoot "k: !!str\nj: 1")).Head
        Expect.equal (spanText entry.Span) "1:1-1:9" "The entry"
        Expect.equal (spanText entry.Value.Span) "1:4-1:9" "The value"
        Expect.equal (entry.Value.PropertyLayout |> List.map (fun placement -> spanText placement.Span)) [ "1:4-1:9" ] "The tag"
        let item = (YamlNode.items (parseRoot "- &a\n- x")).Head
        Expect.equal (spanText item.Span) "1:1-1:5" "The item"
        Expect.equal (spanText item.Node.Span) "1:3-1:5" "The node"

    // yaml@2.6.1: [["a","b"]]. The inner sequence starts at its own dash, and so do its items.
    testCase "the items of a compact nested sequence span from their own dash" <| fun _ ->
        let outer = parseRoot "- - a\n  - b"
        Expect.equal (spanText outer.Span) "1:1-2:6" "The outer sequence"
        let outerItem = (YamlNode.items outer).Head
        Expect.equal (spanText outerItem.Span) "1:1-2:6" "The outer item"
        let inner = outerItem.Node
        Expect.equal (spanText inner.Span) "1:3-2:6" "The inner sequence"
        Expect.equal (YamlNode.items inner |> List.map (fun item -> spanText item.Span)) [ "1:3-1:6"; "2:3-2:6" ] "The inner items"
        Expect.equal (YamlNode.items inner |> List.map (fun item -> spanText item.Node.Span)) [ "1:5-1:6"; "2:5-2:6" ] "The scalars"

    // yaml@2.6.1 reads each of these inputs as {"a":1}, and the last one as {"a":1} and {"b":2}. [209] l-directive-document makes the directives part of the document, so its span starts at the first directive.
    // Without a directive it starts at the `---`, and a bare document starts at its root. Comments and blank lines in front of the first directive or the `---` stand outside the
    // span, and the ones between the directives and the `---` stand inside. The directives of the prelude carry their own spans.
    testCase "a document with a directive and a start marker spans from the first directive" <| fun _ ->
        let document = parseDocument "%YAML 1.2\n---\na: 1\n...\n"
        let directives =
            document.Prelude
            |> List.choose (fun item ->
                match item with
                | PreludeItem.Directive directive -> Some(spanText directive.Span)
                | PreludeItem.PreludeTrivia _ -> None)
        Expect.equal directives [ "1:1-1:10" ] "The directive"
        Expect.equal (spanText (document.StartMarker |> Option.bind (fun marker -> marker.Span))) "2:1-2:4" "The start marker"
        Expect.equal (spanText document.Root.Span) "3:1-3:5" "The root"
        Expect.equal (spanText (document.EndMarker |> Option.bind (fun marker -> marker.Span))) "4:1-4:4" "The end marker"
        Expect.equal (spanText document.Span) "1:1-4:4" "The document"
        let spansOfDocuments (input: string) =
            (parseOk input).Parts
            |> List.choose (fun part ->
                match part with
                | StreamPart.Document value -> Some(spanText value.Span)
                | _ -> None)
        Expect.equal (spansOfDocuments "# c\n%YAML 1.2\n---\na: 1\n...\n") [ "2:1-5:4" ] "A comment in front of the directive stands outside"
        Expect.equal (spansOfDocuments "%YAML 1.2\n# c\n%TAG !e! tag:a,2000:\n\n---\na: 1\n") [ "1:1-6:5" ] "Trivia between the directives and the marker stand inside"
        Expect.equal (spansOfDocuments "# c\n---\na: 1") [ "2:1-3:5" ] "Without a directive the document starts at its marker"
        Expect.equal (spansOfDocuments "# c\na: 1") [ "2:1-2:5" ] "A bare document starts at its root"
        Expect.equal (spansOfDocuments "a: 1\n...\n# c\n%YAML 1.2\n---\nb: 2") [ "1:1-2:4"; "4:1-6:5" ] "The second document starts at its own directive"

    // yaml@2.6.1 reads a key of two U+1F600 characters with the value "x". An astral character is one column on every target.
    testCase "an astral key spans one column for each character" <| fun _ ->
        let entry = (YamlNode.entries (parseRoot "\U0001F600\U0001F600: x")).Head
        Expect.equal (spanText entry.Key.Span) "1:1-1:3" "The key"
        Expect.equal (spanText entry.Value.Span) "1:5-1:6" "The value"
        Expect.equal (spanText entry.Span) "1:1-1:6" "The entry"
]

let private depthTests = testList "Depth" [
    // The parser keeps its state on the heap, so a deep nesting needs no deep stack. A dash behind a dash opens a compact sequence, and each of the 10000
    // dashes opens one.
    testCase "a compact nesting of 10000 dashes parses without a stack overflow" <| fun _ ->
        let stream = parseOk (String.replicate 10000 "- " + "x")
        let data = Projection.data stream
        Expect.stringContains data "\"maxDepth\":10000" "The depth"

    // A mapping in a mapping, each level one space deeper than the level above.
    testCase "a mapping nested to 1000 levels by indentation parses without a stack overflow" <| fun _ ->
        let sb = System.Text.StringBuilder()
        for level in 0 .. 999 do
            sb.Append(String.replicate level " ").Append("k:\n") |> ignore
        sb.Append(String.replicate 1000 " ").Append("v\n") |> ignore
        let stream = parseOk (sb.ToString())
        Expect.stringContains (Projection.data stream) "\"maxDepth\":1000" "The depth"

    // The same with sequences that stand on lines of their own, and with the end of the nesting in one step.
    testCase "a sequence nested to 1000 levels by indentation closes in one step" <| fun _ ->
        let sb = System.Text.StringBuilder()
        for level in 0 .. 999 do
            sb.Append(String.replicate level " ").Append("-\n") |> ignore
        sb.Append(String.replicate 1000 " ").Append("v\n").Append("# end\n") |> ignore
        let stream = parseOk (sb.ToString())
        Expect.stringContains (Projection.data stream) "\"maxDepth\":1000" "The depth"
]

// The inputs of the scaling tests. The reference is a single pass over the characters.

let private mappingInput (lines: int) =
    let sb = System.Text.StringBuilder()
    for i in 0 .. lines - 1 do
        if i % 10 = 0 then
            sb.Append(sprintf "n%d:\n  a: 1\n  b: two words\n" i) |> ignore
        else
            sb.Append(sprintf "key%d: value %d\n" i i) |> ignore
    sb.ToString()

let private sequenceInput (lines: int) =
    let sb = System.Text.StringBuilder()
    sb.Append("items:\n") |> ignore
    for i in 0 .. lines - 1 do
        if i % 10 = 0 then
            sb.Append(sprintf "- id: %d\n  run: tool.cwl\n" i) |> ignore
        else
            sb.Append(sprintf "- item %d\n" i) |> ignore
    sb.ToString()

let private commentInput (lines: int) =
    let sb = System.Text.StringBuilder()
    sb.Append("a:\n  b:\n    c: 1\n") |> ignore
    for i in 0 .. lines - 1 do
        if i % 3 = 0 then sb.Append(sprintf "      # comment %d\n" i) |> ignore
        elif i % 3 = 1 then sb.Append("\n") |> ignore
        else sb.Append(sprintf "  # comment %d\n" i) |> ignore
    sb.ToString()

let private interleavedCommentInput (lines: int) =
    let sb = System.Text.StringBuilder()
    for i in 0 .. lines - 1 do
        sb.Append(sprintf "# about %d\nk%d: v # trailing %d\n" i i i) |> ignore
    sb.ToString()

let private blockScalarInput (lines: int) =
    let sb = System.Text.StringBuilder()
    for i in 0 .. lines - 1 do
        sb.Append(sprintf "k%d: |\n  line a %d\n  line b\n" i i) |> ignore
    sb.ToString()

let private documentInput (lines: int) =
    let sb = System.Text.StringBuilder()
    for i in 0 .. lines - 1 do
        sb.Append(sprintf "---\nk: %d\n" i) |> ignore
    sb.ToString()

let private propertyInput (lines: int) =
    let sb = System.Text.StringBuilder()
    sb.Append("base: &base\n  a: 1\n") |> ignore
    for i in 0 .. lines - 1 do
        sb.Append(sprintf "k%d: !!str &a%d value\nr%d: *base\n" i i i) |> ignore
    sb.ToString()

let private parseOnly (input: string) = Parser.parseStream input |> ignore

/// The builder of a scaling test with a check that the parser accepts its input and that the tree passes Validate.stream. expectLinear builds each input once,
/// outside the timed runs, and the timed runs ignore the result of the parser.
let private accepted (build: int -> string) : int -> string =
    fun lines ->
        let input = build lines
        parseOk input |> ignore
        input

let private performanceTests = testList "Parser performance" [
    testCase "block mappings take time linear in the length of the input" <| fun _ ->
        expectLinear "The parser on block mappings" parseOnly countLineBreaks (accepted mappingInput)

    testCase "block sequences take time linear in the length of the input" <| fun _ ->
        expectLinear "The parser on block sequences" parseOnly countLineBreaks (accepted sequenceInput)

    testCase "comments at the end of a nested block take time linear in the length of the input" <| fun _ ->
        expectLinear "The parser on trailing comments" parseOnly countLineBreaks (accepted commentInput)

    testCase "a comment in front of every entry takes time linear in the length of the input" <| fun _ ->
        expectLinear "The parser on interleaved comments" parseOnly countLineBreaks (accepted interleavedCommentInput)

    testCase "block scalars take time linear in the length of the input" <| fun _ ->
        expectLinear "The parser on block scalars" parseOnly countLineBreaks (accepted blockScalarInput)

    testCase "a stream of many documents takes time linear in the length of the input" <| fun _ ->
        expectLinear "The parser on many documents" parseOnly countLineBreaks (accepted documentInput)

    testCase "properties and aliases take time linear in the length of the input" <| fun _ ->
        expectLinear "The parser on properties and aliases" parseOnly countLineBreaks (accepted propertyInput)
]

let rec private hasStructuredKey (node: YamlNode) : bool =
    match node.Kind with
    | YamlNodeKind.Mapping collection ->
        collection.Items
        |> List.exists (fun entry ->
            (match entry.Key.Kind with
             | YamlNodeKind.Scalar _ -> false
             | _ -> true)
            || hasStructuredKey entry.Key
            || hasStructuredKey entry.Value)
    | YamlNodeKind.Sequence collection -> collection.Items |> List.exists (fun item -> hasStructuredKey item.Node)
    | _ -> false

// The cases of the N1 task, from N1-scratch/cases.jsonl. Each input has the yaml@2.6.1 value or error codes in the data. The cases with a flow collection are here too since M4: the rejected ones in the list below, N1-D02, N1-D03 and N1-D27 in the table of Tests.Flow and N1-F06 to N1-F10 in Tests.Keys.
let private n1Rejected : (string * string) list = [
    "N1-A01", "k: \"q\"\n  x"
    "N1-A03", "k: [a]\n  x"
    "N1-A09", "s: &a x\nk: *a\n  [b]"
    "N1-A16", "k: [a]\n  [b]"
    "N1-A20", "- k: [a]\n    b"
    "N1-A22", "k: [a,\n  b]\n  x"
    "N1-B04", "k:\n  [a]\n  b"
    "N1-G04", "k2:\n   - [ \"x\", 'y' ]\n  v"
    "N1-G05", "key:\n   \"a #b\": [1, 2]\n  x y: 3\" wide"
    "N1-A02", "- \"q\"\n  x"
    "N1-A04", "s: &a x\nk: *a\n  x"
    "N1-A05", "s: &a x\nl:\n- *a\n  b"
    "N1-A06", "s: &a x\nl:\n  - *a\n    b"
    "N1-A07", "s: &a x\nk: *a\n  j: b"
    "N1-A08", "s: &a x\nk: *a\n  - b"
    "N1-A10", "s: &a x\nk: *a # c\n  b"
    "N1-A11", "s: &a x\nk: *a\n\n  b"
    "N1-A12", "s: &a x\nk: *a\n# c\n  b"
    "N1-A13", "s: &a x\nl:\n- *a\n    - b"
    "N1-A14", "k: 'q'\n  j: b"
    "N1-A15", "k: \"q\"\n  - b"
    "N1-A17", "k: \"q\" # c\n  x"
    "N1-A18", "k: \"q\"\n\n  x"
    "N1-A19", "k: \"q\"\n# c\n  x"
    "N1-A21", "k: !!str \"q\"\n  x"
    "N1-A23", "k: \"q\"\n  \"b\""
    "N1-A24", "k: a # c\n  b"
    "N1-A25", "? \"q\"\n  x"
    "N1-B01", "k:\n  \"a\"\n  \"b\""
    "N1-B02", "\"a\"\n\"b\""
    "N1-B03", "s: &a x\nk:\n  *a\n  b"
    "N1-B05", "k:\n  \"a\"\n  j: 1"
    "N1-B06", "--- \"a\"\n\"b\""
    "N1-B07", "k:\n  a # c\n  b"
    "N1-B08", "a: 1\nb\nc: 2"
    "N1-B09", "- a: 1\n  b"
    "N1-B10", "s: &a x\na: 1\n*a"
    "N1-B11", "a: !!str\nb"
    "N1-B12", "a: 1\n&x\nj: 2"
    "N1-B13", "k: v\n&x"
    "N1-B14", "k: v\n!!str\nj: w"
    "N1-B15", "? a\n:b"
    "N1-B16", "a:\n  b: 1\n  c"
    "N1-B17", "a: 1\n'q'"
    "N1-B18", "? a\n: 1\nb"
    "N1-B19", "a: 1\n|\n x"
    "N1-C01", "a: 1\n- x"
    "N1-C02", "- a\nb: 1"
    "N1-C03", "k: v\n- x\nj: 2"
    "N1-C04", "- a: 1\n  - b"
    "N1-C05", "? a\n- x"
    "N1-C06", "- a\n? b\n: c"
    "N1-C07", "- a\n#c\nb: 1"
    "N1-C08", "- a\nb"
    "N1-E01", "&a : v: w"
    "N1-E02", "!!str : v: w"
    "N1-E03", "- &a : v: w"
    "N1-E10", "- : v: w"
    "N1-G01", "a:\n  b: foo\n c: 1"
    "N1-G02", "- k: 3.5\n j: 1"
    "N1-G03", "\"a #b\":\n  - don't\n  - don't #\n x y: &anc 'u' # c"
    "N1-G06", "a:\n:  b: true\n  c: 1"
    "N1-G07", "a:\n  b: \"q\"\n c: 1"
]

let private n1Accepted : (string * string * string) list = [
    "N1-D01", "k: \"q\"\nj: 1", "{\"k\":\"q\",\"j\":1}"
    "N1-D04", "k:\n  \"a\"\nj: \"b\"", "{\"k\":\"a\",\"j\":\"b\"}"
    "N1-D05", "s: &a x\nl:\n- *a\n- b", "{\"s\":\"x\",\"l\":[\"x\",\"b\"]}"
    "N1-D06", "s: &a x\nk: *a\nj: b", "{\"s\":\"x\",\"k\":\"x\",\"j\":\"b\"}"
    "N1-D07", "s: &a x\nk:\n  *a\nj: b", "{\"s\":\"x\",\"k\":\"x\",\"j\":\"b\"}"
    "N1-D08", "s: &a x\nl:\n  - *a\n  - b", "{\"s\":\"x\",\"l\":[\"x\",\"b\"]}"
    "N1-D09", "s: &a x\nk: *a # c\n  # d\nj: 1", "{\"s\":\"x\",\"k\":\"x\",\"j\":1}"
    "N1-D10", "s: &a x\nk: *a # c\nj: 1", "{\"s\":\"x\",\"k\":\"x\",\"j\":1}"
    "N1-D11", "k:\n- x\nj: 1", "{\"k\":[\"x\"],\"j\":1}"
    "N1-D12", "k: !!seq\n- x", "{\"k\":[\"x\"]}"
    "N1-D13", "- k:\n  - x\n  - y\n- z", "[{\"k\":[\"x\",\"y\"]},\"z\"]"
    "N1-D14", "k:\n# c\n- x\n# d\nj: 1", "{\"k\":[\"x\"],\"j\":1}"
    "N1-D15", "- a\n# c\n- b", "[\"a\",\"b\"]"
    "N1-D16", "a: 1\n# c\nb: 2\n\nc: 3", "{\"a\":1,\"b\":2,\"c\":3}"
    "N1-D17", "? a\n: b\n? c\n: d", "{\"a\":\"b\",\"c\":\"d\"}"
    "N1-D18", "- ? a\n  : b\n- ? c\n  : d", "[{\"a\":\"b\"},{\"c\":\"d\"}]"
    "N1-D19", "k:\n  v", "{\"k\":\"v\"}"
    "N1-D20", "k:\n  &a\n  v", "{\"k\":\"v\"}"
    "N1-D21", "k: !!str\n  v", "{\"k\":\"v\"}"
    "N1-D23", "a:\n  b:\n    c: 1\n  d: 2\ne: 3", "{\"a\":{\"b\":{\"c\":1},\"d\":2},\"e\":3}"
    "N1-D24", "- - - a\n    - b\n  - c\n- d", "[[[\"a\",\"b\"],\"c\"],\"d\"]"
    "N1-D25", "- a\n-\n- b", "[\"a\",null,\"b\"]"
    "N1-D26", "steps:\n  - id: a\n    run: b\n    in:\n      x: y\n  - id: c", "{\"steps\":[{\"id\":\"a\",\"run\":\"b\",\"in\":{\"x\":\"y\"}},{\"id\":\"c\"}]}"
    "N1-D28", "k:\n\n  v", "{\"k\":\"v\"}"
    "N1-D29", "a:\n  - b\n    c\n  - d", "{\"a\":[\"b c\",\"d\"]}"
    "N1-D30", "a: b\n  c\nd: e", "{\"a\":\"b c\",\"d\":\"e\"}"
    "N1-D31", "? a\n:\n  - x\n  - y", "{\"a\":[\"x\",\"y\"]}"
    "N1-D32", "?\n  a\n  b\n: v", "{\"a b\":\"v\"}"
    "N1-D33", "\"a\": b\n? \"c\"\n: d", "{\"a\":\"b\",\"c\":\"d\"}"
    "N1-D34", "? a\n:\n- x\n- y", "{\"a\":[\"x\",\"y\"]}"
    "N1-E04", "&a : v", "{\"\":\"v\"}"
    "N1-E05", "!!str : v", "{\"\":\"v\"}"
    "N1-E06", "? a\n: b", "{\"a\":\"b\"}"
    "N1-E07", "? a\n: b: c", "{\"a\":{\"b\":\"c\"}}"
    "N1-E08", "- : v", "[{\"\":\"v\"}]"
    "N1-E09", ": v", "{\"\":\"v\"}"
    "N1-F17", "? |\n  a\n: v", "{\"a\\n\":\"v\"}"
    "N1-F18", "? a\n  b\n: v", "{\"a b\":\"v\"}"
    "N1-F19", "?\n  \"a\"\n: v", "{\"a\":\"v\"}"
    "N1-F20", "? \"a\"\n: v", "{\"a\":\"v\"}"
]

let private n1Collections : (string * string) list = [
    "N1-F01", "s: &a x\n*a : v"
    "N1-F02", "l:\n- &a x\n- *a : v"
    "N1-F03", "s: &a x\n? *a\n: v"
    "N1-F04", "s: &a x\n?\n  *a\n: v"
    "N1-F05", "x: &a 1\nk:\n  - *a : v"
    "N1-F11", "? - a\n: v"
    "N1-F12", "? a: b\n: c"
    "N1-F13", "?\n  - a\n  - b\n: v"
    "N1-F14", "?\n  a: 1\n  b: 2\n: v"
    "N1-F15", "?\n- a\n: b"
    "N1-F16", "? !!seq\n  - a\n: v"
]

let private n1Tests = testList "N1 cases" [
    // yaml@2.6.1 rejects most of these inputs (BAD_INDENT, MISSING_CHAR, UNEXPECTED_TOKEN, MULTILINE_IMPLICIT_KEY, BLOCK_AS_IMPLICIT_KEY). It accepts
    // N1-A25 ({"q":null}) and N1-C05 ({"a":null}) and drops the text behind the node, which is a yaml quirk. The specification rejects all of them.
    testCase "a line that no open block owns, a second node at one indentation and a mixed collection are errors" <| fun _ ->
        for (id, input) in n1Rejected do
            match Parser.parseStream input with
            | Error _ -> ()
            | Ok _ -> failwithf "%s: the parser accepts %A" id input

    // yaml@2.6.1 value of each input: the data of the list.
    testCase "the valid layouts read as the yaml values" <| fun _ ->
        for (id, input, value) in n1Accepted do
            Expect.equal (valueJson (parseRoot input)) value (sprintf "%s: the value of %A" id input)

    // N1-D22 is "k: |" and "  text" and "j: >" and "  folded" without a final line break. yaml@2.6.1 reads {"k":"text\n","j":"folded\n"}. [165] b-chomped-last makes the end of the
    // input the end of the last line, so the clipped folded value has no final line feed here, and the line feed is back when the input has a line break at its end.
    testCase "N1-D22 reads the folded value without a final line feed at the end of the input" <| fun _ ->
        Expect.equal (valueJson (parseRoot "k: |\n  text\nj: >\n  folded")) "{\"k\":\"text\\n\",\"j\":\"folded\"}" "Without a line break at the end"
        Expect.equal (valueJson (parseRoot "k: |\n  text\nj: >\n  folded\n")) "{\"k\":\"text\\n\",\"j\":\"folded\\n\"}" "With a line break at the end"

    // The old project rejected these inputs, because it kept only scalar keys. yaml@2.6.1 resolves an alias to its value or builds a string
    // from a collection to make the key. The new model keeps the alias or the collection as the key node.
    testCase "an alias or a collection as a key is accepted" <| fun _ ->
        for (id, input) in n1Collections do
            Expect.isTrue (hasStructuredKey (parseRoot input)) (sprintf "%s: %A has a key that is no scalar" id input)
]

let private strTag = YamlTag.core "str"

let private advancedTests = testList "Advanced features" [
    // Tests.AdvancedFeatures "2.2.1 Primary tag handle" and "2.2.2 Named tag handle". yaml@2.6.1 reads both as "bar" with the tag tag:example.com,2000:app/foo.
    testCase "a tag handle of a TAG directive resolves to its prefix" <| fun _ ->
        let tagged = YamlNode.withTag (YamlTag.Named { Prefix = "tag:example.com,2000:app/"; Suffix = "foo" }) (dq "bar")
        expectRoot "%TAG ! tag:example.com,2000:app/\n---\n!foo \"bar\"\n" tagged
        expectRoot "%TAG !e! tag:example.com,2000:app/\n---\n!e!foo \"bar\"\n" tagged

    // Tests.AdvancedFeatures "3.1.1 Simple anchor and alias", "3.2.1 Alias reuse after anchor override" and "3.2.2 Anchor in sequence". yaml@2.6.1 resolves the aliases.
    // The tree keeps them symbolic, and an anchor name can be used again.
    testCase "anchors and aliases stay symbolic" <| fun _ ->
        expectRoot
            "First occurrence: &anchor Value\nSecond occurrence: *anchor"
            (mapOf [ "First occurrence", YamlNode.withAnchor "anchor" (sc "Value"); "Second occurrence", YamlNode.alias "anchor" ])
        expectRoot
            "First occurrence: &anchor Foo\nSecond occurrence: *anchor\nOverride anchor: &anchor Bar\nReuse anchor: *anchor"
            (mapOf [
                "First occurrence", YamlNode.withAnchor "anchor" (sc "Foo")
                "Second occurrence", YamlNode.alias "anchor"
                "Override anchor", YamlNode.withAnchor "anchor" (sc "Bar")
                "Reuse anchor", YamlNode.alias "anchor"
            ])
        expectRoot
            "hr:\n  - Mark McGwire\n  - &SS Sammy Sosa\nrbi:\n  - *SS\n  - Ken Griffey"
            (mapOf [
                "hr", seqOf [ sc "Mark McGwire"; YamlNode.withAnchor "SS" (sc "Sammy Sosa") ]
                "rbi", seqOf [ YamlNode.alias "SS"; sc "Ken Griffey" ]
            ])

    // Tests.AdvancedFeatures "4.1.1 Verbatim tags complex", "4.2.1 Secondary tag handle" and "4.3.1 Non-specific tag forces string". yaml@2.6.1: {"foo":"baz"},
    // {"foo":"bar"} and ["12",12,"12"] with the tags tag:yaml.org,2002:str and !bar. A verbatim tag has the empty prefix and its URI as the suffix (see the doc comment of YamlTag).
    testCase "verbatim, secondary and non-specific tags" <| fun _ ->
        let verbatim (uri: string) = YamlTag.Named { Prefix = ""; Suffix = uri }
        expectRoot
            "!<tag:yaml.org,2002:str> foo :\n  !<!bar> baz"
            (YamlNode.mapping [ { YamlNode.entry (YamlNode.withTag (verbatim "tag:yaml.org,2002:str") (sc "foo")) (YamlNode.withTag (verbatim "!bar") (sc "baz")) with Colon = Some(newLine None []) } ])
        expectRoot "foo: !!str bar" (mapOf [ "foo", YamlNode.withTag strTag (sc "bar") ])
        expectRoot "- \"12\"\n- 12\n- ! 12" (seqOf [ dq "12"; sc "12"; YamlNode.withTag YamlTag.NonSpecific (sc "12") ])

    // Tests.AdvancedFeatures "5.1.1 Explicit block mapping key", "5.1.4 Explicit key with tag and anchor", "5.1.5 Explicit quoted key preserves style" and "5.1.2 Sequence as
    // mapping key". yaml@2.6.1: {"explicit key":"explicit value"}, {"key":"value"} twice and {"[ Detroit Tigers, Chicago cubs ]":["2001-07-23"]}, where it makes a string key
    // from the sequence. The old project rejected the sequence key. The new model keeps the sequence as the key node.
    testCase "explicit keys with properties, a quoted key and a sequence as the key" <| fun _ ->
        expectRoot "? explicit key\n: explicit value" (YamlNode.mapping [ explicitEntry (sc "explicit key") (sc "explicit value") ])
        let keyed = withLayout [ placement PropertyName.Anchor Separation.Inline; placement PropertyName.Tag Separation.Inline ] (YamlNode.withTag (YamlTag.Named { Prefix = ""; Suffix = "tag:foo" }) (YamlNode.withAnchor "a1" (sc "key")))
        expectRoot "? &a1 !<tag:foo> key\n: value" (YamlNode.mapping [ explicitEntry keyed (sc "value") ])
        expectRoot "? &a1 \"@id\"\n: value" (YamlNode.mapping [ explicitEntry (YamlNode.withAnchor "a1" (dq "@id")) (sc "value") ])
        expectRoot
            "? - Detroit Tigers\n  - Chicago cubs\n: - 2001-07-23"
            (YamlNode.mapping [ explicitEntry (seqOf [ sc "Detroit Tigers"; sc "Chicago cubs" ]) (seqOf [ sc "2001-07-23" ]) ])

    // Tests.AdvancedFeatures "An explicit value reads the strings and comments of its own document", "An explicit value reads an indented block behind the colon", "An explicit value that
    // starts a block scalar keeps its body as written". yaml@2.6.1: {"k":"q","a":"x"}, {"a":1,"b":"c","d":2}, {"a":"b"}, {"k":"q","a":{"m":"v"}} and {"k":"q","a":"<c f=0/>\n"}.
    testCase "an explicit value after another entry" <| fun _ ->
        expectRoot "k: \"q\"\n? a\n: \"x\"" (YamlNode.mapping [ YamlNode.entry (sc "k") (dq "q"); explicitEntry (sc "a") (dq "x") ])
        expectRoot
            "a: 1\n? b\n: c # note\nd: 2"
            (YamlNode.mapping [ YamlNode.entry (sc "a") (sc "1"); explicitEntry (sc "b") (withNote " note" (sc "c")); YamlNode.entry (sc "d") (sc "2") ])
        expectRoot "? \"a\"\n: \"b\"" (YamlNode.mapping [ explicitEntry (dq "a") (dq "b") ])
        expectRoot
            "k: \"q\"\n? a\n:\n  m: \"v\""
            (YamlNode.mapping [ YamlNode.entry (sc "k") (dq "q"); { explicitEntry (sc "a") (mapOf [ "m", dq "v" ]) with Colon = Some(newLine None []) } ])
        expectRoot "k: \"q\"\n? a\n: |\n  <c f=0/>\n" (YamlNode.mapping [ YamlNode.entry (sc "k") (dq "q"); explicitEntry (sc "a") (lit "<c f=0/>\n") ])
        // The explicit key over several lines and the explicit value over several lines.
        expectRoot "k: \"q\"\n? a\n  b\n: \"x\"" (YamlNode.mapping [ YamlNode.entry (sc "k") (dq "q"); explicitEntry (sc "a b") (dq "x") ])
        expectRoot
            "k: \"q\"\n?\n  a\n  b\n: \"x\""
            (YamlNode.mapping [ YamlNode.entry (sc "k") (dq "q"); { explicitEntry (sc "a b") (dq "x") with Question = Some(newLine None []) } ])
        expectRoot "k: \"q\"\n? a\n: c\n  d" (YamlNode.mapping [ YamlNode.entry (sc "k") (dq "q"); explicitEntry (sc "a") (sc "c d") ])

    // Tests.AdvancedFeatures "Long explicit keys with quoted values between plain entries read their own values". yaml@2.6.1 reads one entry per line pair with the values "1641",
    // "1665", "1669" and "1687".
    testCase "long explicit keys with quoted values between plain entries" <| fun _ ->
        let longKey1 = "topica/long-branch-one/flow-entry-collection-assembled-from-records--first-pass--sample-merging-with-global-reference-copy-version-"
        let longKey2 = "topica/habitat-range-estimation/flow-entry-habitat-range-estimation-with-planner-from-sourcedata-sets---reference-library-copy"
        let yaml =
            "assembly/a/first-process: '1641'\n"
            + "? " + longKey1 + "\n: '1665'\n"
            + "topicb/radio-scanning/satellite-1-samples: '1669'\n"
            + "? " + longKey2 + "\n: '1687'\n"
        expectRoot
            yaml
            (YamlNode.mapping [
                YamlNode.entry (sc "assembly/a/first-process") (sq "1641")
                explicitEntry (sc longKey1) (sq "1665")
                YamlNode.entry (sc "topicb/radio-scanning/satellite-1-samples") (sq "1669")
                explicitEntry (sc longKey2) (sq "1687")
            ])

    // Tests.AdvancedFeatures "7.1 Properties on their own line belong to the plain scalar behind them". yaml@2.6.1: {"k":"a b"} twice, {"k":"a"} and ["a b"].
    testCase "properties on their own line belong to the plain scalar behind them" <| fun _ ->
        let multiline = tagOnOwnLine strTag (sc "a b")
        expectRoot "k: !!str\n  a\n  b" (mapOf [ "k", multiline ])
        expectRoot "k:\n  !!str\n  a\n  b" (YamlNode.mapping [ entryOnNewLine "k" multiline ])
        expectRoot "k: &x\n  a" (mapOf [ "k", anchorOnOwnLine "x" (sc "a") ])
        expectRoot "- !!str\n  a\n  b" (seqOf [ multiline ])

    // Tests.AdvancedFeatures "7.2 Properties on their own line belong to the block scalar behind them". yaml@2.6.1: {"k":"x\n"} and ["y"].
    testCase "properties on their own line belong to the block scalar behind them" <| fun _ ->
        // [165] b-chomped-last: the last content line ends the input, so the clipped value has no final line feed. yaml@2.6.1 reads {"k":"x\n"}.
        expectRoot "k: !t\n  |\n    x" (mapOf [ "k", tagOnOwnLine (YamlTag.local "t") (lit "x") ])
        expectRoot "k: !t\n  |\n    x\n" (mapOf [ "k", tagOnOwnLine (YamlTag.local "t") (lit "x\n") ])
        expectRoot "- &a\n  >-\n    y" (seqOf [ anchorOnOwnLine "a" (blockScalar BlockScalarStyle.Folded ChompingMode.Strip None "y") ])

    // Tests.AdvancedFeatures "7.3 Core tags on their own line before a block mapping or sequence add no empty scalar" and "7.11 An anchor or another tag on a collection is an error".
    // yaml@2.6.1: [{"k":"v"}], {"k":{"a":1}}, [["x"]], {"k":["a"]}, {"base":{"k":"v"}}, {"k":["x"]}, [{"k":"v"}] and {"k":["x"]}. The old project had no field for the
    // properties of a collection, so it dropped the core tags and rejected the others. The new tree keeps the properties of every collection.
    testCase "properties on a line of their own belong to the block collection behind them" <| fun _ ->
        expectRoot "- !!map\n  k: v" (seqOf [ tagOnOwnLine (YamlTag.core "map") (mapOf [ "k", sc "v" ]) ])
        expectRoot "k: !!map\n  a: 1" (mapOf [ "k", tagOnOwnLine (YamlTag.core "map") (mapOf [ "a", sc "1" ]) ])
        expectRoot "- !!seq\n  - x" (seqOf [ tagOnOwnLine (YamlTag.core "seq") (seqOf [ sc "x" ]) ])
        expectRoot "k: !!seq\n  - a" (mapOf [ "k", tagOnOwnLine (YamlTag.core "seq") (seqOf [ sc "a" ]) ])
        expectRoot "base: &b\n  k: v" (mapOf [ "base", anchorOnOwnLine "b" (mapOf [ "k", sc "v" ]) ])
        expectRoot "k: !custom\n  - x" (mapOf [ "k", tagOnOwnLine (YamlTag.local "custom") (seqOf [ sc "x" ]) ])
        expectRoot "k: &a\n- x" (mapOf [ "k", anchorOnOwnLine "a" (seqOf [ sc "x" ]) ])
        expectRoot "- &a\n  k: v" (seqOf [ anchorOnOwnLine "a" (mapOf [ "k", sc "v" ]) ])
        expectRoot "k: !!str\n  - x" (mapOf [ "k", tagOnOwnLine strTag (seqOf [ sc "x" ]) ])
        expectRoot "k: !<tag:yaml.org,2002:seq>\n  - a" (mapOf [ "k", tagOnOwnLine (YamlTag.Named { Prefix = ""; Suffix = "tag:yaml.org,2002:seq" }) (seqOf [ sc "a" ]) ])
        expectRoot "--- !!map\nk: v" (tagOnOwnLine (YamlTag.core "map") (mapOf [ "k", sc "v" ]))

    // Tests.AdvancedFeatures "7.4 A comment behind properties on their own line stays a comment". yaml@2.6.1: {"k":"a"}. The comment goes to the separation of the tag (rule R1).
    testCase "a comment behind properties on their own line goes to the last property" <| fun _ ->
        expectRoot
            "k: !!str # c\n  a"
            (mapOf [ "k", withLayout [ placement PropertyName.Tag (newLine (Some " c") []) ] (YamlNode.withTag strTag (sc "a")) ])

    // Tests.AdvancedFeatures "7.5 A node cannot have two tags or two anchors". yaml@2.6.1 reports "A node can have at most one tag" and "A node can have at most one anchor".
    testCase "two tags or two anchors on one node are an error also across lines" <| fun _ ->
        Expect.equal (parseErrorOf "k: !!str\n  !!int a") "duplicate-tag@2" "Two tags"
        Expect.equal (parseErrorOf "k: &x\n  &y a") "duplicate-anchor@2" "Two anchors"

    // Tests.AdvancedFeatures "7.9 Two tags or two anchors on one line are an error". yaml@2.6.1 rejects each input, and reads one tag and one anchor in either order.
    testCase "two tags or two anchors on one line are an error" <| fun _ ->
        for (input, code) in [ "k: !!str !!int 1", "duplicate-tag"; "k: &a &b 1", "duplicate-anchor"; "k: !!str &a !!int 1", "duplicate-tag"; "- &a &b x", "duplicate-anchor"
                               "? !!str !!int a\n: v", "duplicate-tag"; "&a &b k: v", "duplicate-anchor" ] do
            Expect.equal (parseErrorOf input) (code + "@1") (sprintf "The error of %A" input)
        let tagFirst = withLayout [ placement PropertyName.Tag Separation.Inline; placement PropertyName.Anchor Separation.Inline ] (YamlNode.withAnchor "a" (YamlNode.withTag strTag (sc "1")))
        let anchorFirst = withLayout [ placement PropertyName.Anchor Separation.Inline; placement PropertyName.Tag Separation.Inline ] tagFirst
        expectRoot "k: !!str &a 1" (mapOf [ "k", tagFirst ])
        expectRoot "k: &a !!str 1" (mapOf [ "k", anchorFirst ])
        Expect.equal (valueJson (parseRoot "k: !!str &a 1")) (valueJson (parseRoot "k: &a !!str 1")) "Both read as the same value"

    // Tests.AdvancedFeatures "7.6 A blank line between properties and the node behind them is part of the node". yaml@2.6.1: {"k":"a"} and {"k":{"a":1}}.
    testCase "a blank line between properties and the node behind them stays in the tree" <| fun _ ->
        let scalar = withLayout [ placement PropertyName.Tag (newLine None [ blank ]) ] (YamlNode.withTag strTag (sc "a"))
        expectRoot "k: !!str\n\n  a" (mapOf [ "k", scalar ])
        let collection = tagOnOwnLine (YamlTag.core "map") (YamlNode.mapping [ entryBefore [ blank ] (sc "a") (sc "1") ])
        expectRoot "k: !!map\n\n  a: 1" (mapOf [ "k", collection ])

    // Tests.AdvancedFeatures "7.8 A key with a core tag alone owns the sequence at the indentation of the key". yaml@2.6.1 reads spec Example 8.22 as
    // {"sequence":["entry",["nested"]],"mapping":{"foo":"bar"}}, and the other inputs as {"k":["x"]}, {"a":{"k":["x","y"],"m":1}} and {"k":["x"]}.
    testCase "a core tag in front of an indentless sequence belongs to the sequence" <| fun _ ->
        let seqTag = YamlTag.core "seq"
        expectRoot
            "sequence: !!seq\n- entry\n- !!seq\n - nested\nmapping: !!map\n foo: bar"
            (mapOf [
                "sequence", tagOnOwnLine seqTag (seqOf [ sc "entry"; tagOnOwnLine seqTag (seqOf [ sc "nested" ]) ])
                "mapping", tagOnOwnLine (YamlTag.core "map") (mapOf [ "foo", sc "bar" ])
            ])
        expectRoot "k: !!seq\n- x" (mapOf [ "k", tagOnOwnLine seqTag (seqOf [ sc "x" ]) ])
        expectRoot
            "a:\n  k: !!seq\n  - x\n  - y\n  m: 1"
            (mapOf [ "a", mapOf [ "k", tagOnOwnLine seqTag (seqOf [ sc "x"; sc "y" ]); "m", sc "1" ] ])
        expectRoot
            "k: !!seq # c\n# d\n- x"
            (mapOf [ "k", withLayout [ placement PropertyName.Tag (newLine (Some " c") []) ] (YamlNode.withTag seqTag (YamlNode.sequence [ itemBefore [ note " d" ] (sc "x") ])) ])
]

let private blockScalarStructureTests = testList "Block scalars in the structure" [
    // Tests.BlockScalars "A comment before a scalar header precedes the key", "A comment after a scalar body belongs to the next sibling or to the dedented node" and
    // "A comment at the end of the input stays with the node that owns the scalar". yaml@2.6.1: {"a":"x\n"}, {"top":{"k":1,"s":"body\n"}}, {"a":{"s":"body\n","t":1}},
    // {"a":{"s":"body\n"},"b":1} and {"a":{"s":"body\n"}}.
    testCase "the comments around a block scalar go to the Before of the next entry or to the End of the collection that they close" <| fun _ ->
        expectRoot "# lead\na: |\n  x\n" (YamlNode.mapping [ entryBefore [ note " lead" ] (sc "a") (lit "x\n") ])
        expectRoot
            "top:\n  k: 1\n  # c\n  s: |\n    body\n"
            (mapOf [ "top", YamlNode.mapping [ YamlNode.entry (sc "k") (sc "1"); entryBefore [ note " c" ] (sc "s") (lit "body\n") ] ])
        expectRoot
            "a:\n  s: |\n    body\n  # c\n  t: 1\n"
            (mapOf [ "a", YamlNode.mapping [ YamlNode.entry (sc "s") (lit "body\n"); entryBefore [ note " c" ] (sc "t") (sc "1") ] ])
        expectRoot
            "a:\n  s: |\n    body\n# c\nb: 1\n"
            (YamlNode.mapping [ blockEntry (sc "a") (mapOf [ "s", lit "body\n" ]); entryBefore [ note " c" ] (sc "b") (sc "1") ])
        expectRoot "a:\n  s: |\n    body\n  # c\n" (mapOf [ "a", withEnd [ note " c" ] (mapOf [ "s", lit "body\n" ]) ])
        expectRoot "a:\n  s: |\n    body\n# c\n" (withEnd [ note " c" ] (mapOf [ "a", mapOf [ "s", lit "body\n" ] ]))

    // Tests.BlockScalars "Comments between sequence items that hold scalars keep their place", "A comment behind the first scalar item of an indentless sequence stays between the items" and
    // "A comment after a root scalar stays behind it". yaml@2.6.1: {"x":["a\n","b\n"]}, ["a","x\n","b"], {"key":["x\ny\n","z"]} and "x\n".
    testCase "a comment between two items that hold block scalars goes to the second item" <| fun _ ->
        expectRoot
            "x:\n  - |\n    a\n  # c\n  - |\n    b\n"
            (mapOf [ "x", YamlNode.sequence [ YamlNode.item (lit "a\n"); itemBefore [ note " c" ] (lit "b\n") ] ])
        expectRoot "- a\n- |\n  x\n# c\n- b\n" (YamlNode.sequence [ YamlNode.item (sc "a"); YamlNode.item (lit "x\n"); itemBefore [ note " c" ] (sc "b") ])
        expectRoot "key:\n- |\n  x\n  y\n# c\n- z\n" (mapOf [ "key", YamlNode.sequence [ YamlNode.item (lit "x\ny\n"); itemBefore [ note " c" ] (sc "z") ] ])
        expectDocument "|\n  x\n# c\n" { YamlDocument.create (lit "x\n") with AfterRoot = [ note " c" ] }

    // Tests.BlockScalars "A comment behind the header and a comment behind the body are different comments" and "Tags and anchors on scalars with comments around them".
    // yaml@2.6.1: {"a":"x\n","b":1}, {"a":"x\n","b":"y\n"}, "x\n", "x\n" and ["x\n","y"].
    testCase "the header comment, the tag and the anchor of a block scalar" <| fun _ ->
        let withHeader = YamlNode.withHeaderComment (YamlComment.create " hc") (lit "x\n")
        expectRoot "a: | # hc\n  x\n# c\nb: 1\n" (YamlNode.mapping [ YamlNode.entry (sc "a") withHeader; entryBefore [ note " c" ] (sc "b") (sc "1") ])
        expectRoot
            "a: !!str |\n  x\n# c\nb: &x |\n  y\n"
            (YamlNode.mapping [
                YamlNode.entry (sc "a") (YamlNode.withTag strTag (lit "x\n"))
                entryBefore [ note " c" ] (sc "b") (YamlNode.withAnchor "x" (lit "y\n"))
            ])
        expectRoot "!!str |\n  x\n" (YamlNode.withTag strTag (lit "x\n"))
        expectRoot "- !!str |\n  x\n- &a >-\n  y\n" (seqOf [ YamlNode.withTag strTag (lit "x\n"); YamlNode.withAnchor "a" (blockScalar BlockScalarStyle.Folded ChompingMode.Strip None "y") ])
        expectDocument "&a |\n  x\n# c\n" { YamlDocument.create (YamlNode.withAnchor "a" (lit "x\n")) with AfterRoot = [ note " c" ] }

    // Tests.BlockScalars "Several comments at different indentation keep their order across blank lines" and "Comments at the end of the input stay in the block of the scalar header".
    // yaml@2.6.1: {"a":"x\n","b":1} and {"a":{"b":"x\n"}}. The comments in front of a sibling go to its Before in the order of the text. The comments at the end go by their columns.
    testCase "several comments and blank lines after a block scalar keep their order" <| fun _ ->
        expectRoot
            "a: |\n    x\n  # c1\n # c2\n\n# c3\nb: 1\n"
            (YamlNode.mapping [ YamlNode.entry (sc "a") (lit "x\n"); entryBefore [ note " c1"; note " c2"; blank; note " c3" ] (sc "b") (sc "1") ])
        expectRoot
            "a:\n  b: |\n      x\n    # c1\n\n  # c2\n\n# c3\n"
            (withEnd [ blank; note " c3" ] (mapOf [ "a", withEnd [ note " c1"; blank; note " c2" ] (mapOf [ "b", lit "x\n" ]) ]))
        expectRoot "a: |+\n  x\n\n\n# c\nb: 1\n" (YamlNode.mapping [ YamlNode.entry (sc "a") (blockScalar BlockScalarStyle.Literal ChompingMode.Keep None "x\n\n\n"); entryBefore [ note " c" ] (sc "b") (sc "1") ])
        expectRoot "a: |+\n    x\n\n  # c\nb: 1\n" (YamlNode.mapping [ YamlNode.entry (sc "a") (blockScalar BlockScalarStyle.Literal ChompingMode.Keep None "x\n\n"); entryBefore [ note " c" ] (sc "b") (sc "1") ])
        expectRoot "- k: |\n    text\n# about next\n  next: 1\n" (seqOf [ YamlNode.mapping [ YamlNode.entry (sc "k") (lit "text\n"); entryBefore [ note " about next" ] (sc "next") (sc "1") ] ])

    // Tests.BlockScalars "A trailing comment between two levels joins the outer level of a scalar item". yaml@2.6.1: {"k":["x\n"]} and {"k":[""]}. The comment at 2 spaces lies between the root
    // and the dash at 4, so it goes to the End of the root mapping.
    testCase "a comment between two levels behind a scalar item goes to the outer collection" <| fun _ ->
        expectRoot "k:\n    - |\n      x\n  # note\n" (withEnd [ note " note" ] (mapOf [ "k", seqOf [ lit "x\n" ] ]))
        expectRoot
            "k:\n    - |4-\n\n  # note\n"
            (withEnd [ blank; note " note" ] (mapOf [ "k", seqOf [ blockScalar BlockScalarStyle.Literal ChompingMode.Strip (Some 4) "" ] ]))

    // Tests.BlockScalars "An empty block scalar reads as the empty string as mapping value, sequence item, root scalar, explicit key, nested item and explicit key item".
    // yaml@2.6.1: {"a":"","b":1}, ["","x"], "", {"a":""}, {"":null}, {"":"v"}, [[""],"x"] and [{"":null},"x"].
    testCase "an empty block scalar is the empty string in every position" <| fun _ ->
        expectRoot "a: |\nb: 1\n" (mapOf [ "a", lit ""; "b", sc "1" ])
        expectRoot "- |\n- x\n" (seqOf [ lit ""; sc "x" ])
        expectRoot "|\n" (lit "")
        expectRoot "a: |" (mapOf [ "a", lit "" ])
        expectRoot "? |\n" (YamlNode.mapping [ explicitKeyOnly (lit "") ])
        expectRoot "? |\n: v\n" (YamlNode.mapping [ explicitEntry (lit "") (sc "v") ])
        expectRoot "- - |\n- x\n" (seqOf [ seqOf [ lit "" ]; sc "x" ])
        expectRoot "- ? |\n- x\n" (seqOf [ YamlNode.mapping [ explicitKeyOnly (lit "") ]; sc "x" ])
        expectRoot "? |\n  x\n: v\n" (YamlNode.mapping [ explicitEntry (lit "x\n") (sc "v") ])

    // Tests.BlockScalars "A comment line that ends a scalar before more indented content is an error with locations" and the boundary tests. yaml@2.6.1 rejects each of these inputs
    // with BAD_INDENT (or MISSING_CHAR or UNEXPECTED_TOKEN where noted in the old tests). The lexer reports a line that is deeper than the parent and shallower than the content as
    // block-indent. When a comment ended the scalar before, the text is a deeper line behind a node that ended, which the parser reports as bad-indentation.
    testCase "more indented text behind a block scalar is an error at the line of the text" <| fun _ ->
        for (input, expected) in
            [ "a: >-\n  text\n# c\n  more\n", "bad-indentation@4"
              "a: |\n    x\n  # c\n   bad\n", "bad-indentation@4"
              "x:\n  a: |\n      body\n    # c\n     bad\n", "bad-indentation@5"
              "a: |\n    x\n  # c\n    bad\n", "bad-indentation@4"
              "a: |\n  x\n# c\n    bad\n", "bad-indentation@4"
              "a: |2\n    x\n # c\n      bad\n", "bad-indentation@4"
              "a: |\n  x\n\n# c1\n   # c2\n\n# c3\n  bad\n", "bad-indentation@8"
              "q: 'a\n  b'\ns: |\n  x\n# c\n  bad\n", "bad-indentation@6"
              "a: |\r\n  x\r\n# c\r\n  bad\r\n", "bad-indentation@4"
              "- |\n    x\n  # c\n  - y\n", "bad-indentation@4"
              "a: |\n    text\n  more\n", "block-indent@3"
              "a: |4\n  x\nb: 1\n", "block-indent@2"
              "- - |\n     x\n   y\n", "block-indent@3"
              "? |\n   x\n  : v\n", "block-indent@3"
              "- ? |\n     x\n   : v\n", "block-indent@3"
              "- !!str |\n    x\n  y\n", "block-indent@3"
              "- &a |\n    x\n  y\n", "block-indent@3"
              "--- |\n    x\n  y\n", "block-indent@3"
              "a: |\n  x\n# c\n  bad\n---\nb: 1\n", "bad-indentation@4" ] do
            Expect.equal (parseErrorOf input) expected (sprintf "The error of %A" input)

    // Tests.BlockScalars "The boundary of a nested sequence item is its own dash", "The boundary of an explicit key is its question mark", "A property in front of the header does not move the
    // boundary", "The boundary behind a document start marker is the column of the marker" and "A comment block after a scalar is no error at the end of the input or before a sibling".
    // yaml@2.6.1 reads these inputs as [["x\n","y"]], {"x\n":"v"}, [{"x\n":"v"}], ["x\n","y"], "x\n" and {"a":"x\n","b":1}.
    testCase "a block scalar ends at the column of the node that owns it" <| fun _ ->
        expectRoot "- - |\n     x\n  - y\n" (seqOf [ seqOf [ lit "x\n"; sc "y" ] ])
        expectRoot "? |\n   x\n: v\n" (YamlNode.mapping [ explicitEntry (lit "x\n") (sc "v") ])
        expectRoot "- ? |\n     x\n  : v\n" (seqOf [ YamlNode.mapping [ explicitEntry (lit "x\n") (sc "v") ] ])
        expectRoot "- !!str |\n    x\n- y\n" (seqOf [ YamlNode.withTag strTag (lit "x\n"); sc "y" ])
        expectRoot "- &a |\n    x\n- y\n" (seqOf [ YamlNode.withAnchor "a" (lit "x\n"); sc "y" ])
        expectRoot "--- |\n    x\n...\n" (lit "x\n")
        expectRoot
            "a: |\n    x\n  # c\n\n  # d\nb: 1\n"
            (YamlNode.mapping [ YamlNode.entry (sc "a") (lit "x\n"); entryBefore [ note " c"; blank; note " d" ] (sc "b") (sc "1") ])
        expectRoot "a: |\n    x\n  # c\n" (withEnd [ note " c" ] (mapOf [ "a", lit "x\n" ]))
]

let private moreTests = testList "More ports" [
    // Tests.YamlRead "Nested mapping accepts structural blank line and comment before child block". yaml@2.6.1: {"inputs":{"a":1}}. The blank line and the comment are trivia in front of the
    // first entry of the child mapping (rules R2 and R4).
    testCase "a blank line and a comment in front of the child block go to its first entry" <| fun _ ->
        expectRoot "inputs:\n\n  # comment\n  a: 1\n" (mapOf [ "inputs", YamlNode.mapping [ entryBefore [ blank; note " comment" ] (sc "a") (sc "1") ] ])

    // Tests.YamlRead "Comments between the items of an indentless sequence stay in the sequence". yaml@2.6.1: {"a":{"items":["x","y"],"after":1}}. The comment in front of the first item
    // and the one between the items go to the Before of the item behind them.
    testCase "comments between the items of an indentless sequence go to the items" <| fun _ ->
        expectRoot
            "a:\n  items:\n  # first\n  - x\n  # between\n  - y\n  after: 1"
            (mapOf [
                "a",
                YamlNode.mapping [
                    blockEntry (sc "items") (YamlNode.sequence [ itemBefore [ note " first" ] (sc "x"); itemBefore [ note " between" ] (sc "y") ])
                    YamlNode.entry (sc "after") (sc "1")
                ]
            ])

    // Tests.YamlRead "Quoted key with a hash keeps its block scalar content" and "Single-quoted key with a hash keeps its block scalar content". yaml@2.6.1: {"a #b":"'it''s'\n"} and {"x #y":"'a\nb'\n"}.
    testCase "a hash inside a quoted key does not hide the block scalar header" <| fun _ ->
        expectRoot "\"a #b\": |\n  'it''s'\n" (YamlNode.mapping [ YamlNode.entry (dq "a #b") (lit "'it''s'\n") ])
        expectRoot "'x #y': |\n  'a\n  b'\n" (YamlNode.mapping [ YamlNode.entry (sq "x #y") (lit "'a\nb'\n") ])

    // Tests.YamlRead "Indented multiline plain scalar expression decodes as one string" and "Inline-start multiline plain scalar expression decodes as one string".
    // yaml@2.6.1: {"valueFrom":"${ var picks = null; if (this !== null) { picks = [this]; } return picks; }"} and {"valueFrom":"${ var picks = null; return picks; }"}.
    testCase "a multiline plain scalar expression is one scalar" <| fun _ ->
        expectRoot
            "valueFrom:\n  ${\n    var picks = null;\n    if (this !== null) { picks = [this]; }\n    return picks;\n  }"
            (YamlNode.mapping [ entryOnNewLine "valueFrom" (sc "${ var picks = null; if (this !== null) { picks = [this]; } return picks; }") ])
        expectRoot "valueFrom: ${\n  var picks = null;\n  return picks;\n  }" (mapOf [ "valueFrom", sc "${ var picks = null; return picks; }" ])

    // Tests.YamlRead "Key followed by blank lines and unindented sequence", "Key followed by blank lines, comments, and unindented sequence" and "Blank lines between sequence items do not
    // split the sequence". yaml@2.6.1: {"root":["a","b"]} for each.
    testCase "an unindented sequence behind its key" <| fun _ ->
        expectRoot "root:\n- a\n- b" (mapOf [ "root", seqOf [ sc "a"; sc "b" ] ])
        expectRoot "root:\n# keep\n- a\n- b" (mapOf [ "root", YamlNode.sequence [ itemBefore [ note " keep" ] (sc "a"); YamlNode.item (sc "b") ] ])
        expectRoot "root:\n\n- a\n\n- b" (mapOf [ "root", YamlNode.sequence [ itemBefore [ blank ] (sc "a"); itemBefore [ blank ] (sc "b") ] ])

    // Tests.YamlRead "Block scalar literal". yaml@2.6.1 reads the value of doc as the lines of the block, with the more indented lines and the list lines as written.
    testCase "a literal block scalar with more indented lines and list lines" <| fun _ ->
        let yaml = "doc: |\n  Counting example pipeline for **statistical test on grouped samples**\n  \n  This pipeline runs Tester on the output of the counting pipeline\n  and the settings file.\n  It runs a big script, tester.R, which probably needs a split into several pieces and accordingly several pipeline stages\n    1. Load counts tables\n    2. Prep / run tester\n    3. Draw summary\n\n  ## Tester docs:\n    https://docs.example.org/modules/current/core2/html/Tester.html\n\n  ## Importing counting output with importer\n    https://docs.example.org/modules/current/core2/guidebook/importer/inst/doc/importer.html#counting\n\n  ## Shared image collections\n  - Toolkits plus groups of needed software libraries can be found as shared image collections from [ImageDepot](https://github.com/ImageDepot/shared-image-collections)\n  - Searched for `repo:ImageDepot/shared-image-collections tester importer h5-files`\n  - and found `reg.example.io/images/bundle-v2-0123456789abcdef0123456789abcdef01234567:fedcba9876543210fedcba9876543210fedcba98-0` :tada:"
        let block = "Counting example pipeline for **statistical test on grouped samples**\n\nThis pipeline runs Tester on the output of the counting pipeline\nand the settings file.\nIt runs a big script, tester.R, which probably needs a split into several pieces and accordingly several pipeline stages\n  1. Load counts tables\n  2. Prep / run tester\n  3. Draw summary\n\n## Tester docs:\n  https://docs.example.org/modules/current/core2/html/Tester.html\n\n## Importing counting output with importer\n  https://docs.example.org/modules/current/core2/guidebook/importer/inst/doc/importer.html#counting\n\n## Shared image collections\n- Toolkits plus groups of needed software libraries can be found as shared image collections from [ImageDepot](https://github.com/ImageDepot/shared-image-collections)\n- Searched for `repo:ImageDepot/shared-image-collections tester importer h5-files`\n- and found `reg.example.io/images/bundle-v2-0123456789abcdef0123456789abcdef01234567:fedcba9876543210fedcba9876543210fedcba98-0` :tada:\n"
        // [165] b-chomped-last: the input ends with the last content line, so the clipped value has no final line feed (yaml@2.6.1 keeps it).
        expectRoot yaml (mapOf [ "doc", lit (block.Substring(0, block.Length - 1)) ])
        expectRoot (yaml + "\n") (mapOf [ "doc", lit block ])

    // Tests.BlockScalars "Literal block scalar keeps a more indented hash line as content", "keeps two spaces before a deeper hash line", "keeps mixed indentation depths with hash lines", "Root block scalar
    // keeps the exact indentation of a more indented line" and "Later sequence item block scalars keep their indentation". A hash line inside the body is content.
    testCase "the lines of a body keep their indentation relative to the least indented line" <| fun _ ->
        expectRoot "key: |\n    text\n     # more\n" (mapOf [ "key", lit "text\n # more\n" ])
        expectRoot "key: |\n    text\n      # two\n" (mapOf [ "key", lit "text\n  # two\n" ])
        expectRoot "key: |\n    a\n     # b\n         c\n       # d\n    e\n" (mapOf [ "key", lit "a\n # b\n     c\n   # d\ne\n" ])
        expectRoot "|\n  x\n   y\n" (lit "x\n y\n")
        expectRoot "- a\n- |\n  x\n   y\n- b\n" (seqOf [ sc "a"; lit "x\n y\n"; sc "b" ])

    // Tests.BlockScalars "Comment less indented than the content ends a folded block scalar", "Trailing comment between the header and the content indentation ends the block scalar" and
    // "Keep chomping stops before a comment that is less indented than the content". yaml@2.6.1: {"key":"folded\n","next":1}, {"x":{"a":"body\n"}}, {"a":"x\n"} and {"a":"x\n\n","b":1}.
    testCase "a comment less indented than the content ends the block scalar" <| fun _ ->
        expectRoot
            "key: >\n    folded\n  # c\nnext: 1\n"
            (YamlNode.mapping [ YamlNode.entry (sc "key") (fold "folded\n"); entryBefore [ note " c" ] (sc "next") (sc "1") ])
        expectRoot "x:\n  a: |\n      body\n    # c\n" (mapOf [ "x", withEnd [ note " c" ] (mapOf [ "a", lit "body\n" ]) ])
        expectRoot "a: |\n    x\n  # c\n" (withEnd [ note " c" ] (mapOf [ "a", lit "x\n" ]))
        expectRoot
            "a: |+\n    x\n\n  # c\nb: 1\n"
            (YamlNode.mapping [ YamlNode.entry (sc "a") (blockScalar BlockScalarStyle.Literal ChompingMode.Keep None "x\n\n"); entryBefore [ note " c" ] (sc "b") (sc "1") ])
        expectRoot
            "outer:\n  key: |+\n    x\n\nnext: 1\n"
            (YamlNode.mapping [ blockEntry (sc "outer") (mapOf [ "key", blockScalar BlockScalarStyle.Literal ChompingMode.Keep None "x\n\n" ]); YamlNode.entry (sc "next") (sc "1") ])

    // Tests.YamlGenericRead "Example Mermaid". yaml@2.6.1 reads the input as the plain scalar "classDiagram Animal <|-- Duck ... }". The braces and the angle brackets are text in a plain scalar
    // of the block context. The old test checked the scalar table of the scanner.
    testCase "a Mermaid diagram is one plain scalar over many lines" <| fun _ ->
        let yaml =
            "\nclassDiagram\n    Animal <|-- Duck\n    Animal <|-- Fish\n    Animal <|-- Zebra\n    class Duck{\n      +String beakColor\n      +swim()\n      +quack()\n    }\n"
            + "    class Fish{\n      -int sizeInFeet\n      -canEat()\n    }\n    class Zebra{\n      +bool is_wild\n      +run()\n    }\n"
        let text =
            "classDiagram Animal <|-- Duck Animal <|-- Fish Animal <|-- Zebra class Duck{ +String beakColor +swim() +quack() } class Fish{ -int sizeInFeet -canEat() } class Zebra{ +bool is_wild +run() }"
        expectDocument yaml { YamlDocument.create (sc text) with BeforeRoot = [ blank ] }

    // Tests.YamlGenericRead "Example Comment + String". yaml@2.6.1: {"My Key":"My Value1 \"# This is not a comment!\" My Value3"}. The lines below the key are one plain scalar. The comment behind the key
    // goes to the separation of the colon (rule R1), the comment behind the last line to the scalar (rule R8), and a quote inside the scalar is text.
    testCase "a plain scalar with a quote and a hash, between two comments" <| fun _ ->
        let value = withNote " :::: \"This is also a comment\"" (sc "My Value1 \"# This is not a comment!\" My Value3")
        let entry = { YamlNode.entry (sc "My Key") value with Colon = Some(newLine (Some " This is a comment") []); Before = [ blank ] }
        expectRoot "\nMy Key: # This is a comment\n  My Value1 \n  \"# This is not a comment!\"\n  My Value3 # :::: \"This is also a comment\"" (YamlNode.mapping [ entry ])

    // Tests.BlockScalars "readDocuments rejects the boundary error too". yaml@2.6.1 rejects the first document. The error names the line of the text behind the comment.
    testCase "the boundary error of a block scalar in the first of two documents" <| fun _ ->
        Expect.equal (parseErrorOf "a: |\n  x\n# c\n  bad\n---\nb: 1\n") "bad-indentation@4" "The line of the text"
]

let Main =
    testList "Parser" [
        mappingTests
        sequenceTests
        explicitKeyTests
        propertyTests
        moreTests
        advancedTests
        blockScalarStructureTests
        errorTests
        n1Tests
        spanTests
        depthTests
        performanceTests
    ]
