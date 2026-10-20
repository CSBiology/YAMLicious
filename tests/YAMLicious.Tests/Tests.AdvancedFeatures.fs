module Tests.AdvancedFeatures

open Fable.Pyxpecto
open YAMLicious
open YAMLiciousTypes

let private doubleQuoted (value: string) = YAMLContent.create(value, style=ScalarStyle.DoubleQuoted)

let private singleQuoted (value: string) = YAMLContent.create(value, style=ScalarStyle.SingleQuoted)

/// A mapping entry whose value is the one scalar.
let private scalarEntry (key: YAMLContent) (value: YAMLContent) =
    YAMLElement.Mapping(key, YAMLElement.Object [ YAMLElement.Value value ])

let Main = testList "Advanced Features" [
    // --- Section 1: Multi-Document Stream Support ---

    testCase "2.2.1 Primary tag handle" <| fun _ ->
        let yaml = """%TAG ! tag:example.com,2000:app/
---
!foo "bar"
"""
        let actual = Reader.read yaml
        let expected = YAMLElement.Object [
            YAMLElement.Value(YAMLContent.create("bar", tag="tag:example.com,2000:app/foo", style=ScalarStyle.DoubleQuoted))
        ]
        Expect.equal actual expected "Primary tag handle ! should resolve"

    testCase "2.2.2 Named tag handle" <| fun _ ->
        let yaml = """%TAG !e! tag:example.com,2000:app/
---
!e!foo "bar"
"""
        let actual = Reader.read yaml
        let expected = YAMLElement.Object [
            YAMLElement.Value(YAMLContent.create("bar", tag="tag:example.com,2000:app/foo", style=ScalarStyle.DoubleQuoted))
        ]
        Expect.equal actual expected "Named tag handle !e! should resolve"


    // --- Section 3: Anchors and Aliases ---
    testCase "3.1.1 Simple anchor and alias" <| fun _ ->
        let yaml = """First occurrence: &anchor Value
Second occurrence: *anchor"""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("First occurrence"),
                YAMLElement.Object [YAMLElement.Value(YAMLContent.create("Value", anchor="anchor"))]
            )
            YAMLElement.Mapping(
                YAMLContent.create("Second occurrence"),
                YAMLElement.Object [YAMLElement.Alias("anchor")]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Anchor and Alias should be correctly parsed"

    testCase "3.2.1 Alias reuse after anchor override" <| fun _ ->
        let yaml = """First occurrence: &anchor Foo
Second occurrence: *anchor
Override anchor: &anchor Bar
Reuse anchor: *anchor"""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(YAMLContent.create("First occurrence"), YAMLElement.Object [YAMLElement.Value(YAMLContent.create("Foo", anchor="anchor"))])
            YAMLElement.Mapping(YAMLContent.create("Second occurrence"), YAMLElement.Object [YAMLElement.Alias("anchor")])
            YAMLElement.Mapping(YAMLContent.create("Override anchor"), YAMLElement.Object [YAMLElement.Value(YAMLContent.create("Bar", anchor="anchor"))])
            YAMLElement.Mapping(YAMLContent.create("Reuse anchor"), YAMLElement.Object [YAMLElement.Alias("anchor")])
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Alias nodes should be preserved"

    testCase "3.2.2 Anchor in sequence" <| fun _ ->
        let yaml = """hr:
  - Mark McGwire
  - &SS Sammy Sosa
rbi:
  - *SS
  - Ken Griffey"""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(YAMLContent.create("hr"), YAMLElement.Object [
                YAMLElement.Sequence [
                    YAMLElement.Object [YAMLElement.Value(YAMLContent.create("Mark McGwire"))]
                    YAMLElement.Object [YAMLElement.Value(YAMLContent.create("Sammy Sosa", anchor="SS"))]
                ]
            ])
            YAMLElement.Mapping(YAMLContent.create("rbi"), YAMLElement.Object [
                YAMLElement.Sequence [
                    YAMLElement.Object [YAMLElement.Alias("SS")]
                    YAMLElement.Object [YAMLElement.Value(YAMLContent.create("Ken Griffey"))]
                ]
            ])
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Anchors in sequences should be correctly parsed"

    // --- Section 4: Tags ---
    testCase "4.1.1 Verbatim tags complex" <| fun _ ->
        let yaml = """!<tag:yaml.org,2002:str> foo :
  !<!bar> baz"""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("foo", tag="tag:yaml.org,2002:str"),
                YAMLElement.Object [
                    YAMLElement.Value(YAMLContent.create("baz", tag="!bar"))
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Verbatim tags on keys and values should be handled"

    testCase "4.2.1 Secondary tag handle" <| fun _ ->
        let yaml = "foo: !!str bar"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("foo"),
                YAMLElement.Object [
                    YAMLElement.Value(YAMLContent.create("bar", tag="tag:yaml.org,2002:str"))
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Secondary tag handle !! should resolve"

    testCase "4.3.1 Non-specific tag forces string" <| fun _ ->
        let yaml = """- "12"
- 12
- ! 12"""
        let actual = Reader.read yaml
        match actual with
        | YAMLElement.Object [YAMLElement.Sequence [v1; v2; v3]] ->
            match v1, v2, v3 with
            | YAMLElement.Object [YAMLElement.Value c1], YAMLElement.Object [YAMLElement.Value c2], YAMLElement.Object [YAMLElement.Value c3] ->
                Expect.equal c3.Tag (Some "!") "Third item should have non-specific tag '!'"
            | _ -> failwith "Unexpected sequence structure"
        | _ -> failwith "Unexpected root structure"

    // --- Section 5: Complex Mapping Keys ---
    testCase "5.1.1 Explicit block mapping key" <| fun _ ->
        let yaml = """? explicit key
: explicit value"""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("explicit key"),
                YAMLElement.Object [YAMLElement.Value(YAMLContent.create("explicit value"))]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Explicit key indicator '?' should be handled"

    testCase "5.1.4 Explicit key with tag and anchor" <| fun _ ->
        let yaml = """? &a1 !<tag:foo> key
: value"""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("key", anchor="a1", tag="tag:foo"),
                YAMLElement.Object [YAMLElement.Value(YAMLContent.create("value"))]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Explicit keys should support anchors and tags"

    testCase "5.1.5 Explicit quoted key preserves style" <| fun _ ->
        let yaml = """? &a1 "@id"
: value"""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("@id", anchor="a1", style=ScalarStyle.DoubleQuoted),
                YAMLElement.Object [YAMLElement.Value(YAMLContent.create("value"))]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Explicit quoted keys should restore placeholder content and preserve style"

    testCase "5.1.2 Sequence as mapping key" <| fun _ ->
        let yaml = """? - Detroit Tigers
  - Chicago cubs
: - 2001-07-23"""
        let actual = Reader.read yaml
        let expectedKey = "- Detroit Tigers\n- Chicago cubs"
        // Since we flatten complex keys to string for now:
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                 YAMLContent.create(expectedKey),
                 YAMLElement.Object [YAMLElement.Sequence [YAMLElement.Object [YAMLElement.Value(YAMLContent.create("2001-07-23"))]]]
            )
        ]
        Expect.equal actual expected "Complex key (sequence) should be flattened string"

    testCase "5.1.3 Nested mapping as key" <| fun _ ->
        let yaml = """? [ New York Yankees,
    Atlanta Braves ]
: [ 2001-07-02, 2001-08-12,
    2001-08-14 ]"""
        let actual = Reader.read yaml
        let expectedKey = "[ New York Yankees,\nAtlanta Braves ]"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                 YAMLContent.create(expectedKey),
                 YAMLElement.Object [
                     YAMLElement.Sequence [
                         YAMLElement.Object[YAMLElement.Value(YAMLContent.create("2001-07-02"))]
                         YAMLElement.Object[YAMLElement.Value(YAMLContent.create("2001-08-12"))]
                         YAMLElement.Object[YAMLElement.Value(YAMLContent.create("2001-08-14"))]
                     ]
                 ]
            )
        ]
        Expect.equal actual expected "Nested mapping key should be flattened string"

    testCase "An explicit value reads the strings and comments of its own document" <| fun _ ->
        // yaml@2.6.1: {"k":"q","a":"x"}, {"a":1,"b":"c","d":2} and {"a":"b"}
        Expect.equal
            (Reader.read "k: \"q\"\n? a\n: \"x\"")
            (YAMLElement.Object [ scalarEntry (YAMLContent.create "k") (doubleQuoted "q"); scalarEntry (YAMLContent.create "a") (doubleQuoted "x") ])
            "A quoted value behind another quoted value"
        Expect.equal
            (Reader.read "a: 1\n? b\n: c # note\nd: 2")
            (YAMLElement.Object [
                scalarEntry (YAMLContent.create "a") (YAMLContent.create "1")
                scalarEntry (YAMLContent.create "b") (YAMLContent.create("c", comment=" note"))
                scalarEntry (YAMLContent.create "d") (YAMLContent.create "2")
            ])
            "A comment behind the value"
        Expect.equal
            (Reader.read "? \"a\"\n: \"b\"")
            (YAMLElement.Object [ scalarEntry (doubleQuoted "a") (doubleQuoted "b") ])
            "A quoted key and a quoted value"

    testCase "An explicit value reads an indented block behind the colon" <| fun _ ->
        // yaml@2.6.1: {"k":"q","a":{"m":"v"}}
        let expected =
            YAMLElement.Object [
                scalarEntry (YAMLContent.create "k") (doubleQuoted "q")
                YAMLElement.Mapping(
                    YAMLContent.create "a",
                    YAMLElement.Object [ scalarEntry (YAMLContent.create "m") (doubleQuoted "v") ]
                )
            ]
        Expect.equal (Reader.read "k: \"q\"\n? a\n:\n  m: \"v\"") expected "A mapping behind the colon"

    testCase "An explicit value reads a flow sequence with a quoted item" <| fun _ ->
        // yaml@2.6.1: {"k":"q","a":["x","y"]}
        let expected =
            YAMLElement.Object [
                scalarEntry (YAMLContent.create "k") (doubleQuoted "q")
                YAMLElement.Mapping(
                    YAMLContent.create "a",
                    YAMLElement.Object [
                        YAMLElement.Sequence [
                            YAMLElement.Object [ YAMLElement.Value(YAMLContent.create "x") ]
                            YAMLElement.Object [ YAMLElement.Value(doubleQuoted "y") ]
                        ]
                    ]
                )
            ]
        Expect.equal (Reader.read "k: \"q\"\n? a\n: [x, \"y\"]") expected "A flow sequence on the colon line"

    testCase "An explicit value that starts a block scalar keeps its body as written" <| fun _ ->
        // yaml@2.6.1: {"k":"q","a":"<c f=0/>\n"}
        let expected =
            YAMLElement.Object [
                scalarEntry (YAMLContent.create "k") (doubleQuoted "q")
                scalarEntry
                    (YAMLContent.create "a")
                    (YAMLContent.create("<c f=0/>\n", style=ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Clip, None)))
            ]
        Expect.equal (Reader.read "k: \"q\"\n? a\n: |\n  <c f=0/>\n") expected "The body text is not a placeholder"

    testCase "Long explicit keys with quoted values between plain entries read their own values" <| fun _ ->
        // yaml@2.6.1 reads one entry per line pair with the values "1641", "1665", "1669" and "1687". This is
        // the shape of the entries in a long list of workflow ids and their keys.
        let longKey1 = "topica/long-branch-one/flow-entry-collection-assembled-from-records--first-pass--sample-merging-with-global-reference-copy-version-"
        let longKey2 = "topica/habitat-range-estimation/flow-entry-habitat-range-estimation-with-planner-from-sourcedata-sets---reference-library-copy"
        let yaml =
            "assembly/a/first-process: '1641'\n"
            + "? " + longKey1 + "\n: '1665'\n"
            + "topicb/radio-scanning/satellite-1-samples: '1669'\n"
            + "? " + longKey2 + "\n: '1687'\n"
        let expected =
            YAMLElement.Object [
                scalarEntry (YAMLContent.create "assembly/a/first-process") (singleQuoted "1641")
                scalarEntry (YAMLContent.create longKey1) (singleQuoted "1665")
                scalarEntry (YAMLContent.create "topicb/radio-scanning/satellite-1-samples") (singleQuoted "1669")
                scalarEntry (YAMLContent.create longKey2) (singleQuoted "1687")
            ]
        Expect.equal (Reader.read yaml) expected "Every value is the one that stands behind its key"

    testCase "An explicit key on several lines and a multiline explicit value read from the document" <| fun _ ->
        // yaml@2.6.1: {"k":"q","a b":"x"} for the first two inputs and {"k":"q","a":"c d"} for the last one.
        // The reader keeps the line break of a plain scalar on several lines and gives it the plain style.
        let expected =
            YAMLElement.Object [
                scalarEntry (YAMLContent.create "k") (doubleQuoted "q")
                scalarEntry (YAMLContent.create("a\nb", style=ScalarStyle.Plain)) (doubleQuoted "x")
            ]
        Expect.equal (Reader.read "k: \"q\"\n? a\n  b\n: \"x\"") expected "The key continues on the next line"
        Expect.equal (Reader.read "k: \"q\"\n?\n  a\n  b\n: \"x\"") expected "The key starts on the next line"
        Expect.equal
            (Reader.read "k: \"q\"\n? a\n: c\n  d")
            (YAMLElement.Object [
                scalarEntry (YAMLContent.create "k") (doubleQuoted "q")
                scalarEntry (YAMLContent.create "a") (YAMLContent.create("c\nd", style=ScalarStyle.Plain))
            ])
            "The value continues on the next line"

    // --- Section 6: Single-Quoted Scalars ---

    testCase "6.1.1 Single-quoted with escaped quote" <| fun _ ->
        let yaml = """single: 'here''s to "quotes"'"""
        let actual = Reader.read yaml
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                 YAMLContent.create("single"),
                 YAMLElement.Object [YAMLElement.Value(YAMLContent.create("here's to \"quotes\"", style=ScalarStyle.SingleQuoted))]
            )
        ]
        Expect.equal actual expected "Should handle escaped single quotes"

    testCase "6.1.2 Single-quoted preserves backslashes" <| fun _ ->
        let yaml = """tie-fighter: '|\-*-/|'"""
        let actual = Reader.read yaml
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                 YAMLContent.create("tie-fighter"),
                 YAMLElement.Object [YAMLElement.Value(YAMLContent.create("|\\-*-/|", style=ScalarStyle.SingleQuoted))]
            )
        ]
        Expect.equal actual expected "Should preserve backslashes literal"

    testCase "6.1.3 Multi-line single-quoted" <| fun _ ->
        let yaml = """single: ' 1st non-empty

 2nd non-empty 
 3rd non-empty '"""
        let actual = Reader.read yaml
        let expectedValue = " 1st non-empty\n2nd non-empty 3rd non-empty "
        match actual with
        | YAMLElement.Object [YAMLElement.Mapping(k, YAMLElement.Object [YAMLElement.Value value])] ->
            Expect.equal k.Value "single" "Mapping key should be parsed"
            Expect.equal value.Value expectedValue "Multiline single-quoted scalar should preserve semantic folding"
            Expect.equal value.Style (Some ScalarStyle.SingleQuoted) "Single-quoted style should be preserved"
        | _ ->
            failwithf "Unexpected AST: %A" actual
    // --- Section 7: Tags and anchors on a line of their own ---

    // yaml@2.6.1 reads `k: !!str` followed by `a` and `b` on indented lines as {"k":"a b"}, and the same with the
    // tag on a line of its own below `k:`. It reads `k: &x` followed by `a` as {"k":"a"} and `- !!str` followed by
    // `a` and `b` as ["a b"].
    testCase "7.1 Properties on their own line belong to the plain scalar behind them" <| fun _ ->
        let strTag = "tag:yaml.org,2002:str"
        let multiline = YAMLContent.create("a\nb", tag=strTag, style=ScalarStyle.Plain)
        let expected = YAMLElement.Object [ scalarEntry (YAMLContent.create "k") multiline ]
        Expect.equal (Reader.read "k: !!str\n  a\n  b") expected "The tag stands on the key line"
        Expect.equal (Reader.read "k:\n  !!str\n  a\n  b") expected "The tag stands on a line of its own"
        Expect.equal
            (Reader.read "k: &x\n  a")
            (YAMLElement.Object [ scalarEntry (YAMLContent.create "k") (YAMLContent.create("a", anchor="x")) ])
            "The anchor stands on the key line"
        Expect.equal
            (Reader.read "- !!str\n  a\n  b")
            (YAMLElement.Object [ YAMLElement.Sequence [ YAMLElement.Object [ YAMLElement.Value multiline ] ] ])
            "The tag stands behind a dash"
        let decoded = Reader.read "k: !!str\n  a\n  b" |> Decode.object (fun get -> get.Required.Field "k" Decode.string)
        Expect.equal decoded "a b" "The value has no leading line break"

    // yaml@2.6.1 reads `k: !t` followed by `|` and `x` on indented lines as {"k":"x\n"}, and `- &a` followed by
    // `>-` and `y` as ["y"].
    testCase "7.2 Properties on their own line belong to the block scalar behind them" <| fun _ ->
        let block (style: BlockScalarStyle) (chomp: ChompingMode) = ScalarStyle.Block(style, chomp, None)
        Expect.equal
            (Reader.read "k: !t\n  |\n    x")
            (YAMLElement.Object [ scalarEntry (YAMLContent.create "k") (YAMLContent.create("x\n", tag="!t", style=block BlockScalarStyle.Literal ChompingMode.Clip)) ])
            "The tag stands on the key line"
        Expect.equal
            (Reader.read "- &a\n  >-\n    y")
            (YAMLElement.Object [ YAMLElement.Sequence [ YAMLElement.Object [ YAMLElement.Value(YAMLContent.create("y", anchor="a", style=block BlockScalarStyle.Folded ChompingMode.Strip)) ] ] ])
            "The anchor stands behind a dash"

    // yaml@2.6.1 reads these inputs as [{"k":"v"}], {"k":{"a":1}}, [["x"]] and {"k":["a"]}. The element model has no
    // field for the properties of a mapping or a sequence, so the reader drops them. It adds no empty scalar.
    testCase "7.3 Properties on their own line before a block mapping or sequence add no empty scalar" <| fun _ ->
        let value (text: string) = YAMLElement.Object [ YAMLElement.Value(YAMLContent.create text) ]
        let entry (key: string) (node: YAMLElement) = YAMLElement.Mapping(YAMLContent.create key, node)
        Expect.equal
            (Reader.read "- !!map\n  k: v")
            (YAMLElement.Object [ YAMLElement.Sequence [ YAMLElement.Object [ entry "k" (value "v") ] ] ])
            "A tagged mapping behind a dash"
        Expect.equal
            (Reader.read "k: !!map\n  a: 1")
            (YAMLElement.Object [ entry "k" (YAMLElement.Object [ entry "a" (value "1") ]) ])
            "A tagged mapping behind a key"
        Expect.equal
            (Reader.read "- &a\n  - x")
            (YAMLElement.Object [ YAMLElement.Sequence [ YAMLElement.Object [ YAMLElement.Sequence [ value "x" ] ] ] ])
            "An anchored sequence behind a dash"
        Expect.equal
            (Reader.read "k: &x\n  - a")
            (YAMLElement.Object [ entry "k" (YAMLElement.Object [ YAMLElement.Sequence [ value "a" ] ]) ])
            "An anchored sequence behind a key"

    // yaml@2.6.1 reads `k: !!str # c` followed by `a` as {"k":"a"}. The comment stands in front of the scalar.
    testCase "7.4 A comment behind properties on their own line stays a comment" <| fun _ ->
        Expect.equal
            (Reader.read "k: !!str # c\n  a")
            (YAMLElement.Object [
                YAMLElement.Mapping(
                    YAMLContent.create "k",
                    YAMLElement.Object [
                        YAMLElement.Comment " c"
                        YAMLElement.Value(YAMLContent.create("a", tag="tag:yaml.org,2002:str"))
                    ]
                )
            ])
            "The comment comes first and the scalar keeps the tag"

    // yaml@2.6.1 reports "A node can have at most one tag" and "A node can have at most one anchor" for these inputs.
    testCase "7.5 A node cannot have two tags or two anchors" <| fun _ ->
        Expect.throws (fun _ -> Reader.read "k: !!str\n  !!int a" |> ignore) "Two tags"
        Expect.throws (fun _ -> Reader.read "k: &x\n  &y a" |> ignore) "Two anchors"

    // yaml@2.6.1 reads `k: !!str` followed by a blank line and `a` as {"k":"a"}, and `k: &x` followed by a blank
    // line and `a: 1` as {"k":{"a":1}}.
    testCase "7.6 A blank line between properties and the node behind them is part of the node" <| fun _ ->
        Expect.equal
            (Reader.read "k: !!str\n\n  a")
            (Reader.read "k: !!str\n  a")
            "The scalar reads as without the blank line"
        Expect.equal
            (Reader.read "k: &x\n\n  a: 1")
            (Reader.read "k:\n  a: 1")
            "The mapping reads as without the blank line"

    // yaml@2.6.1 reads each written text again as the same value. The tag and the anchor survive the writer.
    testCase "7.7 A writer round trip keeps the tag and the anchor of the scalars" <| fun _ ->
        for text in [ "k: !!str\n  a\n  b"; "k: &x\n  a"; "- !!str\n  a\n  b"; "k: !t\n  |\n    x"; "- &a\n  >-\n    y" ] do
            let written = Writer.write (Reader.read text) None
            let reread = Reader.read written
            Expect.equal (Writer.write reread None) written $"The text of {text} is stable"
            let contents (element: YAMLElement) =
                let rec collect (el: YAMLElement) =
                    match el with
                    | YAMLElement.Value content -> [ content.Value, content.Tag, content.Anchor ]
                    | YAMLElement.Mapping (_, value) -> collect value
                    | YAMLElement.Object items
                    | YAMLElement.Sequence items -> List.collect collect items
                    | _ -> []
                collect element
            Expect.equal (contents reread) (contents (Reader.read text)) $"The values, tags and anchors of {text} survive"
]
