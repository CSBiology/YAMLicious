module Tests.AdvancedFeatures

open Fable.Pyxpecto
open YAMLicious
open YAMLiciousTypes

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
]
