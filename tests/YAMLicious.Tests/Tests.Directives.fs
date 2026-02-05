module Tests.Directives

open Fable.Pyxpecto
open YAMLicious
open YAMLiciousTypes
open Preprocessing

let Main = testList "Directives" [
    testCase "YAML directive 1.2" <| fun _ ->
        let yaml = "%YAML 1.2\n---\nfoo: bar"
        let pre = Preprocessing.read yaml
        Expect.equal pre.YAMLVersion (Some { Major = 1; Minor = 2 }) "YAML version should be 1.2"

    testCase "TAG directive" <| fun _ ->
        let yaml = "%TAG !m! !mytag:\n---\nfoo: bar"
        let pre = Preprocessing.read yaml
        Expect.isTrue (Map.containsKey "!m!" pre.TagHandles) "Tag handle !m! should exist"
        Expect.equal (Map.find "!m!" pre.TagHandles) "!mytag:" "Tag handle value should match"

    testCase "Anchor and Alias" <| fun _ ->
        let yaml = "foo: &a1 bar\nbaz: *a1"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(YAMLContent.create("foo"), YAMLElement.Object [YAMLElement.Value(YAMLContent.create("bar", anchor="a1"))])
            YAMLElement.Mapping(YAMLContent.create("baz"), YAMLElement.Object [YAMLElement.Alias("a1")])
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Anchor and Alias should be correctly parsed"

    testCase "Verbatim Tag" <| fun _ ->
        let yaml = "foo: !<tag:yaml.org,2002:str> bar"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("foo"),
                YAMLElement.Object [
                    YAMLElement.Value(YAMLContent.create("bar", tag="tag:yaml.org,2002:str"))
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Verbatim tag should be correctly parsed"
]
