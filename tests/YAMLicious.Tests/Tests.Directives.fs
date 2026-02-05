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
    testCase "2.1.2 Invalid duplicate YAML directive" <| fun _ ->
        let yaml = "%YAML 1.2\n%YAML 1.1\n---\nfoo: bar"
        Expect.throws (fun _ -> Preprocessing.read yaml |> ignore) "Should throw on duplicate YAML directive"
]
