module Tests.YamlMatch

open Fable.Pyxpecto
open YAMLicious
open YAMLicious.PatternMatcher

let Main = testList "YamlMatch" [
    testCase "sdöklf" <| fun _ ->
        Expect.equal 1 1 ""
    //testCase "Match KeyValue " <| fun _ ->
    //    let actual = matcher "My Key: My Value"
    //    Expect.equal actual "KeyValue" ""
    //testCase "Match Value " <| fun _ ->
    //    let actual = matcher "My Value"
    //    Expect.equal actual "Value" ""
    //testCase "Match Sequence" <| fun _ ->
    //    let actual = matcher "- My Value 1"
    //    Expect.equal actual "Sequence" ""
]