module Tests.YamlMatch

open Fable.Pyxpecto
open YAMLicious
open YAMLiciousTypes
open Preprocessing
open RegexActivePatterns

let Main = testList "YamlMatch" [
    testCase "Match Key" <| fun _ ->
        let yamlElement = Line "My Key:"
        match yamlElement with
        | Key res -> 
            Expect.equal res.Key "My Key" ""
            Expect.isNone res.Comment ""
        | _ -> Expect.isFalse true ""

    testCase "Match Key + Comment" <| fun _ ->
        let yamlElement = Line "My Key: <c f=1/>"
        match yamlElement with
        | Key res -> 
            Expect.equal res.Key "My Key" ""
            Expect.equal res.Comment (Some 1) ""
        | _ -> Expect.isFalse true ""

    testCase "Match KeyValue" <| fun _ ->
        let yamlElement = Line "My Key: My Value"
        match yamlElement with
        | KeyValue res -> 
            Expect.equal res.Key "My Key" ""
            Expect.equal res.Value "My Value" ""
        | _ -> Expect.isFalse true ""

    testCase "Match Value" <| fun _ ->
        let yamlElement = Line "My Value"
        match yamlElement with
        | YamlValue res -> 
            Expect.equal res.Value "My Value" ""
            Expect.isNone res.Comment ""
        | _ -> Expect.isFalse true ""

    testCase "Match Value + Comment" <| fun _ ->
        let yamlElement = Line "My Value <c f=1/>"
        match yamlElement with
        | YamlValue res -> 
            Expect.equal res.Value "My Value" ""
            Expect.equal res.Comment (Some 1) ""
        | _ -> Expect.isFalse true ""

    testCase "Match SequenceMinusOpener" <| fun _ ->
        let yamlElement = Line "-"
        match yamlElement with
        | SequenceMinusOpener res -> 
            Expect.isNone res.Value ""
        | _ -> Expect.isFalse true ""

    testCase "Match SequenceMinusOpener + Value" <| fun _ ->
        let yamlElement = Line "- My Value"
        match yamlElement with
        | SequenceMinusOpener res -> 
            Expect.equal res.Value (Some "My Value") ""
        | _ -> Expect.isFalse true ""

    testCase "Match SequenceMinusOpener + Comment" <| fun _ ->
        let yamlElement = Line "- <c f=1/>"
        match yamlElement with
        | SequenceMinusOpener res -> 
            Expect.equal res.Value (Some "<c f=1/>") ""
        | _ -> Expect.isFalse true ""

    testCase "Match Comment" <| fun _ ->
        let yamlElement = Line "<c f=11/>"
        match yamlElement with
        | YamlComment res -> 
            Expect.equal res.Comment 11 ""
        | _ -> Expect.isFalse true ""

    testCase "Match FlowStyleArray" <| fun _ ->
        let yamlElement = Line "[abc, def, hij]"
        match yamlElement with
        | FlowStyleArray res ->
            Expect.equal res.Value "abc, def, hij" ""
            Expect.isNone res.Comment ""
        | _ -> Expect.isFalse true ""

    testCase "Match FlowStyleArray + Comment" <| fun _ ->
        let yamlElement = Line "[abc, def, hij] <c f=11/>"
        match yamlElement with
        | FlowStyleArray res ->
            Expect.equal res.Value "abc, def, hij" ""
            Expect.equal res.Comment (Some 11) ""
        | _ -> Expect.isFalse true ""

    // yaml@2.6.1 reads `x: &a:b 1` and `k: *a:b` as {"x":1,"k":1}. An alias name follows the rule of an anchor name.
    testCase "Match AliasNode with a colon in the name" <| fun _ ->
        for (text, name) in [ "*a", "a"; "*a:b", "a:b"; "*a:", "a:" ] do
            match Line text with
            | AliasNode res -> Expect.equal res name $"The name of {text}"
            | _ -> Expect.isFalse true $"{text} is an alias"
        match Line "*a : v" with
        | AliasNode _ -> Expect.isFalse true "A blank ends the name, so this line is an entry"
        | _ -> ()

    // yaml@2.6.1 reads `:a` as the string ":a", `:a: b` as {":a":"b"} and `- :a` as [":a"].
    testCase "Match ExplicitValue needs a blank behind the colon" <| fun _ ->
        for (text, value) in [ ": v", "v"; ":", ""; ":\tv", "v" ] do
            match Line text with
            | ExplicitValue res -> Expect.equal res value $"The value of {text}"
            | _ -> Expect.isFalse true $"{text} is an explicit value"
        for text in [ ":a"; ":a: b"; "::" ] do
            match Line text with
            | ExplicitValue _ -> Expect.isFalse true $"{text} is no explicit value"
            | _ -> ()
]