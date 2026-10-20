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

    testCase "Match SequenceSquareOpener" <| fun _ ->
        let yamlElement = Line "["
        match yamlElement with
        | SequenceSquareOpener res -> 
            Expect.isNone res.Comment ""
        | _ -> Expect.isFalse true ""

    testCase "Match SequenceSquareOpener + Comment" <| fun _ ->
        let yamlElement = Line "[ <c f=1/>"
        match yamlElement with
        | SequenceSquareOpener res -> 
            Expect.equal res.Comment (Some 1) ""
        | _ -> Expect.isFalse true ""

    testCase "Match SequenceSquareCloser" <| fun _ ->
        let yamlElement = Line "]"
        match yamlElement with
        | SequenceSquareCloser res -> 
            Expect.isNone res.Comment ""
        | _ -> Expect.isFalse true ""

    testCase "Match SequenceSquareCloser + Comment" <| fun _ ->
        let yamlElement = Line "] <c f=1/>"
        match yamlElement with
        | SequenceSquareCloser res -> 
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

    testCase "Match InlineSequence" <| fun _ ->
        let yamlElement = Line "[abc, def, hij]"
        match yamlElement with
        | InlineSequence res ->
            Expect.equal res.Value "abc, def, hij" ""
            Expect.isNone res.Comment ""
        | _ -> Expect.isFalse true ""

    testCase "Match InlineSequence + Comment" <| fun _ ->
        let yamlElement = Line "[abc, def, hij] <c f=11/>"
        match yamlElement with
        | InlineSequence res ->
            Expect.equal res.Value "abc, def, hij" ""
            Expect.equal res.Comment (Some 11) ""
        | _ -> Expect.isFalse true ""
]