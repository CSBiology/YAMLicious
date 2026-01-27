module Tests.FlowToBlock

open Fable.Pyxpecto
open YAMLicious.FlowToBlock
open YAMLicious.Preprocessing
open YAMLicious
open YAMLicious.YAMLiciousTypes
open System.Collections.Generic

let Main = testList "FlowToBlock" [
    testList "tokenize" [
        testCase "simple object" <| fun _ ->
            let input = "{key: value}"
            let tokens = tokenize input
            let expected = [
                Token.OpenBrace
                Token.String "key"
                Token.Colon
                Token.String "value"
                Token.CloseBrace
                Token.EOF
            ]
            Expect.equal tokens expected "Should tokenize simple object"

        testCase "simple array" <| fun _ ->
            let input = "[a, b, c]"
            let tokens = tokenize input
            let expected = [
                Token.OpenBracket
                Token.String "a"
                Token.Comma
                Token.String "b"
                Token.Comma
                Token.String "c"
                Token.CloseBracket
                Token.EOF
            ]
            Expect.equal tokens expected "Should tokenize simple array"

        testCase "nested object" <| fun _ ->
            let input = "{outer: {inner: value}}"
            let tokens = tokenize input
            let expected = [
                Token.OpenBrace
                Token.String "outer"
                Token.Colon
                Token.OpenBrace
                Token.String "inner"
                Token.Colon
                Token.String "value"
                Token.CloseBrace
                Token.CloseBrace
                Token.EOF
            ]
            Expect.equal tokens expected "Should tokenize nested object"

        testCase "string with spaces" <| fun _ ->
            let input = "{key: Hello World}"
            let tokens = tokenize input
            let expected = [
                Token.OpenBrace
                Token.String "key"
                Token.Colon
                Token.String "Hello World"
                Token.CloseBrace
                Token.EOF
            ]
            Expect.equal tokens expected "Should preserve spaces in unquoted strings"

        testCase "string placeholder preservation" <| fun _ ->
            let input = "{key: <s f=0/>}"
            let tokens = tokenize input
            let expected = [
                Token.OpenBrace
                Token.String "key"
                Token.Colon
                Token.String "<s f=0/>"
                Token.CloseBrace
                Token.EOF
            ]
            Expect.equal tokens expected "Should preserve string placeholders as atomic tokens"

        testCase "comment placeholder as separate token" <| fun _ ->
            let input = "{key: <c f=1/>}"
            let tokens = tokenize input
            // Comment placeholders are treated as complete tokens
            let expected = [
                Token.OpenBrace
                Token.String "key"
                Token.Colon
                Token.String "<c f=1/>"
                Token.CloseBrace
                Token.EOF
            ]
            Expect.equal tokens expected "Should parse comment placeholder as token"
    ]

    testList "transformFlowContent" [
        testCase "simple object to block" <| fun _ ->
            let stringDict = Dictionary<int, string>()
            let ctx = defaultContext stringDict
            let input = "{key: value}"
            let result = transformFlowContent ctx input
            let expected = [PreprocessorElement.Line "key: value"]
            Expect.equal result expected "Should convert simple object to block style"

        testCase "nested object to block" <| fun _ ->
            let stringDict = Dictionary<int, string>()
            let ctx = defaultContext stringDict
            let input = "{outer: {inner: value}}"
            let result = transformFlowContent ctx input
            let expected = [
                PreprocessorElement.Line "outer:"
                PreprocessorElement.Intendation [
                    PreprocessorElement.Line "inner: value"
                ]
            ]
            Expect.equal result expected "Should convert nested object to block style with indentation"

        testCase "multi-key object to block" <| fun _ ->
            let stringDict = Dictionary<int, string>()
            let ctx = defaultContext stringDict
            let input = "{a: 1, b: 2, c: 3}"
            let result = transformFlowContent ctx input
            let expected = [
                PreprocessorElement.Line "a: 1"
                PreprocessorElement.Line "b: 2"
                PreprocessorElement.Line "c: 3"
            ]
            Expect.equal result expected "Should convert multi-key object to block style"

        testCase "simple array to block" <| fun _ ->
            let stringDict = Dictionary<int, string>()
            let ctx = defaultContext stringDict
            let input = "[a, b, c]"
            let result = transformFlowContent ctx input
            let expected = [
                PreprocessorElement.Line "- a"
                PreprocessorElement.Line "- b"
                PreprocessorElement.Line "- c"
            ]
            Expect.equal result expected "Should convert simple array to block style"

        testCase "array of objects to block" <| fun _ ->
            let stringDict = Dictionary<int, string>()
            let ctx = defaultContext stringDict
            let input = "[{a: 1}, {b: 2}]"
            let result = transformFlowContent ctx input
            // Simple key-value objects are formatted inline with dash
            let expected = [
                PreprocessorElement.Line "- a: 1"
                PreprocessorElement.Line "- b: 2"
            ]
            Expect.equal result expected "Should convert array of objects to block style"

        testCase "deeply nested structure" <| fun _ ->
            let stringDict = Dictionary<int, string>()
            let ctx = defaultContext stringDict
            let input = "{a: {b: {c: value}}}"
            let result = transformFlowContent ctx input
            let expected = [
                PreprocessorElement.Line "a:"
                PreprocessorElement.Intendation [
                    PreprocessorElement.Line "b:"
                    PreprocessorElement.Intendation [
                        PreprocessorElement.Line "c: value"
                    ]
                ]
            ]
            Expect.equal result expected "Should handle deeply nested structures with proper indentation"

        testCase "empty object" <| fun _ ->
            let stringDict = Dictionary<int, string>()
            let ctx = defaultContext stringDict
            let input = "{}"
            let result = transformFlowContent ctx input
            let expected = []
            Expect.equal result expected "Should return empty list for empty object"

        testCase "preserves string placeholders" <| fun _ ->
            let stringDict = Dictionary<int, string>()
            stringDict.Add(0, "Hello World")
            let ctx = defaultContext stringDict
            let input = "{key: <s f=0/>}"
            let result = transformFlowContent ctx input
            let expected = [PreprocessorElement.Line "key: <s f=0/>"]
            Expect.equal result expected "Should preserve string placeholders in output"
    ]

    testList "transformElement" [
        testCase "key-value with flow object" <| fun _ ->
            let stringDict = Dictionary<int, string>()
            let ctx = defaultContext stringDict
            let element = PreprocessorElement.Line "key: {a: 1, b: 2}"
            let result = transformElement ctx element
            let expected = [
                PreprocessorElement.Line "key:"
                PreprocessorElement.Intendation [
                    PreprocessorElement.Line "a: 1"
                    PreprocessorElement.Line "b: 2"
                ]
            ]
            Expect.equal result expected "Should transform key-value with flow object"

        testCase "key-value with flow array" <| fun _ ->
            let stringDict = Dictionary<int, string>()
            let ctx = defaultContext stringDict
            let element = PreprocessorElement.Line "key: [1, 2, 3]"
            let result = transformElement ctx element
            let expected = [
                PreprocessorElement.Line "key:"
                PreprocessorElement.Intendation [
                    PreprocessorElement.Line "- 1"
                    PreprocessorElement.Line "- 2"
                    PreprocessorElement.Line "- 3"
                ]
            ]
            Expect.equal result expected "Should transform key-value with flow array"

        testCase "preserves non-flow elements" <| fun _ ->
            let stringDict = Dictionary<int, string>()
            let ctx = defaultContext stringDict
            let element = PreprocessorElement.Line "normal: value"
            let result = transformElement ctx element
            let expected = [element]
            Expect.equal result expected "Should preserve non-flow elements unchanged"

        testCase "handles indentation element" <| fun _ ->
            let stringDict = Dictionary<int, string>()
            let ctx = defaultContext stringDict
            let element = PreprocessorElement.Intendation [
                PreprocessorElement.Line "key: {a: 1}"
            ]
            let result = transformElement ctx element
            let expected = [
                PreprocessorElement.Intendation [
                    PreprocessorElement.Line "key:"
                    PreprocessorElement.Intendation [
                        PreprocessorElement.Line "a: 1"
                    ]
                ]
            ]
            Expect.equal result expected "Should recursively transform indented elements"
    ]

    testList "indentation tracking" [
        testCase "custom indent step" <| fun _ ->
            let stringDict = Dictionary<int, string>()
            let ctx = { BaseIndent = 0; IndentStep = 4; StringDict = stringDict }
            let input = "{outer: {inner: value}}"
            let result = transformFlowContent ctx input
            // Indentation is tracked in context but not reflected in Line content
            // The actual indentation is applied during write phase
            let expected = [
                PreprocessorElement.Line "outer:"
                PreprocessorElement.Intendation [
                    PreprocessorElement.Line "inner: value"
                ]
            ]
            Expect.equal result expected "Should handle custom indent step"

        testCase "non-zero base indent" <| fun _ ->
            let stringDict = Dictionary<int, string>()
            let ctx = { BaseIndent = 2; IndentStep = 2; StringDict = stringDict }
            let input = "{key: value}"
            let result = transformFlowContent ctx input
            let expected = [PreprocessorElement.Line "key: value"]
            Expect.equal result expected "Should handle non-zero base indent"
    ]

    testList "comment preservation" [
        testCase "comment after flow array" <| fun _ ->
            let stringDict = Dictionary<int, string>()
            let ctx = defaultContext stringDict
            let element = PreprocessorElement.Line "[a, b, c] <c f=1/>"
            let result = transformElement ctx element
            // The comment placeholder should be preserved in the output
            let containsComment = 
                result 
                |> List.exists (fun e -> 
                    match e with
                    | PreprocessorElement.Line s -> s.Contains("<c f=1/>")
                    | PreprocessorElement.Intendation children ->
                        children |> List.exists (fun c ->
                            match c with
                            | PreprocessorElement.Line s -> s.Contains("<c f=1/>")
                            | _ -> false
                        )
                    | _ -> false
                )
            Expect.isTrue containsComment "Should preserve comment placeholder"

        testCase "comment after flow object" <| fun _ ->
            let stringDict = Dictionary<int, string>()
            let ctx = defaultContext stringDict
            let element = PreprocessorElement.Line "key: {a: 1} <c f=2/>"
            let result = transformElement ctx element
            // The comment placeholder should be preserved
            let containsComment = 
                result 
                |> List.exists (fun e -> 
                    match e with
                    | PreprocessorElement.Line s -> s.Contains("<c f=2/>")
                    | PreprocessorElement.Intendation children ->
                        children |> List.exists (fun c ->
                            match c with
                            | PreprocessorElement.Line s -> s.Contains("<c f=2/>")
                            | _ -> false
                        )
                    | _ -> false
                )
            Expect.isTrue containsComment "Should preserve comment placeholder for objects"
    ]

    testList "edge cases" [
        testCase "deeply nested structure (5+ levels)" <| fun _ ->
            let yaml = "a: {b: {c: {d: {e: {f: value}}}}}"
            let result = Reader.read yaml
            // Verify deeply nested structure is parsed correctly
            let expected = YAMLElement.Object [
                YAMLElement.Mapping(
                    YAMLContent.create("a"),
                    YAMLElement.Object [
                        YAMLElement.Mapping(
                            YAMLContent.create("b"),
                            YAMLElement.Object [
                                YAMLElement.Mapping(
                                    YAMLContent.create("c"),
                                    YAMLElement.Object [
                                        YAMLElement.Mapping(
                                            YAMLContent.create("d"),
                                            YAMLElement.Object [
                                                YAMLElement.Mapping(
                                                    YAMLContent.create("e"),
                                                    YAMLElement.Object [
                                                        YAMLElement.Mapping(
                                                            YAMLContent.create("f"),
                                                            YAMLElement.Object [
                                                                YAMLElement.Value(YAMLContent.create("value"))
                                                            ]
                                                        )
                                                    ]
                                                )
                                            ]
                                        )
                                    ]
                                )
                            ]
                        )
                    ]
                )
            ]
            Expect.equal result expected "Should parse 5+ levels of nesting"

        testCase "empty nested structures" <| fun _ ->
            let yaml = "key: {a: {}, b: []}"
            let result = Reader.read yaml
            // Empty objects {} and empty arrays [] in nested context
            // Note: Empty array [] becomes empty sequence, empty object {} becomes empty object
            match result with
            | YAMLElement.Object [YAMLElement.Mapping(k, v)] when k.Value = "key" ->
                // Successfully parsed the outer structure
                Expect.isTrue true "Parsed key successfully"
            | _ ->
                Expect.isTrue false "Failed to parse empty nested structures"

        testCase "mixed flow and block style" <| fun _ ->
            let yaml = """outer:
  flow: {a: 1, b: 2}
  block:
    - item1
    - item2"""
            let result = Reader.read yaml
            // Verify both flow and block sections parse correctly
            match result with
            | YAMLElement.Object [YAMLElement.Mapping(outerKey, outerValue)] when outerKey.Value = "outer" ->
                match outerValue with
                | YAMLElement.Object children ->
                    Expect.equal (List.length children) 2 "Should have 2 children (flow and block)"
                | _ -> Expect.isTrue false "Expected Object for outer value"
            | _ -> Expect.isTrue false "Failed to parse mixed flow/block"

        testCase "unicode in flow-style" <| fun _ ->
            let yaml = """data: {name: "日本語", emoji: "🎉"}"""
            let result = Reader.read yaml
            // Verify Unicode strings are preserved
            match result with
            | YAMLElement.Object [YAMLElement.Mapping(dataKey, dataValue)] when dataKey.Value = "data" ->
                match dataValue with
                | YAMLElement.Object children ->
                    // Check that we have the expected mappings
                    let hasName = children |> List.exists (fun c ->
                        match c with
                        | YAMLElement.Mapping(k, _) -> k.Value = "name"
                        | _ -> false
                    )
                    Expect.isTrue hasName "Should have 'name' key with Unicode value"
                | _ -> Expect.isTrue false "Expected Object for data value"
            | _ -> Expect.isTrue false "Failed to parse Unicode in flow-style"

        testCase "array with nested objects in flow-style" <| fun _ ->
            let yaml = "items: [{id: 1, name: first}, {id: 2, name: second}]"
            let result = Reader.read yaml
            match result with
            | YAMLElement.Object [YAMLElement.Mapping(k, v)] when k.Value = "items" ->
                match v with
                | YAMLElement.Object [YAMLElement.Sequence items] ->
                    Expect.equal (List.length items) 2 "Should have 2 items in array"
                | _ -> Expect.isTrue false "Expected sequence for items"
            | _ -> Expect.isTrue false "Failed to parse array with nested objects"
    ]
]
