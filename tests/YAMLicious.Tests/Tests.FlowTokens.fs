module Tests.FlowTokens

open Fable.Pyxpecto
open YAMLicious.Syntax.FlowTokens
open YAMLicious
open YAMLicious.YAMLiciousTypes

let Main = testList "FlowTokens" [
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
            let expected = [
                Token.OpenBrace
                Token.String "key"
                Token.Colon
                Token.String "<c f=1/>"
                Token.CloseBrace
                Token.EOF
            ]
            Expect.equal tokens expected "Should parse comment placeholder as token"

        testCase "double-quoted string with comma" <| fun _ ->
            let input = "[\"a,b\", c]"
            let tokens = tokenize input
            let expected = [
                Token.OpenBracket
                Token.String "a,b"
                Token.Comma
                Token.String "c"
                Token.CloseBracket
                Token.EOF
            ]
            Expect.equal tokens expected "Should preserve comma inside double-quoted string as one token"

        testCase "double-quoted string in object value" <| fun _ ->
            let input = "{key: \"hello, world\"}"
            let tokens = tokenize input
            let expected = [
                Token.OpenBrace
                Token.String "key"
                Token.Colon
                Token.String "hello, world"
                Token.CloseBrace
                Token.EOF
            ]
            Expect.equal tokens expected "Should preserve comma inside double-quoted string in object"

        testCase "empty object" <| fun _ ->
            let input = "{}"
            let tokens = tokenize input
            let expected = [
                Token.OpenBrace
                Token.CloseBrace
                Token.EOF
            ]
            Expect.equal tokens expected "Should tokenize empty object"

        testCase "empty array" <| fun _ ->
            let input = "[]"
            let tokens = tokenize input
            let expected = [
                Token.OpenBracket
                Token.CloseBracket
                Token.EOF
            ]
            Expect.equal tokens expected "Should tokenize empty array"

        testCase "nested array inside object" <| fun _ ->
            let input = "{key: [a, b]}"
            let tokens = tokenize input
            let expected = [
                Token.OpenBrace
                Token.String "key"
                Token.Colon
                Token.OpenBracket
                Token.String "a"
                Token.Comma
                Token.String "b"
                Token.CloseBracket
                Token.CloseBrace
                Token.EOF
            ]
            Expect.equal tokens expected "Should tokenize nested array inside object"

        testCase "nested object inside array" <| fun _ ->
            let input = "[{a: 1}, {b: 2}]"
            let tokens = tokenize input
            let expected = [
                Token.OpenBracket
                Token.OpenBrace
                Token.String "a"
                Token.Colon
                Token.String "1"
                Token.CloseBrace
                Token.Comma
                Token.OpenBrace
                Token.String "b"
                Token.Colon
                Token.String "2"
                Token.CloseBrace
                Token.CloseBracket
                Token.EOF
            ]
            Expect.equal tokens expected "Should tokenize nested objects inside array"

        testCase "multiline flow content flattened" <| fun _ ->
            let input = "a: 1,\nb: 2"
            let tokens = tokenize input
            let expected = [
                Token.String "a"
                Token.Colon
                Token.String "1"
                Token.Comma
                Token.String "b"
                Token.Colon
                Token.String "2"
                Token.EOF
            ]
            Expect.equal tokens expected "Should tokenize flow content with newlines"
    ]

    testList "edge cases" [
        testCase "deeply nested structure (5+ levels)" <| fun _ ->
            let yaml = "a: {b: {c: {d: {e: {f: value}}}}}"
            let result = Reader.read yaml
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
            match result with
            | YAMLElement.Object [YAMLElement.Mapping(k, v)] when k.Value = "key" ->
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
            match result with
            | YAMLElement.Object [YAMLElement.Mapping(dataKey, dataValue)] when dataKey.Value = "data" ->
                match dataValue with
                | YAMLElement.Object children ->
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