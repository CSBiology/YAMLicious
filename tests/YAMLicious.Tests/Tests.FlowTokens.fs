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

        testCase "comment placeholder ends a plain scalar and starts none" <| fun _ ->
            let tokens = tokenize "[a <c f=0/> b, <c f=1/> c]"
            let expected = [
                Token.OpenBracket
                Token.String "a "
                Token.String "<c f=0/>"
                Token.String "b"
                Token.Comma
                Token.String "<c f=1/>"
                Token.String "c"
                Token.CloseBracket
                Token.EOF
            ]
            Expect.equal tokens expected "A comment placeholder is a token of its own"
            Expect.equal
                (tokenize "[<s f=0/> <c f=1/> ]")
                [ Token.OpenBracket; Token.String "<s f=0/>"; Token.String "<c f=1/>"; Token.CloseBracket; Token.EOF ]
                "A comment behind a string placeholder is a token of its own"

        testCase "colon without a blank behind it is part of a plain scalar" <| fun _ ->
            let tokens = tokenize "[a:b, 12:45, http://x]"
            let expected = [
                Token.OpenBracket
                Token.String "a:b"
                Token.Comma
                Token.String "12:45"
                Token.Comma
                Token.String "http://x"
                Token.CloseBracket
                Token.EOF
            ]
            Expect.equal tokens expected "Only a colon with a blank or a flow indicator behind it separates"

        testCase "colon with a flow indicator or the end behind it separates" <| fun _ ->
            Expect.equal
                (tokenize "{a:, b:}")
                [ Token.OpenBrace; Token.String "a"; Token.Colon; Token.Comma; Token.String "b"; Token.Colon; Token.CloseBrace; Token.EOF ]
                "A comma and a closer behind the colon"
            Expect.equal
                (tokenize "[a: b]")
                [ Token.OpenBracket; Token.String "a"; Token.Colon; Token.String "b"; Token.CloseBracket; Token.EOF ]
                "A pair in a sequence"

        testCase "colon directly behind a quoted scalar or a flow collection separates" <| fun _ ->
            Expect.equal
                (tokenize "{<s f=0/>:b, <s f=1/> : c}")
                [ Token.OpenBrace; Token.String "<s f=0/>"; Token.Colon; Token.String "b"; Token.Comma
                  Token.String "<s f=1/>"; Token.Colon; Token.String "c"; Token.CloseBrace; Token.EOF ]
                "A quoted key"
            Expect.equal
                (tokenize "{[a]:b}")
                [ Token.OpenBrace; Token.OpenBracket; Token.String "a"; Token.CloseBracket; Token.Colon; Token.String "b"; Token.CloseBrace; Token.EOF ]
                "A flow collection as a key"

        testCase "properties, aliases and the question mark of an explicit key are tokens" <| fun _ ->
            Expect.equal
                (tokenize "[!!str a, &x b, *x, ? c: d]")
                [ Token.OpenBracket; Token.Property "!!str"; Token.String "a"; Token.Comma
                  Token.Property "&x"; Token.String "b"; Token.Comma
                  Token.Alias "x"; Token.Comma
                  Token.Explicit; Token.String "c"; Token.Colon; Token.String "d"; Token.CloseBracket; Token.EOF ]
                "Properties, an alias and an explicit key"
            Expect.equal
                (tokenize "{&a <s f=0/>: !t <s f=1/>}")
                [ Token.OpenBrace; Token.Property "&a"; Token.String "<s f=0/>"; Token.Colon
                  Token.Property "!t"; Token.String "<s f=1/>"; Token.CloseBrace; Token.EOF ]
                "Properties in front of a key and a value"
            Expect.equal
                (tokenize "[?a, ?, ? ]")
                [ Token.OpenBracket; Token.String "?a"; Token.Comma; Token.Explicit; Token.Comma; Token.Explicit; Token.CloseBracket; Token.EOF ]
                "A question mark is an indicator only in front of a blank, a flow indicator or the end"
            Expect.equal
                (tokenize "[a ! b, a & b]")
                [ Token.OpenBracket; Token.String "a ! b"; Token.Comma; Token.String "a & b"; Token.CloseBracket; Token.EOF ]
                "A tag mark or an anchor mark inside a plain scalar is text"

        testCase "a verbatim tag is one property token" <| fun _ ->
            Expect.equal
                (tokenize "[!<tag:yaml.org,2002:str> a, !<tag:yaml.org,2002:str>, c]")
                [ Token.OpenBracket; Token.Property "!<tag:yaml.org,2002:str>"; Token.String "a"; Token.Comma
                  Token.Property "!<tag:yaml.org,2002:str>"; Token.Comma; Token.String "c"; Token.CloseBracket; Token.EOF ]
                "The colon and the comma inside the tag do not split it"
            Expect.equal
                (tokenize "{!<tag:yaml.org,2002:str> k: v}")
                [ Token.OpenBrace; Token.Property "!<tag:yaml.org,2002:str>"; Token.String "k"; Token.Colon; Token.String "v"; Token.CloseBrace; Token.EOF ]
                "A tag in front of a key"

        testCase "text in front of a placeholder and the blanks around it stay in the token" <| fun _ ->
            Expect.equal
                (tokenize "[x <s f=0/> y, <s f=1/> tail]")
                [ Token.OpenBracket; Token.String "x <s f=0/> y"; Token.Comma; Token.String "<s f=1/> tail"; Token.CloseBracket; Token.EOF ]
                "A placeholder inside a plain scalar is text"
            Expect.equal
                (tokenize "{a: x <c f=0/>}")
                [ Token.OpenBrace; Token.String "a"; Token.Colon; Token.String "x "; Token.String "<c f=0/>"; Token.CloseBrace; Token.EOF ]
                "A comment placeholder ends the scalar in front of it"
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