module Tests.Escapes

open Fable.Pyxpecto
open YAMLicious
open YAMLicious.Escapes
open YAMLiciousTypes

let Main =
  testList "Escapes" [
    testCase "Double-quoted escape: unicode 32-bit" <| fun _ ->
        let yaml = "key: \"\\U0001F600\""
        let expectedEmoji = "\uD83D\uDE00"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("key"),
                YAMLElement.Object [
                    YAMLElement.Value(YAMLContent.create(expectedEmoji, style=ScalarStyle.DoubleQuoted))
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Should handle 32-bit unicode escape"

    testCase "Double-quoted escape: complex mix" <| fun _ ->
        // \t (tab), \n (newline), \x41 (A)
        let yaml = "key: \"\\t\\n\\x41\""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("key"),
                YAMLElement.Object [
                    YAMLElement.Value(YAMLContent.create("\t\nA", style=ScalarStyle.DoubleQuoted))
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Should handle complex mixed escapes"

    testCase "Double-quoted multiline escaped line break" <| fun _ ->
        let yaml = "key: \"a\\\nb\""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("key"),
                YAMLElement.Object [
                    YAMLElement.Value(YAMLContent.create("ab", style=ScalarStyle.DoubleQuoted))
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Escaped line breaks should be consumed in double-quoted scalars"
  ]
