module Tests.Escapes

open Fable.Pyxpecto
open YAMLicious
open YAMLicious.Escapes
open YAMLiciousTypes

let private doubleQuoted (value: string) = YAMLContent.create(value, style=ScalarStyle.DoubleQuoted)

/// The tree of `k: "value"`.
let private keyWithDoubleQuoted (value: string) =
    YAMLElement.Object [
        YAMLElement.Mapping(
            YAMLContent.create("k"),
            YAMLElement.Object [ YAMLElement.Value(doubleQuoted value) ]
        )
    ]

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

    testCase "Double-quoted escaped continuation with blank escaped line" <| fun _ ->
        // yaml@2.6.1: "folded to a space,\tto a line feed". The blanks in front of a backslash stay.
        let yaml = "\"folded \\\nto a space,\t\\\n \\\nto a line feed\""
        let expected = YAMLElement.Object [
            YAMLElement.Value(YAMLContent.create("folded to a space,\tto a line feed", style=ScalarStyle.DoubleQuoted))
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Escaped continuation lines should fold according to YAML semantics"

    testCase "Double-quoted escaped continuation keeps trailing spaces before the backslash" <| fun _ ->
        // yaml@2.6.1: "trailing spaces   next"
        let yaml = "\"trailing spaces   \\\nnext\""
        let expected = YAMLElement.Object [
            YAMLElement.Value(YAMLContent.create("trailing spaces   next", style=ScalarStyle.DoubleQuoted))
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Escaped line continuation should keep the blanks in front of the backslash"

    testCase "A line break in a double-quoted scalar folds into one space" <| fun _ ->
        // yaml@2.6.1: {"k":"one two"}
        Expect.equal (Reader.read "k: \"one\n  two\"") (keyWithDoubleQuoted "one two") "The break and the indentation become one space"

    testCase "Each empty line in a double-quoted fold becomes a line feed" <| fun _ ->
        // yaml@2.6.1, one value per check:
        // {"k":"one\ntwo"}
        // {"k":"one\n\ntwo"}
        // {"k":"one\ntwo"}
        Expect.equal (Reader.read "k: \"one\n\n  two\"") (keyWithDoubleQuoted "one\ntwo") "One empty line"
        Expect.equal (Reader.read "k: \"one\n\n\n  two\"") (keyWithDoubleQuoted "one\n\ntwo") "Two empty lines"
        Expect.equal (Reader.read "k: \"one\n \t \n  two\"") (keyWithDoubleQuoted "one\ntwo") "A line of blanks is an empty line"

    testCase "A double-quoted fold drops the blanks around the line break" <| fun _ ->
        // yaml@2.6.1: {"k":"one two"} and {"k":"  a b  "}
        Expect.equal (Reader.read "k: \"one \t\n  two\"") (keyWithDoubleQuoted "one two") "Trailing blanks of the first line"
        Expect.equal (Reader.read "k: \"  a\n  b  \"") (keyWithDoubleQuoted "  a b  ") "The first and the last line keep their outer blanks"

    testCase "An escaped line break in a double-quoted scalar joins the lines without a space" <| fun _ ->
        // yaml@2.6.1: {"k":"onetwo"} and {"k":"one two"}
        Expect.equal (Reader.read "k: \"one\\\n  two\"") (keyWithDoubleQuoted "onetwo") "The leading blanks of the next line go away"
        Expect.equal (Reader.read "k: \"one \\\n  two\"") (keyWithDoubleQuoted "one two") "The blank in front of the backslash stays"

    testCase "A blank that an escape produces stays in front of a double-quoted fold" <| fun _ ->
        // yaml@2.6.1, one value per check:
        // {"k":"one\t two"}
        // {"k":"one  two"}
        // {"k":"one  two"}
        // {"k":"one\t two"}
        Expect.equal (Reader.read "k: \"one\\t\n  two\"") (keyWithDoubleQuoted "one\t two") "Escaped tab"
        Expect.equal (Reader.read "k: \"one\\ \n  two\"") (keyWithDoubleQuoted "one  two") "Escaped space"
        Expect.equal (Reader.read "k: \"one\\x20\n  two\"") (keyWithDoubleQuoted "one  two") "Hex escape of a space"
        Expect.equal (Reader.read "k: \"one\\t \n  two\"") (keyWithDoubleQuoted "one\t two") "The literal blank behind the escape is trimmed"

    testCase "CRLF and CR line breaks fold in a double-quoted scalar like LF" <| fun _ ->
        // yaml@2.6.1, one value per check:
        // {"k":"one two"}
        // {"k":"one\ntwo"}
        // {"k":"onetwo"}
        // {"k":"one\r  two"} for the lone CR, because it does not treat a CR as a line break.
        Expect.equal (Reader.read "k: \"one\r\n  two\"") (keyWithDoubleQuoted "one two") "CRLF"
        Expect.equal (Reader.read "k: \"one\r\n\r\n  two\"") (keyWithDoubleQuoted "one\ntwo") "CRLF around an empty line"
        Expect.equal (Reader.read "k: \"one\\\r\n  two\"") (keyWithDoubleQuoted "onetwo") "CRLF behind a backslash"
        Expect.equal (Reader.read "k: \"one\r  two\"") (keyWithDoubleQuoted "one two") "CR"

    testCase "A multiline double-quoted key folds" <| fun _ ->
        // yaml@2.6.1: {"one two":"v"} for the explicit key and {"one two":"x y"} for the flow mapping.
        // It rejects the same key as an implicit key with MULTILINE_IMPLICIT_KEY.
        let expectedBlock =
            YAMLElement.Object [
                YAMLElement.Mapping(doubleQuoted "one two", YAMLElement.Object [ YAMLElement.Value(YAMLContent.create("v")) ])
            ]
        let expectedFlow =
            YAMLElement.Object [
                YAMLElement.Mapping(doubleQuoted "one two", YAMLElement.Object [ YAMLElement.Value(doubleQuoted "x y") ])
            ]
        Expect.equal (Reader.read "? \"one\n  two\"\n: v") expectedBlock "Explicit key"
        Expect.equal (Reader.read "{ \"one\n  two\": \"x\n  y\" }") expectedFlow "Flow mapping"

    testCase "A multiline double-quoted sequence item folds" <| fun _ ->
        // yaml@2.6.1: ["one two","k"] and ["one\ntwo","k"]
        let expected (first: string) =
            YAMLElement.Object [
                YAMLElement.Sequence [
                    YAMLElement.Object [ YAMLElement.Value(doubleQuoted first) ]
                    YAMLElement.Object [ YAMLElement.Value(YAMLContent.create("k")) ]
                ]
            ]
        Expect.equal (Reader.read "- \"one\n  two\"\n- k") (expected "one two") "One break"
        Expect.equal (Reader.read "- \"one\n\n  two\"\n- k") (expected "one\ntwo") "An empty line"

    testCase "Decode.string returns the folded value of a multiline CWL valueFrom" <| fun _ ->
        // yaml@2.6.1: {"inputBinding":{"valueFrom":"$(inputs.a + \"x y\")"}} for both inputs
        let decodeValueFrom (yaml: string) =
            Decode.read yaml
            |> Decode.object (fun get ->
                get.Required.Field "inputBinding" (Decode.object (fun inner -> inner.Required.Field "valueFrom" Decode.string)))
        let expected = "$(inputs.a + \"x y\")"
        Expect.equal (decodeValueFrom "inputBinding:\n  valueFrom: \"$(inputs.a + \\\"x\n    y\\\")\"") expected "Double-quoted"
        Expect.equal (decodeValueFrom "inputBinding:\n  valueFrom: $(inputs.a + \"x\n    y\")") expected "Plain"

    testCase "A single-line double-quoted scalar decodes every escape" <| fun _ ->
        // yaml@2.6.1 reads the same value
        let yaml = "k: \"\\0\\a\\b\\t\\n\\v\\f\\r\\e\\ \\\"\\/\\\\\\N\\_\\L\\P\\x41\\u00E9\\U0001F600\""
        let expected = "\u0000\u0007\u0008\t\n\u000B\u000C\r\u001B \"/\\\u0085\u00A0\u2028\u2029A\u00E9\uD83D\uDE00"
        Expect.equal (Reader.read yaml) (keyWithDoubleQuoted expected) "Every escape of YAML 1.2.2 7.3.1"

    testCase "A double-quoted scalar with a line break round-trips through the writer" <| fun _ ->
        // The writer escapes a line break in a double-quoted key, so the output never relies on folding.
        let key = doubleQuoted "one \n\n  two \t"
        let ast = YAMLElement.Object [ YAMLElement.Mapping(key, YAMLElement.Object [ YAMLElement.Value(YAMLContent.create("v")) ]) ]
        let written = Writer.write ast None
        Expect.trimEqual written "\"one \\n\\n  two \\t\": v" "The writer escapes the line breaks and the tab"
        Expect.equal (Reader.read written) ast "Reading the output returns the key"
  ]
