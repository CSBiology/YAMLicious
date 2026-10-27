module Tests.Decode

// The decoders and the getters. Each decoder gets a success case, a wrong kind of node, an alias, and the tag and style cases of the
// core schema. The error path and the error span are asserted exactly. The expectations of the old Tests.EncoderDecoder and the decoding parts of
// Tests.YamlGenericRead and Tests.ReaderDocuments are ported to the new API, and the yaml@2.6.1 value stands above each of them as before.
//
// Old tests that changed:
// - A test with `[1, 2]` or `{}` in its input builds the node with the constructors, as it did before the parser read flow collections. The test
//   "the flow inputs of the old decode tests" reads the original inputs through the parser.
// - The model has no Nil. A missing value is an empty plain scalar, so Decode.scalarText reads `k:` as the empty text. Decode.string rejects it, as the old decoder
//   rejected Nil.
// - Decode.read is gone. The Reader owns reading, so the tests parse with parseRoot and decode the node.
// - The ExtensionData of the old overflow tests was a Dictionary from key to element. The getters return the entries in source order now.
//
// Old tests that are dropped: Tests.YamlGenericRead tests the preprocessor (Level, Line, Intendation, the string and comment tables), which the
// parser replaces. Its decoding-free tests are covered by Tests.Parser and Tests.Comments.

open Fable.Pyxpecto
open YAMLicious

let private span (line1: int) (column1: int) (line2: int) (column2: int) : SourceSpan option =
    Some
        { Start = { Line = line1; Column = column1 }
          Finish = { Line = line2; Column = column2 } }

/// The value of the first entry of the root mapping, parsed from the text.
let private fieldNode (text: string) : YamlNode = (YamlNode.entries (parseRoot text)).Head.Value

/// The alias `*x` on the second line, parsed from a text. It stands at 2:4-2:6.
let private aliasNode: YamlNode = (YamlNode.entries (parseRoot "a: &x v\nb: *x\n")).Tail.Head.Value

let private flowSeq (nodes: YamlNode list) : YamlNode = YamlNode.flowSequence (List.map YamlNode.item nodes)

let private decodeError (decoder: Decoder<'T>) (node: YamlNode) : DecodeError =
    match Decode.tryDecode decoder node with
    | Error error -> error
    | Ok _ -> failwith "The decoder accepts the node"

/// Checks the path, the span and a part of the message of the error that the decoder raises for the node.
let private expectError (decoder: Decoder<'T>) (node: YamlNode) (path: string list) (location: SourceSpan option) (messagePart: string) =
    let error = decodeError decoder node
    Expect.equal error.Path path (sprintf "The path of the error %s" error.Message)
    Expect.equal error.Span location (sprintf "The span of the error %s" error.Message)
    Expect.stringContains error.Message messagePart "The message"

let private expectRejects (decoder: Decoder<'T>) (node: YamlNode) (message: string) =
    match Decode.tryDecode decoder node with
    | Error _ -> ()
    | Ok _ -> failwith message

let private keyNames (entries: MappingEntry list) : string list = entries |> List.map (fun entry -> Decode.string entry.Key)

/// The trivia as text, `#` and the comment for a comment line and an empty text for a blank line. A parsed trivia line has a span, so the lists are not compared directly.
let private triviaText (trivia: Trivia list) : string list =
    trivia
    |> List.map (fun item ->
        match item with
        | Trivia.FullLineComment comment -> "#" + comment.Text
        | Trivia.BlankLine _ -> "")

// -------------------------------------------------------------------------------------------------------------------
// Scalars

let private stringTests = testList "Decode.string" [
    // yaml@2.6.1 reads each of these as a string. A plain text resolves to no other type in the core schema, and a quoted or block scalar or a scalar under
    // !!str or the non-specific tag ! is a string whatever its text. It reads `%TAG !e! tag:yaml.org,2002:` with `!e!str 5` and the verbatim `!<tag:yaml.org,2002:str> 5` as
    // the string 5 too.
    testCase "reads a string by its tag first and by its style second" <| fun _ ->
        let cases =
            [ "k: abc", "abc"; "k: yes", "yes"; "k: 1_000", "1_000"; "k: 0x", "0x"; "k: -0x10", "-0x10"; "k: a\n  b", "a b"
              "k: \"x\"", "x"; "k: 'it''s'", "it's"; "k: \"\"", ""; "k: ''", ""; "k: \"1\"", "1"; "k: 'true'", "true"; "k: \"null\"", "null"; "k: \"~\"", "~"
              "k: |\n  a\n  b\n", "a\nb\n"; "k: >-\n  a\n  b\n", "a b"; "k: |-\n  5\n", "5"; "k: |-\n  null\n", "null"
              "k: !!str 5", "5"; "k: !!str", ""; "k: !!str \"\"", ""; "k: !!str null", "null"; "k: !!str |-\n  5\n", "5"
              "k: ! 5", "5"; "k: !", ""; "k: ! \"x\"", "x"; "k: !<tag:yaml.org,2002:str> 5", "5" ]
        for (text, expected) in cases do
            Expect.equal (Decode.string (fieldNode text)) expected (sprintf "The string of %s" text)
        Expect.equal (Decode.string (fieldNode "%TAG !e! tag:yaml.org,2002:\n---\nk: !e!str 5")) "5" "The tag is compared by its expanded identifier"

    // yaml@2.6.1 reads the plain 5, 0x1F, -0 and the 30 digits as ints (the digits with { intAsBigInt: true }), 1.5, 1e3, .nan and -.inf as floats, true and
    // FALSE as bools, and null, ~, Null and the missing value as null. It reads a scalar under !!int, !!float, !!bool or !!null by its tag, and `k: !foo bar`
    // as the string bar with the warning TAG_RESOLVE_FAILED.
    testCase "rejects a plain scalar of another type and names the tag that it resolves to" <| fun _ ->
        let resolved =
            [ "5", "!!int"; "0x1F", "!!int"; "-0", "!!int"; "123456789012345678901234567890", "!!int"; "1.5", "!!float"; "1e3", "!!float"
              ".nan", "!!float"; "-.inf", "!!float"; "true", "!!bool"; "FALSE", "!!bool"; "null", "!!null"; "~", "!!null"; "Null", "!!null" ]
        for (text, tag) in resolved do
            let node = fieldNode ("k: " + text)
            expectError Decode.string node [] node.Span (sprintf "Expected a string, found the plain scalar \"%s\", which resolves to %s." text tag)
        expectError Decode.string (fieldNode "k: 5") [] (span 1 4 1 5) "Expected a string, found the plain scalar \"5\", which resolves to !!int."

    testCase "rejects a missing value and an empty plain scalar with an anchor" <| fun _ ->
        expectError Decode.string (fieldNode "k:") [] (span 1 3 1 3) "Expected a string, found a missing value."
        expectError Decode.string (fieldNode "k: &a") [] (span 1 4 1 6) "Expected a string, found a missing value."
        expectError Decode.string YamlNode.empty [] None "Expected a string, found a missing value."

    testCase "rejects every explicit tag other than !!str and ! and names it" <| fun _ ->
        let tagged =
            [ "k: !!int 5", "found the plain scalar \"5\" with the tag !!int."
              "k: !!int \"5\"", "found the double-quoted scalar \"5\" with the tag !!int."
              "k: !!float 1", "found the plain scalar \"1\" with the tag !!float."
              "k: !!bool true", "found the plain scalar \"true\" with the tag !!bool."
              "k: !!null \"\"", "found the double-quoted scalar \"\" with the tag !!null."
              "k: !!null", "found the plain scalar \"\" with the tag !!null."
              "k: !!binary abc", "found the plain scalar \"abc\" with the tag !!binary."
              "k: !foo bar", "found the plain scalar \"bar\" with the tag !foo."
              "k: !foo 'bar'", "found the single-quoted scalar \"bar\" with the tag !foo."
              "k: !<tag:example.com,2000:x> y", "found the plain scalar \"y\" with the tag !<tag:example.com,2000:x>." ]
        for (text, message) in tagged do
            expectError Decode.string (fieldNode text) [] (fieldNode text).Span ("Expected a string, " + message)
        // A text under !!null that is no null stays the error of the tag.
        expectError Decode.string (fieldNode "k: !!null foo") [] (span 1 4 1 14) "The tag !!null needs null"

    testCase "rejects a mapping and a sequence with their span" <| fun _ ->
        expectError Decode.string (fieldNode "k:\n  a: b\n") [] (span 2 3 2 7) "Expected a string, found a mapping."
        expectError Decode.string (fieldNode "k:\n  - a\n") [] (span 2 3 2 6) "Expected a string, found a sequence."

    testCase "rejects an alias and names it with its span" <| fun _ ->
        expectError Decode.string aliasNode [] (span 2 4 2 6) "The alias *x at line 2, column 4 cannot be read as a value."
]

let private scalarTextTests = testList "Decode.scalarText" [
    // yaml@2.6.1 reads each of these as a scalar. Decode.scalarText returns the cooked text of any scalar, also of a number, a bool, a null and a missing
    // value. A tag does not change the text, and a text that its tag rejects is read too.
    testCase "reads the text of every scalar" <| fun _ ->
        let cases =
            [ "k: abc", "abc"; "k: 12", "12"; "k: true", "true"; "k: null", "null"; "k: ~", "~"; "k: ", ""; "k: &a", ""
              "k: \"x\"", "x"; "k: 'it''s'", "it's"; "k: \"\"", ""; "k: !!int 5", "5"; "k: !!str 5", "5"; "k: !foo bar", "bar"
              "k: !!int foo", "foo"; "k: !!null foo", "foo"; "k: !!null \"\"", ""; "k: !!null", ""
              "k: |\n  a\n  b\n", "a\nb\n"; "k: >-\n  a\n  b\n", "a b"; "k: a\n  b", "a b" ]
        for (text, expected) in cases do
            Expect.equal (Decode.scalarText (fieldNode text)) expected (sprintf "The text of %s" text)

    testCase "rejects a mapping, a sequence and an alias with their span" <| fun _ ->
        expectError Decode.scalarText (fieldNode "k:\n  a: b\n") [] (span 2 3 2 7) "Expected a scalar, found a mapping."
        expectError Decode.scalarText (fieldNode "k:\n  - a\n") [] (span 2 3 2 6) "Expected a scalar, found a sequence."
        expectError Decode.scalarText aliasNode [] (span 2 4 2 6) "The alias *x at line 2, column 4 cannot be read as a value."
]

/// Checks that the decoder rejects a mapping, a sequence and an alias, and that the message names what the decoder expected.
let private expectKindErrors (what: string) (decoder: Decoder<'T>) =
    expectError decoder (fieldNode "k:\n  a: b\n") [] (span 2 3 2 7) (sprintf "Expected %s, found a mapping." what)
    expectError decoder (fieldNode "k:\n  - a\n") [] (span 2 3 2 6) (sprintf "Expected %s, found a sequence." what)
    expectError decoder aliasNode [] (span 2 4 2 6) "The alias *x at line 2, column 4 cannot be read as a value."

let private kindTests = testList "Every scalar decoder" [
    testCase "rejects a mapping, a sequence and an alias with the span of the node" <| fun _ ->
        expectKindErrors "a string" Decode.string
        expectKindErrors "a scalar" Decode.scalarText
        expectKindErrors "an int" Decode.int
        expectKindErrors "an int64" Decode.int64
        expectKindErrors "a float" Decode.float
        expectKindErrors "a bool" Decode.bool
        expectKindErrors "a char" Decode.char
        expectKindErrors "a DateTime" Decode.datetime
        expectKindErrors "a DateTimeOffset" Decode.datetimeOffset
]

let private intTests = testList "Decode.int and Decode.int64" [
    // yaml@2.6.1 reads 5, +5, -5, 007, 0o17 and 0x1F as the ints 5, 5, -5, 7, 15 and 31. It reads 0X1F, -0x1, 0o8, 1_000,
    // "5" and '5' as strings, 1.5 and 5.0 as floats, and `!!int "5"` as 5.
    testCase "Decode.int reads the ints of the core schema" <| fun _ ->
        let ints =
            [ "5", 5; "+5", 5; "-5", -5; "007", 7; "0o17", 15; "0x1F", 31; "0xff", 255
              "2147483647", 2147483647; "-2147483648", -2147483648; "0x7FFFFFFF", 2147483647 ]
        for (text, expected) in ints do
            Expect.equal (Decode.int (fieldNode ("k: " + text))) expected (sprintf "%s is an int" text)
        Expect.equal (Decode.int (fieldNode "k: !!int \"5\"")) 5 "An explicit tag fixes the type"
        for text in [ "2147483648"; "-2147483649"; "0x80000000"; "1.5"; "5.0"; "0X1F"; "-0x1"; "0o8"; "1_000"; "\"5\""; "'5'"; "5a"; "!!str 5"; "!!float 5"; "!foo 5"; "! 5" ] do
            expectRejects Decode.int (fieldNode ("k: " + text)) (sprintf "%s is no int" text)
        expectRejects Decode.int (fieldNode "k: |\n  5\n") "A block scalar is a string"

    testCase "Decode.int names the scalar that it found" <| fun _ ->
        expectError Decode.int (fieldNode "k: \"5\"") [] (span 1 4 1 7) "Expected an int, found the double-quoted scalar \"5\"."
        expectError Decode.int (fieldNode "k: abc") [] (span 1 4 1 7) "Expected an int, found the plain scalar \"abc\"."
        expectError Decode.int (fieldNode "k: !!str 5") [] (span 1 4 1 11) "found the plain scalar \"5\" with the tag !!str."
        expectError Decode.int (fieldNode "k: ") [] (span 1 3 1 3) "found a missing value."
        expectError Decode.int (fieldNode "k:\n  - 1\n") [] (span 2 3 2 6) "Expected an int, found a sequence."
        expectError Decode.int aliasNode [] (span 2 4 2 6) "The alias *x"

    // yaml@2.6.1 with { intAsBigInt: true } reads 3000000000, -9223372036854775808, 9223372036854775807, 0x7FFFFFFFFFFFFFFF,
    // 0o777, -0, +12, 0o777777777777777777777 and 00000000000000000000000000012 as the bigints 3000000000n,
    // -9223372036854775808n, 9223372036854775807n, 9223372036854775807n, 511n, 0n, 12n, 9223372036854775807n and 12n. It
    // reads `!!int "3000000000"` as 3000000000n. Without intAsBigInt it reads the 64-bit values as numbers that have lost
    // the low digits, and -0 as -0.
    testCase "Decode.int64 reads the ints of the core schema" <| fun _ ->
        let ints =
            [ "3000000000", 3000000000L; "-9223372036854775808", System.Int64.MinValue
              "9223372036854775807", System.Int64.MaxValue; "0x7FFFFFFFFFFFFFFF", System.Int64.MaxValue
              "0o777", 511L; "-0", 0L; "+12", 12L; "5", 5L; "-5", -5L
              "0o777777777777777777777", System.Int64.MaxValue; "00000000000000000000000000012", 12L ]
        for (text, expected) in ints do
            Expect.equal (Decode.int64 (fieldNode ("k: " + text))) expected (sprintf "%s is an int64" text)
        Expect.equal (Decode.int64 (fieldNode "k: !!int \"3000000000\"")) 3000000000L "An explicit tag fixes the type"

    // yaml@2.6.1 with { intAsBigInt: true } reads 9223372036854775808, -9223372036854775809, 9223372036854775810,
    // -9223372036854775810, 92233720368547758070, 18446744073709551616, 0x8000000000000000, 0x10000000000000000 and
    // 0o1000000000000000000000 as the bigints with the same digits. They are ints without a size limit, and none of them fits in
    // an int64. A wrapped sum would read some of them as a small number, such as 2^64 as 0.
    testCase "Decode.int64 rejects an int beyond 64 bits and a quoted int" <| fun _ ->
        let texts =
            [ "9223372036854775808"; "-9223372036854775809"; "9223372036854775810"; "-9223372036854775810"
              "92233720368547758070"; "18446744073709551616"
              "0x8000000000000000"; "0x10000000000000000"; "0o1000000000000000000000"
              "\"3000000000\""; "'3000000000'"; "!!str 3000000000" ]
        for text in texts do
            expectRejects Decode.int64 (fieldNode ("k: " + text)) (sprintf "%s is no int64" text)
        expectRejects Decode.int64 (fieldNode "k: |\n  3000000000\n") "A block scalar is a string"
        expectError Decode.int64 aliasNode [] (span 2 4 2 6) "The alias *x"

    // yaml@2.6.1 reads `k: 3000000000` as the number 3000000000. Decode.int stays at 32 bits and Decode.int64 reads it.
    testCase "Decode.int stays at 32 bits" <| fun _ ->
        expectRejects Decode.int (fieldNode "k: 3000000000") "3000000000 is no int"
        expectRejects Decode.int (fieldNode "k: -9223372036854775808") "Int64.MinValue is no int"
]

let private floatTests = testList "Decode.float" [
    // yaml@2.6.1 reads each of these plain texts as the number in the expectation.
    testCase "reads the float forms of the core schema" <| fun _ ->
        let cases =
            [ ".nan", nan; ".NaN", nan; ".NAN", nan
              ".inf", infinity; "+.inf", infinity; "-.inf", -infinity
              ".Inf", infinity; "+.Inf", infinity; "-.INF", -infinity
              "1e3", 1000.0; "1E3", 1000.0; ".5", 0.5; "5.", 5.0; "+1.5", 1.5; "-1.5e-3", -0.0015; "7", 7.0; "123.45678", 123.45678 ]
        for (text, expected) in cases do
            Expect.sameFloat (Decode.float (fieldNode ("k: " + text))) expected (sprintf "%s reads as %s" text (string expected))
        Expect.sameFloat (Decode.float (fieldNode "k: !!float 7")) 7.0 "An explicit tag fixes the type"

    // yaml@2.6.1 reads each of these plain texts as a string.
    testCase "rejects the texts that the core schema does not read as a float" <| fun _ ->
        for text in [ "NaN"; "nan"; "Infinity"; "-Infinity"; "inf"; ".nAn"; "+.nan"; "-.nan"; "1_000" ] do
            expectRejects Decode.float (fieldNode ("k: " + text)) (sprintf "%s is not a float" text)

    // yaml@2.6.1 reads `k: "1.5"` as the string 1.5 and `k: 1.5` as the float 1.5.
    testCase "rejects a quoted number and a number under another tag" <| fun _ ->
        Expect.equal (Decode.float (fieldNode "k: 1.5")) 1.5 "A plain 1.5 is a float"
        expectError Decode.float (fieldNode "k: \"1.5\"") [] (span 1 4 1 9) "Expected a float, found the double-quoted scalar \"1.5\"."
        expectRejects Decode.float (fieldNode "k: '1.5'") "A single-quoted 1.5 is a string"
        expectRejects Decode.float (fieldNode "k: !!str 1.5") "A number under !!str is a string"
        expectRejects Decode.float (fieldNode "k: !!int 1.5") "A float text under !!int is no float"
        expectError Decode.float aliasNode [] (span 2 4 2 6) "The alias *x"
]

let private boolTests = testList "Decode.bool" [
    // yaml@2.6.1 reads true, True, TRUE, false, False and FALSE as bools. It reads tRuE, yes, on, 1, "true" and
    // 'true' as strings, and `!!bool "true"` as true.
    testCase "reads the bools of the core schema" <| fun _ ->
        for (text, expected) in [ "true", true; "True", true; "TRUE", true; "false", false; "False", false; "FALSE", false ] do
            Expect.equal (Decode.bool (fieldNode ("k: " + text))) expected (sprintf "%s is a bool" text)
        Expect.equal (Decode.bool (fieldNode "k: !!bool \"true\"")) true "An explicit tag fixes the type"
        for text in [ "tRuE"; "yes"; "on"; "1"; "\"true\""; "'true'"; "!!str true" ] do
            expectRejects Decode.bool (fieldNode ("k: " + text)) (sprintf "%s is no bool" text)
        expectRejects Decode.bool (fieldNode "k: |\n  true\n") "A block scalar is a string"

    testCase "names the scalar and the alias that it rejects" <| fun _ ->
        expectError Decode.bool (fieldNode "k: yes") [] (span 1 4 1 7) "Expected a bool, found the plain scalar \"yes\"."
        expectError Decode.bool (fieldNode "k:\n  a: b\n") [] (span 2 3 2 7) "Expected a bool, found a mapping."
        expectError Decode.bool aliasNode [] (span 2 4 2 6) "The alias *x"
]

let private charTests = testList "Decode.char" [
    // yaml@2.6.1 reads `k: c` and `k: '~'` as the strings c and ~, and `k: "1"` and `k: !!str 1` as the string 1. It reads `k: 1` as the int 1 and `k: ~`
    // as null.
    testCase "reads a string of one character by the rules of Decode.string" <| fun _ ->
        Expect.equal (Decode.char (fieldNode "k: c")) 'c' "Plain"
        Expect.equal (Decode.char (fieldNode "k: \"1\"")) '1' "Quoted"
        Expect.equal (Decode.char (fieldNode "k: !!str 1")) '1' "Under !!str"
        Expect.equal (Decode.char (fieldNode "k: '~'")) '~' "Single-quoted"
        expectError Decode.char (fieldNode "k: 1") [] (span 1 4 1 5) "Expected a char, found the plain scalar \"1\", which resolves to !!int."
        expectError Decode.char (fieldNode "k: ~") [] (span 1 4 1 5) "Expected a char, found the plain scalar \"~\", which resolves to !!null."
        expectError Decode.char (fieldNode "k: !foo c") [] (span 1 4 1 10) "Expected a char, found the plain scalar \"c\" with the tag !foo."

    testCase "rejects a longer text, an empty text, a collection and an alias" <| fun _ ->
        expectError Decode.char (fieldNode "k: ab") [] (span 1 4 1 6) "Expected a char, found the plain scalar \"ab\"."
        expectError Decode.char (fieldNode "k: \"\"") [] (span 1 4 1 6) "Expected a char"
        expectError Decode.char (fieldNode "k:\n  - a\n") [] (span 2 3 2 6) "Expected a char, found a sequence."
        expectError Decode.char aliasNode [] (span 2 4 2 6) "The alias *x"
]

/// The fields of a time that has no zone. Such a time is a local time on every target, so the fields compare the same everywhere.
let private fieldsOf (value: System.DateTime) =
    (value.Year, value.Month, value.Day, value.Hour, value.Minute, value.Second, value.Millisecond)

/// Texts that are no timestamp. The parse of a target reads some of them as a time, such as `1`, `2020`, `5` and `12:00`.
let private notTimestamps =
    [ "1"; "2020"; "5"; "12:00"; "12:00:00"; "yesterday"; ""
      "20181001"; "2018-1-1"; "2018-10-1"; "2018/10/01"; "-2018-10-01"
      "2018-10-01T11"; "2018-10-01T11:12"
      "2018-00-10"; "2018-13-01"; "2018-10-00"; "2018-10-32"; "2018-02-30"; "2019-02-29"; "1900-02-29"
      "2018-10-01T24:00:00"; "2018-10-01T11:60:00"; "2018-10-01T11:12:60"
      "2018-10-01T11:12:55."; "2018-10-01T11:12:55.12345678"
      "2018-10-01T11:12:55z"; "2018-10-01t11:12:55"; "2018-10-01U11:12:55"
      "2018-10-01T11:12:55Z "; " 2018-10-01"; "2018-10-01T11:12:55 +01:00"
      "2018-10-01T11:12:55+0100"; "2018-10-01T11:12:55+01"; "2018-10-01T11:12:55+15:00"; "2018-10-01T11:12:55+14:30"; "2018-10-01T11:12:55+01:60"
      (String.replicate 4 (string (char 0x0661)) + "-10-01") ]

let private dateTests = testList "Decode.datetime and Decode.datetimeOffset" [
    // The old tests of both decoders were pending, because they expected a local time zone. The values below carry their own offset.
    testCase "Decode.datetime reads the ISO 8601 form" <| fun _ ->
        let expected = System.DateTime(2018, 10, 1, 11, 12, 55, System.DateTimeKind.Utc)
        Expect.equal ((Decode.datetime (fieldNode "k: 2018-10-01T11:12:55.00Z")).ToUniversalTime()) expected "A UTC time"
        Expect.equal ((Decode.datetime (fieldNode "k: \"2018-10-01T11:12:55Z\"")).ToUniversalTime()) expected "A quoted time is still text"
        expectError Decode.datetime (fieldNode "k: yesterday") [] (span 1 4 1 13) "Expected a DateTime, found the plain scalar \"yesterday\"."
        expectError Decode.datetime aliasNode [] (span 2 4 2 6) "The alias *x"

    testCase "Decode.datetimeOffset reads the ISO 8601 form with its offset" <| fun _ ->
        let expected = System.DateTimeOffset(2008, 1, 5, 6, 0, 0, System.TimeSpan(1, 0, 0))
        Expect.equal (Decode.datetimeOffset (fieldNode "k: 2008-01-05T06:00:00+01:00")) expected "An offset time"
        Expect.equal (Decode.datetimeOffset (fieldNode "k: 2008-01-05T05:00:00Z")) expected "The same instant in UTC"
        expectError Decode.datetimeOffset (fieldNode "k: soon") [] (span 1 4 1 8) "Expected a DateTimeOffset, found the plain scalar \"soon\"."
        expectError Decode.datetimeOffset aliasNode [] (span 2 4 2 6) "The alias *x"

    // yaml@2.6.1 with the core schema reads `2018-10-01` and `2018-10-01T11:12:55Z` as strings, and `1` as an int. It reads `!!timestamp 2018-10-01` as the date.
    // The decoder accepts a date, a date and a time with a T or a blank, a fraction of one to seven digits and a zone of Z or +hh:mm. A time without a zone is a
    // local time on every target. The tag does not change the check.
    testCase "Decode.datetime reads a date, a date with a time and an optional zone" <| fun _ ->
        let withoutZone =
            [ "2018-10-01", (2018, 10, 1, 0, 0, 0, 0)
              "2020-02-29", (2020, 2, 29, 0, 0, 0, 0)
              "2018-10-01T11:12:55", (2018, 10, 1, 11, 12, 55, 0)
              "2018-10-01 11:12:55", (2018, 10, 1, 11, 12, 55, 0)
              "2018-10-01T11:12:55.5", (2018, 10, 1, 11, 12, 55, 500)
              "2018-10-01 11:12:55.25", (2018, 10, 1, 11, 12, 55, 250)
              "2018-10-01T23:59:59.999", (2018, 10, 1, 23, 59, 59, 999) ]
        for (text, expected) in withoutZone do
            Expect.equal (fieldsOf (Decode.datetime (sc text))) expected (sprintf "The fields of %s" text)
        // A time with a zone is an instant, and the instant is the same on every target.
        let instant = System.DateTime(2018, 10, 1, 11, 12, 55, System.DateTimeKind.Utc)
        let withZone =
            [ "2018-10-01T11:12:55Z"; "2018-10-01 11:12:55Z"; "2018-10-01T11:12:55.0Z"; "2018-10-01T11:12:55.0000000Z"
              "2018-10-01T12:12:55+01:00"; "2018-10-01T06:12:55-05:00"; "2018-10-01T11:12:55.000+00:00"; "2018-10-01T21:12:55+10:00"
              "2018-10-01T01:12:55-10:00"; "2018-10-01T22:12:55+11:00" ]
        for text in withZone do
            let value = (Decode.datetime (sc text)).ToUniversalTime()
            Expect.equal value.Ticks instant.Ticks (sprintf "The instant of %s" text)
        Expect.equal (fieldsOf (Decode.datetime (fieldNode "k: !!timestamp 2018-10-01"))) (2018, 10, 1, 0, 0, 0, 0) "A tag does not change the check"

    testCase "Decode.datetime rejects every other text on every target" <| fun _ ->
        for text in notTimestamps do
            expectError Decode.datetime (sc text) [] None "Expected a DateTime, found"
        // The parse of JavaScript reads the ints, and the parse of .NET reads a time of day. The decoder reads none of them.
        expectError Decode.datetime (fieldNode "k: 1") [] (span 1 4 1 5) "Expected a DateTime, found the plain scalar \"1\"."
        expectError Decode.datetime (fieldNode "k: 2020") [] (span 1 4 1 8) "Expected a DateTime, found the plain scalar \"2020\"."
        expectError Decode.datetime (fieldNode "k: 5") [] (span 1 4 1 5) "Expected a DateTime, found the plain scalar \"5\"."
        expectError Decode.datetime (fieldNode "k: 12:00") [] (span 1 4 1 9) "Expected a DateTime, found the plain scalar \"12:00\"."
        expectError Decode.datetime (fieldNode "k: 2018-10-01T11:12") [] (span 1 4 1 20) "Expected a DateTime"

    testCase "Decode.datetimeOffset reads the forms with a zone and gives the same instant on every target" <| fun _ ->
        let instant = System.DateTimeOffset(2008, 1, 5, 5, 0, 0, System.TimeSpan.Zero)
        for text in [ "2008-01-05T05:00:00Z"; "2008-01-05 05:00:00Z"; "2008-01-05T06:00:00+01:00"; "2008-01-05T00:00:00-05:00"
                      "2008-01-05T05:00:00.0000000+00:00"; "2008-01-05T19:00:00+14:00"; "2008-01-05T05:00:00.5Z" ] do
            let expected = if text.EndsWith(".5Z") then instant.AddMilliseconds(500.0) else instant
            Expect.isTrue (Decode.datetimeOffset (sc text) = expected) (sprintf "The instant of %s" text)

    testCase "Decode.datetimeOffset rejects a text without a zone and every other text on every target" <| fun _ ->
        for text in notTimestamps @ [ "2018-10-01"; "2018-10-01T11:12:55"; "2018-10-01 11:12:55.5"; "2018-10-01T11:12:55.123" ] do
            expectError Decode.datetimeOffset (sc text) [] None "Expected a DateTimeOffset, found"
        // The parse of .NET reads `2018-10-01` with the offset of the machine, and the parse of JavaScript reads the ints.
        expectError Decode.datetimeOffset (fieldNode "k: 2018-10-01") [] (span 1 4 1 14) "Expected a DateTimeOffset, found the plain scalar \"2018-10-01\"."
        expectError Decode.datetimeOffset (fieldNode "k: 1") [] (span 1 4 1 5) "Expected a DateTimeOffset, found the plain scalar \"1\"."
        expectError Decode.datetimeOffset (fieldNode "k: 2020") [] (span 1 4 1 8) "Expected a DateTimeOffset, found the plain scalar \"2020\"."
        expectError Decode.datetimeOffset (fieldNode "k: 12:00") [] (span 1 4 1 9) "Expected a DateTimeOffset, found the plain scalar \"12:00\"."
        expectError Decode.datetimeOffset (fieldNode "k: 2018-10-01T11:12:55") [] (span 1 4 1 23) "Expected a DateTimeOffset"
]

// -------------------------------------------------------------------------------------------------------------------
// Options and null

let private readOption (text: string) : string option = Decode.option Decode.string (fieldNode text)

let private optionTests = testList "Decode.option" [
    // yaml@2.6.1 reads `k: null`, `k: ~`, `k: Null` and `k: NULL` as null. It reads `k:` as null too.
    testCase "reads a plain null, ~ and a missing value as None" <| fun _ ->
        for text in [ "null"; "~"; "Null"; "NULL" ] do
            Expect.equal (readOption ("k: " + text + "\n")) None (sprintf "A plain %s is None" text)
        Expect.equal (readOption "k:\n") None "A key without a value is None"
        Expect.equal (Decode.option Decode.string YamlNode.empty) None "An empty plain scalar is None"
        Expect.equal (readOption "k: &a\n") None "An anchor alone does not make a value"

    // yaml@2.6.1 reads a single-quoted 'null', a double-quoted "~", a single-quoted empty text and a `|-` block
    // scalar with the line null as strings.
    testCase "reads a quoted or block scalar as a string" <| fun _ ->
        Expect.equal (readOption "k: 'null'\n") (Some "null") "A single-quoted null is a string"
        Expect.equal (readOption "k: \"~\"\n") (Some "~") "A double-quoted ~ is a string"
        Expect.equal (readOption "k: ''\n") (Some "") "A single-quoted empty text is a string"
        Expect.equal (readOption "k: \"\"\n") (Some "") "A double-quoted empty text is a string"
        Expect.equal (readOption "k: |-\n  null\n") (Some "null") "A block scalar with the text null is a string"

    // yaml@2.6.1 reads `k: !!str null` as the string null and `k: !!str` as the empty string. It reads `k: !!null`
    // and `k: !!null null` as null. It reads `k: !foo null` and `k: ! null` as the string null.
    testCase "follows an explicit tag" <| fun _ ->
        Expect.equal (readOption "k: !!str null\n") (Some "null") "!!str null is the string"
        Expect.equal (readOption "k: !!str\n") (Some "") "!!str alone is the empty string"
        Expect.equal (readOption "k: !!null\n") None "!!null alone is null"
        Expect.equal (readOption "k: !!null null\n") None "!!null null is null"
        Expect.equal (readOption "k: !!null ~\n") None "!!null ~ is null"
        Expect.equal (Decode.option Decode.scalarText (fieldNode "k: !foo null\n")) (Some "null") "A local tag is not null"
        Expect.equal (readOption "k: ! null\n") (Some "null") "The non-specific tag is not null"
        // The string decoder behind it rejects the local tag.
        expectError (Decode.option Decode.string) (fieldNode "k: !foo null\n") [] (span 1 4 1 13) "found the plain scalar \"null\" with the tag !foo."

    // yaml@2.6.1 reads `k: !!null foo`, `k: !!null "foo"` and `k: !!null 5` as the strings with the warning TAG_RESOLVE_FAILED, and `k: !!null ""` and
    // `k: !!null ''` as null. The library takes the failed tag resolution as an error in every decoder, as `!!int foo` is one for an int.
    testCase "an explicit !!null with a text that is no null is an error" <| fun _ ->
        let message = "The tag !!null needs null, Null, NULL, ~ or an empty text, found \"foo\"."
        let node = fieldNode "k: !!null foo"
        expectError (Decode.option Decode.string) node [] (span 1 4 1 14) message
        expectError Decode.string node [] (span 1 4 1 14) message
        expectError Decode.int node [] (span 1 4 1 14) message
        expectError (Decode.object (fun get -> get.Optional.Field "k" Decode.string)) (parseRoot "k: !!null foo\n") [ "k" ] (span 1 4 1 14) message
        expectError (Decode.option Decode.string) (fieldNode "k: !!null \"foo\"") [] (span 1 4 1 16) "found \"foo\"."
        expectError (Decode.option Decode.string) (fieldNode "k: !!null 5") [] (span 1 4 1 12) "found \"5\"."
        // The null spellings and the empty text stay None, quoted or not.
        for text in [ "k: !!null\n"; "k: !!null null\n"; "k: !!null ~\n"; "k: !!null Null\n"; "k: !!null NULL\n"; "k: !!null \"\"\n"; "k: !!null ''\n"; "k: !!null \"null\"\n" ] do
            Expect.equal (readOption text) None (sprintf "%s is null" text)
        Expect.equal (Decode.scalarText (fieldNode "k: !!null \"\"")) "" "The text decoder reads the empty text"
        expectError Decode.string (fieldNode "k: !!null \"\"") [] (span 1 4 1 13) "found the double-quoted scalar \"\" with the tag !!null."

    // yaml@2.6.1 reads `k:` followed by `- a` and `- b` as {"k":["a","b"]}, `k: [a, b]` as {"k":["a","b"]}, `k:` followed
    // by `  a: b` as {"k":{"a":"b"}}, `k: {}` as {"k":{}} and `k:` as {"k":null}. Only the null goes to None. The flow forms
    // are built with the constructors, as before the parser read flow collections.
    testCase "decodes a sequence and a mapping, which are never null" <| fun _ ->
        let list = Decode.option (Decode.list Decode.string)
        Expect.equal (list (fieldNode "k:\n- a\n- b\n")) (Some [ "a"; "b" ]) "A block sequence"
        Expect.equal (list (flowSeq [ sc "a"; sc "b" ])) (Some [ "a"; "b" ]) "A flow sequence"
        Expect.equal (list (fieldNode "k:\n")) None "A key without a value is null"
        let map = Decode.option (Decode.map Decode.string Decode.string)
        Expect.equal (map (fieldNode "k:\n  a: b\n")) (Some (Map [ "a", "b" ])) "A block mapping"
        Expect.equal (map (YamlNode.mapping [])) (Some Map.empty) "The empty flow mapping is not null"
        Expect.equal (list (YamlNode.sequence [])) (Some []) "The empty flow sequence is not null"

    // yaml@2.6.1 reads the sequence `- a`, `-`, `- b` as ["a", null, "b"].
    testCase "reads an empty sequence item as None" <| fun _ ->
        let items = Decode.list (Decode.option Decode.string) (parseRoot "- a\n-\n- b\n")
        Expect.equal items [ Some "a"; None; Some "b" ] "The empty sequence item is None"

    testCase "passes the error of the inner decoder on and rejects an alias" <| fun _ ->
        expectError (Decode.option Decode.int) (fieldNode "k: abc") [] (span 1 4 1 7) "Expected an int"
        expectError (Decode.option Decode.string) aliasNode [] (span 2 4 2 6) "The alias *x at line 2, column 4"
        Expect.equal (Decode.option Decode.alias aliasNode) (Some "x") "The alias decoder accepts it"
]

// -------------------------------------------------------------------------------------------------------------------
// Collections

let private nonNullTests = testList "Decode.nonNull" [
    // yaml@2.6.1 reads `k:`, `k: ~`, `k: null`, `k: Null`, `k: NULL`, `k: !!null` and `k: !!null ""` as null, and `k: "null"`, `k: 'null'`, `k: ""` and `k: !!str`
    // as strings. The decoder rejects the nulls and the missing value at the node and hands everything else to the decoder it wraps.
    testCase "rejects a missing value with the span of the missing value" <| fun _ ->
        let decoder = Decode.nonNull Decode.string
        expectError decoder (fieldNode "k:") [] (span 1 3 1 3) "Expected a value, found a missing value."
        expectError decoder (fieldNode "k: ") [] (span 1 3 1 3) "Expected a value, found a missing value."
        expectError decoder YamlNode.empty [] None "Expected a value, found a missing value."
        // An item without a node is a missing value as well, and the index goes in the path.
        expectError (Decode.list decoder) (parseRoot "- a\n-\n- c\n") [ "[1]" ] (span 2 2 2 2) "Expected a value, found a missing value."

    testCase "rejects a plain null and names its text" <| fun _ ->
        let decoder = Decode.nonNull Decode.string
        expectError decoder (fieldNode "k: ~") [] (span 1 4 1 5) "Expected a value, found the null \"~\"."
        expectError decoder (fieldNode "k: null") [] (span 1 4 1 8) "Expected a value, found the null \"null\"."
        expectError decoder (fieldNode "k: Null") [] (span 1 4 1 8) "Expected a value, found the null \"Null\"."
        expectError decoder (fieldNode "k: NULL") [] (span 1 4 1 8) "Expected a value, found the null \"NULL\"."
        // The decoder behind it does not matter, the null is rejected first.
        expectError (Decode.nonNull Decode.int) (fieldNode "k: null") [] (span 1 4 1 8) "Expected a value, found the null"
        expectError (Decode.nonNull (Decode.list Decode.int)) (fieldNode "k: ~") [] (span 1 4 1 5) "Expected a value, found the null"

    testCase "rejects an empty text under !!null and an anchored empty node" <| fun _ ->
        let decoder = Decode.nonNull Decode.string
        expectError decoder (fieldNode "k: !!null") [] (span 1 4 1 10) "Expected a value, found the null \"\"."
        expectError decoder (fieldNode "k: !!null \"\"") [] (span 1 4 1 13) "Expected a value, found the null \"\"."
        expectError decoder (fieldNode "k: !!null null") [] (span 1 4 1 15) "Expected a value, found the null \"null\"."
        expectError decoder (fieldNode "k: &a") [] (span 1 4 1 6) "Expected a value, found the null \"\"."
        // A text under !!null that is no null stays the error of the tag, not a missing value.
        expectError decoder (fieldNode "k: !!null foo") [] (span 1 4 1 14) "The tag !!null needs null"

    testCase "passes a quoted null, an empty quoted text and every other scalar to the decoder" <| fun _ ->
        let decoder = Decode.nonNull Decode.string
        for (text, expected) in
            [ "k: \"null\"", "null"; "k: 'null'", "null"; "k: \"~\"", "~"; "k: \"\"", ""; "k: ''", ""; "k: |-\n  null\n", "null"
              "k: !!str null", "null"; "k: !!str", ""; "k: !!str ~", "~"; "k: abc", "abc"; "k: nul", "nul" ] do
            Expect.equal (decoder (fieldNode text)) expected (sprintf "The text of %s" text)
        // A number and a bool reach the decoder, which decides.
        Expect.equal (Decode.nonNull Decode.scalarText (fieldNode "k: 0")) "0" "The text decoder reads 0"
        Expect.equal (Decode.nonNull Decode.scalarText (fieldNode "k: false")) "false" "The text decoder reads false"
        expectError decoder (fieldNode "k: 0") [] (span 1 4 1 5) "Expected a string, found the plain scalar \"0\", which resolves to !!int."
        Expect.equal (Decode.nonNull Decode.int (fieldNode "k: 5")) 5 "A value goes to the int decoder"
        expectError (Decode.nonNull Decode.int) (fieldNode "k: abc") [] (span 1 4 1 7) "Expected an int, found the plain scalar \"abc\"."

    testCase "passes a collection to the decoder, also an empty one" <| fun _ ->
        Expect.equal (Decode.nonNull (Decode.list Decode.int) (fieldNode "k:\n  - 1\n  - 2\n")) [ 1; 2 ] "A block sequence"
        Expect.equal (Decode.nonNull (Decode.list Decode.int) (YamlNode.sequence [])) [] "The empty flow sequence"
        Expect.equal (Decode.nonNull (Decode.map Decode.string Decode.int) (YamlNode.mapping [])) Map.empty "The empty flow mapping"
        Expect.equal (Decode.nonNull (Decode.map Decode.string Decode.int) (fieldNode "k:\n  a: 1\n")) (Map [ "a", 1 ]) "A block mapping"

    testCase "hands an alias to the decoder, which rejects it and names it" <| fun _ ->
        expectError (Decode.nonNull Decode.string) aliasNode [] (span 2 4 2 6) "The alias *x at line 2, column 4 cannot be read as a value."
        Expect.equal (Decode.nonNull Decode.alias aliasNode) "x" "The alias decoder accepts it"

    testCase "inside Required.Field a null field is an error with the name of the field in the path" <| fun _ ->
        let decoder = Decode.object (fun get -> get.Required.Field "k" (Decode.nonNull Decode.string))
        Expect.equal (decoder (parseRoot "k: x\n")) "x" "A value"
        expectError decoder (parseRoot "k:\n") [ "k" ] (span 1 3 1 3) "Expected a value, found a missing value."
        expectError decoder (parseRoot "k: ~\n") [ "k" ] (span 1 4 1 5) "Expected a value, found the null \"~\"."
        expectError decoder (parseRoot "k: null\n") [ "k" ] (span 1 4 1 8) "Expected a value, found the null \"null\"."
        expectError decoder (parseRoot "k: !!null\n") [ "k" ] (span 1 4 1 10) "Expected a value, found the null \"\"."
        Expect.equal (decoder (parseRoot "k: \"null\"\n")) "null" "A quoted null is a string"
        Expect.equal (decoder (parseRoot "k: \"\"\n")) "" "A quoted empty text is a string"
        // A missing field is still the error of Required.Field.
        expectError decoder (parseRoot "j: 1\n") [] (span 1 1 1 5) "The mapping has no field k."

    testCase "inside Optional.Field the getter reads a null as None before the decoder runs" <| fun _ ->
        let decoder = Decode.object (fun get -> get.Optional.Field "k" (Decode.nonNull Decode.string))
        Expect.equal (decoder (parseRoot "k: x\n")) (Some "x") "A value"
        Expect.equal (decoder (parseRoot "k:\n")) None "A missing value"
        Expect.equal (decoder (parseRoot "k: ~\n")) None "A null"
        Expect.equal (decoder (parseRoot "j: 1\n")) None "A missing field"
        Expect.equal (decoder (parseRoot "k: \"null\"\n")) (Some "null") "A quoted null is a string"
        Expect.equal (decoder (parseRoot "k: \"\"\n")) (Some "") "A quoted empty text is a string"
        // The decoder behind it still reports its own errors, with the name of the field.
        let ints = Decode.object (fun get -> get.Optional.Field "k" (Decode.nonNull Decode.int))
        expectError ints (parseRoot "k: abc\n") [ "k" ] (span 1 4 1 7) "Expected an int"

    testCase "inside option a null is None and nonNull never runs" <| fun _ ->
        let decoder = Decode.option (Decode.nonNull Decode.string)
        Expect.equal (decoder (fieldNode "k: ~")) None "A null"
        Expect.equal (decoder (fieldNode "k:")) None "A missing value"
        Expect.equal (decoder (fieldNode "k: x")) (Some "x") "A value"
]

let private sequenceTests = testList "Decode.list, array, seq and resizearray" [
    // yaml@2.6.1 reads `- 1`, `- 2`, `- 3` as [1,2,3].
    testCase "read a sequence of ints" <| fun _ ->
        let node = parseRoot "- 1\n- 2\n- 3"
        Expect.equal (Decode.list Decode.int node) [ 1; 2; 3 ] "list"
        Expect.equal (Decode.array Decode.int node) [| 1; 2; 3 |] "array"
        Expect.equal (List.ofSeq (Decode.seq Decode.int node)) [ 1; 2; 3 ] "seq"
        let resized = Decode.resizearray Decode.int node
        Expect.equal (Seq.item 0 resized) 1 "The first item of the resize array"
        Expect.equal (List.ofSeq resized) [ 1; 2; 3 ] "resizearray"
        Expect.equal (Decode.list Decode.int (flowSeq [ sc "1"; sc "2" ])) [ 1; 2 ] "A flow sequence reads the same"
        Expect.equal (Decode.list Decode.int (YamlNode.sequence [])) [] "The empty sequence"

    // The comments of a block are presentation. They stand in the Before of an item or in the End of the collection, and no decoder sees them.
    // These are the old tests "Decode.list skips comments between items" and the three tests after it.
    testCase "ignore the comments of the tree" <| fun _ ->
        let nested = Decode.object (fun get -> get.Required.Field "a" (Decode.list Decode.int))
        Expect.equal (nested (parseRoot "a:\n  - 1\n  # x\n  - 2")) [ 1; 2 ] "A comment between items"
        Expect.equal (Decode.tuple2 Decode.int Decode.string (parseRoot "- 1\n# c\n- x")) (1, "x") "A comment between tuple items"
        let deep = Decode.object (fun get -> get.Required.Field "a" (Decode.object (fun get2 -> get2.Required.Field "b" Decode.string)))
        Expect.equal (deep (parseRoot "a:\n  # c\n  b: hi")) "hi" "A comment before the first field"
        let last = Decode.object (fun get -> get.Required.Field "r" (Decode.object (fun get2 -> get2.Required.Field "a" (Decode.list Decode.int))))
        Expect.equal (last (parseRoot "r:\n  a:\n    - 1\n    # end")) [ 1 ] "A comment behind the last item"
        // yaml@2.6.1 reads `- # c` followed by `  value` as the item "value".
        Expect.equal (Decode.values Decode.string (parseRoot "- # c\n  value\n- b\n")) [ "value"; "b" ] "A comment inside a sequence item"

    testCase "put the index of the failing item in the path" <| fun _ ->
        let node = parseRoot "- 1\n- 2\n- x\n- 4\n"
        expectError (Decode.list Decode.int) node [ "[2]" ] (span 3 3 3 4) "Expected an int, found the plain scalar \"x\"."
        expectError (Decode.array Decode.int) node [ "[2]" ] (span 3 3 3 4) "Expected an int"
        expectError (Decode.seq Decode.int) node [ "[2]" ] (span 3 3 3 4) "Expected an int"
        expectError (Decode.resizearray Decode.int) node [ "[2]" ] (span 3 3 3 4) "Expected an int"
        expectError (Decode.list Decode.int) (parseRoot "- &x 1\n- *x\n") [ "[1]" ] (span 2 3 2 5) "The alias *x at line 2, column 3"

    testCase "reject a node that is no sequence" <| fun _ ->
        expectError (Decode.list Decode.int) (fieldNode "k: 5") [] (span 1 4 1 5) "Expected a sequence, found the plain scalar \"5\"."
        expectError (Decode.array Decode.int) (fieldNode "k:\n  a: b\n") [] (span 2 3 2 7) "Expected a sequence, found a mapping."
        expectError (Decode.list Decode.int) aliasNode [] (span 2 4 2 6) "The alias *x"
        expectError (Decode.seq Decode.int) (fieldNode "k: ") [] (span 1 3 1 3) "found a missing value."

    testCase "a nested sequence extends the path at every level" <| fun _ ->
        let node = parseRoot "- - 1\n  - 2\n- - 3\n  - y\n"
        expectError (Decode.list (Decode.list Decode.int)) node [ "[1]"; "[1]" ] (span 4 5 4 6) "Expected an int, found the plain scalar \"y\"."
]

let private tupleTests = testList "Decode.tuple2 to tuple8" [
    // yaml@2.6.1 reads `[1, Hello World]` as [1,"Hello World"] and `[1, Hello World, 3, Bye Bye, 5, true, false, 8]` as the eight items.
    // The flow input is built with the constructors, as before the parser read flow collections.
    testCase "read a sequence of exactly n items" <| fun _ ->
        let items = [ "1"; "Hello World"; "3"; "Bye Bye"; "5"; "true"; "false"; "8" ] |> List.map sc
        let upTo (count: int) = flowSeq (List.truncate count items)
        Expect.equal (Decode.tuple2 Decode.int Decode.string (upTo 2)) (1, "Hello World") "tuple2"
        Expect.equal (Decode.tuple3 Decode.int Decode.string Decode.int (upTo 3)) (1, "Hello World", 3) "tuple3"
        Expect.equal (Decode.tuple4 Decode.int Decode.string Decode.int Decode.string (upTo 4)) (1, "Hello World", 3, "Bye Bye") "tuple4"
        Expect.equal
            (Decode.tuple5 Decode.int Decode.string Decode.int Decode.string Decode.int (upTo 5))
            (1, "Hello World", 3, "Bye Bye", 5)
            "tuple5"
        Expect.equal
            (Decode.tuple6 Decode.int Decode.string Decode.int Decode.string Decode.int Decode.bool (upTo 6))
            (1, "Hello World", 3, "Bye Bye", 5, true)
            "tuple6"
        Expect.equal
            (Decode.tuple7 Decode.int Decode.string Decode.int Decode.string Decode.int Decode.bool Decode.bool (upTo 7))
            (1, "Hello World", 3, "Bye Bye", 5, true, false)
            "tuple7"
        Expect.equal
            (Decode.tuple8 Decode.int Decode.string Decode.int Decode.string Decode.int Decode.bool Decode.bool Decode.int (upTo 8))
            (1, "Hello World", 3, "Bye Bye", 5, true, false, 8)
            "tuple8"
        Expect.equal (Decode.tuple2 Decode.int Decode.string (parseRoot "- 1\n- x\n")) (1, "x") "A block sequence"

    testCase "reject a sequence of another length and name both lengths" <| fun _ ->
        expectError (Decode.tuple2 Decode.int Decode.int) (parseRoot "- 1\n- 2\n- 3\n") [] (span 1 1 3 4) "Expected a sequence of 2 items, found a sequence of 3 items."
        expectError (Decode.tuple2 Decode.int Decode.int) (parseRoot "- 1\n") [] (span 1 1 1 4) "found a sequence of 1 items."
        expectError (Decode.tuple3 Decode.int Decode.int Decode.int) (flowSeq [ sc "1" ]) [] None "Expected a sequence of 3 items, found a sequence of 1 items."
        expectError (Decode.tuple2 Decode.int Decode.int) (fieldNode "k: 5") [] (span 1 4 1 5) "Expected a sequence of 2 items, found the plain scalar \"5\"."
        expectError (Decode.tuple2 Decode.int Decode.int) aliasNode [] (span 2 4 2 6) "The alias *x"

    testCase "put the index of the failing item in the path" <| fun _ ->
        expectError (Decode.tuple2 Decode.int Decode.int) (parseRoot "- 1\n- x\n") [ "[1]" ] (span 2 3 2 4) "Expected an int"
        expectError (Decode.tuple2 Decode.int Decode.int) (parseRoot "- x\n- 1\n") [ "[0]" ] (span 1 3 1 4) "Expected an int"
        expectError
            (Decode.tuple8 Decode.int Decode.int Decode.int Decode.int Decode.int Decode.int Decode.int Decode.int)
            (parseRoot "- 1\n- 2\n- 3\n- 4\n- 5\n- 6\n- 7\n- z\n")
            [ "[7]" ]
            (span 8 3 8 4)
            "Expected an int"
]

let private valuesTests = testList "Decode.values" [
    // yaml@2.6.1 reads `MyKey:` followed by `- test1`, `- test2`, `- test3` as {"MyKey":["test1","test2","test3"]} and the bools alike.
    testCase "read a sequence of scalars" <| fun _ ->
        let strings = Decode.object (fun get -> get.Required.Field "MyKey" (Decode.values Decode.string))
        Expect.equal (strings (parseRoot "MyKey: \n  - test1\n  - test2\n  - test3")) [ "test1"; "test2"; "test3" ] "strings"
        let bools = Decode.object (fun get -> get.Required.Field "MyKey" (Decode.values Decode.bool))
        Expect.equal (bools (parseRoot "MyKey: \n  - true\n  - true\n  - false")) [ true; true; false ] "bools"
        Expect.equal (Decode.values Decode.string (YamlNode.sequence [])) [] "The empty sequence"

    testCase "reject an item that is a collection or an alias" <| fun _ ->
        expectError (Decode.values Decode.string) (parseRoot "- a\n- - b\n") [ "[1]" ] (span 2 3 2 6) "Expected a scalar, found a sequence."
        expectError (Decode.values Decode.string) (parseRoot "- a\n- k: v\n") [ "[1]" ] (span 2 3 2 7) "Expected a scalar, found a mapping."
        expectError (Decode.values Decode.string) (parseRoot "- &x a\n- *x\n") [ "[1]" ] (span 2 3 2 5) "The alias *x at line 2, column 3"
        expectError (Decode.values Decode.string) (fieldNode "k: a") [] (span 1 4 1 5) "Expected a sequence of scalars"
        expectError (Decode.values Decode.int) (parseRoot "- 1\n- y\n") [ "[1]" ] (span 2 3 2 4) "Expected an int"
]

// -------------------------------------------------------------------------------------------------------------------
// Maps and dictionaries

let private mapTests = testList "Decode.map and Decode.dict" [
    // yaml@2.6.1 reads the mapping `my key 1: 1`, `my key 2: 2`, `my key 3: 3` as the three entries.
    testCase "read a mapping with decoded keys and values" <| fun _ ->
        let node = parseRoot "my key 1: 1\nmy key 2: 2\nmy key 3: 3"
        let expected = [ "my key 1", 1; "my key 2", 2; "my key 3", 3 ]
        Expect.equal (Decode.map Decode.string Decode.int node) (Map expected) "map"
        let dictionary = Decode.dict Decode.string Decode.int node
        Expect.equal [ for KeyValue (key, value) in dictionary -> key, value ] expected "dict keeps the source order"
        Expect.equal (Decode.map Decode.int Decode.string (parseRoot "1: a\n2: b\n")) (Map [ 1, "a"; 2, "b" ]) "The key decoder sees the key node"
        Expect.equal (Decode.map Decode.string Decode.int (YamlNode.mapping [])) Map.empty "The empty mapping"

    testCase "the key decoder gets the full key node" <| fun _ ->
        let tagOf (key: YamlNode) : string =
            match key.Properties.Tag with
            | Some tag -> YamlTag.identifier tag
            | None -> "none"
        Expect.equal (Decode.map tagOf Decode.int (parseRoot "!!str a: 1\nb: 2\n")) (Map [ "tag:yaml.org,2002:str", 1; "none", 2 ]) "The decoder sees the tag"
        let sequenceKey = parseRoot "? - a\n  - b\n: v\n"
        let decoded = Decode.dict (Decode.list Decode.string) Decode.string sequenceKey
        Expect.equal [ for KeyValue (key, value) in decoded -> key, value ] [ [ "a"; "b" ], "v" ] "A sequence key"

    // yaml@2.6.1 resolves the alias key *k to the string a and rejects the two keys a with DUPLICATE_KEY. Aliases stay symbolic here, so an alias as a key is
    // an error before any key decoder runs, also one that reads aliases. After Aliases.expandNode the two keys are the same YAML value.
    testCase "an alias as a key is an error before the key decoder runs" <| fun _ ->
        let node = parseRoot "&k a: 1\n*k : 2\n"
        expectError (Decode.map Decode.string Decode.int) node [] (span 2 1 2 3) "The alias *k at line 2, column 1"
        expectError (Decode.map Decode.alias Decode.int) node [] (span 2 1 2 3) "The alias *k at line 2, column 1"
        match Aliases.expandNode AliasOptions.defaults node with
        | Ok expanded -> expectError (Decode.map Decode.string Decode.int) expanded [] (span 1 1 1 5) "The key a occurs twice, at line 1, column 1 and at line 1, column 1."
        | Error error -> failwith error.Message

    testCase "put the key in the path of an error" <| fun _ ->
        let node = parseRoot "a: 1\nb: x\n"
        expectError (Decode.map Decode.string Decode.int) node [ "{b}" ] (span 2 4 2 5) "Expected an int, found the plain scalar \"x\"."
        expectError (Decode.dict Decode.string Decode.int) node [ "{b}" ] (span 2 4 2 5) "Expected an int"
        expectError (Decode.map Decode.int Decode.int) (parseRoot "1: 1\nz: 2\n") [ "{z}" ] (span 2 1 2 2) "Expected an int, found the plain scalar \"z\"."
        let nested = parseRoot "outer:\n  a: 1\n  b: x\n"
        expectError (Decode.map Decode.string (Decode.map Decode.string Decode.int)) nested [ "{outer}"; "{b}" ] (span 3 6 3 7) "Expected an int"

    testCase "reject a node that is no mapping" <| fun _ ->
        expectError (Decode.map Decode.string Decode.int) (fieldNode "k:\n  - a\n") [] (span 2 3 2 6) "Expected a mapping, found a sequence."
        expectError (Decode.dict Decode.string Decode.int) (fieldNode "k: 5") [] (span 1 4 1 5) "Expected a mapping, found the plain scalar \"5\"."
        expectError (Decode.map Decode.string Decode.int) aliasNode [] (span 2 4 2 6) "The alias *x"

    // yaml@2.6.1 rejects a mapping with the same key twice (DUPLICATE_KEY), and so it rejects the ints 1 and 01. The library never reads the last entry silently.
    testCase "two keys that are the same YAML value are an error at the second key that names both" <| fun _ ->
        let node = parseRoot "a: 1\nb: 2\na: 3\n"
        let message = "The key a occurs twice, at line 1, column 1 and at line 3, column 1."
        expectError (Decode.map Decode.string Decode.int) node [] (span 3 1 3 2) message
        expectError (Decode.dict Decode.string Decode.int) node [] (span 3 1 3 2) message
        // The check of the YAML keys runs before the key decoder, so the ints 1 and 01 are one key also for a decoder of their text.
        let ints = parseRoot "1: a\n01: b\n"
        let intMessage = "The key 01 at line 2, column 1 repeats the key 1 at line 1, column 1."
        expectError (Decode.map Decode.int Decode.string) ints [] (span 2 1 2 3) intMessage
        expectError (Decode.map Decode.scalarText Decode.string) ints [] (span 2 1 2 3) intMessage
        // Different keys of the node stay different after decoding when the key decoder keeps them apart.
        Expect.equal (Decode.map Decode.string Decode.int (parseRoot "\"1\": 1\n\"01\": 2\n")) (Map [ "1", 1; "01", 2 ]) "\"1\" and \"01\" differ as strings"
        let constructed = YamlNode.mapping [ YamlNode.entry (sc "a") (sc "1"); YamlNode.entry (sc "a") (sc "2") ]
        expectError (Decode.map Decode.string Decode.int) constructed [] None "The key a occurs twice, at an earlier entry and at a later entry."
        // A key that the string decoder rejects is an error of the key decoder with the key in the path.
        expectError (Decode.map Decode.string Decode.int) (parseRoot "1: 1\n") [ "{1}" ] (span 1 1 1 2) "Expected a string, found the plain scalar \"1\", which resolves to !!int."

    // yaml@2.6.1 reads `1: a` and `"1": b` as the two keys 1 and "1". The int key and the string key are distinct YAML values, and the text decoder makes
    // the same string of both. The int 1 and the float 1.0 are distinct values that the float decoder makes the same float.
    testCase "distinct YAML keys that decode to the same map key are an error that names both" <| fun _ ->
        let node = parseRoot "1: a\n\"1\": b\n"
        let message =
            "Distinct YAML keys decode to the same map key: the plain scalar \"1\" at line 1, column 1 and the double-quoted scalar \"1\" at line 2, column 1."
        expectError (Decode.map Decode.scalarText Decode.string) node [] (span 2 1 2 4) message
        expectError (Decode.dict Decode.scalarText Decode.string) node [] (span 2 1 2 4) message
        expectError (Decode.map Decode.float Decode.string) (parseRoot "1: a\n1.0: b\n") [] (span 2 1 2 4) "Distinct YAML keys decode to the same map key"
        Expect.equal (Decode.map Decode.scalarText Decode.string (parseRoot "1: a\n\"01\": b\n")) (Map [ "1", "a"; "01", "b" ]) "Distinct texts stay apart"

    // yaml@2.6.1 rejects `~: 1` followed by `null: 2` with DUPLICATE_KEY. It reads `.nan: 1` followed by `.NaN: 2` as two keys without an error, because NaN differs
    // from itself in JavaScript. Here both pairs are the same YAML key on every target. The Dictionary of .NET rejects a null key and the Dictionary of Fable
    // never finds a NaN again, so the index of the decoded keys wraps them, and two distinct YAML keys that decode to null or NaN collide.
    testCase "two null keys and two NaN keys are duplicates on every target" <| fun _ ->
        let nullKey = Decode.option Decode.string
        let nullKeys = parseRoot "~: 1\nnull: 2\n"
        expectError (Decode.map nullKey Decode.int) nullKeys [] (span 2 1 2 5) "The key null at line 2, column 1 repeats the key ~ at line 1, column 1."
        let nanKeys = parseRoot ".nan: 1\n.NaN: 2\n"
        let nanMessage = "The key .NaN at line 2, column 1 repeats the key .nan at line 1, column 1."
        expectError (Decode.map Decode.float Decode.int) nanKeys [] (span 2 1 2 5) nanMessage
        expectError (Decode.dict Decode.float Decode.int) nanKeys [] (span 2 1 2 5) nanMessage
        // The empty key of a missing key is a null key too.
        expectError (Decode.map nullKey Decode.int) (parseRoot "? \n: 1\n~: 2\n") [] (span 3 1 3 2) "The key ~ at line 3, column 1 repeats the empty key at line 1"
        // Two distinct YAML keys that the key decoder turns into null or NaN collide after decoding.
        let toNull (_: YamlNode) : string option = None
        let toNaN (_: YamlNode) : float = System.Double.NaN
        let distinct = parseRoot "a: 1\nb: 2\n"
        expectError (Decode.map toNull Decode.int) distinct [] (span 2 1 2 2) "Distinct YAML keys decode to the same map key: the plain scalar \"a\""
        expectError (Decode.map toNaN Decode.int) distinct [] (span 2 1 2 2) "Distinct YAML keys decode to the same map key"
        expectError (Decode.dict toNaN Decode.int) distinct [] (span 2 1 2 2) "Distinct YAML keys decode to the same map key"
        // One null key is a key in a map, and one NaN key is a key in a dictionary.
        Expect.equal (Decode.map nullKey Decode.int (parseRoot "~: 1\na: 2\n")) (Map [ None, 1; Some "a", 2 ]) "One null key in a map"
        let dictionary = Decode.dict Decode.float Decode.int (parseRoot ".nan: 1\n1.5: 2\n")
        Expect.equal dictionary.Count 2 "One NaN key in a dictionary"
        Expect.isTrue (dictionary |> Seq.exists (fun pair -> System.Double.IsNaN pair.Key && pair.Value = 1)) "The NaN key keeps its value"
        // .NET finds the NaN key again. The Dictionary of Fable never does, as the documentation of Decode.dict says, so the other targets enumerate.
#if !FABLE_COMPILER
        Expect.equal dictionary.[System.Double.NaN] 1 "A lookup finds the NaN key on .NET"
#endif

    // A .NET dictionary has no null key, so the decoder reports it on every target.
    testCase "a dictionary rejects a key that decodes to null on every target" <| fun _ ->
        let nullKey = Decode.option Decode.string
        expectError (Decode.dict nullKey Decode.int) (parseRoot "~: 1\n") [] (span 1 1 1 2) "The key ~ decodes to null, which a dictionary cannot hold. Decode a map instead."
        expectError (Decode.dict nullKey Decode.int) (parseRoot "a: 1\nnull: 2\n") [] (span 2 1 2 5) "The key null decodes to null"
        let some = Decode.dict nullKey Decode.int (parseRoot "a: 1\nb: 2\n")
        Expect.equal some.Count 2 "Keys that are not null still work"
]

// -------------------------------------------------------------------------------------------------------------------
// Entries, nodes and aliases

let private nodeTests = testList "Decode.entries, node, alias and tryDecode" [
    testCase "entries returns the entries of a mapping in source order" <| fun _ ->
        let node = parseRoot "# first\na: 1\nb: 2\n"
        let entries = Decode.entries node
        Expect.equal (keyNames entries) [ "a"; "b" ] "The names"
        Expect.equal (triviaText entries.Head.Before) [ "# first" ] "The entries keep their trivia"
        expectError Decode.entries (parseRoot "- a\n") [] (span 1 1 1 4) "Expected a mapping, found a sequence."
        expectError Decode.entries (fieldNode "k: 5") [] (span 1 4 1 5) "Expected a mapping, found the plain scalar \"5\"."
        expectError Decode.entries aliasNode [] (span 2 4 2 6) "The alias *x"

    testCase "node returns every node, an alias included" <| fun _ ->
        let node = parseRoot "a: 1\n"
        Expect.isTrue (LanguagePrimitives.PhysicalEquality (Decode.node node) node) "The same node"
        Expect.isTrue (LanguagePrimitives.PhysicalEquality (Decode.node aliasNode) aliasNode) "An alias too"

    testCase "alias returns the name of the anchor and rejects everything else" <| fun _ ->
        Expect.equal (Decode.alias aliasNode) "x" "The name without the star"
        expectError Decode.alias (fieldNode "k: x") [] (span 1 4 1 5) "Expected an alias, found the plain scalar \"x\"."
        expectError Decode.alias (fieldNode "k:\n  - a\n") [] (span 2 3 2 6) "Expected an alias, found a sequence."

    testCase "a decoder raises YamlDecodeException and tryDecode returns the error" <| fun _ ->
        let node = fieldNode "k: abc"
        let raised =
            try
                Decode.int node |> ignore
                None
            with YamlDecodeException error ->
                Some error
        Expect.equal raised (Some (decodeError Decode.int node)) "The exception holds the error of tryDecode"
        Expect.equal (Decode.tryDecode Decode.string node) (Ok "abc") "The success case"
        Expect.equal (decodeError Decode.int node).Message "Expected an int, found the plain scalar \"abc\"." "The message"

    testCase "the exception message names the path, the message and the place" <| fun _ ->
        let error = decodeError (Decode.object (fun get -> get.Required.Field "steps" (Decode.list Decode.int))) (parseRoot "steps:\n  - 1\n  - x\n")
        Expect.equal error.Path [ "steps"; "[1]" ] "The path"
        Expect.equal (DecodeError.pathText error) "steps[1]" "The path as text"
        Expect.equal (DecodeError.text error) "At steps[1]: Expected an int, found the plain scalar \"x\". (line 3, column 5)" "The text"
        // The Message override of an F# exception reaches e.Message on .NET only. Fable compiles it to a method that JavaScript and Python never call.
#if !FABLE_COMPILER
        let message =
            try
                Decode.int (fieldNode "k: abc") |> ignore
                ""
            with e ->
                e.Message
        Expect.stringContains message "Expected an int, found the plain scalar \"abc\"." "The message of the exception"
#endif
]

// -------------------------------------------------------------------------------------------------------------------
// Getters

type private Package = { Name: string; Version: string option }

type private DocValidation = { FormatDefinitionVersion: string; Packages: Package list }

type private DocValidationExtensionData =
    { FormatDefinitionVersion: string
      Packages: Package list
      ExtensionData: MappingEntry list }

let private packageDecoder: Decoder<Package> =
    Decode.object (fun get ->
        { Name = get.Required.Field "name" Decode.string
          Version = get.Optional.Field "version" Decode.string })

let private validationString =
    "format_definition: 3.1.4-draft\ninspection_packages:\n  - name: package1\n    version: 1.0.0\n  - name: package2\n    version: 2.0.0\n  - name: package3"

let private validationOverflowString =
    validationString + "\nauthor: TestAuthor\nauthor_emails:\n  - test1@email.com\n  - test2@email.com"

let private inspectionPackages =
    [ { Name = "package1"; Version = Some "1.0.0" }
      { Name = "package2"; Version = Some "2.0.0" }
      { Name = "package3"; Version = None } ]

let private getterTests = testList "Decode.object" [
    // yaml@2.6.1 reads `myValue1: [1,2,3,4]`, `Hello World: 42`, `ByeBye World: oh no` as the three fields. The flow sequence is built with the
    // constructors, and the test "the flow inputs of the old decode tests" keeps the original input.
    testCase "Required.Field reads the fields of a mapping" <| fun _ ->
        let node =
            YamlNode.mapping [
                YamlNode.entry (sc "myValue1") (flowSeq [ sc "1"; sc "2"; sc "3"; sc "4" ])
                YamlNode.entry (sc "Hello World") (sc "42")
                YamlNode.entry (sc "ByeBye World") (sc "oh no")
            ]
        let decoder =
            Decode.object (fun get ->
                get.Required.Field "myValue1" (Decode.list Decode.int), get.Required.Field "Hello World" Decode.int, get.Required.Field "ByeBye World" Decode.string)
        Expect.equal (decoder node) ([ 1; 2; 3; 4 ], 42, "oh no") "The three fields"

    testCase "Required.Field reports a missing field with the span of the mapping" <| fun _ ->
        let decoder = Decode.object (fun get -> get.Required.Field "b" Decode.int)
        expectError decoder (parseRoot "a: 1\n") [] (span 1 1 1 5) "The mapping has no field b."
        let outer = Decode.object (fun get -> get.Required.Field "outer" decoder)
        expectError outer (parseRoot "outer:\n  a: 1\n") [ "outer" ] (span 2 3 2 7) "The mapping has no field b."

    testCase "Required.Field puts the name of the field in the path of an error" <| fun _ ->
        expectError (Decode.object (fun get -> get.Required.Field "a" Decode.int)) (parseRoot "a: x\n") [ "a" ] (span 1 4 1 5) "Expected an int"
        // A missing value is an error for the string decoder, the empty text for the text decoder and a null for the option decoder.
        expectError (Decode.object (fun get -> get.Required.Field "label" Decode.string)) (parseRoot "label:\n") [ "label" ] (span 1 7 1 7) "Expected a string, found a missing value."
        Expect.equal (Decode.object (fun get -> get.Required.Field "label" Decode.scalarText) (parseRoot "label:\n")) "" "The empty text"
        Expect.equal (Decode.object (fun get -> get.Required.Field "label" (Decode.option Decode.string)) (parseRoot "label:\n")) None "The option decoder reads it as None"
        expectError (Decode.object (fun get -> get.Required.Field "label" Decode.int)) (parseRoot "label:\n") [ "label" ] (span 1 7 1 7) "found a missing value."

    // yaml@2.6.1 reads `myValue1: [1,2,3,4]` and `Hello World: 42` as the two fields and a third field is absent.
    testCase "Optional.Field gives None for a missing field" <| fun _ ->
        let node = YamlNode.mapping [ YamlNode.entry (sc "myValue1") (flowSeq [ sc "1"; sc "2" ]); YamlNode.entry (sc "Hello World") (sc "42") ]
        let decoder =
            Decode.object (fun get ->
                get.Optional.Field "myValue1" (Decode.list Decode.int), get.Optional.Field "Hello World" Decode.int, get.Optional.Field "ByeBye World" Decode.string)
        Expect.equal (decoder node) (Some [ 1; 2 ], Some 42, None) "The three fields"

    // yaml@2.6.1 reads `label:`, `label: # c`, `label: ~`, `label: null`, `label: Null` and `label: NULL` as {"label":null}.
    // It reads `label: "null"`, `label: 'null'` and `label: !!str null` as the string null. A missing label is absent.
    testCase "Optional.Field reads a missing field and a null value as None" <| fun _ ->
        let optional (yaml: string) = Decode.object (fun get -> get.Optional.Field "label" Decode.string) (parseRoot yaml)
        for yaml in [ "label:\n"; "label: # c\n"; "label: ~\n"; "label: null\n"; "label: Null\n"; "label: NULL\n"; "other: 1\n"; "label: !!null\n" ] do
            Expect.equal (optional yaml) None (sprintf "The field is null or missing in %s" yaml)
        for yaml in [ "label: \"null\"\n"; "label: 'null'\n"; "label: !!str null\n" ] do
            Expect.equal (optional yaml) (Some "null") (sprintf "The field is the string null in %s" yaml)
        Expect.equal (optional "label: x\n") (Some "x") "A value is Some"
        // The difference between a missing field and a null value stays visible in the entries.
        let hasField = Decode.object (fun get -> get.Entries |> List.exists (fun entry -> Decode.string entry.Key = "label"))
        Expect.isTrue (hasField (parseRoot "label:\n")) "A null field is an entry"
        Expect.isFalse (hasField (parseRoot "other: 1\n")) "A missing field is no entry"

    // yaml@2.6.1 reads `label:` followed by `- a` as {"label":["a"]}, followed by `  a: b` as {"label":{"a":"b"}}, and
    // `label: {}` as {"label":{}}.
    testCase "Optional.Field decodes a sequence and a mapping" <| fun _ ->
        let optional (decoder: Decoder<'T>) (node: YamlNode) = Decode.object (fun get -> get.Optional.Field "label" decoder) node
        Expect.equal (optional (Decode.list Decode.string) (parseRoot "label:\n- a\n")) (Some [ "a" ]) "A block sequence"
        Expect.equal (optional (Decode.map Decode.string Decode.string) (parseRoot "label:\n  a: b\n")) (Some (Map [ "a", "b" ])) "A block mapping"
        let empty = YamlNode.mapping [ YamlNode.entry (sc "label") (YamlNode.mapping []) ]
        Expect.equal (optional (Decode.map Decode.string Decode.string) empty) (Some Map.empty) "The empty flow mapping is not null"

    testCase "Optional.Field puts the name of the field in the path of an error" <| fun _ ->
        expectError (Decode.object (fun get -> get.Optional.Field "a" Decode.int)) (parseRoot "a: x\n") [ "a" ] (span 1 4 1 5) "Expected an int"

    // yaml@2.6.1 reads `label: 5` as the int 5, `label: ""` as the empty string, `label: "5"` as the string 5 and `label:` as null.
    testCase "Optional.Field with the string decoder reads None for a null and rejects a number" <| fun _ ->
        let strict = Decode.object (fun get -> get.Optional.Field "label" Decode.string)
        expectError strict (parseRoot "label: 5\n") [ "label" ] (span 1 8 1 9) "Expected a string, found the plain scalar \"5\", which resolves to !!int."
        Expect.equal (strict (parseRoot "label: \"\"\n")) (Some "") "A quoted empty text is the empty string"
        Expect.equal (strict (parseRoot "label: \"5\"\n")) (Some "5") "A quoted number is a string"
        Expect.equal (strict (parseRoot "label:\n")) None "A missing value is None"
        let text = Decode.object (fun get -> get.Optional.Field "label" Decode.scalarText)
        Expect.equal (text (parseRoot "label: 5\n")) (Some "5") "The text decoder reads the number"
        Expect.equal (text (parseRoot "label:\n")) None "The getter hides the null from the text decoder too"
        Expect.equal (text (parseRoot "label: ~\n")) None "A null spelling is None"

    // yaml@2.6.1 reads the document as the three top-level fields and a list of three packages, one of them without a version.
    testCase "nested objects read a typed value" <| fun _ ->
        let decoder =
            Decode.object (fun get ->
                { DocValidation.FormatDefinitionVersion = get.Required.Field "format_definition" Decode.string
                  Packages = get.Required.Field "inspection_packages" (Decode.list packageDecoder) })
        let actual = decoder (parseRoot validationString)
        Expect.equal actual { DocValidation.FormatDefinitionVersion = "3.1.4-draft"; Packages = inspectionPackages } "The record"

    // yaml@2.6.1 reads the same document with the fields author and author_emails besides.
    testCase "MultipleOptional.FieldList returns the entries with the given names in source order" <| fun _ ->
        let decoder =
            Decode.object (fun get ->
                { FormatDefinitionVersion = get.Required.Field "format_definition" Decode.string
                  Packages = get.Required.Field "inspection_packages" (Decode.list packageDecoder)
                  ExtensionData = get.MultipleOptional.FieldList [ "author_emails"; "author"; "absent" ] })
        let actual = decoder (parseRoot validationOverflowString)
        Expect.equal actual.FormatDefinitionVersion "3.1.4-draft" "The version"
        Expect.equal actual.Packages inspectionPackages "The packages"
        Expect.equal (keyNames actual.ExtensionData) [ "author"; "author_emails" ] "The entries come in source order, whatever the order of the names"
        Expect.equal (Decode.string actual.ExtensionData.Head.Value) "TestAuthor" "The first value"
        Expect.equal (Decode.list Decode.string actual.ExtensionData.Tail.Head.Value) [ "test1@email.com"; "test2@email.com" ] "The second value"

    testCase "Overflow.FieldList returns the entries without the given names in source order" <| fun _ ->
        let decoder =
            Decode.object (fun get ->
                { FormatDefinitionVersion = get.Required.Field "format_definition" Decode.string
                  Packages = get.Required.Field "inspection_packages" (Decode.list packageDecoder)
                  ExtensionData = get.Overflow.FieldList [ "format_definition"; "inspection_packages" ] })
        let actual = decoder (parseRoot validationOverflowString)
        Expect.equal actual.Packages inspectionPackages "The packages"
        Expect.equal (keyNames actual.ExtensionData) [ "author"; "author_emails" ] "The other entries"
        let none = Decode.object (fun get -> get.Overflow.FieldList [ "a"; "b" ]) (parseRoot "a: 1\nb: 2\n")
        Expect.isTrue none.IsEmpty "Nothing is left"
        let all = Decode.object (fun get -> get.MultipleOptional.FieldList []) (parseRoot "a: 1\nb: 2\n")
        Expect.isTrue all.IsEmpty "No name matches nothing"

    testCase "Entries returns every entry with its trivia" <| fun _ ->
        let entries = Decode.object (fun get -> get.Entries) (parseRoot "a: 1\n\n# c\nb: 2\n")
        Expect.equal (keyNames entries) [ "a"; "b" ] "The names"
        Expect.equal (triviaText entries.Tail.Head.Before) [ ""; "# c" ] "The trivia in front of the second entry"

    testCase "the path of an error through nested objects and lists" <| fun _ ->
        let text = "steps:\n  - id: a\n    run: 1\n  - id: b\n    run: 2\n  - id: c\n    run: oops\n"
        let step = Decode.object (fun get -> get.Required.Field "run" Decode.int)
        let workflow = Decode.object (fun get -> get.Required.Field "steps" (Decode.list step))
        expectError workflow (parseRoot text) [ "steps"; "[2]"; "run" ] (span 7 10 7 14) "Expected an int, found the plain scalar \"oops\"."
        let error = decodeError workflow (parseRoot text)
        Expect.equal (DecodeError.pathText error) "steps[2].run" "The path as text"

    testCase "reject a node that is no mapping" <| fun _ ->
        let decoder = Decode.object (fun get -> get.Required.Field "a" Decode.int)
        expectError decoder (fieldNode "k:\n  - a\n") [] (span 2 3 2 6) "Expected a mapping, found a sequence."
        expectError decoder (fieldNode "k: 5") [] (span 1 4 1 5) "Expected a mapping, found the plain scalar \"5\"."
        expectError decoder (fieldNode "k: ") [] (span 1 3 1 3) "Expected a mapping, found a missing value."
        expectError decoder aliasNode [] (span 2 4 2 6) "The alias *x at line 2, column 4"

    testCase "a field name that occurs twice is an error that names both" <| fun _ ->
        let decoder = Decode.object (fun get -> get.Optional.Field "b" Decode.int)
        expectError decoder (parseRoot "a: 1\nb: 2\na: 3\n") [] (span 3 1 3 2) "The key a occurs twice, at line 1, column 1 and at line 3, column 1."
        // The spelling of the name does not matter, only its text.
        expectError decoder (parseRoot "a: 1\n\"a\": 2\n") [] (span 2 1 2 4) "The key a occurs twice, at line 1, column 1 and at line 2, column 1."
        expectError decoder (parseRoot "a: 1\n!!str a: 2\n") [] (span 2 1 2 8) "The key a occurs twice, at line 1, column 1 and at line 2, column 1."
        let constructed = YamlNode.mapping [ YamlNode.entry (sc "a") (sc "1"); YamlNode.entry (sc "a") (sc "2") ]
        expectError decoder constructed [] None "The key a occurs twice, at an earlier entry and at a later entry."
        // Two keys of another type that are the same value are reported before the field names are read.
        expectError decoder (parseRoot "1: a\n0x1: b\n") [] (span 2 1 2 4) "The key 0x1 at line 2, column 1 repeats the key 1 at line 1, column 1."

    // yaml@2.6.1 reads the keys 1, true, null and `!!int 1` by the core schema and `"1"` as the string. It turns the sequence key of `? - a` into a string.
    testCase "a field name must be a string" <| fun _ ->
        let decoder = Decode.object (fun get -> get.Optional.Field "a" Decode.int)
        expectError decoder (parseRoot "? - a\n: v\n") [] (span 1 3 1 6) "Expected a string as a field name, found a sequence."
        expectError decoder (parseRoot "&x a: 1\n*x : 2\n") [] (span 2 1 2 3) "The alias *x at line 2, column 1"
        expectError decoder (parseRoot "1: a\n") [] (span 1 1 1 2) "Expected a string as a field name, found the plain scalar \"1\", which resolves to !!int."
        expectError decoder (parseRoot "true: a\n") [] (span 1 1 1 5) "found the plain scalar \"true\", which resolves to !!bool."
        expectError decoder (parseRoot "null: a\n") [] (span 1 1 1 5) "found the plain scalar \"null\", which resolves to !!null."
        expectError decoder (parseRoot "? \n: a\n") [] (span 1 2 1 2) "Expected a string as a field name, found a missing value."
        expectError decoder (parseRoot "!!int 1: a\n") [] (span 1 1 1 8) "found the plain scalar \"1\" with the tag !!int."
        expectError decoder (parseRoot "!foo a: 1\n") [] (span 1 1 1 7) "found the plain scalar \"a\" with the tag !foo."
        // The int 1 and the string "1" are distinct YAML keys, so the mapping is valid YAML, and the int is no field name.
        expectError decoder (parseRoot "1: a\n\"1\": b\n") [] (span 1 1 1 2) "Expected a string as a field name, found the plain scalar \"1\""
        Expect.equal (Decode.object (fun get -> get.Required.Field "1" Decode.string) (parseRoot "\"1\": b\n")) "b" "A quoted 1 is the field 1"
        Expect.equal (Decode.object (fun get -> get.Required.Field "1" Decode.string) (parseRoot "!!str 1: b\n")) "b" "!!str 1 is the field 1"
        Expect.equal (Decode.object (fun get -> get.Required.Field "yes" Decode.string) (parseRoot "yes: b\n")) "b" "yes is a string in YAML 1.2"
        // The names are checked when the object is read, whichever fields the builder asks for.
        expectError (Decode.object (fun _ -> 0)) (parseRoot "? - a\n: v\n") [] (span 1 3 1 6) "a string as a field name"
        // Decode.entries keeps every key, so a mapping with other keys reads through it.
        Expect.equal (Decode.entries (parseRoot "1: a\ntrue: b\n")).Length 2 "The entries of a mapping with an int key and a bool key"
]

// -------------------------------------------------------------------------------------------------------------------
// Flow inputs

let private flowTests = testList "Flow inputs" [
    // The old tests "tuple 2", "tuple 8", "object required", "object optional", "Decode.option decodes a sequence and a mapping", "Optional.Field decodes a
    // sequence and a mapping" and "Decode.option reads an empty node as None" with their original flow input.
    testCase "the flow inputs of the old decode tests" <| fun _ ->
        Expect.equal (Decode.tuple2 Decode.int Decode.string (parseRoot "[1, Hello World]")) (1, "Hello World") "tuple 2"
        Expect.equal
            (Decode.tuple8 Decode.int Decode.string Decode.int Decode.string Decode.int Decode.bool Decode.bool Decode.int (parseRoot "[1, Hello World, 3, Bye Bye, 5, true, false, 8]"))
            (1, "Hello World", 3, "Bye Bye", 5, true, false, 8)
            "tuple 8"
        let required =
            Decode.object (fun get ->
                get.Required.Field "myValue1" (Decode.list Decode.int), get.Required.Field "Hello World" Decode.int, get.Required.Field "ByeBye World" Decode.string)
        Expect.equal (required (parseRoot "myValue1: [1,2,3,4]\nHello World: 42\nByeBye World: oh no")) ([ 1; 2; 3; 4 ], 42, "oh no") "object required"
        let optional =
            Decode.object (fun get ->
                get.Optional.Field "myValue1" (Decode.list Decode.int), get.Optional.Field "Hello World" Decode.int, get.Optional.Field "ByeBye World" Decode.string)
        Expect.equal (optional (parseRoot "myValue1: [1,2,3,4]\nHello World: 42")) (Some [ 1; 2; 3; 4 ], Some 42, None) "object optional"
        Expect.equal (Decode.option (Decode.list Decode.string) (fieldNode "k: [a, b]\n")) (Some [ "a"; "b" ]) "A flow sequence"
        Expect.equal (Decode.option (Decode.map Decode.string Decode.string) (fieldNode "k: {}\n")) (Some Map.empty) "The empty flow mapping is no null"
        Expect.equal (Decode.option (Decode.map Decode.string Decode.string) (parseRoot "{}")) (Some Map.empty) "The empty flow mapping is no empty document"
]

let private oldSuiteTests = testList "Tests of the old suite" [
    // The old tests "integer", "float", "char", "bool", "some option" and "none option" of Tests.EncoderDecoder. yaml@2.6.1 reads 23, 123.45678, c, true, 42 and null as the int, the float,
    // the string c, the bool, the int and the null.
    testCase "a scalar as the root reads as an int, a float, a char, a bool and an option" <| fun _ ->
        Expect.equal (Decode.int (parseRoot "23")) 23 "integer"
        Expect.sameFloat (Decode.float (parseRoot "123.45678")) 123.45678 "float"
        Expect.equal (Decode.char (parseRoot "c")) 'c' "char"
        Expect.equal (Decode.bool (parseRoot "true")) true "bool"
        Expect.equal (Decode.option Decode.int (parseRoot "42")) (Some 42) "some option"
        Expect.equal (Decode.option Decode.int (parseRoot "null")) None "none option"

    // Tests.YamlRead "Decode reads the shape of an OpenAPI file with indentless sequences". yaml@2.6.1 reads the servers and the tags of the root, and for the operation behind the key
    // /participants.json the tags, the summary, the parameters and the response 200 behind them. The sequences stand at the column of their key.
    testCase "an OpenAPI file with indentless sequences decodes into its servers, tags and operation" <| fun _ ->
        let yaml =
            String.concat "\n" [
                "openapi: 3.0.1"
                "servers:"
                "- url: https://reference.example-site.com/reference-library/api/"
                "- url: http://localhost:4400/reference-library/api/"
                ""
                "tags:"
                "- name: documents"
                "  description: Some details on the sections offered by the service"
                "paths:"
                "  /participants.json:"
                "    get:"
                "      tags:"
                "      - participants"
                "      summary: List all Participants"
                "      parameters:"
                "      - name: id"
                "        in: path"
                "        required: true"
                "        schema:"
                "          type: string"
                "      responses:"
                "        200:"
                "          description: request went through"
            ]
        let decodeParameter = Decode.object (fun p -> p.Required.Field "name" Decode.string, p.Required.Field "in" Decode.string)
        let decodeGet =
            Decode.object (fun get ->
                get.Required.Field "tags" (Decode.list Decode.string),
                get.Required.Field "summary" Decode.string,
                get.Required.Field "parameters" (Decode.list decodeParameter),
                // The plain key 200 is an int, so the responses read as a map with int keys.
                get.Required.Field "responses" (Decode.map Decode.int (Decode.object (fun ok -> ok.Required.Field "description" Decode.string))) |> Map.find 200)
        let decoded =
            parseRoot yaml
            |> Decode.object (fun root ->
                root.Required.Field "servers" (Decode.list (Decode.object (fun s -> s.Required.Field "url" Decode.string))),
                root.Required.Field "tags" (Decode.list (Decode.object (fun t -> t.Required.Field "name" Decode.string))),
                root.Required.Field "paths" (Decode.object (fun paths ->
                    paths.Required.Field "/participants.json" (Decode.object (fun path -> path.Required.Field "get" decodeGet)))))
        let servers, tagNames, (getTags, summary, parameters, description) = decoded
        Expect.equal servers [ "https://reference.example-site.com/reference-library/api/"; "http://localhost:4400/reference-library/api/" ] "The servers"
        Expect.equal tagNames [ "documents" ] "The tags at the root"
        Expect.equal getTags [ "participants" ] "The indentless tags of the operation"
        Expect.equal summary "List all Participants" "The sibling behind the tags"
        Expect.equal parameters [ ("id", "path") ] "The indentless parameters"
        Expect.equal description "request went through" "The responses behind the parameters"

    // Tests.YamlRead "Decode reads an OpenAPI path with a parameter in its key". yaml@2.6.1 reads the key /participants/{id}.json as a plain scalar.
    testCase "an OpenAPI path with a parameter in its key decodes" <| fun _ ->
        let yaml = "paths:\n  /participants/{id}.json:\n    get:\n      tags:\n      - participants\n      parameters:\n      - name: id\n        in: path\n"
        let decoded =
            parseRoot yaml
            |> Decode.object (fun root ->
                root.Required.Field "paths" (Decode.object (fun paths ->
                    paths.Required.Field "/participants/{id}.json" (Decode.object (fun path ->
                        path.Required.Field "get" (Decode.object (fun get ->
                            get.Required.Field "tags" (Decode.list Decode.string),
                            get.Required.Field "parameters" (Decode.list (Decode.object (fun p -> p.Required.Field "name" Decode.string, p.Required.Field "in" Decode.string))))))))))
        Expect.equal decoded ([ "participants" ], [ ("id", "path") ]) "The operation behind the braces key"
]

// -------------------------------------------------------------------------------------------------------------------
// The identity of keys

/// The mapping with the two keys as explicit keys, so that any key text of one line fits.
let private twoKeys (first: string) (second: string) : YamlNode = parseRoot ("? " + first + "\n: 1\n? " + second + "\n: 2\n")

/// True when the check of the keys finds the two keys equal. Any other error fails the test.
let private sameKey (first: string) (second: string) : bool =
    match Decode.tryDecode (Decode.uniqueKeys Decode.node) (twoKeys first second) with
    | Ok _ -> false
    | Error error ->
        if error.Message.Contains("occurs twice") || error.Message.Contains("repeats") then
            true
        else
            failwith (sprintf "%s and %s: %s" first second error.Message)

/// True when the decoder accepts the node.
let private passes (decoder: Decoder<'T>) (node: YamlNode) : bool =
    match Decode.tryDecode decoder node with
    | Ok _ -> true
    | Error _ -> false

/// Every two keys of the group are one key.
let private expectOneKey (group: string list) =
    for first in group do
        for second in group do
            Expect.isTrue (sameKey first second) (sprintf "%s and %s are one key" first second)

/// Every two keys of the list are distinct keys.
let private expectDistinctKeys (keys: string list) =
    let indexed = List.indexed keys
    for (i, first) in indexed do
        for (j, second) in indexed do
            if i <> j then
                Expect.isFalse (sameKey first second) (sprintf "%s and %s are distinct keys" first second)

/// A sequence or a mapping of one entry, nested `depth` levels around the scalar.
let private deepSequence (depth: int) (leaf: string) : YamlNode =
    let mutable node = sc leaf
    for _ in 1 .. depth do
        node <- flowSeq [ node ]
    node

let private deepMapping (depth: int) (leaf: string) : YamlNode =
    let mutable node = sc leaf
    for _ in 1 .. depth do
        node <- YamlNode.mapping [ YamlNode.entry (sc "k") node ]
    node

let private keyIdentityTests = testList "The identity of keys" [
    // yaml@2.6.1 with { intAsBigInt: true } finds each pair of these ints a DUPLICATE_KEY, also the 30 digits and their hexadecimal spelling.
    testCase "an int is one key in every radix, sign and size" <| fun _ ->
        expectOneKey [ "1"; "0x1"; "0o1"; "+1"; "01"; "!!int 1"; "!!int \"0x1\""; "!!int '+001'" ]
        expectOneKey [ "0"; "-0"; "+0"; "000"; "0x0"; "0o0"; "!!int -0" ]
        expectOneKey [ "123456789012345678901234567890"; "0x18ee90ff6c373e0ee4e3f0ad2"; "0x18EE90FF6C373E0EE4E3F0AD2"; "0o143564417755415637016711617605322"; "+000123456789012345678901234567890" ]
        expectOneKey [ "-123456789012345678901234567890"; "-0123456789012345678901234567890" ]
        expectDistinctKeys [ "1"; "2"; "-1"; "123456789012345678901234567890"; "123456789012345678901234567891"; "-123456789012345678901234567890"; "0x10"; "0o10"; "10" ]

    // yaml@2.6.1 finds 1.0, 1.00, 10e-1 and 0.1e1 duplicates, and 0.0 and -0.0. It finds `!!float 1` distinct from 1.0 and `!!float 0` distinct from 0.0, and
    // 1e400 and 1e401 duplicates, because both are Infinity. The identity here is the exact decimal value, so 1e400 and 1e401 differ, and so do 1e-400 and
    // 2e-400, which both round to zero.
    testCase "a float is one key for its exact decimal value" <| fun _ ->
        expectOneKey [ "1.0"; "1.00"; "10e-1"; "0.1e1"; "100E-2"; "+1.0"; "1."; "!!float 1"; "!!float \"1.0\"" ]
        expectOneKey [ "0.0"; "-0.0"; "+0.0"; ".0"; "0e5"; "-0e-5"; "!!float 0"; "!!float -0" ]
        expectOneKey [ "-2.5"; "-25e-1"; "-0.25E1" ]
        expectOneKey [ "1e100000000000000000000"; "10e99999999999999999999"; "0.1e100000000000000000001"; "100000e99999999999999999995" ]
        expectOneKey [ "1e-100000000000000000000"; "0.01e-99999999999999999998"; "10e-100000000000000000001" ]
        expectOneKey [ "123456789012345678901234567890.5"; "1234567890123456789012345678905e-1" ]
        expectDistinctKeys [ "1.0"; "1.1"; "-1.0"; "1e400"; "1e401"; "1e-400"; "2e-400"; "0.1"; "0.10000000000000001"; "1e100000000000000000000"; "1e100000000000000000001" ]

    // yaml@2.6.1 finds .inf, .Inf, .INF and +.inf duplicates. It finds two spellings of .nan distinct, because NaN differs from itself in JavaScript.
    testCase "the infinities and not-a-number are one key each" <| fun _ ->
        expectOneKey [ ".inf"; ".Inf"; ".INF"; "+.inf"; "!!float .inf" ]
        expectOneKey [ "-.inf"; "-.Inf"; "-.INF" ]
        expectOneKey [ ".nan"; ".NaN"; ".NAN"; "!!float .nan" ]
        expectDistinctKeys [ ".inf"; "-.inf"; ".nan"; "0.0" ]

    // yaml@2.6.1 finds null, ~, Null and the empty key duplicates, and the spellings of true.
    testCase "every null spelling is one key and so is every spelling of a bool" <| fun _ ->
        expectOneKey [ "null"; "~"; "Null"; "NULL"; ""; "!!null"; "!!null ~"; "!!null \"\""; "!!null 'null'" ]
        expectOneKey [ "true"; "True"; "TRUE"; "!!bool true"; "!!bool \"True\"" ]
        expectOneKey [ "false"; "False"; "FALSE"; "!!bool FALSE" ]
        expectDistinctKeys [ "null"; "true"; "false"; "\"null\""; "\"true\""; "\"\"" ]

    // yaml@2.6.1 finds a, 'a', "a", `!!str a` and `! a` duplicates.
    testCase "a string is one key for its cooked text in every style" <| fun _ ->
        expectOneKey [ "a"; "'a'"; "\"a\""; "!!str a"; "! a"; "!<tag:yaml.org,2002:str> a"; "\"\\x61\"" ]
        expectOneKey [ "\"1\""; "'1'"; "!!str 1"; "! 1" ]
        expectOneKey [ "\"\""; "''"; "!!str"; "!" ]
        expectDistinctKeys [ "a"; "A"; "\"a \""; "\"01\""; "\"1\"" ]

    // yaml@2.6.1 finds the int 1, the string "1", the float 1.0 and the bool true distinct, and the int 0 distinct from the float 0.0.
    testCase "the tag keeps the types apart" <| fun _ ->
        expectDistinctKeys [ "1"; "\"1\""; "1.0"; "true"; "0"; "0.0"; "null"; "\"null\""; "!foo 1"; "!bar 1"; "!!binary 1"; "[1]"; "{1: 1}" ]

    // yaml@2.6.1 finds `!foo 1` and `!foo 01` distinct. A tag that is no tag of the core schema compares by its expanded identifier and the cooked text.
    testCase "a scalar under another tag is one key for its tag and its cooked text" <| fun _ ->
        expectOneKey [ "!foo a"; "!foo 'a'"; "!<!foo> a" ]
        expectOneKey [ "!!binary abc"; "!<tag:yaml.org,2002:binary> abc" ]
        expectDistinctKeys [ "!foo 1"; "!foo 01"; "!foo a"; "!bar a"; "a" ]
        // A %TAG directive spells the same tag with a handle.
        let directive = parseRoot "%TAG !e! tag:example.com,2000:\n---\n? !e!x a\n: 1\n? !<tag:example.com,2000:x> a\n: 2\n"
        expectError (Decode.uniqueKeys Decode.node) directive [] (span 5 3 5 30) "The key a occurs twice"

    // yaml@2.6.1 compares a collection key by its object and finds [1, a] and [0x1, 'a'] distinct, and {a: 1, b: 2} and {b: 2, a: 1}. YAML 1.2.2 compares the
    // items of a sequence in order and the entries of a mapping in any order (section 3.2.1.3).
    testCase "a sequence key compares its items in order and a mapping key its entries in any order" <| fun _ ->
        expectOneKey [ "[1, a]"; "[0x1, 'a']"; "!!seq [1, a]"; "! [+1, \"a\"]" ]
        expectOneKey [ "[]"; "!!seq []" ]
        expectOneKey [ "{a: 1, b: 2}"; "{b: 2, a: 1}"; "{\"b\": 02, a: +1}"; "!!map {b: 2, a: 1}" ]
        expectOneKey [ "{a: [1, {b: c}]}"; "{a: [0x1, {'b': c}]}" ]
        expectOneKey [ "!foo [1]"; "!foo [0x1]" ]
        expectDistinctKeys [ "[1, a]"; "[a, 1]"; "[1]"; "[1, 1]"; "[]"; "{}"; "{a: 1}"; "{a: 2}"; "{a: 1, b: 2}"; "!foo [1]"; "[[1]]" ]

    // yaml@2.6.1 reads `!!int nonsense` as the string with TAG_RESOLVE_FAILED. Here an explicit tag of the core schema needs a text of its type and a node of its kind.
    testCase "an explicit tag of the core schema needs a text of its type and a node of its kind" <| fun _ ->
        let check = Decode.uniqueKeys Decode.node
        expectError check (parseRoot "!!int nonsense: 1\n") [] (span 1 1 1 15) "The tag !!int needs an int of the core schema, found \"nonsense\"."
        expectError check (parseRoot "!!float 0x1: 1\n") [] (span 1 1 1 12) "The tag !!float needs a float of the core schema, found \"0x1\"."
        expectError check (parseRoot "!!bool yes: 1\n") [] (span 1 1 1 11) "The tag !!bool needs true, True, TRUE, false, False or FALSE, found \"yes\"."
        expectError check (parseRoot "!!null foo: 1\n") [] (span 1 1 1 11) "The tag !!null needs null, Null, NULL, ~ or an empty text, found \"foo\"."
        expectError check (parseRoot "!!seq a: 1\n") [] (span 1 1 1 8) "The tag !!seq needs a sequence, found \"a\"."
        expectError check (parseRoot "!!map a: 1\n") [] (span 1 1 1 8) "The tag !!map needs a mapping, found \"a\"."
        expectError check (parseRoot "? !!str [a]\n: 1\n") [] (span 1 3 1 12) "The tag !!str needs a scalar, found a sequence."
        expectError check (parseRoot "? !!map [a]\n: 1\n") [] (span 1 3 1 12) "The tag !!map needs a mapping, found a sequence."
        expectError check (parseRoot "? !!seq {a: 1}\n: 1\n") [] (span 1 3 1 15) "The tag !!seq needs a sequence, found a mapping."
        expectError check (parseRoot "? [!!int x]\n: 1\n") [] (span 1 4 1 11) "The tag !!int needs an int of the core schema"

    // yaml@2.6.1 resolves an alias key to its value. Aliases stay symbolic here, so an alias as a key or inside a key is an error.
    testCase "an alias as a key or inside a key is an error at the alias" <| fun _ ->
        let check = Decode.uniqueKeys Decode.node
        expectError check (parseRoot "&a x: 1\n*a : 2\n") [] (span 2 1 2 3) "The alias *a at line 2, column 1 cannot be read as a value."
        expectError check (parseRoot "&a x: 1\n[*a]: 2\n") [] (span 2 2 2 4) "The alias *a at line 2, column 2"
        expectError check (parseRoot "&a x: 1\n{k: *a}: 2\n") [] (span 2 5 2 7) "The alias *a at line 2, column 5"
        // An alias as a value is no key, so the check passes it.
        Expect.isTrue (passes check (parseRoot "a: &x {k: 1}\nb: *x\n")) "An alias value"

    // yaml@2.6.1 rejects the mapping inside the key with DUPLICATE_KEY.
    testCase "a mapping inside a key with two equal keys is an error at the second one" <| fun _ ->
        let check = Decode.uniqueKeys Decode.node
        expectError check (parseRoot "? {a: 1, a: 2}\n: v\n") [] (span 1 10 1 11) "The key a occurs twice, at line 1, column 4 and at line 1, column 10."
        expectError check (parseRoot "? [{1: 1, 0x1: 2}]\n: v\n") [] (span 1 11 1 14) "The key 0x1 at line 1, column 11 repeats the key 1 at line 1, column 5."

    testCase "a collection key nested 10000 levels deep needs no deep call stack" <| fun _ ->
        let check = Decode.uniqueKeys Decode.node
        let sequences = YamlNode.mapping [ YamlNode.entry (deepSequence 10000 "a") (sc "1"); YamlNode.entry (deepSequence 10000 "a") (sc "2") ]
        expectError check sequences [] None "The key (sequence) occurs twice, at an earlier entry and at a later entry."
        let differ = YamlNode.mapping [ YamlNode.entry (deepSequence 10000 "a") (sc "1"); YamlNode.entry (deepSequence 10000 "b") (sc "2") ]
        Expect.isTrue (passes check differ) "Two deep sequences that differ at the bottom"
        let mappings = YamlNode.mapping [ YamlNode.entry (deepMapping 10000 "a") (sc "1"); YamlNode.entry (deepMapping 10000 "a") (sc "2") ]
        expectError check mappings [] None "The key (mapping) occurs twice"
        expectError (Decode.object (fun _ -> 0)) mappings [] None "The key (mapping) occurs twice"
]

let private uniqueKeysTests = testList "Decode.uniqueKeys" [
    // yaml@2.6.1 rejects the repeated x inside the ignored value b with DUPLICATE_KEY.
    testCase "finds a repeated key in a value that the decoder ignores" <| fun _ ->
        let text = "a: 1\nb:\n  - x: 1\n    y: 2\n    x: 3\n"
        let decoder = Decode.object (fun get -> get.Required.Field "a" Decode.int)
        Expect.equal (decoder (parseRoot text)) 1 "The decoder alone reads the field"
        expectError (Decode.uniqueKeys decoder) (parseRoot text) [ "{b}"; "[0]" ] (span 5 5 5 6) "The key x occurs twice, at line 3, column 5 and at line 5, column 5."

    testCase "returns what the decoder returns when every mapping has distinct keys" <| fun _ ->
        let decoder = Decode.object (fun get -> get.Required.Field "a" Decode.int)
        Expect.equal (Decode.uniqueKeys decoder (parseRoot "a: 1\nb: {x: 1, y: [{x: 2}]}\n")) 1 "The value"
        let node = parseRoot "a: 1\n"
        Expect.isTrue (LanguagePrimitives.PhysicalEquality (Decode.uniqueKeys Decode.node node) node) "The node decoder returns the node"
        // An error of the decoder comes after the check.
        expectError (Decode.uniqueKeys decoder) (parseRoot "a: x\n") [ "a" ] (span 1 4 1 5) "Expected an int"

    testCase "checks a mapping before the mappings inside it, and the mappings in the order of the text" <| fun _ ->
        let nested = "a:\n  x: 1\n  x: 2\nb:\n  y: 1\n  y: 2\n"
        expectError (Decode.uniqueKeys Decode.node) (parseRoot (nested + "0x1: c\n1: d\n")) [] (span 8 1 8 2) "The key 1 at line 8, column 1 repeats the key 0x1 at line 7, column 1."
        expectError (Decode.uniqueKeys Decode.node) (parseRoot nested) [ "{a}" ] (span 3 3 3 4) "The key x occurs twice, at line 2, column 3 and at line 3, column 3."
        expectError (Decode.uniqueKeys Decode.node) (parseRoot "a: [{k: 1}, {k: 1, k: 2}]\n") [ "{a}"; "[1]" ] (span 1 20 1 21) "The key k occurs twice"

    testCase "walks a value nested 10000 levels deep with its own stack" <| fun _ ->
        let mutable node = YamlNode.mapping [ YamlNode.entry (sc "x") (sc "1"); YamlNode.entry (sc "x") (sc "2") ]
        for _ in 1 .. 10000 do
            node <- YamlNode.mapping [ YamlNode.entry (sc "k") (flowSeq [ node ]) ]
        let error = decodeError (Decode.uniqueKeys Decode.node) node
        Expect.equal error.Path.Length 20000 "A field and an item for each level"
        Expect.equal error.Path.Head "{k}" "The path starts at the root"
        Expect.equal (List.last error.Path) "[0]" "The path ends at the item that holds the mapping"
        Expect.stringContains error.Message "The key x occurs twice" "The message"

    // yaml@2.6.1 reports the repeated key while it parses. The presentation checks of Validate do not look at the values of keys.
    testCase "Validate.document keeps its presentation checks" <| fun _ ->
        Expect.equal (Validate.document (parseDocument "a: 1\na: 2\n")) [] "A repeated key breaks no rule of the presentation"
]

let Main =
    testList "Decode" [
        stringTests
        scalarTextTests
        kindTests
        intTests
        floatTests
        boolTests
        charTests
        dateTests
        optionTests
        nonNullTests
        sequenceTests
        tupleTests
        valuesTests
        mapTests
        keyIdentityTests
        uniqueKeysTests
        nodeTests
        getterTests
        flowTests
        oldSuiteTests
    ]
