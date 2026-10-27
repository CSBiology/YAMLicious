module Tests.Decode

// The decoders and the getters. Each decoder gets a success case, a wrong kind of node, an alias, and the tag and style cases of the
// core schema. The error path and the error span are asserted exactly. The expectations of the old Tests.EncoderDecoder and the decoding parts of
// Tests.YamlGenericRead and Tests.ReaderDocuments are ported to the new API, and the yaml@2.6.1 value stands above each of them as before.
//
// Old tests that changed:
// - A test with `[1, 2]` or `{}` in its input builds the node with the constructors, as it did before the parser read flow collections. The test
//   "the flow inputs of the old decode tests" reads the original inputs through the parser.
// - The model has no Nil. A missing value is an empty plain scalar, so Decode.string reads `k:` as the empty text where the old decoder rejected Nil.
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
    // yaml@2.6.1 reads each of these as a scalar. Decode.string returns the cooked text of any scalar, also of a number, a bool, a null and
    // a missing value. A tag does not change the text.
    testCase "reads the text of every scalar" <| fun _ ->
        let cases =
            [ "k: abc", "abc"; "k: 12", "12"; "k: true", "true"; "k: null", "null"; "k: ~", "~"; "k: ", ""
              "k: \"x\"", "x"; "k: 'it''s'", "it's"; "k: \"\"", ""; "k: !!int 5", "5"; "k: !!str 5", "5"; "k: !foo bar", "bar"
              "k: |\n  a\n  b\n", "a\nb\n"; "k: >-\n  a\n  b\n", "a b"; "k: a\n  b", "a b" ]
        for (text, expected) in cases do
            Expect.equal (Decode.string (fieldNode text)) expected (sprintf "The text of %s" text)

    testCase "rejects a mapping and a sequence with their span" <| fun _ ->
        expectError Decode.string (fieldNode "k:\n  a: b\n") [] (span 2 3 2 7) "Expected a string, found a mapping."
        expectError Decode.string (fieldNode "k:\n  - a\n") [] (span 2 3 2 6) "Expected a string, found a sequence."

    testCase "rejects an alias and names it with its span" <| fun _ ->
        expectError Decode.string aliasNode [] (span 2 4 2 6) "The alias *x at line 2, column 4 cannot be read as a value."
]

/// Checks that the decoder rejects a mapping, a sequence and an alias, and that the message names what the decoder expected.
let private expectKindErrors (what: string) (decoder: Decoder<'T>) =
    expectError decoder (fieldNode "k:\n  a: b\n") [] (span 2 3 2 7) (sprintf "Expected %s, found a mapping." what)
    expectError decoder (fieldNode "k:\n  - a\n") [] (span 2 3 2 6) (sprintf "Expected %s, found a sequence." what)
    expectError decoder aliasNode [] (span 2 4 2 6) "The alias *x at line 2, column 4 cannot be read as a value."

let private kindTests = testList "Every scalar decoder" [
    testCase "rejects a mapping, a sequence and an alias with the span of the node" <| fun _ ->
        expectKindErrors "a string" Decode.string
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
    // yaml@2.6.1 reads `k: c` as the string c and `k: "1"` as the string 1.
    testCase "reads a scalar of one character in any style" <| fun _ ->
        Expect.equal (Decode.char (fieldNode "k: c")) 'c' "Plain"
        Expect.equal (Decode.char (fieldNode "k: \"1\"")) '1' "Quoted"
        Expect.equal (Decode.char (fieldNode "k: 1")) '1' "A number is a scalar of one character"
        Expect.equal (Decode.char (fieldNode "k: '~'")) '~' "Single-quoted"

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
        Expect.equal (readOption "k: !foo null\n") (Some "null") "A local tag is not null"
        Expect.equal (readOption "k: ! null\n") (Some "null") "The non-specific tag is not null"

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
        Expect.equal (Decode.string (fieldNode "k: !!null \"\"")) "" "The string decoder reads the empty text"

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
              "k: !!str null", "null"; "k: !!str", ""; "k: !!str ~", "~"; "k: abc", "abc"; "k: 0", "0"; "k: false", "false"; "k: nul", "nul" ] do
            Expect.equal (decoder (fieldNode text)) expected (sprintf "The text of %s" text)
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
        let node = parseRoot "&k a: 1\n*k : 2\n"
        // The alias key is rejected by the string decoder and accepted by the alias decoder.
        expectError (Decode.map Decode.string Decode.int) node [ "{*k}" ] (span 2 1 2 3) "The alias *k at line 2, column 1"
        let keyKind =
            Decode.map (fun (key: YamlNode) -> (match key.Kind with YamlNodeKind.Alias name -> "alias " + name | _ -> "node")) Decode.int node
        Expect.equal keyKind (Map [ "alias k", 2; "node", 1 ]) "The decoder sees the alias and the scalar"
        let sequenceKey = parseRoot "? - a\n  - b\n: v\n"
        let decoded = Decode.dict (Decode.list Decode.string) Decode.string sequenceKey
        Expect.equal [ for KeyValue (key, value) in decoded -> key, value ] [ [ "a"; "b" ], "v" ] "A sequence key"

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

    // yaml@2.6.1 rejects a mapping with the same key twice (DUPLICATE_KEY). The library never reads the last entry silently.
    testCase "two entries with the same decoded key are an error that names both" <| fun _ ->
        let node = parseRoot "a: 1\nb: 2\na: 3\n"
        let message = "The key a occurs twice, at line 1, column 1 and at line 3, column 1."
        expectError (Decode.map Decode.string Decode.int) node [] (span 3 1 3 2) message
        expectError (Decode.dict Decode.string Decode.int) node [] (span 3 1 3 2) message
        // The keys are equal after decoding, whatever the spelling: the plain 1 and the quoted "1" are the same string and the ints 1 and 01 are the same int.
        expectError (Decode.map Decode.string Decode.string) (parseRoot "1: a\n\"1\": b\n") [] (span 2 1 2 4) "The key 1 occurs twice, at line 1, column 1 and at line 2, column 1."
        expectError (Decode.map Decode.int Decode.string) (parseRoot "1: a\n01: b\n") [] (span 2 1 2 3) "The key 01 occurs twice"
        // Different keys of the node stay different after decoding when the key decoder keeps them apart.
        Expect.equal (Decode.map Decode.string Decode.int (parseRoot "1: 1\n01: 2\n")) (Map [ "1", 1; "01", 2 ]) "1 and 01 differ as strings"
        let constructed = YamlNode.mapping [ YamlNode.entry (sc "a") (sc "1"); YamlNode.entry (sc "a") (sc "2") ]
        expectError (Decode.map Decode.string Decode.int) constructed [] None "The key a occurs twice, at an earlier entry and at a later entry."

    // yaml@2.6.1 rejects `~: 1` followed by `null: 2` with DUPLICATE_KEY. It reads `.nan: 1` followed by `.NaN: 2` as one key without an error. Here both pairs are
    // duplicates on every target. The Dictionary of .NET rejects a null key and the Dictionary of Fable never finds a NaN again, so the index wraps the keys.
    testCase "two null keys and two NaN keys are duplicates on every target" <| fun _ ->
        let nullKey = Decode.option Decode.string
        let nullKeys = parseRoot "~: 1\nnull: 2\n"
        expectError (Decode.map nullKey Decode.int) nullKeys [] (span 2 1 2 5) "The key null occurs twice, at line 1, column 1 and at line 2, column 1."
        let nanKeys = parseRoot ".nan: 1\n.NaN: 2\n"
        let nanMessage = "The key .NaN occurs twice, at line 1, column 1 and at line 2, column 1."
        expectError (Decode.map Decode.float Decode.int) nanKeys [] (span 2 1 2 5) nanMessage
        expectError (Decode.dict Decode.float Decode.int) nanKeys [] (span 2 1 2 5) nanMessage
        // The empty key of a missing key is a null key too.
        expectError (Decode.map nullKey Decode.int) (parseRoot "? \n: 1\n~: 2\n") [] (span 3 1 3 2) "The key ~ occurs twice"
        // One null key is a key in a map, and one NaN key is a key in a dictionary.
        Expect.equal (Decode.map nullKey Decode.int (parseRoot "~: 1\na: 2\n")) (Map [ None, 1; Some "a", 2 ]) "One null key in a map"
        let dictionary = Decode.dict Decode.float Decode.int (parseRoot ".nan: 1\n1.5: 2\n")
        Expect.equal dictionary.Count 2 "One NaN key in a dictionary"
        Expect.isTrue (dictionary |> Seq.exists (fun pair -> System.Double.IsNaN pair.Key && pair.Value = 1)) "The NaN key keeps its value"

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
        // A missing value is the empty text for the string decoder and a null for the option decoder.
        Expect.equal (Decode.object (fun get -> get.Required.Field "label" Decode.string) (parseRoot "label:\n")) "" "The empty text"
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
        expectError decoder (parseRoot "a: 1\nb: 2\na: 3\n") [] (span 3 1 3 2) "The field a occurs twice, at line 1, column 1 and at line 3, column 1."
        // The spelling of the name does not matter, only its text.
        expectError decoder (parseRoot "a: 1\n\"a\": 2\n") [] (span 2 1 2 4) "The field a occurs twice, at line 1, column 1 and at line 2, column 1."
        let constructed = YamlNode.mapping [ YamlNode.entry (sc "a") (sc "1"); YamlNode.entry (sc "a") (sc "2") ]
        expectError decoder constructed [] None "The field a occurs twice, at an earlier entry and at a later entry."

    testCase "a field name must be a scalar" <| fun _ ->
        let decoder = Decode.object (fun get -> get.Optional.Field "a" Decode.int)
        expectError decoder (parseRoot "? - a\n: v\n") [] (span 1 3 1 6) "Expected a scalar as a field name, found a sequence."
        expectError decoder (parseRoot "&x a: 1\n*x : 2\n") [] (span 2 1 2 3) "The alias *x at line 2, column 1"
        // The names are checked when the object is read, whichever fields the builder asks for.
        expectError (Decode.object (fun _ -> 0)) (parseRoot "? - a\n: v\n") [] (span 1 3 1 6) "a scalar as a field name"
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
                get.Required.Field "responses" (Decode.object (fun r -> r.Required.Field "200" (Decode.object (fun ok -> ok.Required.Field "description" Decode.string)))))
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

let Main =
    testList "Decode" [
        stringTests
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
        nodeTests
        getterTests
        flowTests
        oldSuiteTests
    ]
