module Tests.CoreSchema

open Fable.Pyxpecto
open YAMLicious

let private scalarWith (style: ScalarStyle) (text: string) : YamlScalar =
    { Value = text
      Style = style
      HeaderComment = None
      Layout = None }

let private plain (text: string) = scalarWith ScalarStyle.Plain text
let private doubleQuoted (text: string) = scalarWith ScalarStyle.DoubleQuoted text
let private singleQuoted (text: string) = scalarWith ScalarStyle.SingleQuoted text
let private literal (text: string) = scalarWith (ScalarStyle.Block (BlockScalarStyle.Literal, ChompingMode.Strip, None)) text

let private isNull (scalar: YamlScalar) = CoreSchema.isNull None scalar
let private asBool (scalar: YamlScalar) = CoreSchema.tryParseBool None scalar
let private asInt (scalar: YamlScalar) = CoreSchema.tryParseInt None scalar
let private asInt64 (scalar: YamlScalar) = CoreSchema.tryParseInt64 None scalar
let private asFloat (scalar: YamlScalar) = CoreSchema.tryParseFloat None scalar

/// True when no reader of the core schema takes the scalar for null, a bool, an int or a float.
let private isString (tag: YamlTag option) (scalar: YamlScalar) =
    not (CoreSchema.isNull tag scalar)
    && (CoreSchema.tryParseBool tag scalar).IsNone
    && (CoreSchema.tryParseInt64 tag scalar).IsNone
    && (CoreSchema.tryParseFloat tag scalar).IsNone

let tests_string_content = testList "CoreSchema.stringContent" [
    // yaml@2.6.1 reads each of these texts as null, a bool, an int or a float when it is plain, and as the
    // string itself when it is double-quoted. A plain .inf and .nan are floats, and 007 is the int 7.
    testCase "stringContent double-quotes the texts that the core schema reads as null, bool, int or float" <| fun _ ->
        let lookalikes =
            [ "null"; "Null"; "NULL"; "~"; ""
              "true"; "True"; "TRUE"; "false"; "False"; "FALSE"
              "1"; "-1"; "+1"; "007"; "0o17"; "0x1F"
              "1.5"; "-1.5"; ".5"; "5."; "1e3"; "1.5E-3"; "-.5e+2"
              ".inf"; "-.inf"; "+.inf"; ".Inf"; ".INF"; ".nan"; ".NaN"; ".NAN" ]
        for text in lookalikes do
            let made = CoreSchema.stringContent text
            Expect.equal made.Style ScalarStyle.DoubleQuoted $"'{text}' has the double-quoted style"
            Expect.equal made.Value text $"'{text}' keeps its text"
            Expect.isTrue (CoreSchema.resolvesToNonString text) $"'{text}' resolves to a non-string"
            Expect.isTrue (isString None made) $"'{text}' reads back as the string"
            Expect.isFalse (isString None (plain text)) $"'{text}' as a plain scalar is no string"

    // yaml@2.6.1 reads each of these plain texts as the string itself. YAML 1.2 does not resolve yes, no, on
    // and off, so they stay plain although YAML 1.1 reads them as bools.
    testCase "stringContent leaves the other strings plain" <| fun _ ->
        let strings = [ "abc"; "1a"; "true1"; "nulls"; "1.0.0"; "0x"; "yes"; "no"; "on"; "off" ]
        for text in strings do
            let made = CoreSchema.stringContent text
            Expect.equal made.Style ScalarStyle.Plain $"'{text}' stays plain"
            Expect.equal made.HeaderComment None $"'{text}' has no header comment"
            Expect.isFalse (CoreSchema.resolvesToNonString text) $"'{text}' resolves to a string"
            Expect.isTrue (isString None made) $"'{text}' reads back as the string"

    // yaml@2.6.1 reads a plain text with a line break as a multiline string, never as a number or null.
    testCase "a text with a line break never resolves" <| fun _ ->
        for text in [ "1\n"; "null\n"; "true\n"; "1.5\n"; "\n1"; "1\n2" ] do
            let label = text.Replace('\n', '|')
            Expect.isFalse (CoreSchema.resolvesToNonString text) (label + " resolves to a string")
            Expect.isFalse (CoreSchema.resolvesToNull text) (label + " is no null")
            Expect.isTrue (isString None (plain text)) (label + " is a string")
]

let tests_null = testList "CoreSchema.isNull" [
    // yaml@2.6.1 reads `k: null`, `k: ~`, `k: Null` and `k: NULL` as null. It reads `k:` as null too.
    testCase "isNull reads a plain null, ~ and empty text" <| fun _ ->
        for text in [ "null"; "~"; "Null"; "NULL"; "" ] do
            Expect.isTrue (isNull (plain text)) $"A plain '{text}' is null"
        for text in [ "nULL"; "nil"; "None"; "~~"; "0"; "false"; " null"; "null " ] do
            Expect.isFalse (isNull (plain text)) $"A plain '{text}' is no null"

    // yaml@2.6.1 reads a single-quoted 'null', a double-quoted "~", a single-quoted empty text and a `|-` block
    // scalar with the line null as strings.
    testCase "isNull reads a quoted or block scalar as a string" <| fun _ ->
        Expect.isFalse (isNull (singleQuoted "null")) "A single-quoted null is a string"
        Expect.isFalse (isNull (doubleQuoted "~")) "A double-quoted ~ is a string"
        Expect.isFalse (isNull (singleQuoted "")) "A single-quoted empty text is a string"
        Expect.isFalse (isNull (literal "null")) "A block scalar with the text null is a string"

    // yaml@2.6.1 reads `k: !!str null` as the string null and `k: !!str` as the empty string. It reads `k: !!null`
    // and `k: !!null null` as null. It reads `k: !foo null` and `k: ! null` as the string null.
    testCase "isNull follows an explicit tag" <| fun _ ->
        Expect.isFalse (CoreSchema.isNull (Some (YamlTag.core "str")) (plain "null")) "!!str null is the string"
        Expect.isFalse (CoreSchema.isNull (Some (YamlTag.core "str")) (plain "")) "!!str alone is the empty string"
        Expect.isTrue (CoreSchema.isNull (Some (YamlTag.core "null")) (plain "")) "!!null alone is null"
        Expect.isTrue (CoreSchema.isNull (Some (YamlTag.core "null")) (plain "null")) "!!null null is null"
        Expect.isTrue (CoreSchema.isNull (Some (YamlTag.core "null")) (doubleQuoted "~")) "The tag fixes the type, whatever the style"
        Expect.isFalse (CoreSchema.isNull (Some (YamlTag.core "null")) (plain "text")) "!!null with another text is no null"
        Expect.isFalse (CoreSchema.isNull (Some (YamlTag.local "foo")) (plain "null")) "A local tag is not null"
        Expect.isFalse (CoreSchema.isNull (Some YamlTag.NonSpecific) (plain "null")) "The non-specific tag is not null"
        Expect.isFalse (CoreSchema.isNull (Some YamlTag.NonSpecific) (plain "")) "The non-specific tag on an empty scalar is not null"

    // A %TAG directive can split the identifier tag:yaml.org,2002:null into other prefixes and suffixes.
    testCase "a tag is the same tag whatever the split of its identifier" <| fun _ ->
        let split = YamlTag.Named { Prefix = "tag:yaml.org,"; Suffix = "2002:null" }
        Expect.equal (YamlTag.identifier split) "tag:yaml.org,2002:null" "identifier"
        Expect.isTrue (CoreSchema.isNull (Some split) (plain "~")) "The split tag is !!null"
        let other = YamlTag.Named { Prefix = "tag:yaml.org,2002:"; Suffix = "nullx" }
        Expect.isFalse (CoreSchema.isNull (Some other) (plain "~")) "A longer suffix is another tag"
]

let tests_bool = testList "CoreSchema.tryParseBool" [
    // yaml@2.6.1 reads true, True, TRUE, false, False and FALSE as bools. It reads tRuE, yes, on, 1, "true" and
    // 'true' as strings, and `!!bool "true"` as true.
    testCase "tryParseBool reads the bools of the core schema" <| fun _ ->
        for (text, expected) in [ "true", true; "True", true; "TRUE", true; "false", false; "False", false; "FALSE", false ] do
            Expect.equal (asBool (plain text)) (Some expected) $"{text} is a bool"
        Expect.equal (CoreSchema.tryParseBool (Some (YamlTag.core "bool")) (doubleQuoted "true")) (Some true) "An explicit tag fixes the type"
        for text in [ "tRuE"; "yes"; "on"; "1"; "" ] do
            Expect.equal (asBool (plain text)) None $"{text} is no bool"
        Expect.equal (asBool (doubleQuoted "true")) None "A double-quoted true is a string"
        Expect.equal (asBool (singleQuoted "true")) None "A single-quoted true is a string"
        Expect.equal (asBool (literal "true")) None "A block scalar is a string"
        Expect.equal (CoreSchema.tryParseBool (Some (YamlTag.core "str")) (plain "true")) None "!!str true is a string"
]

let tests_int = testList "CoreSchema.tryParseInt and tryParseInt64" [
    // yaml@2.6.1 reads 5, +5, -5, 007, 0o17 and 0x1F as the ints 5, 5, -5, 7, 15 and 31. It reads 0X1F, -0x1, 0o8, 1_000,
    // "5" and '5' as strings, 1.5 and 5.0 as floats, and `!!int "5"` as 5.
    testCase "tryParseInt reads the ints of the core schema" <| fun _ ->
        let ints =
            [ "5", 5; "+5", 5; "-5", -5; "007", 7; "0o17", 15; "0x1F", 31; "0xff", 255
              "2147483647", 2147483647; "-2147483648", -2147483648; "0x7FFFFFFF", 2147483647 ]
        for (text, expected) in ints do
            Expect.equal (asInt (plain text)) (Some expected) $"{text} is an int"
        Expect.equal (CoreSchema.tryParseInt (Some (YamlTag.core "int")) (doubleQuoted "5")) (Some 5) "An explicit tag fixes the type"
        for text in [ "2147483648"; "-2147483649"; "0x80000000"; "1.5"; "5.0"; "0X1F"; "-0x1"; "0o8"; "1_000"; "5a"; "" ] do
            Expect.equal (asInt (plain text)) None $"{text} is no int"
        Expect.equal (asInt (doubleQuoted "5")) None "A double-quoted 5 is a string"
        Expect.equal (asInt (singleQuoted "5")) None "A single-quoted 5 is a string"

    // yaml@2.6.1 with { intAsBigInt: true } reads 3000000000, -9223372036854775808, 9223372036854775807, 0x7FFFFFFFFFFFFFFF,
    // 0o777, -0, +12, 0o777777777777777777777 and 00000000000000000000000000012 as the bigints 3000000000n,
    // -9223372036854775808n, 9223372036854775807n, 9223372036854775807n, 511n, 0n, 12n, 9223372036854775807n and 12n. It
    // reads `!!int "3000000000"` as 3000000000n. Without intAsBigInt it reads the 64-bit values as numbers that have lost
    // the low digits, and -0 as -0.
    testCase "tryParseInt64 reads the ints of the core schema" <| fun _ ->
        let ints =
            [ "3000000000", 3000000000L; "-9223372036854775808", System.Int64.MinValue
              "9223372036854775807", System.Int64.MaxValue; "0x7FFFFFFFFFFFFFFF", System.Int64.MaxValue
              "0o777", 511L; "-0", 0L; "+12", 12L; "5", 5L; "-5", -5L
              "0o777777777777777777777", System.Int64.MaxValue; "00000000000000000000000000012", 12L ]
        for (text, expected) in ints do
            Expect.equal (asInt64 (plain text)) (Some expected) $"{text} is an int64"
        Expect.equal (CoreSchema.tryParseInt64 (Some (YamlTag.core "int")) (doubleQuoted "3000000000")) (Some 3000000000L) "An explicit tag fixes the type"

    // yaml@2.6.1 with { intAsBigInt: true } reads 9223372036854775808, -9223372036854775809, 9223372036854775810,
    // -9223372036854775810, 92233720368547758070, 18446744073709551616, 0x8000000000000000, 0x10000000000000000 and
    // 0o1000000000000000000000 as the bigints with the same digits (the hexadecimal and octal texts as
    // 9223372036854775808n, 18446744073709551616n and 9223372036854775808n). They are ints without a size limit, and
    // none of them fits in an int64. A wrapped sum would read some of them as a small number, such as 2^64 as 0.
    testCase "tryParseInt64 rejects an int beyond 64 bits" <| fun _ ->
        let texts =
            [ "9223372036854775808"; "-9223372036854775809"; "9223372036854775810"; "-9223372036854775810"
              "92233720368547758070"; "18446744073709551616"
              "0x8000000000000000"; "0x10000000000000000"; "0o1000000000000000000000" ]
        for text in texts do
            Expect.equal (asInt64 (plain text)) None $"{text} is no int64"
            Expect.equal (asInt (plain text)) None $"{text} is no int"

    // yaml@2.6.1 reads `k: "3000000000"` and `k: '3000000000'` as the string 3000000000, and a `|` block scalar with
    // that line as a string too.
    testCase "tryParseInt64 rejects a quoted int" <| fun _ ->
        Expect.equal (asInt64 (doubleQuoted "3000000000")) None "A double-quoted int is a string"
        Expect.equal (asInt64 (singleQuoted "3000000000")) None "A single-quoted int is a string"
        Expect.equal (asInt64 (literal "3000000000")) None "A block scalar is a string"
        Expect.equal (CoreSchema.tryParseInt64 (Some (YamlTag.core "str")) (plain "3000000000")) None "!!str 3000000000 is a string"
        Expect.equal (CoreSchema.tryParseInt64 (Some (YamlTag.core "float")) (plain "3000000000")) None "!!float 3000000000 is no int"

    // yaml@2.6.1 reads `k: 3000000000` as the number 3000000000. The 32 bit view stays at 32 bits.
    testCase "tryParseInt stays at 32 bits" <| fun _ ->
        Expect.equal (asInt (plain "3000000000")) None "tryParseInt has no int"
        Expect.equal (asInt (plain "-9223372036854775808")) None "Int64.MinValue is no int"
        Expect.equal (asInt64 (plain "3000000000")) (Some 3000000000L) "tryParseInt64 has an int64"

    // yaml@2.6.1 reads `k: 9223372036854775808` as an int. It is no int64 here and a float, so the
    // writer still double-quotes the string.
    testCase "a text beyond 64 bits stays a non-string of the core schema" <| fun _ ->
        Expect.isTrue (CoreSchema.resolvesToNonString "9223372036854775808") "9223372036854775808 resolves"
        Expect.equal (asFloat (plain "9223372036854775808")) (Some 9223372036854775808.0) "It is a float"
        Expect.equal (CoreSchema.stringContent "9223372036854775808").Style ScalarStyle.DoubleQuoted "The string is quoted"

    // The bounds of the sum are exact on every target. Each text is one step from a bound.
    testCase "tryParseInt64 is exact at the bounds of each radix" <| fun _ ->
        let cases =
            [ "9223372036854775806", Some 9223372036854775806L
              "-9223372036854775807", Some (System.Int64.MinValue + 1L)
              "0x7FFFFFFFFFFFFFFE", Some 9223372036854775806L
              "0o777777777777777777776", Some 9223372036854775806L
              "9223372036854775808", None
              "-9223372036854775809", None
              "0x8000000000000000", None
              "0xFFFFFFFFFFFFFFFF", None
              "0o1000000000000000000000", None
              "0o2000000000000000000000", None ]
        for (text, expected) in cases do
            Expect.equal (asInt64 (plain text)) expected $"{text}"
]

let tests_float = testList "CoreSchema.tryParseFloat and floatText" [
    // yaml@2.6.1 reads each of these plain texts as the number in the expectation.
    testCase "tryParseFloat reads the float forms of the core schema" <| fun _ ->
        let cases =
            [ ".nan", nan; ".NaN", nan; ".NAN", nan
              ".inf", infinity; "+.inf", infinity; "-.inf", -infinity
              ".Inf", infinity; "+.Inf", infinity; "-.INF", -infinity
              "1e3", 1000.0; "1E3", 1000.0; ".5", 0.5; "5.", 5.0; "+1.5", 1.5; "-1.5e-3", -0.0015; "7", 7.0
              "-0.0", -0.0; "0.0", 0.0; "1.7976931348623157e308", 1.7976931348623157e308; "5e-324", 5e-324 ]
        for (text, expected) in cases do
            match asFloat (plain text) with
            | Some actual -> Expect.sameFloat actual expected $"{text}"
            | None -> failwith $"{text} is a float"

    // yaml@2.6.1 reads each of these plain texts as a string.
    testCase "tryParseFloat rejects the texts that the core schema does not read as a float" <| fun _ ->
        for text in [ "NaN"; "nan"; "Infinity"; "-Infinity"; "inf"; ".nAn"; "+.nan"; "-.nan"; "1_000"; "0x1F"; "1.5.2"; "e3"; "" ] do
            Expect.equal (asFloat (plain text)) None $"{text} is not a float"

    // yaml@2.6.1 reads `k: "1.5"` as the string 1.5 and `k: 1.5` as the float 1.5.
    testCase "tryParseFloat rejects a quoted number" <| fun _ ->
        Expect.equal (asFloat (plain "1.5")) (Some 1.5) "A plain 1.5 is a float"
        Expect.equal (asFloat (doubleQuoted "1.5")) None "A double-quoted 1.5 is a string"
        Expect.equal (asFloat (singleQuoted "1.5")) None "A single-quoted 1.5 is a string"
        Expect.equal (asFloat (literal "1.5")) None "A block scalar is a string"
        Expect.equal (CoreSchema.tryParseFloat (Some (YamlTag.core "float")) (doubleQuoted "1.5")) (Some 1.5) "!!float fixes the type"
        Expect.equal (CoreSchema.tryParseFloat (Some (YamlTag.core "str")) (plain "1.5")) None "!!str 1.5 is a string"

    // The core schema of yaml@2.6.1 resolves 1.0, 100.0 and -2.0 as floats and 1 and 1000000000000000000000 as ints.
    testCase "floatText gives a whole number a point" <| fun _ ->
        Expect.equal (CoreSchema.floatText "1") "1.0" "1"
        Expect.equal (CoreSchema.floatText "100") "100.0" "100"
        Expect.equal (CoreSchema.floatText "-2") "-2.0" "-2"
        Expect.equal (CoreSchema.floatText "+3") "+3.0" "+3"
        Expect.equal (CoreSchema.floatText "1000000000000000000000") "1000000000000000000000.0" "1e21 spelled out"

    testCase "floatText leaves a text with a point or an exponent alone" <| fun _ ->
        for text in [ "1.5"; "1e+21"; "1E+21"; "5e-324"; "-0.0"; ".nan"; ".inf"; "-.inf"; "1.0" ] do
            Expect.equal (CoreSchema.floatText text) text $"{text}"
]

let Main = testList "CoreSchema" [
    tests_string_content
    tests_null
    tests_bool
    tests_int
    tests_float
]
