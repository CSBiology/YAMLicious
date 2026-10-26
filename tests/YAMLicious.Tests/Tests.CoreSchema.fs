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

// The texts of the kind tests, grouped by the kind that the core schema gives them.
let private nullTexts = [ "null"; "Null"; "NULL"; "~"; "" ]
let private boolTexts = [ "true"; "True"; "TRUE"; "false"; "False"; "FALSE" ]
let private integerTexts =
    [ "0"; "-0"; "+0"; "5"; "-5"; "+5"; "007"; "-007"; "+007"
      "0o0"; "0o17"; "0x0"; "0x1F"; "0xff"; "0xAbC"
      "9223372036854775807"; "-9223372036854775808"; "9223372036854775808"; "-9223372036854775809"
      "0x7FFFFFFFFFFFFFFF"; "0x8000000000000000"; "0o777777777777777777777"; "0o1000000000000000000000" ]
let private floatTexts =
    [ "1.5"; "-1.5"; "+1.5"; ".5"; "5."; "1e3"; "1E3"; "1.5E-3"; "-.5e+2"; "5.0"; "-0.0"; "0.0"
      ".inf"; "-.inf"; "+.inf"; ".Inf"; "-.Inf"; ".INF"; ".nan"; ".NaN"; ".NAN" ]
let private stringTexts =
    [ "abc"; "1a"; "true1"; "nulls"; "1.0.0"; "tRuE"; "nULL"; ".nAn"; "+.nan"; "-.nan"
      "NaN"; "nan"; "Infinity"; "-Infinity"; "inf"; "0x"; "0o"; "0o8"; "0xG"; "e3"; "1e"
      "_1"; "1_"; "--1"; "+-1"; "."; "+."; "+"; "1.2.3" ]
let private yaml11Texts =
    [ "1_000"; "1__0"; "0b101"; "0B101"; "0x_1"; "-0x10"; "+0x10"; "-0o17"; "+0o17"; "0X1F"; "0O17"
      "1:20"; "1:20:30"; "yes"; "Yes"; "no"; "on"; "off"; "y"; "n" ]
let private everyKindText = nullTexts @ boolTexts @ integerTexts @ floatTexts @ stringTexts @ yaml11Texts

/// The first characters of a text and its length, as the label of a test over a long text.
let private shorten (text: string) =
    if text.Length <= 40 then text else text.Substring(0, 24) + "... (" + string text.Length + " characters)"

/// 2 to the power of the exponent. Doubling is exact, so the result needs no Math.Pow.
let private powerOfTwo (exponent: int) : float =
    let mutable value = 1.0
    for _ in 1 .. exponent do
        value <- value * 2.0
    value

let private expectFloat (text: string) (expected: float) =
    match CoreSchema.integerToFloat text with
    | Ok actual -> Expect.sameFloat actual expected (shorten text)
    | Error _ -> failwith (shorten text + " gives no float")

let private expectError (text: string) (expected: IntegerToFloatError) =
    match CoreSchema.integerToFloat text with
    | Ok _ -> failwith (shorten text + " gives a float")
    | Error error -> Expect.equal error expected (shorten text)

// Python: 2**1024 - 2**971 is the largest finite float. 2**1024 - 2**970 is halfway to 2**1024, and the tie goes
// to the even significand of 2**1024, so it is the first integer that rounds beyond the range.
let private largestDecimal = "179769313486231570814527423731704356798070567525844996598917476803157260780028538760589558632766878171540458953514382464234321326889464182768467546703537516986049910576551282076245490090389328944075868508455133942304583236903222948165808559332123348274797826204144723168738177180919299881250404026184124858368"
let private tieDecimal = "179769313486231580793728971405303415079934132710037826936173778980444968292764750946649017977587207096330286416692887910946555547851940402630657488671505820681908902000708383676273854845817711531764475730270069855571366959622842914819860834936475292719074168444365510704342711559699508093042880177904174497792"
let private belowTieDecimal = "179769313486231580793728971405303415079934132710037826936173778980444968292764750946649017977587207096330286416692887910946555547851940402630657488671505820681908902000708383676273854845817711531764475730270069855571366959622842914819860834936475292719074168444365510704342711559699508093042880177904174497791"
let private power1023Decimal = "89884656743115795386465259539451236680898848947115328636715040578866337902750481566354238661203768010560056939935696678829394884407208311246423715319737062188883946712432742638151109800623047059726541476042502884419075341171231440736956555270413618581675255342293149119973622969239858152417678164812112068608"
let private power1024Decimal = "179769313486231590772930519078902473361797697894230657273430081157732675805500963132708477322407536021120113879871393357658789768814416622492847430639474124377767893424865485276302219601246094119453082952085005768838150682342462881473913110540827237163350510684586298239947245938479716304835356329624224137216"

let tests_kind = testList "CoreSchema.resolveKind" [
    // yaml@2.6.1 with { intAsBigInt: true } reads each text of the first group as null, each of the second as a
    // bool, each of the third as a bigint and each of the fourth as a number. It reads -0 and 007 as the bigints
    // 0 and 7, and ruamel reads them as the ints 0 and 7.
    testCase "resolveKind gives each form of the core schema its kind" <| fun _ ->
        for text in nullTexts do
            Expect.equal (CoreSchema.resolveKind text) CoreKind.Null $"'{text}' is null"
        for text in boolTexts do
            Expect.equal (CoreSchema.resolveKind text) CoreKind.Bool $"'{text}' is a bool"
        for text in integerTexts do
            Expect.equal (CoreSchema.resolveKind text) CoreKind.Integer $"'{text}' is an integer"
        for text in floatTexts do
            Expect.equal (CoreSchema.resolveKind text) CoreKind.Float $"'{text}' is a float"
        for text in stringTexts do
            Expect.equal (CoreSchema.resolveKind text) CoreKind.Str $"'{text}' is a string"

    // yaml@2.6.1 with { intAsBigInt: true } reads each of these as the bigint of the same value, and ruamel reads
    // each as a Python int of any size. tryParseInt64 returns None for all of them, for the same reason as for
    // a text that is no integer.
    testCase "resolveKind reads an integer of any size as an Integer" <| fun _ ->
        let nines = String.replicate 1000 "9"
        let texts =
            [ "9223372036854775808"; "-9223372036854775809"; "92233720368547758070"; "18446744073709551616"
              "0x8000000000000000"; "0xFFFFFFFFFFFFFFFF"; "0x10000000000000000"
              "0o1000000000000000000000"; "0o2000000000000000000000"
              nines; "-" + nines; "+" + nines; "0x" + String.replicate 1000 "F"; "0o" + String.replicate 1000 "7"
              largestDecimal; power1024Decimal ]
        for text in texts do
            Expect.equal (CoreSchema.resolveKind text) CoreKind.Integer (shorten text + " is an integer")
            Expect.equal (CoreSchema.tryParseInt64 None (plain text)) None (shorten text + " has no int64")
        // The chain of tryParseInt64 and tryParseFloat finds no number in a hexadecimal or octal integer
        // beyond 64 bits, and it finds a float in a decimal one.
        Expect.isTrue (isString None (plain "0x8000000000000000")) "The chain finds no number in 0x8000000000000000"
        Expect.isTrue (isString None (plain "0o2000000000000000000000")) "The chain finds no number in 0o2000000000000000000000"
        Expect.isFalse (isString None (plain "18446744073709551616")) "The chain finds a float in 18446744073709551616"
        let padded = String.replicate 1000 "0" + "5"
        Expect.equal (CoreSchema.resolveKind padded) CoreKind.Integer "Leading zeros keep the integer"
        Expect.equal (CoreSchema.tryParseInt64 None (plain padded)) (Some 5L) "and its int64"

    // yaml@2.6.1 reads each of these plain texts as a string. ruamel with the resolver of yaml_no_ts reads 1_000,
    // 1__0, 0b101, 0x_1, -0x10, +0x10, -0o17 and +0o17 as ints and the other texts as strings. The core schema
    // has no underscore, no 0b, no sign on 0x and 0o, no capital X or O, no sexagesimal and no boolean yes.
    testCase "resolveKind reads the spellings of YAML 1.1 as strings" <| fun _ ->
        for text in yaml11Texts do
            Expect.equal (CoreSchema.resolveKind text) CoreKind.Str $"'{text}' is a string"

    // yaml@2.6.1 reads a plain text with a line break as a multiline string, never as a number or null. A plain
    // scalar never starts or ends with a blank, so a text with one there is no number.
    testCase "resolveKind reads a text with a line break or an edge blank as a string" <| fun _ ->
        for text in [ "1\n"; "null\n"; "true\n"; "1.5\n"; ".inf\n"; "\n1"; "1\n2"; "0x1F\n"; " 1"; "1 "; " null"; "true " ] do
            let label = text.Replace('\n', '|')
            Expect.equal (CoreSchema.resolveKind text) CoreKind.Str (label + " is a string")

    testCase "resolveKind agrees with resolvesToNull, resolvesToNonString and the readers" <| fun _ ->
        for text in everyKindText do
            let kind = CoreSchema.resolveKind text
            let scalar = plain text
            Expect.equal (kind = CoreKind.Null) (CoreSchema.resolvesToNull text) $"'{text}' null"
            Expect.equal (kind = CoreKind.Null) (CoreSchema.isNull None scalar) $"'{text}' isNull"
            Expect.equal (kind <> CoreKind.Str) (CoreSchema.resolvesToNonString text) $"'{text}' resolvesToNonString"
            Expect.equal (kind = CoreKind.Bool) (CoreSchema.tryParseBool None scalar).IsSome $"'{text}' tryParseBool"
            if kind = CoreKind.Str then
                Expect.isTrue (isString None scalar) $"'{text}' isString"
            if kind = CoreKind.Null || kind = CoreKind.Bool || kind = CoreKind.Float then
                Expect.isFalse (isString None scalar) $"'{text}' isString"
            if kind = CoreKind.Float then
                Expect.isTrue (CoreSchema.tryParseFloat None scalar).IsSome $"'{text}' tryParseFloat"
                Expect.isTrue (CoreSchema.tryParseInt64 None scalar).IsNone $"'{text}' tryParseInt64"
            if kind = CoreKind.Integer then
                let isRadixPrefixed = text.Length > 1 && text.[0] = '0' && (text.[1] = 'x' || text.[1] = 'o')
                Expect.equal (CoreSchema.tryParseFloat None scalar).IsSome (not isRadixPrefixed) $"'{text}' tryParseFloat"
]

let tests_integer_to_float = testList "CoreSchema.integerToFloat" [
    // yaml@2.6.1 reads each of these as the number in the expectation, and ruamel reads each as the int of
    // that value, which float turns into the same number. yaml@2.6.1 reads -0 as the number -0, and ruamel reads
    // it as the int 0. The integer zero has no sign, so the result is +0.0.
    testCase "integerToFloat converts the integers that a float holds exactly" <| fun _ ->
        let cases =
            [ "0", 0.0; "-0", 0.0; "+0", 0.0; "0x0", 0.0; "0o0", 0.0; "000", 0.0
              "5", 5.0; "-5", -5.0; "+5", 5.0; "007", 7.0; "-007", -7.0
              "0o17", 15.0; "0x1F", 31.0; "0x1f", 31.0; "0xff", 255.0; "0xFF", 255.0; "0xAbC", 2748.0
              "9007199254740992", powerOfTwo 53; "-9007199254740992", -(powerOfTwo 53)
              "0x20000000000000", powerOfTwo 53; "0o400000000000000000", powerOfTwo 53
              "9007199254740991", powerOfTwo 53 - 1.0; "0x1FFFFFFFFFFFFF", powerOfTwo 53 - 1.0 ]
        for (text, expected) in cases do
            expectFloat text expected

    // yaml@2.6.1 reads 9007199254740993 as 9007199254740992, 9007199254740995 as 9007199254740996,
    // 0x10000000000000800 as 18446744073709552000 (2^64), 0x10000000000000801 as 18446744073709556000 (2^64 + 4096),
    // 0x10000000000001800 as 18446744073709560000 (2^64 + 8192) and 0x100000000000017FF as 2^64 + 4096. The
    // decimal forms of these four values read as the same numbers. ruamel reads the ints, and float of an int
    // rounds a tie to the even significand in the same way. The spacing of the floats is 2 at 2^53 and 4096 at 2^64.
    testCase "integerToFloat rounds a value halfway between two floats to the even one" <| fun _ ->
        let twoTo64 = powerOfTwo 64
        let cases =
            [ "9007199254740993", powerOfTwo 53
              "9007199254740995", powerOfTwo 53 + 4.0
              "-9007199254740993", -(powerOfTwo 53)
              "-9007199254740995", -(powerOfTwo 53 + 4.0)
              "0x10000000000000800", twoTo64
              "0x10000000000000801", twoTo64 + powerOfTwo 12
              "0x10000000000001800", twoTo64 + powerOfTwo 13
              "0x100000000000017FF", twoTo64 + powerOfTwo 12
              "0x100000000000007FF", twoTo64
              "18446744073709553664", twoTo64
              "18446744073709553665", twoTo64 + powerOfTwo 12
              "18446744073709557760", twoTo64 + powerOfTwo 13
              "18446744073709557759", twoTo64 + powerOfTwo 12
              "18446744073709553663", twoTo64
              "0o2000000000000000004000", twoTo64
              "0o2000000000000000004001", twoTo64 + powerOfTwo 12
              "0o2000000000000000014000", twoTo64 + powerOfTwo 13
              "0o2000000000000000013777", twoTo64 + powerOfTwo 12
              "-18446744073709553664", -twoTo64
              "-18446744073709553665", -(twoTo64 + powerOfTwo 12)
              "-18446744073709557760", -(twoTo64 + powerOfTwo 13) ]
        for (text, expected) in cases do
            expectFloat text expected

    // A tie needs every bit below the halfway bit to be clear, and one set bit far below breaks it. The texts
    // are the hexadecimal ones of the previous test followed by 40 digits, which multiplies them by 2^160.
    // Python: float(int("0x10000000000000800" + "0" * 40, 16)) is 2.0**224, and the same text with a final 1 is
    // 2.0**224 + 2.0**172. float(int("0x10000000000001800" + "0" * 40, 16)) is 2.0**224 + 2.0**173.
    testCase "integerToFloat looks at every bit below the halfway bit" <| fun _ ->
        let zeros = String.replicate 40 "0"
        expectFloat ("0x10000000000000800" + zeros) (powerOfTwo 224)
        expectFloat ("0x10000000000000800" + String.replicate 39 "0" + "1") (powerOfTwo 224 + powerOfTwo 172)
        expectFloat ("0x10000000000001800" + zeros) (powerOfTwo 224 + powerOfTwo 173)
        expectFloat ("0x100000000000017FF" + String.replicate 40 "F") (powerOfTwo 224 + powerOfTwo 172)
        expectFloat ("0x100000000000007FF" + String.replicate 40 "F") (powerOfTwo 224)

    // yaml@2.6.1 reads 9223372036854775807 and 9223372036854775808 as the number 9223372036854776000 (2^63),
    // -9223372036854775809 as -9223372036854776000, and 0xFFFFFFFFFFFFFFFF and 0o1777777777777777777777 as
    // 18446744073709552000 (2^64). The floats below 2^63 are 1024 apart, and the ones above it 2048 apart.
    testCase "integerToFloat rounds the integers around the bounds of the int64" <| fun _ ->
        let twoTo63 = powerOfTwo 63
        let twoTo64 = powerOfTwo 64
        let cases =
            [ "9223372036854775807", twoTo63
              "9223372036854775808", twoTo63
              "-9223372036854775807", -twoTo63
              "-9223372036854775808", -twoTo63
              "-9223372036854775809", -twoTo63
              "9223372036854777855", twoTo63 + powerOfTwo 11
              "0x7FFFFFFFFFFFFFFF", twoTo63
              "0x8000000000000000", twoTo63
              "0x8000000000000400", twoTo63
              "0x8000000000000401", twoTo63 + powerOfTwo 11
              "0xFFFFFFFFFFFFFFFF", twoTo64
              "0o777777777777777777777", twoTo63
              "0o1000000000000000000000", twoTo63
              "0o1777777777777777777777", twoTo64
              "18446744073709551615", twoTo64
              "18446744073709551616", twoTo64
              "18446744073709551617", twoTo64 ]
        for (text, expected) in cases do
            expectFloat text expected

    // ruamel reads these as ints, and Python float of each is the power of two. yaml@2.6.1 reads each of them as that
    // power of two too.
    testCase "integerToFloat converts the powers of two up to 2^1023" <| fun _ ->
        expectFloat ("0x8" + String.replicate 255 "0") (powerOfTwo 1023)
        expectFloat ("0o1" + String.replicate 341 "0") (powerOfTwo 1023)
        expectFloat power1023Decimal (powerOfTwo 1023)
        expectFloat ("-" + power1023Decimal) (-(powerOfTwo 1023))
        expectFloat ("0x1" + String.replicate 100 "0") (powerOfTwo 400)
        expectFloat ("0o4" + String.replicate 100 "0") (powerOfTwo 302)

    // ruamel reads these as ints. Python float of the largest finite float and of the integer one below the tie
    // gives 1.7976931348623157e308, and float of the tie 2**1024 - 2**970 and of 2**1024 raises OverflowError (int
    // too large to convert to float). yaml@2.6.1 reads the tie and 2**1024 as Infinity. The tie rounds to the
    // even significand of 2^1024, so the first integer beyond the range is the tie.
    testCase "integerToFloat keeps the largest finite float and reports the first integer beyond it" <| fun _ ->
        let largest = System.Double.MaxValue
        let hexadecimal (digit: string) (fill: string) = "0x" + String.replicate 13 "F" + digit + String.replicate 242 fill
        let octal (digit: string) (fill: string) = "0o1" + String.replicate 17 "7" + digit + String.replicate 323 fill
        expectFloat largestDecimal largest
        expectFloat ("-" + largestDecimal) (-largest)
        expectFloat (hexadecimal "8" "0") largest
        expectFloat (octal "4" "0") largest
        expectFloat belowTieDecimal largest
        expectFloat ("-" + belowTieDecimal) (-largest)
        expectFloat (hexadecimal "B" "F") largest
        expectFloat (octal "5" "7") largest
        expectError tieDecimal IntegerToFloatError.BeyondFloatRange
        expectError ("-" + tieDecimal) IntegerToFloatError.BeyondFloatRange
        expectError (hexadecimal "C" "0") IntegerToFloatError.BeyondFloatRange
        expectError (octal "6" "0") IntegerToFloatError.BeyondFloatRange
        expectError power1024Decimal IntegerToFloatError.BeyondFloatRange
        expectError ("-" + power1024Decimal) IntegerToFloatError.BeyondFloatRange
        expectError ("0x1" + String.replicate 256 "0") IntegerToFloatError.BeyondFloatRange
        expectError ("0o2" + String.replicate 341 "0") IntegerToFloatError.BeyondFloatRange
        expectError ("0x" + String.replicate 257 "F") IntegerToFloatError.BeyondFloatRange

    // yaml@2.6.1 reads 1 followed by 400 zeros as Infinity, and ruamel reads it as an int that float rejects.
    // A text of 100000 digits is a hostile input, and the conversion stops at the first digit that passes 1040 bits.
    testCase "integerToFloat skips leading zeros and gives up early on a long text" <| fun _ ->
        expectFloat (String.replicate 400 "0" + "7") 7.0
        expectFloat ("-" + String.replicate 400 "0" + "7") (-7.0)
        expectFloat ("-" + String.replicate 400 "0") 0.0
        expectFloat ("0x" + String.replicate 100000 "0" + "1") 1.0
        expectFloat ("0o" + String.replicate 100000 "0" + "7") 7.0
        expectFloat (String.replicate 100000 "0" + "5") 5.0
        expectFloat ("1" + String.replicate 308 "0") 1e308
        expectError ("1" + String.replicate 309 "0") IntegerToFloatError.BeyondFloatRange
        expectError ("1" + String.replicate 400 "0") IntegerToFloatError.BeyondFloatRange
        expectError (String.replicate 100000 "9") IntegerToFloatError.BeyondFloatRange
        expectError ("-" + String.replicate 100000 "9") IntegerToFloatError.BeyondFloatRange
        expectError ("0x" + String.replicate 100000 "F") IntegerToFloatError.BeyondFloatRange
        expectError ("0o" + String.replicate 100000 "7") IntegerToFloatError.BeyondFloatRange

    // yaml@2.6.1 reads these plain texts as strings or floats, and ruamel reads 0b101, -0x10, +0x10, -0o17, +0o17
    // and 1_000 as ints. The core schema has none of these as an integer.
    testCase "integerToFloat reports a text that is no core integer" <| fun _ ->
        let texts =
            [ ""; "1.5"; "5.0"; "5."; ".5"; "1e3"; "1E3"; ".inf"; "-.inf"; ".nan"; "null"; "~"; "true"; "abc"
              "0b101"; "-0x10"; "+0x10"; "-0o17"; "+0o17"; "0X1F"; "0O17"; "1_000"; "1:20"; "yes"
              "0x"; "0o"; "0o8"; "0xG"; "0x1.8"; " 1"; "1 "; "1\n"; "\n1"; "--1"; "+-1"; "+"; "-" ]
        for text in texts do
            expectError text IntegerToFloatError.NotAnInteger
        for text in everyKindText do
            let isInteger = CoreSchema.resolveKind text = CoreKind.Integer
            let notAnInteger =
                match CoreSchema.integerToFloat text with
                | Error IntegerToFloatError.NotAnInteger -> true
                | _ -> false
            Expect.equal notAnInteger (not isInteger) $"'{text}' follows resolveKind"

    // The readers of the library give the same number for the integers in their range. yaml@2.6.1 reads each
    // decimal text here as the JavaScript number nearest to it.
    testCase "integerToFloat agrees with tryParseInt64 and tryParseFloat in their range" <| fun _ ->
        let decimals =
            [ "0"; "7"; "-7"; "+7"; "123456789"; "-123456789012345"; "9007199254740991"; "9007199254740993"
              "9223372036854775807"; "-9223372036854775808"; "-9223372036854775807"; "1000000000000000000" ]
        for text in decimals do
            match CoreSchema.tryParseFloat None (plain text) with
            | Some parsed -> expectFloat text parsed
            | None -> failwith (text + " is a float")
        let ints = decimals @ [ "0x7FFFFFFFFFFFFFFF"; "0x1F"; "0o17"; "0o777777777777777777777"; "0xFFFFFFFF"; "0x123456789ABCDEF" ]
        for text in ints do
            match CoreSchema.tryParseInt64 None (plain text) with
            | Some parsed -> expectFloat text (float parsed)
            | None -> failwith (text + " is an int64")
]

let Main = testList "CoreSchema" [
    tests_string_content
    tests_null
    tests_bool
    tests_int
    tests_float
    tests_kind
    tests_integer_to_float
]
