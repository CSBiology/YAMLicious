module Tests.Encode

// The value encoders. Each encoder is checked against the node that the constructors build, the lookalikes of the core schema are checked for quoting,
// and every type round trips through its decoder over 1000 generated values. The generator is a seeded Park-Miller loop on floats, which gives the
// same sequence on .NET, JavaScript and Python (FsCheck is not available on Fable).
//
// The old tests of Tests.EncoderDecoder asserted the text that Writer.write made from the element. The text belongs to the writer, so these tests assert
// the node and the value that the decoder reads back. The writer tests assert the text.
//
// Old tests that changed:
// - Encode.string, int, int64, float and bool had no style in the old element. The node carries its style now, so the tests compare nodes.
// - Encode.tryInclude and Encode.choose are gone. Encode.optionalField returns None for a missing value and the caller combines the fields with List.choose.
// - Encode.map and Encode.dict took a key encoder to string. They take an encoder to a node now, so a key can be any node.
// - Encode.values writes a block sequence of strings. The old element was a flow sequence only for the writer to pick.
// - Encode.write is gone with the writer before 2.0.0. The writer has its own tests in Tests.Writer.

open Fable.Pyxpecto
open YAMLicious

let private scalarText (node: YamlNode) : string =
    match node.Kind with
    | YamlNodeKind.Scalar scalar -> scalar.Value
    | _ -> failwith "The node is no scalar"

let private flowSeq (nodes: YamlNode list) : YamlNode = YamlNode.flowSequence (List.map YamlNode.item nodes)

/// A document around the node, to check the tree against the rules of the model.
let private expectValid (name: string) (node: YamlNode) =
    let problems = Validate.document (YamlDocument.create node)
    if not problems.IsEmpty then
        failwith (name + " breaks the slot rules: " + String.concat "; " problems)

// -------------------------------------------------------------------------------------------------------------------
// Scalars

let private stringTests = testList "Encode.string" [
    // yaml@2.6.1 reads each of these texts as null, a bool, an int or a float when it is plain, and as the string itself when it is double-quoted.
    // A plain .inf and .nan are floats, and 007 is the int 7.
    testCase "double-quotes the texts that the core schema reads as null, bool, int or float" <| fun _ ->
        let lookalikes =
            [ "null"; "Null"; "NULL"; "~"; ""
              "true"; "True"; "TRUE"; "false"; "False"; "FALSE"
              "1"; "-1"; "+1"; "007"; "0o17"; "0x1F"
              "1.5"; "-1.5"; ".5"; "5."; "1e3"; "1.5E-3"; "-.5e+2"
              ".inf"; "-.inf"; "+.inf"; ".Inf"; ".INF"; ".nan"; ".NaN"; ".NAN" ]
        for text in lookalikes do
            Expect.nodeEqual (Encode.string text) (dq text) (sprintf "'%s' has the double-quoted style" text)
            Expect.equal (Decode.option Decode.string (Encode.string text)) (Some text) (sprintf "'%s' reads back as the string" text)
            Expect.equal (Decode.string (Encode.string text)) text (sprintf "'%s' reads back as the text" text)
            Expect.isFalse (Decode.tryDecode Decode.int (Encode.string text) |> Result.isOk) (sprintf "'%s' is no int" text)
            Expect.isFalse (Decode.tryDecode Decode.float (Encode.string text) |> Result.isOk) (sprintf "'%s' is no float" text)
            Expect.isFalse (Decode.tryDecode Decode.bool (Encode.string text) |> Result.isOk) (sprintf "'%s' is no bool" text)

    // yaml@2.6.1 reads each of these plain texts as the string itself. YAML 1.2 does not resolve yes, no, on and off, so they stay plain although
    // YAML 1.1 reads them as bools.
    testCase "leaves the other strings plain" <| fun _ ->
        for text in [ "abc"; "1a"; "true1"; "nulls"; "1.0.0"; "0x"; "yes"; "no"; "on"; "off"; "3.1.4-draft"; "a b"; "x: y"; " lead" ] do
            Expect.nodeEqual (Encode.string text) (sc text) (sprintf "'%s' has the plain style" text)
            Expect.equal (Decode.option Decode.string (Encode.string text)) (Some text) (sprintf "'%s' reads back as the string" text)

    // yaml@2.6.1 with intAsBigInt reads the plain text 9223372036854775808 as an int, which no int64 holds. The core schema still reads the text as a non-string.
    testCase "double-quotes a text beyond 64 bits" <| fun _ ->
        Expect.isTrue (CoreSchema.resolvesToNonString "9223372036854775808") "9223372036854775808 resolves"
        Expect.nodeEqual (Encode.string "9223372036854775808") (dq "9223372036854775808") "The string is quoted"
        Expect.sameFloat (Decode.float (sc "9223372036854775808")) 9223372036854775808.0 "The plain text is a float"
        Expect.sameFloat (Decode.object (fun get -> get.Required.Field "k" Decode.float) (parseRoot "k: 9223372036854775808\n")) 9223372036854775808.0 "The old input is a float behind a key"

    // yaml@2.6.1 reads `k: "1"` and `k: "~"` as the strings 1 and ~. It reads the plain 1 and ~ as a number and null.
    testCase "Encode.char quotes a char that the core schema reads as an int or null" <| fun _ ->
        Expect.nodeEqual (Encode.char '1') (dq "1") "'1' is quoted"
        Expect.nodeEqual (Encode.char '~') (dq "~") "'~' is quoted"
        Expect.nodeEqual (Encode.char 'a') (sc "a") "'a' stays plain"
        Expect.equal (Decode.char (Encode.char '1')) '1' "'1' reads back"
        Expect.equal (Decode.char (Encode.char '~')) '~' "'~' reads back"
        Expect.equal (Decode.char (Encode.char 'a')) 'a' "'a' reads back"
]

let private numberTests = testList "Encode.int, int64, bool and nullValue" [
    // yaml@2.6.1 reads `k: 5`, `k: -5` and `k: 1.5` as the numbers.
    testCase "Encode.int and Encode.int64 are plain" <| fun _ ->
        Expect.nodeEqual (Encode.int 5) (sc "5") "int"
        Expect.nodeEqual (Encode.int -5) (sc "-5") "negative int"
        Expect.nodeEqual (Encode.int 0) (sc "0") "zero"
        Expect.nodeEqual (Encode.int64 3000000000L) (sc "3000000000") "The int64 is plain"
        Expect.equal (Decode.int (Encode.int System.Int32.MinValue)) System.Int32.MinValue "Int32.MinValue"

    // yaml@2.6.1 with { intAsBigInt: true } reads `k: -9223372036854775808` as the bigint -9223372036854775808n and
    // `k: 9223372036854775807` as 9223372036854775807n. Without intAsBigInt it reads both as a number that has lost
    // the low digits, so only the bigint reading is exact.
    testCase "Encode.int64 round trips Int64.MinValue and Int64.MaxValue" <| fun _ ->
        Expect.nodeEqual (Encode.int64 System.Int64.MinValue) (sc "-9223372036854775808") "Int64.MinValue"
        Expect.nodeEqual (Encode.int64 System.Int64.MaxValue) (sc "9223372036854775807") "Int64.MaxValue"
        for value in [ System.Int64.MinValue; System.Int64.MaxValue; 0L; -1L; 3000000000L ] do
            Expect.equal (Decode.int64 (Encode.int64 value)) value (sprintf "%d reads back" value)

    // yaml@2.6.1 reads `k: true` and `k: false` as the bools. The .NET ToString gives True and False.
    testCase "Encode.bool spells true and false" <| fun _ ->
        Expect.nodeEqual (Encode.bool true) (sc "true") "true"
        Expect.nodeEqual (Encode.bool false) (sc "false") "false"
        Expect.equal (Decode.bool (Encode.bool true)) true "true reads back"
        Expect.equal (Decode.bool (Encode.bool false)) false "false reads back"

    testCase "Encode.nullValue is a plain null" <| fun _ ->
        Expect.nodeEqual Encode.nullValue (sc "null") "The node"
        Expect.equal (Decode.option Decode.string Encode.nullValue) None "It reads as None"
]

let private floatTests = testList "Encode.float" [
    // yaml@2.6.1 reads `k: .nan` as NaN. It reads `k: .inf` as Infinity and `k: -.inf` as -Infinity. It reads
    // the texts NaN and Infinity, which ToString gives, as strings.
    testCase "writes nan and the infinities in the core schema spelling" <| fun _ ->
        Expect.nodeEqual (Encode.float nan) (sc ".nan") "nan"
        Expect.nodeEqual (Encode.float infinity) (sc ".inf") "infinity"
        Expect.nodeEqual (Encode.float (-infinity)) (sc "-.inf") "negative infinity"
        Expect.nodeEqual (Encode.float -0.0) (sc "-0.0") "negative zero"

    // The core schema of yaml@2.6.1 resolves 1.0, 100.0 and -2.0 as floats and 1 and 1000000000000000000000 as ints.
    testCase "writes a whole number with a point" <| fun _ ->
        Expect.nodeEqual (Encode.float 1.0) (sc "1.0") "1.0"
        Expect.nodeEqual (Encode.float 100.0) (sc "100.0") "100.0"
        Expect.nodeEqual (Encode.float -2.0) (sc "-2.0") "-2.0"
        Expect.nodeEqual (Encode.float 0.0) (sc "0.0") "0.0"
        Expect.nodeEqual (Encode.float 1.5) (sc "1.5") "1.5"

    // The core schema of yaml@2.6.1 resolves a text as an int when it has only digits and a sign. A float needs a
    // point or an exponent. The texts stay short on every target, also for 5e-324, which Python wrote with 300 digits.
    testCase "writes no int and no long text" <| fun _ ->
        for value in [ 1.0; 1e15; 1e16; 1e21; 1e22; 1e100; 1e-7; -1e-100; 5e-324; 1.7976931348623157e308; 123456789012345680000.0 ] do
            let text = scalarText (Encode.float value)
            Expect.isTrue (text.Contains(".") || text.Contains("e") || text.Contains("E")) (sprintf "%s is a float of the core schema" text)
            Expect.isTrue (text.Length <= 25) (sprintf "%s is short" text)
            Expect.isFalse (Decode.tryDecode Decode.int64 (sc text) |> Result.isOk) (sprintf "%s is no int" text)

    // yaml@2.6.1 reads the text that each target writes as the same number. The exponent form differs by
    // target: 1E+21 on .NET and 1e+21 on JavaScript and Python. It reads both as 1e+21. It reads `-0.0` as -0 and `.nan` as NaN.
    testCase "round trips the values that the old tests used" <| fun _ ->
        let values =
            [ nan; infinity; -infinity; 0.1; 1e21; -0.0; 0.0; 123.0; -1.5; 1e-7; 1e16
              0.30000000000000004; 5e-324; 1.7976931348623157e308; 1.0; 1e15; 1e100; -1e-100 ]
        for value in values do
            let node = Encode.float value
            Expect.sameFloat (Decode.float node) value (sprintf "%s reads back" (scalarText node))
]

let private dateTests = testList "Encode.datetime and Encode.datetimeOffset" [
    // The round trip format of ISO 8601 starts with the date, a T and the time. The fraction and the zone depend on the target, so the test checks the
    // front of the text and the value that the decoder reads back.
    testCase "write the ISO 8601 round trip format" <| fun _ ->
        let moment = System.DateTime(2018, 10, 1, 11, 12, 55, System.DateTimeKind.Utc)
        Expect.isTrue ((scalarText (Encode.datetime moment)).StartsWith("2018-10-01T11:12:55")) (scalarText (Encode.datetime moment))
        Expect.nodeEqual (Encode.datetime moment) (sc (scalarText (Encode.datetime moment))) "The scalar is plain"
        let backMoment = Decode.datetime (Encode.datetime moment)
        // On Fable the kind of a time with a Z depends on the time zone of the host, so the check compares the instant there.
        let backUtc = backMoment.ToUniversalTime()
        Expect.equal backUtc.Ticks moment.Ticks "The UTC time reads back"
#if !FABLE_COMPILER
        Expect.equal backMoment.Kind System.DateTimeKind.Utc "RoundtripKind keeps the UTC kind"
#endif
        let utc = System.DateTimeOffset(2008, 1, 5, 6, 0, 0, System.TimeSpan.Zero)
        Expect.isTrue ((scalarText (Encode.datetimeOffset utc)).StartsWith("2008-01-05T06:00:00")) (scalarText (Encode.datetimeOffset utc))
        Expect.isTrue (Decode.datetimeOffset (Encode.datetimeOffset utc) = utc) "The UTC instant reads back"
        let offset = System.DateTimeOffset(2008, 1, 5, 6, 0, 0, System.TimeSpan(1, 0, 0))
        Expect.isTrue ((scalarText (Encode.datetimeOffset offset)).StartsWith("2008-01-05T06:00:00")) (scalarText (Encode.datetimeOffset offset))
        Expect.isTrue ((scalarText (Encode.datetimeOffset offset)).EndsWith("+01:00")) (scalarText (Encode.datetimeOffset offset))
        let back = Decode.datetimeOffset (Encode.datetimeOffset offset)
        Expect.isTrue (back = offset) "The instant reads back"
        // JavaScript reads the offset of a time as the offset of the machine.
#if !FABLE_COMPILER_JAVASCRIPT
        Expect.equal back.Offset offset.Offset "The offset reads back"
#endif
]

// -------------------------------------------------------------------------------------------------------------------
// Collections

let private collectionTests = testList "Encode collections" [
    testCase "Encode.option writes the value or a plain null" <| fun _ ->
        Expect.nodeEqual (Encode.option Encode.int (Some 5)) (sc "5") "Some"
        Expect.nodeEqual (Encode.option Encode.int None) (sc "null") "None"
        Expect.nodeEqual (Encode.option Encode.string (Some "null")) (dq "null") "A string null keeps its quotes"
        // yaml@2.6.1 reads `k: "null"` and `k: "~"` as the strings null and ~, and `k: ""` as the empty string.
        for text in [ "null"; "~"; "" ] do
            Expect.equal (Decode.option Decode.string (Encode.option Encode.string (Some text))) (Some text) (sprintf "Some '%s' reads back" text)
        Expect.equal (Decode.option Decode.string (Encode.option Encode.string None)) None "None reads back as None"

    testCase "list, array, seq and resizearray write a block sequence" <| fun _ ->
        let expected = seqOf [ sc "1"; sc "2"; sc "3" ]
        Expect.nodeEqual (Encode.list Encode.int [ 1; 2; 3 ]) expected "list"
        Expect.nodeEqual (Encode.array Encode.int [| 1; 2; 3 |]) expected "array"
        Expect.nodeEqual (Encode.seq Encode.int (seq { 1; 2; 3 })) expected "seq"
        Expect.nodeEqual (Encode.resizearray Encode.int (ResizeArray([ 1; 2; 3 ]))) expected "resizearray"
        Expect.nodeEqual (Encode.list Encode.string [ "a"; "true" ]) (seqOf [ sc "a"; dq "true" ]) "The items go through their encoder"

    testCase "an empty collection is the flow collection" <| fun _ ->
        let emptySequence = YamlNode.sequence []
        Expect.nodeEqual (Encode.list Encode.int []) emptySequence "list"
        Expect.nodeEqual (Encode.array Encode.int [||]) emptySequence "array"
        Expect.nodeEqual (Encode.seq Encode.int Seq.empty) emptySequence "seq"
        Expect.nodeEqual (Encode.resizearray Encode.int (ResizeArray())) emptySequence "resizearray"
        Expect.nodeEqual (Encode.object []) (YamlNode.mapping []) "object"
        Expect.nodeEqual (Encode.map Encode.string Encode.int Map.empty) (YamlNode.mapping []) "map"
        Expect.nodeEqual (Encode.values id []) emptySequence "values"
        expectValid "the empty sequence" emptySequence
        expectValid "the empty mapping" (YamlNode.mapping [])

    // yaml@2.6.1 reads `k: [a, b c, "true"]` as {"k":["a","b c","true"]} and the root sequence `- a`, `- b` as ["a","b"].
    // An Object of sibling scalars would be written as lines that read back as one multiline scalar.
    testCase "Encode.values writes a sequence that Decode.values reads back" <| fun _ ->
        let node = Encode.values id [ "a"; "b c"; "true" ]
        Expect.nodeEqual node (seqOf [ sc "a"; sc "b c"; dq "true" ]) "The items stay separate and a bool lookalike is quoted"
        Expect.equal (Decode.values Decode.string node) [ "a"; "b c"; "true" ] "The items read back"
        Expect.nodeEqual (Encode.values string [ 1; 2 ]) (seqOf [ dq "1"; dq "2" ]) "The text of an int is a string"

    testCase "the tuples write a block sequence of their items" <| fun _ ->
        Expect.nodeEqual (Encode.tuple2 Encode.int Encode.string (1, "x")) (seqOf [ sc "1"; sc "x" ]) "tuple2"
        Expect.nodeEqual (Encode.tuple3 Encode.int Encode.string Encode.bool (1, "x", true)) (seqOf [ sc "1"; sc "x"; sc "true" ]) "tuple3"
        let nodes = [ sc "1"; sc "a"; sc "true"; sc "2"; sc "b"; sc "false"; sc "3"; sc "c" ]
        let upTo (count: int) = seqOf (List.truncate count nodes)
        Expect.nodeEqual (Encode.tuple4 Encode.int Encode.string Encode.bool Encode.int (1, "a", true, 2)) (upTo 4) "tuple4"
        Expect.nodeEqual (Encode.tuple5 Encode.int Encode.string Encode.bool Encode.int Encode.string (1, "a", true, 2, "b")) (upTo 5) "tuple5"
        Expect.nodeEqual
            (Encode.tuple6 Encode.int Encode.string Encode.bool Encode.int Encode.string Encode.bool (1, "a", true, 2, "b", false))
            (upTo 6)
            "tuple6"
        Expect.nodeEqual
            (Encode.tuple7 Encode.int Encode.string Encode.bool Encode.int Encode.string Encode.bool Encode.int (1, "a", true, 2, "b", false, 3))
            (upTo 7)
            "tuple7"
        Expect.nodeEqual
            (Encode.tuple8 Encode.int Encode.string Encode.bool Encode.int Encode.string Encode.bool Encode.int Encode.char (1, "a", true, 2, "b", false, 3, 'c'))
            (upTo 8)
            "tuple8"

    // yaml@2.6.1 with mapAsMap reads the quoted keys "true", "1", "null" and "1.5" as strings. It reads the plain
    // keys true and 1 as the bool true and the number 1.
    testCase "map, dict and object keep keys such as true and 1 as strings" <| fun _ ->
        let map = Encode.map Encode.string Encode.int (Map [ "true", 1; "1", 2 ])
        // A map lists its entries in the order of its keys.
        Expect.nodeEqual map (YamlNode.mapping [ YamlNode.entry (dq "1") (sc "2"); YamlNode.entry (dq "true") (sc "1") ]) "map keys are quoted"
        Expect.equal (Decode.map Decode.string Decode.int map) (Map [ "true", 1; "1", 2 ]) "map keys read back"
        let dictionary = System.Collections.Generic.Dictionary<string, int>()
        dictionary.Add("true", 1)
        dictionary.Add("1", 2)
        // A dictionary lists its entries in the order of insertion.
        Expect.nodeEqual
            (Encode.dict Encode.string Encode.int dictionary)
            (YamlNode.mapping [ YamlNode.entry (dq "true") (sc "1"); YamlNode.entry (dq "1") (sc "2") ])
            "dict keys are quoted"
        let node = Encode.object [ "null", Encode.int 1; "1.5", Encode.int 2; "name", Encode.int 3 ]
        Expect.nodeEqual
            node
            (YamlNode.mapping [ YamlNode.entry (dq "null") (sc "1"); YamlNode.entry (dq "1.5") (sc "2"); YamlNode.entry (sc "name") (sc "3") ])
            "object keys are quoted when they need it"

    testCase "the key encoder builds any node" <| fun _ ->
        Expect.nodeEqual (Encode.map Encode.int Encode.string (Map [ 1, "a" ])) (YamlNode.mapping [ YamlNode.entry (sc "1") (sc "a") ]) "An int key"
        let tupleKey = Encode.map (Encode.tuple2 Encode.int Encode.int) Encode.string (Map [ (1, 2), "a" ])
        Expect.nodeEqual tupleKey (YamlNode.mapping [ YamlNode.entry (seqOf [ sc "1"; sc "2" ]) (sc "a") ]) "A sequence key"
        expectValid "the sequence key" tupleKey
        Expect.equal (Decode.map (Decode.tuple2 Decode.int Decode.int) Decode.string tupleKey) (Map [ (1, 2), "a" ]) "A sequence key reads back"

    // yaml@2.6.1 reads the encoded validation package as {"name":"package1","version":"1.0.0"}.
    testCase "optionalField leaves out a missing value" <| fun _ ->
        let missing = Encode.optionalField "version" Encode.string None
        Expect.isTrue missing.IsNone "None"
        let present = Encode.optionalField "version" Encode.string (Some "1.0.0")
        Expect.equal (present |> Option.map fst) (Some "version") "The name"
        Expect.nodeEqual (present |> Option.get |> snd) (sc "1.0.0") "The node"
        let fields = [ Some ("name", Encode.string "package3"); Encode.optionalField "version" Encode.string None ] |> List.choose id
        Expect.nodeEqual (Encode.object fields) (YamlNode.mapping [ YamlNode.entry (sc "name") (sc "package3") ]) "A missing version is no entry"
        // A null value is a field with the value null. The caller decides which one it wants.
        Expect.nodeEqual
            (Encode.object [ "version", Encode.option Encode.string None ])
            (YamlNode.mapping [ YamlNode.entry (sc "version") (sc "null") ])
            "The null stays an entry"
]

// -------------------------------------------------------------------------------------------------------------------
// The validation package example

type private Package = { Name: string; Version: string option }

type private DocValidation = { FormatDefinitionVersion: string; Packages: Package list }

let private packageEncoder (package: Package) : YamlNode =
    [ Some ("name", Encode.string package.Name); Encode.optionalField "version" Encode.string package.Version ]
    |> List.choose id
    |> Encode.object

let private validationEncoder (doc: DocValidation) : YamlNode =
    Encode.object [ "format_definition", Encode.string doc.FormatDefinitionVersion; "inspection_packages", Encode.list packageEncoder doc.Packages ]

let private validation =
    { FormatDefinitionVersion = "3.1.4-draft"
      Packages =
        [ { Name = "package1"; Version = Some "1.0.0" }
          { Name = "package2"; Version = Some "2.0.0" }
          { Name = "package3"; Version = None } ] }

let private validationText =
    "format_definition: 3.1.4-draft\ninspection_packages:\n  - name: package1\n    version: 1.0.0\n  - name: package2\n    version: 2.0.0\n  - name: package3"

/// The data projection of a node, which compares what a parse and an encoder say without the presentation.
let private dataOf (root: YamlNode) : string =
    Projection.data { Parts = [ StreamPart.Document(YamlDocument.create root) ] }

let private validationTests = testList "The validation package example" [
    // yaml@2.6.1 reads the text as the two fields, the second a list of three packages of which the last has no version. The old test compared the text that
    // the old writer wrote. The data projection of the encoded node must equal the projection of the parsed text.
    testCase "the encoded value has the data of the parsed text" <| fun _ ->
        let encoded = validationEncoder validation
        expectValid "the encoded validation" encoded
        Expect.equal (dataOf encoded) (dataOf (parseRoot validationText)) "The data projections are equal"

    // The old test "encode overflow" appended the extension data of a decoded value to the typed fields. The entries that the getter returns keep their nodes, so the
    // fields of the extension can go back into the object.
    testCase "the extension entries of a decoded value go back into the object" <| fun _ ->
        let text = validationText + "\nauthor: TestAuthor\nauthor_emails:\n  - test1@email.com\n  - test2@email.com"
        let extension = Decode.object (fun get -> get.Overflow.FieldList [ "format_definition"; "inspection_packages" ]) (parseRoot text)
        let typedFields =
            [ "format_definition", Encode.string validation.FormatDefinitionVersion
              "inspection_packages", Encode.list packageEncoder validation.Packages ]
        let extensionFields = [ for entry in extension -> Decode.string entry.Key, entry.Value ]
        let encoded = Encode.object (typedFields @ extensionFields)
        expectValid "the encoded value with its extension" encoded
        Expect.equal (dataOf encoded) (dataOf (parseRoot text)) "The data projections are equal"
]

/// The text of a mapping with the node as the value of the key k.
let private writeValue (node: YamlNode) : string =
    Writer.writeNode WriterOptions.defaults (YamlNode.mapping [ YamlNode.entry (sc "k") node ])

/// The text of the node as a document.
let private writeDocument (node: YamlNode) : string = Writer.writeNode WriterOptions.defaults node

/// The value of the key k in the text.
let private readValue (decoder: Decoder<'T>) (text: string) : 'T =
    Decode.object (fun get -> get.Required.Field "k" decoder) (parseRoot text)

let private writtenTests = testList "Encoders through the writer" [
    // The old tests of Tests.EncoderDecoder compared the text that Writer.write made from the element. The tests above compare the node, and these tests keep the text. Each text
    // goes through the parser and the decoder again. yaml@2.6.1 reads each of these texts as null, a bool, an int or a float when it is plain, and as the string itself when it is
    // double-quoted.
    testCase "Encode.string is written in double quotes when the core schema reads it as null, bool, int or float, and plain otherwise" <| fun _ ->
        let lookalikes =
            [ "null"; "Null"; "NULL"; "~"; ""
              "true"; "True"; "TRUE"; "false"; "False"; "FALSE"
              "1"; "-1"; "+1"; "007"; "0o17"; "0x1F"
              "1.5"; "-1.5"; ".5"; "5."; "1e3"; "1.5E-3"; "-.5e+2"
              ".inf"; "-.inf"; "+.inf"; ".Inf"; ".INF"; ".nan"; ".NaN"; ".NAN" ]
        for text in lookalikes do
            let written = writeValue (Encode.string text)
            Expect.equal written (sprintf "k: \"%s\"\n" text) (sprintf "'%s' is written in quotes" text)
            Expect.equal (readValue Decode.string written) text (sprintf "'%s' reads back as the string" text)
        // yaml@2.6.1 reads each of these plain texts as the string itself. YAML 1.2 does not resolve yes, no, on and off, so they stay plain.
        for text in [ "abc"; "1a"; "true1"; "nulls"; "1.0.0"; "0x"; "yes"; "no"; "on"; "off" ] do
            let written = writeValue (Encode.string text)
            Expect.equal written (sprintf "k: %s\n" text) (sprintf "'%s' is written plain" text)
            Expect.equal (readValue Decode.string written) text (sprintf "'%s' reads back as the string" text)

    // yaml@2.6.1 reads `k: true`, `k: false`, `k: 5`, `k: -5` and `k: 1.5` as the bool and the numbers, and `k: -9223372036854775808` and `k: 9223372036854775807` as the bigints
    // when it runs with intAsBigInt.
    testCase "Encode.bool, int, int64 and float are written plain and read back" <| fun _ ->
        Expect.equal (writeValue (Encode.bool true)) "k: true\n" "true"
        Expect.equal (writeValue (Encode.bool false)) "k: false\n" "false"
        Expect.equal (readValue Decode.bool "k: true\n") true "Decode.bool reads true"
        Expect.equal (readValue Decode.bool "k: false\n") false "Decode.bool reads false"
        Expect.equal (writeValue (Encode.int 5)) "k: 5\n" "int"
        Expect.equal (writeValue (Encode.int -5)) "k: -5\n" "negative int"
        Expect.equal (writeValue (Encode.float 1.5)) "k: 1.5\n" "float"
        Expect.equal (writeValue (Encode.int64 System.Int64.MinValue)) "k: -9223372036854775808\n" "Int64.MinValue"
        Expect.equal (writeValue (Encode.int64 System.Int64.MaxValue)) "k: 9223372036854775807\n" "Int64.MaxValue"
        for value in [ System.Int64.MinValue; System.Int64.MaxValue; 0L; -1L; 3000000000L ] do
            Expect.equal (readValue Decode.int64 (writeValue (Encode.int64 value))) value (sprintf "%d reads back" value)

    // yaml@2.6.1 reads `k: .nan`, `k: .inf` and `k: -.inf` as NaN, Infinity and -Infinity. The core schema of yaml@2.6.1 resolves 1.0, 100.0 and -2.0 as floats.
    testCase "Encode.float is written in the core schema spelling and reads back" <| fun _ ->
        Expect.equal (writeValue (Encode.float nan)) "k: .nan\n" "nan"
        Expect.equal (writeValue (Encode.float infinity)) "k: .inf\n" "infinity"
        Expect.equal (writeValue (Encode.float (-infinity))) "k: -.inf\n" "negative infinity"
        Expect.equal (writeValue (Encode.float 1.0)) "k: 1.0\n" "1.0"
        Expect.equal (writeValue (Encode.float 100.0)) "k: 100.0\n" "100.0"
        Expect.equal (writeValue (Encode.float -2.0)) "k: -2.0\n" "-2.0"
        Expect.equal (writeValue (Encode.float 0.0)) "k: 0.0\n" "0.0"
        let values =
            [ nan; infinity; -infinity; 0.1; 1e21; -0.0; 0.0; 123.0; -1.5; 1e-7; 1e16
              0.30000000000000004; 5e-324; 1.7976931348623157e308; 1.0; 1e15; 1e100; -1e-100 ]
        for value in values do
            let written = writeValue (Encode.float value)
            Expect.sameFloat (readValue Decode.float written) value (sprintf "%s reads back" written)

    // yaml@2.6.1 reads `k: "null"`, `k: "~"` and `k: "1"` as the strings null, ~ and 1, and `k: ""` as the empty string.
    testCase "Encode.option and Encode.char are written so that they read back" <| fun _ ->
        for text in [ "null"; "~"; "" ] do
            let written = writeValue (Encode.option Encode.string (Some text))
            Expect.equal (readValue (Decode.option Decode.string) written) (Some text) (sprintf "Some '%s' reads back" text)
        Expect.equal (readValue (Decode.option Decode.string) (writeValue (Encode.option Encode.string None))) None "None reads back as None"
        Expect.equal (writeValue (Encode.char '1')) "k: \"1\"\n" "'1' is quoted"
        Expect.equal (writeValue (Encode.char '~')) "k: \"~\"\n" "'~' is quoted"
        Expect.equal (writeValue (Encode.char 'a')) "k: a\n" "'a' stays plain"
        Expect.equal (readValue Decode.char "k: \"1\"\n") '1' "'1' reads back"
        Expect.equal (readValue Decode.char "k: \"~\"\n") '~' "'~' reads back"

    // yaml@2.6.1 with mapAsMap reads the quoted keys "true", "1", "null" and "1.5" as strings. It reads the plain keys true and 1 as the bool true and the number 1. A map lists its entries
    // in the order of its keys and a dictionary in the order of insertion.
    testCase "Encode.map, Encode.dict and Encode.object write the keys such as true and 1 in double quotes" <| fun _ ->
        let map = Encode.map Encode.string Encode.int (Map [ "true", 1; "1", 2 ])
        Expect.equal (writeDocument map) "\"1\": 2\n\"true\": 1\n" "map keys are quoted"
        Expect.equal (writeDocument map |> parseRoot |> Decode.map Decode.string Decode.int) (Map [ "true", 1; "1", 2 ]) "map keys read back"
        let dictionary = System.Collections.Generic.Dictionary<string, int>()
        dictionary.Add("true", 1)
        dictionary.Add("1", 2)
        Expect.equal (writeDocument (Encode.dict Encode.string Encode.int dictionary)) "\"true\": 1\n\"1\": 2\n" "dict keys are quoted"
        let node = Encode.object [ "null", Encode.int 1; "1.5", Encode.int 2; "name", Encode.int 3 ]
        Expect.equal (writeDocument node) "\"null\": 1\n\"1.5\": 2\nname: 3\n" "object keys are quoted when they need it"

    // yaml@2.6.1 reads `k:` followed by `- a`, `- b c` and `- "true"` as {"k":["a","b c","true"]} and the root sequence `- a`, `- b` as ["a","b"]. The old writer wrote the
    // items of Encode.values in a flow sequence. The encoder builds a block sequence now.
    testCase "Encode.values is written as a block sequence that Decode.values reads back" <| fun _ ->
        let written = writeValue (Encode.values id [ "a"; "b c"; "true" ])
        Expect.equal written "k:\n  - a\n  - b c\n  - \"true\"\n" "The items stay separate"
        Expect.equal (readValue (Decode.values Decode.string) written) [ "a"; "b c"; "true" ] "The items read back"
        let root = writeDocument (Encode.values id [ "a"; "b" ])
        Expect.equal root "- a\n- b\n" "The root is a block sequence"
        Expect.equal (root |> parseRoot |> Decode.values Decode.string) [ "a"; "b" ] "The root items read back"

    // yaml@2.6.1 reads the texts as the two fields, the second a list of three packages of which the last has no version. The old texts put the dash of each package on a line of its own.
    // A mapping in an item continues the line of its dash in the new writer, as the tests of the writer show.
    testCase "the validation package is written as the text that the parser reads" <| fun _ ->
        Expect.equal (writeDocument (validationEncoder validation)) (validationText + "\n") "The encoded value"
        let extension = "\nauthor: TestAuthor\nauthor_emails:\n  - test1@email.com\n  - test2@email.com"
        let read = Decode.object (fun get -> get.Overflow.FieldList [ "format_definition"; "inspection_packages" ]) (parseRoot (validationText + extension))
        let typedFields =
            [ "format_definition", Encode.string validation.FormatDefinitionVersion
              "inspection_packages", Encode.list packageEncoder validation.Packages ]
        let extensionFields = [ for entry in read -> Decode.string entry.Key, entry.Value ]
        Expect.equal (writeDocument (Encode.object (typedFields @ extensionFields))) (validationText + extension + "\n") "The encoded value with its extension"
]

// -------------------------------------------------------------------------------------------------------------------
// Generated values

/// A Park-Miller generator on floats. The product of the state and 48271 stays below 2^47, so the float arithmetic is exact on every target.
type private Rng(seed: int) =
    let mutable state = float seed

    member _.Next() : float =
        state <- (state * 48271.0) % 2147483647.0
        state

    member this.Below(bound: int) : int = int (this.Next() % float bound)

let private genInt (r: Rng) : int =
    match r.Below 40 with
    | 0 -> System.Int32.MinValue
    | 1 -> System.Int32.MaxValue
    | 2 -> 0
    | 3 -> -1
    | _ -> int (float (r.Below 65536) * 65536.0 + float (r.Below 65536) - 2147483648.0)

let private genInt64 (r: Rng) : int64 =
    match r.Below 40 with
    | 0 -> System.Int64.MinValue
    | 1 -> System.Int64.MaxValue
    | 2 -> 0L
    | 3 -> -1L
    | _ ->
        let high = int64 (genInt r)
        let low = int64 (r.Below 65536) * 65536L + int64 (r.Below 65536)
        high * 4294967296L + low

let private genFloat (r: Rng) : float =
    match r.Below 40 with
    | 0 -> nan
    | 1 -> infinity
    | 2 -> -infinity
    | 3 -> -0.0
    | 4 -> 0.0
    | 5 -> 1e21
    | 6 -> 5e-324
    | 7 -> System.Double.MaxValue
    | 8 -> 1.0
    | _ -> float (genInt r) * (10.0 ** float (r.Below 41 - 20))

let private tokens =
    [| "a"; "b"; "Z"; "0"; "1"; "7"; "."; "-"; "+"; "e"; "x"; " "; ":"; "#"; "'"; "\""; "["; "]"; "{"; "}"; ","; "&"; "*"; "!"; "|"; ">"; "%"; "@"
       "`"; "\\"; "\n"; "\t"; string (char 0xE9); string (char 0x65E5); "\U0001F600"; "null"; "~"; "true"; "False"; ".inf"; ".nan"; "0x1F"; "0o7"; "yes"; "" |]

let private genString (r: Rng) : string =
    let sb = System.Text.StringBuilder()
    for _ in 1 .. r.Below 6 do
        sb.Append(tokens.[r.Below tokens.Length]) |> ignore
    sb.ToString()

let private genChar (r: Rng) : char =
    match r.Below 10 with
    | 0 -> char 0xE9
    | 1 -> char 0x65E5
    | _ -> char (32 + r.Below 95)

let private genBool (r: Rng) : bool = r.Below 2 = 0

let private genList (gen: Rng -> 'T) (r: Rng) : 'T list = [ for _ in 1 .. r.Below 6 -> gen r ]

let private genOption (gen: Rng -> 'T) (r: Rng) : 'T option = if r.Below 3 = 0 then None else Some (gen r)

/// A time with whole milliseconds, which every target keeps.
let private genDateTime (r: Rng) : System.DateTime =
    System.DateTime(1990, 1, 1, 0, 0, 0, System.DateTimeKind.Utc).AddMilliseconds(float (r.Below 2000000000) * 100.0 + float (r.Below 1000))

/// A time with an offset of whole hours.
let private genDateTimeOffset (r: Rng) : System.DateTimeOffset =
    let local = System.DateTime(1990 + r.Below 100, 1 + r.Below 12, 1 + r.Below 28, r.Below 24, r.Below 60, r.Below 60, r.Below 1000)
    System.DateTimeOffset(local, System.TimeSpan(r.Below 27 - 12, 0, 0))

/// Runs the check on 1000 values from the generator.
let private repeat (seed: int) (check: Rng -> unit) =
    let r = Rng(seed)
    for _ in 1 .. 1000 do
        check r

/// Fails when the decoded value differs from the original.
let private expectBack (name: string) (value: 'T) (decoded: 'T) =
    if decoded <> value then
        failwith (sprintf "%s: %A came back as %A" name value decoded)

let private roundTripTests = testList "decode (encode x) = x over 1000 generated values" [
    testCase "int, int64, bool and char" <| fun _ ->
        repeat 1 (fun r ->
            let value = genInt r
            expectBack "int" value (Decode.int (Encode.int value)))
        repeat 2 (fun r ->
            let value = genInt64 r
            expectBack "int64" value (Decode.int64 (Encode.int64 value)))
        repeat 3 (fun r ->
            let value = genBool r
            expectBack "bool" value (Decode.bool (Encode.bool value)))
        repeat 4 (fun r ->
            let value = genChar r
            expectBack "char" value (Decode.char (Encode.char value)))

    testCase "float" <| fun _ ->
        repeat 5 (fun r ->
            let value = genFloat r
            let node = Encode.float value
            Expect.sameFloat (Decode.float node) value (sprintf "%s reads back" (scalarText node))
            // The float text is never an int, so the int decoders reject it.
            Expect.isFalse (Decode.tryDecode Decode.int64 node |> Result.isOk) (sprintf "%s is no int64" (scalarText node)))

    testCase "string, with the style that the core schema needs" <| fun _ ->
        repeat 6 (fun r ->
            let value = genString r
            let node = Encode.string value
            expectBack "string" value (Decode.string node)
            expectBack "option string" (Some value) (Decode.option Decode.string node)
            let expectedNode = if CoreSchema.resolvesToNonString value then dq value else sc value
            if not (YamlNode.equalIgnoringSpans node expectedNode) then
                failwith (sprintf "The string %A has the wrong style" value)
            // A string is never an int, a float or a bool, whatever its text.
            if Decode.tryDecode Decode.int node |> Result.isOk
               || Decode.tryDecode Decode.float node |> Result.isOk
               || Decode.tryDecode Decode.bool node |> Result.isOk then
                failwith (sprintf "The string %A reads as a number or a bool" value))

    testCase "datetime and datetimeOffset" <| fun _ ->
        repeat 7 (fun r ->
            let value = genDateTime r
            let decoded = Decode.datetime (Encode.datetime value)
            if decoded.ToUniversalTime().Ticks <> value.Ticks then
                failwith (sprintf "datetime: %s came back as %s" (scalarText (Encode.datetime value)) (scalarText (Encode.datetime decoded))))
        repeat 8 (fun r ->
            let value = genDateTimeOffset r
            let decoded = Decode.datetimeOffset (Encode.datetimeOffset value)
            // JavaScript reads the offset of a time as the offset of the machine, so only the instant is compared there.
            let sameOffset =
#if FABLE_COMPILER_JAVASCRIPT
                true
#else
                decoded.Offset = value.Offset
#endif
            if decoded <> value || not sameOffset then
                failwith (sprintf "datetimeOffset: %s came back as %s" (scalarText (Encode.datetimeOffset value)) (scalarText (Encode.datetimeOffset decoded))))

    testCase "option, list, array, seq, resizearray and values" <| fun _ ->
        repeat 9 (fun r ->
            let value = genOption genString r
            expectBack "option" value (Decode.option Decode.string (Encode.option Encode.string value)))
        repeat 10 (fun r ->
            let value = genList genInt r
            expectBack "list" value (Decode.list Decode.int (Encode.list Encode.int value))
            expectBack "array" (List.toArray value) (Decode.array Decode.int (Encode.array Encode.int (List.toArray value)))
            expectBack "seq" value (List.ofSeq (Decode.seq Decode.int (Encode.seq Encode.int value)))
            expectBack "resizearray" value (List.ofSeq (Decode.resizearray Decode.int (Encode.resizearray Encode.int (ResizeArray(value))))))
        repeat 11 (fun r ->
            let value = genList (genOption genString) r
            expectBack "list of options" value (Decode.list (Decode.option Decode.string) (Encode.list (Encode.option Encode.string) value)))
        repeat 12 (fun r ->
            let value = genList (genList genString) r
            expectBack "list of lists" value (Decode.list (Decode.list Decode.string) (Encode.list (Encode.list Encode.string) value)))
        repeat 13 (fun r ->
            let value = genList genString r
            expectBack "values" value (Decode.values Decode.string (Encode.values id value)))

    testCase "tuple2 to tuple8" <| fun _ ->
        repeat 14 (fun r ->
            let value = (genInt r, genString r)
            expectBack "tuple2" value (Decode.tuple2 Decode.int Decode.string (Encode.tuple2 Encode.int Encode.string value)))
        repeat 15 (fun r ->
            let value = (genInt r, genString r, genBool r)
            expectBack "tuple3" value (Decode.tuple3 Decode.int Decode.string Decode.bool (Encode.tuple3 Encode.int Encode.string Encode.bool value)))
        repeat 16 (fun r ->
            let value = (genInt r, genString r, genBool r, genInt64 r)
            expectBack
                "tuple4"
                value
                (Decode.tuple4 Decode.int Decode.string Decode.bool Decode.int64 (Encode.tuple4 Encode.int Encode.string Encode.bool Encode.int64 value)))
        repeat 17 (fun r ->
            let value = (genInt r, genString r, genBool r, genInt64 r, genChar r)
            expectBack
                "tuple5"
                value
                (Decode.tuple5
                    Decode.int Decode.string Decode.bool Decode.int64 Decode.char
                    (Encode.tuple5 Encode.int Encode.string Encode.bool Encode.int64 Encode.char value)))
        repeat 18 (fun r ->
            let value = (genInt r, genString r, genBool r, genInt64 r, genChar r, genOption genString r)
            expectBack
                "tuple6"
                value
                (Decode.tuple6
                    Decode.int Decode.string Decode.bool Decode.int64 Decode.char (Decode.option Decode.string)
                    (Encode.tuple6 Encode.int Encode.string Encode.bool Encode.int64 Encode.char (Encode.option Encode.string) value)))
        repeat 19 (fun r ->
            let value = (genInt r, genString r, genBool r, genInt64 r, genChar r, genOption genString r, genList genInt r)
            expectBack
                "tuple7"
                value
                (Decode.tuple7
                    Decode.int Decode.string Decode.bool Decode.int64 Decode.char (Decode.option Decode.string) (Decode.list Decode.int)
                    (Encode.tuple7
                        Encode.int Encode.string Encode.bool Encode.int64 Encode.char (Encode.option Encode.string) (Encode.list Encode.int)
                        value)))
        repeat 20 (fun r ->
            let value = (genInt r, genString r, genBool r, genInt64 r, genChar r, genOption genString r, genList genInt r, (genInt r, genString r))
            expectBack
                "tuple8"
                value
                (Decode.tuple8
                    Decode.int Decode.string Decode.bool Decode.int64 Decode.char (Decode.option Decode.string) (Decode.list Decode.int)
                    (Decode.tuple2 Decode.int Decode.string)
                    (Encode.tuple8
                        Encode.int Encode.string Encode.bool Encode.int64 Encode.char (Encode.option Encode.string) (Encode.list Encode.int)
                        (Encode.tuple2 Encode.int Encode.string)
                        value)))

    testCase "map and dict" <| fun _ ->
        repeat 21 (fun r ->
            let value = Map.ofList (genList (fun r -> genString r, genInt r) r)
            expectBack "map" value (Decode.map Decode.string Decode.int (Encode.map Encode.string Encode.int value)))
        repeat 22 (fun r ->
            let value = Map.ofList (genList (fun r -> genString r, genList genString r) r)
            let dictionary = System.Collections.Generic.Dictionary<string, string list>()
            for pair in Map.toList value do
                dictionary.Add(fst pair, snd pair)
            let decoded = Decode.dict Decode.string (Decode.list Decode.string) (Encode.dict Encode.string (Encode.list Encode.string) dictionary)
            expectBack "dict" [ for KeyValue (key, items) in dictionary -> key, items ] [ for KeyValue (key, items) in decoded -> key, items ])
        repeat 23 (fun r ->
            let value = Map.ofList (genList (fun r -> genInt r, genBool r) r)
            expectBack "map with int keys" value (Decode.map Decode.int Decode.bool (Encode.map Encode.int Encode.bool value)))

    testCase "an object with optional fields" <| fun _ ->
        repeat 24 (fun r ->
            let name = genString r
            let version = genOption genString r
            let note = genOption genInt r
            let node =
                [ Some ("name", Encode.string name); Encode.optionalField "version" Encode.string version; Encode.optionalField "note" Encode.int note ]
                |> List.choose id
                |> Encode.object
            expectValid "the object" node
            let decoder =
                Decode.object (fun get ->
                    get.Required.Field "name" Decode.string, get.Optional.Field "version" Decode.string, get.Optional.Field "note" Decode.int)
            expectBack "object" (name, version, note) (decoder node))
]

let Main =
    testList "Encode" [
        stringTests
        numberTests
        floatTests
        dateTests
        collectionTests
        validationTests
        writtenTests
        roundTripTests
    ]
