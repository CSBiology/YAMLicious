module Tests.StringCleanUp

open Fable.Pyxpecto
open System.Collections.Generic
open YAMLicious
open YAMLicious.YAMLiciousTypes

module private Examples =

    [<Literal>]
    let StringReplace = """
My Key: "[{Special character place # |}" # A # in string is allowed!
"""

    [<Literal>]
    let StringsReplace = """
My Key: "[{Special character place # |}" ### A # in string is allowed!
My Key2: "3" ### A # in string is allowed!
My Key2: "Lorem ipsum dolor et" ### A # in string is allowed!
My Key2: "Ehhhhh makarena" ### A # in string is allowed!
"""

let Main = testList "StringCleanUp" [
    testCase "cleanup returns buffered text" <| fun () ->
        let stringMap = new Dictionary<int, StringMapEntry>()
        let actual = Persil.stringCleanUp stringMap "\"value\""
        Expect.equal actual "<s f=0/>" "content should be materialized from the internal string buffer"

    testCase "single special char" <| fun () ->
        let stringMap = new Dictionary<int, StringMapEntry>()
        let actual = Persil.stringCleanUp stringMap Examples.StringReplace
        let expected = """
My Key: <s f=0/> # A # in string is allowed!
"""
        let expectedDict =
            Dictionary(Map[|
                0, { Value = "[{Special character place # |}"; Kind = QuotedStringKind.DoubleQuotedString }
            |])
        let normalizeNewlines (s: string) = s.Replace("\r\n", "\n")
        Expect.equal (normalizeNewlines actual) (normalizeNewlines expected) "content"
        Expect.dictEqual stringMap expectedDict "map"

    testCase "multiple comments" <| fun () ->
        let stringMap = new Dictionary<int, StringMapEntry>()
        let actual = Persil.stringCleanUp stringMap Examples.StringsReplace
        let expected = """
My Key: <s f=0/> ### A # in string is allowed!
My Key2: <s f=1/> ### A # in string is allowed!
My Key2: <s f=2/> ### A # in string is allowed!
My Key2: <s f=3/> ### A # in string is allowed!
"""
        let expectedDict =
            Dictionary(Map [|
                0, { Value = "[{Special character place # |}"; Kind = QuotedStringKind.DoubleQuotedString }
                1, { Value = "3"; Kind = QuotedStringKind.DoubleQuotedString }
                2, { Value = "Lorem ipsum dolor et"; Kind = QuotedStringKind.DoubleQuotedString }
                3, { Value = "Ehhhhh makarena"; Kind = QuotedStringKind.DoubleQuotedString }
            |])
        let normalizeNewlines (s: string) = s.Replace("\r\n", "\n")
        Expect.equal (normalizeNewlines actual) (normalizeNewlines expected) "content"
        Expect.dictEqual stringMap expectedDict "map"

    testCase "lone apostrophe does not swallow later double-quoted token" <| fun () ->
        // Regression: a lone apostrophe (a valid character inside a plain scalar) used to make
        // the double-quote pass treat everything up to EOF as an unclosed single-quoted segment,
        // skipping every later "..." token. The double-quoted value on the following line must
        // still be extracted despite the preceding apostrophe.
        let stringMap = new Dictionary<int, StringMapEntry>()
        let actual = Persil.stringCleanUp stringMap "desc: don't stop\nid: \"value\""
        Expect.equal actual "desc: don't stop\nid: <s f=0/>" "trailing double-quoted token must still be replaced"
        Expect.equal stringMap.[0] { Value = "value"; Kind = QuotedStringKind.DoubleQuotedString } "double-quoted entry after a lone apostrophe should be captured"

    testCase "lone double quote does not swallow later single-quoted token" <| fun () ->
        // Symmetric regression for the single-quote pass.
        let stringMap = new Dictionary<int, StringMapEntry>()
        let actual = Persil.singleQuotedStringCleanUp stringMap "desc: 3 \" inch mark\nid: 'value'"
        Expect.equal actual "desc: 3 \" inch mark\nid: <s f=0/>" "trailing single-quoted token must still be replaced"
        Expect.equal stringMap.[0] { Value = "value"; Kind = QuotedStringKind.SingleQuotedString } "single-quoted entry after a lone double quote should be captured"

    testCase "lone apostrophe keeps later double-quoted keys parseable" <| fun () ->
        // End-to-end (both passes): a plain scalar containing an apostrophe must not stop a later
        // double-quoted key on a following line from being parsed.
        let hasIdEntry (desc: string) =
            let input = sprintf "description: %s\ndataset:\n  \"@id\": ./ref" desc
            (Persil.pipeline input).StringMap.Values
            |> Seq.exists (fun e -> e.Value = "@id" && e.Kind = QuotedStringKind.DoubleQuotedString)
        // both a lone apostrophe and a closed 'quoted' word inside the plain scalar must work
        Expect.isTrue (hasIdEntry "don't stop now") "double-quoted key after a lone apostrophe must still be parsed"
        Expect.isTrue (hasIdEntry "a 'quoted' word here") "double-quoted key after a closed 'quoted' word must still be parsed"

    testCase "mixed single and double quoted placeholders use unique indices" <| fun () ->
        let stringMap = new Dictionary<int, StringMapEntry>()
        let input = """mixed: ['a', "b"]"""
        let withSingles = Persil.singleQuotedStringCleanUp stringMap input
        let actual = Persil.stringCleanUp stringMap withSingles
        let expected = """mixed: [<s f=0/>, <s f=1/>]"""
        Expect.equal actual expected "content"
        Expect.equal stringMap.[0] { Value = "a"; Kind = QuotedStringKind.SingleQuotedString } "single quoted entry should be typed"
        Expect.equal stringMap.[1] { Value = "b"; Kind = QuotedStringKind.DoubleQuotedString } "double quoted entry should keep typed kind"
]
