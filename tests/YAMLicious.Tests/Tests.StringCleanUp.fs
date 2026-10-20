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

let private code = Persil.LineKind.Code
let private blank = Persil.LineKind.Blank
let private header = Persil.LineKind.BlockHeader
let private body = Persil.LineKind.BlockBody

let private single (value: string) : StringMapEntry = { Value = value; Kind = QuotedStringKind.SingleQuotedString }
let private dquote (value: string) : StringMapEntry = { Value = value; Kind = QuotedStringKind.DoubleQuotedString }

let private stringMapOf (entries: (int * StringMapEntry) list) =
    Dictionary<int, StringMapEntry>(Map entries)

let private kindsOf (tokenized: Persil.TokenizedYaml) =
    tokenized.Lines |> Array.map (fun line -> line.Kind) |> List.ofArray

let private normalizeNewlines (s: string) = s.Replace("\r\n", "\n")

/// Asserts the emitted text, the line kinds and the string map. No example here has a comment.
let private expectTokens (input: string) (text: string) (kinds: Persil.LineKind list) (entries: (int * StringMapEntry) list) =
    let actual = Persil.tokenize input
    Expect.equal (tokenizedText actual) text "text"
    Expect.equal (kindsOf actual) kinds "line kinds"
    Expect.dictEqual actual.StringMap (stringMapOf entries) "string map"
    Expect.equal actual.CommentMap.Count 0 "no comments"

let Main = testList "StringCleanUp" [
    testCase "tokenize returns buffered text" <| fun () ->
        expectTokens "\"value\"" "<s f=0/>" [code] [0, dquote "value"]

    testCase "single special char" <| fun () ->
        let actual = Persil.tokenize Examples.StringReplace
        let expected = """
My Key: <s f=0/> <c f=0/>
"""
        Expect.equal (normalizeNewlines (tokenizedText actual)) (normalizeNewlines expected) "content"
        Expect.equal (kindsOf actual) [blank; code; blank] "line kinds"
        Expect.dictEqual actual.StringMap (stringMapOf [0, dquote "[{Special character place # |}"]) "string map"
        Expect.dictEqual actual.CommentMap (Dictionary<int, string>(Map [0, " A # in string is allowed!"])) "comment map"

    testCase "multiple comments" <| fun () ->
        let actual = Persil.tokenize Examples.StringsReplace
        let expected = """
My Key: <s f=0/> <c f=0/>
My Key2: <s f=1/> <c f=1/>
My Key2: <s f=2/> <c f=2/>
My Key2: <s f=3/> <c f=3/>
"""
        let expectedStrings =
            stringMapOf [
                0, dquote "[{Special character place # |}"
                1, dquote "3"
                2, dquote "Lorem ipsum dolor et"
                3, dquote "Ehhhhh makarena"
            ]
        let comment = "## A # in string is allowed!"
        let expectedComments = Dictionary<int, string>(Map [0, comment; 1, comment; 2, comment; 3, comment])
        Expect.equal (normalizeNewlines (tokenizedText actual)) (normalizeNewlines expected) "content"
        Expect.dictEqual actual.StringMap expectedStrings "string map"
        Expect.dictEqual actual.CommentMap expectedComments "comment map keeps every hash after the first"

    testCase "lone apostrophe does not swallow later double-quoted token" <| fun () ->
        // Regression: a lone apostrophe (a valid character inside a plain scalar) used to make
        // the double-quote pass treat everything up to EOF as an unclosed single-quoted segment,
        // skipping every later "..." token. The double-quoted value on the following line must
        // still be extracted despite the preceding apostrophe.
        expectTokens
            "desc: don't stop\nid: \"value\""
            "desc: don't stop\nid: <s f=0/>"
            [code; code]
            [0, dquote "value"]

    testCase "lone double quote does not swallow later single-quoted token" <| fun () ->
        // Symmetric regression for single-quoted tokens.
        expectTokens
            "desc: 3 \" inch mark\nid: 'value'"
            "desc: 3 \" inch mark\nid: <s f=0/>"
            [code; code]
            [0, single "value"]

    testCase "lone apostrophe keeps later double-quoted keys parseable" <| fun () ->
        // A plain scalar that contains an apostrophe must not stop a later double-quoted key on a
        // following line from being found.
        let hasIdEntry (desc: string) =
            let input = sprintf "description: %s\ndataset:\n  \"@id\": ./ref" desc
            (Persil.pipeline input).StringMap.Values
            |> Seq.exists (fun e -> e.Value = "@id" && e.Kind = QuotedStringKind.DoubleQuotedString)
        // both a lone apostrophe and a closed 'quoted' word inside the plain scalar must work
        Expect.isTrue (hasIdEntry "don't stop now") "double-quoted key after a lone apostrophe must still be parsed"
        Expect.isTrue (hasIdEntry "a 'quoted' word here") "double-quoted key after a closed 'quoted' word must still be parsed"

    testCase "mixed single and double quoted placeholders use unique indices" <| fun () ->
        expectTokens
            """mixed: ['a', "b"]"""
            """mixed: [<s f=0/>, <s f=1/>]"""
            [code]
            [0, single "a"; 1, dquote "b"]

    testCase "string ids follow the source order for double before single" <| fun () ->
        expectTokens
            """mixed: ["a", 'b', "c", 'd']"""
            """mixed: [<s f=0/>, <s f=1/>, <s f=2/>, <s f=3/>]"""
            [code]
            [0, dquote "a"; 1, single "b"; 2, dquote "c"; 3, single "d"]

    testCase "empty quotes" <| fun () ->
        // yaml@2.6.1 reads both as the empty string
        expectTokens
            "a: ''\nb: \"\"\n"
            "a: <s f=0/>\nb: <s f=1/>\n"
            [code; code; blank]
            [0, single ""; 1, dquote ""]

    testCase "doubled apostrophes are one apostrophe" <| fun () ->
        expectTokens
            "a: 'it''s'\nb: '''x'''\n"
            "a: <s f=0/>\nb: <s f=1/>\n"
            [code; code; blank]
            [0, single "it's"; 1, single "'x'"]

    testCase "escaped double quotes and backslashes stay raw interior text" <| fun () ->
        expectTokens
            "a: \"say \\\"hi\\\"\"\nb: \"back\\\\\"\nc: \"tab\\t\"\n"
            "a: <s f=0/>\nb: <s f=1/>\nc: <s f=2/>\n"
            [code; code; code; blank]
            [0, dquote "say \\\"hi\\\""; 1, dquote "back\\\\"; 2, dquote "tab\\t"]

    testCase "single-quoted scalar folds its line breaks" <| fun () ->
        expectTokens
            "a: 'one\n  two\n\n  three'\n"
            "a: <s f=0/>\n"
            [code; blank]
            [0, single "one two\nthree"]

    testCase "a multiline quote emits one line" <| fun () ->
        let actual = Persil.tokenize "a: 'x\n  y'\nb: 1\n"
        Expect.equal actual.Lines.Length 3 "the line break inside the quote is not emitted"
        Expect.equal (actual.Lines |> Array.map (fun line -> line.Text)) [| "a: <s f=0/>"; "b: 1"; "" |] "lines"

    testCase "multiline quote before a block scalar keeps the later string (single first)" <| fun () ->
        // Regression: the collapsed line shifted the line numbers of the block scalar detection,
        // so the double-quote pass ran inside the following block and corrupted the later string.
        expectTokens
            "a: 'x\n  y'\nb: |\n  \"hi\"\nc: \"z\"\n"
            "a: <s f=0/>\nb: |\n  \"hi\"\nc: <s f=1/>\n"
            [code; header; body; code; blank]
            [0, single "x y"; 1, dquote "z"]

    testCase "multiline quote before a block scalar keeps the later string (double first)" <| fun () ->
        expectTokens
            "a: \"x\n  y\"\nb: |\n  'hi'\nc: 'z'\n"
            "a: <s f=0/>\nb: |\n  'hi'\nc: <s f=1/>\n"
            [code; header; body; code; blank]
            [0, dquote "x\n  y"; 1, single "z"]

    testCase "several multiline quotes before a block scalar" <| fun () ->
        let input = "a: 'p\n  q'\nb: \"r\n  s\"\nc: 't\n  u'\nd: |\n  \"keep\"\n  'keep'\ne: \"v\"\nf: 'w'\n"
        expectTokens
            input
            "a: <s f=0/>\nb: <s f=1/>\nc: <s f=2/>\nd: |\n  \"keep\"\n  'keep'\ne: <s f=3/>\nf: <s f=4/>\n"
            [code; code; code; header; body; body; code; code; blank]
            [0, single "p q"; 1, dquote "r\n  s"; 2, single "t u"; 3, dquote "v"; 4, single "w"]

    testCase "unclosed single quote does not swallow later strings or comments" <| fun () ->
        let actual = Persil.tokenize "a: 'unclosed\nb: \"x\" # c\nc: 'y'\n"
        Expect.equal (tokenizedText actual) "a: 'unclosed\nb: <s f=0/> <c f=0/>\nc: <s f=1/>\n" "text"
        Expect.dictEqual actual.StringMap (stringMapOf [0, dquote "x"; 1, single "y"]) "string map"
        Expect.dictEqual actual.CommentMap (Dictionary<int, string>(Map [0, " c"])) "comment map"

    testCase "unclosed double quote at the end of the input does not swallow later strings or comments" <| fun () ->
        let actual = Persil.tokenize "a: \"never closed\nb: 'ok' # c"
        Expect.equal (tokenizedText actual) "a: \"never closed\nb: <s f=0/> <c f=0/>" "text"
        Expect.dictEqual actual.StringMap (stringMapOf [0, single "ok"]) "string map"
        Expect.dictEqual actual.CommentMap (Dictionary<int, string>(Map [0, " c"])) "comment map"

    testCase "several unclosed quotes in a row keep every closed string" <| fun () ->
        let actual = Persil.tokenize "a: 'p\nb: \"q\nc: 'r'\nd: \"s\"\n"
        Expect.equal (tokenizedText actual) "a: 'p\nb: \"q\nc: <s f=0/>\nd: <s f=1/>\n" "text"
        Expect.dictEqual actual.StringMap (stringMapOf [0, single "r"; 1, dquote "s"]) "string map"

    testCase "a closing quote with a bad end boundary leaves the text alone and keeps later strings" <| fun () ->
        // The quote closes, but a word follows it, so it was never a scalar.
        expectTokens
            "a: 'x' y\nb: \"z\" w\nc: 'ok'\n"
            "a: 'x' y\nb: \"z\" w\nc: <s f=0/>\n"
            [code; code; code; blank]
            [0, single "ok"]

    testCase "a probe does not search past the first closing quote when its boundary fails" <| fun () ->
        // The first `'x'` fails its boundary. The scanner must not read on to the next quote and
        // pair `'x' y\nb: '` as one scalar.
        let actual = Persil.tokenize "a: 'x' y\nb: 'z'\n"
        Expect.equal (tokenizedText actual) "a: 'x' y\nb: <s f=0/>\n" "text"
        Expect.dictEqual actual.StringMap (stringMapOf [0, single "z"]) "string map"

    testCase "quote at the end of the input without a line break" <| fun () ->
        expectTokens "a: 'x'" "a: <s f=0/>" [code] [0, single "x"]
        expectTokens "a: 'x" "a: 'x" [code] []
        expectTokens "a: \"x" "a: \"x" [code] []

    testCase "quotes in a word are not scalars" <| fun () ->
        expectTokens
            "a: rock 'n' roll\nb: don't\nc: 3\" wide\nd: x\"y\"z\n"
            "a: rock 'n' roll\nb: don't\nc: 3\" wide\nd: x\"y\"z\n"
            [code; code; code; code; blank]
            []

    testCase "tags and anchors may precede a quote" <| fun () ->
        expectTokens
            "a: !!str \"x\"\nb: &anchor 'y'\nc: !tag\t\"z\"\n- !<tag:yaml.org,2002:str> 'v'\n"
            "a: !!str <s f=0/>\nb: &anchor <s f=1/>\nc: !tag\t<s f=2/>\n- !<tag:yaml.org,2002:str> <s f=3/>\n"
            [code; code; code; code; blank]
            [0, dquote "x"; 1, single "y"; 2, dquote "z"; 3, single "v"]

    testCase "a quote without a space behind a property is accepted lexically" <| fun () ->
        // The boundary rule has always accepted this spelling. The scanner must not insert a space,
        // and the lexical acceptance says nothing about whether the Reader accepts the property.
        expectTokens
            "a: !tag\"x\"\nb: &anchor'y'\n"
            "a: !tag<s f=0/>\nb: &anchor<s f=1/>\n"
            [code; code; blank]
            [0, dquote "x"; 1, single "y"]

    testCase "quotes after indicators and in flow collections" <| fun () ->
        expectTokens
            "- 'a'\n- \"b\"\n? 'c'\n: \"d\"\ne: ['f', \"g\"]\nh: {'i': \"j\"}\n"
            "- <s f=0/>\n- <s f=1/>\n? <s f=2/>\n: <s f=3/>\ne: [<s f=4/>, <s f=5/>]\nh: {<s f=6/>: <s f=7/>}\n"
            [code; code; code; code; code; code; blank]
            [0, single "a"; 1, dquote "b"; 2, single "c"; 3, dquote "d"; 4, single "f"; 5, dquote "g"; 6, single "i"; 7, dquote "j"]
]
