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

    testCase "a quote at a node start that never closes is an error that names its line" <| fun () ->
        // yaml@2.6.1 rejects all four inputs with MISSING_CHAR
        let expectUnclosed (input: string) (line: int) =
            Expect.stringContains
                (errorOf (fun () -> Persil.tokenize input |> ignore))
                $"Line {line}: the quoted scalar that starts here is not closed."
                $"The error of {input}"
        expectUnclosed "a: 'unclosed\nb: 1\n" 1
        expectUnclosed "a: \"never closed\nb: 'ok' # c" 1
        expectUnclosed "a: 1\nb: 'p\nc: 2\n" 2
        expectUnclosed "- [a, \"b]\n" 1

    testCase "text behind a closing quote is an error that names the line of the quote" <| fun () ->
        // yaml@2.6.1 rejects all inputs with UNEXPECTED_TOKEN or MISSING_CHAR
        let expectTextBehind (input: string) (line: int) =
            Expect.stringContains
                (errorOf (fun () -> Persil.tokenize input |> ignore))
                $"Line {line}: text follows the closing quote of a quoted scalar."
                $"The error of {input}"
        expectTextBehind "a: 'x' y\nb: 'ok'\n" 1
        expectTextBehind "a: 1\nb: \"z\" w\n" 2
        expectTextBehind "k: 'it''s' x" 1
        expectTextBehind "a: 'x\n  y' z\n" 2
        expectTextBehind "k: \"a\", b" 1
        expectTextBehind "k: \"a\"]" 1
        expectTextBehind "k: \"a\":b" 1
        expectTextBehind "- [\"a\" b]" 1
        // A line break does not end a node inside a flow collection.
        expectTextBehind "[\"a\"\n b]" 1

    testCase "a probe does not search past the first closing quote when its boundary fails" <| fun () ->
        // The first `'x'` fails its boundary, so the error names line 1. The scanner does not read on to
        // the next quote and pair `'x' y\nb: '` as one scalar.
        Expect.stringContains
            (errorOf (fun () -> Persil.tokenize "a: 'x' y\nb: 'z'\n" |> ignore))
            "Line 1: text follows the closing quote of a quoted scalar."
            "The error names the first quote"

    testCase "quote at the end of the input without a line break" <| fun () ->
        expectTokens "a: 'x'" "a: <s f=0/>" [code] [0, single "x"]
        // yaml@2.6.1 rejects both unclosed quotes with MISSING_CHAR
        Expect.stringContains (errorOf (fun () -> Persil.tokenize "a: 'x" |> ignore)) "Line 1: the quoted scalar that starts here is not closed." "single"
        Expect.stringContains (errorOf (fun () -> Persil.tokenize "a: \"x" |> ignore)) "Line 1: the quoted scalar that starts here is not closed." "double"

    testCase "a closing quote followed by a blank, a colon or the end is a boundary" <| fun () ->
        // yaml@2.6.1: {"a":"x","b":"y","k":1,"j":2}
        expectTokens
            "a: 'x'  \nb: \"y\"\n'k': 1\n\"j\" : 2\n"
            "a: <s f=0/>  \nb: <s f=1/>\n<s f=2/>: 1\n<s f=3/> : 2\n"
            [code; code; code; code; blank]
            [0, single "x"; 1, dquote "y"; 2, single "k"; 3, dquote "j"]

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

    testCase "a double quote without a space behind a tag is accepted lexically" <| fun () ->
        // The boundary rule has always accepted this spelling. The scanner must not insert a space,
        // and the lexical acceptance says nothing about whether the Reader accepts the property.
        expectTokens
            "a: !tag\"x\"\n"
            "a: !tag<s f=0/>\n"
            [code; blank]
            [0, dquote "x"]

    testCase "a quote inside an anchor name or a single quote inside a tag name is part of the name" <| fun () ->
        // yaml@2.6.1: {"a":null}, {"b":"z"}, {"c":"p"} and ["z"]. The quotes are characters of the names, so they open no scalar.
        expectTokens
            "a: &anchor'y'\nb: &x\"y z\nc: !t'y p\n- &x\"y\" z\n"
            "a: &anchor'y'\nb: &x\"y z\nc: !t'y p\n- &x\"y\" z\n"
            [code; code; code; code; blank]
            []

    testCase "quotes after indicators and in flow collections" <| fun () ->
        expectTokens
            "- 'a'\n- \"b\"\n? 'c'\n: \"d\"\ne: ['f', \"g\"]\nh: {'i': \"j\"}\n"
            "- <s f=0/>\n- <s f=1/>\n? <s f=2/>\n: <s f=3/>\ne: [<s f=4/>, <s f=5/>]\nh: {<s f=6/>: <s f=7/>}\n"
            [code; code; code; code; code; code; blank]
            [0, single "a"; 1, dquote "b"; 2, single "c"; 3, dquote "d"; 4, single "f"; 5, dquote "g"; 6, single "i"; 7, dquote "j"]
]
