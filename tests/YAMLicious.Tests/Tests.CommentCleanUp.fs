module Tests.CommentCleanUp

open Fable.Pyxpecto
open YAMLicious
open YAMLiciousTypes
open System.Collections.Generic

let private code = Persil.LineKind.Code
let private blank = Persil.LineKind.Blank
let private commentOnly = Persil.LineKind.CommentOnly

let private commentMapOf (entries: (int * string) list) =
    Dictionary<int, string>(Map entries)

let private stringMapOf (entries: (int * StringMapEntry) list) =
    Dictionary<int, StringMapEntry>(Map entries)

let private kindsOf (tokenized: Persil.TokenizedYaml) =
    tokenized.Lines |> Array.map (fun line -> line.Kind) |> List.ofArray

/// Asserts the emitted text, the line kinds and the comment map. The string map is checked only
/// when entries are given.
let private expectComments (input: string) (text: string) (kinds: Persil.LineKind list) (comments: (int * string) list) =
    let actual = Persil.tokenize input
    Expect.encodeEqual (tokenizedText actual) text "text"
    Expect.equal (kindsOf actual) kinds "line kinds"
    Expect.dictEqual actual.CommentMap (commentMapOf comments) "comment map"
    actual

let Main = testList "CommentCleanUp" [
    testCase "full line" <| fun _ ->
        let s = """
# This is a line comment
"""
        let expected = """
<c f=0/>
"""
        let actual = Persil.tokenize s
        Expect.encodeEqual (tokenizedText actual) expected ""
        Expect.dictEqual actual.CommentMap (commentMapOf [0, " This is a line comment"]) ""
        Expect.equal (kindsOf actual) [blank; commentOnly; blank] "the line is a comment only"
        Expect.equal actual.StringMap.Count 0 "no strings"

    testCase "comments in values and behind values" <| fun _ ->
        // The `<s f=0/>` in the input is plain text, not a placeholder. The scanner moves the plain scalar
        // of the three lines, with that text in it, into the string table, so every placeholder in the
        // emitted text is its own. A quote inside a comment is comment text.
        let s = """
My Key: # This is a comment
  My Value1
  <s f=0/>
  My Value3 # :::: "This is also a comment"
"""
        let expected = """
My Key: <c f=0/>
  <s f=0/> <c f=1/>
"""
        let actual = Persil.tokenize s
        Expect.encodeEqual (tokenizedText actual) expected ""
        Expect.dictEqual actual.CommentMap (commentMapOf [0, " This is a comment"; 1, " :::: \"This is also a comment\""]) ""
        Expect.equal (kindsOf actual) [blank; code; code; blank] "a trailing comment keeps the line code"
        Expect.dictEqual actual.StringMap (stringMapOf [0, { Value = "My Value1\n<s f=0/>\nMy Value3"; Kind = QuotedStringKind.PlainScalar }]) "the scalar is the only string"

    testCase "Text in the shape of a comment placeholder is text and not a comment" <| fun _ ->
        let actual = expectComments "a: x <c f=0/>\nb: 1 # real\n" "a: x <s f=0/>\nb: 1 <c f=0/>\n" [code; code; blank] [0, " real"]
        Expect.dictEqual actual.StringMap (stringMapOf [0, { Value = "<c f=0/>"; Kind = QuotedStringKind.PlainScalar }]) "the literal text moved into the string table"
        Expect.equal (actual.Lines |> Array.map (fun line -> line.CommentId) |> List.ofArray) [Persil.NoComment; 0; Persil.NoComment] "only the second line has a comment"

    testCase "Hash without preceding whitespace is not a comment" <| fun _ ->
        expectComments "a: tool.cwl#main # real" "a: tool.cwl#main <c f=0/>" [code] [0, " real"] |> ignore

    testCase "C# and a hash behind a word are text" <| fun _ ->
        expectComments
            "lang: C# # real\nother: C#\nc: a#b#c\n"
            "lang: C# <c f=0/>\nother: C#\nc: a#b#c\n"
            [code; code; code; blank]
            [0, " real"]
        |> ignore

    testCase "a comment keeps its payload as written" <| fun _ ->
        expectComments
            "a: 1 #   spaced  \nb: 2 ### more # hashes\nc: 3 #\nd: 4 #end"
            "a: 1 <c f=0/>\nb: 2 <c f=1/>\nc: 3 <c f=2/>\nd: 4 <c f=3/>"
            [code; code; code; code]
            [0, "   spaced  "; 1, "## more # hashes"; 2, ""; 3, "end"]
        |> ignore

    testCase "a tab before the hash starts a comment" <| fun _ ->
        expectComments "a: 1\t# c\n" "a: 1\t<c f=0/>\n" [code; blank] [0, " c"] |> ignore

    testCase "comment-only lines keep their indentation" <| fun _ ->
        expectComments
            "a:\n  # one\n    # two\nb: 1\n#three\n"
            "a:\n  <c f=0/>\n    <c f=1/>\nb: 1\n<c f=2/>\n"
            [code; commentOnly; commentOnly; code; commentOnly; blank]
            [0, " one"; 1, " two"; 2, "three"]
        |> ignore

    testCase "a comment at the end of the input without a line break" <| fun _ ->
        expectComments "a: 1 # end" "a: 1 <c f=0/>" [code] [0, " end"] |> ignore
        expectComments "# only" "<c f=0/>" [commentOnly] [0, " only"] |> ignore

    testCase "quotes inside a comment are comment text" <| fun _ ->
        let actual = expectComments "a: 1 # it's \"x\" 'y'\nb: 'z'\n" "a: 1 <c f=0/>\nb: <s f=0/>\n" [code; code; blank] [0, " it's \"x\" 'y'"]
        Expect.dictEqual actual.StringMap (stringMapOf [0, { Value = "z"; Kind = QuotedStringKind.SingleQuotedString }]) "only the real string"

    testCase "a hash inside a quoted scalar is not a comment" <| fun _ ->
        let actual =
            expectComments
                "a: \"x # y\" # c\nb: 'p # q' # d\n"
                "a: <s f=0/> <c f=0/>\nb: <s f=1/> <c f=1/>\n"
                [code; code; blank]
                [0, " c"; 1, " d"]
        let expected =
            stringMapOf [
                0, { Value = "x # y"; Kind = QuotedStringKind.DoubleQuotedString }
                1, { Value = "p # q"; Kind = QuotedStringKind.SingleQuotedString }
            ]
        Expect.dictEqual actual.StringMap expected "strings keep their hashes"

    testCase "a quote glued to a word is not a string but a separated comment still is one" <| fun _ ->
        // `x"#suffix` has no string and no comment. `"x" # comment` has both.
        let glued = expectComments "x\"#suffix" "x\"#suffix" [code] []
        Expect.equal glued.StringMap.Count 0 "a quote inside a word opens no string"
        let separated = expectComments "\"x\" # comment" "<s f=0/> <c f=0/>" [code] [0, " comment"]
        Expect.dictEqual separated.StringMap (stringMapOf [0, { Value = "x"; Kind = QuotedStringKind.DoubleQuotedString }]) "string"

    testCase "a hash glued to a closing quote is an error" <| fun _ ->
        // yaml@2.6.1 rejects the input with MISSING_CHAR
        Expect.throws (fun () -> Persil.tokenize "\"x\"#suffix" |> ignore) "A comment needs a blank in front of its hash."

    testCase "a comment behind an unclosed quote is still found" <| fun _ ->
        expectComments "a: 'open # c\nb: 1 # d\n" "a: 'open <c f=0/>\nb: 1 <c f=1/>\n" [code; code; blank] [0, " c"; 1, " d"] |> ignore

    testCase "comments inside a multiline flow collection stay comments" <| fun _ ->
        let actual =
            expectComments
                "a: [ # open\n  'x', # one\n  \"y\" # two\n] # close\n"
                "a: [ <c f=0/>\n  <s f=0/>, <c f=1/>\n  <s f=1/> <c f=2/>\n] <c f=3/>\n"
                [code; code; code; code; blank]
                [0, " open"; 1, " one"; 2, " two"; 3, " close"]
        Expect.equal actual.StringMap.Count 2 "both strings"
]
