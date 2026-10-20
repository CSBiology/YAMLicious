module Tests.BlockScalars

open Fable.Pyxpecto
open YAMLicious
open YAMLiciousTypes

let private blockScalar (style: BlockScalarStyle) (chomp: ChompingMode) (indent: int option) (value: string) =
    YAMLElement.Value(YAMLContent.create(value, style=ScalarStyle.Block(style, chomp, indent)))

let private literalBlock (value: string) =
    blockScalar BlockScalarStyle.Literal ChompingMode.Clip None value

let private plainNode (value: string) =
    YAMLElement.Object [YAMLElement.Value(YAMLContent.create(value))]

let private mapping (key: string) (value: YAMLElement) =
    YAMLElement.Mapping(YAMLContent.create(key), value)

let private comment (text: string) = YAMLElement.Comment text

/// The message of the exception that the action throws, or an empty string when it succeeds.
let private errorOf (action: unit -> unit) =
    try
        action ()
        ""
    with e ->
        e.Message

/// Asserts that Preprocessing.read and Reader.read reject the input with the comment boundary error.
let private expectCommentBoundaryError (yaml: string) (commentText: string) (scalarLine: int) (commentLine: int) (contentLine: int) =
    let locations = sprintf "(Scalar line %d; comment line %d; content line %d.)" scalarLine commentLine contentLine
    let messages =
        [ errorOf (fun () -> Preprocessing.read yaml |> ignore)
          errorOf (fun () -> Reader.read yaml |> ignore) ]
    for message in messages do
        Expect.stringContains message (sprintf "A comment line (#%s) ends a block scalar, so the more indented content after it is invalid." commentText) "The error should name the comment."
        Expect.stringContains message locations "The error should name the three source lines."

/// Asserts that Preprocessing.read and Reader.read reject the input with the indentation error, which
/// does not name a comment.
let private expectIndentationError (yaml: string) (scalarLine: int) (contentLine: int) =
    let locations = sprintf "(Scalar line %d; content line %d.)" scalarLine contentLine
    let messages =
        [ errorOf (fun () -> Preprocessing.read yaml |> ignore)
          errorOf (fun () -> Reader.read yaml |> ignore) ]
    for message in messages do
        Expect.stringContains message "A line that is more indented than the node of a block scalar" "The error should describe the indentation."
        Expect.stringContains message locations "The error should name the two source lines."
        Expect.isFalse (message.Contains "comment") "The error should not blame a comment."

let private unitTests = testList "Block scalar units" [
    testCase "A comment between the header and the content indentation ends the scalar and precedes the next key" <| fun _ ->
        // yaml@2.6.1: {"a":"x\n","b":1}
        let expected = YAMLElement.Object [ mapping "a" (literalBlock "x\n"); comment " c"; mapping "b" (plainNode "1") ]
        Expect.equal (Reader.read "a: |\n    x\n # c\nb: 1\n") expected "The comment at one space is outside the scalar"

    testCase "Several comments at different indentation keep their order across blank lines" <| fun _ ->
        // yaml@2.6.1: {"a":"x\n","b":1}
        let expected =
            YAMLElement.Object [
                mapping "a" (literalBlock "x\n")
                comment " c1"
                comment " c2"
                comment " c3"
                mapping "b" (plainNode "1")
            ]
        Expect.equal (Reader.read "a: |\n    x\n  # c1\n # c2\n\n# c3\nb: 1\n") expected "All three comments follow the scalar in order"

    testCase "Comments and blank lines after a terminating comment stay outside a keep scalar" <| fun _ ->
        // yaml@2.6.1: {"a":"x\n\n\n","b":1}
        let expected =
            YAMLElement.Object [
                mapping "a" (blockScalar BlockScalarStyle.Literal ChompingMode.Keep None "x\n\n\n")
                comment " c"
                mapping "b" (plainNode "1")
            ]
        Expect.equal (Reader.read "a: |+\n  x\n\n\n# c\nb: 1\n") expected "Both blank lines before the comment belong to the scalar"

    testCase "Comments at the end of the input stay in the block of the scalar header" <| fun _ ->
        // yaml@2.6.1: {"a":{"b":"x\n"}}
        let expected =
            YAMLElement.Object [
                mapping "a" (YAMLElement.Object [ mapping "b" (literalBlock "x\n"); comment " c1"; comment " c2" ])
                comment " c3"
            ]
        Expect.equal (Reader.read "a:\n  b: |\n      x\n    # c1\n\n  # c2\n\n# c3\n") expected "A comment is no deeper than the header and no deeper than its own indentation"

    testCase "A comment before a scalar header precedes the key" <| fun _ ->
        // yaml@2.6.1: {"a":"x\n"} and {"top":{"k":1,"s":"body\n"}}
        Expect.equal
            (Reader.read "# lead\na: |\n  x\n")
            (YAMLElement.Object [ comment " lead"; mapping "a" (literalBlock "x\n") ])
            "The comment is the first entry of the root"
        Expect.equal
            (Reader.read "top:\n  k: 1\n  # c\n  s: |\n    body\n")
            (YAMLElement.Object [
                mapping "top" (YAMLElement.Object [ mapping "k" (plainNode "1"); comment " c"; mapping "s" (literalBlock "body\n") ])
            ])
            "The comment stays in the block of its scalar"

    testCase "A comment after a scalar body belongs to the next sibling or to the dedented node" <| fun _ ->
        // yaml@2.6.1: {"a":{"s":"body\n","t":1}} and {"a":{"s":"body\n"},"b":1}
        Expect.equal
            (Reader.read "a:\n  s: |\n    body\n  # c\n  t: 1\n")
            (YAMLElement.Object [
                mapping "a" (YAMLElement.Object [ mapping "s" (literalBlock "body\n"); comment " c"; mapping "t" (plainNode "1") ])
            ])
            "A sibling follows the comment in the same block"
        Expect.equal
            (Reader.read "a:\n  s: |\n    body\n# c\nb: 1\n")
            (YAMLElement.Object [
                mapping "a" (YAMLElement.Object [ mapping "s" (literalBlock "body\n") ])
                comment " c"
                mapping "b" (plainNode "1")
            ])
            "A dedented node takes the comment into the outer block"

    testCase "A comment at the end of the input stays with the node that owns the scalar" <| fun _ ->
        // yaml@2.6.1: {"a":{"s":"body\n"}} for both inputs
        Expect.equal
            (Reader.read "a:\n  s: |\n    body\n  # c\n")
            (YAMLElement.Object [ mapping "a" (YAMLElement.Object [ mapping "s" (literalBlock "body\n"); comment " c" ]) ])
            "A comment at the indentation of the header joins the block of the header"
        Expect.equal
            (Reader.read "a:\n  s: |\n    body\n# c\n")
            (YAMLElement.Object [ mapping "a" (YAMLElement.Object [ mapping "s" (literalBlock "body\n") ]); comment " c" ])
            "A comment at the root indentation stays in the root"

    testCase "Comments between sequence items that hold scalars keep their place" <| fun _ ->
        // yaml@2.6.1: {"x":["a\n","b\n"]} and ["a",  "x\n", "b"]
        Expect.equal
            (Reader.read "x:\n  - |\n    a\n  # c\n  - |\n    b\n")
            (YAMLElement.Object [
                mapping "x" (YAMLElement.Object [
                    YAMLElement.Sequence [ YAMLElement.Object [ literalBlock "a\n" ]; comment " c"; YAMLElement.Object [ literalBlock "b\n" ] ]
                ])
            ])
            "The comment sits between the two scalar items"
        Expect.equal
            (Reader.read "- a\n- |\n  x\n# c\n- b\n")
            (YAMLElement.Object [
                YAMLElement.Sequence [ plainNode "a"; YAMLElement.Object [ literalBlock "x\n" ]; comment " c"; plainNode "b" ]
            ])
            "A later scalar item is followed by the comment"

    testCase "A comment behind the first scalar item of an indentless sequence stays between the items" <| fun _ ->
        // yaml@2.6.1: {"key":["x\ny\n","z"]}
        Expect.equal
            (Reader.read "key:\n- |\n  x\n  y\n# c\n- z\n")
            (YAMLElement.Object [
                mapping "key" (YAMLElement.Object [
                    YAMLElement.Sequence [ YAMLElement.Object [ literalBlock "x\ny\n" ]; comment " c"; plainNode "z" ]
                ])
            ])
            "The comment sits between the scalar item and the next item"

    testCase "A comment after a root scalar stays behind it" <| fun _ ->
        // yaml@2.6.1: "x\n"
        Expect.equal
            (Reader.read "|\n  x\n# c\n")
            (YAMLElement.Object [ literalBlock "x\n"; comment " c" ])
            "The root scalar keeps its comment"

    testCase "A comment behind the header and a comment behind the body are different comments" <| fun _ ->
        // yaml@2.6.1: {"a":"x\n","b":1}
        let header = YAMLElement.Value(YAMLContent.create("x\n", comment=" hc", style=ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Clip, None)))
        Expect.equal
            (Reader.read "a: | # hc\n  x\n# c\nb: 1\n")
            (YAMLElement.Object [ mapping "a" header; comment " c"; mapping "b" (plainNode "1") ])
            "The header comment belongs to the scalar"

    testCase "Tags and anchors on scalars with comments around them" <| fun _ ->
        // yaml@2.6.1: {"a":"x\n","b":"y\n"}, "x\n", "x\n" and ["x\n","y"]
        let literalWith (tag: string option) (anchor: string option) (value: string) =
            YAMLElement.Value(YAMLContent.create(value, ?tag=tag, ?anchor=anchor, style=ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Clip, None)))
        Expect.equal
            (Reader.read "a: !!str |\n  x\n# c\nb: &x |\n  y\n")
            (YAMLElement.Object [
                mapping "a" (literalWith (Some "tag:yaml.org,2002:str") None "x\n")
                comment " c"
                mapping "b" (literalWith None (Some "x") "y\n")
            ])
            "Mapping values keep tag and anchor"
        Expect.equal
            (Reader.read "!!str |\n  x\n")
            (YAMLElement.Object [ literalWith (Some "tag:yaml.org,2002:str") None "x\n" ])
            "A root scalar keeps its tag"
        Expect.equal
            (Reader.read "&a |\n  x\n# c\n")
            (YAMLElement.Object [ literalWith None (Some "a") "x\n"; comment " c" ])
            "A root scalar keeps its anchor and the comment follows"
        Expect.equal
            (Reader.read "- !!str |\n  x\n- &a >-\n  y\n")
            (YAMLElement.Object [
                YAMLElement.Sequence [
                    YAMLElement.Object [ literalWith (Some "tag:yaml.org,2002:str") None "x\n" ]
                    YAMLElement.Object [ YAMLElement.Value(YAMLContent.create("y", anchor="a", style=ScalarStyle.Block(BlockScalarStyle.Folded, ChompingMode.Strip, None))) ]
                ]
            ])
            "Sequence items keep tag and anchor"

    testCase "A comment line that ends a scalar before more indented content is an error with locations" <| fun _ ->
        // yaml@2.6.1 rejects every input in this test case with BAD_INDENT
        expectCommentBoundaryError "a: >-\n  text\n# c\n  more\n" " c" 1 3 4
        expectCommentBoundaryError "a: |\n    x\n  # c\n   bad\n" " c" 1 3 4
        expectCommentBoundaryError "x:\n  a: |\n      body\n    # c\n     bad\n" " c" 2 4 5

    testCase "More indented content after a comment is an error at or above the content indentation too" <| fun _ ->
        expectCommentBoundaryError "a: |\n    x\n  # c\n    bad\n" " c" 1 3 4
        expectCommentBoundaryError "a: |\n  x\n# c\n    bad\n" " c" 1 3 4
        expectCommentBoundaryError "a: |2\n    x\n # c\n      bad\n" " c" 1 3 4

    testCase "The error names the first comment after the scalar and the first content line behind them" <| fun _ ->
        // Blank lines and later comments do not change the report.
        expectCommentBoundaryError "a: |\n  x\n\n# c1\n   # c2\n\n# c3\n  bad\n" " c1" 1 4 8

    testCase "The source lines count a multiline quoted scalar before the block scalar" <| fun _ ->
        expectCommentBoundaryError "q: 'a\n  b'\ns: |\n  x\n# c\n  bad\n" " c" 3 5 6

    testCase "The source lines count CRLF line endings" <| fun _ ->
        expectCommentBoundaryError "a: |\r\n  x\r\n# c\r\n  bad\r\n" " c" 1 3 4

    testCase "A sequence item scalar ends at the dash and rejects content behind a comment" <| fun _ ->
        // yaml@2.6.1: BAD_INDENT
        expectCommentBoundaryError "- |\n    x\n  # c\n  - y\n" " c" 1 3 4

    testCase "Content more indented than the node of the scalar is an error without a comment" <| fun _ ->
        // yaml@2.6.1: BAD_INDENT
        expectIndentationError "a: |\n    text\n  more\n" 1 3
        expectIndentationError "a: |4\n  x\nb: 1\n" 1 2

    testCase "A comment block after a scalar is no error at the end of the input or before a sibling" <| fun _ ->
        // yaml@2.6.1: {"a":"x\n","b":1}
        let expected = YAMLElement.Object [ mapping "a" (literalBlock "x\n"); comment " c"; comment " d"; mapping "b" (plainNode "1") ]
        Expect.equal (Reader.read "a: |\n    x\n  # c\n\n  # d\nb: 1\n") expected "A sibling at the indentation of the node is valid"
        Expect.equal
            (Reader.read "a: |\n    x\n  # c\n")
            (YAMLElement.Object [ mapping "a" (literalBlock "x\n"); comment " c" ])
            "The end of the input is valid"

    testCase "A sibling of a compact mapping scalar after a terminating comment is no boundary error" <| fun _ ->
        // yaml@2.6.1: [{"k":"text\n","next":1}]. The compact header is still read wrongly later on, so
        // this checks only that the boundary uses the column of the key and not the column of the dash.
        Expect.equal (errorOf (fun () -> Preprocessing.read "- k: |\n    text\n# about next\n  next: 1\n" |> ignore)) "" "Preprocessing accepts the input"

    testCase "readDocuments rejects the boundary error too" <| fun _ ->
        let message = errorOf (fun () -> Reader.readDocuments "a: |\n  x\n# c\n  bad\n---\nb: 1\n" |> ignore)
        Expect.stringContains message "A comment line (# c) ends a block scalar" "The first document is invalid"
        Expect.stringContains message "(Scalar line 1; comment line 3; content line 4.)" "The line numbers count from the start of the input"

    testCase "Empty and blank-only bodies keep line breaks only under keep chomping" <| fun _ ->
        // yaml@2.6.1: an empty body reads as "" for every chomping. Blank lines count under keep only.
        let styles = [ "|", BlockScalarStyle.Literal; ">", BlockScalarStyle.Folded ]
        let chompings = [ "-", ChompingMode.Strip; "", ChompingMode.Clip; "+", ChompingMode.Keep ]
        for (indicator, style) in styles do
            for (suffix, chomp) in chompings do
                for blanks in [ 0; 2 ] do
                    let value = if chomp = ChompingMode.Keep then String.replicate blanks "\n" else ""
                    let scalar = blockScalar style chomp None value
                    let header = "a: " + indicator + suffix + "\n" + String.replicate blanks "\n"
                    Expect.equal
                        (Reader.read (header + "b: 1\n"))
                        (YAMLElement.Object [ mapping "a" scalar; mapping "b" (plainNode "1") ])
                        (sprintf "Followed by a sibling: %A" header)
                    Expect.equal
                        (Reader.read header)
                        (YAMLElement.Object [ mapping "a" scalar ])
                        (sprintf "At the end of the input: %A" header)

    testCase "Leading and trailing blank lines of a body follow the chomping" <| fun _ ->
        // yaml@2.6.1: "\nx" for strip, "\nx\n" for clip and "\nx\n\n\n" for keep, with both styles
        let expectedValues = [ "-", ChompingMode.Strip, "\nx"; "", ChompingMode.Clip, "\nx\n"; "+", ChompingMode.Keep, "\nx\n\n\n" ]
        for (indicator, style) in [ "|", BlockScalarStyle.Literal; ">", BlockScalarStyle.Folded ] do
            for (suffix, chomp, value) in expectedValues do
                Expect.equal
                    (Reader.read ("a: " + indicator + suffix + "\n\n  x\n\n\nb: 1\n"))
                    (YAMLElement.Object [ mapping "a" (blockScalar style chomp None value); mapping "b" (plainNode "1") ])
                    (sprintf "Style %s with chomping %A" indicator chomp)

    testCase "An empty block scalar reads as the empty string in every position" <| fun _ ->
        // yaml@2.6.1: {"a":"","b":1}, ["","x"], "" and {"a":""}
        Expect.equal
            (Reader.read "a: |\nb: 1\n")
            (YAMLElement.Object [ mapping "a" (literalBlock ""); mapping "b" (plainNode "1") ])
            "A mapping value"
        Expect.equal
            (Reader.read "- |\n- x\n")
            (YAMLElement.Object [ YAMLElement.Sequence [ YAMLElement.Object [ literalBlock "" ]; plainNode "x" ] ])
            "A sequence item"
        Expect.equal (Reader.read "|\n") (YAMLElement.Object [ literalBlock "" ]) "A root scalar"
        Expect.equal (Reader.read "a: |") (YAMLElement.Object [ mapping "a" (literalBlock "") ]) "The end of the input without a line break"

    testCase "Hash lines and placeholder-shaped text in a body are content" <| fun _ ->
        // yaml@2.6.1: {"a":"# not a comment\n<c f=0/>\n<s f=3/>\n"}
        Expect.equal
            (Reader.read "a: |\n  # not a comment\n  <c f=0/>\n  <s f=3/>\n")
            (YAMLElement.Object [ mapping "a" (literalBlock "# not a comment\n<c f=0/>\n<s f=3/>\n") ])
            "Every line is text"

    testCase "A body line that looks like the placeholder of a comment is not the comment of the document" <| fun _ ->
        // yaml@2.6.1: {"a":"<c f=0/>\n","b":1}
        let expected =
            YAMLElement.Object [
                mapping "a" (literalBlock "<c f=0/>\n")
                mapping "b" (YAMLElement.Object [ YAMLElement.Value(YAMLContent.create("1", comment=" real")) ])
            ]
        Expect.equal (Reader.read "a: |\n  <c f=0/>\nb: 1 # real\n") expected "The real comment belongs to b and the body keeps its text"

    testCase "A body line that looks like the placeholder of a string is not a string" <| fun _ ->
        // yaml@2.6.1: {"a":"<s f=0/>\n","b":"q"}
        let expected =
            YAMLElement.Object [
                mapping "a" (literalBlock "<s f=0/>\n")
                mapping "b" (YAMLElement.Object [ YAMLElement.Value(YAMLContent.create("q", style=ScalarStyle.SingleQuoted)) ])
            ]
        Expect.equal (Reader.read "a: |\n  <s f=0/>\nb: 'q'\n") expected "The body keeps its text"

    testCase "A body line shaped like a comment placeholder reads end to end as text" <| fun _ ->
        // yaml@2.6.1: {"a":"<c f=0/>\n"}
        let yaml = "a: |\n  <c f=0/>\n"
        let expected = YAMLElement.Object [ mapping "a" (literalBlock "<c f=0/>\n") ]
        Expect.equal (Reader.read yaml) expected "Reader.read"
        Expect.equal (Reader.readDocuments yaml) [ expected ] "Reader.readDocuments"
        Expect.equal (Decode.read yaml |> Decode.object (fun get -> get.Required.Field "a" Decode.string)) "<c f=0/>\n" "Decode"

    testCase "A lead comment and a hash line at the end of a body do not mix" <| fun _ ->
        // yaml@2.6.1: {"a":"<c f=0/>\n# d\n"}
        Expect.equal
            (Reader.read "# c\na: |\n  <c f=0/>\n  # d\n")
            (YAMLElement.Object [ comment " c"; mapping "a" (literalBlock "<c f=0/>\n# d\n") ])
            "The first comment is the document comment and the second is text"
        // yaml@2.6.1: {"a":"x\n\n# tail\n"}
        Expect.equal
            (Reader.read "a: |\n  x\n\n  # tail\n")
            (YAMLElement.Object [ mapping "a" (literalBlock "x\n\n# tail\n") ])
            "A hash line at the content indentation after a blank line is text"

    testCase "Header-looking text in a body stays literal in a sequence item and a folded scalar" <| fun _ ->
        // yaml@2.6.1: ["a: |\n  b\n- c\n"] and {"key":"x: | y\n"}
        Expect.equal
            (Reader.read "- |\n  a: |\n    b\n  - c\n")
            (YAMLElement.Object [ YAMLElement.Sequence [ YAMLElement.Object [ literalBlock "a: |\n  b\n- c\n" ] ] ])
            "A sequence item body"
        Expect.equal
            (Reader.read "key: >\n  x: |\n  y\n")
            (YAMLElement.Object [ mapping "key" (blockScalar BlockScalarStyle.Folded ChompingMode.Clip None "x: | y\n") ])
            "A folded body"

    testCase "The indentation pass keeps the header with an empty body" <| fun _ ->
        Expect.equal
            (Preprocessing.read "a: |\nb: 1\n").AST
            (Level [ Line "a: |"; Intendation []; Line "b: 1" ])
            "The empty body is an empty Intendation"

    testCase "The indentation pass keeps a body flat and moves the comment to the next node" <| fun _ ->
        Expect.equal
            (Preprocessing.read "a:\n  s: |\n    body\n  # c\n  t: 1\n").AST
            (Level [
                Line "a:"
                Intendation [
                    Line "s: |"
                    Intendation [ Line "  body" ]
                    Line "<c f=0/>"
                    Line "t: 1"
                ]
            ])
            "The comment is a line of the block of the header"
        Expect.equal
            (Preprocessing.read "a: |\n    x\n  # c\n\n  # d\nb: 1\n").AST
            (Level [
                Line "a: |"
                Intendation [ Line "    x" ]
                Line "<c f=0/>"
                Line ""
                Line "<c f=1/>"
                Line "b: 1"
            ])
            "Comments less indented than the content leave the block of the scalar and take the indentation of the next node"

    testCase "The indentation pass does not align body lines that look like comments" <| fun _ ->
        Expect.equal
            (Preprocessing.read "# lead\na: |\n  <c f=0/>\n  # d\n").AST
            (Level [
                Line "<c f=0/>"
                Line "a: |"
                Intendation [ Line "  <c f=0/>"; Line "  # d" ]
            ])
            "Only the real comment is a comment line"
]

/// An input of the given number of lines, repeated to the size that a run needs. Each generator returns
/// a document that stresses one part of the indentation pass.
let private bodyInput (lines: int) =
    let sb = System.Text.StringBuilder()
    sb.Append("a: |\n") |> ignore
    for i in 0 .. lines - 1 do
        sb.Append(sprintf "  line %d\n" i) |> ignore
    sb.Append("b: 1\n") |> ignore
    sb.ToString()

let private commentRunInput (comments: int) =
    let sb = System.Text.StringBuilder()
    sb.Append("root:\n  a: |\n    x\n") |> ignore
    for i in 0 .. comments - 1 do
        sb.Append(String.replicate (i % 5 * 2) " ").Append(sprintf "# c%d\n" i) |> ignore
        if i % 7 = 0 then sb.Append("\n") |> ignore
    sb.Append("  b: 1\nc: 2\n") |> ignore
    sb.ToString()

let private trailingCommentRunInput (comments: int) =
    let sb = System.Text.StringBuilder()
    sb.Append("root:\n  a: |\n    x\n") |> ignore
    for i in 0 .. comments - 1 do
        sb.Append(String.replicate (i % 5 * 2) " ").Append(sprintf "# c%d\n" i) |> ignore
    sb.ToString()

let private flowCloserCommentRunInput (comments: int) =
    let sb = System.Text.StringBuilder()
    sb.Append("a:\n  - [\n    1,\n") |> ignore
    for i in 0 .. comments - 1 do
        sb.Append(sprintf "# c%d\n" i) |> ignore
    sb.Append("    ]\n") |> ignore
    sb.ToString()

let private blankRunInput (blanks: int) =
    "a:\n  b: 1\n" + String.replicate blanks "\n" + "  c: 2\nd: 3\n" + String.replicate blanks "\n"

let private siblingScalarsInput (scalars: int) =
    let sb = System.Text.StringBuilder()
    for i in 0 .. scalars / 3 - 1 do
        sb.Append(sprintf "k%d: |\n  x\n# c\n" i) |> ignore
    sb.ToString()

/// Runs the action repeatedly for a fixed time and returns the milliseconds of one run.
let private sampleMilliseconds (action: unit -> unit) =
    let startTime = System.DateTime.UtcNow
    let mutable runs = 0
    let mutable elapsed = 0.0
    while elapsed < 100.0 do
        action ()
        runs <- runs + 1
        elapsed <- (System.DateTime.UtcNow - startTime).TotalMilliseconds
    elapsed / float runs

/// The fastest of three samples, which keeps a pause of the garbage collector out of the result.
let private millisecondsPerRun (action: unit -> unit) =
    [ sampleMilliseconds action; sampleMilliseconds action; sampleMilliseconds action ] |> List.min

/// Eight times the lines take about eight times as long in a linear pass and about sixty-four times as
/// long in a quadratic one. The limit sits between both and leaves room for the noise of a short
/// measurement, such as garbage collection when the input outgrows the cache.
let private expectLinear (name: string) (build: int -> string) =
    let small = millisecondsPerRun (fun () -> Preprocessing.read (build 2000) |> ignore)
    let large = millisecondsPerRun (fun () -> Preprocessing.read (build 16000) |> ignore)
    let ratio = large / small
    Expect.isTrue (ratio < 24.0) (sprintf "%s: 2000 lines took %.3f ms and 16000 lines took %.3f ms, a ratio of %.2f" name small large ratio)

let private scalingTests = testList "Block scalar scaling" [
    testCase "A body of more than 5000 lines reads as one scalar" <| fun _ ->
        let lines = 5200
        let expectedValue = String.concat "" [ for i in 0 .. lines - 1 -> sprintf "line %d\n" i ]
        let expected = YAMLElement.Object [ mapping "a" (literalBlock expectedValue); mapping "b" (plainNode "1") ]
        Expect.equal (Reader.read (bodyInput lines)) expected "The body keeps every line in order"

    testCase "A run of more than 5000 comments after a scalar keeps its order" <| fun _ ->
        let comments = 5200
        match Reader.read (commentRunInput comments) with
        | YAMLElement.Object [ YAMLElement.Mapping(_, YAMLElement.Object nested); YAMLElement.Mapping(_, _) ] ->
            let texts = nested |> List.choose (function YAMLElement.Comment text -> Some text | _ -> None)
            Expect.equal texts.Length comments "Every comment is in the block of the header"
            Expect.equal (List.head texts) " c0" "The first comment stays first"
            Expect.equal (List.last texts) (sprintf " c%d" (comments - 1)) "The last comment stays last"
            Expect.equal (nested |> List.last) (mapping "b" (plainNode "1")) "The sibling follows the comments"
        | _ -> failwith "The root should hold the mapping of the header and its sibling."

    testCase "The time grows linearly with the lines of a body" <| fun _ ->
        expectLinear "body" bodyInput

    testCase "The time grows linearly with a run of comments between nodes" <| fun _ ->
        expectLinear "comments" commentRunInput

    testCase "The time grows linearly with a run of comments at the end of the input" <| fun _ ->
        expectLinear "trailing comments" trailingCommentRunInput

    testCase "The time grows linearly with a run of comments in front of a flow closer" <| fun _ ->
        expectLinear "flow closer" flowCloserCommentRunInput

    testCase "The time grows linearly with a run of blank lines" <| fun _ ->
        expectLinear "blank lines" blankRunInput

    testCase "The time grows linearly with the number of scalars and comments" <| fun _ ->
        expectLinear "sibling scalars" siblingScalarsInput
]

let Main =
  testList "Block Scalars" [
    testCase "Explicit indentation preserves leading content spaces" <| fun _ ->
        let yaml = """- |1
  explicit
"""
        let expected = YAMLElement.Object [
            YAMLElement.Sequence [
                YAMLElement.Object [
                    YAMLElement.Value(
                        YAMLContent.create(
                            " explicit\n",
                            style=ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Clip, Some 1)
                        )
                    )
                ]
            ]
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Explicit indent indicator should preserve leading content spaces"

    testCase "Explicit Indentation Indicator" <| fun _ ->
        let yaml = """doc: |2
  explicit
"""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("doc"),
                // Since base indent is 2, content "  explicit" indented by 2 -> "explicit"
                // But wait, the standard usually implies relative indentation.
                // In YAMLicious Preprocessing, indentation is handled by Level parsing.
                // If we use |2, we tell the parser the indentation level.
                // NOTE: Current YAMLicious preprocessing separates indentation before Reader sees it.
                // We test if Reader accepts the header "|2".
                YAMLElement.Value(YAMLContent.create("explicit\n", style=ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Clip, Some 2)))
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Should parse explicit indentation header"

    testCase "Chomping Strip" <| fun _ ->
        let yaml = """strip: |-
  text
"""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("strip"),
                YAMLElement.Value(YAMLContent.create("text", style=ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Strip, None)))
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Should strip trailing newline"

    testCase "Chomping Keep" <| fun _ ->
        let yaml = """keep: |+
  text

"""
        let expected = YAMLElement.Object [
             YAMLElement.Mapping(
                YAMLContent.create("keep"),
                // "text\n" (from text line) + "\n" (empty line) -> "text\n\n"
                YAMLElement.Value(YAMLContent.create("text\n\n", style=ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Keep, None)))
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Should keep all trailing newlines"

    testCase "Folded Style" <| fun _ ->
        let yaml = """folded: >
  line 1
  line 2
"""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("folded"),
                YAMLElement.Value(YAMLContent.create("line 1 line 2\n", style=ScalarStyle.Block(BlockScalarStyle.Folded, ChompingMode.Clip, None)))
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Should fold newlines to spaces"

    testCase "Literal style preserves additional indentation" <| fun _ ->
        let yaml = """doc: |
  line1
    indented
  line3
"""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("doc"),
                YAMLElement.Value(
                    YAMLContent.create(
                        "line1\n  indented\nline3\n",
                        style=ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Clip, None)
                    )
                )
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Literal block should preserve relative indentation"

    testCase "Folded style preserves more-indented lines" <| fun _ ->
        let yaml = """folded: >
  Sammy Sosa completed another
  fine season with great stats.

    63 Home Runs
    0.288 Batting Average

  What a year!
"""
        let expectedValue = "Sammy Sosa completed another fine season with great stats.\n\n  63 Home Runs\n  0.288 Batting Average\n\nWhat a year!\n"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("folded"),
                YAMLElement.Value(
                    YAMLContent.create(
                        expectedValue,
                        style=ScalarStyle.Block(BlockScalarStyle.Folded, ChompingMode.Clip, None)
                    )
                )
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Folded block should preserve more-indented line breaks"

    testCase "Block scalar header supports inline comments" <| fun _ ->
        let yaml = """doc: | # comment
  hello
"""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("doc"),
                YAMLElement.Value(
                    YAMLContent.create(
                        "hello\n",
                        comment=" comment",
                        style=ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Clip, None)
                    )
                )
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Inline header comments should be parsed and preserved"

    testCase "Root-level folded scalar parses" <| fun _ ->
        let yaml = """>
 text line1
 text line2
"""
        let expected = YAMLElement.Object [
            YAMLElement.Value(
                YAMLContent.create(
                    "text line1 text line2\n",
                    style=ScalarStyle.Block(BlockScalarStyle.Folded, ChompingMode.Clip, None)
                )
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Root block scalar should parse as value node"

    testCase "Block scalar preserves leading blank content line" <| fun _ ->
        let yaml = """doc: |

  hello
"""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("doc"),
                YAMLElement.Value(
                    YAMLContent.create(
                        "\nhello\n",
                        style=ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Clip, None)
                    )
                )
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Leading blank lines after a block scalar header should stay as scalar content"

    testCase "Sequence item supports literal block scalar" <| fun _ ->
        let yaml = """- |
  text
"""
        let expected = YAMLElement.Object [
            YAMLElement.Sequence [
                YAMLElement.Object [
                    YAMLElement.Value(
                        YAMLContent.create(
                            "text\n",
                            style=ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Clip, None)
                        )
                    )
                ]
            ]
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Sequence block scalar should parse as a single scalar value item"

    testCase "Literal block scalar preserves trailing spaces" <| fun _ ->
        let yaml = """doc: |
  a 
  b  
"""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("doc"),
                YAMLElement.Value(
                    YAMLContent.create(
                        "a \nb  \n",
                        style=ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Clip, None)
                    )
                )
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Trailing spaces in literal block content should be preserved"

    testCase "Block scalar keeps quote delimiters exactly" <| fun _ ->
        let yaml = """doc: |
  sql: 'it''s'
  if x == "y"
"""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("doc"),
                YAMLElement.Value(
                    YAMLContent.create(
                        "sql: 'it''s'\nif x == \"y\"\n",
                        style=ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Clip, None)
                    )
                )
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Quoted delimiters inside block scalars should remain literal content"

    testCase "Block scalar keeps quote delimiters on every body line" <| fun _ ->
        let yaml = """expression: >
  ${ return (function() {
    var allowedFields = ['class', 'basename'];
    if (sanitized.basename === "arc") return null;
    if (sanitized.class === "Directory") return null;
  })(); }
"""
        let actual = Reader.read yaml
        match actual with
        | YAMLElement.Object [YAMLElement.Mapping(_, YAMLElement.Value content)] ->
            Expect.equal (content.Value.Contains("'class'")) true "Should preserve single-quoted strings in block scalars"
            Expect.equal (content.Value.Contains("\"arc\"")) true "Should preserve double-quoted strings in block scalars"
            Expect.equal (content.Value.Contains("\"Directory\"")) true "Should preserve all double-quoted strings in block scalars"
        | _ ->
            failwithf "Unexpected AST: %A" actual

    testCase "Block scalar with hash content" <| fun _ ->
        let yaml = """comment: |
  # Not a comment
  value"""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("comment"),
                YAMLElement.Value(YAMLContent.create("# Not a comment\nvalue\n", style=ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Clip, None)))
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Hash inside block scalar should be treated as content, not a comment"

    testCase "Block scalar preserves final hash content before dedent" <| fun _ ->
        let yaml = """doc: |
  Workflow doc line.
  # This line is doc text, not a YAML comment.
next: value"""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("doc"),
                YAMLElement.Value(YAMLContent.create("Workflow doc line.\n# This line is doc text, not a YAML comment.\n", style=ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Clip, None)))
            )
            YAMLElement.Mapping(
                YAMLContent.create("next"),
                YAMLElement.Object [
                    YAMLElement.Value(YAMLContent.create("value"))
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Indented hash content should stay in the block scalar even when the following line dedents."

    testCase "Combined explicit indent and strip chomping" <| fun _ ->
        let yaml = """data: |2-
  text
  more

"""
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("data"),
                YAMLElement.Value(YAMLContent.create("text\nmore", style=ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Strip, Some 2)))
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Combined explicit indent and strip chomping should parse correctly"

    testCase "Literal block scalar keeps the exact indentation of a more indented line" <| fun _ ->
        let yaml = "key: |\n    text\n     more\n"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(YAMLContent.create("key"), literalBlock "text\n more\n")
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "One extra space in the source should stay one extra space in the value"

    testCase "Literal block scalar keeps a more indented hash line as content" <| fun _ ->
        let yaml = "key: |\n    text\n     # more\n"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(YAMLContent.create("key"), literalBlock "text\n # more\n")
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "A hash line inside the block is content and keeps its extra space"

    testCase "Literal block scalar keeps two spaces before a deeper hash line" <| fun _ ->
        let yaml = "key: |\n    text\n      # two\n"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(YAMLContent.create("key"), literalBlock "text\n  # two\n")
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Two extra spaces in the source should stay two extra spaces in the value"

    testCase "Folded block scalar keeps the line breaks around a more indented line" <| fun _ ->
        let yaml = "key: >\n    text\n     more\n    end\n"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(YAMLContent.create("key"), blockScalar BlockScalarStyle.Folded ChompingMode.Clip None "text\n more\nend\n")
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "A more indented line is not folded and keeps its extra space"

    testCase "Literal block scalar keeps mixed indentation depths with hash lines" <| fun _ ->
        let yaml = "key: |\n    a\n     # b\n         c\n       # d\n    e\n"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(YAMLContent.create("key"), literalBlock "a\n # b\n     c\n   # d\ne\n")
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Every line should keep its indentation relative to the least indented line"

    testCase "Nested mapping block scalar keeps indentation and the next key" <| fun _ ->
        let yaml = "outer:\n  key: |\n    x\n     y\n  next: 1\n"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("outer"),
                YAMLElement.Object [
                    YAMLElement.Mapping(YAMLContent.create("key"), literalBlock "x\n y\n")
                    YAMLElement.Mapping(YAMLContent.create("next"), YAMLElement.Object [YAMLElement.Value(YAMLContent.create("1"))])
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "A nested block scalar should end at the dedented sibling key"

    testCase "Root block scalar keeps the exact indentation of a more indented line" <| fun _ ->
        let yaml = "|\n  x\n   y\n"
        let expected = YAMLElement.Object [literalBlock "x\n y\n"]
        let actual = Reader.read yaml
        Expect.equal actual expected "A root block scalar should keep one extra space"

    testCase "First sequence item block scalar keeps indentation and the next item" <| fun _ ->
        let yaml = "- |\n  x\n   y\n- z\n"
        let expected = YAMLElement.Object [
            YAMLElement.Sequence [
                YAMLElement.Object [literalBlock "x\n y\n"]
                YAMLElement.Object [YAMLElement.Value(YAMLContent.create("z"))]
            ]
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "The first item should keep one extra space and the second item should stay a plain value"

    testCase "Later sequence item block scalars keep their indentation" <| fun _ ->
        let yaml = "- a\n- |\n  x\n   y\n- b\n"
        let expected = YAMLElement.Object [
            YAMLElement.Sequence [
                YAMLElement.Object [YAMLElement.Value(YAMLContent.create("a"))]
                YAMLElement.Object [literalBlock "x\n y\n"]
                YAMLElement.Object [YAMLElement.Value(YAMLContent.create("b"))]
            ]
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "A block scalar after the first item should read as one scalar"

    testCase "Indentless sequence under a key keeps block scalar indentation" <| fun _ ->
        let yaml = "key:\n- |\n  x\n   y\n- z\n"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("key"),
                YAMLElement.Object [
                    YAMLElement.Sequence [
                        YAMLElement.Object [literalBlock "x\n y\n"]
                        YAMLElement.Object [YAMLElement.Value(YAMLContent.create("z"))]
                    ]
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "The first item of an indentless sequence should read as one scalar"

    testCase "Explicit indentation indicator keeps spaces beyond the indicated baseline" <| fun _ ->
        let yaml = "key: |2\n     x\n   y\n"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("key"),
                blockScalar BlockScalarStyle.Literal ChompingMode.Clip (Some 2) "   x\n y\n"
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "The indicator sets the baseline even when the first line is deeper than a later line"

    testCase "Keep chomping retains trailing blank lines before a dedented sibling" <| fun _ ->
        let yaml = "a: |+\n  x\n\n\nb: 1\n"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(YAMLContent.create("a"), blockScalar BlockScalarStyle.Literal ChompingMode.Keep None "x\n\n\n")
            YAMLElement.Mapping(YAMLContent.create("b"), YAMLElement.Object [YAMLElement.Value(YAMLContent.create("1"))])
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Both trailing blank lines belong to the scalar and the sibling key stays separate"

    testCase "Keep chomping retains trailing blank lines of a nested scalar" <| fun _ ->
        let yaml = "outer:\n  key: |+\n    x\n\nnext: 1\n"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("outer"),
                YAMLElement.Object [
                    YAMLElement.Mapping(YAMLContent.create("key"), blockScalar BlockScalarStyle.Literal ChompingMode.Keep None "x\n\n")
                ]
            )
            YAMLElement.Mapping(YAMLContent.create("next"), YAMLElement.Object [YAMLElement.Value(YAMLContent.create("1"))])
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "A blank line before a dedent to an outer key still belongs to the nested scalar"

    testCase "Header-looking body text stays literal content" <| fun _ ->
        let yaml = "key: |\n  a: |\n    b\n  - c\n"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(YAMLContent.create("key"), literalBlock "a: |\n  b\n- c\n")
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "Text that looks like a header or a sequence item inside a body is not interpreted"

    testCase "Preprocessing keeps a block scalar body flat with only the header indentation removed" <| fun _ ->
        let yaml = "outer:\n  key: |\n    text\n     # more\n       deeper\n  next: 1\n"
        let expected =
            Level [
                Line "outer:"
                Intendation [
                    Line "key: |"
                    Intendation [Line "  text"; Line "   # more"; Line "     deeper"]
                    Line "next: 1"
                ]
            ]
        let actual = Preprocessing.read yaml
        Expect.equal actual.AST expected "The body should be one Intendation of lines without nested blocks"

    testCase "Comment less indented than the content ends a nested block scalar" <| fun _ ->
        // yaml@2.6.1: {"a":{"b":"x\n","d":1}}
        let yaml = "a:\n  b: |\n      x\n    # c\n  d: 1\n"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("a"),
                YAMLElement.Object [
                    YAMLElement.Mapping(YAMLContent.create("b"), literalBlock "x\n")
                    YAMLElement.Comment(" c")
                    YAMLElement.Mapping(YAMLContent.create("d"), plainNode "1")
                ]
            )
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "A comment between the header and the content indentation is a comment, not content"

    testCase "Comment less indented than an explicit indicator ends the block scalar" <| fun _ ->
        // yaml@2.6.1: {"a":"  x\n","b":1}
        let yaml = "a: |2\n    x\n # c\nb: 1\n"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(YAMLContent.create("a"), blockScalar BlockScalarStyle.Literal ChompingMode.Clip (Some 2) "  x\n")
            YAMLElement.Comment(" c")
            YAMLElement.Mapping(YAMLContent.create("b"), plainNode "1")
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "The indicator sets the content indentation, so a comment at one space ends the scalar"

    testCase "Comment less indented than the content ends a sequence item block scalar" <| fun _ ->
        // yaml@2.6.1: ["x\n","y"]
        let yaml = "- |\n    x\n  # c\n- y\n"
        let expected = YAMLElement.Object [
            YAMLElement.Sequence [
                YAMLElement.Object [literalBlock "x\n"]
                YAMLElement.Comment(" c")
                plainNode "y"
            ]
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "The comment ends the scalar and stays between the sequence items"

    testCase "Comment less indented than the content ends a folded block scalar" <| fun _ ->
        // yaml@2.6.1: {"key":"folded\n","next":1}
        let yaml = "key: >\n    folded\n  # c\nnext: 1\n"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(YAMLContent.create("key"), blockScalar BlockScalarStyle.Folded ChompingMode.Clip None "folded\n")
            YAMLElement.Comment(" c")
            YAMLElement.Mapping(YAMLContent.create("next"), plainNode "1")
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "The content indentation is the one of the first line, not of the header"

    testCase "Trailing comment between the header and the content indentation ends the block scalar" <| fun _ ->
        // yaml@2.6.1: {"x":{"a":"body\n"}} and {"a":"x\n"}
        let nested = "x:\n  a: |\n      body\n    # c\n"
        let expectedNested = YAMLElement.Object [
            YAMLElement.Mapping(
                YAMLContent.create("x"),
                YAMLElement.Object [
                    YAMLElement.Mapping(YAMLContent.create("a"), literalBlock "body\n")
                    YAMLElement.Comment(" c")
                ]
            )
        ]
        Expect.equal (Reader.read nested) expectedNested "A trailing comment stays in the block of the header"
        let root = "a: |\n    x\n  # c\n"
        let expectedRoot = YAMLElement.Object [
            YAMLElement.Mapping(YAMLContent.create("a"), literalBlock "x\n")
            YAMLElement.Comment(" c")
        ]
        Expect.equal (Reader.read root) expectedRoot "A trailing comment must not be read as scalar content"

    testCase "Keep chomping stops before a comment that is less indented than the content" <| fun _ ->
        // yaml@2.6.1: {"a":"x\n\n","b":1}
        let yaml = "a: |+\n    x\n\n  # c\nb: 1\n"
        let expected = YAMLElement.Object [
            YAMLElement.Mapping(YAMLContent.create("a"), blockScalar BlockScalarStyle.Literal ChompingMode.Keep None "x\n\n")
            YAMLElement.Comment(" c")
            YAMLElement.Mapping(YAMLContent.create("b"), plainNode "1")
        ]
        let actual = Reader.read yaml
        Expect.equal actual expected "The blank line before the comment belongs to the scalar and the comment does not"

    unitTests
    scalingTests
  ]
