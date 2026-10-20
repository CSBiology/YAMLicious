module Tests.Tokenizer

open Fable.Pyxpecto
open System.Collections.Generic
open YAMLicious
open YAMLicious.YAMLiciousTypes

let private code = Persil.LineKind.Code
let private blank = Persil.LineKind.Blank
let private commentOnly = Persil.LineKind.CommentOnly
let private directive = Persil.LineKind.Directive
let private start = Persil.LineKind.DocumentStart
let private finish = Persil.LineKind.DocumentEnd
let private header = Persil.LineKind.BlockHeader
let private body = Persil.LineKind.BlockBody

let private single (value: string) : StringMapEntry = { Value = value; Kind = QuotedStringKind.SingleQuotedString }
let private dquote (value: string) : StringMapEntry = { Value = value; Kind = QuotedStringKind.DoubleQuotedString }
let private plainEntry (value: string) : StringMapEntry = { Value = value; Kind = QuotedStringKind.PlainScalar }

let private kindsOf (tokenized: Persil.TokenizedYaml) =
    tokenized.Lines |> Array.map (fun line -> line.Kind) |> List.ofArray

let private blockIdsOf (tokenized: Persil.TokenizedYaml) =
    tokenized.Lines |> Array.map (fun line -> line.BlockId) |> List.ofArray

let private stringsOf (tokenized: Persil.TokenizedYaml) =
    [ for entry in tokenized.StringMap -> entry.Key, entry.Value ]

let private commentsOf (tokenized: Persil.TokenizedYaml) =
    [ for entry in tokenized.CommentMap -> entry.Key, entry.Value ]

/// Tokenizes and asserts the text and the line kinds. Returns the result for more assertions.
let private expectLines (input: string) (text: string) (kinds: Persil.LineKind list) =
    let actual = Persil.tokenize input
    Expect.equal (tokenizedText actual) text "text"
    Expect.equal (kindsOf actual) kinds "line kinds"
    actual

let private block (style: BlockScalarStyle) (chomp: ChompingMode) (indent: int option) (value: string) =
    YAMLElement.Value(YAMLContent.create(value, style=ScalarStyle.Block(style, chomp, indent)))

let private literal (value: string) = block BlockScalarStyle.Literal ChompingMode.Clip None value

let private plainNode (value: string) =
    YAMLElement.Object [YAMLElement.Value(YAMLContent.create(value))]

let private quotedNode (style: ScalarStyle) (value: string) =
    YAMLElement.Object [YAMLElement.Value(YAMLContent.create(value, style=style))]

let private mapping (key: string) (value: YAMLElement) =
    YAMLElement.Mapping(YAMLContent.create(key), value)

let private expectHeader (code: string) (expected: (BlockScalarStyle * int option * ChompingMode * int) option) =
    let actual =
        Syntax.BlockScalar.tryParseHeaderCode code
        |> Option.map (fun found -> found.Header.Style, found.Header.Indent, found.Header.Chomp, found.Baseline)
    Expect.equal actual expected (sprintf "header of %A" code)

/// The message of the exception that the action throws, or an empty string when it succeeds.
let private errorOf (action: unit -> unit) =
    try
        action ()
        ""
    with e ->
        e.Message

/// Every key and value of the tree, so that a test can look for placeholder text in them.
let rec private textsOf (element: YAMLElement) : string list =
    match element with
    | YAMLElement.Value content -> [ content.Value ]
    | YAMLElement.Mapping (key, value) -> key.Value :: textsOf value
    | YAMLElement.Object items
    | YAMLElement.Sequence items -> items |> List.collect textsOf
    | _ -> []

let private expectNoPlaceholderText (element: YAMLElement) =
    for text in textsOf element do
        Expect.isFalse (text.Contains "<c f=" || text.Contains "<s f=") $"No placeholder text in the value {text}"

let private doubleQuotedKey (value: string) = YAMLContent.create(value, style = ScalarStyle.DoubleQuoted)

/// A list of lines with unclosed quotes between valid scalars and comments. Each group of six lines
/// holds two strings and one comment.
let private malformedInput (lines: int) =
    let sb = System.Text.StringBuilder()
    for i in 0 .. lines - 1 do
        let text =
            match i % 6 with
            | 0 -> sprintf "k%d: 'unclosed %d" i i
            | 1 -> sprintf "k%d: \"unclosed %d" i i
            | 2 -> sprintf "k%d: 'ok %d' # c%d" i i i
            | 3 -> sprintf "k%d: \"bad\" tail %d" i i
            | 4 -> sprintf "k%d: \"ok %d\"" i i
            | _ -> sprintf "k%d: plain %s" i (System.String('x', 120))
        sb.Append(text).Append("\n") |> ignore
    sb.ToString()

/// Runs the action repeatedly for a fixed time and returns the milliseconds of one run. Fable has no
/// Stopwatch, so the clock is the system time, and a window of 50 ms holds many runs of a short action.
let private sampleMilliseconds (action: unit -> unit) =
    let startTime = System.DateTime.UtcNow
    let mutable runs = 0
    let mutable elapsed = 0.0
    while elapsed < 50.0 do
        action ()
        runs <- runs + 1
        elapsed <- (System.DateTime.UtcNow - startTime).TotalMilliseconds
    elapsed / float runs

/// The lines of the small and the large input in the scaling test. Fable Python runs far slower than
/// the other targets, so it keeps a shorter large input to hold the time of the suite down.
let private smallLines = 2000
#if FABLE_COMPILER_PYTHON
let private largeLines = 16000
#else
let private largeLines = 32000
#endif

/// The reference of the scaling test, a single pass over the characters of the input. The count goes
/// to a cell that outlives the call, so the compiler cannot drop the loop.
let private lineBreakCount = ref 0

let private countLineBreaks (input: string) =
    let mutable count = 0
    let mutable i = 0
    while i < input.Length do
        if input.[i] = '\n' then count <- count + 1
        i <- i + 1
    lineBreakCount.Value <- count

/// Compares how the time of `operation` grows from the small to the large input with how the time of
/// `reference`, a linear pass, grows on the same inputs. A linear operation scores about 1 and a quadratic
/// one about the ratio of the two sizes. The reference absorbs the cost that a large input pays for the
/// garbage collector and the caches. Every round measures the four series one after the other, so a load
/// on the machine hits the operation and the reference alike. The first round warms up the code and is
/// dropped. The fastest of the five rounds after it keeps a pause of the garbage collector out of the result.
let private expectLinear (name: string) (operation: string -> unit) (reference: string -> unit) (build: int -> string) =
    let smallInput = build smallLines
    let largeInput = build largeLines
    let actions: (unit -> unit)[] = [|
        fun () -> operation smallInput
        fun () -> operation largeInput
        fun () -> reference smallInput
        fun () -> reference largeInput
    |]
    let fastest = Array.create actions.Length infinity
    for round in 0 .. 5 do
        for i in 0 .. actions.Length - 1 do
            let milliseconds = sampleMilliseconds actions.[i]
            if round > 0 then fastest.[i] <- min fastest.[i] milliseconds
    let operationRatio = fastest.[1] / fastest.[0]
    let referenceRatio = fastest.[3] / fastest.[2]
    let normalized = operationRatio / referenceRatio
    // The limit is the geometric mean of 1 (linear) and the size ratio (quadratic), the middle on a log scale.
    let limit = sqrt (float largeLines / float smallLines)
    Expect.isTrue
        (normalized < limit)
        (sprintf "%s took %.3f ms for %d lines and %.3f ms for %d lines, a ratio of %.2f. The reference took %.3f ms and %.3f ms, a ratio of %.2f. Normalized: %.2f, limit: %.2f"
            name fastest.[0] smallLines fastest.[1] largeLines operationRatio fastest.[2] fastest.[3] referenceRatio normalized limit)

/// A mapping whose first value is a plain scalar of the given number of continuation lines.
let private plainScalarInput (lines: int) =
    let sb = System.Text.StringBuilder()
    sb.Append("k: start\n") |> ignore
    for i in 0 .. lines - 1 do
        sb.Append(sprintf "  line %d\n" i) |> ignore
    sb.Append("j: 1\n") |> ignore
    sb.ToString()

let private headerTests = testList "Block scalar header" [
    testCase "standalone, mapping value, sequence entry and explicit key positions" <| fun _ ->
        expectHeader "|" (Some (BlockScalarStyle.Literal, None, ChompingMode.Clip, 0))
        expectHeader "a: >-" (Some (BlockScalarStyle.Folded, None, ChompingMode.Strip, 0))
        expectHeader "  key: |2" (Some (BlockScalarStyle.Literal, Some 2, ChompingMode.Clip, 2))
        expectHeader "  - |2" (Some (BlockScalarStyle.Literal, Some 2, ChompingMode.Clip, 2))
        expectHeader "- >+" (Some (BlockScalarStyle.Folded, None, ChompingMode.Keep, 0))
        expectHeader "- - |" (Some (BlockScalarStyle.Literal, None, ChompingMode.Clip, 2))
        expectHeader "? |" (Some (BlockScalarStyle.Literal, None, ChompingMode.Clip, 0))
        expectHeader ": >" (Some (BlockScalarStyle.Folded, None, ChompingMode.Clip, 0))
        expectHeader "a: |   " (Some (BlockScalarStyle.Literal, None, ChompingMode.Clip, 0))
        expectHeader "a: |1-" (Some (BlockScalarStyle.Literal, Some 1, ChompingMode.Strip, 0))

    testCase "a document start marker may carry the header" <| fun _ ->
        expectHeader "--- |" (Some (BlockScalarStyle.Literal, None, ChompingMode.Clip, 0))
        expectHeader "--- >-" (Some (BlockScalarStyle.Folded, None, ChompingMode.Strip, 0))
        expectHeader "--- !!str |2" (Some (BlockScalarStyle.Literal, Some 2, ChompingMode.Clip, 0))
        expectHeader "---" None
        expectHeader "---|" None

    testCase "properties are whole tokens and a verbatim tag may contain colons" <| fun _ ->
        let literalHeader = Some (BlockScalarStyle.Literal, None, ChompingMode.Clip, 0)
        expectHeader "k: !<tag:yaml.org,2002:str> |" literalHeader
        expectHeader "- !<tag:yaml.org,2002:str> |" literalHeader
        expectHeader "!<tag:yaml.org,2002:str> |" literalHeader
        expectHeader "k: &a !!str |" literalHeader
        expectHeader "&a |" literalHeader
        expectHeader "!tag a: |" literalHeader
        expectHeader "&a k: !<x:y> |" literalHeader

    testCase "the mapping separator is structural" <| fun _ ->
        let literalHeader = Some (BlockScalarStyle.Literal, None, ChompingMode.Clip, 0)
        expectHeader "<s f=0/>: |" literalHeader
        expectHeader "a:b: |" literalHeader
        expectHeader "http://x.y/z: |" literalHeader
        expectHeader "url: http://x.y/z |" None
        expectHeader "a:|" None

    testCase "the header must end the code" <| fun _ ->
        expectHeader "a: | extra" None
        expectHeader "a: |x" None
        expectHeader "a: b |" None
        expectHeader "text: a|b" None
        expectHeader "a: >1-2" None
        expectHeader "a: |-+" None

    testCase "a flow collection never holds a header" <| fun _ ->
        expectHeader "a: [|" None
        expectHeader "[a, b: |" None
        expectHeader "{a: |" None
        expectHeader "a: { b: >" None

    testCase "a compact mapping reports the column of its key" <| fun _ ->
        expectHeader "- key: |2" (Some (BlockScalarStyle.Literal, Some 2, ChompingMode.Clip, 2))
        expectHeader "  - key: |" (Some (BlockScalarStyle.Literal, None, ChompingMode.Clip, 4))
        expectHeader "- &a key: |" (Some (BlockScalarStyle.Literal, None, ChompingMode.Clip, 2))

    testCase "a header behind entry indicators reports the column of the last indicator" <| fun _ ->
        expectHeader "- - |2" (Some (BlockScalarStyle.Literal, Some 2, ChompingMode.Clip, 2))
        expectHeader "- ? |" (Some (BlockScalarStyle.Literal, None, ChompingMode.Clip, 2))
        expectHeader "  - !!str |" (Some (BlockScalarStyle.Literal, None, ChompingMode.Clip, 2))
        expectHeader "- &a |2" (Some (BlockScalarStyle.Literal, Some 2, ChompingMode.Clip, 0))
]

let private blockScannerTests = testList "Block scalars in the scanner" [
    testCase "an indentation indicator counts from the key of a compact mapping" <| fun _ ->
        // yaml@2.6.1: [{"key":"x\n","other":1}], and the input with the body at 3 spaces is invalid (BAD_INDENT)
        expectLines "- key: |2\n    x\n  other: 1\n" "- key: |2\n    x\n  other: 1\n" [header; body; code; blank] |> ignore
        expectLines "- key: |2\n   x\n" "- key: |2\n   x\n" [header; code; blank] |> ignore

    testCase "an indentation indicator counts from the dash of a sequence item" <| fun _ ->
        // yaml@2.6.1: [" x\ny\n"] and [" x\n","z"]
        expectLines "- |2\n   x\n  y\n" "- |2\n   x\n  y\n" [header; body; body; blank] |> ignore
        expectLines "- |2\n   x\n- z\n" "- |2\n   x\n- z\n" [header; body; code; blank] |> ignore

    testCase "an indentation indicator counts from the last dash of a nested item" <| fun _ ->
        // yaml@2.6.1: [["x\n","y"]] for the first input. The second input is invalid (MISSING_CHAR).
        expectLines "- - |2\n    x\n  - y\n" "- - |2\n    x\n  - y\n" [header; body; code; blank] |> ignore
        expectLines "- - |\n  x\n" "- - |\n  x\n" [header; code; blank] |> ignore

    testCase "a quoted key with a hash and a colon opens a block" <| fun _ ->
        let actual =
            expectLines
                "\"a #b\": |\n  x # y\n  'z'\n\"c: d\": >\n  'e'\n"
                "<s f=0/>: |\n  x # y\n  'z'\n<s f=1/>: >\n  'e'\n"
                [header; body; body; header; body; blank]
        Expect.equal (blockIdsOf actual) [0; 0; 0; 1; 1; -1] "block ids"
        Expect.equal (stringsOf actual) [0, dquote "a #b"; 1, dquote "c: d"] "only the keys are strings"
        Expect.equal actual.CommentMap.Count 0 "a hash in a body is no comment"

    testCase "a comment hides a header" <| fun _ ->
        let actual =
            expectLines
                "- # note: |\n  k: 'it''s'\n"
                "- <c f=0/>\n  k: <s f=0/>\n"
                [code; code; blank]
        Expect.equal (stringsOf actual) [0, single "it's"] "the line after a commented-out header is read"
        Expect.equal (commentsOf actual) [0, " note: |"] "comment"

    testCase "a header is found behind a trailing comment" <| fun _ ->
        let actual =
            expectLines
                "a: | # note\n  'x'\nb: 'y'\n"
                "a: | <c f=0/>\n  'x'\nb: <s f=0/>\n"
                [header; body; code; blank]
        Expect.equal (stringsOf actual) [0, single "y"] "strings"

    testCase "properties before a header" <| fun _ ->
        let actual =
            expectLines
                "k: !<tag:yaml.org,2002:str> |\n  'a'\nl: &x !!str >\n  \"b\"\n- !<t:u> |\n  'c'\nm: 'd'\n"
                "k: !<tag:yaml.org,2002:str> |\n  'a'\nl: &x !!str >\n  \"b\"\n- !<t:u> |\n  'c'\nm: <s f=0/>\n"
                [header; body; header; body; header; body; code; blank]
        Expect.equal (blockIdsOf actual) [0; 0; 1; 1; 2; 2; -1; -1] "block ids"

    testCase "a header ends the block of the previous header on the same line" <| fun _ ->
        let actual =
            expectLines
                "a: |\n  x\nb: >\n  y\n"
                "a: |\n  x\nb: >\n  y\n"
                [header; body; header; body; blank]
        Expect.equal (blockIdsOf actual) [0; 0; 1; 1; -1] "block ids"

    testCase "explicit indentation indicator" <| fun _ ->
        let actual =
            expectLines
                "a: |2\n    x\n  y\n c\nb: 'q'\n"
                "a: |2\n    x\n  y\n c\nb: <s f=0/>\n"
                [header; body; body; code; code; blank]
        Expect.equal (blockIdsOf actual) [0; 0; 0; -1; -1; -1] "a line indented less than the indicator ends the block"

    testCase "the first nonblank line sets the content indentation" <| fun _ ->
        // `  y` is less indented than the first line, so it ends the block. It is read as a line of its own.
        let actual =
            expectLines
                "a: |\n    x\n  y: 'z'\nb: 'q'\n"
                "a: |\n    x\n  y: <s f=0/>\nb: <s f=1/>\n"
                [header; body; code; code; blank]
        Expect.equal (stringsOf actual) [0, single "z"; 1, single "q"] "strings"

    testCase "a comment between the header indentation and the content indentation ends the block" <| fun _ ->
        let actual =
            expectLines
                "a: |\n    x\n  # c\nb: 1\n"
                "a: |\n    x\n  <c f=0/>\nb: 1\n"
                [header; body; commentOnly; code; blank]
        Expect.equal (commentsOf actual) [0, " c"] "comment"

    testCase "blank lines belong to the body and do not set the indentation" <| fun _ ->
        let actual =
            expectLines
                "a: |\n\n  x\n\n   \nb: 'q'\n\n"
                "a: |\n\n  x\n\n   \nb: <s f=0/>\n\n"
                [header; body; body; body; body; code; blank; blank]
        Expect.equal (blockIdsOf actual) [0; 0; 0; 0; 0; -1; -1; -1] "the blank lines before and after the content are body lines"

    testCase "a tab after the indentation is content" <| fun _ ->
        let actual =
            expectLines
                "a: |\n  \tx 'q'\n  y\nb: 'z'\n"
                "a: |\n  \tx 'q'\n  y\nb: <s f=0/>\n"
                [header; body; body; code; blank]
        Expect.equal (stringsOf actual) [0, single "z"] "strings"

    testCase "nested sequence headers and explicit key positions" <| fun _ ->
        let actual =
            expectLines
                "- |\n  a 'b'\n- >\n  c \"d\"\n- 'e'\n? |\n  f 'g'\n: |\n  h 'i'\nj: 'k'\n"
                "- |\n  a 'b'\n- >\n  c \"d\"\n- <s f=0/>\n? |\n  f 'g'\n: |\n  h 'i'\nj: <s f=1/>\n"
                [header; body; header; body; code; header; body; header; body; code; blank]
        Expect.equal (blockIdsOf actual) [0; 0; 1; 1; -1; 2; 2; 3; 3; -1; -1] "block ids"

    testCase "header-looking text in a body is literal" <| fun _ ->
        let actual =
            expectLines
                "a: |\n  b: |\n    'x'\n  - c: 'y'\nd: 'z'\n"
                "a: |\n  b: |\n    'x'\n  - c: 'y'\nd: <s f=0/>\n"
                [header; body; body; body; code; blank]
        Expect.equal (blockIdsOf actual) [0; 0; 0; 0; -1; -1] "one block"
        Expect.equal (stringsOf actual) [0, single "z"] "strings"

    testCase "an unbalanced bracket at the start of a continuation line does not hide the blocks after it" <| fun _ ->
        // yaml@2.6.1: {"desc":"see the list [draft","script":"echo 'hi'\n","next":{"run":"x # y\n"}}
        let actual =
            expectLines
                "desc: see the list\n  [draft\nscript: |\n  echo 'hi'\nnext:\n  run: >\n    x # y\n"
                "desc: <s f=0/>\nscript: |\n  echo 'hi'\nnext:\n  run: >\n    x # y\n"
                [code; header; body; code; header; body; blank]
        Expect.equal (blockIdsOf actual) [-1; 0; 0; -1; 1; 1; -1] "both headers open a block"
        Expect.equal (stringsOf actual) [0, plainEntry "see the list\n[draft"] "the bracket is part of the scalar, and the bodies hold no placeholder"
        Expect.equal actual.CommentMap.Count 0 "the hash in the second body is text"

    testCase "an unbalanced brace or a dash and bracket on a continuation line does not hide the block after it" <| fun _ ->
        // yaml@2.6.1: {"desc":"see the list {bar","script":"echo 'hi'\n"} and {"desc":"see the list - [bar","script":"echo 'hi'\n"}
        for continuation in [ "  {bar"; "  - [bar" ] do
            let input = "desc: see the list\n" + continuation + "\nscript: |\n  echo 'hi'\n"
            let actual = expectLines input "desc: <s f=0/>\nscript: |\n  echo 'hi'\n" [code; header; body; blank]
            Expect.equal (blockIdsOf actual) [-1; 0; 0; -1] $"the header after {continuation.Trim()} opens a block"
            Expect.equal (stringsOf actual) [0, plainEntry ("see the list\n" + continuation.Trim())] "the scalar"

    testCase "an unbalanced bracket in a sequence item does not hide the block after it" <| fun _ ->
        // yaml@2.6.1: ["desc [bar","echo 'hi'\n"]
        let actual = expectLines "- desc\n  [bar\n- |\n  echo 'hi'\n" "- <s f=0/>\n- |\n  echo 'hi'\n" [code; header; body; blank]
        Expect.equal (blockIdsOf actual) [-1; 0; 0; -1] "the header opens a block"
        Expect.equal (stringsOf actual) [0, plainEntry "desc\n[bar"] "the scalar"

    testCase "a flow mapping with a header-looking value opens no block" <| fun _ ->
        let actual =
            expectLines
                "a: {b: |\n}\nc: 'x'\n"
                "a: {b: |\n}\nc: <s f=0/>\n"
                [code; code; code; blank]
        Expect.equal (stringsOf actual) [0, single "x"] "strings"

    testCase "quoted bars in a multiline flow collection are strings and the block after it opens" <| fun _ ->
        let actual =
            expectLines
                "a: [\n  '|', # one\n  \">\"\n]\nb: |\n  \"q\"\n"
                "a: [\n  <s f=0/>, <c f=0/>\n  <s f=1/>\n]\nb: |\n  \"q\"\n"
                [code; code; code; code; header; body; blank]
        Expect.equal (stringsOf actual) [0, single "|"; 1, dquote ">"] "the bars are strings"
        Expect.equal (commentsOf actual) [0, " one"] "comment"

    testCase "brackets in plain text do not hide the block scalars after them" <| fun _ ->
        let actual =
            expectLines
                "a: foo [bar\nb: |\n  x\nc: x {y\nd: >\n  z\n"
                "a: foo [bar\nb: |\n  x\nc: x {y\nd: >\n  z\n"
                [code; header; body; code; header; body; blank]
        Expect.equal (blockIdsOf actual) [-1; 0; 0; -1; 1; 1; -1] "block ids"

    testCase "a multiline flow collection behind a tag reads its quotes and the block after it opens" <| fun _ ->
        let actual =
            expectLines
                "a: !!seq [\n  'b',\n  \"c\"\n]\nd: |\n  'x'\n"
                "a: !!seq [\n  <s f=0/>,\n  <s f=1/>\n]\nd: |\n  'x'\n"
                [code; code; code; code; header; body; blank]
        Expect.equal (stringsOf actual) [0, single "b"; 1, dquote "c"] "strings"

    testCase "text behind the bar means there is no header" <| fun _ ->
        let actual =
            expectLines
                "a: | extra\n  \"q\"\n"
                "a: | extra\n  <s f=0/>\n"
                [code; code; blank]
        Expect.equal (stringsOf actual) [0, dquote "q"] "the line below is not a body"

    testCase "line endings are normalized once" <| fun _ ->
        let actual = Persil.tokenize "a: 'x\r\n  y'\r\nb: |\r\n  \"q\"\rc: \"z\"\r\n"
        Expect.equal (tokenizedText actual) "a: <s f=0/>\nb: |\n  \"q\"\nc: <s f=1/>\n" "text"
        Expect.equal (kindsOf actual) [code; header; body; code; blank] "line kinds"

    testCase "a block at the end of the input" <| fun _ ->
        let atHeader = expectLines "a: |" "a: |" [header]
        Expect.equal (blockIdsOf atHeader) [0] "block ids"
        let withBody = expectLines "a: |\n  x" "a: |\n  x" [header; body]
        Expect.equal (blockIdsOf withBody) [0; 0] "block ids"
        expectLines "a: |\n" "a: |\n" [header; blank] |> ignore
        expectLines "a: |\nx\n" "a: |\nx\n" [header; code; blank] |> ignore

    testCase "a document marker ends a block" <| fun _ ->
        let actual =
            expectLines
                "a: |\n  x\n---\nb: 'q'\n"
                "a: |\n  x\n---\nb: <s f=0/>\n"
                [header; body; start; code; blank]
        Expect.equal (blockIdsOf actual) [0; 0; -1; -1; -1] "block ids"

    testCase "a body keeps text that looks like markers and directives" <| fun _ ->
        let actual =
            expectLines
                "a: |\n  ---\n  ...\n  %YAML 1.2\nb: 1\n"
                "a: |\n  ---\n  ...\n  %YAML 1.2\nb: 1\n"
                [header; body; body; body; code; blank]
        Expect.equal actual.CommentMap.Count 0 "no comments"

    testCase "a body is copied without placeholders and the maps ignore it" <| fun _ ->
        let actual =
            expectLines
                "a: |\n  it's \"x\" # y\n  'z' ### w\nb: 'q' # c\n"
                "a: |\n  it's \"x\" # y\n  'z' ### w\nb: <s f=0/> <c f=0/>\n"
                [header; body; body; code; blank]
        Expect.equal (stringsOf actual) [0, single "q"] "only the string behind the block"
        Expect.equal (commentsOf actual) [0, " c"] "only the comment behind the block"

    testCase "placeholder spellings in a body stay text" <| fun _ ->
        let actual =
            expectLines
                "a: |\n  <s f=0/>\n  <c f=0/>\nb: 'q'\n"
                "a: |\n  <s f=0/>\n  <c f=0/>\nb: <s f=0/>\n"
                [header; body; body; code; blank]
        Expect.equal (stringsOf actual) [0, single "q"] "the only string"
        Expect.equal actual.CommentMap.Count 0 "no comment"
        Expect.equal (actual.Lines |> Array.map (fun line -> line.Text) |> Array.item 1) "  <s f=0/>" "the body line is verbatim"
]

let private documentScannerTests = testList "Directives and document markers in the scanner" [
    testCase "a comment before a directive" <| fun _ ->
        expectLines "# lead\n%YAML 1.2\n---\na: 1\n" "<c f=0/>\n%YAML 1.2\n---\na: 1\n" [commentOnly; directive; start; code; blank] |> ignore

    testCase "a comment between a directive and the marker" <| fun _ ->
        expectLines "%YAML 1.2\n# lead\n---\na: 1\n" "%YAML 1.2\n<c f=0/>\n---\na: 1\n" [directive; commentOnly; start; code; blank] |> ignore

    testCase "a comment before the marker" <| fun _ ->
        expectLines "# lead\n---\na: 1\n" "<c f=0/>\n---\na: 1\n" [commentOnly; start; code; blank] |> ignore

    testCase "a comment between an end marker and the next start marker" <| fun _ ->
        expectLines "a: 1\n...\n# c\n---\nb: 2\n" "a: 1\n...\n<c f=0/>\n---\nb: 2\n" [code; finish; commentOnly; start; code; blank] |> ignore

    testCase "a comment behind the start marker" <| fun _ ->
        expectLines "--- # c\na: 1\n" "--- <c f=0/>\na: 1\n" [start; code; blank] |> ignore

    testCase "a block scalar behind the start marker" <| fun _ ->
        let actual = expectLines "--- |\n  x 'q'\n" "--- |\n  x 'q'\n" [start; body; blank]
        Expect.equal (blockIdsOf actual) [0; 0; -1] "the marker line carries the block id"
        Expect.equal actual.StringMap.Count 0 "no string"

    testCase "quoted content behind the start marker" <| fun _ ->
        let actual = expectLines "--- 'x' # c\n--- !!str \"y\"\n" "--- <s f=0/> <c f=0/>\n--- !!str <s f=1/>\n" [start; start; blank]
        Expect.equal (stringsOf actual) [0, single "x"; 1, dquote "y"] "strings"

    testCase "a directive keeps quotes and a separated comment" <| fun _ ->
        let actual = expectLines "%TAG !e! tag:it's \"x\" # c\n---\n" "%TAG !e! tag:it's \"x\" <c f=0/>\n---\n" [directive; start; blank]
        Expect.equal actual.StringMap.Count 0 "no string in a directive"
        Expect.equal (commentsOf actual) [0, " c"] "comment"

    testCase "a percent sign in content is no directive" <| fun _ ->
        // yaml@2.6.1 rejects the first two inputs with BAD_SCALAR_START and reads the third as {"a":"b\n%TAG\n"}
        for input in [ "a: 1\n%YAML 1.2\n"; "a: |\n  b\n%TAG\n" ] do
            Expect.throws (fun () -> Persil.tokenize input |> ignore) $"{input} holds a plain scalar that starts with a percent sign"
        let inBody = expectLines "a: |\n  b\n  %TAG\n" "a: |\n  b\n  %TAG\n" [header; body; body; blank]
        Expect.equal (blockIdsOf inBody) [0; 0; 0; -1] "the percent sign inside a block body is content"

    testCase "a directive follows an end marker and every start marker ends the prelude" <| fun _ ->
        // yaml@2.6.1 reads the first input as {"a":1} and {"b":2} and rejects the others (BAD_SCALAR_START)
        expectLines "a: 1\n...\n%YAML 1.2\n---\nb: 2\n" "a: 1\n...\n%YAML 1.2\n---\nb: 2\n" [code; finish; directive; start; code; blank] |> ignore
        for input in [ "---\na: 1\n---\n%TAG ! t!\n---\nb: 2\n"; "---\n%TAG ! t!\n" ] do
            Expect.throws (fun () -> Persil.tokenize input |> ignore) $"{input} holds a plain scalar that starts with a percent sign"

    testCase "a plain scalar cannot start with a percent sign, an at sign or a backquote" <| fun _ ->
        // yaml@2.6.1 rejects all ten inputs with BAD_SCALAR_START
        let cases =
            [ ("[%bad]", 1, "%")
              ("{@a: 1}", 1, "@")
              ("- `x`", 1, "`")
              ("k: %x", 1, "%")
              ("- %x", 1, "%")
              ("? %x", 1, "%")
              ("[a, @b]", 1, "@")
              ("k: !!str %x", 1, "%")
              ("{a: %x}", 1, "%")
              ("a: 1\n%YAML 1.2\n---\nb: 2", 2, "%") ]
        for (input, line, character) in cases do
            Expect.stringContains
                (errorOf (fun () -> Persil.tokenize input |> ignore))
                $"Line {line}: a plain scalar cannot start with `{character}`."
                $"The error of {input}"
        Expect.throws (fun () -> Reader.read "a: 1\n%YAML 1.2\n---\nb: 2" |> ignore) "Reader.read"
        Expect.throws (fun () -> Reader.readDocuments "a: 1\n%YAML 1.2\n---\nb: 2" |> ignore) "Reader.readDocuments"

    testCase "the reserved characters are text inside a plain scalar and on a continuation line" <| fun _ ->
        // yaml@2.6.1: {"k":"a %x"}, {"k":"a@b"}, {"k":"x`y"} and {"k":"a %x"}
        for input in [ "k: a %x"; "k: a@b"; "k: x`y" ] do
            let actual = expectLines input input [code]
            Expect.equal actual.StringMap.Count 0 $"no string in {input}"
        let continuation = expectLines "k: a\n  %x" "k: <s f=0/>" [code]
        Expect.equal (stringsOf continuation) [0, plainEntry "a\n%x"] "a continuation line"

    testCase "a marker needs a separator behind it and sits at column 0" <| fun _ ->
        // The line `...foo` opens a plain scalar and the line ` --- x` continues it.
        expectLines "---a: 1\n...foo\n --- x\n--- a\n..." "---a: 1\n<s f=0/>\n--- a\n..." [code; code; start; finish] |> ignore
        expectLines "---\t\n---" "---\t\n---" [start; start] |> ignore

    testCase "a quote does not cross a marker" <| fun _ ->
        let inSingle = expectLines "a: 'x\n---\ny'\n" "a: 'x\n---\ny'\n" [code; start; code; blank]
        Expect.equal inSingle.StringMap.Count 0 "no string"
        let inDouble = expectLines "a: \"x\n...\ny\"\n---\nb: 'z'\n" "a: \"x\n...\ny\"\n---\nb: <s f=0/>\n" [code; finish; code; start; code; blank]
        Expect.equal (stringsOf inDouble) [0, single "z"] "the string behind the marker"
        let escaped = expectLines "a: \"x\\\n---\ny\"\n" "a: \"x\\\n---\ny\"\n" [code; start; code; blank]
        Expect.equal escaped.StringMap.Count 0 "an escaped line break does not hide the marker"

    testCase "a marker inside a quote that closes before it is no marker" <| fun _ ->
        let actual = expectLines "a: 'x\n  ---\n  y'\n" "a: <s f=0/>\n" [code; blank]
        Expect.equal (stringsOf actual) [0, single "x --- y"] "an indented marker is text"
]

let private quoteScannerTests = testList "Quotes in the scanner" [
    testCase "a quote behind a bracket in a plain scalar is text" <| fun _ ->
        // yaml@2.6.1: {"k":"o[\"k\"]"}, {"k":"$(o['k'] || [1, 2])"} and {"k":"$(inputs[\"my-input\"].path)"}
        for input in [ "k: o[\"k\"]"; "k: $(o['k'] || [1, 2])"; "k: $(inputs[\"my-input\"].path)" ] do
            let actual = expectLines input input [code]
            Expect.equal actual.StringMap.Count 0 $"no string in {input}"

    testCase "a quote behind a comma in a plain scalar is text" <| fun _ ->
        // yaml@2.6.1: {"k":"a, \"b\""}
        let actual = expectLines "k: a, \"b\"" "k: a, \"b\"" [code]
        Expect.equal actual.StringMap.Count 0 "the comma is text, so the quote is text"

    testCase "a quote behind a brace in a plain scalar is text" <| fun _ ->
        // yaml@2.6.1 rejects this input with BLOCK_AS_IMPLICIT_KEY, because the `: ` ends a plain scalar
        // that would be an implicit key. The quote sits inside that plain scalar.
        let actual = expectLines "k: x {\"a\": 1}" "k: x {\"a\": 1}" [code]
        Expect.equal actual.StringMap.Count 0 "no string"

    testCase "a quote in a single-line flow collection opens a scalar" <| fun _ ->
        // yaml@2.6.1: {"k":["a","b"]}, {"k":{"a":"b"}}, {"k":["x","y, z"]} and [["a","b"]]
        let cases =
            [ ("k: [\"a\", 'b']", "k: [<s f=0/>, <s f=1/>]", [ (0, dquote "a"); (1, single "b") ])
              ("k: {\"a\": \"b\"}", "k: {<s f=0/>: <s f=1/>}", [ (0, dquote "a"); (1, dquote "b") ])
              ("k: [x, \"y, z\"]", "k: [x, <s f=0/>]", [ (0, dquote "y, z") ])
              ("- [\"a\", \"b\"]", "- [<s f=0/>, <s f=1/>]", [ (0, dquote "a"); (1, dquote "b") ]) ]
        for (input, text, strings) in cases do
            let actual = expectLines input text [code]
            Expect.equal (stringsOf actual) strings $"strings of {input}"

    testCase "a quote behind a property at a node start opens a scalar" <| fun _ ->
        // yaml@2.6.1: {"k":"x"}
        let actual = expectLines "k: !!str \"x\"" "k: !!str <s f=0/>" [code]
        Expect.equal (stringsOf actual) [0, dquote "x"] "the string"

    testCase "an indicator character or a tag-like word inside a plain scalar starts no node" <| fun _ ->
        // yaml@2.6.1: {"k":"a- \"b\""}, {"k":"a? \"b\""}, {"k":"a:\"b\""} and {"k":"a !b \"c\""}
        for input in [ "k: a- \"b\""; "k: a? \"b\""; "k: a:\"b\""; "k: a !b \"c\"" ] do
            let actual = expectLines input input [code]
            Expect.equal actual.StringMap.Count 0 $"no string in {input}"

    testCase "a hash right behind a closed quoted scalar or flow collection is an error" <| fun _ ->
        // yaml@2.6.1 rejects all four inputs with MISSING_CHAR
        for input in [ "k: \"b\"#c"; "k: 'b'#c"; "k: [a]#c"; "k: {a: 1}#c" ] do
            Expect.stringContains
                (errorOf (fun () -> Persil.tokenize input |> ignore))
                "Line 1: a comment needs a space or a tab before the #."
                $"The error of {input}"
            Expect.throws (fun () -> Reader.read input |> ignore) $"Reader.read rejects {input}"

    testCase "a hash behind a blank starts a comment and a hash inside a word is text" <| fun _ ->
        // yaml@2.6.1: {"k":"b"}, {"k":"C#"}, {"k":"tool.cwl#main"}, {"k":"a#b"} and {"k":"a]#c"}
        let comment = expectLines "k: \"b\" #c" "k: <s f=0/> <c f=0/>" [code]
        Expect.equal (commentsOf comment) [0, "c"] "the comment behind the string"
        for input in [ "k: C#"; "k: tool.cwl#main"; "k: a#b"; "k: a]#c" ] do
            let actual = expectLines input input [code]
            Expect.equal actual.CommentMap.Count 0 $"no comment in {input}"
]

let private continuationScannerTests = testList "Plain scalar continuation lines in the scanner" [
    /// Asserts that the input becomes the text with one plain string, whose value joins the segments.
    let expectMerged (input: string) (text: string) (value: string) =
        let lineCount = text.Split([| '\n' |]).Length
        let actual = expectLines input text (List.replicate lineCount code)
        Expect.equal (stringsOf actual) [0, plainEntry value] $"the string of {input}"
        actual

    testCase "the lines of a plain scalar become one string with a quote as text" <| fun _ ->
        // yaml@2.6.1: {"k":"$(inputs.a + \"x\")"}, {"k":"$(f(inputs.a, \"x\", 1))"}, {"k":"a \"b\""}, {"k":"a 'b' c"},
        // ["a \"b\""], "a \"b\"" and {"k":"a b \"c\""}
        expectMerged "k: $(inputs.a +\n  \"x\")" "k: <s f=0/>" "$(inputs.a +\n\"x\")" |> ignore
        expectMerged "k: $(f(inputs.a,\n  \"x\", 1))" "k: <s f=0/>" "$(f(inputs.a,\n\"x\", 1))" |> ignore
        expectMerged "k: a\n  \"b\"" "k: <s f=0/>" "a\n\"b\"" |> ignore
        expectMerged "k: a\n  'b' c" "k: <s f=0/>" "a\n'b' c" |> ignore
        expectMerged "- a\n  \"b\"" "- <s f=0/>" "a\n\"b\"" |> ignore
        expectMerged "a\n  \"b\"" "<s f=0/>" "a\n\"b\"" |> ignore
        expectMerged "k: a\n  b\n  \"c\"" "k: <s f=0/>" "a\nb\n\"c\"" |> ignore

    testCase "an indicator character on a continuation line is text" <| fun _ ->
        // yaml@2.6.1: {"k":"a - b"}, {"k":"a [b"}, {"k":"a {b"}, {"k":"a - \"b\""}, {"k":"a [\"b\", c]"}, {"k":"a {\"b\"}"},
        // {"k":"a ? |"}, {"k":"a http://x"}, {"k":"a b:c"} and {"k":"a b#c"}
        expectMerged "k: a\n  - b" "k: <s f=0/>" "a\n- b" |> ignore
        expectMerged "k: a\n  [b" "k: <s f=0/>" "a\n[b" |> ignore
        expectMerged "k: a\n  {b" "k: <s f=0/>" "a\n{b" |> ignore
        expectMerged "k: a\n  - \"b\"" "k: <s f=0/>" "a\n- \"b\"" |> ignore
        expectMerged "k: a\n  [\"b\", c]" "k: <s f=0/>" "a\n[\"b\", c]" |> ignore
        expectMerged "k: a\n  {\"b\"}" "k: <s f=0/>" "a\n{\"b\"}" |> ignore
        expectMerged "k: a\n  ? |" "k: <s f=0/>" "a\n? |" |> ignore
        expectMerged "k: a\n  http://x" "k: <s f=0/>" "a\nhttp://x" |> ignore
        expectMerged "k: a\n  b:c" "k: <s f=0/>" "a\nb:c" |> ignore
        expectMerged "k: a\n  b#c" "k: <s f=0/>" "a\nb#c" |> ignore

    testCase "a continuation line that looks like a block scalar header opens no block" <| fun _ ->
        // yaml@2.6.1: [{"k":"v ? |"},"b"]
        let actual = expectLines "- k: v\n      ? |\n- b" "- k: <s f=0/>\n- b" [code; code]
        Expect.equal (stringsOf actual) [0, plainEntry "v\n? |"] "the string"
        Expect.equal (blockIdsOf actual) [-1; -1] "no block"

    testCase "every segment is trimmed and a blank line is an empty segment" <| fun _ ->
        // yaml@2.6.1: {"k":"a b c"}, {"k":"a\nb"} and {"k":"a\n\nb"}
        expectMerged "k: a   \n  b  \n  \tc\t" "k: <s f=0/>" "a\nb\nc" |> ignore
        expectMerged "k: a\n\n  b" "k: <s f=0/>" "a\n\nb" |> ignore
        expectMerged "k: a\n\n\n  b" "k: <s f=0/>" "a\n\n\nb" |> ignore

    testCase "a blank line behind the scalar stays a line of its own" <| fun _ ->
        // yaml@2.6.1: {"k":"a b","j":1}
        let actual = expectLines "k: a\n  b\n\nj: 1" "k: <s f=0/>\n\nj: 1" [code; blank; code]
        Expect.equal (stringsOf actual) [0, plainEntry "a\nb"] "the string"
        let atEnd = expectLines "k: a\n  b\n\n" "k: <s f=0/>\n\n" [code; blank; blank]
        Expect.equal (stringsOf atEnd) [0, plainEntry "a\nb"] "the string at the end of the input"

    testCase "a comment behind the last line stays on the merged line" <| fun _ ->
        // yaml@2.6.1: {"k":"a b"}
        let actual = expectLines "k: a\n  b #c" "k: <s f=0/> <c f=0/>" [code]
        Expect.equal (stringsOf actual) [0, plainEntry "a\nb"] "the string"
        Expect.equal (commentsOf actual) [0, "c"] "the comment"
        Expect.equal actual.Lines.[0].CodeLength 12 "the comment placeholder starts behind the string placeholder"

    testCase "a comment ends the plain scalar and a deeper line behind it is an error" <| fun _ ->
        // yaml@2.6.1 rejects all six inputs with BAD_INDENT and MISSING_CHAR, the last one with UNEXPECTED_TOKEN
        let message (line: int) (commentLine: int) =
            $"Line {line}: a comment ended the plain scalar on line {commentLine}, so this more indented line is invalid."
        let expectMessage (yaml: string) (expected: string) =
            Expect.stringContains (errorOf (fun () -> Persil.tokenize yaml |> ignore)) expected $"The error of {yaml}"
        expectMessage "k: a # c\n  \"b\"" (message 2 1)
        expectMessage "k: a #c\n  b" (message 2 1)
        expectMessage "k: a\n# c\n  b" (message 3 2)
        expectMessage "k: a\n  # c\n  b" (message 3 2)
        expectMessage "k: a\n  b #c\n  d" (message 3 2)
        expectMessage "a #c\nb" (message 2 1)

    testCase "a comment ends the plain scalar only for a line that is deeper" <| fun _ ->
        // yaml@2.6.1: {"k":"b"}, {"k":"a","j":"x"}, [{"k":"a","j":"x"}], {"a":"b"} and two documents, "a" and "b"
        let afterEmptyValue = expectLines "k: # c\n  \"b\"" "k: <c f=0/>\n  <s f=0/>" [code; code]
        Expect.equal (stringsOf afterEmptyValue) [0, dquote "b"] "a key without a value has no scalar to end"
        let sibling = expectLines "k: a # c\nj: \"x\"" "k: a <c f=0/>\nj: <s f=0/>" [code; code]
        Expect.equal (stringsOf sibling) [0, dquote "x"] "a key at the same indentation"
        let inItem = expectLines "- k: a # c\n  j: \"x\"" "- k: a <c f=0/>\n  j: <s f=0/>" [code; code]
        Expect.equal (stringsOf inItem) [0, dquote "x"] "a sibling key in a sequence item"
        let explicitValue = expectLines "? a # c\n: b" "? a <c f=0/>\n: b" [code; code]
        Expect.equal explicitValue.StringMap.Count 0 "the explicit value indicator is not deeper than the key"
        let marker = expectLines "a # c\n--- \"b\"" "a <c f=0/>\n--- <s f=0/>" [code; start]
        Expect.equal (stringsOf marker) [0, dquote "b"] "a document marker ends everything"

    testCase "no plain scalar is open behind a quoted scalar or a bare key" <| fun _ ->
        // yaml@2.6.1 rejects `k: "a"\n  "b"` with BAD_INDENT and MISSING_CHAR, and reads the second input as {"k":"b"}.
        let afterQuote = expectLines "k: \"a\"\n  \"b\"" "k: <s f=0/>\n  <s f=1/>" [code; code]
        Expect.equal (stringsOf afterQuote) [0, dquote "a"; 1, dquote "b"] "both strings"
        let afterKey = expectLines "k:\n  \"b\"" "k:\n  <s f=0/>" [code; code]
        Expect.equal (stringsOf afterKey) [0, dquote "b"] "the string"

    testCase "a line that is not deeper than the owner of the scalar is no continuation" <| fun _ ->
        // yaml@2.6.1 rejects the first four inputs and reads the fifth as [{"k":"v \"b\""}].
        // The owner is the key column in `k: a` and `- k: v`, and the column of the last dash in `- a` and `- - a`.
        for (input, text) in [ ("k: a\n\"b\"", "k: a\n<s f=0/>"); ("- a\n\"b\"", "- a\n<s f=0/>"); ("- - a\n  \"b\"", "- - a\n  <s f=0/>"); ("- k: v\n  \"b\"", "- k: v\n  <s f=0/>") ] do
            let actual = expectLines input text [code; code]
            Expect.equal (stringsOf actual) [0, dquote "b"] $"the string in {input}"
        expectMerged "- k: v\n   \"b\"" "- k: <s f=0/>" "v\n\"b\"" |> ignore

    testCase "the scalar ends at the next line that is not deeper" <| fun _ ->
        // yaml@2.6.1: {"k":"a \"b\"","m":"z","n":"x\n"}
        let actual =
            expectLines
                "k: a\n  \"b\"\nm: \"z\"\nn: |\n  x\n"
                "k: <s f=0/>\nm: <s f=1/>\nn: |\n  x\n"
                [code; code; header; body; blank]
        Expect.equal (stringsOf actual) [0, plainEntry "a\n\"b\""; 1, dquote "z"] "the strings"

    testCase "the parent node owns a value on a line of its own" <| fun _ ->
        // yaml@2.6.1: {"valueFrom":"$(f(inputs.a, \"x\", 1))"}, ["a \"b\", c"], {"k":"a \"b\""} and "a \"b\""
        expectMerged "valueFrom:\n  $(f(inputs.a,\n  \"x\", 1))" "valueFrom:\n  <s f=0/>" "$(f(inputs.a,\n\"x\", 1))" |> ignore
        expectMerged "-\n  a\n  \"b\", c" "-\n  <s f=0/>" "a\n\"b\", c" |> ignore
        expectMerged "k:\n  a\n  \"b\"" "k:\n  <s f=0/>" "a\n\"b\"" |> ignore
        expectMerged "a\n\"b\"" "<s f=0/>" "a\n\"b\"" |> ignore

    testCase "a line that is not deeper than the parent ends a value on a line of its own" <| fun _ ->
        // yaml@2.6.1: {"k":"a","j":"x"} and ["a","b"]
        let key = expectLines "k:\n  a\nj: \"x\"" "k:\n  a\nj: <s f=0/>" [code; code; code]
        Expect.equal (stringsOf key) [0, dquote "x"] "the key at the shallow indentation"
        let item = expectLines "-\n  a\n- \"b\"" "-\n  a\n- <s f=0/>" [code; code; code]
        Expect.equal (stringsOf item) [0, dquote "b"] "the next sequence item"

    testCase "the key of a sequence item owns its value, and a sibling key ends the scalar" <| fun _ ->
        // yaml@2.6.1: [{"key":"a \"b\"","other":"c"}], [["a","b"]] and [["a \"b\"","c"]]
        let mapping = expectLines "- key:\n    a\n    \"b\"\n  other: \"c\"" "- key:\n    <s f=0/>\n  other: <s f=1/>" [code; code; code]
        Expect.equal (stringsOf mapping) [0, plainEntry "a\n\"b\""; 1, dquote "c"] "the sibling key opens a string, and the quote on the line before is text"
        let nested = expectLines "- -\n    a\n  - \"b\"" "- -\n    a\n  - <s f=0/>" [code; code; code]
        Expect.equal (stringsOf nested) [0, dquote "b"] "the second item of the inner sequence"
        let inner = expectLines "- - a\n    \"b\"\n  - \"c\"" "- - <s f=0/>\n  - <s f=1/>" [code; code]
        Expect.equal (stringsOf inner) [0, plainEntry "a\n\"b\""; 1, dquote "c"] "a quote deeper than the inner dash is text"

    testCase "a line of properties alone leaves the value to the node in front of it" <| fun _ ->
        // yaml@2.6.1: {"k":"x \"y\""}
        let actual = expectLines "k:\n  &a\n    x\n  \"y\"" "k:\n  &a\n    <s f=0/>" [code; code; code]
        Expect.equal (stringsOf actual) [0, plainEntry "x\n\"y\""] "the quote is text, because the owner of x is the key"

    testCase "a document marker ends the plain scalar and a plain scalar on the marker line is owned by the root" <| fun _ ->
        // yaml@2.6.1: two documents, "a" and "b", and "a \"b\""
        let quoted = expectLines "a\n--- \"b\"" "a\n--- <s f=0/>" [code; start]
        Expect.equal (stringsOf quoted) [0, dquote "b"] "the quote behind the marker opens a scalar"
        let plain = expectLines "--- a\n  \"b\"" "--- <s f=0/>" [start]
        Expect.equal (stringsOf plain) [0, plainEntry "a\n\"b\""] "the quote on the second line is text"

    testCase "text in the shape of a placeholder starts a plain scalar" <| fun _ ->
        // yaml@2.6.1: {"k":"<s f=0/> [x, \"y\"]"}
        expectMerged "k: <s f=0/>\n  [x, \"y\"]" "k: <s f=0/>" "<s f=0/>\n[x, \"y\"]" |> ignore

    testCase "a continuation line that holds a mapping indicator is an error" <| fun _ ->
        // yaml@2.6.1 rejects all four inputs with BLOCK_AS_IMPLICIT_KEY and MULTILINE_IMPLICIT_KEY
        for input in [ "k: a\n  b: c"; "k: a\n  \"b: c\""; "k: a\n  b:"; "k: a\n  b: # c" ] do
            Expect.stringContains
                (errorOf (fun () -> Persil.tokenize input |> ignore))
                "Line 2: a multi-line plain scalar cannot hold a mapping key (`: `)."
                $"The error of {input}"
        Expect.stringContains
            (errorOf (fun () -> Persil.tokenize "k: a\n  b\n\n  c: d" |> ignore))
            "Line 4: a multi-line plain scalar cannot hold a mapping key (`: `)."
            "The error names the physical line behind a blank line"
]

/// The pair `"a": 1` and the pair `"second": 2` of a flow mapping.
let private flowPairs (second: string) =
    [ YAMLElement.Mapping(doubleQuotedKey "a", plainNode "1")
      YAMLElement.Mapping(doubleQuotedKey second, plainNode "2") ]

let private flowScannerTests = testList "Flow collections across lines in the scanner" [
    testCase "a flow collection that spans lines keeps the quoted scalars on a line with two of them" <| fun _ ->
        // yaml@2.6.1: {"k":["a","b #c"]}, {"k":{"a":1,"b #c":2}}, {"a":1,"b #c":2}, {"k":{"a":1,"b\tc":2}} and [{"a":1,"b":2}]
        let doubleQuoted (value: string) = quotedNode ScalarStyle.DoubleQuoted value
        let cases =
            [ ("k:\n  [\n    \"a\", \"b #c\"\n  ]",
               YAMLElement.Object [ mapping "k" (YAMLElement.Object [ YAMLElement.Sequence [ doubleQuoted "a"; doubleQuoted "b #c" ] ]) ])
              ("k: {\n  \"a\": 1, \"b #c\": 2\n}",
               YAMLElement.Object [ mapping "k" (YAMLElement.Object (flowPairs "b #c")) ])
              ("{\n  \"a\": 1, \"b #c\": 2\n}",
               YAMLElement.Object (flowPairs "b #c"))
              ("k: {\n  \"a\": 1, \"b\\tc\": 2\n}",
               YAMLElement.Object [ mapping "k" (YAMLElement.Object (flowPairs "b\tc")) ])
              ("- {\n  \"a\": 1, \"b\": 2\n  }",
               YAMLElement.Object [ YAMLElement.Sequence [ YAMLElement.Object (flowPairs "b") ] ]) ]
        for (yaml, expected) in cases do
            let actual = Reader.read yaml
            Expect.equal actual expected $"The tree of {yaml}"
            expectNoPlaceholderText actual

    testCase "a line in a flow collection starts where the previous line ended" <| fun _ ->
        // yaml@2.6.1: ["a \"b\""] for the first input and ["a","b"] for the second. The third gives {"a":"b"}.
        let afterText = expectLines "[\n  a\n  \"b\"\n]" "[\n  a\n  \"b\"\n]" [code; code; code; code]
        Expect.equal afterText.StringMap.Count 0 "a quote behind plain text on the previous line is text"
        let afterComma = expectLines "[\n  a,\n  \"b\"\n]" "[\n  a,\n  <s f=0/>\n]" [code; code; code; code]
        Expect.equal (stringsOf afterComma) [0, dquote "b"] "a quote behind a comma opens a scalar"
        let afterColon = expectLines "{\n  a:\n  \"b\"\n}" "{\n  a:\n  <s f=0/>\n}" [code; code; code; code]
        Expect.equal (stringsOf afterColon) [0, dquote "b"] "a quote behind the colon of a pair opens a scalar"

    testCase "a flow collection nested in another one balances" <| fun _ ->
        // yaml@2.6.1: {"k":{"a":["b"]}}
        let actual = expectLines "k: {a: [b]}" "k: {a: [b]}" [code]
        Expect.equal actual.StringMap.Count 0 "no string"
        Expect.equal (Reader.read "k: {a: [b]}") (YAMLElement.Object [ mapping "k" (YAMLElement.Object [ mapping "a" (YAMLElement.Object [ YAMLElement.Sequence [ plainNode "b" ] ]) ]) ]) "tree"

    testCase "a closer of the other kind is an error that names the line of the opener" <| fun _ ->
        // yaml@2.6.1 rejects all three inputs
        let expectMessage (yaml: string) (message: string) =
            Expect.stringContains (errorOf (fun () -> Persil.tokenize yaml |> ignore)) message $"The error of {yaml}"
        expectMessage "k: [a}" "Line 1: `}` closes a `[`."
        expectMessage "k: {a]" "Line 1: `]` closes a `{`."
        expectMessage "a: 1\nk: [\n  {a: b\n  ]" "Line 3: `]` closes a `{`."

    testCase "a flow collection that is still open at the end or at a document marker is an error" <| fun _ ->
        // yaml@2.6.1 rejects all four inputs
        let expectMessage (yaml: string) (message: string) =
            Expect.stringContains (errorOf (fun () -> Persil.tokenize yaml |> ignore)) message $"The error of {yaml}"
        expectMessage "k: [\n  x" "Line 1: the flow collection opened here is not closed."
        expectMessage "a: 1\nk: [\n  x\n" "Line 2: the flow collection opened here is not closed."
        expectMessage "[a,\n---\nk: v" "Line 1: the flow collection opened here is not closed."
        expectMessage "[a,\n...\n" "Line 1: the flow collection opened here is not closed."

    testCase "a balanced flow collection before a document marker is no error" <| fun _ ->
        // yaml@2.6.1: ["a"] and ["b"] as two documents
        let actual = expectLines "[a]\n---\n[b]" "[a]\n---\n[b]" [code; start; code]
        Expect.equal actual.StringMap.Count 0 "no string"

    testCase "a line inside a flow collection opens no block scalar" <| fun _ ->
        // yaml@2.6.1 rejects the input with BAD_SCALAR_START
        let actual = expectLines "[\n  |\n    x\n]" "[\n  |\n    x\n]" [code; code; code; code]
        Expect.equal (blockIdsOf actual) [-1; -1; -1; -1] "no block"
]

let private pipelineTests = testList "Persil.pipeline" [
    let linesOf (prepared: Persil.PreparedYaml) =
        prepared.Lines |> Array.map (fun line -> line.Text, line.Kind) |> List.ofArray

    testCase "directives and the start marker are removed and prelude comments stay" <| fun _ ->
        let prepared = Persil.pipeline "# lead\n%YAML 1.2\n---\na: 1\n"
        Expect.equal (linesOf prepared) ["<c f=0/>", commentOnly; "a: 1", code] "lines"
        Expect.equal prepared.YAMLVersion (Some { Major = 1; Minor = 2 }) "version"
        Expect.equal (prepared.CommentMap.[0]) " lead" "comment"

    testCase "a comment behind a directive stays as a comment line" <| fun _ ->
        let prepared = Persil.pipeline "%YAML 1.2 # c\n---\na: 1\n"
        Expect.equal (linesOf prepared) ["<c f=0/>", commentOnly; "a: 1", code] "lines"
        Expect.equal prepared.YAMLVersion (Some { Major = 1; Minor = 2 }) "version"

    testCase "a comment behind the start marker stays as a comment line" <| fun _ ->
        let prepared = Persil.pipeline "--- # c\na: 1\n"
        Expect.equal (linesOf prepared) ["<c f=0/>", commentOnly; "a: 1", code] "lines"

    testCase "content behind the start marker becomes the first line" <| fun _ ->
        Expect.equal (linesOf (Persil.pipeline "--- 'q'\n")) ["<s f=0/>", code] "quoted scalar"
        let block = Persil.pipeline "--- |\n  x\n"
        Expect.equal (linesOf block) ["|", header; "  x", body] "block scalar"
        Expect.equal (block.Lines |> Array.map (fun line -> line.BlockId)) [| 0; 0 |] "the header keeps the id of its body"

    testCase "tag directives" <| fun _ ->
        let prepared = Persil.pipeline "%TAG !e! tag:example.com,2000:app/ # c\n%FOO bar\n---\n!e!foo bar\n"
        Expect.equal prepared.TagHandles.["!e!"] "tag:example.com,2000:app/" "handle"
        Expect.equal prepared.TagHandles.["!!"] "tag:yaml.org,2002:" "default handle"
        Expect.equal (linesOf prepared) ["<c f=0/>", commentOnly; "!e!foo bar", code] "an unknown directive is dropped"

    testCase "a duplicate YAML directive throws" <| fun _ ->
        Expect.throws (fun () -> Persil.pipeline "%YAML 1.2\n%YAML 1.1\n---\na: 1\n" |> ignore) "duplicate"

    testCase "a later marker stays a line" <| fun _ ->
        let prepared = Persil.pipeline "a: 1\n---\nb: 2\n"
        Expect.equal (linesOf prepared) ["a: 1", code; "---", start; "b: 2", code] "only the leading marker is removed"

    testCase "the body ends at the first end marker the scanner found" <| fun _ ->
        // yaml@2.6.1 reads the first input as {"a":1} and {"b":2}, and the second as {"a":1}, {"b":2} and {"c":3}
        Expect.equal (linesOf (Persil.pipeline "a: 1\n...\nb: 2\n")) ["a: 1", code] "the lines behind the marker are dropped"
        Expect.equal
            (linesOf (Persil.pipeline "a: 1\n---\nb: 2\n...\nc: 3\n"))
            ["a: 1", code; "---", start; "b: 2", code]
            "a start marker in front of it stays a line"
        Expect.equal (linesOf (Persil.pipeline "k:\n  - ...\n  - y\n")) ["k:", code; "  - ...", code; "  - y", code] "an indented triple dot is no marker"

    testCase "the empty first and last line are cut and body blanks stay" <| fun _ ->
        Expect.equal (linesOf (Persil.pipeline "\n# c\na: 1\n")) ["<c f=0/>", commentOnly; "a: 1", code] "cut"
        let keep = Persil.pipeline "a: |+\n  x\n\n\n"
        Expect.equal (linesOf keep) ["a: |+", header; "  x", body; "", body; "", body] "two blank lines of the body remain"
]

let private documentsTests = testList "Persil.prepareDocuments" [
    let linesOf (prepared: Persil.PreparedYaml) =
        prepared.Lines |> Array.map (fun line -> line.Text, line.Kind) |> List.ofArray

    testCase "every document keeps its own directives" <| fun _ ->
        // yaml@2.6.1: two documents, {"a":1} and {"b":2}
        let documents = Persil.prepareDocuments (Persil.tokenize "%YAML 1.2\n---\na: 1\n...\n%TAG !e! tag:e,2000:\n---\nb: 2\n")
        match documents with
        | [first; second] ->
            Expect.equal first.YAMLVersion (Some { Major = 1; Minor = 2 }) "version of the first document"
            Expect.isFalse (first.TagHandles.ContainsKey "!e!") "the tag handle belongs to the second document"
            Expect.isNone second.YAMLVersion "the version belongs to the first document"
            Expect.equal second.TagHandles.["!e!"] "tag:e,2000:" "tag handle of the second document"
            Expect.equal (linesOf first) ["a: 1", code] "lines of the first document"
            Expect.equal (linesOf second) ["b: 2", code] "lines of the second document"
        | other -> failwithf "Expected two documents, got %d" other.Length

    testCase "a start marker does not pass its directives on" <| fun _ ->
        // yaml@2.6.1: [null, {"b":2}]. The empty first document is dropped.
        let documents = Persil.prepareDocuments (Persil.tokenize "%YAML 1.2\n---\n---\nb: 2\n")
        match documents with
        | [only] ->
            Expect.isNone only.YAMLVersion "the second document has no directive"
            Expect.equal (linesOf only) ["b: 2", code] "lines"
        | other -> failwithf "Expected one document, got %d" other.Length

    testCase "a document that holds only directives is dropped" <| fun _ ->
        // yaml@2.6.1: [{"a":1}]
        let documents = Persil.prepareDocuments (Persil.tokenize "a: 1\n...\n%YAML 1.2\n")
        Expect.equal (documents |> List.map linesOf) [["a: 1", code]] "only the first document remains"

    testCase "a prelude comment is the first line of the document behind it" <| fun _ ->
        // yaml@2.6.1: two documents, {"a":1} and {"b":2}
        let documents = Persil.prepareDocuments (Persil.tokenize "a: 1\n...\n# c\n---\nb: 2\n")
        Expect.equal (documents |> List.map linesOf) [["a: 1", code]; ["<c f=0/>", commentOnly; "b: 2", code]] "lines"
]

let private sourceLinesOf (lines: Persil.TokenLine[]) =
    lines |> Array.map (fun line -> line.SourceLine) |> List.ofArray

let private lineDataTests = testList "Line data" [
    testCase "every emitted line knows its physical line" <| fun _ ->
        let actual = Persil.tokenize "a: 1\n\nb: |\n  x\n\n  y\nc: 2\n"
        Expect.equal (sourceLinesOf actual.Lines) [1; 2; 3; 4; 5; 6; 7; 8] "the last empty line is line 8"

    testCase "a multiline quoted scalar keeps the number of its first line and the lines behind it count on" <| fun _ ->
        let single = Persil.tokenize "a: 'x\n  y'\nb: 1\n"
        Expect.equal (sourceLinesOf single.Lines) [1; 3; 4] "single-quoted"
        let double = Persil.tokenize "a: \"x\n\n  y\"\nb: 1\n"
        Expect.equal (sourceLinesOf double.Lines) [1; 4; 5] "double-quoted with a blank line inside"
        let escaped = Persil.tokenize "a: \"x\\\n  y\"\nb: 1\n"
        Expect.equal (sourceLinesOf escaped.Lines) [1; 3; 4] "double-quoted with an escaped line break"
        let several = Persil.tokenize "a: 'p\n q'\nb: \"r\n s\"\nc: |\n  z\n"
        Expect.equal (sourceLinesOf several.Lines) [1; 3; 5; 6; 7] "several multiline scalars before a block scalar"

    testCase "a merged plain scalar keeps the number of its first line and the lines behind it count on" <| fun _ ->
        let plain = Persil.tokenize "a: x\n  y\nb: 1\n"
        Expect.equal (sourceLinesOf plain.Lines) [1; 3; 4] "two lines"
        let blank = Persil.tokenize "a: x\n\n  y\nb: 1\n"
        Expect.equal (sourceLinesOf blank.Lines) [1; 4; 5] "a blank line inside"
        let trailing = Persil.tokenize "a: x\n  y\n\nb: 1\n"
        Expect.equal (sourceLinesOf trailing.Lines) [1; 3; 4; 5] "a blank line behind the scalar is a line of its own"
        let comment = Persil.tokenize "a: x\n  y # c\nb: 1\n"
        Expect.equal (sourceLinesOf comment.Lines) [1; 3; 4] "a comment behind the last line"

    testCase "line endings do not change the numbers" <| fun _ ->
        Expect.equal (sourceLinesOf (Persil.tokenize "a: 1\r\nb: |\r\n  x\r\nc: 2\r\n").Lines) [1; 2; 3; 4; 5] "CRLF"
        Expect.equal (sourceLinesOf (Persil.tokenize "a: 1\rb: 2\r").Lines) [1; 2; 3] "CR"

    testCase "the preparation keeps the numbers of the lines it keeps" <| fun _ ->
        Expect.equal (sourceLinesOf (Persil.pipeline "%YAML 1.2\n---\na: 1\n").Lines) [3] "directive and marker removed"
        Expect.equal (sourceLinesOf (Persil.pipeline "%YAML 1.2 # c\n---\na: 1\n").Lines) [1; 3] "the comment behind a directive stays on its line"
        Expect.equal (sourceLinesOf (Persil.pipeline "--- # c\na: 1\n").Lines) [1; 2] "the comment behind the marker"
        Expect.equal (sourceLinesOf (Persil.pipeline "--- |\n  x\n").Lines) [1; 2] "the header behind the marker"

    testCase "a line knows its comment and the length of its code" <| fun _ ->
        let actual = Persil.tokenize "a: 1 # c\n  # d\nb: 2\nc: |\n  x\n"
        let data = actual.Lines |> Array.map (fun line -> line.CommentId, line.CodeLength) |> List.ofArray
        Expect.equal
            data
            [ 0, 5; 1, 2; Persil.NoComment, 4; Persil.NoComment, 4; Persil.NoComment, 3; Persil.NoComment, 0 ]
            "comment ids and code lengths, with the empty last line"
        Expect.equal actual.Lines.[0].Text "a: 1 <c f=0/>" "text of a trailing comment"
        Expect.equal (actual.Lines.[0].Text.Substring actual.Lines.[0].CodeLength) "<c f=0/>" "the comment placeholder starts at the code length"

    testCase "the comment of a directive line stays a comment line and the directive loses it" <| fun _ ->
        let prepared = Persil.pipeline "%YAML 1.2 # c\n---\na: 1\n"
        let comment = prepared.Lines.[0]
        Expect.equal (comment.Text, comment.Kind, comment.CommentId, comment.CodeLength) ("<c f=0/>", commentOnly, 0, 0) "comment line"
]

let private readerTests = testList "Reader on scanner output" [
    testCase "a multiline single-quoted scalar before a block scalar leaves the later string alone" <| fun _ ->
        // yaml@2.6.1: {"a":"x y","b":"\"hi\"\n","c":"z"}
        let expected =
            YAMLElement.Object [
                mapping "a" (quotedNode ScalarStyle.SingleQuoted "x y")
                mapping "b" (literal "\"hi\"\n")
                mapping "c" (quotedNode ScalarStyle.DoubleQuoted "z")
            ]
        Expect.equal (Reader.read "a: 'x\n  y'\nb: |\n  \"hi\"\nc: \"z\"\n") expected "c must be z without quotes"

    testCase "a multiline double-quoted scalar before a block scalar leaves the later string alone" <| fun _ ->
        // yaml@2.6.1: {"a":"x y","b":"'hi'\n","c":"z"}. The Reader does not fold the double-quoted
        // line break yet, so only b and c are compared here.
        match Reader.read "a: \"x\n  y\"\nb: |\n  'hi'\nc: 'z'\n" with
        | YAMLElement.Object [_; b; c] ->
            Expect.equal b (mapping "b" (literal "'hi'\n")) "block scalar"
            Expect.equal c (mapping "c" (quotedNode ScalarStyle.SingleQuoted "z")) "c must be z without quotes"
        | actual -> failwithf "Unexpected AST: %A" actual

    testCase "several multiline quotes before a block scalar" <| fun _ ->
        // yaml@2.6.1: {"a":"p q","b":"r s","c":"t u","d":"\"keep\"\n'keep'\n","e":"v","f":"w"}
        let yaml = "a: 'p\n  q'\nb: 'r\n  s'\nc: 't\n  u'\nd: |\n  \"keep\"\n  'keep'\ne: \"v\"\nf: 'w'\n"
        let expected =
            YAMLElement.Object [
                mapping "a" (quotedNode ScalarStyle.SingleQuoted "p q")
                mapping "b" (quotedNode ScalarStyle.SingleQuoted "r s")
                mapping "c" (quotedNode ScalarStyle.SingleQuoted "t u")
                mapping "d" (literal "\"keep\"\n'keep'\n")
                mapping "e" (quotedNode ScalarStyle.DoubleQuoted "v")
                mapping "f" (quotedNode ScalarStyle.SingleQuoted "w")
            ]
        Expect.equal (Reader.read yaml) expected "every later string must keep its value"

    testCase "a folded block and a literal block after a multiline quote" <| fun _ ->
        // yaml@2.6.1: {"a":"p q","b":"one\n\"two\"\n","c":"three 'four'\n","d":"five"}
        let yaml = "a: 'p\n  q'\nb: |\n  one\n  \"two\"\nc: >\n  three 'four'\nd: \"five\"\n"
        let expected =
            YAMLElement.Object [
                mapping "a" (quotedNode ScalarStyle.SingleQuoted "p q")
                mapping "b" (literal "one\n\"two\"\n")
                mapping "c" (block BlockScalarStyle.Folded ChompingMode.Clip None "three 'four'\n")
                mapping "d" (quotedNode ScalarStyle.DoubleQuoted "five")
            ]
        Expect.equal (Reader.read yaml) expected "the folded block and the string behind it"

    testCase "a quoted key with a hash keeps the following block and strings" <| fun _ ->
        // yaml@2.6.1: {"a #b":"x # not a comment\n'y'\n","z":"w"}
        let expected =
            YAMLElement.Object [
                YAMLElement.Mapping(
                    YAMLContent.create("a #b", style=ScalarStyle.DoubleQuoted),
                    literal "x # not a comment\n'y'\n"
                )
                mapping "z" (quotedNode ScalarStyle.DoubleQuoted "w")
            ]
        Expect.equal (Reader.read "\"a #b\": |\n  x # not a comment\n  'y'\nz: \"w\"\n") expected "key, block and string"

    testCase "tags and anchors before a header" <| fun _ ->
        // yaml@2.6.1 reads all four as {"k":"x \"q\"\n","m":"n"}
        let scalar (tag: string option) (anchor: string option) =
            YAMLElement.Value(
                YAMLContent.create(
                    "x \"q\"\n",
                    ?tag = tag,
                    ?anchor = anchor,
                    style = ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Clip, None)
                )
            )
        let expected (tag: string option) (anchor: string option) =
            YAMLElement.Object [
                mapping "k" (scalar tag anchor)
                mapping "m" (quotedNode ScalarStyle.DoubleQuoted "n")
            ]
        let str = Some "tag:yaml.org,2002:str"
        Expect.equal (Reader.read "k: !<tag:yaml.org,2002:str> |\n  x \"q\"\nm: \"n\"\n") (expected str None) "verbatim tag with colons"
        Expect.equal (Reader.read "k: !!str |\n  x \"q\"\nm: \"n\"\n") (expected str None) "shorthand tag"
        Expect.equal (Reader.read "k: &a |\n  x \"q\"\nm: \"n\"\n") (expected None (Some "a")) "anchor"
        Expect.equal (Reader.read "k: &a !!str |\n  x \"q\"\nm: \"n\"\n") (expected str (Some "a")) "anchor and tag"

    testCase "a sequence entry with a verbatim tag and a header" <| fun _ ->
        // yaml@2.6.1: ["x \"q\"\n","v"]
        let expected =
            YAMLElement.Object [
                YAMLElement.Sequence [
                    YAMLElement.Object [
                        YAMLElement.Value(
                            YAMLContent.create(
                                "x \"q\"\n",
                                tag = "tag:yaml.org,2002:str",
                                style = ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Clip, None)
                            )
                        )
                    ]
                    quotedNode ScalarStyle.SingleQuoted "v"
                ]
            ]
        Expect.equal (Reader.read "- !<tag:yaml.org,2002:str> |\n  x \"q\"\n- 'v'\n") expected "entry"

    testCase "text behind the bar is no header" <| fun _ ->
        // yaml@2.6.1 reports an error for the input
        Expect.throws (fun () -> Reader.read "a: | extra\n  \"q\"\n" |> ignore) "the line below `a: | extra` is not a body"

    testCase "placeholder spellings in a body are content" <| fun _ ->
        // yaml@2.6.1: {"a":"<s f=0/> and \"q\"\n","b":"w","c":"<s f=1/>\n"}
        let expected =
            YAMLElement.Object [
                mapping "a" (literal "<s f=0/> and \"q\"\n")
                mapping "b" (quotedNode ScalarStyle.DoubleQuoted "w")
                mapping "c" (literal "<s f=1/>\n")
            ]
        Expect.equal (Reader.read "a: |\n  <s f=0/> and \"q\"\nb: \"w\"\nc: |\n  <s f=1/>\n") expected "bodies are read verbatim"

    testCase "empty quoted scalars are empty" <| fun _ ->
        // yaml@2.6.1: {"a":"","b":""}
        let expected =
            YAMLElement.Object [
                mapping "a" (quotedNode ScalarStyle.SingleQuoted "")
                mapping "b" (quotedNode ScalarStyle.DoubleQuoted "")
            ]
        Expect.equal (Reader.read "a: ''\nb: \"\"\n") expected "empty strings"

    testCase "a quoted scalar behind a verbatim tag" <| fun _ ->
        // yaml@2.6.1: ["v"]
        let expected =
            YAMLElement.Object [
                YAMLElement.Sequence [
                    YAMLElement.Object [
                        YAMLElement.Value(YAMLContent.create("v", tag = "tag:yaml.org,2002:str", style = ScalarStyle.SingleQuoted))
                    ]
                ]
            ]
        Expect.equal (Reader.read "- !<tag:yaml.org,2002:str> 'v'\n") expected "tag and string"

    testCase "an unclosed quote leaves the later strings alone" <| fun _ ->
        // Invalid YAML, which yaml@2.6.1 rejects. The Reader keeps reading the later strings.
        match Reader.read "a: 'unclosed\nb: \"x\" # c\nc: 'y'\n" with
        | YAMLElement.Object [_; _; c] ->
            Expect.equal c (mapping "c" (quotedNode ScalarStyle.SingleQuoted "y")) "c is the closed string"
        | actual -> failwithf "Unexpected AST: %A" actual

    testCase "leading comments and directives are read as the start of the document" <| fun _ ->
        // yaml@2.6.1 reads all four as {"a":1}. Reader.read keeps the comments of the prelude.
        let a1 = mapping "a" (plainNode "1")
        let withComment = YAMLElement.Object [YAMLElement.Comment " lead"; a1]
        Expect.equal (Reader.read "# lead\n%YAML 1.2\n---\na: 1\n") withComment "comment before the directive"
        Expect.equal (Reader.read "%YAML 1.2\n# lead\n---\na: 1\n") withComment "comment behind the directive"
        Expect.equal (Reader.read "# lead\n---\na: 1\n") withComment "comment before the marker"
        Expect.equal (Reader.read "--- # c\na: 1\n") (YAMLElement.Object [YAMLElement.Comment " c"; a1]) "comment behind the marker"

    testCase "a block scalar behind the start marker" <| fun _ ->
        // yaml@2.6.1: "x\n"
        let expected = YAMLElement.Object [literal "x\n"]
        Expect.equal (Reader.read "--- |\n  x\n") expected "read"
        Expect.equal (Reader.readDocuments "--- |\n  x\n") [expected] "readDocuments"
        Expect.equal (Reader.readDocuments "--- |\n  x \"q\"\n  y 'r'\n") [YAMLElement.Object [literal "x \"q\"\ny 'r'\n"]] "quotes in the body"

    testCase "an unbalanced bracket or brace at the start of a continuation line does not hide the blocks after it" <| fun _ ->
        // yaml@2.6.1: {"desc":"see the list [draft","script":"echo 'hi'\n","next":{"run":"x # y\n"}} and the same with {bar.
        // The Reader keeps the line break of a plain multiline scalar and Decode folds it.
        let plainLines (value: string) = YAMLElement.Object [ YAMLElement.Value(YAMLContent.create(value, style = ScalarStyle.Plain)) ]
        for opener in [ "["; "{" ] do
            let expected =
                YAMLElement.Object [
                    mapping "desc" (plainLines ("see the list\n" + opener + "draft"))
                    mapping "script" (literal "echo 'hi'\n")
                    mapping "next" (YAMLElement.Object [ mapping "run" (block BlockScalarStyle.Folded ChompingMode.Clip None "x # y\n") ])
                ]
            let yaml = "desc: see the list\n  " + opener + "draft\nscript: |\n  echo 'hi'\nnext:\n  run: >\n    x # y\n"
            Expect.equal (Reader.read yaml) expected $"The scalars after the line that starts with {opener}"

    testCase "an unbalanced bracket in a sequence item does not hide the block scalar after it" <| fun _ ->
        // yaml@2.6.1: ["desc [bar","echo 'hi'\n"]
        let expected =
            YAMLElement.Object [
                YAMLElement.Sequence [
                    YAMLElement.Object [ YAMLElement.Value(YAMLContent.create("desc\n[bar", style = ScalarStyle.Plain)) ]
                    YAMLElement.Object [ literal "echo 'hi'\n" ]
                ]
            ]
        Expect.equal (Reader.read "- desc\n  [bar\n- |\n  echo 'hi'\n") expected "The second item is a block scalar"

    testCase "text in the shape of a comment placeholder is a value and the real comment stays with its line" <| fun _ ->
        // yaml@2.6.1: {"a":"x <c f=0/>","b":1}
        let expected =
            YAMLElement.Object [
                mapping "a" (plainNode "x <c f=0/>")
                mapping "b" (YAMLElement.Object [ YAMLElement.Value(YAMLContent.create("1", comment = " real")) ])
            ]
        Expect.equal (Reader.read "a: x <c f=0/>\nb: 1 # real\n") expected "Only b has a comment"
        Expect.equal (Reader.read "a: x <c f=0/>\n") (YAMLElement.Object [ mapping "a" (plainNode "x <c f=0/>") ]) "Without any real comment the text is no key of the comment table"

    testCase "text in the shape of a string placeholder is a value" <| fun _ ->
        // yaml@2.6.1: ["x <s f=7/>","y"] and {"a":"<c f=0/>"}
        Expect.equal
            (Reader.read "- x <s f=7/>\n- y\n")
            (YAMLElement.Object [ YAMLElement.Sequence [ plainNode "x <s f=7/>"; plainNode "y" ] ])
            "A string placeholder inside a plain scalar"
        Expect.equal
            (Decode.read "a: <c f=0/>\n" |> Decode.object (fun get -> get.Required.Field "a" Decode.string))
            "<c f=0/>"
            "A value that consists of the text only"

    testCase "only ASCII digits make the shape of a placeholder" <| fun _ ->
        // yaml@2.6.1: {"a":"x <s f=D/>"}, {"a":"x <c f=D/>"} and {"<c f=D/>":1}, where D is U+0661, an Arabic-Indic digit
        Expect.equal (Reader.read "a: x <s f=\u0661/>\n") (YAMLElement.Object [ mapping "a" (plainNode "x <s f=\u0661/>") ]) "string shape"
        Expect.equal (Reader.read "a: x <c f=\u0661/>\n") (YAMLElement.Object [ mapping "a" (plainNode "x <c f=\u0661/>") ]) "comment shape"
        Expect.equal (Reader.read "<c f=\u0661/>: 1\n") (YAMLElement.Object [ mapping "<c f=\u0661/>" (plainNode "1") ]) "key"

    testCase "a placeholder-shaped value reads like a plain scalar and survives the writer" <| fun _ ->
        // yaml@2.6.1: {"a":"<c f=0/>"} and {"a":"<s f=1/>"}
        for text in [ "<c f=0/>"; "<s f=1/>" ] do
            let parsed = Reader.read $"a: {text}\n"
            Expect.equal parsed (YAMLElement.Object [ mapping "a" (plainNode text) ]) "The value has no style, as in `a: x`"
            let written = Writer.write parsed None
            Expect.equal (Reader.read written) parsed $"The writer output {written} reads as the same value"

    testCase "a value on a line of its own continues over the next lines" <| fun _ ->
        // yaml@2.6.1: {"valueFrom":"$(f(inputs.a, \"x\", 1))"}, {"k":"a \"b\""}, {"k":"a","j":1}, "a \"b\"" and ["a \"b\", c"]
        let field (name: string) (yaml: string) =
            Reader.read yaml |> Decode.object (fun get -> get.Required.Field name Decode.string)
        Expect.equal (field "valueFrom" "valueFrom:\n  $(f(inputs.a,\n  \"x\", 1))") "$(f(inputs.a, \"x\", 1))" "an expression over two lines"
        Expect.equal (field "k" "k:\n  a\n  \"b\"") "a \"b\"" "a quote on the second line"
        Expect.equal (field "k" "k:\n  a\nj: 1") "a" "the scalar ends at the shallow key"
        Expect.equal (Reader.read "--- a\n  \"b\"" |> Decode.string) "a \"b\"" "a scalar on the marker line"
        Expect.equal (Reader.read "-\n  a\n  \"b\", c" |> Decode.list Decode.string) [ "a \"b\", c" ] "an item over two lines"
        Expect.equal (Reader.read "-\n  a\n- b" |> Decode.list Decode.string) [ "a"; "b" ] "the next item ends the scalar"

    testCase "a triple dot that is not at column 0 is text and one at column 0 ends the document" <| fun _ ->
        // yaml@2.6.1: {"k":["x","...","y"]}, {"k":"...","j":1}, ["...","y"], {"k":"..."} and {"a":1}
        let items (yaml: string) = Reader.read yaml |> Decode.object (fun get -> get.Required.Field "k" (Decode.list Decode.string))
        Expect.equal (items "k:\n  - x\n  - ...\n  - y") [ "x"; "..."; "y" ] "an indented item"
        let pair (yaml: string) =
            Reader.read yaml |> Decode.object (fun get -> get.Required.Field "k" Decode.string, get.Required.Field "j" Decode.string)
        Expect.equal (pair "k: ...\nj: 1") ("...", "1") "a value"
        Expect.equal (Reader.read "- ...\n- y" |> Decode.list Decode.string) [ "..."; "y" ] "a dash"
        Expect.equal (Reader.read "k:\n  ..." |> Decode.object (fun get -> get.Required.Field "k" Decode.string)) "..." "a line of its own"
        Expect.equal (Reader.read "a: 1\n...\nb: 2\n") (YAMLElement.Object [ mapping "a" (plainNode "1") ]) "the end marker at column 0"

    testCase "the lines of a plain scalar are one value that no stage reads as structure" <| fun _ ->
        // yaml@2.6.1: {"k":"a - x"}, {"k":"v ? | x"}, {"k":"a [x, \"y\"]"} and [{"k":"v ? |"},"b"]
        let plainLines (value: string) = YAMLElement.Object [ YAMLElement.Value(YAMLContent.create(value, style = ScalarStyle.Plain)) ]
        Expect.equal (Reader.read "k: a\n  - x") (YAMLElement.Object [ mapping "k" (plainLines "a\n- x") ]) "a dash"
        Expect.equal (Reader.read "k: v\n  ? |\n  x") (YAMLElement.Object [ mapping "k" (plainLines "v\n? |\nx") ]) "an explicit key mark"
        Expect.equal (Reader.read "k: a\n  [x, \"y\"]") (YAMLElement.Object [ mapping "k" (plainLines "a\n[x, \"y\"]") ]) "a flow sequence with a quote"
        Expect.equal
            (Reader.read "- k: v\n      ? |\n- b")
            (YAMLElement.Object [ YAMLElement.Sequence [ YAMLElement.Object [ mapping "k" (plainLines "v\n? |") ]; plainNode "b" ] ])
            "a mapping in a sequence item"

    testCase "Decode folds the lines of a plain scalar" <| fun _ ->
        // yaml@2.6.1: {"valueFrom":"$(f(inputs.a, \"x\", 1))"}, {"k":"a b c"}, {"k":"a\nb"}, {"k":"a - x"} and "a b"
        let field (name: string) (yaml: string) =
            Reader.read yaml |> Decode.object (fun get -> get.Required.Field name Decode.string)
        Expect.equal (field "valueFrom" "valueFrom: $(f(inputs.a,\n  \"x\", 1))") "$(f(inputs.a, \"x\", 1))" "an expression"
        Expect.equal (field "k" "k: a\n  b  \n  c") "a b c" "trailing blanks"
        Expect.equal (field "k" "k: a\n\n  b") "a\nb" "a blank line is a line break"
        Expect.equal (field "k" "k: a\n  - x") "a - x" "a dash"
        Expect.equal (Reader.read "a\nb" |> Decode.string) "a b" "a root scalar"

    testCase "a comment behind the last line of a plain scalar is the comment of the value" <| fun _ ->
        // yaml@2.6.1: {"k":"a b"}
        let expected =
            YAMLElement.Object [
                mapping "k" (YAMLElement.Object [ YAMLElement.Value(YAMLContent.create("a\nb", comment = "c", style = ScalarStyle.Plain)) ])
            ]
        Expect.equal (Reader.read "k: a\n  b #c") expected "the value and its comment"

    testCase "a plain scalar can be the explicit key or the explicit value" <| fun _ ->
        // yaml@2.6.1: {"a b":"c"} and {"key":"a b"}. Decode does not fold the line break of a key.
        let key = YAMLContent.create("a\nb", style = ScalarStyle.Plain)
        Expect.equal (Reader.read "? a\n  b\n: c") (YAMLElement.Object [ YAMLElement.Mapping(key, plainNode "c") ]) "the key"
        Expect.equal (Reader.read "? key\n: a\n  b" |> Decode.object (fun get -> get.Required.Field "key" Decode.string)) "a b" "the value"

    testCase "a mapping key or a comment inside a plain scalar is an error" <| fun _ ->
        // yaml@2.6.1 rejects both inputs
        Expect.throws (fun () -> Reader.read "k: a\n  b: c" |> ignore) "a colon followed by a blank"
        Expect.throws (fun () -> Reader.read "k: a # c\n  b" |> ignore) "a comment ends the scalar"

    testCase "a plain scalar of more than 5000 lines reads as one value" <| fun _ ->
        let lines = 5200
        let raw = "start" + String.concat "" [ for i in 0 .. lines - 1 -> sprintf "\nline %d" i ]
        let expected =
            YAMLElement.Object [
                mapping "k" (YAMLElement.Object [ YAMLElement.Value(YAMLContent.create(raw, style = ScalarStyle.Plain)) ])
                mapping "j" (plainNode "1")
            ]
        Expect.equal (Reader.read (plainScalarInput lines)) expected "every line in order, and the sibling behind it"
]

let private performanceTests = testList "Scanner performance" [
    testCase "malformed quotes keep every closed string and comment" <| fun _ ->
        let lines = 6000
        let actual = Persil.tokenize (malformedInput lines)
        Expect.equal actual.Lines.Length (lines + 1) "one emitted line per source line plus the empty last line"
        Expect.equal actual.StringMap.Count (lines / 3) "two closed strings in each group of six lines"
        Expect.equal actual.CommentMap.Count (lines / 6) "one comment in each group of six lines"
        let firstGroup = actual.Lines |> Array.take 6 |> Array.map (fun line -> line.Text) |> List.ofArray
        let expectedPlain = "k5: plain " + System.String('x', 120)
        Expect.equal
            firstGroup
            [ "k0: 'unclosed 0"
              "k1: \"unclosed 1"
              "k2: <s f=0/> <c f=0/>"
              "k3: \"bad\" tail 3"
              "k4: <s f=1/>"
              expectedPlain ]
            "the unclosed quotes stay literal and the closed strings are replaced"

    testCase "the time grows linearly with the number of lines" <| fun _ ->
        expectLinear "The tokenizer" (Persil.tokenize >> ignore) countLineBreaks malformedInput

    testCase "the time grows linearly with the lines of a plain scalar" <| fun _ ->
        expectLinear "The tokenizer" (Persil.tokenize >> ignore) countLineBreaks plainScalarInput

    testCase "the read time grows linearly with the lines of a plain scalar" <| fun _ ->
        expectLinear "The read" (Preprocessing.read >> ignore) (Persil.tokenize >> ignore) plainScalarInput

    testCase "one unclosed quote in front of a long input" <| fun _ ->
        // The first probe runs to the end of the input and every later probe reuses its result.
        let input = "a: 'open\n" + String.replicate 5000 "b: ''x\nc: \"y\"\n"
        let actual = Persil.tokenize input
        Expect.equal actual.StringMap.Count 5000 "the double-quoted strings behind the unclosed quote"
        Expect.equal actual.Lines.Length (10000 + 2) "lines"
]

let Main = testList "Tokenizer" [
    headerTests
    blockScannerTests
    documentScannerTests
    quoteScannerTests
    continuationScannerTests
    flowScannerTests
    pipelineTests
    documentsTests
    lineDataTests
    readerTests
    performanceTests
]
