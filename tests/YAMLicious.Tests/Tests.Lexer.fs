module Tests.Lexer

open Fable.Pyxpecto
open YAMLicious

/// Asserts the lexemes of the input without the stream start, the stream end and the line starts. The
/// driver in Driver.fs chooses the context of every lexeme like the parser will.
let private expectLexemes (input: string) (expected: string list) =
    Expect.equal (lex input) expected (sprintf "Lexemes of %A" input)

/// Asserts all lexemes of the input, also the stream start, the stream end and the line starts.
let private expectFull (input: string) (expected: string list) =
    Expect.equal (lexFull input) expected (sprintf "Lexemes of %A" input)

/// Asserts that the lexer fails with the code on the span of the input, as in `1:4-1:5`.
let private expectError (input: string) (code: string) (span: string) =
    Expect.equal (errorCodeOf input) code (sprintf "Error code of %A" input)
    Expect.equal (errorSpanOf input) span (sprintf "Error span of %A" input)

let private streamTests = testList "Lexer stream" [
    testCase "an empty text holds the stream start and the stream end" <| fun _ ->
        expectFull "" [ "stream-start"; "stream-end" ]

    testCase "a line break alone is one blank line and the last line break starts no line" <| fun _ ->
        expectFull "\n" [ "stream-start"; "blank"; "stream-end" ]
        expectFull "a\n" [ "stream-start"; "line:0"; "plain:a"; "stream-end" ]
        expectFull "a\n\n" [ "stream-start"; "line:0"; "plain:a"; "blank"; "stream-end" ]

    testCase "blanks without a line break at the end of the text are a blank line" <| fun _ ->
        expectFull "   " [ "stream-start"; "blank"; "stream-end" ]
        expectFull "a\n  " [ "stream-start"; "line:0"; "plain:a"; "blank"; "stream-end" ]

    testCase "the stream end repeats for every call behind the end" <| fun _ ->
        let cursor = Cursor("a")
        let context = { Grammar = GrammarContext.BlockOut; ParentIndent = -1; SimpleKey = false }
        let next () =
            match Lexer.next context cursor with
            | Ok lexeme -> show lexeme.Value
            | Error failure -> describeError failure
        Expect.equal [ next (); next (); next (); next (); next (); next () ] [ "stream-start"; "line:0"; "plain:a"; "stream-end"; "stream-end"; "stream-end" ] "The end repeats"

    testCase "a byte order mark at the start is skipped and takes no column" <| fun _ ->
        Expect.equal (lexSpans "\uFEFFa: b") (lexSpans "a: b") "The marks do not change"
        Expect.equal (lexSpans "\uFEFFa: b") [ "plain:a 1:1-1:2"; ": 1:2-1:3"; "plain:b 1:4-1:5" ] "The spans"
        Expect.equal (Cursor("\uFEFFa").Origin) 1 "The cursor starts behind the mark"
        Expect.equal (Cursor("a").Origin) 0 "A text without the mark starts at 0"

    // yaml@2.6.1 reads `--- a\n...\n\uFEFFb` as "a" and "b". [202] l-document-prefix allows the mark in front of every document. The lexer
    // returns a run of marks at the start of a block line as one ByteOrderMark lexeme and leaves the question of its place to the parser ([211]).
    testCase "a byte order mark at the start of a block line is one lexeme and takes no column" <| fun _ ->
        expectLexemes "--- a\n...\n\uFEFFb" [ "---"; "plain:a"; "..."; "bom"; "plain:b" ]
        Expect.equal
            (lexSpans "--- a\n...\n\uFEFFb")
            [ "--- 1:1-1:4"; "plain:a 1:5-1:6"; "... 2:1-2:4"; "bom 3:1-3:2"; "plain:b 3:1-3:2" ]
            "The mark takes no column, and its span is the one column that it would fill"
        expectLexemes "a\n...\n\uFEFF---\nb" [ "plain:a"; "..."; "bom"; "---"; "plain:b" ]
        expectLexemes "a\n...\n\uFEFF%YAML 1.2\n---\nb" [ "plain:a"; "..."; "bom"; "%YAML 1.2"; "---"; "plain:b" ]
        expectLexemes "a\n...\n\uFEFF# c\nb" [ "plain:a"; "..."; "bom"; "#own: c"; "plain:b" ]

    // yaml@2.6.1 reads `\uFEFF%YAML 1.2\n---\na` as "a". [202] l-document-prefix puts the mark at the start of the prefix, and the directives of [209] follow the
    // prefix. The prefix may hold comment lines in front of the mark of the next prefix.
    testCase "a byte order mark in front of the directives and behind the comment lines of a prefix" <| fun _ ->
        expectLexemes "\uFEFF%YAML 1.2\n---\na" [ "%YAML 1.2"; "---"; "plain:a" ]
        expectLexemes "a\n...\n# c\n\n\uFEFFb" [ "plain:a"; "..."; "#own: c"; "blank"; "bom"; "plain:b" ]
        expectLexemes "a\n...\n\uFEFF%YAML 1.2\n---\nb" [ "plain:a"; "..."; "bom"; "%YAML 1.2"; "---"; "plain:b" ]
        expectLexemes "\uFEFF# c\na" [ "#own: c"; "plain:a" ]
        // [211] l-yaml-stream repeats the prefix, and each one may open with a mark, so a run of marks is one lexeme. The first mark of the text
        // is not part of the lexemes, because the cursor starts behind it.
        expectLexemes "\uFEFF\uFEFFa" [ "bom"; "plain:a" ]
        expectLexemes "\uFEFF# c\n\uFEFF\uFEFFa" [ "#own: c"; "bom"; "plain:a" ]
        expectLexemes "a\n...\n\uFEFF\uFEFF\uFEFFb" [ "plain:a"; "..."; "bom"; "plain:b" ]
        Expect.equal (lexSpans "\uFEFF# c\n\uFEFF\uFEFFa") [ "#own: c 1:1-1:4"; "bom 2:1-2:2"; "plain:a 2:1-2:2" ] "A run of marks takes no column"

    // yaml@2.6.1 accepts the mark behind a directive and inside a document and keeps it in the text. The mark is no nb-char ([27]) and no ns-char ([34]), so
    // it can only stand at the start of a block line, where [202] and [211] allow it. The lexer reports a mark at that place as a lexeme whatever stands
    // in front of it, and the parser decides whether the stream rule allows it there.
    testCase "a byte order mark at the start of a line is a lexeme wherever it stands in a block context" <| fun _ ->
        expectLexemes "%YAML 1.2\n\uFEFF---\na" [ "%YAML 1.2"; "bom"; "---"; "plain:a" ]
        expectLexemes "a\n\uFEFFb" [ "plain:a"; "bom"; "plain:b" ]
        expectLexemes "k: 1\n\uFEFFj: 2" [ "plain:k"; ":"; "plain:1"; "bom"; "plain:j"; ":"; "plain:2" ]
        expectLexemes "--- # c\n\uFEFFa" [ "---"; "#: c"; "bom"; "plain:a" ]
        Expect.equal (lexIn GrammarContext.BlockOut -1 false "a\n\uFEFFb") [ "plain:a"; "bom"; "plain:b" ] "A plain scalar ends in front of the mark"
        // A mark behind blanks stands inside the line.
        expectError "a\n \uFEFFb" "invalid-character" "2:2-2:3"
        expectError "k:\n  \uFEFFa" "invalid-character" "2:3-2:4"

    // [211] l-yaml-stream lets a mark end the document, as a marker does. So the body of a block scalar ends at a line that starts with a mark, also at the root where the
    // indentation of the parent is -1 and a line at the column 0 can belong to the scalar. The mark behind spaces is inside the line and stays an error.
    testCase "a line that starts with a byte order mark ends the body of a block scalar" <| fun _ ->
        expectLexemes "|\n  x\n\uFEFF# c" [ "literal:x\\n"; "bom"; "#own: c" ]
        expectLexemes "|+\n  x\n\n\uFEFF---\ny" [ "literal+:x\\n\\n"; "bom"; "---"; "plain:y" ]
        expectLexemes ">\n  x\n\uFEFF" [ "folded:x\\n"; "bom" ]
        expectLexemes "|\nx\n\uFEFFy" [ "literal:x\\n"; "bom"; "plain:y" ]
        expectLexemes "|\n\uFEFF# c" [ "literal:"; "bom"; "#own: c" ]
        expectLexemes "k: |\n  x\n\uFEFF# c" [ "plain:k"; ":"; "literal:x\\n"; "bom"; "#own: c" ]
        expectError "|\n  x\n \uFEFF" "block-indent" "3:2-3:3"

    // [202] l-document-prefix lets a directive follow a mark, so the lexer reads a directive behind a mark wherever it stands. Whether the stream allows it there is up to the parser ([211]).
    testCase "a percent sign line behind a byte order mark is a directive for the lexer" <| fun _ ->
        expectLexemes "a\n\uFEFF%YAML 1.2\n---\nb" [ "plain:a"; "bom"; "%YAML 1.2"; "---"; "plain:b" ]
        expectLexemes "a\n\uFEFF\n# c\n%YAML 1.2\n---\nb" [ "plain:a"; "bom"; "blank"; "#own: c"; "%YAML 1.2"; "---"; "plain:b" ]
        // Without a mark the line continues the plain scalar.
        expectLexemes "a\n%YAML 1.2\n---\nb" [ "plain:a %YAML 1.2"; "---"; "plain:b" ]

    // Inside a flow collection a mark is invalid at the start of a line too, and so is one behind a block scalar header or inside a block scalar.
    testCase "a byte order mark at the start of a line inside a flow collection is an error" <| fun _ ->
        expectError "[a,\n\uFEFFb]" "invalid-character" "2:1-2:2"
        expectError "{a: 1,\n\uFEFFb: 2}" "invalid-character" "2:1-2:2"
        Expect.equal (lexIn GrammarContext.FlowIn -1 false "[a,\n\uFEFFb]") [ "["; "plain:a"; ","; "error:invalid-character" ] "The error follows the comma"

    // yaml@2.6.1 accepts the mark in each of these places and keeps it in the text, or drops it together with the comment. [27] nb-char and [34] ns-char exclude it.
    testCase "a byte order mark inside a plain scalar, a block scalar, a comment, a name or a directive is an error" <| fun _ ->
        expectError "a\uFEFFb" "invalid-character" "1:2-1:3"
        expectError "k: a\uFEFFb" "invalid-character" "1:5-1:6"
        expectError "k: a\n  b\uFEFFc" "invalid-character" "2:4-2:5"
        expectError "[a, b\uFEFF]" "invalid-character" "1:6-1:7"
        expectError "k: \uFEFFa" "invalid-character" "1:4-1:5"
        expectError "k: |\n  a\uFEFF" "invalid-character" "2:4-2:5"
        expectError "k: >\n  a\n  \uFEFFb" "invalid-character" "3:3-3:4"
        expectError "# c\uFEFF" "invalid-character" "1:4-1:5"
        expectError "a # c\uFEFF" "invalid-character" "1:6-1:7"
        expectError "k: | # c\uFEFF\n  a" "invalid-character" "1:9-1:10"
        expectError "&a\uFEFFb c" "invalid-character" "1:3-1:4"
        expectError "*a\uFEFF" "invalid-character" "1:3-1:4"
        expectError "%FOO a\uFEFFb\n---" "invalid-character" "1:7-1:8"
        expectLexemes "a\uFEFFb" [ "plain:a"; "error:invalid-character" ]
        expectLexemes "&a\uFEFFb" [ "&a"; "error:invalid-character" ]

    // [107] nb-double-char and [118] nb-single-char take every character of [2] nb-json, and 1.2.2 section 5.2 says that byte order marks are allowed inside quoted scalars.
    // The mark is content there, wherever it stands in the scalar, and it takes the column that its position gives it unless it starts a line.
    testCase "a byte order mark inside a quoted scalar is content" <| fun _ ->
        expectLexemes "\"a\uFEFFb\"" [ "double:a\uFEFFb" ]
        expectLexemes "'a\uFEFFb'" [ "single:a\uFEFFb" ]
        expectLexemes "\"\uFEFF\"" [ "double:\uFEFF" ]
        expectLexemes "'\uFEFF'" [ "single:\uFEFF" ]
        expectLexemes "\"\uFEFF\uFEFF\"" [ "double:\uFEFF\uFEFF" ]
        expectLexemes "[\"a\uFEFF\", {'\uFEFFb': 1}]" [ "["; "double:a\uFEFF"; ","; "{"; "single:\uFEFFb"; ":"; "plain:1"; "}"; "]" ]
        expectLexemes "k: \"a\uFEFFb\"" [ "plain:k"; ":"; "double:a\uFEFFb" ]
        expectLexemes "\"\uFEFFa\": b" [ "double:\uFEFFa"; ":"; "plain:b" ]
        Expect.equal (lexSpans "\"a\uFEFFb\"") [ "double:a\uFEFFb 1:1-1:6" ] "The mark inside the line takes a column"
        // A mark at the start of a continuation line is content as well. It hides a document marker behind it, because the marker has to stand at the start of the line.
        expectLexemes "\"a\n\uFEFFb\"" [ "double:a \uFEFFb" ]
        expectLexemes "'a\n\uFEFF---'" [ "single:a \uFEFF---" ]
        expectLexemes "\"a\n\n\uFEFF\"" [ "double:a\\n\uFEFF" ]
        expectLexemes "- \"a\n  \uFEFFb\"" [ "-"; "double:a \uFEFFb" ]
        // The line of a continuation stands at any indentation, so the mark in the first column is content in a flow collection as well.
        expectLexemes "k: \"a\n\uFEFFb\"" [ "plain:k"; ":"; "double:a \uFEFFb" ]
        expectLexemes "k: [\"a\n\uFEFFb\"]" [ "plain:k"; ":"; "["; "double:a \uFEFFb"; "]" ]
        // A mark behind the closing quote is outside the scalar.
        expectError "\"a\"\uFEFF" "invalid-character" "1:4-1:5"

    // yaml@2.6.1 reads the escape for the mark in a double-quoted scalar as the mark. The escape stays valid.
    testCase "the escape for the byte order mark stays valid in a double-quoted scalar" <| fun _ ->
        Expect.equal (lexIn GrammarContext.BlockOut -1 false "\"a\\uFEFFb\"") [ "double:a\uFEFFb" ] "The escape"
        Expect.equal (lexIn GrammarContext.BlockOut -1 false "\"\\uFEFF\"") [ "double:\uFEFF" ] "The escape alone"
]

let private blockTests = testList "Lexer block lexemes" [
    testCase "a mapping entry is a key, a colon and a value" <| fun _ ->
        expectLexemes "a: b" [ "plain:a"; ":"; "plain:b" ]
        expectFull "a: b" [ "stream-start"; "line:0"; "plain:a"; ":"; "plain:b"; "stream-end" ]

    testCase "a sequence is dashes and the indentation of every line" <| fun _ ->
        expectFull "- a\n- b" [ "stream-start"; "line:0"; "-"; "plain:a"; "line:0"; "-"; "plain:b"; "stream-end" ]
        expectFull "k:\n  - a\n  - b" [ "stream-start"; "line:0"; "plain:k"; ":"; "line:2"; "-"; "plain:a"; "line:2"; "-"; "plain:b"; "stream-end" ]

    testCase "compact nesting puts the indicators in a row" <| fun _ ->
        expectLexemes "- - a" [ "-"; "-"; "plain:a" ]
        expectLexemes "- k: v" [ "-"; "plain:k"; ":"; "plain:v" ]
        expectLexemes "- ? a\n  : b" [ "-"; "?"; "plain:a"; ":"; "plain:b" ]

    testCase "an explicit entry is a question mark and a colon at the start of lines" <| fun _ ->
        expectLexemes "? a\n: b" [ "?"; "plain:a"; ":"; "plain:b" ]
        expectLexemes "?\n: b" [ "?"; ":"; "plain:b" ]

    // yaml@2.6.1 reads "-a" as the string "-a", "?a: b" as {"?a":"b"}, ":a" as ":a", "a:b" as "a:b", "a :b" as "a :b"
    // and "k: b:c" as {"k":"b:c"}. An indicator needs a blank or the end of the line behind it.
    testCase "an indicator character without a blank behind it begins a plain scalar" <| fun _ ->
        expectLexemes "-a" [ "plain:-a" ]
        expectLexemes "?a: b" [ "plain:?a"; ":"; "plain:b" ]
        expectLexemes ":a" [ "plain::a" ]
        expectLexemes "a:b" [ "plain:a:b" ]
        expectLexemes "a :b" [ "plain:a :b" ]
        expectLexemes "k: b:c" [ "plain:k"; ":"; "plain:b:c" ]

    testCase "an indicator at the end of the line stands alone" <| fun _ ->
        expectLexemes "-\n- a" [ "-"; "-"; "plain:a" ]
        expectLexemes "k:\nj: 1" [ "plain:k"; ":"; "plain:j"; ":"; "plain:1" ]
        expectLexemes "-" [ "-" ]
        expectLexemes "?" [ "?" ]

    // yaml@2.6.1 reads "a:\tb" as {"a":"b"}, "-\ta" as ["a"] and "a\t: b" as {"a":"b"}
    testCase "a tab separates the tokens of a line" <| fun _ ->
        expectLexemes "a:\tb" [ "plain:a"; ":"; "plain:b" ]
        expectLexemes "-\ta" [ "-"; "plain:a" ]
        expectLexemes "a\t: b" [ "plain:a"; ":"; "plain:b" ]

    testCase "blank lines and comment lines are lexemes between the nodes" <| fun _ ->
        expectLexemes "k: a\n\n\nj: b" [ "plain:k"; ":"; "plain:a"; "blank"; "blank"; "plain:j"; ":"; "plain:b" ]
        expectLexemes "a: b\n  \nc: d" [ "plain:a"; ":"; "plain:b"; "blank"; "plain:c"; ":"; "plain:d" ]
        expectLexemes "a: 1\n# c\n  # d\nb: 2" [ "plain:a"; ":"; "plain:1"; "#own: c"; "#own: d"; "plain:b"; ":"; "plain:2" ]

    testCase "the line start reports the indentation and the tabs among the blanks" <| fun _ ->
        expectFull "a:\n    b: 1" [ "stream-start"; "line:0"; "plain:a"; ":"; "line:4"; "plain:b"; ":"; "plain:1"; "stream-end" ]
        // yaml@2.6.1 rejects "\ta: b" and " \ta: b" with TAB_AS_INDENT, and reads "a:\n \tb" as {"a":"b"}. The lexer reports the tab and
        // leaves the decision about a key to the parser.
        expectFull "\ta: b" [ "stream-start"; "line:0,tab"; "plain:a"; ":"; "plain:b"; "stream-end" ]
        expectFull "a:\n \tb" [ "stream-start"; "line:0"; "plain:a"; ":"; "line:1,tab"; "plain:b"; "stream-end" ]
]

let private flowTests = testList "Lexer flow lexemes" [
    testCase "a flow sequence and a flow mapping are brackets, commas and nodes" <| fun _ ->
        expectLexemes "[a, b]" [ "["; "plain:a"; ","; "plain:b"; "]" ]
        expectLexemes "{a: 1, b: [c, d]}" [ "{"; "plain:a"; ":"; "plain:1"; ","; "plain:b"; ":"; "["; "plain:c"; ","; "plain:d"; "]"; "}" ]
        expectLexemes "k: [a, b]\nj: {c: d}" [ "plain:k"; ":"; "["; "plain:a"; ","; "plain:b"; "]"; "plain:j"; ":"; "{"; "plain:c"; ":"; "plain:d"; "}" ]

    // yaml@2.6.1: [["a:b"]], [[{"a":"b"}]], [{"a":null}] for `{a:}` and [{"a":null,"b":null}] for `{a:,b}`
    testCase "a colon in a flow collection ends a key behind a blank or a flow indicator only" <| fun _ ->
        expectLexemes "[a:b]" [ "["; "plain:a:b"; "]" ]
        expectLexemes "[a: b]" [ "["; "plain:a"; ":"; "plain:b"; "]" ]
        expectLexemes "{a:}" [ "{"; "plain:a"; ":"; "}" ]
        expectLexemes "{a:,b}" [ "{"; "plain:a"; ":"; ","; "plain:b"; "}" ]

    // yaml@2.6.1: {"a":"b"}, {"a":"b"} for the blank in front of the colon and [{"a":"b"}] for `[[a]:b]` is a collection key
    testCase "a colon right behind a quoted scalar or a flow collection is a value indicator" <| fun _ ->
        expectLexemes "{\"a\":b}" [ "{"; "double:a"; ":"; "plain:b"; "}" ]
        expectLexemes "{'a':b}" [ "{"; "single:a"; ":"; "plain:b"; "}" ]
        expectLexemes "[\"a\" : b]" [ "["; "double:a"; ":"; "plain:b"; "]" ]
        expectLexemes "[[a]:b]" [ "["; "["; "plain:a"; "]"; ":"; "plain:b"; "]" ]
        expectLexemes "{\"a\": b}" [ "{"; "double:a"; ":"; "plain:b"; "}" ]

    // [149] c-ns-flow-map-adjacent-value: a colon behind a quoted scalar or a flow collection takes its value without a blank, whatever stands between the node and the colon.
    // yaml@2.6.1 reads `{"a" :b}` as {"a":"b"}. The colon after any other node needs a blank or a flow indicator behind it.
    testCase "a colon behind blanks, comments and line breaks after a JSON-like node takes its value without a blank" <| fun _ ->
        expectLexemes "{\"a\" :b}" [ "{"; "double:a"; ":"; "plain:b"; "}" ]
        expectLexemes "{\"a\"\n :b}" [ "{"; "double:a"; ":"; "plain:b"; "}" ]
        expectLexemes "{\"a\" # c\n :b}" [ "{"; "double:a"; "#: c"; ":"; "plain:b"; "}" ]
        expectLexemes "[[a] :b]" [ "["; "["; "plain:a"; "]"; ":"; "plain:b"; "]" ]
        expectLexemes "{a :b}" [ "{"; "plain:a :b"; "}" ]
        expectLexemes "{\"a\", :b}" [ "{"; "double:a"; ","; "plain::b"; "}" ]
        expectLexemes "{\"a\" b :c}" [ "{"; "double:a"; "plain:b :c"; "}" ]

    testCase "a colon right behind a quoted scalar is a value indicator in a block key too" <| fun _ ->
        expectLexemes "\"a\": b" [ "double:a"; ":"; "plain:b" ]
        expectLexemes "\"a\" : b" [ "double:a"; ":"; "plain:b" ]

    testCase "a flow collection goes on in the lines below without line starts" <| fun _ ->
        expectFull "[a,\n b]" [ "stream-start"; "line:0"; "["; "plain:a"; ","; "plain:b"; "]"; "stream-end" ]
        expectLexemes "[a\n b]" [ "["; "plain:a b"; "]" ]
        expectLexemes "[a,\n\n b]" [ "["; "plain:a"; ","; "blank"; "plain:b"; "]" ]
        expectLexemes "[a, # c\n b]" [ "["; "plain:a"; ","; "#: c"; "plain:b"; "]" ]
        expectLexemes "[\n  # c\n  a\n]" [ "["; "#own: c"; "plain:a"; "]" ]

    // yaml@2.6.1 reads `{? a, b: c}` as {"a":null,"b":"c"}, `[?a]` as [["?a"]] and `[?]`, `[?, a]` and `{?}` as an explicit key with an
    // empty node: [{"":null}], [{"":null},"a"] and {"":null}. [142] and [150] ask for s-separate behind the `?`, and ns-plain-first
    // rejects a `?` in front of a flow indicator, so the lexer rejects what yaml@2.6.1 accepts.
    testCase "an explicit key in a flow collection stands in front of a blank and a question mark in front of a flow indicator is an error" <| fun _ ->
        expectLexemes "{? a, b: c}" [ "{"; "?"; "plain:a"; ","; "plain:b"; ":"; "plain:c"; "}" ]
        expectLexemes "[?a]" [ "["; "plain:?a"; "]" ]
        expectError "[?]" "reserved-indicator" "1:2-1:3"
        expectError "[?, a]" "reserved-indicator" "1:2-1:3"
        expectError "{?}" "reserved-indicator" "1:2-1:3"
        expectError "{a: ?}" "reserved-indicator" "1:5-1:6"

    testCase "an empty collection and a trailing comma" <| fun _ ->
        expectLexemes "[]" [ "["; "]" ]
        expectLexemes "{}" [ "{"; "}" ]
        expectLexemes "[a, ]" [ "["; "plain:a"; ","; "]" ]

    // yaml@2.6.1 rejects `[- a]` with MISSING_CHAR and reads `[-a]` as ["-a"]
    testCase "a dash in a flow collection needs a plain character behind it" <| fun _ ->
        expectLexemes "[- a]" [ "["; "error:flow-dash" ]
        expectLexemes "[-a]" [ "["; "plain:-a"; "]" ]

    // yaml@2.6.1 rejects `]` and `a: [x]]` with UNEXPECTED_TOKEN and reads `a: x]` as {"a":"x]"}
    testCase "a closer stands only inside a flow collection and is text in a plain scalar of a block" <| fun _ ->
        expectLexemes "]" [ "error:unopened-flow" ]
        expectLexemes "a: x]" [ "plain:a"; ":"; "plain:x]" ]
        expectLexemes "a: [x]]" [ "plain:a"; ":"; "["; "plain:x"; "]"; "error:unopened-flow" ]

    testCase "a comma outside a flow collection cannot start a node and is text inside a plain scalar" <| fun _ ->
        // yaml@2.6.1: `, a` is rejected with UNEXPECTED_TOKEN, `a, b` is the string "a, b"
        expectLexemes ", a" [ "error:reserved-indicator" ]
        expectLexemes "a, b" [ "plain:a, b" ]
]

let private propertyTests = testList "Lexer properties and aliases" [
    testCase "an anchor, a tag and an alias are lexemes of their own" <| fun _ ->
        expectLexemes "&a !!str b" [ "&a"; "tag:!!|str"; "plain:b" ]
        expectLexemes "*a" [ "*a" ]
        expectLexemes "k: &x v\nj: *x" [ "plain:k"; ":"; "&x"; "plain:v"; "plain:j"; ":"; "*x" ]

    // yaml@2.6.1 reads `!<tag:yaml.org,2002:str> x`, `!local x`, `! x`, `!e!suffix x` after a %TAG directive and `!!str` as tags
    testCase "a tag is a verbatim uri or a handle and a suffix" <| fun _ ->
        expectLexemes "!<tag:yaml.org,2002:str> x" [ "!<tag:yaml.org,2002:str>"; "plain:x" ]
        expectLexemes "!local x" [ "tag:!|local"; "plain:x" ]
        expectLexemes "! x" [ "tag:!|"; "plain:x" ]
        expectLexemes "!e!suffix x" [ "tag:!e!|suffix"; "plain:x" ]
        expectLexemes "!!str x" [ "tag:!!|str"; "plain:x" ]
        expectLexemes "!ns:thing/a.b x" [ "tag:!|ns:thing/a.b"; "plain:x" ]

    testCase "a tag keeps its percent escapes and a verbatim tag may hold commas and brackets" <| fun _ ->
        expectLexemes "!a%20b x" [ "tag:!|a%20b"; "plain:x" ]
        expectLexemes "!<a,b[c]> x" [ "!<a,b[c]>"; "plain:x" ]

    // yaml@2.6.1 reads `a: !t'y p` as {"a":"p"}, a tag with a single quote in its name, and `!tag"x"` is rejected for the missing blank. The lexer
    // ends a tag at a double quote and leaves the blank to the parser.
    testCase "a quote inside a tag name is part of the name and a double quote ends it" <| fun _ ->
        expectLexemes "!t'y p" [ "tag:!|t'y"; "plain:p" ]
        expectLexemes "!tag\"x\"" [ "tag:!|tag"; "double:x" ]

    testCase "a tag in a flow collection ends at a flow indicator" <| fun _ ->
        expectLexemes "[!t, a]" [ "["; "tag:!|t"; ","; "plain:a"; "]" ]
        expectLexemes "[!!str]" [ "["; "tag:!!|str"; "]" ]

    // yaml@2.6.1 reads `x: &a:b 1` and `k: *a:b` with the name "a:b", and `&a#c` and `*a'q'` as names too
    testCase "an anchor name and an alias name may hold colons, quotes and hashes" <| fun _ ->
        expectLexemes "&a:b c" [ "&a:b"; "plain:c" ]
        expectLexemes "*a'q'" [ "*a'q'" ]
        expectLexemes "&a#c" [ "&a#c" ]
        expectLexemes "*a:" [ "*a:" ]

    testCase "a name ends at a flow indicator" <| fun _ ->
        expectLexemes "[&a x, *b]" [ "["; "&a"; "plain:x"; ","; "*b"; "]" ]
        expectLexemes "[*a,*b]" [ "["; "*a"; ","; "*b"; "]" ]

    // yaml@2.6.1 rejects `&` and `*` with BAD_ALIAS ("Anchor cannot be an empty string")
    testCase "an anchor and an alias need a name" <| fun _ ->
        expectError "&" "empty-anchor" "1:1-1:2"
        expectError "k: &\n" "empty-anchor" "1:4-1:5"
        expectError "*" "empty-alias" "1:1-1:2"
        expectError "[&]" "empty-anchor" "1:2-1:3"

    testCase "a tag needs a name and a closing bracket and valid escapes" <| fun _ ->
        expectError "!!" "invalid-tag" "1:1-1:3"
        expectError "!e! x" "invalid-tag" "1:1-1:4"
        expectError "!<>" "invalid-tag" "1:1-1:4"
        expectError "!<abc" "invalid-tag" "1:1-1:6"
        expectError "!a%2" "invalid-tag" "1:3-1:5"
        expectError "!a%zz" "invalid-tag" "1:3-1:6"

    // yaml@2.6.1 reads `!<{x}>` as the empty string and `!<a%2> x` without an error. [98] c-verbatim-tag holds ns-uri-char+ ([39]) only, which
    // are the alphanumerics, `-;/?:@&=+$,_.!~*'()[]#` and a `%` followed by two hexadecimal digits.
    testCase "a verbatim tag holds URI characters only" <| fun _ ->
        expectError "!<{x}>" "invalid-tag" "1:3-1:4"
        expectError "!<a b>" "invalid-tag" "1:1-1:4"
        expectError "!<a\"b>" "invalid-tag" "1:4-1:5"
        expectError "!<a%2> x" "invalid-tag" "1:4-1:7"
        expectError "!<a%zz> x" "invalid-tag" "1:4-1:7"
        expectError "!<\u00E9>" "invalid-tag" "1:3-1:4"
        expectLexemes "!<a%20b> x" [ "!<a%20b>"; "plain:x" ]
        expectLexemes "!<-;/?:@&=+$,_.!~*'()[]#> x" [ "!<-;/?:@&=+$,_.!~*'()[]#>"; "plain:x" ]

    // yaml@2.6.1 rejects `!<x>#c` with MISSING_CHAR. A verbatim tag ends at its bracket, and the hash cannot start a comment without a blank.
    testCase "a hash right behind a verbatim tag is a glued comment" <| fun _ ->
        expectError "!<x>#c" "glued-comment" "1:5-1:6"
        expectError "k: !<x>#c" "glued-comment" "1:8-1:9"
        expectError "[!<x>#c]" "glued-comment" "1:6-1:7"
        expectLexemes "!<x> #c" [ "!<x>"; "#:c" ]
        // A hash inside the name of a shorthand tag, an anchor or an alias is part of the name.
        expectLexemes "!x#c y" [ "tag:!|x#c"; "plain:y" ]
]

let private commentTests = testList "Lexer comments" [
    testCase "a comment on a line of its own and a comment behind a node" <| fun _ ->
        expectFull "# c" [ "stream-start"; "#own: c"; "stream-end" ]
        expectLexemes "a # c" [ "plain:a"; "#: c" ]
        expectLexemes "a #c" [ "plain:a"; "#:c" ]
        expectLexemes "a\t# c" [ "plain:a"; "#: c" ]
        expectLexemes "k: 'x' # c" [ "plain:k"; ":"; "single:x"; "#: c" ]
        expectLexemes "- # c\n  a" [ "-"; "#: c"; "plain:a" ]

    testCase "a comment keeps everything behind the hash up to the line break" <| fun _ ->
        expectLexemes "# a # b" [ "#own: a # b" ]
        expectLexemes "#" [ "#own:" ]
        expectLexemes "  #x\n" [ "#own:x" ]

    // yaml@2.6.1 reads `a#c` as "a#c", `k: b#c` as {"k":"b#c"} and `C#` as "C#". A hash starts a comment behind a blank only.
    testCase "a hash that follows a character of a plain scalar is text" <| fun _ ->
        expectLexemes "a#c" [ "plain:a#c" ]
        expectLexemes "k: b#c" [ "plain:k"; ":"; "plain:b#c" ]
        expectLexemes "k: C#" [ "plain:k"; ":"; "plain:C#" ]
        expectLexemes "k: tool.cwl#main" [ "plain:k"; ":"; "plain:tool.cwl#main" ]

    // yaml@2.6.1 rejects `k: "b"#c`, `k: 'b'#c`, `k: [a]#c` and `k: {a: 1}#c` with MISSING_CHAR
    testCase "a hash right behind a closed quoted scalar or flow collection is an error" <| fun _ ->
        expectError "k: \"b\"#c" "glued-comment" "1:7-1:8"
        expectError "k: 'b'#c" "glued-comment" "1:7-1:8"
        expectError "k: [a]#c" "glued-comment" "1:7-1:8"
        expectError "k: {a: 1}#c" "glued-comment" "1:10-1:11"
        expectLexemes "k: \"b\" #c" [ "plain:k"; ":"; "double:b"; "#:c" ]

    testCase "a hash at the start of a node behind a flow indicator is no comment and no scalar" <| fun _ ->
        // yaml@2.6.1 rejects `[#a]` with UNEXPECTED_TOKEN
        expectLexemes "[#a]" [ "["; "error:reserved-indicator" ]
        expectLexemes "[a,#b]" [ "["; "plain:a"; ","; "error:reserved-indicator" ]
]

let private markerTests = testList "Lexer directives and document markers" [
    testCase "a marker at the start of a line" <| fun _ ->
        expectFull "---\na" [ "stream-start"; "line:0"; "---"; "line:0"; "plain:a"; "stream-end" ]
        expectLexemes "--- a" [ "---"; "plain:a" ]
        expectLexemes "--- # c" [ "---"; "#: c" ]
        expectLexemes "a: 1\n...\nb: 2" [ "plain:a"; ":"; "plain:1"; "..."; "plain:b"; ":"; "plain:2" ]
        expectLexemes "--- |\n x\n" [ "---"; "literal:x\\n" ]
        expectLexemes "--- 'x'\n--- \"y\"" [ "---"; "single:x"; "---"; "double:y" ]

    // yaml@2.6.1 reads " ---" as "---", "----" as "----", "...x" as "...x" and "---a" as "---a". A marker needs a blank behind it and stands at column 1.
    testCase "text that looks like a marker is a plain scalar" <| fun _ ->
        expectLexemes " ---" [ "plain:---" ]
        expectLexemes "----" [ "plain:----" ]
        expectLexemes "...x" [ "plain:...x" ]
        expectLexemes "---a" [ "plain:---a" ]
        expectLexemes "k: ---" [ "plain:k"; ":"; "plain:---" ]

    testCase "a marker ends a plain scalar that would go on" <| fun _ ->
        expectLexemes "a\n---\nb" [ "plain:a"; "---"; "plain:b" ]
        expectLexemes "a\n...\nb" [ "plain:a"; "..."; "plain:b" ]
        expectLexemes "a\n ---\n..." [ "plain:a ---"; "..." ]

    // yaml@2.6.1: the directives are accepted in the prelude of a document
    testCase "a directive is a lexeme in the prelude" <| fun _ ->
        expectLexemes "%YAML 1.2\n---" [ "%YAML 1.2"; "---" ]
        expectLexemes "%TAG !e! tag:example.com,2000:\n---" [ "%TAG !e! tag:example.com,2000:"; "---" ]
        expectLexemes "%TAG ! tag:x,2000:\n%TAG !! tag:y,2000:\n---" [ "%TAG ! tag:x,2000:"; "%TAG !! tag:y,2000:"; "---" ]
        expectLexemes "%FOO bar baz\n---" [ "%FOO bar baz"; "---" ]
        expectLexemes "%FOO\n---" [ "%FOO "; "---" ]

    testCase "a comment behind a directive and before it" <| fun _ ->
        expectLexemes "%YAML 1.2 # c\n---" [ "%YAML 1.2"; "#: c"; "---" ]
        expectLexemes "# lead\n%YAML 1.2\n# mid\n---" [ "#own: lead"; "%YAML 1.2"; "#own: mid"; "---" ]
        expectLexemes "%TAG !e! tag:it's # c\n---" [ "%TAG !e! tag:it's"; "#: c"; "---" ]
        expectLexemes "%TAG !e! tag:it's \"x\" # c\n---" [ "error:invalid-directive" ]

    // yaml@2.6.1 reads the first input as {"a":1} and {"b":2} and rejects the others with BAD_SCALAR_START
    testCase "a directive follows an end marker only and every start marker ends the prelude" <| fun _ ->
        expectLexemes "a: 1\n...\n%YAML 1.2\n---\nb: 2" [ "plain:a"; ":"; "plain:1"; "..."; "%YAML 1.2"; "---"; "plain:b"; ":"; "plain:2" ]
        expectError "a: 1\n%YAML 1.2\n---\nb: 2" "reserved-indicator" "2:1-2:2"
        expectError "---\n%TAG ! t!\n" "reserved-indicator" "2:1-2:2"
        // yaml@2.6.1 reads "a\n%YAML 1.2" as the string "a %YAML 1.2". A line that goes on a plain scalar may start with a percent sign.
        expectLexemes "a\n%YAML 1.2" [ "plain:a %YAML 1.2" ]

    // yaml@2.6.1 rejects `%YAML 1.2 x` ("should contain exactly one part"), `%YAML x`, and `%TAG !e! ` ("exactly two parts")
    testCase "a directive with the wrong parts is an error" <| fun _ ->
        expectError "%YAML\n---" "invalid-directive" "1:1-1:6"
        expectError "%YAML 1.2 x\n---" "invalid-directive" "1:1-1:12"
        expectError "%YAML x\n---" "invalid-directive" "1:7-1:8"
        expectError "%YAML 1.\n---" "invalid-directive" "1:7-1:9"
        expectError "%TAG !e!\n---" "invalid-directive" "1:1-1:9"
        expectError "%TAG e! x\n---" "invalid-directive" "1:6-1:8"
        expectError "%TAG !e!x y\n---" "invalid-directive" "1:6-1:10"
        expectError "%\n---" "invalid-directive" "1:1-1:2"

    // yaml@2.6.1 reads `% YAML 1.2\n---\na` as "a". [82] l-directive and [84] ns-directive-name put the name right behind the `%`.
    testCase "a blank behind the percent sign leaves a directive without a name" <| fun _ ->
        expectError "% YAML 1.2\n---" "invalid-directive" "1:1-1:2"
        expectError "%\tTAG !e! x\n---" "invalid-directive" "1:1-1:2"
        expectError "% \n---" "invalid-directive" "1:1-1:2"
        expectLexemes "%YAML 1.2\n---" [ "%YAML 1.2"; "---" ]

    // yaml@2.6.1 reads `%TAG !e! {bad}\n---\n!e!x y` as "y". [93] to [95] build the prefix of ns-uri-char ([39]), and a global prefix
    // starts with an ns-tag-char ([40]), which is no `!` and no flow indicator.
    testCase "the prefix of a tag directive holds URI characters only" <| fun _ ->
        expectError "%TAG !e! {bad}\n---" "invalid-directive" "1:10-1:11"
        expectError "%TAG !e! a\"b\n---" "invalid-directive" "1:11-1:12"
        expectError "%TAG !e! a%zz\n---" "invalid-directive" "1:11-1:14"
        expectError "%TAG !e! a%\n---" "invalid-directive" "1:11-1:12"
        expectError "%TAG !e! ,x\n---" "invalid-directive" "1:10-1:11"
        expectError "%TAG !e! [x]\n---" "invalid-directive" "1:10-1:11"
        expectLexemes "%TAG !e! !local,[x]%20\n---" [ "%TAG !e! !local,[x]%20"; "---" ]
        expectLexemes "%TAG !e! tag:x,2000:[y]%41\n---" [ "%TAG !e! tag:x,2000:[y]%41"; "---" ]

    // yaml@2.6.1: {"a":"b\n%TAG\n"}. A percent sign inside a block scalar body is content.
    testCase "a percent sign inside a block scalar body is content" <| fun _ ->
        expectLexemes "a: |\n  b\n  %TAG\n" [ "plain:a"; ":"; "literal:b\\n%TAG\\n" ]
]

let private tabTests = testList "Lexer tabs" [
    // yaml@2.6.1 rejects each of these with TAB_AS_INDENT
    testCase "a tab in the indentation in front of a block indicator is an error" <| fun _ ->
        expectError "\t- a" "tab-indentation" "1:1-1:2"
        expectError " \t- a" "tab-indentation" "1:1-1:3"
        expectError "a:\n \t- b" "tab-indentation" "2:1-2:3"
        expectError "? a\n\t: b" "tab-indentation" "2:1-2:2"

    // yaml@2.6.1 rejects each of these with TAB_AS_INDENT, because a tab cannot separate an indicator from a block collection behind it
    testCase "a tab between two block indicators is an error" <| fun _ ->
        expectError "-\t- a" "tab-indentation" "1:2-1:3"
        expectError "- \t- a" "tab-indentation" "1:2-1:4"
        expectError "?\t- a" "tab-indentation" "1:2-1:3"
        expectError "a:\t- b" "tab-indentation" "1:3-1:4"

    // yaml@2.6.1 reads each of these without an error: {"a":"b"}, ["a"], ["a"] and {"a":["b"]} for the tab behind the dash
    testCase "a tab in front of a scalar or behind an indicator is separation" <| fun _ ->
        expectLexemes "a:\tb" [ "plain:a"; ":"; "plain:b" ]
        expectLexemes "- \ta" [ "-"; "plain:a" ]
        expectLexemes "---\ta" [ "---"; "plain:a" ]
        expectLexemes "?\ta" [ "?"; "plain:a" ]

    // Ported from the earlier test "a tab in the indentation or between an indicator and a block collection is an error".
    // yaml@2.6.1 rejects these inputs with TAB_AS_INDENT. The lexer cannot tell whether a block collection follows,
    // so it reports the tab on the line start and the parser makes the decision.
    testCase "a tab in front of a node that could be a key is reported on the line start for the parser" <| fun _ ->
        expectFull "k: a\n\tb" [ "stream-start"; "line:0"; "plain:k"; ":"; "plain:a"; "line:0,tab"; "plain:b"; "stream-end" ]
        expectFull "k:\n\tv" [ "stream-start"; "line:0"; "plain:k"; ":"; "line:0,tab"; "plain:v"; "stream-end" ]
        expectFull "- a\n\tb" [ "stream-start"; "line:0"; "-"; "plain:a"; "line:0,tab"; "plain:b"; "stream-end" ]
        expectFull "k: a\n\n\tb" [ "stream-start"; "line:0"; "plain:k"; ":"; "plain:a"; "blank"; "line:0,tab"; "plain:b"; "stream-end" ]
        expectFull "k:\n  \ta: b" [ "stream-start"; "line:0"; "plain:k"; ":"; "line:2,tab"; "plain:a"; ":"; "plain:b"; "stream-end" ]
        expectFull "a: b\n# c\n\tc: d" [ "stream-start"; "line:0"; "plain:a"; ":"; "plain:b"; "#own: c"; "line:0,tab"; "plain:c"; ":"; "plain:d"; "stream-end" ]

    // Ported from earlier tests. yaml@2.6.1 rejects `-\tk: v` and `---\tk: v` with TAB_AS_INDENT. The tab flag of the cursor
    // lets the parser see that the key behind the tab starts a block mapping.
    testCase "a tab behind an indicator in front of a scalar sets the tab flag of the scalar" <| fun _ ->
        let flagsOf (input: string) =
            let cursor = Cursor(input)
            let context = { Grammar = GrammarContext.BlockOut; ParentIndent = -1; SimpleKey = false }
            let flags = ResizeArray<string>()
            let mutable finished = false
            while not finished do
                match Lexer.next context cursor with
                | Ok lexeme ->
                    if cursor.SeparationHadTab then flags.Add(show lexeme.Value)
                    if lexeme.Value = Lexeme.StreamEnd then finished <- true
                | Error failure -> failwith (describeError failure)
            List.ofSeq flags
        Expect.equal (flagsOf "-\tk: v") [ "plain:k" ] "Behind the dash"
        Expect.equal (flagsOf "---\tk: v") [ "plain:k" ] "Behind the document start marker"
        Expect.equal (flagsOf "?\tk: v") [ "plain:k" ] "Behind the question mark"
        Expect.equal (flagsOf "k:\n  -\tk: v") [ "plain:k" ] "Behind a nested dash"
        Expect.equal (flagsOf "- k: v") [] "A blank is no tab"

    testCase "a tab in a flow collection is separation" <| fun _ ->
        // yaml@2.6.1: ["a","b"] for both inputs
        expectLexemes "[a,\tb]" [ "["; "plain:a"; ","; "plain:b"; "]" ]
        expectLexemes "[a,\n \tb]" [ "["; "plain:a"; ","; "plain:b"; "]" ]

    // yaml@2.6.1 rejects `a: [\n\tb]` ("must be sufficiently indented"), because a tab is no indentation, and reads `a: [\n \tb]` as {"a":["b"]}. ruamel reads both.
    testCase "a flow line may start with a tab" <| fun _ ->
        expectLexemes "a: [\n\tb]" [ "plain:a"; ":"; "["; "plain:b"; "]" ]
        expectLexemes "a: [\n \tb]" [ "plain:a"; ":"; "["; "plain:b"; "]" ]

    testCase "a tab inside a plain scalar and the tab flag of the cursor" <| fun _ ->
        expectLexemes "a\tb" [ "plain:a\\tb" ]
        let cursor = Cursor("a:\tb")
        let context = { Grammar = GrammarContext.BlockOut; ParentIndent = -1; SimpleKey = false }
        let flags = ResizeArray<bool>()
        let mutable finished = false
        while not finished do
            match Lexer.next context cursor with
            | Ok lexeme ->
                flags.Add cursor.SeparationHadTab
                if lexeme.Value = Lexeme.StreamEnd then finished <- true
            | Error failure -> failwith (describeError failure)
        // stream-start, line start, `a`, `:`, `b`, stream-end
        Expect.equal (List.ofSeq flags) [ false; false; false; false; true; false ] "Only the scalar behind the tab reports it"
]

let private contextTests = testList "Lexer contexts" [
    testCase "a plain scalar goes on in the lines that are deeper than the parent" <| fun _ ->
        Expect.equal (lexIn GrammarContext.BlockOut 0 false "a\n  b") [ "plain:a b" ] "Deeper than the parent"
        Expect.equal (lexIn GrammarContext.BlockOut 2 false "a\n  b") [ "plain:a"; "plain:b" ] "As deep as the parent"
        Expect.equal (lexIn GrammarContext.BlockOut -1 false "a\nb") [ "plain:a b" ] "At the root every line goes on"
        Expect.equal (lexIn GrammarContext.BlockOut 0 false "a\nb") [ "plain:a"; "plain:b" ] "Not deeper than the parent"

    testCase "the key contexts and a simple key keep a plain scalar on its line" <| fun _ ->
        // yaml@2.6.1 rejects `a\n b: c` with MULTILINE_IMPLICIT_KEY. The parser asks for a simple key where only a key can stand.
        Expect.equal (lexIn GrammarContext.BlockOut -1 false "a\n b: c") [ "plain:a b"; ":"; "plain:c" ] "A value"
        Expect.equal (lexIn GrammarContext.BlockOut -1 true "a\n b: c") [ "plain:a"; "plain:b"; ":"; "plain:c" ] "A simple key"
        Expect.equal (lexIn GrammarContext.BlockKey -1 false "a\n b: c") [ "plain:a"; "plain:b"; ":"; "plain:c" ] "A block key"
        Expect.equal (lexIn GrammarContext.FlowKey -1 false "a\n b: c") [ "plain:a"; "plain:b"; ":"; "plain:c" ] "A flow key"

    testCase "a flow indicator ends a plain scalar in the flow contexts only" <| fun _ ->
        Expect.equal (lexIn GrammarContext.FlowOut -1 false "a,b]") [ "plain:a,b]" ] "FlowOut"
        Expect.equal (lexIn GrammarContext.BlockOut -1 false "a,b]") [ "plain:a,b]" ] "BlockOut"
        Expect.equal (lexIn GrammarContext.FlowIn -1 false "a,b]") [ "plain:a"; ","; "plain:b"; "]" ] "FlowIn"
        Expect.equal (lexIn GrammarContext.FlowKey -1 false "a,b]") [ "plain:a"; ","; "plain:b"; "]" ] "FlowKey"

    testCase "a block scalar cannot start in a flow collection or as an implicit key" <| fun _ ->
        Expect.equal (lexIn GrammarContext.FlowIn -1 false "|\n x") [ "error:block-scalar-position" ] "FlowIn"
        Expect.equal (lexIn GrammarContext.BlockKey -1 false "|\n x") [ "error:block-scalar-position" ] "BlockKey"
        Expect.equal (lexIn GrammarContext.BlockOut -1 false "|\n x\n") [ "literal:x\\n" ] "BlockOut"

    testCase "the block indicators are lexemes in a block context and the flow rules apply in a flow context" <| fun _ ->
        Expect.equal (lexIn GrammarContext.FlowOut -1 false "- a") [ "-"; "plain:a" ] "FlowOut treats a dash like a block context"
        Expect.equal (lexIn GrammarContext.FlowIn -1 false "- a") [ "error:flow-dash" ] "FlowIn"
        Expect.equal (lexIn GrammarContext.FlowIn -1 false "a: b") [ "plain:a"; ":"; "plain:b" ] "A colon behind a blank"
        Expect.equal (lexIn GrammarContext.BlockOut -1 false "a:,") [ "plain:a:," ] "A comma behind a colon is text in a block"

    // yaml@2.6.1 rejects a line of a flow collection that is not indented deeper than the key that owns it, and reads the closer of the outermost collection at the column of the key. ruamel reads every input below.
    // The lexer takes the lines of a flow collection at any indentation, because the collection runs to its closer.
    // The first line of every input starts with blanks, because the lexer treats the first line of its input as a continuation line too.
    testCase "a line of a flow collection stands at any indentation" <| fun _ ->
        Expect.equal (lexIn GrammarContext.FlowIn 0 false "  a,\n  b") [ "plain:a"; ","; "plain:b" ] "Deeper"
        Expect.equal (lexIn GrammarContext.FlowIn 0 false "  a,\nb") [ "plain:a"; ","; "plain:b" ] "As deep as the parent"
        Expect.equal (lexIn GrammarContext.FlowIn 2 false "    a,\n b") [ "plain:a"; ","; "plain:b" ] "Left of the parent"
        Expect.equal (lexIn GrammarContext.FlowIn 0 false "  a,\n]") [ "plain:a"; ","; "]" ] "A closer at the parent"
        Expect.equal (lexIn GrammarContext.FlowIn 2 false "    a,\n  ]") [ "plain:a"; ","; "]" ] "A closer at the column of the parent"
        Expect.equal (lexIn GrammarContext.FlowIn 2 false "    a,\n ]") [ "plain:a"; ","; "]" ] "A closer left of the parent"
        Expect.equal (lexIn GrammarContext.FlowIn 0 false "  a,\n\t]") [ "plain:a"; ","; "]" ] "A tab in front of the closer"
        Expect.equal (lexIn GrammarContext.FlowIn 2 false "    a,\n \t]") [ "plain:a"; ","; "]" ] "A tab behind too few spaces"
        Expect.equal (lexIn GrammarContext.FlowIn 2 false "    a,\n\t]") [ "plain:a"; ","; "]" ] "A tab and no spaces, with the parent at the column 2"
        Expect.equal (lexIn GrammarContext.FlowIn 0 false "  a,\n# c\nb") [ "plain:a"; ","; "#own: c"; "plain:b" ] "A comment line stands anywhere"
        Expect.equal (lexIn GrammarContext.FlowIn -1 false "a,\nb") [ "plain:a"; ","; "plain:b" ] "At the root every line is deep enough"

    // yaml@2.6.1 rejects `k: [ a` followed by `b ]` (BAD_INDENT) and reads a second line that stands deeper than the key as "a b". ruamel reads both as "a b". It rejects a second line that starts with a tab
    // behind too few spaces and a comment line in between, because the scalar ends there and a second entry follows without a comma.
    testCase "a plain scalar in a flow collection goes on in a line at any indentation" <| fun _ ->
        Expect.equal (lexIn GrammarContext.FlowIn 2 false "    a\nb") [ "plain:a b" ] "Left of the parent"
        Expect.equal (lexIn GrammarContext.FlowIn 2 false "    a\n  b") [ "plain:a b" ] "At the column of the parent"
        Expect.equal (lexIn GrammarContext.FlowIn 2 false "    a\n\n b") [ "plain:a\\nb" ] "After an empty line"
        Expect.equal (lexIn GrammarContext.FlowIn 2 false "    a\n   \tb") [ "plain:a b" ] "A tab behind enough spaces"
        Expect.equal (lexIn GrammarContext.FlowIn 2 false "    a\n \tb") [ "plain:a"; "plain:b" ] "A tab behind too few spaces ends the scalar"
        Expect.equal (lexIn GrammarContext.FlowIn 2 false "    a\n\tb") [ "plain:a"; "plain:b" ] "A tab and no space ends the scalar"
        Expect.equal (lexIn GrammarContext.FlowIn 2 false "    a\n# c\nb") [ "plain:a"; "#own: c"; "plain:b" ] "A comment line ends the scalar"

    // yaml@2.6.1 rejects `k: "b\n\t\n  c"` and `k: b\n\t\n  c` (BAD_INDENT), and reads `"b\n\t\n  c"` and `b\n\t\n  c` at the root as "b\nc".
    // [70] l-empty(n,c) is n spaces and then any blanks, or fewer than n spaces and nothing else, with n one above the parent. A tab behind
    // fewer spaces makes the line no empty line, so a plain scalar ends in front of it. A quoted scalar reads the line at any indentation, in a flow collection as well.
    testCase "an empty line inside a folded scalar holds a tab only behind the indentation" <| fun _ ->
        Expect.equal (lexIn GrammarContext.BlockOut 0 false "b\n\t\n  c") [ "plain:b"; "blank"; "plain:c" ] "A tab with no space under parent 0"
        Expect.equal (lexIn GrammarContext.BlockOut 0 false "b\n \t\n  c") [ "plain:b\\nc" ] "One space and a tab under parent 0"
        Expect.equal (lexIn GrammarContext.BlockOut 2 false "b\n  \t\n    c") [ "plain:b"; "blank"; "plain:c" ] "Two spaces and a tab under parent 2"
        Expect.equal (lexIn GrammarContext.BlockOut 2 false "b\n   \t\n    c") [ "plain:b\\nc" ] "Three spaces and a tab under parent 2"
        Expect.equal (lexIn GrammarContext.BlockOut -1 false "b\n\t\n  c") [ "plain:b\\nc" ] "At the root"
        Expect.equal (lexIn GrammarContext.FlowIn 0 false "  b\n\t\n  c") [ "plain:b"; "blank"; "plain:c" ] "In a flow collection"
        Expect.equal (lexIn GrammarContext.FlowIn 0 false "  \"b\n\t\n  c\"") [ "double:b\\nc" ] "A quoted scalar in a flow collection under parent 0"
        Expect.equal (lexIn GrammarContext.FlowIn 0 false "  \"b\\\n\t\n  c\"") [ "double:b\\nc" ] "A quoted scalar after an escaped line break"
        Expect.equal (lexIn GrammarContext.FlowIn 0 false "  \"b\n \t\n  c\"") [ "double:b\\nc" ] "One space and a tab in a quoted scalar"
        Expect.equal (lexIn GrammarContext.BlockOut 0 false "\"b\n\t\n  c\"") [ "double:b\\nc" ] "A tab and no space in a quoted scalar of the block structure"
        Expect.equal (lexIn GrammarContext.BlockOut 0 false "\"b\\\n\t\n  c\"") [ "double:b\\nc" ] "After an escaped line break"
        Expect.equal (lexIn GrammarContext.BlockOut -1 false "\"b\n\t\n  c\"") [ "double:b\\nc" ] "A quoted scalar at the root"
        expectLexemes "k: [\"b\n\t\n  c\"]" [ "plain:k"; ":"; "["; "double:b\\nc"; "]" ]
        expectLexemes "k: ['b\n \n\t\n  c']" [ "plain:k"; ":"; "["; "single:b\\n\\nc"; "]" ]

    testCase "a document marker inside a flow collection is an error" <| fun _ ->
        Expect.equal (lexIn GrammarContext.FlowIn -1 false "a,\n---") [ "plain:a"; ","; "error:marker-in-flow" ] "Start marker"
        Expect.equal (lexIn GrammarContext.FlowIn -1 false "a,\n...") [ "plain:a"; ","; "error:marker-in-flow" ] "End marker"
]

let private positionTests = testList "Lexer positions" [
    testCase "a lexeme has a range from its first character to the one behind its last" <| fun _ ->
        Expect.equal (lexSpans "a: b") [ "plain:a 1:1-1:2"; ": 1:2-1:3"; "plain:b 1:4-1:5" ] "Plain scalars and a colon"
        Expect.equal (lexSpans "- 'x'") [ "- 1:1-1:2"; "single:x 1:3-1:6" ] "A dash and a quoted scalar"
        Expect.equal (lexSpans "a # c") [ "plain:a 1:1-1:2"; "#: c 1:3-1:6" ] "A comment"
        Expect.equal (lexSpans "  # c") [ "#own: c 1:3-1:6" ] "A comment line"
        Expect.equal (lexSpans "&a !!str *b") [ "&a 1:1-1:3"; "tag:!!|str 1:4-1:9"; "*b 1:10-1:12" ] "Properties and an alias"

    testCase "a column counts one for a character above U+FFFF" <| fun _ ->
        // A .NET string and a JavaScript string hold the emoji as two UTF-16 code units, a Python string as one character.
        Expect.equal (lexSpans "a: \U0001F600x") [ "plain:a 1:1-1:2"; ": 1:2-1:3"; "plain:\U0001F600x 1:4-1:6" ] "In a value"
        Expect.equal (lexSpans "\U0001F600: x") [ "plain:\U0001F600 1:1-1:2"; ": 1:2-1:3"; "plain:x 1:4-1:5" ] "In a key"
        Expect.equal (lexSpans "\U0001F600\U0001F600 x # c") [ "plain:\U0001F600\U0001F600 x 1:1-1:5"; "#: c 1:6-1:9" ] "Two of them"
        Expect.equal (lexSpans "k: \"\U0001F600\" # c") [ "plain:k 1:1-1:2"; ": 1:2-1:3"; "double:\U0001F600 1:4-1:7"; "#: c 1:8-1:11" ] "In a quoted scalar"

    testCase "a line ends at LF, CRLF and a lone CR" <| fun _ ->
        Expect.equal (lexSpans "a: b\nc: d") [ "plain:a 1:1-1:2"; ": 1:2-1:3"; "plain:b 1:4-1:5"; "plain:c 2:1-2:2"; ": 2:2-2:3"; "plain:d 2:4-2:5" ] "LF"
        Expect.equal (lexSpans "a: b\r\nc: d") [ "plain:a 1:1-1:2"; ": 1:2-1:3"; "plain:b 1:4-1:5"; "plain:c 2:1-2:2"; ": 2:2-2:3"; "plain:d 2:4-2:5" ] "CRLF"
        Expect.equal (lexSpans "a: b\rc: d") [ "plain:a 1:1-1:2"; ": 1:2-1:3"; "plain:b 1:4-1:5"; "plain:c 2:1-2:2"; ": 2:2-2:3"; "plain:d 2:4-2:5" ] "CR"
        Expect.equal (lexSpans "a: b\r\n\r\nc: d\n\re: f") [ "plain:a 1:1-1:2"; ": 1:2-1:3"; "plain:b 1:4-1:5"; "blank 2:1-2:1"; "plain:c 3:1-3:2"; ": 3:2-3:3"; "plain:d 3:4-3:5"; "blank 4:1-4:1"; "plain:e 5:1-5:2"; ": 5:2-5:3"; "plain:f 5:4-5:5" ] "Mixed"

    testCase "a tab is one column" <| fun _ ->
        Expect.equal (lexSpans "\ta: b") [ "plain:a 1:2-1:3"; ": 1:3-1:4"; "plain:b 1:5-1:6" ] "A tab in front"
        Expect.equal (lexSpans "a:\tb") [ "plain:a 1:1-1:2"; ": 1:2-1:3"; "plain:b 1:4-1:5" ] "A tab behind the colon"

    testCase "a scalar over several lines spans from its first to its last character" <| fun _ ->
        Expect.equal (lexSpans "k:\n  a\n  b") [ "plain:k 1:1-1:2"; ": 1:2-1:3"; "plain:a b 2:3-3:4" ] "Plain"
        Expect.equal (lexSpans "\"a\n b\"") [ "double:a b 1:1-2:4" ] "Double-quoted"
        Expect.equal (lexSpans "k: |\n  a\n  b\nj: 1") [ "plain:k 1:1-1:2"; ": 1:2-1:3"; "literal:a\\nb\\n 1:4-3:4"; "plain:j 4:1-4:2"; ": 4:2-4:3"; "plain:1 4:4-4:5" ] "Block"
        Expect.equal (lexSpans "k: |\n\nj: 1") [ "plain:k 1:1-1:2"; ": 1:2-1:3"; "literal: 1:4-1:5"; "blank 2:1-2:1"; "plain:j 3:1-3:2"; ": 3:2-3:3"; "plain:1 3:4-3:5" ] "A block scalar without content ends at its header and leaves the empty line to the parser"
        Expect.equal (lexSpans "k: | # c\nj: 1") [ "plain:k 1:1-1:2"; ": 1:2-1:3"; "literal: 1:4-1:5"; "plain:j 2:1-2:2"; ": 2:2-2:3"; "plain:1 2:4-2:5" ] "A header comment is no part of the span"
        Expect.equal (lexSpans "k: >- # c\n  a\nj: 1") [ "plain:k 1:1-1:2"; ": 1:2-1:3"; "folded-:a 1:4-2:4"; "plain:j 3:1-3:2"; ": 3:2-3:3"; "plain:1 3:4-3:5" ] "Nor is it with content"

    testCase "the line start, the blank line and the stream end have ranges too" <| fun _ ->
        let cursor = Cursor("  a\n\n  \n")
        let context = { Grammar = GrammarContext.BlockOut; ParentIndent = -1; SimpleKey = false }
        let spans = ResizeArray<string>()
        let mutable finished = false
        while not finished do
            match Lexer.next context cursor with
            | Ok lexeme ->
                spans.Add(show lexeme.Value + " " + showSpan lexeme.Span)
                if lexeme.Value = Lexeme.StreamEnd then finished <- true
            | Error failure -> failwith (describeError failure)
        Expect.equal
            (List.ofSeq spans)
            [ "stream-start 1:1-1:1"; "line:2 1:1-1:3"; "plain:a 1:3-1:4"; "blank 2:1-2:1"; "blank 3:1-3:3"; "stream-end 4:1-4:1" ]
            "The ranges"

    testCase "a byte order mark at the start of a line takes no column" <| fun _ ->
        let cursor = Cursor("a\n\uFEFFb\r\n\uFEFF\uFEFFc")
        Expect.equal (cursor.MarkAt 3) { Line = 2; Column = 1 } "The character behind the mark"
        Expect.equal (cursor.MarkAt 4) { Line = 2; Column = 2 } "The next character"
        Expect.equal (cursor.MarkAt 8) { Line = 3; Column = 1 } "The character behind a run of marks"
        Expect.equal (Cursor("a\uFEFFb").MarkAt 2) { Line = 1; Column = 3 } "A mark behind another character takes a column"

    testCase "marks are the same when they are asked out of order" <| fun _ ->
        let text = "a: 1\n\U0001F600b\r\nc"
        let cursor = Cursor(text)
        let fresh = Cursor(text)
        let indexes = [ 0; 2; 5; text.Length ]
        let first = [ for index in indexes -> cursor.MarkAt index ]
        let backwards = [ for index in List.rev indexes -> fresh.MarkAt index ] |> List.rev
        Expect.equal first backwards "The tracker restarts when a mark lies behind it"
]

let private errorTests = testList "Lexer errors" [
    // yaml@2.6.1 rejects each of these with MISSING_CHAR ("Missing closing quote")
    testCase "an unclosed quote is an error on the opening quote and names the line" <| fun _ ->
        expectError "a: 'x" "unclosed-quote" "1:4-1:5"
        expectError "a: \"x" "unclosed-quote" "1:4-1:5"
        expectError "a: 1\nb: 'p\n  c: 2" "unclosed-quote" "2:4-2:5"
        expectError "[a, \"b]" "unclosed-quote" "1:5-1:6"
        match errorOfInput "a: 1\nb: 'p\n  c" with
        | Some failure -> Expect.stringContains failure.Message "line 2" "The message names the line of the opening quote"
        | None -> failwith "The input is invalid"

    // yaml@2.6.1 rejects `\q`, `\x4"`, `\u00zz` ("Invalid escape sequence") and `\U00110000` (an invalid code point)
    testCase "an unknown or malformed escape is an error on the backslash and its characters" <| fun _ ->
        expectError "a: \"\\q\"" "invalid-escape" "1:5-1:7"
        expectError "a: \"\\x4\"" "invalid-escape" "1:5-1:9"
        expectError "a: \"\\u00zz\"" "invalid-escape" "1:5-1:11"
        expectError "a: \"\\U00110000\"" "invalid-escape" "1:5-1:15"
        expectError "a: \"\\U0000D800\"" "invalid-escape" "1:5-1:15"
        expectError "a: \"x\\" "unclosed-quote" "1:4-1:5"

    // yaml@2.6.1 rejects `|x`, `|11`, `|0` and `|+-` with "Block scalar header includes extra characters" and `|#c` for the missing blank
    testCase "a block scalar header allows one digit from 1 to 9, one chomping sign and a comment behind a blank" <| fun _ ->
        expectError "a: |x\n  b" "invalid-block-header" "1:4-1:6"
        expectError "a: |11\n  b" "invalid-block-header" "1:6-1:7"
        expectError "a: |0\n  b" "invalid-block-header" "1:4-1:6"
        expectError "a: |+-\n  b" "invalid-block-header" "1:6-1:7"
        expectError "a: |1 x\n  b" "invalid-block-header" "1:4-1:8"
        expectError "a: |#c\n  b" "glued-comment" "1:5-1:6"
        expectError "a: >-- \n  b" "invalid-block-header" "1:6-1:7"

    // Ported from the earlier test "a bar or an angle bracket at a node start needs a valid block scalar header".
    // yaml@2.6.1 rejects the first eight inputs with UNEXPECTED_TOKEN and `k: |# c` with MISSING_CHAR.
    testCase "a bar or an angle bracket at a node start needs a valid block scalar header" <| fun _ ->
        expectError "valueFrom: > 5" "invalid-block-header" "1:12-1:15"
        expectError "a: | extra" "invalid-block-header" "1:4-1:11"
        expectError "a: | extra\n  \"q\"\n" "invalid-block-header" "1:4-1:11"
        expectError "k: |+ x" "invalid-block-header" "1:4-1:8"
        expectError "k: >5x" "invalid-block-header" "1:4-1:7"
        expectError "| x" "invalid-block-header" "1:1-1:4"
        expectError "> x: y" "invalid-block-header" "1:1-1:5"
        expectError "a: |\n  b\nc: > 5" "invalid-block-header" "3:4-3:7"
        expectError "k: |# c" "glued-comment" "1:5-1:6"

    // Ported from earlier tests. yaml@2.6.1 rejects each of these with BAD_SCALAR_START
    testCase "a block scalar cannot start inside a flow collection" <| fun _ ->
        expectError "k: {\n  a: |\n}" "block-scalar-position" "2:6-2:7"
        expectError "[ > 5 ]" "block-scalar-position" "1:3-1:4"
        expectError "a: {b: |\n}\nc: 'x'\n" "block-scalar-position" "1:8-1:9"
        expectError "[\n  |\n    x\n]" "block-scalar-position" "2:3-2:4"
        expectError "{a: >}" "block-scalar-position" "1:5-1:6"

    // Ported from the earlier test "a closer without an open flow collection, a comma or a hash at a node start is an error".
    // yaml@2.6.1 rejects the closers with UNEXPECTED_TOKEN, `k: , a` and `- ,` with BAD_SCALAR_START and the hashes with MISSING_CHAR.
    testCase "a closer without an open flow collection, a comma or a hash at a node start is an error" <| fun _ ->
        expectError "k: ]" "unopened-flow" "1:4-1:5"
        expectError "k: a\n]" "unopened-flow" "2:1-2:2"
        expectError "k: }" "unopened-flow" "1:4-1:5"
        expectError "[a]]" "unopened-flow" "1:4-1:5"
        expectError "k: {a}}" "unopened-flow" "1:7-1:8"
        expectError "- ]" "unopened-flow" "1:3-1:4"
        expectError "k: , a" "reserved-indicator" "1:4-1:5"
        expectError "- ," "reserved-indicator" "1:3-1:4"
        expectError "[#a]" "reserved-indicator" "1:2-1:3"
        expectError "[a,#b]" "reserved-indicator" "1:4-1:5"
        expectError "{#a: 1}" "reserved-indicator" "1:2-1:3"

    // Ported from earlier tests. yaml@2.6.1 rejects each of these with BLOCK_IN_FLOW, because a plain scalar cannot be
    // a lone dash in a flow collection. It reads `{-: 1}` and `[-a]` without an error.
    testCase "a dash in front of a blank, a flow indicator or the end is an error in a flow collection" <| fun _ ->
        expectError "[- a]" "flow-dash" "1:2-1:3"
        expectError "[-]" "flow-dash" "1:2-1:3"
        expectError "{a: -}" "flow-dash" "1:5-1:6"
        expectError "[a, -]" "flow-dash" "1:5-1:6"
        expectError "[a, -,b]" "flow-dash" "1:5-1:6"
        expectLexemes "{-: 1}" [ "{"; "plain:-"; ":"; "plain:1"; "}" ]
        expectLexemes "k: -x" [ "plain:k"; ":"; "plain:-x" ]

    // yaml@2.6.1 rejects a quoted scalar that holds a document marker with MISSING_CHAR
    testCase "a document marker inside a quoted scalar is an error that relates to the opening quote" <| fun _ ->
        expectError "a: \"x\n---\ny\"" "marker-in-quote" "2:1-2:4"
        expectError "a: 'x\n...\ny'" "marker-in-quote" "2:1-2:4"
        match errorOfInput "a: \"x\n---\ny\"" with
        | Some failure -> Expect.equal (failure.Related |> List.map showSpan) [ "1:4-1:5" ] "The opening quote"
        | None -> failwith "The input is invalid"

    // yaml@2.6.1 rejects the texts below with errors such as MISSING_CHAR and BAD_INDENT. The lexer reads a line of a quoted scalar at any indentation,
    // because the scalar runs to its closing quote. ruamel reads these layouts with the same value.
    testCase "a line of a quoted scalar needs no indentation" <| fun _ ->
        expectLexemes "a: \"x\ny\"" [ "plain:a"; ":"; "double:x y" ]
        expectLexemes "- 'x\ny'" [ "-"; "single:x y" ]
        expectLexemes "a:\n  b: \"x\ny\"" [ "plain:a"; ":"; "plain:b"; ":"; "double:x y" ]
        expectLexemes "a:\n  b: 'x\n y'" [ "plain:a"; ":"; "plain:b"; ":"; "single:x y" ]
        expectLexemes "a:\n  b: \"x\n\ty\"" [ "plain:a"; ":"; "plain:b"; ":"; "double:x y" ]
        expectLexemes "a: [\"x\ny\"]" [ "plain:a"; ":"; "["; "double:x y"; "]" ]
        expectLexemes "- ['x\ny']" [ "-"; "["; "single:x y"; "]" ]
        expectLexemes "a:\n  b: [\"x\n  y\"]" [ "plain:a"; ":"; "plain:b"; ":"; "["; "double:x y"; "]" ]
        Expect.equal (lexIn GrammarContext.BlockOut 3 false "\"x\ny\"") [ "double:x y" ] "Left of the parent"
        Expect.equal (lexIn GrammarContext.BlockOut 3 false "\"x\n   y\"") [ "double:x y" ] "At the column of the parent"
        Expect.equal (lexIn GrammarContext.FlowIn 0 false "  \"x\ny\"") [ "double:x y" ] "A flow collection, as deep as the parent"
        Expect.equal (lexIn GrammarContext.FlowIn 3 false "    \"x\ny\"") [ "double:x y" ] "A flow collection, left of the parent"
        Expect.equal (lexIn GrammarContext.FlowIn 0 false "  \"x\n y\"") [ "double:x y" ] "A flow collection, deeper than the parent"
        Expect.equal (lexIn GrammarContext.BlockOut -1 false "\"x\ny\"") [ "double:x y" ] "At the root every line is deep enough"

    // yaml@2.6.1 rejects @, the backquote and a percent sign at the start of a plain scalar with BAD_SCALAR_START
    testCase "a reserved indicator cannot start a plain scalar" <| fun _ ->
        expectError "@a" "reserved-indicator" "1:1-1:2"
        expectError "`a" "reserved-indicator" "1:1-1:2"
        expectError "k: %x" "reserved-indicator" "1:4-1:5"
        expectError "- %x" "reserved-indicator" "1:3-1:4"
        expectError "[a, @b]" "reserved-indicator" "1:5-1:6"
        expectError "{a: %x}" "reserved-indicator" "1:5-1:6"
        // yaml@2.6.1 reads `k: a %x`, `k: a@b` and `k: x`y` as plain scalars
        expectLexemes "k: a %x" [ "plain:k"; ":"; "plain:a %x" ]
        expectLexemes "k: a@b" [ "plain:k"; ":"; "plain:a@b" ]
        expectLexemes "k: x`y" [ "plain:k"; ":"; "plain:x`y" ]

    // yaml@2.6.1 rejects a block scalar whose body ends with a line that is deeper than the key and shallower than the content
    testCase "a line deeper than the parent but shallower than the content ends the block scalar with an error" <| fun _ ->
        expectError "k: |\n    x\n  y: 1" "block-indent" "3:3-3:4"
        expectError "k: |\n    x\n \"d\"" "block-indent" "3:2-3:3"
        // A comment line of any depth and a line that is not deeper than the parent end the scalar without an error.
        Expect.equal (lex "k: |\n    x\n  # c\nj: 1") [ "plain:k"; ":"; "literal:x\\n"; "#own: c"; "plain:j"; ":"; "plain:1" ] "A comment"

    // yaml@2.6.1 rejects the first input with "Block scalar lines must not be less indented than their first line"
    testCase "a line of spaces in front of the first content line may not be longer than that line" <| fun _ ->
        expectError "k: |\n    \n  a" "leading-empty-line" "2:1-3:1"
        Expect.equal (lex "k: |\n  \n  a\n") [ "plain:k"; ":"; "literal:\\na\\n" ] "As long as the content indentation"

    // The span of an error stays on the line of its first character. Without that, a span that reaches a few characters past the end of the line
    // would end between the CR and the LF of a CRLF, where the column is 0.
    testCase "an error span ends on its line and never inside a CRLF" <| fun _ ->
        expectError "a: \"\\x4\r\n  b\"" "invalid-escape" "1:5-1:8"
        expectError "a: \"\\x4\n  b\"" "invalid-escape" "1:5-1:8"
        expectError "a: \"\\u00\r\n  b\"" "invalid-escape" "1:5-1:9"
        expectError "!a%2\r\nb" "invalid-tag" "1:3-1:5"
        expectError "!<a%\r\nb>" "invalid-tag" "1:4-1:5"
        expectError "%TAG !e! a%\r\n---" "invalid-directive" "1:11-1:12"
        expectError "% \r\n---" "invalid-directive" "1:1-1:2"
        // A span from the start of one line to the start of a later one keeps covering whole lines.
        expectError "k: |\r\n    \r\n  a\r\n" "leading-empty-line" "2:1-3:1"
        for input in [ "a: \"\\x4\r\n  b\""; "a: \"\\u00\r\n  b\""; "!a%2\r\nb"; "!<a%\r\nb>"; "%TAG !e! a%\r\n---"; "[a, \"b\\q\r\n]"; "k: \"x\\\r\n\"" ] do
            match errorOfInput input with
            | Some failure ->
                Expect.isTrue (failure.Span.Start.Column >= 1 && failure.Span.Finish.Column >= 1) (sprintf "A column of the span of %A is 0" input)
            | None -> ()
]

/// A surrogate on its own. An F# literal cannot spell it, because the compiler turns "\uD800" into U+FFFD.
let private highSurrogate = string (char 0xD800)

let private lowSurrogate = string (char 0xDC00)

let private characterTests = testList "Lexer characters" [
    // yaml@2.6.1 checks no character class: it accepts every input of this test, in the plain scalar, the key, the comment, the name, the block scalar and the directive alike. The tag
    // inputs fail there with MISSING_CHAR, a tag error. [1] c-printable and [27] nb-char exclude the C0 controls except the tab, DEL, the C1 controls except U+0085, U+FFFE and U+FFFF.
    testCase "a control character, DEL, a C1 control or U+FFFE in plain text is an error" <| fun _ ->
        expectError "a: b\u0001c" "invalid-character" "1:5-1:6"
        expectError "a: b\u0000c" "invalid-character" "1:5-1:6"
        expectError "a: b\u0008c" "invalid-character" "1:5-1:6"
        expectError "a: b\u000Bc" "invalid-character" "1:5-1:6"
        expectError "a: b\u001Fc" "invalid-character" "1:5-1:6"
        expectError "a: b\u007Fc" "invalid-character" "1:5-1:6"
        expectError "a: b\u0080c" "invalid-character" "1:5-1:6"
        expectError "a: b\u0081c" "invalid-character" "1:5-1:6"
        expectError "a: b\u0084c" "invalid-character" "1:5-1:6"
        expectError "a: b\u0086c" "invalid-character" "1:5-1:6"
        expectError "a: b\u009Fc" "invalid-character" "1:5-1:6"
        expectError "a: b\uFFFEc" "invalid-character" "1:5-1:6"
        expectError "a: b\uFFFFc" "invalid-character" "1:5-1:6"
        // The first character of a scalar, a continuation line and a flow collection.
        expectError "a: \u0001b" "invalid-character" "1:4-1:5"
        expectError "a: b\n  c\u0001d" "invalid-character" "2:4-2:5"
        expectError "a: b\n  \u0001d" "invalid-character" "2:3-2:4"
        expectError "[a, b\u0001c]" "invalid-character" "1:6-1:7"
        expectError "{a: b\u0081}" "invalid-character" "1:6-1:7"
        // The character at the end of the scalar, behind blanks, and alone on its line.
        expectError "a: b\u0001" "invalid-character" "1:5-1:6"
        expectError "a: b \u0001" "invalid-character" "1:6-1:7"
        expectError "a:\n  \u0001" "invalid-character" "2:3-2:4"

    // yaml@2.6.1 accepts the keys and the names below as well. A name is a run of [102] ns-anchor-char, a key is a plain or quoted scalar, and a directive line holds ns-char parameters.
    testCase "a control character in a key, a name, a tag, a comment, a block scalar or a directive is an error" <| fun _ ->
        expectError "b\u0002: c" "invalid-character" "1:2-1:3"
        expectError "\u0002b: c" "invalid-character" "1:1-1:2"
        expectError "? b\u0002\n: c" "invalid-character" "1:4-1:5"
        expectError "a: &x\u0001 b" "invalid-character" "1:6-1:7"
        expectError "a: &\u0001x b" "invalid-character" "1:5-1:6"
        expectError "a: &x b\nc: *x\u0001" "invalid-character" "2:6-2:7"
        expectError "a: &x b\nc: *\u0001" "invalid-character" "2:5-2:6"
        expectError "a: !x\u0001 b" "invalid-character" "1:6-1:7"
        expectError "a: b # x\u0001y" "invalid-character" "1:9-1:10"
        expectError "# x\u0001y\na: b" "invalid-character" "1:4-1:5"
        expectError "a: |  # x\u0001y\n  b" "invalid-character" "1:10-1:11"
        expectError "a: |\n  b\u0001c\n" "invalid-character" "2:4-2:5"
        expectError "a: >\n  b\n  c\u007Fd\n" "invalid-character" "3:4-3:5"
        expectError "%FOO a\u0001\n---\nx" "invalid-character" "1:7-1:8"
        expectError "%YAML 1.2\u0001\n---\nx" "invalid-character" "1:10-1:11"
        expectError "%TAG ! tag:x\u0001\n---\nx" "invalid-character" "1:13-1:14"

    // yaml@2.6.1 accepts a surrogate on its own. A string of UTF-16 code units holds a character above U+FFFF as a high and a low surrogate in this order, and a surrogate without its partner
    // is no character ([1] c-printable ends at U+D7FF and resumes at U+E000). The span of the error is the one code unit.
    testCase "a surrogate without its partner is an error and a surrogate pair is one character" <| fun _ ->
        expectError ("a: b" + highSurrogate + "c") "invalid-character" "1:5-1:6"
        expectError ("a: b" + lowSurrogate + "c") "invalid-character" "1:5-1:6"
        expectError ("a: b" + lowSurrogate + highSurrogate + "c") "invalid-character" "1:5-1:6"
        expectError ("a: b" + highSurrogate) "invalid-character" "1:5-1:6"
        expectError ("a: \"b" + highSurrogate + "c\"") "invalid-character" "1:6-1:7"
        expectError ("a: 'b" + lowSurrogate + "c'") "invalid-character" "1:6-1:7"
        expectError ("a: |\n  b" + highSurrogate + "c\n") "invalid-character" "2:4-2:5"
        expectError ("a: b # x" + lowSurrogate) "invalid-character" "1:9-1:10"
        expectError ("b" + highSurrogate + ": c") "invalid-character" "1:2-1:3"
        expectError ("a: &x" + highSurrogate + " b") "invalid-character" "1:6-1:7"
        // A pair next to a surrogate on its own: the lone one is the error, and the pair takes one column.
        expectError ("a: b\U0001F600" + lowSurrogate) "invalid-character" "1:6-1:7"
        expectError ("a: b" + highSurrogate + "\U0001F600") "invalid-character" "1:5-1:6"
        expectLexemes "a: b\U0001F600c" [ "plain:a"; ":"; "plain:b\U0001F600c" ]
        expectLexemes "a: b\U0010FFFFc" [ "plain:a"; ":"; "plain:b\U0010FFFFc" ]
        Expect.equal (lexSpans "a: b\U0001F600c") [ "plain:a 1:1-1:2"; ": 1:2-1:3"; "plain:b\U0001F600c 1:4-1:7" ] "A pair is one column"

    // yaml@2.6.1 accepts every input of this test. [2] nb-json is the tab and every character from U+0020 up, so a quoted scalar holds DEL, the C1 controls, U+FFFE, U+FFFF and the byte
    // order mark as raw characters ([107], [118]). It refuses the other C0 controls. A surrogate needs its partner there too.
    testCase "a quoted scalar holds every character of nb-json and refuses the C0 controls" <| fun _ ->
        for text in [ "\u007F"; "\u0080"; "\u0085"; "\u009F"; "\u00A0"; "\u2028"; "\u2029"; "\uFFFD"; "\uFFFE"; "\uFFFF"; "\uFEFF"; "\U0001F600" ] do
            expectLexemes ("\"a" + text + "b\"") [ "double:a" + text + "b" ]
            expectLexemes ("'a" + text + "b'") [ "single:a" + text + "b" ]
            expectLexemes ("[\"a" + text + "b\"]") [ "["; "double:a" + text + "b"; "]" ]
        expectLexemes "\"a\tb\"" [ "double:a\\tb" ]
        expectLexemes "'a\tb'" [ "single:a\\tb" ]
        for code in [ 0x00; 0x01; 0x07; 0x08; 0x0B; 0x0C; 0x0E; 0x1F ] do
            let text = string (char code)
            expectError ("\"a" + text + "b\"") "invalid-character" "1:3-1:4"
            expectError ("'a" + text + "b'") "invalid-character" "1:3-1:4"
            expectError ("k: \"a" + text + "b\"") "invalid-character" "1:6-1:7"
        expectError "\"a\n\u0001b\"" "invalid-character" "2:1-2:2"
        expectError "'a\n  b\u0001'" "invalid-character" "2:4-2:5"

    // yaml@2.6.1 reads U+0085, U+2028 and U+2029 as text, like this lexer. In YAML 1.2 b-char is the line feed and the carriage return alone, and [27] nb-char includes the three characters.
    // U+00A0 is no s-white, so it neither separates tokens nor counts as indentation.
    testCase "U+0085, U+2028, U+2029 and U+00A0 are text, no line breaks and no blanks" <| fun _ ->
        for text in [ "\u0085"; "\u2028"; "\u2029"; "\u00A0" ] do
            expectLexemes ("a: b" + text + "c") [ "plain:a"; ":"; "plain:b" + text + "c" ]
            expectLexemes ("b" + text + ": c") [ "plain:b" + text; ":"; "plain:c" ]
            expectLexemes ("a: b\n  c" + text + "d") [ "plain:a"; ":"; "plain:b c" + text + "d" ]
            expectLexemes ("a: |\n  b" + text + "c\n") [ "plain:a"; ":"; "literal:b" + text + "c\\n" ]
            expectLexemes ("a: b # x" + text + "y") [ "plain:a"; ":"; "plain:b"; "#: x" + text + "y" ]
            expectLexemes ("# x" + text + "y\na") [ "#own: x" + text + "y"; "plain:a" ]
            expectLexemes ("a: &x" + text + " b") [ "plain:a"; ":"; "&x" + text; "plain:b" ]
            expectLexemes ("%FOO a" + text + "\n---\nx") [ "%FOO a" + text; "---"; "plain:x" ]
            expectLexemes ("[a" + text + "b]") [ "["; "plain:a" + text + "b"; "]" ]
            Expect.equal (lexSpans ("a: b" + text + "c")) [ "plain:a 1:1-1:2"; ": 1:2-1:3"; "plain:b" + text + "c 1:4-1:7" ] "One line and three columns"
        // A line that starts with U+00A0 has no indentation, so it ends a mapping value like any other text at the column 1.
        expectLexemes "a: b\n\u00A0c" [ "plain:a"; ":"; "plain:b" ; "plain:\u00A0c" ]
]

let private apiTests = testList "Lexer API" [
    // YAML 1.2.2, example 6.26: the tag !e!tag%21 with the prefix tag:example.com,2000:app/ is the tag tag:example.com,2000:app/tag!. The % escapes of a suffix are UTF-8 bytes.
    testCase "the escapes of a tag suffix decode as UTF-8" <| fun _ ->
        Expect.equal (Lexer.decodeTagEscapes "tag%21") (Some "tag!") "An ASCII escape"
        Expect.equal (Lexer.decodeTagEscapes "plain") (Some "plain") "No escape"
        Expect.equal (Lexer.decodeTagEscapes "%C3%A9") (Some "é") "Two bytes"
        Expect.equal (Lexer.decodeTagEscapes "a%E2%82%ACb") (Some "a€b") "Three bytes"
        Expect.equal (Lexer.decodeTagEscapes "%F0%9F%98%80") (Some "\U0001F600") "Four bytes, a character beyond the BMP"
        Expect.equal (Lexer.decodeTagEscapes "%7e%7E") (Some "~~") "Lower case and upper case digits"
        // Bytes that spell no UTF-8 text: a lone continuation byte, a cut-off sequence, an overlong sequence, a surrogate and a byte that never starts a sequence.
        for bad in [ "%80"; "%C3"; "%C3%28"; "%E0%80%80"; "%ED%A0%80"; "%FF"; "%F4%90%80%80" ] do
            Expect.equal (Lexer.decodeTagEscapes bad) None (sprintf "%s is no UTF-8 text" bad)

    // The parser asks whether a dash follows the indentation of a line before it decides that the value of a key has ended.
    testCase "a block entry ahead is a dash followed by a blank or the end" <| fun _ ->
        let aheadOf (text: string) =
            let cursor = Cursor(text)
            let context = { Grammar = GrammarContext.BlockOut; ParentIndent = -1; SimpleKey = false }
            let mutable finished = false
            // Read up to the first line start, where the cursor stands on the first character of the content.
            while not finished do
                match Lexer.next context cursor with
                | Ok lexeme ->
                    match lexeme.Value with
                    | Lexeme.LineStart _ -> finished <- true
                    | Lexeme.StreamEnd -> finished <- true
                    | _ -> ()
                | Error failure -> failwith (describeError failure)
            Lexer.nextIsBlockEntry cursor
        Expect.isTrue (aheadOf "- a") "A dash and a blank"
        Expect.isTrue (aheadOf "  -") "A dash at the end"
        Expect.isTrue (aheadOf "-\n") "A dash and a line break"
        Expect.isFalse (aheadOf "-a") "A dash and a letter"
        Expect.isFalse (aheadOf "--- a") "A document start marker"
        Expect.isFalse (aheadOf "a: -") "A key"

    testCase "a snapshot restores the cursor to the lexeme that follows it" <| fun _ ->
        let cursor = Cursor("k: [a, \"b c\"] # c\nj: |\n  x\n")
        let context = { Grammar = GrammarContext.BlockOut; ParentIndent = -1; SimpleKey = false }
        let nextText () =
            match Lexer.next context cursor with
            | Ok lexeme -> show lexeme.Value + " " + showSpan lexeme.Span
            | Error failure -> describeError failure
        let _ = nextText ()
        let _ = nextText ()
        let state = cursor.Snapshot()
        let first = [ for _ in 1 .. 6 -> nextText () ]
        cursor.Restore state
        let second = [ for _ in 1 .. 6 -> nextText () ]
        Expect.equal second first "The same lexemes follow after the restore"

    testCase "a second call returns the lexeme after the first one and never the same one twice" <| fun _ ->
        let cursor = Cursor("a b")
        let context = { Grammar = GrammarContext.BlockOut; ParentIndent = -1; SimpleKey = false }
        let texts =
            [ for _ in 1 .. 4 ->
                  match Lexer.next context cursor with
                  | Ok lexeme -> show lexeme.Value
                  | Error failure -> describeError failure ]
        Expect.equal texts [ "stream-start"; "line:0"; "plain:a b"; "stream-end" ] "Each call advances"

    testCase "the error of a call carries a code, a message and a span" <| fun _ ->
        match errorOfInput "a: 'x" with
        | Some failure ->
            Expect.equal (YamlErrorCode.name failure.Code) "unclosed-quote" "code"
            Expect.isTrue (failure.Message.Length > 0) "message"
            Expect.equal (showSpan failure.Span) "1:4-1:5" "span"
            Expect.equal failure.Related [] "related"
        | None -> failwith "The input is invalid"
]

/// One plain scalar over the given number of lines.
let private plainScalarInput (lines: int) =
    let sb = System.Text.StringBuilder()
    sb.Append("k: start\n") |> ignore
    for i in 0 .. lines - 1 do
        sb.Append(sprintf "  line %d with $(f(inputs.a, \"x\", 1)) and more words\n" i) |> ignore
    sb.Append("j: 1\n") |> ignore
    sb.ToString()

/// Lines with quoted scalars that hold escapes, a comment behind the scalar and, in every fifth line, a
/// double-quoted scalar over two lines with an escaped line break.
let private quotedInput (lines: int) =
    let sb = System.Text.StringBuilder()
    for i in 0 .. lines - 1 do
        let text =
            match i % 5 with
            | 0 -> sprintf "k%d: \"esc \\t \\u00e9 \\\"x\\\" \\U0001F600 %d\" # c%d" i i i
            | 1 -> sprintf "k%d: 'it''s %d'" i i
            | 2 -> sprintf "k%d: \"first %d\n  second \\\n  third\"" i i
            | 3 -> sprintf "k%d: 'a\n\n  b %d'" i i
            | _ -> sprintf "k%d: plain %d # c" i i
        sb.Append(text).Append("\n") |> ignore
    sb.ToString()

/// A literal block scalar and a folded block scalar of the given number of lines each, with more indented
/// lines, empty lines and lines of spaces among them. A third part holds one block scalar for every line,
/// each with a comment behind its header, with and without content and with every chomping sign. The
/// scalar without content ends at its header in front of the comment, which once sent the tracker of the
/// marks back to the start of the text.
let private blockInput (lines: int) =
    let sb = System.Text.StringBuilder()
    for header in [ "a: |+"; "b: >" ] do
        sb.Append(header).Append("\n") |> ignore
        for i in 0 .. lines - 1 do
            let text =
                match i % 7 with
                | 0 -> sprintf "  line %d" i
                | 1 -> sprintf "    deeper %d" i
                | 2 -> ""
                | 3 -> "   "
                | 4 -> sprintf "  # hash %d" i
                | _ -> sprintf "  line %d with 'quotes' and \"more\"" i
            sb.Append(text).Append("\n") |> ignore
    for i in 0 .. lines - 1 do
        let text =
            match i % 8 with
            | 0 -> sprintf "h%d: | # c%d" i i
            | 1 -> sprintf "h%d: >- # c%d" i i
            | 2 -> sprintf "h%d: |+ # c%d" i i
            | 3 -> sprintf "h%d: >+ # c%d" i i
            | 4 -> sprintf "h%d: | # c%d\n  body %d" i i i
            | 5 -> sprintf "h%d: >- # c%d\n  body %d" i i i
            | 6 -> sprintf "h%d: |+ # c%d\n  body %d\n" i i i
            | _ -> sprintf "h%d: >+ # c%d\n  body %d\n\n" i i i
        sb.Append(text).Append("\n") |> ignore
    sb.ToString()

/// Lines of the same width, so that the index of a line is its number times the width.
let private lineWidth = 11

let private fixedWidthInput (lines: int) = String.replicate lines "key: value\n"

/// The line of the last mark, in a cell that outlives the call so that the compiler keeps the loop.
let private markedLine = ref 0

/// Takes a snapshot at every line, looks a few lines ahead and restores. The restore puts the tracker of the
/// marks back to the line, so the mark of the next line lies ahead of it again.
let private rewindMarks (input: string) =
    let cursor = Cursor(input)
    for i in 0 .. input.Length / lineWidth - 1 do
        let mark = cursor.MarkAt(i * lineWidth)
        let state = cursor.Snapshot()
        cursor.MarkAt(min input.Length (i * lineWidth + 3 * lineWidth)) |> ignore
        cursor.Restore state
        markedLine.Value <- mark.Line

/// Comment lines with indentation and a blank line in between now and then.
let private commentRunInput (lines: int) =
    let sb = System.Text.StringBuilder()
    sb.Append("root:\n  a: |\n    x\n") |> ignore
    for i in 0 .. lines - 1 do
        sb.Append(String.replicate (i % 5 * 2) " ").Append(sprintf "# comment %d\n" i) |> ignore
        if i % 7 = 0 then sb.Append("\n") |> ignore
    sb.Append("  b: 1\n") |> ignore
    sb.ToString()

/// A plain scalar, then a run of blank lines that the plain scalar looks over, then a key.
let private blankRunInput (lines: int) =
    "k: a\n" + String.replicate lines "\n" + "j: b\n" + String.replicate lines "  \n" + "i: c\n"

/// A flow sequence over the given number of lines with quoted scalars, comments and nested collections.
let private flowInput (lines: int) =
    let sb = System.Text.StringBuilder()
    sb.Append("k: [\n") |> ignore
    for i in 0 .. lines - 1 do
        let text =
            match i % 5 with
            | 0 -> sprintf "  \"quoted %d\"," i
            | 1 -> sprintf "  plain %d, # comment %d" i i
            | 2 -> sprintf "  [nested %d, {a: 1, b: *x}]," i
            | 3 -> sprintf "  first %d" i
            | _ -> sprintf "  second %d," i
        sb.Append(text).Append("\n") |> ignore
    sb.Append("]\n") |> ignore
    sb.ToString()

/// Many short entries with properties.
let private entriesInput (lines: int) =
    let sb = System.Text.StringBuilder()
    for i in 0 .. lines - 1 do
        let text =
            match i % 4 with
            | 0 -> sprintf "k%d: v%d" i i
            | 1 -> sprintf "- !!str &a%d item %d" i i
            | 2 -> sprintf "? key %d\n: *a%d" i i
            | _ -> sprintf "- - nested %d" i
        sb.Append(text).Append("\n") |> ignore
    sb.ToString()

/// One plain scalar and one quoted scalar of the given number of characters on a single line each.
let private longLineInput (lines: int) =
    "k: " + String.replicate lines "word " + "\nj: \"" + String.replicate lines "word " + "\"\n"

/// Lines with unbalanced quotes inside plain scalars. No quote stands at a node start, so none of them opens a scalar.
let private plainQuoteInput (lines: int) =
    let sb = System.Text.StringBuilder()
    for i in 0 .. lines - 1 do
        let text =
            match i % 3 with
            | 0 -> sprintf "k%d: it's a 'quoted word %d" i i
            | 1 -> sprintf "k%d: 3\" wide \"x %d" i i
            | _ -> sprintf "k%d: rock 'n' roll %d" i i
        sb.Append(text).Append("\n") |> ignore
    sb.ToString()

/// Groups of six lines with four quoted scalars, one comment, a plain scalar with an apostrophe in a word and a long plain scalar.
let private oldQuotedInput (lines: int) =
    let sb = System.Text.StringBuilder()
    for i in 0 .. lines - 1 do
        let text =
            match i % 6 with
            | 0 -> sprintf "k%d: 'ok %d'" i i
            | 1 -> sprintf "k%d: \"ok %d\"" i i
            | 2 -> sprintf "k%d: 'ok %d' # c%d" i i i
            | 3 -> sprintf "k%d: don't stop %d" i i
            | 4 -> sprintf "k%d: \"ok %d\"" i i
            | _ -> sprintf "k%d: plain %s" i (System.String('x', 120))
        sb.Append(text).Append("\n") |> ignore
    sb.ToString()

let private performanceTests = testList "Lexer performance" [
    // The lexer reads the text once. The empty lines behind a plain scalar are read twice at most, once by the look for a continuation line and once
    // as blank lines. The reference is a single pass over the characters.
    testCase "the time grows linearly with the lines of a plain scalar" <| fun _ ->
        expectLinear "The lexer on a plain scalar over many lines" (countLexemes >> ignore) countLineBreaks plainScalarInput

    testCase "the time grows linearly with the number of quoted scalars" <| fun _ ->
        expectLinear "The lexer on quoted scalars with escapes" (countLexemes >> ignore) countLineBreaks quotedInput

    testCase "the time grows linearly with the lines of block scalars" <| fun _ ->
        expectLinear "The lexer on block scalars" (countLexemes >> ignore) countLineBreaks blockInput

    testCase "the time grows linearly with a run of comments" <| fun _ ->
        expectLinear "The lexer on a run of comments" (countLexemes >> ignore) countLineBreaks commentRunInput

    testCase "the time grows linearly with a run of blank lines behind a plain scalar" <| fun _ ->
        expectLinear "The lexer on a run of blank lines" (countLexemes >> ignore) countLineBreaks blankRunInput

    testCase "the time grows linearly with the lines of a flow collection" <| fun _ ->
        expectLinear "The lexer on a flow collection" (countLexemes >> ignore) countLineBreaks flowInput

    testCase "the time grows linearly with the number of entries" <| fun _ ->
        expectLinear "The lexer on many entries" (countLexemes >> ignore) countLineBreaks entriesInput

    testCase "the time grows linearly with the length of a line" <| fun _ ->
        expectLinear "The lexer on a long line" (countLexemes >> ignore) countLineBreaks longLineInput

    testCase "the marks of the spans cost no more than a pass over the text" <| fun _ ->
        expectLinear "The driver with spans" (fun input -> lexWith false input |> ignore) countLineBreaks entriesInput

    testCase "a restore does not send the marks back to the start of the text" <| fun _ ->
        expectLinear "Marks around a snapshot and a restore" rewindMarks countLineBreaks fixedWidthInput

    // Earlier tests "the time grows linearly with unbalanced quotes inside plain scalars" and "a long input of quoted scalars keeps every string and comment". The old tests counted the string
    // table and the comment table of the scanner. The lexer returns the quoted scalars and the comments as lexemes, so the test counts those.
    testCase "unbalanced quotes inside plain scalars open no scalar and take time linear in the length of the input" <| fun _ ->
        let styles = scalarsOf (plainQuoteInput 600) |> List.map (fun scalar -> scalar.Style)
        Expect.isTrue (styles |> List.forall (fun style -> style = ScalarStyle.Plain)) "No quote stands at a node start, so no scalar is quoted"
        expectLinear "The lexer on unbalanced quotes" (countLexemes >> ignore) countLineBreaks plainQuoteInput

    testCase "a long input of quoted scalars keeps every scalar and comment" <| fun _ ->
        let lines = 6000
        let lexemes = lex (oldQuotedInput lines)
        let count (prefix: string) = lexemes |> List.filter (fun lexeme -> lexeme.StartsWith prefix) |> List.length
        Expect.equal (count "single:" + count "double:") (lines / 6 * 4) "Four quoted scalars in each group of six lines"
        Expect.equal (count "#") (lines / 6) "One comment in each group of six lines"
        Expect.isTrue (List.contains "plain:don't stop 3" lexemes) "The apostrophe in a word stays text"
        Expect.isTrue (List.contains ("plain:plain " + System.String('x', 120)) lexemes) "A plain scalar behind the quoted ones stays plain"
        expectLinear "The lexer on the old quoted scalars" (countLexemes >> ignore) countLineBreaks oldQuotedInput
]

let Main = testList "Lexer" [
    streamTests
    blockTests
    flowTests
    propertyTests
    commentTests
    markerTests
    tabTests
    contextTests
    positionTests
    errorTests
    characterTests
    apiTests
    performanceTests
]
