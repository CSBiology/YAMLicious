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

    // yaml@2.6.1: ["a:b"], [{"a":"b"}], [{"a":null}] for `{a:}` and ["x"] for `[?]`
    testCase "a colon in a flow collection ends a key behind a blank or a flow indicator only" <| fun _ ->
        expectLexemes "[a:b]" [ "["; "plain:a:b"; "]" ]
        expectLexemes "[a: b]" [ "["; "plain:a"; ":"; "plain:b"; "]" ]
        expectLexemes "{a:}" [ "{"; "plain:a"; ":"; "}" ]
        expectLexemes "{a:,b}" [ "{"; "plain:a"; ":"; ","; "plain:b"; "}" ]

    // yaml@2.6.1: {"a":"b"}, {"a":"b"} for the blank in front of the colon and [{"a":"b"}] for `[[a]:b]` is a collection key
    testCase "a colon right behind a quoted scalar or a flow collection is an adjacent value indicator" <| fun _ ->
        expectLexemes "{\"a\":b}" [ "{"; "double:a"; ":adjacent"; "plain:b"; "}" ]
        expectLexemes "{'a':b}" [ "{"; "single:a"; ":adjacent"; "plain:b"; "}" ]
        expectLexemes "[\"a\" : b]" [ "["; "double:a"; ":"; "plain:b"; "]" ]
        expectLexemes "[[a]:b]" [ "["; "["; "plain:a"; "]"; ":adjacent"; "plain:b"; "]" ]
        expectLexemes "{\"a\": b}" [ "{"; "double:a"; ":adjacent"; "plain:b"; "}" ]

    testCase "an adjacent colon behind a quoted scalar is a value indicator in a block key too" <| fun _ ->
        expectLexemes "\"a\": b" [ "double:a"; ":adjacent"; "plain:b" ]
        expectLexemes "\"a\" : b" [ "double:a"; ":"; "plain:b" ]

    testCase "a flow collection goes on in the lines below without line starts" <| fun _ ->
        expectFull "[a,\n b]" [ "stream-start"; "line:0"; "["; "plain:a"; ","; "plain:b"; "]"; "stream-end" ]
        expectLexemes "[a\n b]" [ "["; "plain:a b"; "]" ]
        expectLexemes "[a,\n\n b]" [ "["; "plain:a"; ","; "blank"; "plain:b"; "]" ]
        expectLexemes "[a, # c\n b]" [ "["; "plain:a"; ","; "#: c"; "plain:b"; "]" ]
        expectLexemes "[\n  # c\n  a\n]" [ "["; "#own: c"; "plain:a"; "]" ]

    testCase "an explicit key in a flow collection stands in front of a blank or a flow indicator" <| fun _ ->
        expectLexemes "{? a, b: c}" [ "{"; "?"; "plain:a"; ","; "plain:b"; ":"; "plain:c"; "}" ]
        expectLexemes "[?]" [ "["; "?"; "]" ]
        expectLexemes "[?a]" [ "["; "plain:?a"; "]" ]

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
        expectLexemes "--- |\n x" [ "---"; "literal:x\\n" ]
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

    // Ported from Tests.Tokenizer "a tab in the indentation or between an indicator and a block collection is an error".
    // yaml@2.6.1 rejects these inputs with TAB_AS_INDENT. The lexer cannot tell whether a block collection follows,
    // so it reports the tab on the line start and the parser makes the decision.
    testCase "a tab in front of a node that could be a key is reported on the line start for the parser" <| fun _ ->
        expectFull "k: a\n\tb" [ "stream-start"; "line:0"; "plain:k"; ":"; "plain:a"; "line:0,tab"; "plain:b"; "stream-end" ]
        expectFull "k:\n\tv" [ "stream-start"; "line:0"; "plain:k"; ":"; "line:0,tab"; "plain:v"; "stream-end" ]
        expectFull "- a\n\tb" [ "stream-start"; "line:0"; "-"; "plain:a"; "line:0,tab"; "plain:b"; "stream-end" ]
        expectFull "k: a\n\n\tb" [ "stream-start"; "line:0"; "plain:k"; ":"; "plain:a"; "blank"; "line:0,tab"; "plain:b"; "stream-end" ]
        expectFull "k:\n  \ta: b" [ "stream-start"; "line:0"; "plain:k"; ":"; "line:2,tab"; "plain:a"; ":"; "plain:b"; "stream-end" ]
        expectFull "a: b\n# c\n\tc: d" [ "stream-start"; "line:0"; "plain:a"; ":"; "plain:b"; "#own: c"; "line:0,tab"; "plain:c"; ":"; "plain:d"; "stream-end" ]

    // Ported from Tests.Tokenizer. yaml@2.6.1 rejects `-\tk: v` and `---\tk: v` with TAB_AS_INDENT. The tab flag of the cursor
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

    // yaml@2.6.1 rejects `a: [\n\tb]` ("must be sufficiently indented"), because a tab is no indentation, and reads `a: [\n \tb]` as {"a":["b"]}
    testCase "the indentation of a flow line counts spaces only" <| fun _ ->
        expectError "a: [\n\tb]" "flow-indent" "2:1-2:3"
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
        Expect.equal (lexIn GrammarContext.BlockOut -1 false "|\n x") [ "literal:x\\n" ] "BlockOut"

    testCase "the block indicators are lexemes in a block context and the flow rules apply in a flow context" <| fun _ ->
        Expect.equal (lexIn GrammarContext.FlowOut -1 false "- a") [ "-"; "plain:a" ] "FlowOut treats a dash like a block context"
        Expect.equal (lexIn GrammarContext.FlowIn -1 false "- a") [ "error:flow-dash" ] "FlowIn"
        Expect.equal (lexIn GrammarContext.FlowIn -1 false "a: b") [ "plain:a"; ":"; "plain:b" ] "A colon behind a blank"
        Expect.equal (lexIn GrammarContext.BlockOut -1 false "a:,") [ "plain:a:," ] "A comma behind a colon is text in a block"

    testCase "a line of a flow collection has to stand deeper than the parent" <| fun _ ->
        // yaml@2.6.1 rejects a line of a flow collection that is not indented deeper than the key that owns it
        // The first line of every input starts with blanks, because the lexer treats the first line of its input as a continuation line too.
        Expect.equal (lexIn GrammarContext.FlowIn 0 false "  a,\n  b") [ "plain:a"; ","; "plain:b" ] "Deeper"
        Expect.equal (lexIn GrammarContext.FlowIn 0 false "  a,\nb") [ "plain:a"; ","; "error:flow-indent" ] "As deep as the parent"
        Expect.equal (lexIn GrammarContext.FlowIn 0 false "  a,\n]") [ "plain:a"; ","; "]" ] "A closer may stand at the parent"
        Expect.equal (lexIn GrammarContext.FlowIn 2 false "    a,\n  ]") [ "plain:a"; ","; "]" ] "A closer at the column of the parent"
        Expect.equal (lexIn GrammarContext.FlowIn 2 false "    a,\n ]") [ "plain:a"; ","; "error:flow-indent" ] "A closer behind the parent is too shallow"
        Expect.equal (lexIn GrammarContext.FlowIn 0 false "  a,\n# c\nb") [ "plain:a"; ","; "#own: c"; "error:flow-indent" ] "A comment line stands anywhere"
        Expect.equal (lexIn GrammarContext.FlowIn -1 false "a,\nb") [ "plain:a"; ","; "plain:b" ] "At the root every line is deep enough"

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
        Expect.equal (lexSpans "k: |\n\nj: 1") [ "plain:k 1:1-1:2"; ": 1:2-1:3"; "literal: 1:4-1:5"; "plain:j 3:1-3:2"; ": 3:2-3:3"; "plain:1 3:4-3:5" ] "A block scalar without content ends at its header"

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

    // Ported from Tests.Tokenizer "a bar or an angle bracket at a node start needs a valid block scalar header".
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

    // Ported from Tests.Tokenizer. yaml@2.6.1 rejects each of these with BAD_SCALAR_START
    testCase "a block scalar cannot start inside a flow collection" <| fun _ ->
        expectError "k: {\n  a: |\n}" "block-scalar-position" "2:6-2:7"
        expectError "[ > 5 ]" "block-scalar-position" "1:3-1:4"
        expectError "a: {b: |\n}\nc: 'x'\n" "block-scalar-position" "1:8-1:9"
        expectError "[\n  |\n    x\n]" "block-scalar-position" "2:3-2:4"
        expectError "{a: >}" "block-scalar-position" "1:5-1:6"

    // Ported from Tests.Tokenizer "a closer without an open flow collection, a comma or a hash at a node start is an error".
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

    // Ported from Tests.Tokenizer. yaml@2.6.1 rejects each of these with BLOCK_IN_FLOW, because a plain scalar cannot be
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

    // yaml@2.6.1 rejects a quoted scalar over lines that are not indented deeper than the key: `a: "x\ny"`
    testCase "a line of a quoted scalar has to be deeper than the parent" <| fun _ ->
        expectError "a: \"x\ny\"" "quoted-indent" "2:1-2:2"
        expectError "- 'x\ny'" "quoted-indent" "2:1-2:2"
        Expect.equal (lexIn GrammarContext.BlockOut -1 false "\"x\ny\"") [ "double:x y" ] "At the root every line is deep enough"
        Expect.equal (lexIn GrammarContext.BlockOut 0 false "\"x\n y\"") [ "double:x y" ] "Deeper than the parent"

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
        Expect.equal (lex "k: |\n  \n  a") [ "plain:k"; ":"; "literal:\\na\\n" ] "As long as the content indentation"
]

let private apiTests = testList "Lexer API" [
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
            Expect.equal failure.Code "unclosed-quote" "code"
            Expect.isTrue (failure.Message.Length > 0) "message"
            Expect.equal (showSpan failure.Span) "1:4-1:5" "span"
            Expect.equal failure.Related [] "related"
        | None -> failwith "The input is invalid"
]

/// The reference of the scaling tests, a single pass over the characters of the input. The count goes to
/// a cell that outlives the call, so the compiler cannot drop the loop.
let private lineBreakCount = ref 0

let private countLineBreaks (input: string) =
    let mutable count = 0
    let mutable i = 0
    while i < input.Length do
        if input.[i] = '\n' then count <- count + 1
        i <- i + 1
    lineBreakCount.Value <- count

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
/// lines, empty lines and lines of spaces among them.
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
    sb.ToString()

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
    apiTests
    performanceTests
]
