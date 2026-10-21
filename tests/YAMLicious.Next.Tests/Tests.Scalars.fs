module Tests.Scalars

// The scalar expectations of the old tests, as the cooked value and the style of every scalar that the lexer
// returns. The old tests read a whole document, and these tests lex it with the driver of Driver.fs. A plain
// scalar over several lines is folded here, where the old tests kept its physical lines for the writer. The
// comments keep the yaml@2.6.1 values of the old tests. The sources are:
//
// - Tests.Escapes.fs (all but the writer round trip and the Decode test)
// - Tests.StringCleanUp.fs (the tests of quoted scalars and of quotes in plain scalars)
// - Tests.BlockScalars.fs (the unit tests of the lexical part, the lines of spaces and the group Block Scalars)
// - Tests.ScalarStyles.fs (the chomping matrix and the explicit indentation indicator)
// - Tests.Tokenizer.fs (the plain scalar continuation lines, the quotes and the plain scalars in flow collections)
// - Tests.YamlRead.fs (quoted scalars, escapes, plain continuation, quotes in plain scalars and in flow collections)

open Fable.Pyxpecto
open YAMLicious

let private pl (value: string) = value, ScalarStyle.Plain
let private sq (value: string) = value, ScalarStyle.SingleQuoted
let private dq (value: string) = value, ScalarStyle.DoubleQuoted
let private blk (style: BlockScalarStyle) (chomping: ChompingMode) (indicator: int option) (value: string) =
    value, ScalarStyle.Block(style, chomping, indicator)
let private lit (value: string) = blk BlockScalarStyle.Literal ChompingMode.Clip None value
let private fold (value: string) = blk BlockScalarStyle.Folded ChompingMode.Clip None value

/// The scalars of the input as value and style. The lexer has to accept the whole text.
let private scalars (input: string) : (string * ScalarStyle) list =
    match errorOfInput input with
    | Some failure -> failwithf "The lexer rejects %A: %s" input (describeError failure)
    | None -> scalarsOf input |> List.map (fun scalar -> scalar.Value, scalar.Style)

let private expectScalars (input: string) (expected: (string * ScalarStyle) list) =
    Expect.equal (scalars input) expected (sprintf "Scalars of %A" input)

/// The text of the lexemes without the line starts, for the inputs whose comments and blank lines matter.
let private expectLexemes (input: string) (expected: string list) =
    Expect.equal (lex input) expected (sprintf "Lexemes of %A" input)

let private expectError (input: string) (code: string) (span: string) =
    Expect.equal (errorCodeOf input) code (sprintf "Error code of %A" input)
    Expect.equal (errorSpanOf input) span (sprintf "Error span of %A" input)

let private quotedTests = testList "Quoted scalars" [
    // Tests.YamlRead "Single-quoted string", "Single-quoted with escaped quote" and "Single-quoted preserves backslashes"
    testCase "a single-quoted scalar keeps its text, a doubled quote is one quote and a backslash is text" <| fun _ ->
        expectScalars "single: 'hello world'" [ pl "single"; sq "hello world" ]
        expectScalars "single: 'here''s to quotes'" [ pl "single"; sq "here's to quotes" ]
        expectScalars "tie-fighter: '|\\-*-/|'" [ pl "tie-fighter"; sq "|\\-*-/|" ]

    // Tests.YamlRead "Double-quoted escape: newline", "tab", "backslash", "escaped quote", "hex unicode", "unicode 16-bit" and "null character"
    testCase "a double-quoted scalar decodes its escapes" <| fun _ ->
        expectScalars "key: \"line1\\nline2\"" [ pl "key"; dq "line1\nline2" ]
        expectScalars "key: \"before\\tafter\"" [ pl "key"; dq "before\tafter" ]
        expectScalars "key: \"path\\\\to\\\\file\"" [ pl "key"; dq "path\\to\\file" ]
        expectScalars "key: \"a \\\"b\\\" c\"" [ pl "key"; dq "a \"b\" c" ]
        expectScalars "key: \"\\x41\\x42\\x43\"" [ pl "key"; dq "ABC" ]
        expectScalars "key: \"\\u263A\"" [ pl "key"; dq "\u263A" ]
        expectScalars "key: \"before\\0after\"" [ pl "key"; dq "before\u0000after" ]

    // Tests.YamlRead "Double-quoted mapping key", "Double-quoted string with embedded single quotes stays intact",
    // "Plain scalar with embedded single quotes stays intact" and "Plain scalar with embedded double quotes stays intact"
    testCase "a quoted key keeps its style and a quote inside the other quote or inside a plain scalar is text" <| fun _ ->
        expectScalars "\"@id\": MyIdentifier" [ dq "@id"; pl "MyIdentifier" ]
        expectScalars "key: \"a 'b' c\"" [ pl "key"; dq "a 'b' c" ]
        expectScalars "key: rock 'n' roll" [ pl "key"; pl "rock 'n' roll" ]
        expectScalars "key: he said \"hi\"" [ pl "key"; pl "he said \"hi\"" ]

    // Tests.StringCleanUp "tokenize returns buffered text", "empty quotes", "doubled apostrophes are one apostrophe"
    // yaml@2.6.1 reads both empty quotes as the empty string
    testCase "an empty quoted scalar and a scalar of doubled apostrophes" <| fun _ ->
        expectScalars "\"value\"" [ dq "value" ]
        expectScalars "a: ''\nb: \"\"\n" [ pl "a"; sq ""; pl "b"; dq "" ]
        expectScalars "a: 'it''s'\nb: '''x'''\n" [ pl "a"; sq "it's"; pl "b"; sq "'x'" ]

    // Tests.StringCleanUp "single special char" and "multiple comments". yaml@2.6.1 reads the hash and the bar as text of the scalar.
    testCase "a quoted scalar may hold a hash and the comment behind it keeps every hash after the first" <| fun _ ->
        expectLexemes "\nMy Key: \"[{Special character place # |}\" # A # in string is allowed!\n" [ "blank"; "plain:My Key"; ":"; "double:[{Special character place # |}"; "#: A # in string is allowed!" ]
        expectLexemes
            "My Key: \"[{Special character place # |}\" ### A # in string is allowed!\nMy Key2: \"3\" ### A # in string is allowed!\n"
            [ "plain:My Key"; ":"; "double:[{Special character place # |}"; "#:## A # in string is allowed!"
              "plain:My Key2"; ":"; "double:3"; "#:## A # in string is allowed!" ]

    // Tests.StringCleanUp "lone apostrophe does not swallow later double-quoted token", "lone double quote does not swallow later
    // single-quoted token" and "lone apostrophe keeps later double-quoted keys parseable"
    testCase "a lone quote inside a plain scalar opens no scalar and the quoted scalars behind it stay" <| fun _ ->
        expectScalars "desc: don't stop\nid: \"value\"" [ pl "desc"; pl "don't stop"; pl "id"; dq "value" ]
        expectScalars "desc: 3 \" inch mark\nid: 'value'" [ pl "desc"; pl "3 \" inch mark"; pl "id"; sq "value" ]
        expectScalars "description: don't stop now\ndataset:\n  \"@id\": ./ref" [ pl "description"; pl "don't stop now"; pl "dataset"; dq "@id"; pl "./ref" ]
        expectScalars "description: a 'quoted' word here\ndataset:\n  \"@id\": ./ref" [ pl "description"; pl "a 'quoted' word here"; pl "dataset"; dq "@id"; pl "./ref" ]

    // Tests.StringCleanUp "mixed single and double quoted placeholders use unique indices" and "string ids follow the source order for double
    // before single". The new lexer has no placeholders, so the order of the scalars is the order of the text.
    testCase "quoted scalars in a flow sequence come in the order of the text" <| fun _ ->
        expectScalars "mixed: ['a', \"b\"]" [ pl "mixed"; sq "a"; dq "b" ]
        expectScalars "mixed: [\"a\", 'b', \"c\", 'd']" [ pl "mixed"; dq "a"; sq "b"; dq "c"; sq "d" ]

    // Tests.StringCleanUp "escaped double quotes and backslashes stay raw interior text". The old scanner kept the raw interior,
    // and the lexer cooks the escapes.
    testCase "the escapes of a double-quoted scalar are decoded" <| fun _ ->
        expectScalars "a: \"say \\\"hi\\\"\"\nb: \"back\\\\\"\nc: \"tab\\t\"\n" [ pl "a"; dq "say \"hi\""; pl "b"; dq "back\\"; pl "c"; dq "tab\t" ]

    // Tests.StringCleanUp "single-quoted scalar folds its line breaks". yaml@2.6.1: {"a":"one two\nthree"}
    testCase "a single-quoted scalar folds its line breaks" <| fun _ ->
        expectScalars "a: 'one\n  two\n\n  three'\n" [ pl "a"; sq "one two\nthree" ]

    // Tests.StringCleanUp "multiline quote before a block scalar keeps the later string (single first)", "(double first)" and "several
    // multiline quotes before a block scalar". The old scanner kept the raw interior "x\n  y" of the double-quoted scalar.
    testCase "a multiline quoted scalar in front of a block scalar leaves the block scalar and the later scalars alone" <| fun _ ->
        expectScalars "a: 'x\n  y'\nb: |\n  \"hi\"\nc: \"z\"\n" [ pl "a"; sq "x y"; pl "b"; lit "\"hi\"\n"; pl "c"; dq "z" ]
        expectScalars "a: \"x\n  y\"\nb: |\n  'hi'\nc: 'z'\n" [ pl "a"; dq "x y"; pl "b"; lit "'hi'\n"; pl "c"; sq "z" ]
        expectScalars
            "a: 'p\n  q'\nb: \"r\n  s\"\nc: 't\n  u'\nd: |\n  \"keep\"\n  'keep'\ne: \"v\"\nf: 'w'\n"
            [ pl "a"; sq "p q"; pl "b"; dq "r s"; pl "c"; sq "t u"; pl "d"; lit "\"keep\"\n'keep'\n"; pl "e"; dq "v"; pl "f"; sq "w" ]

    // Tests.StringCleanUp "a quote at a node start that never closes is an error that names its line", "quote at the end of the input without
    // a line break". yaml@2.6.1 rejects all inputs with MISSING_CHAR. The old inputs continue the quote at the column of the key, which is the
    // error quoted-indent here, so the continuation lines stand two spaces in.
    testCase "a quote that never closes is an error on the opening quote" <| fun _ ->
        expectError "a: 'unclosed\n  b: 1\n" "unclosed-quote" "1:4-1:5"
        expectError "a: \"never closed\n  b: 'ok' # c" "unclosed-quote" "1:4-1:5"
        expectError "a: 1\nb: 'p\n  c: 2\n" "unclosed-quote" "2:4-2:5"
        expectError "- [a, \"b]\n" "unclosed-quote" "1:7-1:8"
        expectError "a: 'x" "unclosed-quote" "1:4-1:5"
        expectError "a: \"x" "unclosed-quote" "1:4-1:5"
        expectScalars "a: 'x'" [ pl "a"; sq "x" ]

    // Tests.StringCleanUp "a closing quote followed by a blank, a colon or the end is a boundary". yaml@2.6.1: {"a":"x","b":"y","k":1,"j":2}
    testCase "a closing quote may be followed by blanks, a colon or the end" <| fun _ ->
        expectLexemes "a: 'x'  \nb: \"y\"\n'k': 1\n\"j\" : 2\n" [ "plain:a"; ":"; "single:x"; "plain:b"; ":"; "double:y"; "single:k"; ":adjacent"; "plain:1"; "double:j"; ":"; "plain:2" ]

    // Tests.StringCleanUp "quotes in a word are not scalars". yaml@2.6.1 reads each of them as a plain scalar.
    testCase "a quote inside a word is text" <| fun _ ->
        expectScalars "a: rock 'n' roll\nb: don't\nc: 3\" wide\nd: x\"y\"z\n" [ pl "a"; pl "rock 'n' roll"; pl "b"; pl "don't"; pl "c"; pl "3\" wide"; pl "d"; pl "x\"y\"z" ]

    // Tests.StringCleanUp "tags and anchors may precede a quote", "a double quote without a space behind a tag is accepted lexically",
    // "a quote inside an anchor name or a single quote inside a tag name is part of the name" and "quotes after indicators and in flow collections"
    testCase "a quote behind a property or an indicator opens a scalar" <| fun _ ->
        expectLexemes "a: !!str \"x\"\nb: &anchor 'y'\nc: !tag\t\"z\"\nd:\n- !<tag:yaml.org,2002:str> 'v'\n"
            [ "plain:a"; ":"; "tag:!!|str"; "double:x"; "plain:b"; ":"; "&anchor"; "single:y"; "plain:c"; ":"; "tag:!|tag"; "double:z"; "plain:d"; ":"; "-"; "!<tag:yaml.org,2002:str>"; "single:v" ]
        expectLexemes "a: !tag\"x\"\n" [ "plain:a"; ":"; "tag:!|tag"; "double:x" ]
        // yaml@2.6.1: {"a":null,"b":"z","c":"p","d":["z"]}. The quotes are characters of the names, so they open no scalar.
        expectLexemes "a: &anchor'y'\nb: &x\"y z\nc: !t'y p\nd:\n- &x\"y\" z\n"
            [ "plain:a"; ":"; "&anchor'y'"; "plain:b"; ":"; "&x\"y"; "plain:z"; "plain:c"; ":"; "tag:!|t'y"; "plain:p"; "plain:d"; ":"; "-"; "&x\"y\""; "plain:z" ]
        expectScalars "s:\n- 'a'\n- \"b\"\n? 'c'\n: \"d\"\ne: ['f', \"g\"]\nh: {'i': \"j\"}\n"
            [ pl "s"; sq "a"; dq "b"; sq "c"; dq "d"; pl "e"; sq "f"; dq "g"; pl "h"; sq "i"; dq "j" ]
]

let private escapeTests = testList "Escapes" [
    // Tests.Escapes "Double-quoted escape: unicode 32-bit" and "complex mix". yaml@2.6.1 reads the emoji as one character.
    testCase "a 32-bit escape is one code point and the escapes mix" <| fun _ ->
        expectScalars "key: \"\\U0001F600\"" [ pl "key"; dq "\U0001F600" ]
        expectScalars "key: \"\\t\\n\\x41\"" [ pl "key"; dq "\t\nA" ]

    // Tests.Escapes "A single-line double-quoted scalar decodes every escape". yaml@2.6.1 reads the same value.
    testCase "a double-quoted scalar decodes every escape of YAML 1.2.2" <| fun _ ->
        let input = "k: \"\\0\\a\\b\\t\\n\\v\\f\\r\\e\\ \\\"\\/\\\\\\N\\_\\L\\P\\x41\\u00E9\\U0001F600\""
        let expected = "\u0000\u0007\u0008\t\n\u000B\u000C\r\u001B \"/\\\u0085\u00A0\u2028\u2029A\u00E9\U0001F600"
        expectScalars input [ pl "k"; dq expected ]

    // yaml@2.6.1 reads "\uD83D\uDE00" as the emoji, "\ud800" as a lone surrogate and "\<TAB>" as a tab. The lexer combines two
    // escapes that spell a surrogate pair, so a Python string holds the one character too.
    testCase "two escapes that spell a surrogate pair are one code point" <| fun _ ->
        expectScalars "a: \"\\uD83D\\uDE00\"" [ pl "a"; dq "\U0001F600" ]
        expectScalars "a: \"x\\uD83D\\uDE00y\"" [ pl "a"; dq "x\U0001F600y" ]
        expectScalars "a: \"\\U0001F600\"" [ pl "a"; dq "\U0001F600" ]
        expectScalars "a: \"x\\\ty\"" [ pl "a"; dq "x\ty" ]
        // yaml@2.6.1 reads "\ud800" as a lone surrogate. A lone surrogate is no Unicode scalar value, and a Python string cannot hold it
        // through a StringBuilder, so the lexer rejects it on every target.
        expectError "a: \"\\ud800\"" "invalid-escape" "1:5-1:11"
        expectError "a: \"\\uDE00\"" "invalid-escape" "1:5-1:11"
        expectError "a: \"\\uD83D\\u0041\"" "invalid-escape" "1:5-1:11"

    // Tests.Escapes "Double-quoted multiline escaped line break". The old test continues the line at column 0, which yaml@2.6.1 rejects
    // under a key (BAD_INDENT), so the second line stands two spaces in.
    testCase "an escaped line break joins the lines without a space" <| fun _ ->
        expectScalars "key: \"a\\\n  b\"" [ pl "key"; dq "ab" ]

    // Tests.Escapes "Double-quoted escaped continuation with blank escaped line" and "keeps trailing spaces before the backslash".
    // yaml@2.6.1: "folded to a space,\tto a line feed" and "trailing spaces   next". The blanks in front of a backslash stay.
    testCase "an escaped continuation keeps the blanks in front of the backslash" <| fun _ ->
        expectScalars "\"folded \\\nto a space,\t\\\n \\\nto a line feed\"" [ dq "folded to a space,\tto a line feed" ]
        expectScalars "\"trailing spaces   \\\nnext\"" [ dq "trailing spaces   next" ]

    // Tests.Escapes "A line break in a double-quoted scalar folds into one space". yaml@2.6.1: {"k":"one two"}
    testCase "a line break in a double-quoted scalar folds into one space" <| fun _ ->
        expectScalars "k: \"one\n  two\"" [ pl "k"; dq "one two" ]

    // Tests.Escapes "Each empty line in a double-quoted fold becomes a line feed". yaml@2.6.1: {"k":"one\ntwo"}, {"k":"one\n\ntwo"} and {"k":"one\ntwo"}
    testCase "each empty line in a double-quoted fold becomes a line feed" <| fun _ ->
        expectScalars "k: \"one\n\n  two\"" [ pl "k"; dq "one\ntwo" ]
        expectScalars "k: \"one\n\n\n  two\"" [ pl "k"; dq "one\n\ntwo" ]
        expectScalars "k: \"one\n \t \n  two\"" [ pl "k"; dq "one\ntwo" ]

    // Tests.Escapes "A double-quoted fold drops the blanks around the line break". yaml@2.6.1: {"k":"one two"} and {"k":"  a b  "}
    testCase "a double-quoted fold drops the blanks around the line break" <| fun _ ->
        expectScalars "k: \"one \t\n  two\"" [ pl "k"; dq "one two" ]
        expectScalars "k: \"  a\n  b  \"" [ pl "k"; dq "  a b  " ]

    // Tests.Escapes "An escaped line break in a double-quoted scalar joins the lines without a space". yaml@2.6.1: {"k":"onetwo"} and {"k":"one two"}
    testCase "an escaped line break drops the blanks of the next line and keeps the blank in front of the backslash" <| fun _ ->
        expectScalars "k: \"one\\\n  two\"" [ pl "k"; dq "onetwo" ]
        expectScalars "k: \"one \\\n  two\"" [ pl "k"; dq "one two" ]

    // Tests.Escapes "A blank that an escape produces stays in front of a double-quoted fold".
    // yaml@2.6.1: {"k":"one\t two"}, {"k":"one  two"}, {"k":"one  two"} and {"k":"one\t two"}
    testCase "a blank that an escape produces stays in front of a fold" <| fun _ ->
        expectScalars "k: \"one\\t\n  two\"" [ pl "k"; dq "one\t two" ]
        expectScalars "k: \"one\\ \n  two\"" [ pl "k"; dq "one  two" ]
        expectScalars "k: \"one\\x20\n  two\"" [ pl "k"; dq "one  two" ]
        expectScalars "k: \"one\\t \n  two\"" [ pl "k"; dq "one\t two" ]

    // Tests.Escapes "CRLF and CR line breaks fold in a double-quoted scalar like LF". yaml@2.6.1: {"k":"one two"}, {"k":"one\ntwo"},
    // {"k":"onetwo"} and {"k":"one\r  two"} for the lone CR, because it does not treat a CR as a line break. YAML 1.2.2 (5.4) names CR as a
    // line break, and the lexer follows it.
    testCase "CRLF and CR fold like LF" <| fun _ ->
        expectScalars "k: \"one\r\n  two\"" [ pl "k"; dq "one two" ]
        expectScalars "k: \"one\r\n\r\n  two\"" [ pl "k"; dq "one\ntwo" ]
        expectScalars "k: \"one\\\r\n  two\"" [ pl "k"; dq "onetwo" ]
        expectScalars "k: \"one\r  two\"" [ pl "k"; dq "one two" ]

    // Tests.Escapes "A multiline double-quoted key folds" and "A multiline double-quoted sequence item folds".
    // yaml@2.6.1: {"one two":"v"}, {"one two":"x y"}, ["one two","k"] and ["one\ntwo","k"]. It rejects the key as an implicit key.
    testCase "a multiline double-quoted key and item fold" <| fun _ ->
        expectScalars "? \"one\n  two\"\n: v" [ dq "one two"; pl "v" ]
        expectScalars "{ \"one\n  two\": \"x\n  y\" }" [ dq "one two"; dq "x y" ]
        expectScalars "- \"one\n  two\"\n- k" [ dq "one two"; pl "k" ]
        expectScalars "- \"one\n\n  two\"\n- k" [ dq "one\ntwo"; pl "k" ]

    // Tests.Escapes "Decode.string returns the folded value of a multiline CWL valueFrom". yaml@2.6.1: {"inputBinding":{"valueFrom":"$(inputs.a + \"x y\")"}}
    testCase "a multiline CWL expression folds in a double-quoted and in a plain scalar" <| fun _ ->
        expectScalars "inputBinding:\n  valueFrom: \"$(inputs.a + \\\"x\n    y\\\")\"" [ pl "inputBinding"; pl "valueFrom"; dq "$(inputs.a + \"x y\")" ]
        expectScalars "inputBinding:\n  valueFrom: $(inputs.a + \"x\n    y\")" [ pl "inputBinding"; pl "valueFrom"; pl "$(inputs.a + \"x y\")" ]

    // YAML 1.2.2 (7.3.1) turns each empty line behind an escaped line break into a line feed. yaml@2.6.1 folds the first one into a space and reads
    // {"k":"a b"} and {"k":"a\nb"}, so this is a deviation from the reference that the specification adjudicates.
    testCase "an empty line behind an escaped line break is a line feed" <| fun _ ->
        expectScalars "k: \"a\\\n\n  b\"" [ pl "k"; dq "a\nb" ]
        expectScalars "k: \"a\\\n\n\n  b\"" [ pl "k"; dq "a\n\nb" ]

    // yaml@2.6.1 reads each of these without an error
    testCase "a single-quoted scalar folds blank lines and trims the blanks around a break" <| fun _ ->
        expectScalars "k: 'one \t\n \t two'" [ pl "k"; sq "one two" ]
        expectScalars "k: '  a\n\n\n  b  '" [ pl "k"; sq "  a\n\nb  " ]
        expectScalars "k: 'a\r\n  b'" [ pl "k"; sq "a b" ]
]

let private plainTests = testList "Plain scalars" [
    // Tests.YamlRead "KeyValue plain continuation folds into one scalar", "Root plain continuation folds into one scalar".
    // yaml@2.6.1: {"My Key":"My Value1 My Value2"} and "My Value1 My Value2". The old Reader kept "My Value1\nMy Value2" for the writer.
    testCase "a plain continuation folds into one scalar" <| fun _ ->
        expectScalars "\nMy Key: My Value1\n  My Value2\n" [ pl "My Key"; pl "My Value1 My Value2" ]
        expectScalars "\nMy Value1\n  My Value2\n" [ pl "My Value1 My Value2" ]
        // Tests.ScalarStyles "Reader preserves plain multiline sequence style and Decode folds semantics"
        expectScalars "- My Value 1\n  My Value 2\n" [ pl "My Value 1 My Value 2" ]
        // Tests.ScalarStyles "Indented mapping values remain a plain multiline scalar" and the boolean-looking variant
        expectScalars "MyKey:\n  test1\n  test2\n  test3\n" [ pl "MyKey"; pl "test1 test2 test3" ]
        expectScalars "MyKey:\n  true\n  true\n  false\n" [ pl "MyKey"; pl "true true false" ]

    // Tests.YamlRead "Indented multiline plain scalar expression decodes as one string" and "Inline-start multiline plain scalar expression
    // decodes as one string". yaml@2.6.1: {"valueFrom":"${ var picks = null; if (this !== null) { picks = [this]; } return picks; }"} and
    // {"valueFrom":"${ var picks = null; return picks; }"}
    testCase "a plain CWL expression over several lines is one scalar" <| fun _ ->
        expectScalars
            "valueFrom:\n  ${\n    var picks = null;\n    if (this !== null) { picks = [this]; }\n    return picks;\n  }"
            [ pl "valueFrom"; pl "${ var picks = null; if (this !== null) { picks = [this]; } return picks; }" ]
        expectScalars "valueFrom: ${\n  var picks = null;\n  return picks;\n  }" [ pl "valueFrom"; pl "${ var picks = null; return picks; }" ]

    // Tests.YamlRead "Hash without preceding whitespace stays in the scalar"
    testCase "a hash that follows a character is part of a plain scalar" <| fun _ ->
        expectScalars "run: tool.cwl#main\nlang: C#\nurl: http://x.org/a#frag" [ pl "run"; pl "tool.cwl#main"; pl "lang"; pl "C#"; pl "url"; pl "http://x.org/a#frag" ]

    // Tests.YamlRead "Hash directly after a flow closer is an error". yaml@2.6.1 rejects both inputs with MISSING_CHAR.
    testCase "a hash directly behind a flow closer is an error" <| fun _ ->
        expectError "k: [a]#c" "glued-comment" "1:7-1:8"
        expectError "Say: [Hello, World]# 420 blaze it" "glued-comment" "1:20-1:21"

    // Tests.YamlRead "Quotes behind a bracket in a plain value are text" and "Quotes behind a comma or a brace in a plain value are text".
    // yaml@2.6.1: {"k":"o[\"k\"]"}, {"k":"$(o['k'] || [1, 2])"}, {"k":"$(inputs[\"my-input\"].path)"}, {"k":"a, \"b\""} and {"k":"x {\"a\", 'b'}"}
    testCase "a quote behind a bracket, a comma or a brace in a plain scalar is text" <| fun _ ->
        expectScalars "k: o[\"k\"]" [ pl "k"; pl "o[\"k\"]" ]
        expectScalars "k: $(o['k'] || [1, 2])" [ pl "k"; pl "$(o['k'] || [1, 2])" ]
        expectScalars "k: $(inputs[\"my-input\"].path)" [ pl "k"; pl "$(inputs[\"my-input\"].path)" ]
        expectScalars "k: a, \"b\"" [ pl "k"; pl "a, \"b\"" ]
        expectScalars "k: x {\"a\", 'b'}" [ pl "k"; pl "x {\"a\", 'b'}" ]

    // Tests.YamlRead "A quote behind a property at a node start is a quoted scalar". yaml@2.6.1: {"k":"x"}
    testCase "a quote behind a tag is a quoted scalar" <| fun _ ->
        expectLexemes "k: !!str \"x\"" [ "plain:k"; ":"; "tag:!!|str"; "double:x" ]

    // Tests.YamlRead "Decode.string keeps the quotes of a CWL expression" and "folds a plain CWL expression over two lines". yaml@2.6.1:
    // {"valueFrom":"$(inputs[\"my-input\"].path)"}, {"valueFrom":"$(inputs.a + \"x\")"} and {"valueFrom":"$(f(inputs.a, \"x\", 1))"}
    testCase "a plain CWL expression keeps its quotes and folds its lines" <| fun _ ->
        expectScalars "valueFrom: $(inputs[\"my-input\"].path)" [ pl "valueFrom"; pl "$(inputs[\"my-input\"].path)" ]
        expectScalars "valueFrom: $(inputs.a +\n  \"x\")" [ pl "valueFrom"; pl "$(inputs.a + \"x\")" ]
        expectScalars "valueFrom: $(f(inputs.a,\n  \"x\", 1))" [ pl "valueFrom"; pl "$(f(inputs.a, \"x\", 1))" ]

    // Tests.Tokenizer "the lines of a plain scalar become one string with a quote as text". yaml@2.6.1: {"k":"$(inputs.a + \"x\")"},
    // {"k":"$(f(inputs.a, \"x\", 1))"}, {"k":"a \"b\""}, {"k":"a 'b' c"}, ["a \"b\""], "a \"b\"" and {"k":"a b \"c\""}
    testCase "the lines of a plain scalar fold into one scalar with a quote as text" <| fun _ ->
        expectScalars "k: $(inputs.a +\n  \"x\")" [ pl "k"; pl "$(inputs.a + \"x\")" ]
        expectScalars "k: $(f(inputs.a,\n  \"x\", 1))" [ pl "k"; pl "$(f(inputs.a, \"x\", 1))" ]
        expectScalars "k: a\n  \"b\"" [ pl "k"; pl "a \"b\"" ]
        expectScalars "k: a\n  'b' c" [ pl "k"; pl "a 'b' c" ]
        expectScalars "- a\n  \"b\"" [ pl "a \"b\"" ]
        expectScalars "a\n  \"b\"" [ pl "a \"b\"" ]
        expectScalars "k: a\n  b\n  \"c\"" [ pl "k"; pl "a b \"c\"" ]

    // Tests.Tokenizer "an indicator character on a continuation line is text". yaml@2.6.1: {"k":"a - b"}, {"k":"a [b"}, {"k":"a {b"},
    // {"k":"a - \"b\""}, {"k":"a [\"b\", c]"}, {"k":"a {\"b\"}"}, {"k":"a ? |"}, {"k":"a http://x"}, {"k":"a b:c"} and {"k":"a b#c"}
    testCase "an indicator character on a continuation line is text" <| fun _ ->
        expectScalars "k: a\n  - b" [ pl "k"; pl "a - b" ]
        expectScalars "k: a\n  [b" [ pl "k"; pl "a [b" ]
        expectScalars "k: a\n  {b" [ pl "k"; pl "a {b" ]
        expectScalars "k: a\n  - \"b\"" [ pl "k"; pl "a - \"b\"" ]
        expectScalars "k: a\n  [\"b\", c]" [ pl "k"; pl "a [\"b\", c]" ]
        expectScalars "k: a\n  {\"b\"}" [ pl "k"; pl "a {\"b\"}" ]
        expectScalars "k: a\n  ? |" [ pl "k"; pl "a ? |" ]
        expectScalars "k: a\n  http://x" [ pl "k"; pl "a http://x" ]
        expectScalars "k: a\n  b:c" [ pl "k"; pl "a b:c" ]
        expectScalars "k: a\n  b#c" [ pl "k"; pl "a b#c" ]

    // Tests.Tokenizer "a continuation line that looks like a block scalar header opens no block". yaml@2.6.1: [{"k":"v ? |"},"b"]
    testCase "a continuation line that looks like a block scalar header opens no block" <| fun _ ->
        expectScalars "- k: v\n      ? |\n- b" [ pl "k"; pl "v ? |"; pl "b" ]

    // Tests.Tokenizer "every segment is trimmed and a blank line is an empty segment". yaml@2.6.1: {"k":"a b c"}, {"k":"a\nb"} and {"k":"a\n\nb"}
    testCase "every line is trimmed and an empty line is a line feed" <| fun _ ->
        expectScalars "k: a   \n  b  \n  \tc\t" [ pl "k"; pl "a b c" ]
        expectScalars "k: a\n\n  b" [ pl "k"; pl "a\nb" ]
        expectScalars "k: a\n\n\n  b" [ pl "k"; pl "a\n\nb" ]

    // Tests.Tokenizer "a blank line behind the scalar stays a line of its own" and "a comment behind the last line stays on the merged line".
    // yaml@2.6.1: {"k":"a b","j":1} and {"k":"a b"}
    testCase "a blank line or a comment behind the last line is not part of the scalar" <| fun _ ->
        expectLexemes "k: a\n  b\n\nj: 1" [ "plain:k"; ":"; "plain:a b"; "blank"; "plain:j"; ":"; "plain:1" ]
        expectLexemes "k: a\n  b\n\n" [ "plain:k"; ":"; "plain:a b"; "blank" ]
        expectLexemes "k: a\n  b #c" [ "plain:k"; ":"; "plain:a b"; "#:c" ]

    // Tests.Tokenizer "the scalar ends at the next line that is not deeper". yaml@2.6.1: {"k":"a \"b\"","m":"z","n":"x\n"}
    testCase "a plain scalar ends at the next line that is not deeper" <| fun _ ->
        expectScalars "k: a\n  \"b\"\nm: \"z\"\nn: |\n  x\n" [ pl "k"; pl "a \"b\""; pl "m"; dq "z"; pl "n"; lit "x\n" ]

    // Tests.Tokenizer "the parent node owns a value on a line of its own". yaml@2.6.1: {"valueFrom":"$(f(inputs.a, \"x\", 1))"},
    // ["a \"b\", c"], {"k":"a \"b\""} and "a \"b\""
    testCase "the parent owns a value on a line of its own" <| fun _ ->
        expectScalars "valueFrom:\n  $(f(inputs.a,\n  \"x\", 1))" [ pl "valueFrom"; pl "$(f(inputs.a, \"x\", 1))" ]
        expectScalars "-\n  a\n  \"b\", c" [ pl "a \"b\", c" ]
        expectScalars "k:\n  a\n  \"b\"" [ pl "k"; pl "a \"b\"" ]
        expectScalars "a\n\"b\"" [ pl "a \"b\"" ]

    // Tests.Tokenizer "a line that is not deeper than the parent ends a value on a line of its own". yaml@2.6.1: {"k":"a","j":"x"} and ["a","b"]
    testCase "a line that is not deeper than the parent ends a value on a line of its own" <| fun _ ->
        expectScalars "k:\n  a\nj: \"x\"" [ pl "k"; pl "a"; pl "j"; dq "x" ]
        expectScalars "-\n  a\n- \"b\"" [ pl "a"; dq "b" ]

    // Tests.Tokenizer "the key of a sequence item owns its value, and a sibling key ends the scalar". yaml@2.6.1: [{"key":"a \"b\"","other":"c"}],
    // [["a","b"]] and [["a \"b\"","c"]]
    testCase "the key of a sequence item owns its value and a sibling key ends the scalar" <| fun _ ->
        expectScalars "- key:\n    a\n    \"b\"\n  other: \"c\"" [ pl "key"; pl "a \"b\""; pl "other"; dq "c" ]
        expectScalars "- -\n    a\n  - \"b\"" [ pl "a"; dq "b" ]
        expectScalars "- - a\n    \"b\"\n  - \"c\"" [ pl "a \"b\""; dq "c" ]

    // Tests.Tokenizer "a line of properties alone leaves the value to the node in front of it". yaml@2.6.1: {"k":"x \"y\""}
    testCase "a line of properties alone leaves the value to the node in front of it" <| fun _ ->
        expectScalars "k:\n  &a\n    x\n  \"y\"" [ pl "k"; pl "x \"y\"" ]

    // Tests.Tokenizer "a document marker ends the plain scalar and a plain scalar on the marker line is owned by the root". yaml@2.6.1: two
    // documents, "a" and "b", and "a \"b\""
    testCase "a document marker ends a plain scalar and a plain scalar behind the marker is owned by the root" <| fun _ ->
        expectLexemes "a\n--- \"b\"" [ "plain:a"; "---"; "double:b" ]
        expectScalars "--- a\n  \"b\"" [ pl "a \"b\"" ]

    // Tests.Tokenizer "text in the shape of a placeholder starts a plain scalar". yaml@2.6.1: {"k":"<s f=0/> [x, \"y\"]"}
    testCase "text in the shape of a placeholder is plain text" <| fun _ ->
        expectScalars "k: <s f=0/>\n  [x, \"y\"]" [ pl "k"; pl "<s f=0/> [x, \"y\"]" ]

    // Tests.Tokenizer "a continuation line that holds a mapping indicator is an error". yaml@2.6.1 rejects all four inputs with
    // BLOCK_AS_IMPLICIT_KEY and MULTILINE_IMPLICIT_KEY. The lexer returns the scalar over two lines and the colon behind it, and the
    // parser rejects an implicit key over several lines.
    testCase "a continuation line with a colon behind a blank ends the scalar and the colon is a value indicator" <| fun _ ->
        expectLexemes "k: a\n  b: c" [ "plain:k"; ":"; "plain:a b"; ":"; "plain:c" ]
        expectLexemes "k: a\n  b:" [ "plain:k"; ":"; "plain:a b"; ":" ]
        Expect.equal (lexSpans "k: a\n  b: c" |> List.item 2) "plain:a b 1:4-2:4" "The scalar spans two lines, which the parser rejects for a key"

    // Tests.Tokenizer "a comment ends the plain scalar and a deeper line behind it is an error". The lexer ends the scalar at the comment, and the
    // parser rejects the line behind it. yaml@2.6.1 rejects all six inputs with BAD_INDENT or MISSING_CHAR.
    testCase "a comment ends a plain scalar" <| fun _ ->
        expectLexemes "k: a # c\n  \"b\"" [ "plain:k"; ":"; "plain:a"; "#: c"; "double:b" ]
        expectLexemes "k: a\n# c\n  b" [ "plain:k"; ":"; "plain:a"; "#own: c"; "plain:b" ]
        expectLexemes "k: a\n  b #c\n  d" [ "plain:k"; ":"; "plain:a b"; "#:c"; "plain:d" ]
]

let private blockTests = testList "Block scalars" [
    // Tests.BlockScalars "Explicit indentation preserves leading content spaces", "Explicit Indentation Indicator", "Chomping Strip",
    // "Chomping Keep", "Folded Style" and "Literal style preserves additional indentation"
    testCase "the indentation indicator, the chomping and the style" <| fun _ ->
        expectScalars "- |1\n  explicit\n" [ blk BlockScalarStyle.Literal ChompingMode.Clip (Some 1) " explicit\n" ]
        expectScalars "doc: |2\n  explicit\n" [ pl "doc"; blk BlockScalarStyle.Literal ChompingMode.Clip (Some 2) "explicit\n" ]
        expectScalars "strip: |-\n  text\n" [ pl "strip"; blk BlockScalarStyle.Literal ChompingMode.Strip None "text" ]
        expectScalars "keep: |+\n  text\n\n" [ pl "keep"; blk BlockScalarStyle.Literal ChompingMode.Keep None "text\n\n" ]
        expectScalars "folded: >\n  line 1\n  line 2\n" [ pl "folded"; fold "line 1 line 2\n" ]
        expectScalars "doc: |\n  line1\n    indented\n  line3\n" [ pl "doc"; lit "line1\n  indented\nline3\n" ]

    // Tests.ScalarStyles "Chomping matrix round-trip preserves style and semantic value", the reading half. yaml@2.6.1 gives the values.
    testCase "the six combinations of style and chomping give the cooked values and keep the header" <| fun _ ->
        let cases =
            [ "k: |-\n  a\n  b\n", blk BlockScalarStyle.Literal ChompingMode.Strip None "a\nb"
              "k: |\n  a\n  b\n", blk BlockScalarStyle.Literal ChompingMode.Clip None "a\nb\n"
              "k: |+\n  a\n  b\n\n", blk BlockScalarStyle.Literal ChompingMode.Keep None "a\nb\n\n"
              "k: >-\n  a\n  b\n", blk BlockScalarStyle.Folded ChompingMode.Strip None "a b"
              "k: >\n  a\n  b\n", blk BlockScalarStyle.Folded ChompingMode.Clip None "a b\n"
              "k: >+\n  a\n  b\n\n", blk BlockScalarStyle.Folded ChompingMode.Keep None "a b\n\n" ]
        for (input, expected) in cases do
            expectScalars input [ pl "k"; expected ]

    // Tests.ScalarStyles "Explicit indentation indicator round-trip keeps a root scalar". yaml@2.6.1 reads "  x\n".
    testCase "an indentation indicator at the root keeps the spaces beyond it" <| fun _ ->
        expectScalars "|2\n    x\n" [ blk BlockScalarStyle.Literal ChompingMode.Clip (Some 2) "  x\n" ]

    // Tests.ScalarStyles "Root block scalar preserves tag/anchor/comment on write" and "Resolved tag round-trip stays semantically stable"
    testCase "a tag, an anchor and a header comment in front of a root block scalar are lexemes of their own" <| fun _ ->
        expectLexemes "!<tag:foo> &a1 | # c\n  hi\n" [ "!<tag:foo>"; "&a1"; "literal:hi\\n" ]
        let headerComments = scalarsOf "!<tag:foo> &a1 | # c\n  hi\n" |> List.map (fun scalar -> scalar.HeaderComment |> Option.map (fun comment -> comment.Text))
        Expect.equal headerComments [ Some " c" ] "The header comment sits on the scalar"
        expectLexemes "foo: !!str bar" [ "plain:foo"; ":"; "tag:!!|str"; "plain:bar" ]

    // Tests.BlockScalars "Folded style preserves more-indented lines"
    testCase "a folded scalar keeps the line breaks around a more indented line" <| fun _ ->
        expectScalars
            "folded: >\n  Sammy Sosa completed another\n  fine season with great stats.\n\n    63 Home Runs\n    0.288 Batting Average\n\n  What a year!\n"
            [ pl "folded"; fold "Sammy Sosa completed another fine season with great stats.\n\n  63 Home Runs\n  0.288 Batting Average\n\nWhat a year!\n" ]

    // Tests.BlockScalars "Block scalar header supports inline comments"
    testCase "the comment behind the header belongs to the scalar" <| fun _ ->
        let found = scalarsOf "doc: | # comment\n  hello\n" |> List.item 1
        Expect.equal found.Value "hello\n" "The value"
        Expect.equal (found.HeaderComment |> Option.map (fun comment -> comment.Text)) (Some " comment") "The header comment"
        Expect.equal (found.HeaderComment |> Option.bind (fun comment -> comment.Span) |> Option.map showSpan) (Some "1:8-1:17") "Its span"

    // Tests.BlockScalars "Root-level folded scalar parses", "Block scalar preserves leading blank content line", "Sequence item supports literal block scalar",
    // "Literal block scalar preserves trailing spaces"
    testCase "a root scalar, a leading blank line, a sequence item and trailing spaces" <| fun _ ->
        expectScalars ">\n text line1\n text line2\n" [ fold "text line1 text line2\n" ]
        expectScalars "doc: |\n\n  hello\n" [ pl "doc"; lit "\nhello\n" ]
        expectScalars "- |\n  text\n" [ lit "text\n" ]
        expectScalars "doc: |\n  a \n  b  \n" [ pl "doc"; lit "a \nb  \n" ]

    // Tests.BlockScalars "Block scalar keeps quote delimiters exactly", "Block scalar keeps quote delimiters on every body line",
    // "Block scalar with hash content" and "Block scalar preserves final hash content before dedent"
    testCase "quotes and hashes in a body are content" <| fun _ ->
        expectScalars "doc: |\n  sql: 'it''s'\n  if x == \"y\"\n" [ pl "doc"; lit "sql: 'it''s'\nif x == \"y\"\n" ]
        expectScalars
            "expression: >\n  ${ return (function() {\n    var permittedKeys = ['class', 'basename'];\n    if (shortened.basename === \"own\") return null;\n  })(); }\n"
            [ pl "expression"; fold "${ return (function() {\n  var permittedKeys = ['class', 'basename'];\n  if (shortened.basename === \"own\") return null;\n})(); }\n" ]
        expectScalars "comment: |\n  # Not a comment\n  value\n" [ pl "comment"; lit "# Not a comment\nvalue\n" ]
        expectScalars
            "doc: |\n  Workflow doc line.\n  # This line is doc text, not a YAML comment.\nnext: value"
            [ pl "doc"; lit "Workflow doc line.\n# This line is doc text, not a YAML comment.\n"; pl "next"; pl "value" ]

    // Tests.YamlRead "Hash inside a literal block scalar is content". The old Reader found no comment in the tree.
    testCase "a hash line inside a literal block scalar is content and no comment lexeme follows" <| fun _ ->
        expectLexemes "s: |\n  line\n  # not a comment\n" [ "plain:s"; ":"; "literal:line\\n# not a comment\\n" ]

    // Tests.BlockScalars "Combined explicit indent and strip chomping" and "Explicit indentation indicator keeps spaces beyond the indicated baseline"
    testCase "an indentation digit and a chomping sign together" <| fun _ ->
        expectScalars "data: |2-\n  text\n  more\n\n" [ pl "data"; blk BlockScalarStyle.Literal ChompingMode.Strip (Some 2) "text\nmore" ]
        expectScalars "key: |2\n     x\n   y\n" [ pl "key"; blk BlockScalarStyle.Literal ChompingMode.Clip (Some 2) "   x\n y\n" ]
        expectScalars "a: |-1\n x\n" [ pl "a"; blk BlockScalarStyle.Literal ChompingMode.Strip (Some 1) "x" ]
        expectScalars "a: |1-\n x\n" [ pl "a"; blk BlockScalarStyle.Literal ChompingMode.Strip (Some 1) "x" ]

    // Tests.BlockScalars "Literal block scalar keeps the exact indentation of a more indented line", "keeps a more indented hash line as content",
    // "keeps two spaces before a deeper hash line", "Folded block scalar keeps the line breaks around a more indented line" and "keeps mixed
    // indentation depths with hash lines"
    testCase "the indentation of a body is relative to the least indented line" <| fun _ ->
        expectScalars "key: |\n    text\n     more\n" [ pl "key"; lit "text\n more\n" ]
        expectScalars "key: |\n    text\n     # more\n" [ pl "key"; lit "text\n # more\n" ]
        expectScalars "key: |\n    text\n      # two\n" [ pl "key"; lit "text\n  # two\n" ]
        expectScalars "key: >\n    text\n     more\n    end\n" [ pl "key"; fold "text\n more\nend\n" ]
        expectScalars "key: |\n    a\n     # b\n         c\n       # d\n    e\n" [ pl "key"; lit "a\n # b\n     c\n   # d\ne\n" ]

    // Tests.BlockScalars "Nested mapping block scalar keeps indentation and the next key", "Root block scalar keeps the exact indentation of a
    // more indented line", "First sequence item block scalar keeps indentation and the next item", "Later sequence item block scalars keep their
    // indentation" and "Indentless sequence under a key keeps block scalar indentation"
    testCase "a body ends at the sibling of the node that owns it" <| fun _ ->
        expectScalars "outer:\n  key: |\n    x\n     y\n  next: 1\n" [ pl "outer"; pl "key"; lit "x\n y\n"; pl "next"; pl "1" ]
        expectScalars "|\n  x\n   y\n" [ lit "x\n y\n" ]
        expectScalars "- |\n  x\n   y\n- z\n" [ lit "x\n y\n"; pl "z" ]
        expectScalars "- a\n- |\n  x\n   y\n- b\n" [ pl "a"; lit "x\n y\n"; pl "b" ]
        expectScalars "key:\n- |\n  x\n   y\n- z\n" [ pl "key"; lit "x\n y\n"; pl "z" ]

    // Tests.BlockScalars "Keep chomping retains trailing blank lines before a dedented sibling" and "of a nested scalar"
    testCase "keep chomping retains the blank lines in front of the sibling" <| fun _ ->
        expectScalars "a: |+\n  x\n\n\nb: 1\n" [ pl "a"; blk BlockScalarStyle.Literal ChompingMode.Keep None "x\n\n\n"; pl "b"; pl "1" ]
        expectScalars "outer:\n  key: |+\n    x\n\nnext: 1\n" [ pl "outer"; pl "key"; blk BlockScalarStyle.Literal ChompingMode.Keep None "x\n\n"; pl "next"; pl "1" ]

    // Tests.BlockScalars "Header-looking body text stays literal content" and "Header-looking text in a body stays literal in a sequence item and a folded scalar".
    // yaml@2.6.1: ["a: |\n  b\n- c\n"] and {"key":"x: | y\n"}
    testCase "text that looks like a header or an indicator inside a body is content" <| fun _ ->
        expectScalars "key: |\n  a: |\n    b\n  - c\n" [ pl "key"; lit "a: |\n  b\n- c\n" ]
        expectScalars "- |\n  a: |\n    b\n  - c\n" [ lit "a: |\n  b\n- c\n" ]
        expectScalars "key: >\n  x: |\n  y\n" [ pl "key"; fold "x: | y\n" ]

    // Tests.BlockScalars "Hash lines and placeholder-shaped text in a body are content", "A body line that looks like the placeholder of a comment is
    // not the comment of the document", "of a string", "A body line shaped like a comment placeholder reads end to end as text" and "A lead comment and a
    // hash line at the end of a body do not mix". yaml@2.6.1: {"a":"# not a comment\n<c f=0/>\n<s f=3/>\n"}, {"a":"<c f=0/>\n","b":1},
    // {"a":"<s f=0/>\n","b":"q"}, {"a":"<c f=0/>\n"}, {"a":"<c f=0/>\n# d\n"} and {"a":"x\n\n# tail\n"}
    testCase "a body keeps text in the shape of a placeholder" <| fun _ ->
        expectScalars "a: |\n  # not a comment\n  <c f=0/>\n  <s f=3/>\n" [ pl "a"; lit "# not a comment\n<c f=0/>\n<s f=3/>\n" ]
        expectLexemes "a: |\n  <c f=0/>\nb: 1 # real\n" [ "plain:a"; ":"; "literal:<c f=0/>\\n"; "plain:b"; ":"; "plain:1"; "#: real" ]
        expectScalars "a: |\n  <s f=0/>\nb: 'q'\n" [ pl "a"; lit "<s f=0/>\n"; pl "b"; sq "q" ]
        expectScalars "a: |\n  <c f=0/>\n" [ pl "a"; lit "<c f=0/>\n" ]
        expectLexemes "# c\na: |\n  <c f=0/>\n  # d\n" [ "#own: c"; "plain:a"; ":"; "literal:<c f=0/>\\n# d\\n" ]
        expectScalars "a: |\n  x\n\n  # tail\n" [ pl "a"; lit "x\n\n# tail\n" ]

    // Tests.BlockScalars "A header-shaped continuation line of a plain scalar does not throw" is under the plain scalars.

    // Tests.BlockScalars "Empty and blank-only bodies keep line breaks only under keep chomping". yaml@2.6.1: an empty body reads as "" for every
    // chomping. Blank lines count under keep only.
    testCase "empty and blank-only bodies keep line breaks only under keep chomping" <| fun _ ->
        for (indicator, style) in [ "|", BlockScalarStyle.Literal; ">", BlockScalarStyle.Folded ] do
            for (suffix, chomping) in [ "-", ChompingMode.Strip; "", ChompingMode.Clip; "+", ChompingMode.Keep ] do
                for blanks in [ 0; 2 ] do
                    let value = if chomping = ChompingMode.Keep then String.replicate blanks "\n" else ""
                    let header = "a: " + indicator + suffix + "\n" + String.replicate blanks "\n"
                    expectScalars (header + "b: 1\n") [ pl "a"; blk style chomping None value; pl "b"; pl "1" ]
                    expectScalars header [ pl "a"; blk style chomping None value ]

    // Tests.BlockScalars "Leading and trailing blank lines of a body follow the chomping". yaml@2.6.1: "\nx" for strip, "\nx\n" for clip and
    // "\nx\n\n\n" for keep, with both styles
    testCase "leading and trailing blank lines of a body follow the chomping" <| fun _ ->
        for (indicator, style) in [ "|", BlockScalarStyle.Literal; ">", BlockScalarStyle.Folded ] do
            for (suffix, chomping, value) in [ "-", ChompingMode.Strip, "\nx"; "", ChompingMode.Clip, "\nx\n"; "+", ChompingMode.Keep, "\nx\n\n\n" ] do
                expectScalars ("a: " + indicator + suffix + "\n\n  x\n\n\nb: 1\n") [ pl "a"; blk style chomping None value; pl "b"; pl "1" ]

    // Tests.BlockScalars "An empty block scalar reads as the empty string as mapping value, sequence item, root scalar, explicit key, nested item and
    // explicit key item". yaml@2.6.1: {"a":"","b":1}, ["","x"], "", {"a":""}, {"":null}, {"":"v"}, [[""],"x"] and [{"":null},"x"]
    testCase "an empty block scalar is the empty string in every position" <| fun _ ->
        expectScalars "a: |\nb: 1\n" [ pl "a"; lit ""; pl "b"; pl "1" ]
        expectScalars "- |\n- x\n" [ lit ""; pl "x" ]
        expectScalars "|\n" [ lit "" ]
        expectScalars "a: |" [ pl "a"; lit "" ]
        expectScalars "? |\n" [ lit "" ]
        expectScalars "? |\n: v\n" [ lit ""; pl "v" ]
        expectScalars "- - |\n- x\n" [ lit ""; pl "x" ]
        expectScalars "- ? |\n- x\n" [ lit ""; pl "x" ]

    // Tests.BlockScalars "A block scalar header reads its body as an explicit key and as a nested sequence item". yaml@2.6.1: {"x\n":"v"},
    // {"x\n":null}, [["x\n"]] and [{"x\n":null}]
    testCase "a block scalar is the body of an explicit key and of a nested sequence item" <| fun _ ->
        expectScalars "? |\n  x\n: v\n" [ lit "x\n"; pl "v" ]
        expectScalars "? |\n  x\n" [ lit "x\n" ]
        expectScalars "- - |\n    x\n" [ lit "x\n" ]
        expectScalars "- ? |\n    x\n" [ lit "x\n" ]

    // Tests.BlockScalars "A comment between the header and the content indentation ends the scalar and precedes the next key" and the tests
    // "Comment less indented than the content ends a nested block scalar", "Comment less indented than an explicit indicator ends the block scalar",
    // "Comment less indented than the content ends a sequence item block scalar" and "... a folded block scalar". yaml@2.6.1: {"a":"x\n","b":1},
    // {"a":{"b":"x\n","d":1}}, {"a":"  x\n","b":1}, ["x\n","y"] and {"key":"folded\n","next":1}
    testCase "a comment that is less indented than the content ends the scalar and stays a lexeme of its own" <| fun _ ->
        expectLexemes "a: |\n    x\n # c\nb: 1\n" [ "plain:a"; ":"; "literal:x\\n"; "#own: c"; "plain:b"; ":"; "plain:1" ]
        expectLexemes "a:\n  b: |\n      x\n    # c\n  d: 1\n" [ "plain:a"; ":"; "plain:b"; ":"; "literal:x\\n"; "#own: c"; "plain:d"; ":"; "plain:1" ]
        expectLexemes "a: |2\n    x\n # c\nb: 1\n" [ "plain:a"; ":"; "literal2:  x\\n"; "#own: c"; "plain:b"; ":"; "plain:1" ]
        expectLexemes "- |\n    x\n  # c\n- y\n" [ "-"; "literal:x\\n"; "#own: c"; "-"; "plain:y" ]
        expectLexemes "key: >\n    folded\n  # c\nnext: 1\n" [ "plain:key"; ":"; "folded:folded\\n"; "#own: c"; "plain:next"; ":"; "plain:1" ]

    // Tests.BlockScalars "Several comments at different indentation keep their order across blank lines", "Comments and blank lines after a
    // terminating comment stay outside a keep scalar", "Comments at the end of the input stay in the block of the scalar header", "Trailing comment
    // between the header and the content indentation ends the block scalar" and "Keep chomping stops before a comment that is less indented than the
    // content". yaml@2.6.1: {"a":"x\n","b":1}, {"a":"x\n\n\n","b":1}, {"a":{"b":"x\n"}}, {"x":{"a":"body\n"}} and {"a":"x\n\n","b":1}
    testCase "the comments and blank lines behind a scalar are lexemes in their order" <| fun _ ->
        expectLexemes "a: |\n    x\n  # c1\n # c2\n\n# c3\nb: 1\n" [ "plain:a"; ":"; "literal:x\\n"; "#own: c1"; "#own: c2"; "blank"; "#own: c3"; "plain:b"; ":"; "plain:1" ]
        expectLexemes "a: |+\n  x\n\n\n# c\nb: 1\n" [ "plain:a"; ":"; "literal+:x\\n\\n\\n"; "#own: c"; "plain:b"; ":"; "plain:1" ]
        expectLexemes "a:\n  b: |\n      x\n    # c1\n\n  # c2\n\n# c3\n" [ "plain:a"; ":"; "plain:b"; ":"; "literal:x\\n"; "#own: c1"; "blank"; "#own: c2"; "blank"; "#own: c3" ]
        expectLexemes "x:\n  a: |\n      body\n    # c\n" [ "plain:x"; ":"; "plain:a"; ":"; "literal:body\\n"; "#own: c" ]
        expectLexemes "a: |+\n    x\n\n  # c\nb: 1\n" [ "plain:a"; ":"; "literal+:x\\n\\n"; "#own: c"; "plain:b"; ":"; "plain:1" ]

    // Tests.BlockScalars "A trailing comment between two levels joins the outer level of a scalar item". yaml@2.6.1: {"k":["x\n"]} and {"k":[""]}
    testCase "a scalar item and a scalar with an indicator and no content are followed by their comment" <| fun _ ->
        expectLexemes "k:\n    - |\n      x\n  # note\n" [ "plain:k"; ":"; "-"; "literal:x\\n"; "#own: note" ]
        expectLexemes "k:\n    - |4-\n\n  # note\n" [ "plain:k"; ":"; "-"; "literal-4:"; "#own: note" ]

    // Tests.BlockScalars "A comment before a scalar header precedes the key", "A comment after a scalar body belongs to the next sibling or to the
    // dedented node". yaml@2.6.1: {"a":"x\n"}, {"top":{"k":1,"s":"body\n"}}, {"a":{"s":"body\n","t":1}} and {"a":{"s":"body\n"},"b":1}
    testCase "a comment before a scalar header and after a body" <| fun _ ->
        expectLexemes "# lead\na: |\n  x\n" [ "#own: lead"; "plain:a"; ":"; "literal:x\\n" ]
        expectLexemes "top:\n  k: 1\n  # c\n  s: |\n    body\n" [ "plain:top"; ":"; "plain:k"; ":"; "plain:1"; "#own: c"; "plain:s"; ":"; "literal:body\\n" ]
        expectLexemes "a:\n  s: |\n    body\n  # c\n  t: 1\n" [ "plain:a"; ":"; "plain:s"; ":"; "literal:body\\n"; "#own: c"; "plain:t"; ":"; "plain:1" ]
        expectLexemes "a:\n  s: |\n    body\n# c\nb: 1\n" [ "plain:a"; ":"; "plain:s"; ":"; "literal:body\\n"; "#own: c"; "plain:b"; ":"; "plain:1" ]
]

let private spaceLineTests = testList "Block scalar lines of spaces" [
    // Tests.BlockScalars "A literal line of spaces up to the content indentation is an empty line". yaml@2.6.1: "a\n\nb\n" for a line of two spaces
    // and "a\n\n\nb\n" for a line without spaces and a line of one space
    testCase "a literal line of spaces up to the content indentation is an empty line" <| fun _ ->
        expectScalars "k: |\n  a\n  \n  b\n" [ pl "k"; lit "a\n\nb\n" ]
        expectScalars "k: |\n  a\n\n \n  b\n" [ pl "k"; lit "a\n\n\nb\n" ]

    // Tests.BlockScalars "A literal line of spaces beyond the content indentation keeps the spaces beyond it". yaml@2.6.1: "a\n  \nb\n" for four spaces
    // at a content indentation of two, and {"a":{"k":"x\n  \ny\n","z":1}} for the nested input
    testCase "a literal line of spaces beyond the content indentation keeps the spaces beyond it" <| fun _ ->
        expectScalars "k: |\n  a\n    \n  b\n" [ pl "k"; lit "a\n  \nb\n" ]
        expectScalars "a:\n  k: |\n    x\n      \n    y\n  z: 1\n" [ pl "a"; pl "k"; lit "x\n  \ny\n"; pl "z"; pl "1" ]

    // Tests.BlockScalars "Trailing lines of spaces follow the chomping". yaml@2.6.1: "a" and "a\n\n " for strip, "a\n\n \n" for clip and "a\n\n \n\n" for
    // keep. A line of three spaces is content, the others are empty.
    testCase "trailing lines of spaces follow the chomping" <| fun _ ->
        expectScalars "k: |-\n  a\n  \n  \n" [ pl "k"; blk BlockScalarStyle.Literal ChompingMode.Strip None "a" ]
        expectScalars "k: |-\n  a\n  \n   \n" [ pl "k"; blk BlockScalarStyle.Literal ChompingMode.Strip None "a\n\n " ]
        expectScalars "k: |\n  a\n  \n   \n" [ pl "k"; lit "a\n\n \n" ]
        expectScalars "k: |+\n  a\n  \n   \n  \n" [ pl "k"; blk BlockScalarStyle.Literal ChompingMode.Keep None "a\n\n \n\n" ]

    // Tests.BlockScalars "A folded line of spaces between folded lines is an empty line". yaml@2.6.1: "a\nb\n"
    testCase "a folded line of spaces between folded lines is an empty line" <| fun _ ->
        expectScalars "k: >\n  a\n  \n  b\n" [ pl "k"; fold "a\nb\n" ]

    // Tests.BlockScalars "A folded line of spaces beyond the content indentation is a more indented line". yaml@2.6.1: "a\n  \nb\n" and "a\n  x\n  \nb\n"
    testCase "a folded line of spaces beyond the content indentation is a more indented line" <| fun _ ->
        expectScalars "k: >\n  a\n    \n  b\n" [ pl "k"; fold "a\n  \nb\n" ]
        expectScalars "k: >\n  a\n    x\n    \n  b\n" [ pl "k"; fold "a\n  x\n  \nb\n" ]

    // Tests.BlockScalars "A folded line of spaces next to a more indented line is an empty line". yaml@2.6.1: "a\n\n  x\nb\n" for the line in front of
    // the more indented line and "a\n  x\n\nb\n" for the line behind it
    testCase "a folded line of spaces next to a more indented line is an empty line" <| fun _ ->
        expectScalars "k: >\n  a\n  \n    x\n  b\n" [ pl "k"; fold "a\n\n  x\nb\n" ]
        expectScalars "k: >\n  a\n    x\n  \n  b\n" [ pl "k"; fold "a\n  x\n\nb\n" ]

    // Tests.BlockScalars "Lines of spaces in a scalar with an indentation indicator". yaml@2.6.1: "a\n\n  \nb\n" for a line of one space and a line of four,
    // and "  \na\n" for a first line of four spaces
    testCase "lines of spaces in a scalar with an indentation indicator" <| fun _ ->
        expectScalars "k: |2\n  a\n \n    \n  b\n" [ pl "k"; blk BlockScalarStyle.Literal ChompingMode.Clip (Some 2) "a\n\n  \nb\n" ]
        expectScalars "k: |2\n    \n  a\n" [ pl "k"; blk BlockScalarStyle.Literal ChompingMode.Clip (Some 2) "  \na\n" ]

    // Tests.BlockScalars "A line of spaces in a sequence item scalar". yaml@2.6.1: ["a\n  \nb\n","c"]
    testCase "a line of spaces in a sequence item scalar" <| fun _ ->
        expectScalars "- |\n  a\n    \n  b\n- c\n" [ lit "a\n  \nb\n"; pl "c" ]

    // Tests.BlockScalars "A tab is content and never indentation". yaml@2.6.1: "a\n\t\nb\n"
    testCase "a tab is content and never indentation" <| fun _ ->
        expectScalars "k: |\n  a\n  \t\n  b\n" [ pl "k"; lit "a\n\t\nb\n" ]

    // Tests.BlockScalars "A line of spaces in front of the first content line is empty and no longer than that line". yaml@2.6.1: "\na\n" in both
    // styles. It rejects a line of four spaces in front of a content line with two.
    testCase "a line of spaces in front of the first content line may not be longer than that line" <| fun _ ->
        expectScalars "k: |\n  \n  a\n" [ pl "k"; lit "\na\n" ]
        expectScalars "k: >\n  \n  a\n" [ pl "k"; fold "\na\n" ]
        expectError "k: |\n    \n  a\n" "leading-empty-line" "2:1-3:1"

    // yaml@2.6.1 and YAML 1.2.2 agree: without an indentation indicator the longest of the lines of spaces sets the indentation, so each of them is
    // an empty line and the body is empty. Under keep chomping the lines count as line feeds.
    testCase "a body of lines of spaces only and no indentation indicator holds empty lines" <| fun _ ->
        expectScalars "k: |-\n   \n  \n\nj: 1\n" [ pl "k"; blk BlockScalarStyle.Literal ChompingMode.Strip None ""; pl "j"; pl "1" ]
        expectScalars "- |+\n  \n  \n \n- 1\n" [ blk BlockScalarStyle.Literal ChompingMode.Keep None "\n\n\n"; pl "1" ]
        expectScalars "k: >\n \n   \nj: 1\n" [ pl "k"; fold ""; pl "j"; pl "1" ]

    // Deliberate difference from yaml@2.6.1, which reads `k: |1\n   \nj: 1` as {"k":"","j":1}. With an indentation indicator the content indentation
    // is fixed, and a line of spaces is empty only when it is no longer than that ([171] l-nb-literal-text(n) is l-empty(n,block-in)* followed by
    // s-indent(n) nb-char+, and [170] adds the indicator to n). A line of spaces beyond the indentation is content, with the spaces beyond it.
    testCase "a line of spaces beyond an indentation indicator is content even when no other line is" <| fun _ ->
        expectScalars "k: |1\n   \nj: 1" [ pl "k"; blk BlockScalarStyle.Literal ChompingMode.Clip (Some 1) "  \n"; pl "j"; pl "1" ]
        expectScalars "k: |1-\n   \nj: 1" [ pl "k"; blk BlockScalarStyle.Literal ChompingMode.Strip (Some 1) "  "; pl "j"; pl "1" ]
        expectScalars "k: >1\n   \nj: 1" [ pl "k"; blk BlockScalarStyle.Folded ChompingMode.Clip (Some 1) "  \n"; pl "j"; pl "1" ]
        // A line of spaces up to the indentation is empty, and so is the body of such lines.
        expectScalars "k: |1\n \nj: 1" [ pl "k"; blk BlockScalarStyle.Literal ChompingMode.Clip (Some 1) ""; pl "j"; pl "1" ]

    // yaml@2.6.1 rejects `|\n  a\n\t\n` and `k: |\n  a\n\t\nj: 1` with BAD_INDENT. A line of blanks with a tab in it is no content line and no
    // empty line of the scalar, so it ends the scalar, and [78] l-comment makes it a blank line of the document prefix or of the mapping ([211], [202]).
    testCase "a line of blanks with a tab ends a block scalar without an error" <| fun _ ->
        expectLexemes "|\n  a\n\t\n" [ "literal:a\\n"; "blank" ]
        expectLexemes "|\n  a\n \t\n" [ "literal:a\\n"; "blank" ]
        expectLexemes "k: |\n    a\n  \t\nj: 1" [ "plain:k"; ":"; "literal:a\\n"; "blank"; "plain:j"; ":"; "plain:1" ]
        expectLexemes "k: >-\n    a\n\t\nj: 1" [ "plain:k"; ":"; "folded-:a"; "blank"; "plain:j"; ":"; "plain:1" ]
        // A tab in front of text behind too few spaces is still an error.
        expectError "k: |\n    a\n  \tb\nj: 1" "block-indent" "3:3-3:4"
        expectError "|\n  a\n\tb\n" "block-indent" "3:1-3:2"
]

let private lineBreakTests = testList "Line breaks in scalars" [
    testCase "CRLF and CR in a block scalar and in a plain scalar are line feeds" <| fun _ ->
        expectScalars "k: |\r\n  a\r\n  b\r\n" [ pl "k"; lit "a\nb\n" ]
        expectScalars "k: |\r  a\r  b\r" [ pl "k"; lit "a\nb\n" ]
        expectScalars "k: >\r\n  a\r\n\r\n  b\r\n" [ pl "k"; fold "a\nb\n" ]
        expectScalars "k: a\r\n  b\r\n\r\n  c" [ pl "k"; pl "a b\nc" ]
        expectScalars "k: 'a\r\n  b'" [ pl "k"; sq "a b" ]

    testCase "a byte order mark and the end of the text without a line break" <| fun _ ->
        expectScalars "\uFEFFa: b" [ pl "a"; pl "b" ]
        expectScalars "k: |-\n  a" [ pl "k"; blk BlockScalarStyle.Literal ChompingMode.Strip None "a" ]
        expectScalars "k: a\n  b" [ pl "k"; pl "a b" ]

    // Deliberate difference from yaml@2.6.1, which reads `k: |\n  a` as {"k":"a\n"} and `k: |+\n  a` as {"k":"a\n"}. [165] b-chomped-last(t) is
    // b-as-line-feed or the end of the input for clip and keep, so a last content line that ends the input has no line break to keep. The value is
    // "a" for every chomping, and the line break is back as soon as the input has one.
    testCase "a last content line that ends the input has no final line feed" <| fun _ ->
        expectScalars "k: |\n  a" [ pl "k"; lit "a" ]
        expectScalars "k: |+\n  a" [ pl "k"; blk BlockScalarStyle.Literal ChompingMode.Keep None "a" ]
        expectScalars "k: |-\n  a" [ pl "k"; blk BlockScalarStyle.Literal ChompingMode.Strip None "a" ]
        expectScalars "|\n  a" [ lit "a" ]
        expectScalars "k: >\n  a\n  b" [ pl "k"; fold "a b" ]
        expectScalars "k: >+\n  a" [ pl "k"; blk BlockScalarStyle.Folded ChompingMode.Keep None "a" ]
        expectScalars "- |\n  a" [ lit "a" ]
        expectScalars "k: |\n  a\n" [ pl "k"; lit "a\n" ]
        expectScalars "k: |+\n  a\n" [ pl "k"; blk BlockScalarStyle.Literal ChompingMode.Keep None "a\n" ]
        expectScalars "k: |+\n  a\n\n" [ pl "k"; blk BlockScalarStyle.Literal ChompingMode.Keep None "a\n\n" ]
        expectScalars "k: |\r\n  a\r\n" [ pl "k"; lit "a\n" ]
        // A line of spaces at the end of the input without a line break is no empty line ([70] l-empty ends in b-as-line-feed).
        expectScalars "k: |+\n  a\n  " [ pl "k"; blk BlockScalarStyle.Literal ChompingMode.Keep None "a\n" ]
        // A line of spaces beyond the indentation is content, and it ends the input here.
        expectScalars "k: |\n  a\n    " [ pl "k"; lit "a\n  " ]
]

let private flowScalarTests = testList "Scalars in flow collections" [
    // Tests.YamlRead "Flow plain scalar value may contain colon". yaml@2.6.1: {"data":{"url":"http://example.com","time":"12:45"}}
    testCase "a plain scalar in a flow collection may hold a colon that no blank follows" <| fun _ ->
        expectScalars "data: {url: http://example.com, time: 12:45}" [ pl "data"; pl "url"; pl "http://example.com"; pl "time"; pl "12:45" ]

    // Tests.YamlRead "Inline flow sequence with double-quoted value containing comma", "Inline flow object with double-quoted value containing comma"
    // and "Inline flow sequence nested in block sequence with double-quoted comma"
    testCase "a quoted scalar in a flow collection may hold a comma" <| fun _ ->
        expectScalars "items: [\"hello, world\", c]" [ pl "items"; dq "hello, world"; pl "c" ]
        expectScalars "data: {key: \"a, b, c\"}" [ pl "data"; pl "key"; dq "a, b, c" ]
        expectScalars "steps:\n  - [\"a,b\", c]\n  - [d, e]" [ pl "steps"; dq "a,b"; pl "c"; pl "d"; pl "e" ]

    // Tests.YamlRead "Quotes in single-line flow collections stay quoted scalars". yaml@2.6.1: {"k":["a","b"]}, {"k":{"a":"b"}}, {"k":["x","y, z"]}
    // and [["a","b"]]
    testCase "quotes in a single-line flow collection stay quoted scalars" <| fun _ ->
        expectScalars "k: [\"a\", 'b']" [ pl "k"; dq "a"; sq "b" ]
        expectScalars "k: {\"a\": \"b\"}" [ pl "k"; dq "a"; dq "b" ]
        expectScalars "k: [x, \"y, z\"]" [ pl "k"; pl "x"; dq "y, z" ]
        expectScalars "- [\"a\", \"b\"]" [ dq "a"; dq "b" ]

    // Tests.Tokenizer "a plain scalar in a flow collection goes on in the next lines and a quote there is text". yaml@2.6.1: ["a \"b\""] for the
    // first input and ["a","b"] for the second. The third gives {"a":"b"}.
    testCase "a plain scalar in a flow collection goes on in the next lines and a quote there is text" <| fun _ ->
        expectScalars "[\n  a\n  \"b\"\n]" [ pl "a \"b\"" ]
        expectScalars "[\n  a,\n  \"b\"\n]" [ pl "a"; dq "b" ]
        expectScalars "{\n  a:\n  \"b\"\n}" [ pl "a"; dq "b" ]

    // Tests.Tokenizer "a plain scalar over several lines in a flow collection is one string table entry". yaml@2.6.1: [["a b"],["a\nb"],["a b","c d"],["a b #c"]].
    // The last result is for the hash without a blank, which the comment of the old test spells wrongly.
    testCase "a plain scalar over several lines in a flow collection folds" <| fun _ ->
        expectScalars "[\n  a\n  b\n]" [ pl "a b" ]
        expectScalars "[a\n\n b]" [ pl "a\nb" ]
        expectScalars "[\n  a\n  b,\n  c\n  d\n]" [ pl "a b"; pl "c d" ]
        expectScalars "[a\n b#c]" [ pl "a b#c" ]

    // Tests.Tokenizer "a plain scalar in a flow collection ends at a flow indicator, a key colon or a comment". yaml@2.6.1: ["a b","c"], ["a b:c"] and
    // {"a b":"c d"} for the last. The comment ends the scalar of the fourth.
    testCase "a plain scalar in a flow collection ends at a flow indicator, a colon behind a blank or a comment" <| fun _ ->
        expectScalars "[a\n b, c]" [ pl "a b"; pl "c" ]
        expectScalars "[a\n b:c]" [ pl "a b:c" ]
        expectLexemes "[a\n b # c\n]" [ "["; "plain:a b"; "#: c"; "]" ]
        expectLexemes "[a\n]" [ "["; "plain:a"; "]" ]
        expectLexemes "{a\n b: c d}" [ "{"; "plain:a b"; ":"; "plain:c d"; "}" ]

    // Tests.Tokenizer "the lines of a flow collection must be indented deeper than the node that owns it" and "a closer may sit at the column of the
    // node that owns the flow collection". yaml@2.6.1 rejects the first six inputs with BAD_INDENT and reads the others. The closer at the column of
    // the owner is a deliberate difference from [69] s-flow-line-prefix, [80] s-separate and [81] s-separate-lines, which ask for the indentation of
    // the owner plus one. The loader of cwltool (ruamel) and yaml@2.6.1 read that layout, and real CWL files use it for `expressionLib: [`.
    testCase "the lines of a flow collection have to be deeper than the node that owns it, and a closer may stand at its column" <| fun _ ->
        expectError "k: [a,\nb]" "flow-indent" "2:1-2:2"
        expectError "k: [\na,\nb\n]" "flow-indent" "2:1-2:2"
        expectError "k:\n  - [a,\n  b]" "flow-indent" "3:1-3:4"
        expectError "- k: [\n  a\n  ]" "flow-indent" "2:1-2:4"
        expectError "k:\n  j: [\n    a\n]" "flow-indent" "4:1-4:2"
        expectError "- [a,\nb]" "flow-indent" "2:1-2:2"
        expectLexemes "k: [\n  a\n]" [ "plain:k"; ":"; "["; "plain:a"; "]" ]
        expectLexemes "k: [\n  a\n ]" [ "plain:k"; ":"; "["; "plain:a"; "]" ]
        expectLexemes "[a,\nb]" [ "["; "plain:a"; ","; "plain:b"; "]" ]
        expectLexemes "- [\n a\n]" [ "-"; "["; "plain:a"; "]" ]
        expectLexemes "k: [a,\n]" [ "plain:k"; ":"; "["; "plain:a"; ","; "]" ]
        expectLexemes "k: [ a,\n# c\n  b ]" [ "plain:k"; ":"; "["; "plain:a"; ","; "#own: c"; "plain:b"; "]" ]
]

let Main = testList "Scalars" [
    quotedTests
    escapeTests
    plainTests
    blockTests
    spaceLineTests
    lineBreakTests
    flowScalarTests
]
