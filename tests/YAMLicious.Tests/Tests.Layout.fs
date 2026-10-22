module Tests.Layout

// The tests of the layout of a scalar. The parser keeps the line breaks that folded into the value of a scalar, and the
// writer writes the lines of the author again while they fit the value. Most tests parse a text and
// expect the writer to give the text back, because the writer indents the continuation lines by one indentation step. The
// text then has to read as the same stream, the layouts included, and a second write has to change nothing.
//
// The positions in a layout are indexes of the value in the unit of the target, so a test that has an astral character in
// front of a boundary computes the position with IndexOf.

open Fable.Pyxpecto
open YAMLicious

let private defaults = WriterOptions.defaults

let private withStyles (policy: StylePolicy) : WriterOptions = { defaults with Styles = policy }

/// The text parses to a stream and the writer gives the expected text with the options. That text reads as the same stream, layouts included,
/// and a second write gives it again.
let private expectLinesWith (options: WriterOptions) (input: string) (expected: string) =
    let stream = parseOk input
    let text = Writer.writeDocuments options stream
    Expect.equal text expected (sprintf "The text written for %A" input)
    Expect.streamEqual (parseOk text) stream (sprintf "The stream read from the text written for %A" input)
    Expect.equal (Writer.writeDocuments options (parseOk text)) text (sprintf "The second write of %A" input)

let private expectLines (input: string) (expected: string) = expectLinesWith defaults input expected

/// The writer gives the input back.
let private expectKept (input: string) = expectLines input input

/// The scalar behind the first key of the root mapping of the text.
let private firstValue (input: string) : YamlScalar =
    match (parseRoot input).Kind with
    | YamlNodeKind.Mapping collection ->
        match collection.Items.Head.Value.Kind with
        | YamlNodeKind.Scalar scalar -> scalar
        | _ -> failwith "A scalar was expected."
    | _ -> failwith "A mapping was expected."

let private scalarOf (node: YamlNode) : YamlScalar =
    match node.Kind with
    | YamlNodeKind.Scalar scalar -> scalar
    | _ -> failwith "A scalar was expected."

/// A value with the boundaries of the layout.
let private withBreaks (boundaries: (int * LayoutKind) list) (node: YamlNode) : YamlNode =
    let scalar = scalarOf node
    { node with Kind = YamlNodeKind.Scalar { scalar with Layout = Some { Boundaries = [ for (at, kind) in boundaries -> { At = at; Kind = kind } ] } } }

/// The node with the scalar changed.
let private updateScalar (change: YamlScalar -> YamlScalar) (node: YamlNode) : YamlNode =
    { node with Kind = YamlNodeKind.Scalar(change (scalarOf node)) }

/// A character above U+FFFF. It takes two indexes of a value on .NET and JavaScript and one on Python.
let private astral = "\U0001F600"

/// The first scalar of the root: the value of the first entry of a mapping or the first item of a sequence.
let private firstNode (root: YamlNode) : YamlNode =
    match root.Kind with
    | YamlNodeKind.Mapping collection -> collection.Items.Head.Value
    | YamlNodeKind.Sequence collection -> collection.Items.Head.Node
    | _ -> root

/// The root with another first scalar.
let private withFirstNode (replacement: YamlNode) (root: YamlNode) : YamlNode =
    match root.Kind with
    | YamlNodeKind.Mapping collection -> { root with Kind = YamlNodeKind.Mapping { collection with Items = { collection.Items.Head with Value = replacement } :: collection.Items.Tail } }
    | YamlNodeKind.Sequence collection -> { root with Kind = YamlNodeKind.Sequence { collection with Items = { collection.Items.Head with Node = replacement } :: collection.Items.Tail } }
    | _ -> replacement

let private nel = string (char 0x85)
let private lineSeparator = string (char 0x2028)
let private paragraphSeparator = string (char 0x2029)

let private wrapped =
    "doc: >\n  Reads the sample table and the peak list from the input folder, then\n  matches every peak against the reference list and writes one summary table\n"
    + "  with the matches for each sample group.\n"

let private wrappedValue =
    "Reads the sample table and the peak list from the input folder, then matches every peak against the reference list and writes one summary table with the matches for each sample group.\n"

let private wrappedTests = testList "A wrapped description" [
    // yaml@2.6.1 reads the text as {"doc":"Reads the sample table and the peak list from the input folder, then matches every peak against the reference list and writes one summary table with the matches for each sample group.\n"}.
    // The two line breaks of the folded block scalar are the two boundaries, and the writer gives the three lines back.
    testCase "the writer gives a folded block scalar its three lines" <| fun _ ->
        let spaceBefore (word: string) = wrappedValue.IndexOf(" " + word)
        let expected = mapOf [ "doc", withBreaks [ spaceBefore "matches", LayoutKind.Fold 0; spaceBefore "with", LayoutKind.Fold 0 ] (fold wrappedValue) ]
        Expect.nodeEqual (parseRoot wrapped) expected "The layout names the two spaces"
        expectKept wrapped

    // The indentation of the continuation lines comes from the writer, and the line breaks of the input have no effect on the layout.
    testCase "the lines come back with a wider indentation and from a text with CRLF" <| fun _ ->
        let wide = { defaults with Indent = 4 }
        expectLinesWith wide wrapped (wrapped.Replace("\n  ", "\n    "))
        Expect.nodeEqual (parseRoot (wrapped.Replace("\n", "\r\n"))) (parseRoot wrapped) "The tree of the text with CRLF"
        Expect.equal (Writer.writeDocuments defaults (parseOk (wrapped.Replace("\n", "\r\n")))) wrapped "The text with CRLF gives the lines with LF"
        let crlf = { defaults with NewLine = "\r\n" }
        Expect.equal (Writer.writeDocuments crlf (parseOk wrapped)) (wrapped.Replace("\n", "\r\n")) "The line break of the options"
]

let private plainTests = testList "Plain scalars" [
    // yaml@2.6.1 reads the texts as {"a":"one two three four"}, {"a":"one two"}, ["one two","three"] and {"a":"one two"} in turn. The lines of the author stay, and the comment
    // behind the last line belongs to the scalar.
    testCase "a plain scalar in a block position goes back to its lines" <| fun _ ->
        expectKept "a: one\n  two three\n  four\n"
        expectKept "a: one\n  two # c\n"
        expectKept "- one\n  two\n- three\n"
        expectKept "a:\n  one\n  two\n"
        Expect.equal (Writer.writeDocuments { defaults with Indent = 4 } (parseOk "a: one\n  two\n")) "a: one\n    two\n" "A wider indentation"

    // yaml@2.6.1 reads the texts as {"a":"x#y z:w [p] {q},r"}, {"k":"one --- two ... three"} and {"k":"one - two ? three"}. A continuation line can start with an indicator or with text that looks like a document marker, because
    // it stands behind the indentation of the block.
    testCase "continuation lines may hold a hash, a colon, flow indicators and text that looks like a document marker" <| fun _ ->
        expectKept "a: x#y\n  z:w\n  [p]\n  {q},r\n"
        expectKept "k: one\n  --- two\n  ... three\n"
        expectKept "k: one\n  - two\n  ? three\n"

    // yaml@2.6.1 reads both texts as "one two". The continuation line of a root scalar stands one indentation step in.
    testCase "a root plain scalar goes back to its lines" <| fun _ ->
        expectLines "one\ntwo\n" "one\n  two\n"
        expectKept "--- one\n  two\n"
        expectLines "- 1\n  2\n" "- 1\n  2\n"

    // yaml@2.6.1 reads the texts as ["a b","c"], ["a b"], ["a - b"] and {"k":{"a":"b c","d":["e f"]}}. A flow collection with a scalar over several lines puts each item on a line of its own, and the
    // comment behind the last line stays behind it.
    testCase "a plain scalar in a flow collection goes back to its lines" <| fun _ ->
        expectLines "[a\n b, c]\n" "[\n  a\n  b,\n  c\n]\n"
        expectLines "[ a\n  b # c\n]\n" "[\n  a\n  b # c\n]\n"
        expectLines "[a\n - b]\n" "[\n  a\n  - b\n]\n"
        expectLines "k: {a: b\n  c, d: [e\n  f]}\n" "k: {\n  a: b\n  c,\n  d: [\n    e\n    f\n  ]\n  }\n"

    // yaml@2.6.1 reads the text as {"one two":"v"}. A flow mapping with braces may have a key over several lines, and the key goes back to its lines.
    testCase "a key of a braced flow mapping goes back to its lines" <| fun _ ->
        expectLines "{ one\n  two: v }\n" "{\n  one\n  two: v\n}\n"
]

let private singleTests = testList "Single-quoted scalars" [
    // yaml@2.6.1 reads the texts as {"a":"one two  three\nfour"}, {"a":"it's fine"} and {"a":"one two"}. A line break folds into one space, and an empty line is a line feed of the value.
    testCase "a single-quoted scalar goes back to its lines" <| fun _ ->
        expectKept "a: 'one\n  two  three\n\n  four'\n"
        expectKept "a: 'it''s\n  fine'\n"
        expectLines "a: 'one\n    two'\n" "a: 'one\n  two'\n"

    // yaml@2.6.1 reads the texts as {"a":" x"}, {"a":"x "} and {"a":" "}. A quote can stand alone on a line, and the break next to it folds into a space of the value.
    testCase "a line break next to the opening or the closing quote is a boundary" <| fun _ ->
        expectKept "a: '\n  x'\n"
        expectKept "a: 'x\n  '\n"
        expectKept "a: '\n  '\n"
        Expect.equal (firstValue "a: '\n  x'\n").Value " x" "The value of the first"
        Expect.equal (firstValue "a: 'x\n  '\n").Value "x " "The value of the second"
]

let private doubleTests = testList "Double-quoted scalars" [
    // yaml@2.6.1 reads the first text as {"a":"one twothree four   five"} and the others as {"a":"x y"}, {"a":"xy"} and {"a":"xy"}. The empty line behind the escaped break is a line feed in this library ([112]) and a space in
    // yaml@2.6.1. A fold, an escaped break, an empty line behind an escaped break and consecutive escaped breaks are boundaries of different kinds.
    testCase "a double-quoted scalar goes back to its folds and escaped breaks" <| fun _ ->
        expectKept "a: \"one\n  two\\\n  three\\\n\n  four\n  \\  five\"\n"
        expectKept "a: \"x\n  y\"\n"
        expectKept "a: \"x\\\n  y\"\n"
        expectKept "a: \"x\\\n  \\\n  y\"\n"
        let scalar = firstValue "a: \"one\n  two\\\n  three\\\n\n  four\n  \\  five\"\n"
        Expect.equal scalar.Value "one twothree\nfour   five" "The value"
        let expected =
            [ 3, LayoutKind.Fold 0; 7, LayoutKind.EscapedBreak 0; 12, LayoutKind.EscapedBreak 1; 17, LayoutKind.Fold 0 ]
            |> List.map (fun (at, kind) -> { At = at; Kind = kind })
        Expect.equal scalar.Layout (Some { Boundaries = expected }) "The boundaries"

    // yaml@2.6.1 reads the first two texts as {"a":"x\ny"} and the next two as {"a":"x\n\ny"} and {"a":"x y"}. A physical empty line and the escape \n give the same value and two layouts.
    testCase "an empty line and the escape of a line feed are two layouts" <| fun _ ->
        let physical = "a: \"x\n\n  y\"\n"
        let escaped = "a: \"x\\ny\"\n"
        Expect.equal (firstValue physical).Value (firstValue escaped).Value "The same value"
        Expect.isTrue ((firstValue physical).Layout.IsSome) "The empty line is a boundary"
        Expect.isTrue ((firstValue escaped).Layout.IsNone) "The escape is no boundary"
        Expect.isFalse (YamlNode.equalIgnoringSpans (parseRoot physical) (parseRoot escaped)) "The trees differ"
        expectKept physical
        expectKept escaped
        expectKept "a: \"x\n\n\n  y\"\n"
        expectKept "a: \"x\\\n\n  y\"\n"

    // yaml@2.6.1 reads the texts as {"a":"x  y"}, {"a":"x\t y"}, {"a":"x  y"}, {"a":"x y"}, {"a":"x y"} and {"a":"x y"}. A blank next to a fold would go away when the reader folds the line, so the writer
    // escapes it. A blank in front of an escaped break stays, and a blank behind it goes away, so the writer escapes it.
    testCase "the writer escapes a blank next to a boundary of a double-quoted scalar where the reader would drop it" <| fun _ ->
        expectKept "a: \"x\\ \n  y\"\n"
        expectKept "a: \"x\\t\n  y\"\n"
        expectKept "a: \"x\n  \\ y\"\n"
        expectKept "a: \"x \\\n  y\"\n"
        expectKept "a: \"x\\\n  \\ y\"\n"
        expectLines "a: \"x   \n   y\"\n" "a: \"x\n  y\"\n"

    // yaml@2.6.1 reads the texts as {"a":"x\" y\\ z"}, {"a":" x"}, {"a":"x "}, {"a":" "}, {"a":"x"} and {"a":"x"}. An escaped quote or backslash next to a break is text of the value, and a break next to the quotes
    // makes the first or the last piece empty.
    testCase "escapes next to a boundary and empty pieces at the quotes" <| fun _ ->
        expectKept "a: \"x\\\"\n  y\\\\\n  z\"\n"
        expectKept "a: \"\n  x\"\n"
        expectKept "a: \"x\n  \"\n"
        expectKept "a: \"\n  \"\n"
        expectKept "a: \"\\\n  x\"\n"
        expectKept "a: \"x\\\n  \"\n"

    // yaml@2.6.1 reads the texts as ["a b","c d"] and ["a b","c"]. A double-quoted scalar in a flow collection goes back to its lines, with one item on each line.
    testCase "a double-quoted scalar in a flow collection or a sequence goes back to its lines" <| fun _ ->
        expectKept "- \"a\n  b\"\n- 'c\n  d'\n"
        expectLines "[\"a\n b\", c]\n" "[\n  \"a\n  b\",\n  c\n]\n"
]

let private foldedTests = testList "Folded block scalars" [
    // yaml@2.6.1 reads the texts as {"a":"one two\nthree four\n"}, {"a":"one two\n more\n more2\nthree\n"}, {"a":"one two\n  \nthree\n"}, {"a":"one\n\ttwo\nthree\n"} and {"a":"one\n\ntwo three\n"}. A fold joins two lines that start with text.
    // An empty line is a line feed of the value, and no fold happens next to a line that starts with a blank.
    testCase "a folded block scalar goes back to its lines with paragraphs, more indented lines and lines of spaces" <| fun _ ->
        expectKept "a: >\n  one\n  two\n\n  three\n  four\n"
        expectKept "a: >\n  one\n  two\n   more\n   more2\n  three\n"
        expectKept "a: >\n  one\n  two\n    \n  three\n"
        expectKept "a: >\n  one\n  \ttwo\n  three\n"
        expectKept "a: >\n  one\n\n\n  two\n  three\n"
        Expect.equal (firstValue "a: >\n  one\n  two\n   more\n   more2\n  three\n").Value "one two\n more\n more2\nthree\n" "The value"
        Expect.equal (firstValue "a: >\n  one\n  two\n    \n  three\n").Value "one two\n  \nthree\n" "A line of spaces is content"

    // yaml@2.6.1 reads the text as {"a":"one  two\n"}. The blanks at the end of a line are content of a block scalar, and the fold adds one space.
    testCase "blanks at the end of a line of a folded block scalar are content" <| fun _ ->
        expectKept "a: >\n  one \n  two\n"
        Expect.equal (firstValue "a: >\n  one \n  two\n").Value "one  two\n" "The value"

    // yaml@2.6.1 reads the texts as {"a":" lead\ntwo\n"}, {"a":"one two\n\n","b":1}, {"a":"one two"}, {"a":"one two\n"} for the text without a final line feed, {"a":"one two\n"} and the roots "one two\n". The header, the chomping, the comment behind the header and the end
    // of the text decide the rest of the text, and the folds only split the lines.
    testCase "a folded block scalar keeps its header, its chomping and its comment" <| fun _ ->
        expectKept "a: >2\n   lead\n  two\n"
        expectKept "a: >+\n  one\n  two\n\nb: 1\n"
        expectLines "a: >-\n  one\n  two" "a: >-\n  one\n  two\n"
        expectKept "a: >\n  one\n  two"
        expectKept "a: > # c\n  one\n  two\n"
        expectKept "a: >\n  one\n  two\nb: 1\n"
        expectKept ">\n  one\n  two\n"
        expectLines ">\n one\n two\n" ">\n  one\n  two\n"

    // yaml@2.6.1 reads the texts as {"a":"one\ntwo\n"}, {"a":"one\ntwo"}, {"a":"one\ntwo\n"} and {"a":"one\n two\n"}. A literal block scalar keeps its lines in the value and has no layout, and neither has a fold into a line feed.
    testCase "a literal block scalar has no layout" <| fun _ ->
        Expect.isTrue ((firstValue "a: |\n  one\n  two\n").Layout.IsNone) "A literal block scalar"
        Expect.isTrue ((firstValue "a: |-\n  one\n  two").Layout.IsNone) "A stripped literal block scalar"
        Expect.isTrue ((firstValue "a: >\n  one\n\n  two\n").Layout.IsNone) "A fold into a line feed is no boundary"
        Expect.isTrue ((firstValue "a: >\n  one\n   two\n").Layout.IsNone) "A more indented line is no boundary"
        expectKept "a: |\n  one\n  two\n"
]

let private keyTests = testList "Keys" [
    // yaml@2.6.1 reads the text as {"a b":"v"}. An implicit key stands on one line, so the writer ignores the layout of a key that a program built.
    testCase "an implicit key stays on one line" <| fun _ ->
        let key = withBreaks [ 1, LayoutKind.Fold 0 ] (sc "a b")
        Expect.equal (Writer.writeNode defaults (YamlNode.mapping [ YamlNode.entry key (sc "v") ])) "a b: v\n" "The key"
        Expect.equal (Writer.writeNode defaults (YamlNode.flowMapping [ YamlNode.entry key (sc "v") ])) "{\n  a\n  b: v\n}\n" "A braced flow mapping has no limit"

    // yaml@2.6.1 reads the explicit key of 1025 characters as one key. A key of 1023 or 1024 characters is implicit, and a longer one needs the question mark, where the lines of the layout
    // are valid.
    testCase "a key near the limit of 1024 characters keeps one line while it is implicit" <| fun _ ->
        let keyOf (length: int) = String.replicate 500 "x" + " " + String.replicate (length - 501) "y"
        for length in [ 1023; 1024 ] do
            let text = keyOf length
            let node = withBreaks [ 500, LayoutKind.Fold 0 ] (sc text)
            let written = Writer.writeNode defaults (YamlNode.mapping [ YamlNode.entry node (sc "v") ])
            Expect.equal written (text + ": v\n") (sprintf "A key of %d characters" length)
        let node = withBreaks [ 500, LayoutKind.Fold 0 ] (sc (keyOf 1025))
        let written = Writer.writeNode defaults (YamlNode.mapping [ YamlNode.entry node (sc "v") ])
        Expect.equal written ("? " + String.replicate 500 "x" + "\n  " + String.replicate 524 "y" + "\n: v\n") "A key of 1025 characters is explicit and goes back to its lines"
]

let private policyTests = testList "Policies" [
    // yaml@2.6.1 reads the texts as {"a":"one two"}, {"a":"one two"}, {"a":"xy"} and the description of the first tests. Canonical ignores the layout and writes the normal spelling.
    testCase "Canonical drops the layout and writes the same text again" <| fun _ ->
        let canonical = withStyles Canonical
        for (input, expected) in
            [ "a: one\n  two\n", "a: one two\n"
              "a: 'one\n  two'\n", "a: one two\n"
              "a: \"x\\\n  y\"\n", "a: xy\n"
              wrapped, "doc: \"" + wrappedValue.Replace("\n", "\\n") + "\"\n" ] do
            let once = Writer.writeDocuments canonical (parseOk input)
            Expect.equal once expected (sprintf "The Canonical text of %A" input)
            Expect.equal (Writer.writeDocuments canonical (parseOk once)) once (sprintf "The second Canonical write of %A" input)

    // The layout fits the value at the offsets of its boundaries. A record update of the value can break the fit, and then the writer writes the normal spelling. RequireStored checks the style
    // only, so it writes the normal spelling too.
    testCase "a layout that does not fit the value gives the normal spelling and no error" <| fun _ ->
        let node = parseRoot "a: one\n  two\n"
        let stale = mapOf [ "a", updateScalar (fun scalar -> { scalar with Value = "onetwo" }) (YamlNode.entries node).Head.Value ]
        for policy in [ PreferStored; RequireStored ] do
            Expect.equal (Writer.writeNode (withStyles policy) stale) "a: onetwo\n" (sprintf "The text under %A" policy)
            Expect.equal (Writer.writeNode (withStyles policy) node) "a: one\n  two\n" (sprintf "The text of the fitting layout under %A" policy)

    // A layout names the line breaks of a family of styles. A record update that changes the style leaves the layout in place, and the writer replays it where it fits the new style.
    testCase "a layout follows a style that can hold its boundaries" <| fun _ ->
        let plain = (YamlNode.entries (parseRoot "a: one\n  two\n")).Head.Value
        let restyled (style: ScalarStyle) = mapOf [ "a", updateScalar (fun scalar -> { scalar with Style = style; Value = scalar.Value + "\n" }) plain ]
        Expect.equal (Writer.writeNode defaults (restyled ScalarStyle.DoubleQuoted)) "a: \"one\n  two\\n\"\n" "The double-quoted style"
        Expect.equal (Writer.writeNode defaults (restyled (ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Clip, None)))) "a: |\n  one two\n" "A literal block scalar has no layout"
        Expect.equal (Writer.writeNode defaults (restyled (ScalarStyle.Block(BlockScalarStyle.Folded, ChompingMode.Clip, None)))) "a: >\n  one\n  two\n" "A folded block scalar replays the fold"
]

let private consumerTests = testList "Changing the text of a scalar" [
    // The same text gives the node back. A text that leaves each boundary at its space keeps the lines, and a text that moves a boundary drops them.
    testCase "withScalarValue keeps the layout while it fits" <| fun _ ->
        let node = (YamlNode.entries (parseRoot "a: one two\n  three\n")).Head.Value
        let same = YamlNode.withScalarValue "one two three" node
        Expect.nodeEqual same node "The same text gives the node back"
        Expect.equal same.Span node.Span "The span stays"
        let edited = YamlNode.withScalarValue "uno dos three" node
        Expect.equal (scalarOf edited).Value "uno dos three" "The new text"
        Expect.equal (scalarOf edited).Layout (scalarOf node).Layout "The same offsets"
        Expect.equal (Writer.writeNode defaults (mapOf [ "a", edited ])) "a: uno dos\n  three\n" "The lines of the author"
        let moved = YamlNode.withScalarValue "one two  three" node
        Expect.equal (scalarOf moved).Layout None "A moved boundary drops the layout"
        Expect.equal (Writer.writeNode defaults (mapOf [ "a", moved ])) "a: one two  three\n" "The normal spelling"
        let shorter = YamlNode.withScalarValue "one" node
        Expect.equal (scalarOf shorter).Layout None "A text that ends in front of the boundary drops the layout"

    // The comments and the properties stay with the style, and the span goes. A node that is no scalar is an error.
    testCase "withScalarValue keeps the properties, the comments and the style" <| fun _ ->
        let node = YamlNode.withAnchor "x" (YamlNode.withTag (YamlTag.core "str") (withNote " c" (dq "a b")))
        let edited = YamlNode.withScalarValue "c d" node
        Expect.equal (scalarOf edited).Style ScalarStyle.DoubleQuoted "The style"
        Expect.equal edited.Properties node.Properties "The properties"
        Expect.equal edited.TrailingComment node.TrailingComment "The comment"
        let block = YamlNode.withHeaderComment (YamlComment.create " h") (fold "a b\n")
        Expect.equal (scalarOf (YamlNode.withScalarValue "c d\n" block)).HeaderComment (scalarOf block).HeaderComment "The header comment"
        Expect.throws (fun () -> YamlNode.withScalarValue "x" (seqOf [ sc "a" ]) |> ignore) "A sequence"

    // yaml@2.6.1 reads {"k":"abcd"} for the first text. The edit puts a character above U+FFFF between the two halves of the old value, and the boundary of the escaped break then stands at the index 2, which is
    // between the two halves of its surrogate pair on .NET and JavaScript and behind the character on Python. A layout whose boundary splits a character does not fit, because the writer would cut the pair in two.
    testCase "withScalarValue drops a layout whose boundary would split a surrogate pair" <| fun _ ->
        let node = (YamlNode.entries (parseRoot "k: \"ab\\\n  cd\"\n")).Head.Value
        Expect.equal (scalarOf node).Value "abcd" "The value of the node"
        let edited = YamlNode.withScalarValue ("a" + astral + "d") node
        Expect.equal (scalarOf edited).Layout.IsSome (astral.Length = 1) "The layout stays only where the index 2 is behind the character"
        let written = Writer.writeNode defaults (mapOf [ "k", edited ])
        Expect.equal (firstValue written).Value ("a" + astral + "d") "The text reads as the edited value"
        Expect.equal (Writer.writeNode defaults (parseRoot written)) written "A second write changes nothing"

    // yaml@2.6.1 reads 80 as an int and true as a bool, and Encode.string double-quotes a string with that text. An edit that makes a plain string read as null, a bool, an int or a float takes the style of Encode.string.
    // A plain scalar that was typed before the edit stays plain, so a number can become another number.
    testCase "withScalarValue quotes a plain string that now reads as another type" <| fun _ ->
        let node = sc "name"
        for text in [ "true"; "null"; "12"; "1.5"; "~" ] do
            let edited = YamlNode.withScalarValue text node
            Expect.equal (scalarOf edited).Style (scalarOf (Encode.string text)).Style (sprintf "The style for %s" text)
        Expect.equal (scalarOf (YamlNode.withScalarValue "other" node)).Style ScalarStyle.Plain "A plain text stays plain"
        let tagged = YamlNode.withTag (YamlTag.core "int") (sc "1")
        Expect.equal (scalarOf (YamlNode.withScalarValue "2" tagged)).Style ScalarStyle.Plain "A tagged node keeps its style"
        let quoted = YamlNode.withScalarValue "true" (YamlNode.entries (parseRoot "a: one\n  two\n")).Head.Value
        Expect.equal (scalarOf quoted).Layout None "The layout goes with the style"
        for (before, after) in [ "80", "81"; "true", "false"; "1.5", "2.5"; "null", "~"; "80", "text"; "true", "12"; "", "80" ] do
            let edited = YamlNode.withScalarValue after (sc before)
            Expect.equal (scalarOf edited).Style ScalarStyle.Plain (sprintf "A typed plain scalar %A stays plain when it becomes %A" before after)
            Expect.equal (scalarOf edited).Value after (sprintf "The new text of %A" before)
        Expect.equal (Writer.writeNode defaults (mapOf [ "port", YamlNode.withScalarValue "81" (sc "80") ])) "port: 81\n" "The number stays a number"

    // yaml@2.6.1 reads [", y", z] as the strings ", y" and "z". The scalar keeps a layout that fits its value and style, but a plain scalar cannot start with a comma, so the writer quotes it on one line.
    // The flow collection stays on one line then, because the probe and the writer ask the same question.
    testCase "a flow collection stays on one line when the writer quotes its scalar with a layout" <| fun _ ->
        let root = parseRoot "[x\n y, z]\n"
        let edited = YamlNode.withScalarValue ", y" (firstNode root)
        Expect.isTrue (scalarOf edited).Layout.IsSome "The layout fits the value and the style"
        let written = Writer.writeNode defaults (withFirstNode edited root)
        Expect.equal written "[\", y\", z]\n" "One line"
        Expect.equal (Writer.writeNode defaults (parseRoot written)) written "A second write changes nothing"
]

let private textTests = testList "Characters next to the boundaries" [
    // yaml@2.6.1 reads each text as the value with the characters of the lines and one space for each break. A character above U+FFFF and a combining mark are part of a line, and the position of a boundary is the index of the space in the value on the
    // target. A surrogate pair takes two indexes on .NET and JavaScript, and a Python string takes one.
    testCase "astral and combining characters in front of a boundary" <| fun _ ->
        for input in [ "a: x\U0001F600\n  y\n"; "a: \U0001F600 \U0001F600\n  y\n"; "a: e\u0301\n  b\n"; "a: 'x\U0001F600\n  \u0301y'\n"; "a: \"\U0001F600\\\n  \U0001F600\"\n" ] do
            let scalar = firstValue input
            for boundary in scalar.Layout.Value.Boundaries do
                match boundary.Kind with
                | LayoutKind.Fold 0 -> Expect.equal scalar.Value.[boundary.At] ' ' (sprintf "The fold in %A is a space" input)
                | _ -> ()
            expectKept input
        let value = (firstValue "a: x\U0001F600\n  y\n").Value
        Expect.equal (firstValue "a: x\U0001F600\n  y\n").Layout (Some { Boundaries = [ { At = value.IndexOf ' '; Kind = LayoutKind.Fold 0 } ] }) "The position of the space"

    // yaml@2.6.1 reads the text as {"a":"x\u0085y z\u2028w\u2029v"}. NEL, LS and PS are characters of a line in YAML 1.2, and only the line feed and the carriage return end a line.
    testCase "NEL and the line and paragraph separators are text inside a line" <| fun _ ->
        let input = "a: x" + nel + "y\n  z" + lineSeparator + "w" + paragraphSeparator + "v\n"
        Expect.equal (firstValue input).Value ("x" + nel + "y z" + lineSeparator + "w" + paragraphSeparator + "v") "The value"
        expectKept input
        expectKept ("a: >\n  x" + nel + "y\n  z" + lineSeparator + "w\n")
        expectLines ("a: \"x" + nel + "\n  y\"\n") "a: \"x\\N\n  y\"\n"
]

let private sizeTests = testList "Large scalars" [
    // The reader and the writer take time linear in the number of lines. The writer keeps the layout of a scalar with 10000 lines, and a scalar with 10000 boundaries.
    testCase "a scalar over 10000 lines goes back to its lines" <| fun _ ->
        let lines = [ for i in 1 .. 10000 -> "w" + string i ]
        let folded = "a: >\n" + String.concat "" [ for line in lines -> "  " + line + "\n" ]
        let plain = "a: " + String.concat "\n  " lines + "\n"
        let single = "a: '" + String.concat "\n  " lines + "'\n"
        let double = "a: \"" + String.concat "\\\n  " lines + "\"\n"
        for input in [ folded; plain; single; double ] do
            let scalar = firstValue input
            Expect.equal scalar.Layout.Value.Boundaries.Length 9999 "The boundaries"
            expectKept input

    testCase "writing a scalar with many boundaries takes time linear in the number of lines" <| fun _ ->
        let countLineBreaks (text: string) = text |> Seq.filter (fun c -> c = '\n') |> Seq.length |> ignore
        let writeFolded (text: string) = Writer.writeDocuments defaults (parseOk text) |> ignore
        expectLinear "write of folded scalars" writeFolded countLineBreaks (fun lines -> "a: >\n" + String.replicate lines "  word\n")
        expectLinear "write of plain scalars" writeFolded countLineBreaks (fun lines -> "a: w\n" + String.replicate lines "  word\n")
        expectLinear "write of single-quoted scalars" writeFolded countLineBreaks (fun lines -> "a: 'w\n" + String.replicate lines "  word\n" + "  end'\n")
        expectLinear "write of double-quoted scalars" writeFolded countLineBreaks (fun lines -> "a: \"w\\\n" + String.replicate lines "  word\\\n" + "  end\"\n")
]

let private editTests = testList "Edits through withScalarValue" [
    // The test edits the first scalar of each text a hundred and twenty times with a pseudo-random edit, writes the tree and reads it, then writes it again. The generator is a small linear
    // congruential generator, so every target runs the same edits. A character above U+FFFF goes in at an index that keeps a surrogate pair together.
    testCase "an edited scalar writes and reads back with its value and a layout that stays" <| fun _ ->
        let sources =
            [ "plain in a block", "k: one two\n  three four\n  five six\n"
              "plain in a flow sequence", "[one two\n three four\n five six, z]\n"
              "single in a block", "k: 'one two\n  three  four\n\n  five six'\n"
              "single in a flow sequence", "['one two\n three four\n five six', z]\n"
              "double in a block", "k: \"one two\n  three\\\n  four\n\n  five six\"\n"
              "double in a flow sequence", "[\"one two\n three\\\n four\n five six\", z]\n"
              "folded in a block", "k: >\n  one two\n  three four\n\n  five six\n" ]
        let mutable state = 7
        let next (bound: int) =
            state <- (state * 75 + 74) % 65537
            state % bound
        let pieces = [| "x"; " "; "\n"; "\t"; "#"; ":"; "\""; "'"; "\\"; "-"; astral; "e"; "  "; ", " |]
        let isHalf (c: char) = int c >= 0xD800 && int c <= 0xDFFF
        let edit (value: string) : string =
            let piece = pieces.[next pieces.Length]
            let at = next (value.Length + 1)
            let splits = at > 0 && at < value.Length && isHalf value.[at - 1] && isHalf value.[at]
            match next 3 with
            | 0 -> if splits then value + piece else value.Substring(0, at) + piece + value.Substring(at)
            | 1 -> if at < value.Length && not (isHalf value.[at]) then value.Substring(0, at) + piece + value.Substring(at + 1) else value + piece
            | _ -> if at < value.Length && not (isHalf value.[at]) then value.Substring(0, at) + value.Substring(at + 1) else value
        for (label, source) in sources do
            let root = parseRoot source
            let node = firstNode root
            for round in 1 .. 120 do
                let value = edit (scalarOf node).Value
                let edited = YamlNode.withScalarValue value node
                let text = Writer.writeNode defaults (withFirstNode edited root)
                let back = parseRoot text
                let scalar = scalarOf (firstNode back)
                Expect.equal scalar.Value value (sprintf "%s, round %d: the value read from %A" label round text)
                if scalar.Layout.IsSome then
                    Expect.equal scalar.Layout (scalarOf edited).Layout (sprintf "%s, round %d: the layout read from %A" label round text)
                Expect.equal (Writer.writeNode defaults back) text (sprintf "%s, round %d: the second write of %A" label round text)
]

let private fitsTests = testList "The check of a layout" [
    // The check looks at the value and the style only, and it never moves a boundary. A fold replaces one space, and a fold with empty lines or an escaped break replaces line feeds. The pieces
    // between the boundaries have to keep their text: the lines of a plain scalar neither start nor end with a blank next to a fold, and the lines of a folded block scalar start with text. Single quotes
    // may have an empty first or last piece. Only a double-quoted scalar has folds with empty lines and escaped breaks, and only an escaped break may stand behind an empty piece. A literal block scalar
    // has no layout.
    testCase "a layout fits where the boundaries replace the text they name and the pieces keep text" <| fun _ ->
        let fold (at: int) = at, LayoutKind.Fold 0
        let plain = ScalarStyle.Plain
        let single = ScalarStyle.SingleQuoted
        let double = ScalarStyle.DoubleQuoted
        let folded = ScalarStyle.Block(BlockScalarStyle.Folded, ChompingMode.Clip, None)
        let cases =
            [ "plain, a space between text", plain, "a b", [ fold 1 ], true
              "plain, the boundary is no space", plain, "ab", [ fold 1 ], false
              "plain, past the end of the value", plain, "a b", [ fold 3 ], false
              "plain, a blank at the end of the line in front", plain, "a  b", [ fold 2 ], false
              "plain, a blank at the start of the line behind", plain, "a  b", [ fold 1 ], false
              "plain, a line feed in front of the fold", plain, "a" + "\n" + " b", [ fold 2 ], false
              "plain, two folds in order", plain, "a b c", [ fold 1; fold 3 ], true
              "plain, two folds out of order", plain, "a b c", [ fold 3; fold 1 ], false
              "plain, a fold with empty lines", plain, "a" + "\n" + "b", [ 1, LayoutKind.Fold 1 ], false
              "plain, an escaped break", plain, "ab", [ 1, LayoutKind.EscapedBreak 0 ], false
              "single, a space at the opening quote", single, " x", [ fold 0 ], true
              "single, a space at the closing quote", single, "x ", [ fold 1 ], true
              "single, a space alone", single, " ", [ fold 0 ], true
              "single, an empty piece between two folds", single, "x  y", [ fold 1; fold 2 ], false
              "single, a blank at the end of the line in front", single, "x  y", [ fold 2 ], false
              "double, a fold with empty lines", double, "x" + "\n" + "y", [ 1, LayoutKind.Fold 1 ], true
              "double, empty lines that the value lacks", double, "x y", [ 1, LayoutKind.Fold 1 ], false
              "double, an escaped break that replaces nothing", double, "xy", [ 1, LayoutKind.EscapedBreak 0 ], true
              "double, two escaped breaks in one place", double, "xy", [ 1, LayoutKind.EscapedBreak 0; 1, LayoutKind.EscapedBreak 0 ], true
              "double, an escaped break with an empty line", double, "x" + "\n" + "y", [ 1, LayoutKind.EscapedBreak 1 ], true
              "double, a negative count", double, "xy", [ 1, LayoutKind.EscapedBreak (-1) ], false
              "double, an empty piece between two folds", double, "x  y", [ fold 1; fold 2 ], false
              "double, an empty piece between an escaped break and a fold", double, "x y", [ 1, LayoutKind.EscapedBreak 0; fold 1 ], false
              "double, an empty piece between a fold and an escaped break", double, "x y", [ fold 1; 2, LayoutKind.EscapedBreak 0 ], true
              "double, blanks next to a fold", double, "x  y", [ fold 1 ], true
              "double, an escaped break in front of a character above U+FFFF", double, "a" + astral + "d", [ 1, LayoutKind.EscapedBreak 0 ], true
              "double, an escaped break behind a character above U+FFFF", double, "a" + astral + "d", [ 1 + astral.Length, LayoutKind.EscapedBreak 0 ], true
              "double, an escaped break at the index 2, between the halves of a pair on .NET and JavaScript", double, "a" + astral + "d", [ 2, LayoutKind.EscapedBreak 0 ], (astral.Length = 1)
              "double, a fold with empty lines in front of a character above U+FFFF", double, "a" + "\n" + astral, [ 1, LayoutKind.Fold 1 ], true
              "folded, a space between lines of text", folded, "one two" + "\n", [ fold 3 ], true
              "folded, the line starts with a blank", folded, " a b" + "\n", [ fold 2 ], false
              "folded, the line behind starts with a blank", folded, "a  b" + "\n", [ fold 1 ], false
              "folded, blanks at the end of the line in front", folded, "a  b" + "\n", [ fold 2 ], true
              "folded, a line feed in front of the fold", folded, "a" + "\n" + " b", [ fold 2 ], false
              "literal, any layout", ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Clip, None), "a b" + "\n", [ fold 1 ], false ]
        for (label, style, value, boundaries, expected) in cases do
            let layout: ScalarLayout = { Boundaries = [ for (at, kind) in boundaries -> { At = at; Kind = kind } ] }
            Expect.equal (Layout.fits layout value style) expected label
]

let private equalityTests = testList "Equality and printing" [
    // The comparison and the printout include the layout, and the layout of a constructed scalar is none.
    testCase "equalIgnoringSpans compares the layouts and show prints them" <| fun _ ->
        let wrapped = parseRoot "a: x\n  y\n"
        let flat = parseRoot "a: x y\n"
        Expect.isFalse (YamlNode.equalIgnoringSpans wrapped flat) "A layout and none differ"
        Expect.isTrue (YamlNode.equalIgnoringSpans wrapped (parseRoot "a: x\n    y\n")) "The indentation is no part of the layout"
        Expect.isFalse (YamlNode.equalIgnoringSpans (parseRoot "a: x\n  y\n  z\n") (parseRoot "a: x y\n  z\n")) "Other boundaries differ"
        Expect.stringContains (YamlNode.show wrapped) "breaks=[f@1]" "The printout of a fold"
        Expect.stringContains (YamlNode.show (parseRoot "a: \"x\\\n\n  y\"\n")) "breaks=[e1@1]" "The printout of an escaped break with an empty line"
        Expect.stringContains (YamlNode.show (parseRoot "a: \"x\n\n  y\"\n")) "breaks=[f1@1]" "The printout of a fold with an empty line"
        Expect.isFalse ((YamlNode.show flat).Contains "breaks") "No layout, no printout"
]

let Main = testList "Layout" [ wrappedTests; plainTests; singleTests; doubleTests; foldedTests; keyTests; policyTests; consumerTests; editTests; textTests; sizeTests; fitsTests; equalityTests ]
