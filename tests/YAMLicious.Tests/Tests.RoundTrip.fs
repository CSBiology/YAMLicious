module Tests.RoundTrip

// The round trip of the writer: a text is parsed, written and parsed again. The two trees have to be equal except for the spans, and
// the second write has to give the first text again. The contract holds for the indentations 2 and 4 and for the line breaks LF and
// CRLF. It holds under PreferStored and under RequireStored, because a tree that the parser produced needs no change of style.
//
// The one exception is an empty root block scalar without an indentation indicator that has a comment line behind it, or a blank line when its
// chomping is Keep. The first line behind such a body would be content, so the writer adds the indicator 1 and RequireStored raises. The lists below leave
// that case out, and Tests.Writer tests it. The inputs for the other places hold the texts that the parser reads as a blank line behind a Keep scalar:
// a line with a tab behind a root scalar and a line with a byte order mark behind a scalar.
//
// The block inputs come from the parser tests and from a frozen corpus. The corpus inputs are
// short ones that show a feature of the model: comments in each slot, block scalars with each header, explicit keys, properties,
// aliases, directives, several documents and blank lines.
//
// The flow inputs go through the flow parser like the block inputs, one test for each input.

open Fable.Pyxpecto
open YAMLicious

let private defaults = WriterOptions.defaults

// Characters that a test needs and a source file cannot show: built at run time, so that no editor or compiler changes them.
let private bom = string (char 0xFEFF)
let private nel = string (char 0x85)
let private nbsp = string (char 0xA0)
let private lineSeparator = string (char 0x2028)
let private paragraphSeparator = string (char 0x2029)
let private del = string (char 0x7F)
let private c1Control = string (char 0x81)

let private fourSpaces: WriterOptions = { Indent = 4; NewLine = "\n"; Styles = PreferStored }

let private crlf: WriterOptions = { Indent = 2; NewLine = "\r\n"; Styles = PreferStored }

let private wideCrlf: WriterOptions = { Indent = 4; NewLine = "\r\n"; Styles = PreferStored }

let private required: WriterOptions = { Indent = 2; NewLine = "\n"; Styles = RequireStored }

/// Parses the input, writes it, parses the text again and compares the trees. The second write has to give the first text.
let private expectRoundTrip (options: WriterOptions) (input: string) =
    let stream = parseOk input
    let written = Writer.writeDocuments options stream
    let reparsed =
        match Parser.parseStream written with
        | Ok result -> result
        | Error error -> failwithf "The parser rejects the text %A that the writer gave for %A: %s" written input (describeError error)
    Expect.streamEqual reparsed stream (sprintf "The tree of %A after a round trip, written as %A" input written)
    Expect.equal (Writer.writeDocuments options reparsed) written (sprintf "The second write of %A" input)

let private blockInputs: string list = [
    "Hello World"
    "Say: Hello World"
    "Say: Hello World # 420 blaze it"
    "k:"
    "k: "
    "k: # c"
    ":a"
    ":a: b"
    "- :a"
    "-"
    "? a"
    "- - a"
    "&a : v"
    "!!str : v"
    "&a !!str : v"
    "&a :"
    "- &a : v"
    ": v"
    "k: &a:b x"
    "&a: b"
    "k: &a:"
    "&a:b : v"
    "k: !<tag:example.com,2000:b> c"
    "k: !local c"
    "k: !!str c"
    "k: ! c"
    "!!str &a x"
    "&a !!str x"
    "k: &a !!str x"
    "foo: !!str bar"
    "k: !!str &a 1"
    "k: &a !!str 1"
    "a: |"
    "- # c"
    "? # c"
    "k: v # c"
    "k: 'v' # c"
    "- v # c"
    "k: !!str"
    "value"
    "key: value"
    "--- foo"
    "---foo: bar"
    "---"
    ""
    "# c"
    "..."
    "hr:  65    # Home runs\navg: 0.278 # Batting average\nrbi: 147   # Runs Batted In\n"
    "american:\n- Boston Red Sox\n- Detroit Tigers\n- New York Yankees\nnational:\n- New York Mets\n- Chicago Cubs\n- Atlanta Braves\n"
    "-\n  name: Mark McGwire\n  hr:   65\n  avg:  0.278\n-\n  name: Sammy Sosa\n  hr:   63\n  avg:  0.288\n"
    "# Ranking of 1998 home runs\n---\n- Mark McGwire\n- Sammy Sosa\n- Ken Griffey\n\n# Team ranking\n---\n- Chicago Cubs\n- St Louis Cardinals\n"
    "---\ntime: 20:03:20\nplayer: Sammy Sosa\naction: strike (miss)\n...\n---\ntime: 20:03:47\nplayer: Sammy Sosa\naction: grand slam\n...\n"
    "---\nhr: # 1998 hr ranking\n- Mark McGwire\n- Sammy Sosa\n# 1998 rbi ranking\nrbi:\n- Sammy Sosa\n- Ken Griffey\n"
    "---\n# Products purchased\n- item    : Super Hoop\n  quantity: 1\n- item    : Basketball\n  quantity: 4\n- item    : Big Shoes\n  quantity: 1\n"
    "# ASCII Art\n--- |\n  \\//||\\/||\n  // ||  ||__\n"
    "--- >\n  Mark McGwire's\n  year was crippled\n  by a knee injury.\n"
    "--- >\n Sammy Sosa completed another\n fine season with great stats.\n\n   63 Home Runs\n   0.288 Batting Average\n\n What a year!\n"
    "name: Mark McGwire\naccomplishment: >\n  Mark set a major league\n  home run record in 1998.\nstats: |\n  65 Home Runs\n  0.278 Batting Average\n"
    "plain:\n  This unquoted scalar\n  spans many lines.\n\nquoted: \"So does this\n  quoted scalar.\\n\"\n"
    "# Sets are represented as a\n# Mapping where each key is\n# associated with a null value\n--- !!set\n? Mark McGwire\n? Sammy Sosa\n? Ken Griffey\n"
    "# Ordered maps are represented as\n# A sequence of mappings, with\n# each mapping having one key\n--- !!omap\n- Mark McGwire: 65\n- Sammy Sosa: 63\n- Ken Griffey: 58\n"
    "sequence:\n- one\n- two\nmapping:\n  ? sky\n  : blue\n  sea : green\n"
    "# Comment only.\n\n"
    "anchored: !local &anchor value\nalias: *anchor\n"
    "literal: |\n  some\n  text\nfolded: >\n  some\n  text\n"
    "single: 'text'\ndouble: \"text\"\n"
    "%YAML 1.2\n--- text\n"
    "|\n  Line break (no glyph)\n  Line break (glyphed)\n"
    "# Tabs and spaces\nquoted: \"Quoted \t\"\nblock:\t|\n  void main() {\n  \tprintf(\"Hello, world!\\n\");\n  }\n"
    "- \"Fun with \\\\\"\n- \"\\\" \\a \\b \\e \\f\"\n- \"\\n \\r \\t \\v \\0\"\n- \"\\  \\_ \\N \\L \\P \\\n  \\x41 \\u0041 \\U00000041\"\n"
    "? a\n: -\tb\n  -  -\tc\n     - d\n"
    "- foo:\t bar\n- - baz\n  -\tbaz\n"
    "plain: text\n  lines\nquoted: \"text\n  \tlines\"\nblock: |\n  text\n   \tlines\n"
    "Folding:\n  \"Empty line\n   \t\n  as a line feed\"\nChomping: |\n  Clipped empty lines\n \n"
    ">-\n  trimmed\n  \n \n\n  as\n  space\n"
    ">\n  foo \n \n  \t bar\n\n  baz\n"
    "\"\n  foo \n \n  \t bar\n\n  baz \"\n"
    "key:    # Comment\n  valueeof\n"
    "  # Comment\n   \n\n"
    "key:    # Comment\n        # lines\n  value\n\n"
    "%FOO  bar baz # Should be ignored\n               # with a warning.\n--- \"foo\"\n"
    "# Private\n!foo \"bar\"\n...\n# Global\n%TAG ! tag:example.com,2000:app/\n---\n!foo \"bar\"\n"
    "%TAG !! tag:example.com,2000:app/\n---\n!!int 1 - 3 # Interval, not integer\n"
    "%TAG !e! tag:example.com,2000:app/\n---\n!e!foo \"bar\"\n"
    "%TAG !m! !my-\n--- # Bulb here\n!m!light fluorescent\n...\n%TAG !m! !my-\n--- # Color here\n!m!light green\n"
    "%TAG !e! tag:example.com,2000:app/\n---\n- !e!foo \"bar\"\n"
    "!!str &a1 \"foo\":\n  !!str bar\n&a2 baz : *a1\n"
    "!<tag:yaml.org,2002:str> foo :\n  !<!bar> baz\n"
    "%TAG !e! tag:example.com,2000:app/\n---\n- !local foo\n- !!str bar\n- !e!tag%21 baz\n"
    "# Assuming conventional resolution:\n- \"12\"\n- 12\n- ! 12\n"
    "First occurrence: &anchor Value\nSecond occurrence: *anchor\n"
    "First occurrence: &anchor Foo\nSecond occurrence: *anchor\nOverride anchor: &anchor Bar\nReuse anchor: *anchor\n"
    "\"folded \nto a space,\t\n \nto a line feed, or \t\\\n \\ \tnon-content\"\n"
    "\" 1st non-empty\n\n 2nd non-empty \n\t3rd non-empty \"\n"
    "' 1st non-empty\n\n 2nd non-empty \n\t3rd non-empty '\n"
    "1st non-empty\n\n 2nd non-empty \n\t3rd non-empty\n"
    "- !!str \"a\"\n- 'b'\n- &anchor \"c\"\n- *anchor\n- !!str\n"
    "- | # Empty header\n literal\n- >1 # Indentation indicator\n  folded\n- |+ # Chomping indicator\n keep\n\n- >1- # Both indicators\n  strip\n"
    "- |\n detected\n- >\n \n  \n  # detected\n- |1\n  explicit\n- >\n \t\n detected\n"
    "strip: |-\n  text\nclip: |\n  text\nkeep: |+\n  text\n"
    "# Strip\n  # Comments:\nstrip: |-\n  # text\n  \u21D3\n # Clip\n  # comments:\n\nclip: |\n  # text\n \n # Keep\n  # comments:\n\nkeep: |+\n  # text\n\n # Trail\n  # comments.\n"
    "strip: >-\n\nclip: >\n\nkeep: |+\n\n"
    "|\n literal\n \ttext\n\n"
    "block sequence:\n  - one\n  - two : three\n"
    "- # Empty\n- |\n block node\n- - one # Compact\n  - two # sequence\n- one: two # Compact mapping\n"
    "block mapping:\n key: value\n"
    "? explicit key # Empty value\n? |\n  block key\n: - one # Explicit compact\n  - two # block value\n"
    "plain key: in-line value\n: # Both empty\n\"quoted key\":\n- entry\n"
    "- sun: yellow\n- ? earth: blue\n  : moon: white\n"
    "-\n  \"flow in block\"\n- >\n Block scalar\n- !!map # Block collection\n  foo : bar\n"
    "literal: |2\n  value\nfolded:\n   !foo\n  >1\n value\n"
    "%YAML 1.2\n---\nDocument\n... # Suffix\n"
    "Bare\ndocument\n...\n# No document\n...\n|\n%!PS-Adobe-2.0 # Not the first line\n"
    "%YAML 1.2\n--- |\n%!PS-Adobe-2.0\n...\n%YAML 1.2\n---\n# Empty\n...\n"
    "Document\n---\n# Empty\n...\n%YAML 1.2\n---\nmatches %: 20\n"
    "a:\n  - k:\n      j: 2\n"
    "a:\n  - k:\n    j: 2\n"
    "- k:\n    j: 2\n"
    "a:\n  - k:\n      j: 2\n    m: 3\n"
    "# lead\n%YAML 1.2\n---\na: 1\n"
    "%YAML 1.2\n# lead\n---\na: 1\n"
    "%YAML 1.2\n---\na: 1\n"
    "---\na: 1\n"
    "---\n# c\na: 1\n"
    "%YAML 1.2\n# lead\n# lead2\n---\na: 1\n"
    "? a\n# c\n: b\n"
    "? a\n\n: b\n"
    "? a\n  # c\n: b\n"
    "? a\n: b\n"
    "? a\n# c\n: b\nd: e\n"
    "m:\n  ? a\n  # c\n  : b\n"
    "s: &a x\nl:\n- *a\n- b"
    "s: &a x\nk: *a\nj: b"
    "s: &a x\nk:\n  *a\nj: b"
    "s: &a x\nl:\n  - *a\n  - b"
    "s: &a x\nk: *a # c\n  # d\nj: 1"
    "s: &a x\nk: *a # c\nj: 1"
    "? a\n: b\n? c\n: d"
    "k:\n  &a\n  v"
    "- - - a\n    - b\n  - c\n- d"
    "? a\n:\n  - x\n  - y"
    "\"a\": b\n? \"c\"\n: d"
    "? a\n:\n- x\n- y"
    "? a\n: b"
    "? a\n: b: c"
    "s: &a x\n*a : v"
    "l:\n- &a x\n- *a : v"
    "s: &a x\n? *a\n: v"
    "s: &a x\n?\n  *a\n: v"
    "x: &a 1\nk:\n  - *a : v"
    "a: |2\n    x \"1\"\n  y\nb: 'z'\n"
    "a: |+\n  x\n\n\nb: \"q\"\n"
    "a: !!str\t\"x\"\n"
    "k: |\n  \tx\n  y\nm: 'q'\n"
    "--- # c\na: 1\n"
    "--- |\n  x\n"
    "--- |\n  x \"q\"\n  y 'r'\n"
    "--- !!str |\n  x\n"
    "--- >-\n  x\n  y\n"
    "--- 'x'\n"
    "--- \"x\" # c\n"
    "---\na: 'x'\n---\nb: \"y\"\n"
    "---\n---\na: 1\n"
    "a: |+\nb: 1\n"
    "a: >+\nb: 1\n"
    "a: |+\n\n\nb: 1\n"
    "a: >+\n\n\nb: 1\n"
    "k86_0:\n    - |4-\n\n  # note\n"
    "# lead\n---\n- v\n# c\n- |1\n   indented\n\n- \"back\\\\\" # trailing\n"
    "# a: |\n-\n  - |2\n    ---x\n\n  # a: |\n  - C#\n"
    "key: don't\n\nk-2: 'it''s' #t\nx1: |2 # it's \"x\"\n  key: value\n\n\n"
    "name: |2\n  <s f=0/>\n  - item\n  it's \"quoted\" text\nkey: http://x.y/z\nname: don't\n"
    "a: 1\r\nb: 2\r\n"
    "- a\r\n- b\r\n# c\r\n"
    "k: |\r\n  text\r\n  more\r\n"
    "---\r\n# c\r\na\r\n"
    "a:\r\n  - x\r\n  - y\r\n"
    "k: 'a\r\n  b'\r\n"
    "? a\r\n: b\r\n"
    "a: 1\n\n\n\nb: 2\n"
    "a:\n  b:\n    c:\n      d: 1\n"
    "- - - - a\n"
    "k: v\t# tab comment\n"
    "? |\n  block key\n: |\n  block value\n"
    "a: !!str\nb: &x\nc: *x\n"
    "k: >+\n  a\n\n\n"
    "k: |+\n\n\n"
    // The empty lines behind a Keep scalar are part of its value, and a comment line ends them. The trivia come back in the Before, the End and the prelude.
    "a: |+\n  x\n\n\nb: 1\n"
    "a: |+\n  x\n\n# c\n\nb: 1\n"
    "a: |+\n  x\n# c\n\nb: 1\n"
    "- |+\n  x\n\n- y\n"
    "a:\n  b:\n    c: |+\n      x\n\nd: 1\n"
    "a: |+\n  x\n# c\n\n"
    "? |+\n  x\n\n: w\n"
    "|+\n  x\n\n"
    "|+\n  x\n\n--- y\n"
    "|+\n  x\n# c\n\n--- y\n"
    "k: |2\n   a\n  b\n"
    // A quoted scalar may hold U+FEFF ([107] and [118]). Single quotes have no escape for it, so the writer keeps it raw there.
    "'a\uFEFFb'"
    "k: 'x\uFEFF\n y'\n"
    // The lines that end the body of a Keep scalar: a line with a tab behind a root scalar and a line with a byte order mark behind any scalar.
    "|+\n  a\n\t\n"
    "|1+\n" + bom + " \n\n"
    "|+\n  a\n\t\n\n# c\n"
    "|+\n  a\n\t\n---\ny\n"
    "a: |+\n  x\n" + bom + "\n"
    "a:\n  b: |+\n    x\n" + bom + "\n# c\n"
    "- |+\n  x\n\n" + bom + "\n"
    "a: |+\n  x\n" + bom + "\n---\ny\n"
    // A root plain scalar or key that starts with a document marker, behind a blank or a tab in the text.
    " --- a\n"
    "  ...\n"
    "\t... x\n"
    " --- a\n\n  b\n"
    " --- a: b\n"
    " a: 1\n ... b: v\n"
    "&x\n --- a\n"
    "--- --- a\n"
    // NEL, NBSP and the two Unicode separators in every style, and DEL and a C1 control in single quotes.
    "a: b" + nel + "c\n"
    "a: b" + nbsp + "c\n"
    "b" + lineSeparator + ": v # x" + paragraphSeparator + "y\n"
    "a: 'b" + nel + lineSeparator + "c'\n"
    "a: \"b" + paragraphSeparator + "c\"\n"
    "a: |\n  b" + lineSeparator + "c\n"
    "a: 'b" + del + c1Control + "c'\n"
    "a: \"b\tc\"\n"
    // An implicit key within 1024 characters in its shortest spelling, and short flow keys in the normal spelling.
    "\"" + String.replicate 1010 "a" + String.replicate 8 "\t" + "\": v\n"
    "[" + String.concat "," (List.replicate 500 "a") + "]: v\n"
    "[" + String.concat "," (List.replicate 150 "a: b") + "]: v\n"
    "[a, b]: v\n"
    "[{a: b}, {c:}]: v\n"
    "{c: d, e: f}: g\n"
    "[{a: b, c: d}]: v\n"
    "[\"a\\x7Fb\"]: v\n"
    // The trivia behind the last node split between the Ends of the nested collections: a blank line goes with the comment that follows it (rule R4).
    "a:\n  b: c\n  # x\n\n# y\n"
    "a:\n  b: c\n\n  # x\n"
    "a:\n  b: c\n  # x\n\n"
    "a:\n  b:\n    c: d\n\n    # x\n  # y\n\n# z\n\n"
    "- - a\n  # x\n\n# y\n"
    "a:\n  b: c\n\n"
]

let private flowInputs: string list = [
    "['\n\n" + nel + "']\n"
    "[ a: b, 'c' ]\n"
    "[a, b]\n"
    "{a: 1, b: 2}\n"
    "[]\n"
    "{}\n"
    "k: [a, b]\n"
    "k: {a: 1}\n"
    "k: []\nj: {}\n"
    "- [a, b]\n- [c]\n"
    "- {a: 1}\n- {b: 2}\n"
    "[[a, b], [c, d]]\n"
    "{a: [1, 2], b: {c: d}}\n"
    "[{a: b}]\n"
    "[a: b]\n"
    "[a: b, c: d]\n"
    "[ a, b ]\n"
    "[a,b,c]\n"
    "{ a : 1 , b : 2 }\n"
    "{a, b: 1}\n"
    "{a: , b: 1}\n"
    "{? a : b}\n"
    "{? a, ? b}\n"
    "[\"a\", 'b', c]\n"
    "{\"a\": 1, 'b': 2}\n"
    "[&a x, *a]\n"
    "[!!str x, !local y]\n"
    "{&a k: v}\n"
    "[&a , b]\n"
    "[!!str , b]\n"
    "{!!str : v}\n"
    "[a, # one\n b # two\n]\n"
    "[ # open\n a,\n b\n]\n"
    "[\n a,\n b\n # end\n]\n"
    "[\n # before\n a,\n b\n]\n"
    "{\n a: 1, # one\n b: 2\n}\n"
    "{ a # key\n : b }\n"
    "{ a\n # gap\n : b }\n"
    "k: [\n  a,\n  b\n]\n"
    "k: [\n  a,\n  b\n  ]\n"
    "k: [a, b] # tail\n"
    "k: {a: 1} # tail\n"
    "[a, b] # tail\n"
    "- [a, # one\n  b]\n"
    "[a, b]: v\n"
    "{a: 1}: v\n"
    "? [a, b]\n: v\n"
    "? {a: 1}\n: [x, y]\n"
    "[a\n b, c]\n"
    "[a\n\n b, c]\n"
    "{a: b\n c}\n"
    "[\"a\\nb\", 'c''d']\n"
    "[1, 2.5, true, null, ~]\n"
    "---\n[a, b]\n"
    "--- [a, b]\n"
    "--- {a: 1}\n...\n"
    "k: !!seq [a]\n"
    "k: &x [a]\nj: *x\n"
    "[[[[a]]]]\n"
    "{a: {b: {c: {d: e}}}}\n"
    "[a, [b, [c]]]\n"
    "{a: [b, {c: [d]}]}\n"
    // An empty explicit key needs a blank behind the question mark, also in front of a comma, a closer or a line break.
    "{?\n}\n"
    "{? }\n"
    "{? , a}\n"
    "{? : v}\n"
    "{\n? explicit: entry,\nimplicit: entry,\n?\n}\n"
    "!!map { ?\n , k1\n ,'it''s' :\n 12:45} # c4\n"
    // The last item has properties only and a comment behind its comma.
    "[ &a, # c5\n]\n"
    "{a: &a, # c5\n}\n"
    "{ &a , # c5\n}\n"
    "[x, !!str , # c\n]\n"
    // A key of a flow mapping runs over several lines, as a plain scalar, a quoted scalar or a flow collection.
    "{a\n\nb}\n"
    "{a\n\nb: 1}\n"
    "{'a\n\nb': 1}\n"
    "{[a, # c\n b]: d}\n"
    "{[a, # c\n b]}\n"
    "{{a, # c\n b}: d}\n"
    // A plain key may end with a colon, and the colon of the entry follows it.
    "{? x:: v}\n"
    "{? k1 :: foo}\n"
    "{x:: v}\n"
    // U+FEFF in single quotes, in one line and folded over lines, stays raw in a flow collection too.
    "['a\uFEFFb', 'x\uFEFF\n y']\n"
    "{k: 'x\uFEFF\n y'}\n"
    // A missing key behind a question mark that ends its line has no token on the next line, so the writer writes no line of indentation only, which would read back as a blank line.
    "{? # c\n\n: b}\n"
    "[? # c\n\n: b]\n"
    "{? # c\n# d\n: b}\n"
    "{? # c\n: b}\n"
    "{? # c\n}\n"
    "{a: 1, ? # c\n\n: b, c: 2}\n"
    "- ? # c\n\n  : b\n"
]

let private roundTripList (name: string) (options: WriterOptions) =
    testCase name <| fun _ ->
        for input in blockInputs do
            expectRoundTrip options input

let private blockTests = testList "Block inputs" [
    testCase "the list holds at least 150 inputs" <| fun _ ->
        Expect.isTrue (blockInputs.Length >= 150) (sprintf "The list holds %d inputs" blockInputs.Length)

    roundTripList "every input comes back with an indentation of 2 and the line break LF" defaults
    roundTripList "every input comes back with an indentation of 4" fourSpaces
    roundTripList "every input comes back with the line break CRLF" crlf
    roundTripList "every input comes back with an indentation of 4 and the line break CRLF" wideCrlf
    roundTripList "every input comes back under RequireStored" required

    // The writer gives the same text for a tree that the parser produced from a text with CRLF.
    testCase "a text with CRLF gives the tree of the text with LF" <| fun _ ->
        for input in blockInputs do
            if not (input.Contains "\r") then
                let crlfInput = input.Replace("\n", "\r\n")
                match Parser.parseStream crlfInput with
                | Ok streamOfCrlf -> Expect.streamEqual streamOfCrlf (parseOk input) (sprintf "The tree of %A with CRLF" input)
                | Error error -> failwithf "The parser rejects %A with CRLF: %s" input (describeError error)
]

let private constructedTests = testList "Constructed trees" [
    // A tree that a program builds may carry a style that cannot express its value. The writer changes the style, and the tree that comes
    // back keeps the value. The second round trip changes nothing.
    testCase "a constructed tree reads back with the same values and is stable" <| fun _ ->
        let texts = [ "a: b"; "x\u0001y"; "no final feed"; "- x"; ""; "# y"; "v # not a comment"; "@id"; " lead"; "trail "; "a\nb"; "--- x" ]
        let nodes =
            [ for i, text in List.indexed texts ->
                  let node = if i = 2 then blockScalar BlockScalarStyle.Literal ChompingMode.Clip None text elif i = 1 then sq text else sc text
                  "k" + string i, node ]
        // A last entry keeps the end of the text out of the way of the block scalar.
        let tree = YamlNode.mapping [ for (key, node) in nodes @ [ "last", sc "1" ] -> YamlNode.entry (sc key) node ]
        let written = Writer.writeNode defaults tree
        let reparsed = parseRoot written
        let values = [ for entry in YamlNode.entries reparsed -> (YamlNode.tryScalar entry.Value).Value.Value ]
        Expect.equal values (texts @ [ "1" ]) (sprintf "The values read from %A" written)
        Expect.equal (Writer.writeNode defaults reparsed) written "A second write changes nothing"

    // The plain constructors give a block collection behind a colon a line break and a block collection with properties a line break behind
    // its last property, so the tree equals the tree that the parser reads from the text.
    testCase "a tree of nested collections and properties comes back equal" <| fun _ ->
        let entry (key: string) (value: YamlNode) = YamlNode.entry (sc key) value
        let tagged = YamlNode.mapping [ entry "c" (sc "3") ] |> YamlNode.withTag (YamlTag.core "map") |> YamlNode.withAnchor "m"
        let tree =
            YamlNode.mapping [
                entry "map" (YamlNode.mapping [ entry "a" (sc "1") ])
                entry "seq" (YamlNode.sequence [ YamlNode.item (sc "x"); YamlNode.item (YamlNode.mapping [ entry "b" (sc "2") ]) ])
                entry "tagged" tagged
                entry "list" (YamlNode.sequence [ YamlNode.item (YamlNode.sequence [ YamlNode.item (sc "y") ]) ] |> YamlNode.withTag (YamlTag.core "seq"))
                entry "alias" (YamlNode.alias "m")
            ]
        let written = Writer.writeNode defaults tree
        Expect.nodeEqual (parseRoot written) tree (sprintf "The tree read from %A" written)
        Expect.equal (Writer.writeNode defaults (parseRoot written)) written "A second write changes nothing"

    testCase "a constructed tree with comments in every block slot comes back equal" <| fun _ ->
        let tree =
            YamlNode.mapping [
                { YamlNode.entry (sc "a") (withNote " t" (sc "1")) with Before = [ note " first"; blank ] }
                { YamlNode.entry (sc "b") (sc "v") with Colon = Some(newLine (Some " c") [ note " f" ]) }
                { YamlNode.entry (sc "list") (YamlNode.sequence [ YamlNode.item (sc "x"); YamlNode.item (sc "y") ]) with Colon = Some(newLine (Some " d") []) }
                explicitEntry (sc "k") (sc "v")
            ]
        let written = Writer.writeNode defaults tree
        Expect.nodeEqual (parseRoot written) tree (sprintf "The tree after a round trip of %A" written)

    // The writer puts the comment behind the key of a flow mapping entry on the line of the key and the colon on the next line ([147] has the colon behind the
    // key, and a comment ends the line). The text is in Tests.Writer. These tests build the tree and read the text back. The parser has to read the comment
    // behind a key as the TrailingComment of the key (rule R8). A parser that reads it as a line in BeforeColon makes these tests fail.
    testCase "a trailing comment of a flow mapping key goes behind the key and comes back equal" <| fun _ ->
        let tree = YamlNode.flowMapping [ YamlNode.entry (withNote " key" (sc "a")) (sc "b") ]
        let written = Writer.writeNode defaults tree
        Expect.equal written "{\n  a # key\n  : b\n}\n" "The text of the tree"
        Expect.nodeEqual (parseRoot written) tree (sprintf "The tree read from %A" written)
        Expect.equal (Writer.writeNode defaults (parseRoot written)) written "A second write changes nothing"

    // An entry with a colon and no value has two places for a comment: behind the key and behind the colon. The comment behind the key stays behind the key.
    testCase "a flow mapping entry with a colon and no value keeps the comments of the key and of the colon" <| fun _ ->
        let key = withNote " key" (sc "a")
        let inlineColon = YamlNode.entry key nothing
        let commentedColon = { inlineColon with Colon = Some(newLine (Some " colon") []) }
        for entries in [ [ inlineColon ]; [ commentedColon ]; [ inlineColon; YamlNode.entry (sc "j") (sc "1") ]; [ commentedColon; YamlNode.entry (sc "j") (sc "1") ] ] do
            let tree = YamlNode.flowMapping entries
            let written = Writer.writeNode defaults tree
            Expect.nodeEqual (parseRoot written) tree (sprintf "The tree read from %A" written)
            Expect.equal (Writer.writeNode defaults (parseRoot written)) written "A second write changes nothing"

    // The content of a root block scalar starts at the first column unless an indentation indicator says otherwise, so the writer adds the indicator 1 to a root block
    // scalar without content lines that has trivia behind it. The tree that the parser reads from the text has the indicator, and a second write gives the text again.
    testCase "a root block scalar without content and a comment behind it read back with the indicator" <| fun _ ->
        let cases =
            [ BlockScalarStyle.Literal, ChompingMode.Clip, ""
              BlockScalarStyle.Folded, ChompingMode.Clip, ""
              BlockScalarStyle.Literal, ChompingMode.Strip, ""
              BlockScalarStyle.Literal, ChompingMode.Keep, "\n"
              BlockScalarStyle.Literal, ChompingMode.Keep, "\n\n" ]
        for (style, chomping, value) in cases do
            let tree = { YamlDocument.create (blockScalar style chomping None value) with AfterRoot = [ note " c" ] }
            let written = Writer.write defaults tree
            let expected = { tree with Root = blockScalar style chomping (Some 1) value }
            Expect.streamEqual (parseOk written) { Parts = [ StreamPart.Document expected ] } (sprintf "The document read from %A" written)
            Expect.equal (Writer.writeDocuments defaults (parseOk written)) written "A second write changes nothing"

    // The empty lines behind a Keep scalar are part of its value ([165] to [169]), so a BlankLine trivia directly behind a scalar that the writer would give the chomping
    // Keep reads back as part of the value. The writer double-quotes the scalar, and the tree that comes back has the same value, the double-quoted style and the same trivia.
    testCase "a scalar with two line feeds and a blank line behind it comes back double-quoted with the same value and trivia" <| fun _ ->
        for place in Tests.Writer.keepPlaces do
            for stored in [ ChompingMode.Clip; ChompingMode.Strip ] do
                let written = Writer.writeDocuments defaults (place.Build(blockScalar BlockScalarStyle.Literal stored None "x\n\n"))
                Expect.streamEqual (parseOk written) (place.Build(dq "x\n\n")) (sprintf "The tree read in %s from %A" place.Name written)
                Expect.equal (Writer.writeDocuments defaults (parseOk written)) written (sprintf "The second write in %s" place.Name)

    // A stored Keep scalar with a blank line where only trivia follow comes back as the same tree. The blank line is a byte order mark line behind a scalar in a collection and a
    // tab line behind a root scalar, which the parser reads as a blank line that ends the body.
    testCase "a stored Keep scalar with a blank line at the end of the text comes back equal" <| fun _ ->
        for place in Tests.Writer.keepPlaces do
            if place.Kept.IsSome then
                let tree = place.Build(blockScalar BlockScalarStyle.Literal ChompingMode.Keep None "x\n")
                for options in [ defaults; crlf; fourSpaces; required ] do
                    let written = Writer.writeDocuments options tree
                    Expect.streamEqual (parseOk written) tree (sprintf "The tree read in %s from %A" place.Name written)
                    Expect.equal (Writer.writeDocuments options (parseOk written)) written (sprintf "The second write in %s" place.Name)

    // A comment line between the Keep scalar and the blank line ends the empty lines of the scalar (l-trail-comments), so the scalar stays a block scalar and the trivia come back.
    testCase "a Keep scalar with a comment line and a blank line behind it comes back equal" <| fun _ ->
        let keep = blockScalar BlockScalarStyle.Literal ChompingMode.Keep None "x\n"
        let behind = [ note " c"; blank ]
        let trees =
            [ YamlNode.mapping [ YamlNode.entry (sc "a") keep; { YamlNode.entry (sc "b") (sc "1") with Before = behind } ]
              YamlNode.sequence [ YamlNode.item keep; { YamlNode.item (sc "y") with Before = behind } ]
              withEnd behind (YamlNode.mapping [ YamlNode.entry (sc "a") keep ])
              YamlNode.mapping [ YamlNode.entry (sc "a") (YamlNode.mapping [ YamlNode.entry (sc "b") keep ]); { YamlNode.entry (sc "d") (sc "1") with Before = behind } ] ]
        for tree in trees do
            let written = Writer.writeNode defaults tree
            Expect.nodeEqual (parseRoot written) tree (sprintf "The tree read from %A" written)
            Expect.equal (Writer.writeNode defaults (parseRoot written)) written "A second write changes nothing"

    // The trivia behind a stored Keep scalar at the end of the text stand in the Ends of the collections along the last node. The first blank line is written as a byte order mark line,
    // and a comment line ends the empty lines. The Ends follow the rule R4 for the run, so the tree that comes back has the same Ends.
    testCase "the Ends behind a nested Keep scalar come back equal" <| fun _ ->
        let keep = blockScalar BlockScalarStyle.Literal ChompingMode.Keep None "x\n"
        let inner (value: YamlNode) (lines: Trivia list) = withEnd lines (YamlNode.mapping [ YamlNode.entry (sc "b") value ])
        let outer (lines: Trivia list) (child: YamlNode) = withEnd lines (YamlNode.mapping [ YamlNode.entry (sc "a") child ])
        let trees =
            [ outer [ blank ] (inner keep [])
              outer [] (inner keep [ blank; note " c" ])
              outer [ blank; note " d" ] (inner keep [ blank; note " c" ])
              outer [ blank; note " d"; blank ] (inner keep [ blank; note " c" ])
              outer [] (inner keep [ note " c"; blank ])
              outer [ blank ] (outer [] (inner keep []))
              outer [] (outer [ blank; note " m" ] (inner keep []))
              outer [ note " d"; blank ] (outer [ blank; note " m" ] (inner keep [ blank; note " c" ])) ]
        for tree in trees do
            for options in [ defaults; required ] do
                let written = Writer.writeNode options tree
                Expect.nodeEqual (parseRoot written) tree (sprintf "The tree read from %A" written)
                Expect.equal (Writer.writeNode options (parseRoot written)) written "A second write changes nothing"

    testCase "a comment behind the properties of a flow mapping key without content comes back equal" <| fun _ ->
        let key = withLayout [ placement PropertyName.Anchor (newLine (Some " key") []) ] (YamlNode.withAnchor "a" nothing)
        let tree = YamlNode.flowMapping [ YamlNode.entry key (sc "b") ]
        let written = Writer.writeNode defaults tree
        Expect.equal written "{\n  &a # key\n  : b\n}\n" "The text of the tree"
        Expect.nodeEqual (parseRoot written) tree (sprintf "The tree read from %A" written)
        Expect.equal (Writer.writeNode defaults (parseRoot written)) written "A second write changes nothing"
]

let flowRoundTrips = testList "Flow inputs" [
    for input in flowInputs ->
        testCase (sprintf "%A" input) <| fun _ ->
            expectRoundTrip defaults input
            expectRoundTrip wideCrlf input
            expectRoundTrip required input
]

let Main = testList "RoundTrip" [ blockTests; constructedTests; flowRoundTrips ]
