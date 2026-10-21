module Tests.Projection

open Fable.Pyxpecto
open YAMLicious

let private document (root: YamlNode) : StreamPart = StreamPart.Document (YamlDocument.create root)

let private streamOf (parts: StreamPart list) : YamlStream = { Parts = parts }

let private single (root: YamlNode) : YamlStream = streamOf [ document root ]

let private field (key: string) (value: YamlNode) = YamlNode.entry (YamlNode.plain key) value

/// A comment with a span on one line, from `column` on.
let private located (text: string) (line: int) (column: int) : YamlComment =
    { Text = text
      Span =
        Some
            { Start = { Line = line; Column = column }
              Finish = { Line = line; Column = column + text.Length + 1 } } }

// Pieces of expected JSON. They only spell out the shapes, the structure of every expectation is written by hand.

let private strJson (text: string) =
    "{\"k\":\"scalar\",\"t\":\"str\",\"v\":\"" + text + "\",\"tag\":null,\"anchor\":null}"

let private intJson (text: string) =
    "{\"k\":\"scalar\",\"t\":\"int\",\"v\":\"" + text + "\",\"tag\":null,\"anchor\":null}"

/// The data projection of one document around the JSON of its root.
let private envelope (depth: int) (root: string) =
    "{\"ok\":true,\"docCount\":1,\"maxDepth\":" + string depth + ",\"docs\":[{\"data\":" + root + ",\"maxDepth\":" + string depth + "}]}"

let private noDirectives = "\"directives\":{\"yaml\":null,\"tags\":[],\"reserved\":[]},\"docStart\":false,\"docEnd\":false"

/// The presentation projection of one document without directives and markers.
let private presentationOf (styles: string) (collections: string) (layouts: string) (trivia: string) =
    "{\"docCount\":1,\"docs\":[{\"styles\":[" + styles + "],\"collections\":[" + collections + "],\"layouts\":[" + layouts + "]," + noDirectives + "}],\"trivia\":[" + trivia + "]}"

/// The trivia list of a presentation projection, from "trivia" on.
let private triviaOf (stream: YamlStream) : string =
    let text = Projection.presentation stream
    text.Substring(text.IndexOf "\"trivia\":")

let private typeOf (tag: YamlTag option) (style: ScalarStyle) (text: string) : string =
    let node =
        match tag with
        | Some explicit -> YamlNode.withTag explicit (YamlNode.scalar style text)
        | None -> YamlNode.scalar style text
    let json = Projection.data (single node)
    let start = json.IndexOf "\"t\":\"" + 5
    json.Substring(start, json.IndexOf("\",\"v\":") - start)

let private nestedSequence (levels: int) (leaf: YamlNode) : YamlNode =
    let mutable node = leaf
    for _ in 1 .. levels do
        node <- YamlNode.sequence [ YamlNode.item node ]
    node

let private nestedMapping (levels: int) (leaf: YamlNode) : YamlNode =
    let mutable node = leaf
    for _ in 1 .. levels do
        node <- YamlNode.mapping [ field "k" node ]
    node

let private countOf (text: string) (piece: string) : int =
    let mutable count = 0
    let mutable index = text.IndexOf piece
    while index >= 0 do
        count <- count + 1
        index <- text.IndexOf(piece, index + piece.Length)
    count

let tests_data = testList "Data projection" [
    // yaml@2.6.1 reads these plain texts as null, bool, int and float, and every quoted text as a string.
    testCase "a plain scalar has the type that the core schema reads" <| fun _ ->
        let cases =
            [ "abc", "str"; "1", "int"; "-12", "int"; "+5", "int"; "0o17", "int"; "0x1F", "int"
              "9223372036854775808", "int"; "123456789012345678901234567890", "int"
              "1.5", "float"; ".5", "float"; "5.", "float"; "1e3", "float"; ".inf", "float"; "-.INF", "float"; ".nan", "float"
              "true", "bool"; "False", "bool"; "TRUE", "bool"
              "null", "null"; "Null", "null"; "~", "null"; "", "null"
              "yes", "str"; "on", "str"; "NaN", "str"; "1_000", "str"; "0X1F", "str"; "a\nb", "str" ]
        for (text, expected) in cases do
            Expect.equal (typeOf None ScalarStyle.Plain text) expected ("the type of '" + text + "'")

    testCase "a quoted or block scalar is a string" <| fun _ ->
        for style in [ ScalarStyle.SingleQuoted; ScalarStyle.DoubleQuoted; ScalarStyle.Block (BlockScalarStyle.Literal, ChompingMode.Clip, None) ] do
            for text in [ "1"; "true"; "null"; "1.5"; "" ] do
                Expect.equal (typeOf None style text) "str" ("the type of '" + text + "'")

    // yaml@2.6.1 resolves an explicit tag only when the text has the form of the type: `!!int "2"` is 2,
    // `!!int abc`, `!!float 5` and `!!bool maybe` are the strings, and `!!null ""` is null. It reads `!!set a` and
    // `!!omap a` as the string a with the warning TAG_RESOLVE_FAILED, and `!!binary abc` and `!!timestamp 2001-01-01`
    // as a Buffer and a Date.
    testCase "an explicit core tag fixes the type when the text has its form" <| fun _ ->
        let cases =
            [ "int", ScalarStyle.DoubleQuoted, "2", "int"; "int", ScalarStyle.Plain, "0x1F", "int"; "int", ScalarStyle.Plain, "abc", "str"
              "int", ScalarStyle.Plain, "1.5", "str"; "int", ScalarStyle.Plain, "", "str"; "int", ScalarStyle.Plain, "99999999999999999999", "int"
              "float", ScalarStyle.DoubleQuoted, "1.5", "float"; "float", ScalarStyle.Plain, "5", "str"; "float", ScalarStyle.Plain, "5.0", "float"
              "float", ScalarStyle.Plain, ".inf", "float"; "float", ScalarStyle.Plain, "abc", "str"
              "bool", ScalarStyle.DoubleQuoted, "true", "bool"; "bool", ScalarStyle.Plain, "maybe", "str"
              "null", ScalarStyle.DoubleQuoted, "", "null"; "null", ScalarStyle.Plain, "null", "null"; "null", ScalarStyle.Plain, "x", "str"
              "str", ScalarStyle.Plain, "5", "str"; "str", ScalarStyle.Plain, "true", "str"; "str", ScalarStyle.Plain, "", "str"
              "binary", ScalarStyle.Plain, "abc", "other"; "timestamp", ScalarStyle.Plain, "2001-01-01", "other"; "set", ScalarStyle.Plain, "a", "str"; "omap", ScalarStyle.Plain, "a", "str"
              "seq", ScalarStyle.Plain, "x", "str" ]
        for (suffix, style, text, expected) in cases do
            Expect.equal (typeOf (Some (YamlTag.core suffix)) style text) expected ("the type of !!" + suffix + " '" + text + "'")

    testCase "a local tag, a tag of another namespace and the non-specific tag make strings" <| fun _ ->
        Expect.equal (typeOf (Some (YamlTag.local "foo")) ScalarStyle.Plain "7") "str" "local"
        Expect.equal (typeOf (Some YamlTag.NonSpecific) ScalarStyle.Plain "7") "str" "non-specific"
        let foreign = YamlTag.Named { Prefix = "tag:example.com,2000:"; Suffix = "int" }
        Expect.equal (typeOf (Some foreign) ScalarStyle.Plain "7") "str" "another namespace"

    testCase "a scalar has its cooked text, tag and anchor" <| fun _ ->
        let node = YamlNode.plain "x" |> YamlNode.withTag (YamlTag.core "str") |> YamlNode.withAnchor "a"
        Expect.equal
            (Projection.data (single node))
            (envelope 0 """{"k":"scalar","t":"str","v":"x","tag":"tag:yaml.org,2002:str","anchor":"a"}""")
            "tag and anchor"
        let local = YamlNode.plain "x" |> YamlNode.withTag (YamlTag.local "custom")
        Expect.equal (Projection.data (single local)) (envelope 0 """{"k":"scalar","t":"str","v":"x","tag":"!custom","anchor":null}""") "local tag"
        let nonSpecific = YamlNode.plain "x" |> YamlNode.withTag YamlTag.NonSpecific
        Expect.equal (Projection.data (single nonSpecific)) (envelope 0 """{"k":"scalar","t":"str","v":"x","tag":"!","anchor":null}""") "non-specific tag"
        let expanded = YamlNode.plain "x" |> YamlNode.withTag (YamlTag.Named { Prefix = "tag:example.com,2000:app/"; Suffix = "foo" })
        Expect.equal (Projection.data (single expanded)) (envelope 0 """{"k":"scalar","t":"str","v":"x","tag":"tag:example.com,2000:app/foo","anchor":null}""") "expanded handle"

    testCase "a mapping, a sequence, an alias and the empty collections" <| fun _ ->
        let flowItems = [ YamlNode.item (YamlNode.plain "x"); YamlNode.item (YamlNode.scalar ScalarStyle.DoubleQuoted "y") ]
        let tree =
            YamlNode.mapping
                [ field "a" (YamlNode.plain "1")
                  field "b" (YamlNode.flowSequence flowItems)
                  field "c" (YamlNode.flowMapping [])
                  field "d" (YamlNode.alias "base")
                  field "e" (YamlNode.sequence []) ]
        let entryJson (key: string) (value: string) = "[" + strJson key + "," + value + "]"
        let flowSequenceJson = """{"k":"seq","tag":null,"anchor":null,"style":"flow","items":[""" + strJson "x" + "," + strJson "y" + "]}"
        let entries =
            [ entryJson "a" (intJson "1")
              entryJson "b" flowSequenceJson
              entryJson "c" """{"k":"map","tag":null,"anchor":null,"style":"flow","entries":[]}"""
              entryJson "d" """{"k":"alias","name":"base"}"""
              entryJson "e" """{"k":"seq","tag":null,"anchor":null,"style":"flow","items":[]}""" ]
        let expected = """{"k":"map","tag":null,"anchor":null,"style":"block","entries":[""" + String.concat "," entries + "]}"
        Expect.equal (Projection.data (single tree)) (envelope 2 expected) "entries and items in order"

    testCase "a collection carries its own tag and anchor" <| fun _ ->
        let tree =
            YamlNode.mapping [ field "k" (YamlNode.plain "v") ]
            |> YamlNode.withTag (YamlTag.core "map")
            |> YamlNode.withAnchor "m"
        let expected =
            """{"k":"map","tag":"tag:yaml.org,2002:map","anchor":"m","style":"block","entries":[["""
            + strJson "k" + "," + strJson "v" + "]]}"
        Expect.equal (Projection.data (single tree)) (envelope 1 expected) "tag and anchor"

    testCase "a collection can be a key" <| fun _ ->
        let key = YamlNode.flowSequence [ YamlNode.item (YamlNode.plain "k") ]
        let tree = YamlNode.mapping [ YamlNode.entry key (YamlNode.plain "v") ]
        let expected =
            """{"k":"map","tag":null,"anchor":null,"style":"block","entries":[[{"k":"seq","tag":null,"anchor":null,"style":"flow","items":["""
            + strJson "k" + "]}," + strJson "v" + "]]}"
        Expect.equal (Projection.data (single tree)) (envelope 2 expected) "the key is a sequence"

    // The depth of a scalar root is 0, a collection of scalars 1, and an alias is no collection.
    testCase "maxDepth counts the nesting of collections" <| fun _ ->
        Expect.equal (Projection.data (single (YamlNode.plain "x"))) (envelope 0 (strJson "x")) "scalar root"
        Expect.equal (Projection.data (single (YamlNode.alias "a"))) (envelope 0 """{"k":"alias","name":"a"}""") "alias root"
        let flat = Projection.data (single (YamlNode.sequence [ YamlNode.item (YamlNode.plain "x") ]))
        Expect.stringContains flat "\"maxDepth\":1,\"docs\"" "a collection of scalars"
        let nested = YamlNode.sequence [ YamlNode.item (YamlNode.mapping [ field "k" (YamlNode.sequence [ YamlNode.item (YamlNode.alias "a") ]) ]) ]
        Expect.stringContains (Projection.data (single nested)) "\"maxDepth\":3,\"docs\"" "three collections inside each other"

    testCase "the text is escaped as JSON.stringify escapes it" <| fun _ ->
        let text = "q\"b\\n\n\t\u0001\u001f\u007f é😀"
        let expected = """{"k":"scalar","t":"str","v":"q\"b\\n\n\t\u0001\u001f""" + "\u007f é😀" + """","tag":null,"anchor":null}"""
        Expect.equal (Projection.data (single (YamlNode.plain text))) (envelope 0 expected) "escapes"
#if !FABLE_COMPILER_PYTHON
        // A Python string cannot hold a surrogate without a partner: the string functions of fable-library fail on it.
        let lone = Projection.data (single (YamlNode.plain ("a" + string (char 0xD800) + "b")))
        Expect.stringContains lone "\"v\":\"a\\ud800b\"" "a surrogate without a partner is escaped"
#endif

    testCase "an empty stream, an empty document and two documents" <| fun _ ->
        Expect.equal (Projection.data (streamOf [])) """{"ok":true,"docCount":0,"maxDepth":0,"docs":[]}""" "no document"
        Expect.equal
            (Projection.data (streamOf [ StreamPart.Trivia (FullLineComment (comment " only a comment")) ]))
            """{"ok":true,"docCount":0,"maxDepth":0,"docs":[]}"""
            "a comment is no document"
        Expect.equal
            (Projection.data (single YamlNode.empty))
            (envelope 0 """{"k":"scalar","t":"null","v":"","tag":null,"anchor":null}""")
            "an empty root is a null scalar"
        let two = streamOf [ document (YamlNode.plain "a"); document (YamlNode.sequence [ YamlNode.item (YamlNode.plain "b") ]) ]
        let expected =
            """{"ok":true,"docCount":2,"maxDepth":1,"docs":["""
            + """{"data":""" + strJson "a" + ""","maxDepth":0},"""
            + """{"data":{"k":"seq","tag":null,"anchor":null,"style":"block","items":[""" + strJson "b" + """]},"maxDepth":1}]}"""
        Expect.equal (Projection.data two) expected "two documents"
]

let private literalScalar = YamlNode.scalar (ScalarStyle.Block (BlockScalarStyle.Literal, ChompingMode.Strip, Some 2)) "z\n"

let tests_presentation = testList "Presentation projection: styles, collections and layouts" [
    testCase "every scalar style with its chomping and indentation indicator" <| fun _ ->
        let tree =
            YamlNode.mapping
                [ field "a" (YamlNode.plain "x")
                  field "b" (YamlNode.scalar ScalarStyle.SingleQuoted "y")
                  field "c" literalScalar
                  field "d" (YamlNode.scalar (ScalarStyle.Block (BlockScalarStyle.Folded, ChompingMode.Keep, None)) "w")
                  field "e" (YamlNode.scalar ScalarStyle.DoubleQuoted "q")
                  field "f" (YamlNode.scalar (ScalarStyle.Block (BlockScalarStyle.Literal, ChompingMode.Clip, None)) "v\n") ]
        let styles =
            """{"path":"/a","role":"key","style":"plain"},{"path":"/a","role":"value","style":"plain"},"""
            + """{"path":"/b","role":"key","style":"plain"},{"path":"/b","role":"value","style":"single"},"""
            + """{"path":"/c","role":"key","style":"plain"},{"path":"/c","role":"value","style":"literal","chomp":"strip","indent":2},"""
            + """{"path":"/d","role":"key","style":"plain"},{"path":"/d","role":"value","style":"folded","chomp":"keep","indent":null},"""
            + """{"path":"/e","role":"key","style":"plain"},{"path":"/e","role":"value","style":"double"},"""
            + """{"path":"/f","role":"key","style":"plain"},{"path":"/f","role":"value","style":"literal","chomp":"clip","indent":null}"""
        Expect.equal
            (Projection.presentation (single tree))
            (presentationOf styles """{"path":"","role":"value","style":"block"}""" "" "")
            "styles in the order of the text"

    testCase "collection styles in the order of their openers" <| fun _ ->
        let tree =
            YamlNode.sequence
                [ YamlNode.item (YamlNode.flowSequence [ YamlNode.item (YamlNode.plain "a") ])
                  YamlNode.item (YamlNode.mapping [ field "k" (YamlNode.plain "v") ])
                  YamlNode.item (YamlNode.flowMapping []) ]
        let collections =
            """{"path":"","role":"value","style":"block"},{"path":"/0","role":"value","style":"flow"},"""
            + """{"path":"/1","role":"value","style":"block"},{"path":"/2","role":"value","style":"flow"}"""
        let styles =
            """{"path":"/0/0","role":"value","style":"plain"},{"path":"/1/k","role":"key","style":"plain"},{"path":"/1/k","role":"value","style":"plain"}"""
        Expect.equal (Projection.presentation (single tree)) (presentationOf styles collections "" "") "collections"

    testCase "a path escapes ~ and / and names a key that is not a scalar by its index" <| fun _ ->
        let key = YamlNode.flowSequence [ YamlNode.item (YamlNode.plain "k") ]
        let tree = YamlNode.mapping [ field "a/b" (YamlNode.plain "1"); field "~" (YamlNode.plain "2"); YamlNode.entry key (YamlNode.plain "v") ]
        let styles =
            """{"path":"/a~1b","role":"key","style":"plain"},{"path":"/a~1b","role":"value","style":"plain"},"""
            + """{"path":"/~0","role":"key","style":"plain"},{"path":"/~0","role":"value","style":"plain"},"""
            + """{"path":"/?2/0","role":"value","style":"plain"},{"path":"/?2","role":"value","style":"plain"}"""
        let collections = """{"path":"","role":"value","style":"block"},{"path":"/?2","role":"key","style":"flow"}"""
        Expect.equal (Projection.presentation (single tree)) (presentationOf styles collections "" "") "paths"

    testCase "withTag and withAnchor give a node one layout entry for each property, the tag first" <| fun _ ->
        let node = YamlNode.plain "x" |> YamlNode.withTag (YamlTag.core "str") |> YamlNode.withAnchor "a"
        let layouts = """{"path":"","role":"value","layout":[{"property":"tag","after":"inline"},{"property":"anchor","after":"inline"}]}"""
        Expect.equal
            (Projection.presentation (single node))
            (presentationOf """{"path":"","role":"value","style":"plain"}""" "" layouts "")
            "tag first"

    testCase "the layout of a parsed node keeps the order and the line breaks of its properties" <| fun _ ->
        let after =
            Separation.NewLine
                { Comment = Some (comment " c")
                  Following = [ BlankLine None; FullLineComment (comment " d") ] }
        let node =
            { YamlNode.plain "x" with
                Properties = { Tag = Some (YamlTag.core "str"); Anchor = Some "a" }
                PropertyLayout =
                    [ { Property = PropertyName.Anchor; After = after; Span = None }
                      { Property = PropertyName.Tag; After = Separation.Inline; Span = None } ] }
        let layouts = """{"path":"","role":"value","layout":[{"property":"anchor","after":"newline"},{"property":"tag","after":"inline"}]}"""
        let trivia =
            """{"kind":"comment","text":" c","line":null,"col":null,"placement":"endOfLine","slot":"root.PropertyLayout[0].After.Comment","path":"","doc":0},"""
            + """{"kind":"blank","text":null,"line":null,"col":null,"placement":"ownLine","slot":"root.PropertyLayout[0].After.Following[0]","path":"","doc":0},"""
            + """{"kind":"comment","text":" d","line":null,"col":null,"placement":"ownLine","slot":"root.PropertyLayout[0].After.Following[1]","path":"","doc":0}"""
        Expect.equal
            (Projection.presentation (single node))
            (presentationOf """{"path":"","role":"value","style":"plain"}""" "" layouts trivia)
            "anchor first, line break behind it"
]

let private withEnd (lines: Trivia list) (node: YamlNode) : YamlNode =
    match node.Kind with
    | YamlNodeKind.Sequence content -> { node with Kind = YamlNodeKind.Sequence { content with End = lines } }
    | YamlNodeKind.Mapping content -> { node with Kind = YamlNodeKind.Mapping { content with End = lines } }
    | _ -> failwith "Only a collection has an end."

let tests_comments = testList "Presentation projection: comments and blank lines" [
    // The slots of one mapping entry in the order of the text: the lines in front of it, the explicit `?`, the key,
    // the lines before the colon, the colon and the value.
    testCase "the slots of a mapping entry" <| fun _ ->
        let key = { YamlNode.plain "a" with TrailingComment = Some (located " key" 3 5) }
        let value = { YamlNode.plain "1" with TrailingComment = Some (located " value" 7 8) }
        let entry =
            { YamlNode.entry key value with
                Before = [ FullLineComment (located " before" 1 1); BlankLine None ]
                Question = Some (Separation.NewLine { Comment = Some (located " question" 2 3); Following = [ FullLineComment (located " follow" 2 1) ] })
                BeforeColon = [ FullLineComment (comment " colon") ]
                Colon = Some (Separation.NewLine { Comment = Some (comment " c"); Following = [ BlankLine None ] }) }
        let trivia =
            """{"kind":"comment","text":" before","line":1,"col":1,"placement":"ownLine","slot":"root.entries[0].Before[0]","path":"","doc":0},"""
            + """{"kind":"blank","text":null,"line":null,"col":null,"placement":"ownLine","slot":"root.entries[0].Before[1]","path":"","doc":0},"""
            + """{"kind":"comment","text":" question","line":2,"col":3,"placement":"endOfLine","slot":"root.entries[0].Question.Comment","path":"","doc":0},"""
            + """{"kind":"comment","text":" follow","line":2,"col":1,"placement":"ownLine","slot":"root.entries[0].Question.Following[0]","path":"","doc":0},"""
            + """{"kind":"comment","text":" key","line":3,"col":5,"placement":"endOfLine","slot":"root.entries[0].Key.TrailingComment","path":"/a","doc":0},"""
            + """{"kind":"comment","text":" colon","line":null,"col":null,"placement":"ownLine","slot":"root.entries[0].BeforeColon[0]","path":"","doc":0},"""
            + """{"kind":"comment","text":" c","line":null,"col":null,"placement":"endOfLine","slot":"root.entries[0].Colon.Comment","path":"","doc":0},"""
            + """{"kind":"blank","text":null,"line":null,"col":null,"placement":"ownLine","slot":"root.entries[0].Colon.Following[0]","path":"","doc":0},"""
            + """{"kind":"comment","text":" value","line":7,"col":8,"placement":"endOfLine","slot":"root.entries[0].Value.TrailingComment","path":"/a","doc":0}"""
        Expect.equal (triviaOf (single (YamlNode.mapping [ entry ]))) ("\"trivia\":[" + trivia + "]}") "slots of an entry"

    // The reference projection of yaml@2.6.1 reports a blank line with its line and a null column.
    testCase "a blank line with a span shows its line and a null column" <| fun _ ->
        let span: SourceSpan =
            { Start = { Line = 4; Column = 1 }
              Finish = { Line = 4; Column = 1 } }
        let entry = { field "a" (YamlNode.plain "1") with Before = [ BlankLine (Some span); BlankLine None ] }
        let trivia =
            """{"kind":"blank","text":null,"line":4,"col":null,"placement":"ownLine","slot":"root.entries[0].Before[0]","path":"","doc":0},"""
            + """{"kind":"blank","text":null,"line":null,"col":null,"placement":"ownLine","slot":"root.entries[0].Before[1]","path":"","doc":0}"""
        Expect.equal (triviaOf (single (YamlNode.mapping [ entry ]))) ("\"trivia\":[" + trivia + "]}") "blank lines"

    testCase "the slots of a sequence item and of a flow sequence" <| fun _ ->
        let inner =
            YamlNode.flowSequence [ { YamlNode.item (YamlNode.plain "x") with Before = [ FullLineComment (comment " in") ] } ]
            |> YamlNode.withOpeningComment (comment " open")
            |> withEnd [ FullLineComment (comment " end") ]
            |> YamlNode.withTrailingComment (comment " close")
        let item =
            { YamlNode.item inner with
                Before = [ BlankLine None; FullLineComment (comment " b") ]
                Dash = Some (Separation.NewLine { Comment = Some (comment " dash"); Following = [ BlankLine None ] }) }
        let trivia =
            """{"kind":"blank","text":null,"line":null,"col":null,"placement":"ownLine","slot":"root.items[0].Before[0]","path":"","doc":0},"""
            + """{"kind":"comment","text":" b","line":null,"col":null,"placement":"ownLine","slot":"root.items[0].Before[1]","path":"","doc":0},"""
            + """{"kind":"comment","text":" dash","line":null,"col":null,"placement":"endOfLine","slot":"root.items[0].Dash.Comment","path":"","doc":0},"""
            + """{"kind":"blank","text":null,"line":null,"col":null,"placement":"ownLine","slot":"root.items[0].Dash.Following[0]","path":"","doc":0},"""
            + """{"kind":"comment","text":" open","line":null,"col":null,"placement":"endOfLine","slot":"root.items[0].Node.OpeningComment","path":"/0","doc":0},"""
            + """{"kind":"comment","text":" in","line":null,"col":null,"placement":"ownLine","slot":"root.items[0].Node.items[0].Before[0]","path":"/0","doc":0},"""
            + """{"kind":"comment","text":" end","line":null,"col":null,"placement":"ownLine","slot":"root.items[0].Node.End[0]","path":"/0","doc":0},"""
            + """{"kind":"comment","text":" close","line":null,"col":null,"placement":"endOfLine","slot":"root.items[0].Node.TrailingComment","path":"/0","doc":0}"""
        Expect.equal (triviaOf (single (YamlNode.sequence [ item ]))) ("\"trivia\":[" + trivia + "]}") "slots of an item"

    testCase "the end trivia of a block mapping and the header comment of a block scalar" <| fun _ ->
        let mapping = YamlNode.mapping [ field "k" literalScalar ] |> withEnd [ BlankLine None; FullLineComment (located " tail" 9 1) ]
        let withHeader = YamlNode.mapping [ field "k" (YamlNode.withHeaderComment (comment " h") literalScalar) ]
        let trivia =
            """{"kind":"blank","text":null,"line":null,"col":null,"placement":"ownLine","slot":"root.End[0]","path":"","doc":0},"""
            + """{"kind":"comment","text":" tail","line":9,"col":1,"placement":"ownLine","slot":"root.End[1]","path":"","doc":0}"""
        Expect.equal (triviaOf (single mapping)) ("\"trivia\":[" + trivia + "]}") "end of a block collection"
        let header = """{"kind":"comment","text":" h","line":null,"col":null,"placement":"endOfLine","slot":"root.entries[0].Value.HeaderComment","path":"/k","doc":0}"""
        Expect.equal (triviaOf (single withHeader)) ("\"trivia\":[" + header + "]}") "header comment"

    testCase "the slots of a document: directives, markers, and the trivia around the root" <| fun _ ->
        let directive (kind: DirectiveKind) (note: YamlComment option) : PreludeItem =
            Directive { Kind = kind; TrailingComment = note; Span = None }
        let marker (note: string) : DocumentMarker option = Some { TrailingComment = Some (comment note); Span = None }
        let root = { YamlNode.plain "r" with TrailingComment = Some (comment " root") }
        let doc: YamlDocument =
            { Prelude =
                [ directive (YamlVersion (1, 2)) (Some (comment " v"))
                  directive (TagDirective ("!e!", "tag:example.com,2000:")) None
                  PreludeTrivia (FullLineComment (comment " p"))
                  PreludeTrivia (BlankLine None)
                  directive (ReservedDirective ("FOO", [ "a"; "b" ])) None ]
              StartMarker = marker " s"
              BeforeRoot = [ FullLineComment (comment " br") ]
              Root = root
              AfterRoot = [ BlankLine None; FullLineComment (comment " ar") ]
              EndMarker = marker " e"
              Span = None }
        let text = Projection.presentation (streamOf [ StreamPart.Document doc ])
        let trivia =
            """{"kind":"comment","text":" v","line":null,"col":null,"placement":"endOfLine","slot":"Prelude[0].TrailingComment","path":"","doc":0},"""
            + """{"kind":"comment","text":" p","line":null,"col":null,"placement":"ownLine","slot":"Prelude[2]","path":"","doc":0},"""
            + """{"kind":"blank","text":null,"line":null,"col":null,"placement":"ownLine","slot":"Prelude[3]","path":"","doc":0},"""
            + """{"kind":"comment","text":" s","line":null,"col":null,"placement":"endOfLine","slot":"StartMarker.TrailingComment","path":"","doc":0},"""
            + """{"kind":"comment","text":" br","line":null,"col":null,"placement":"ownLine","slot":"BeforeRoot[0]","path":"","doc":0},"""
            + """{"kind":"comment","text":" root","line":null,"col":null,"placement":"endOfLine","slot":"root.TrailingComment","path":"","doc":0},"""
            + """{"kind":"blank","text":null,"line":null,"col":null,"placement":"ownLine","slot":"AfterRoot[0]","path":"","doc":0},"""
            + """{"kind":"comment","text":" ar","line":null,"col":null,"placement":"ownLine","slot":"AfterRoot[1]","path":"","doc":0},"""
            + """{"kind":"comment","text":" e","line":null,"col":null,"placement":"endOfLine","slot":"EndMarker.TrailingComment","path":"","doc":0}"""
        let expected =
            """{"docCount":1,"docs":[{"styles":[{"path":"","role":"value","style":"plain"}],"collections":[],"layouts":[],"""
            + """"directives":{"yaml":"1.2","tags":[["!e!","tag:example.com,2000:"]],"reserved":[["FOO",["a","b"]]]},"docStart":true,"docEnd":true}],"trivia":["""
            + trivia + "]}"
        Expect.equal text expected "document slots"

    testCase "the parts of a stream and the index of the document" <| fun _ ->
        let first = { YamlNode.plain "a" with TrailingComment = Some (comment " a1") }
        let second = { YamlNode.plain "b" with TrailingComment = Some (comment " b1") }
        let extra: DocumentMarker = { TrailingComment = Some (comment " x"); Span = None }
        let stream =
            streamOf
                [ StreamPart.Trivia (FullLineComment (comment " first"))
                  document first
                  StreamPart.Trivia (BlankLine None)
                  StreamPart.ExtraEndMarker extra
                  document second ]
        let trivia =
            """{"kind":"comment","text":" first","line":null,"col":null,"placement":"ownLine","slot":"Parts[0]","path":"","doc":null},"""
            + """{"kind":"comment","text":" a1","line":null,"col":null,"placement":"endOfLine","slot":"root.TrailingComment","path":"","doc":0},"""
            + """{"kind":"blank","text":null,"line":null,"col":null,"placement":"ownLine","slot":"Parts[2]","path":"","doc":null},"""
            + """{"kind":"comment","text":" x","line":null,"col":null,"placement":"endOfLine","slot":"Parts[3].TrailingComment","path":"","doc":null},"""
            + """{"kind":"comment","text":" b1","line":null,"col":null,"placement":"endOfLine","slot":"root.TrailingComment","path":"","doc":1}"""
        let text = Projection.presentation stream
        Expect.stringContains text "\"docCount\":2" "two documents"
        Expect.equal (text.Substring(text.IndexOf "\"trivia\":")) ("\"trivia\":[" + trivia + "]}") "trivia of the stream"

    testCase "an empty stream and an empty document" <| fun _ ->
        Expect.equal (Projection.presentation (streamOf [])) """{"docCount":0,"docs":[],"trivia":[]}""" "empty stream"
        Expect.equal
            (Projection.presentation (single YamlNode.empty))
            (presentationOf """{"path":"","role":"value","style":"plain"}""" "" "" "")
            "empty document"
]

let private deepLevels = 10000

let tests_deep = testList "Deep trees" [
    testCase "data of a sequence nested 10000 levels deep" <| fun _ ->
        let tree = nestedSequence deepLevels (YamlNode.plain "x")
        let opener = """{"k":"seq","tag":null,"anchor":null,"style":"block","items":["""
        let expected =
            envelope deepLevels (String.replicate deepLevels opener + strJson "x" + String.replicate deepLevels "]}")
        Expect.isTrue (Projection.data (single tree) = expected) "the data of the deep tree"

    testCase "data of a mapping nested 10000 levels deep" <| fun _ ->
        let tree = nestedMapping deepLevels (YamlNode.plain "x")
        let opener = """{"k":"map","tag":null,"anchor":null,"style":"block","entries":[[""" + strJson "k" + ","
        let expected =
            envelope deepLevels (String.replicate deepLevels opener + strJson "x" + String.replicate deepLevels "]]}")
        Expect.isTrue (Projection.data (single tree) = expected) "the data of the deep tree"

    testCase "the presentation of a deep tree stays small: a path deeper than 64 levels shows its depth and tail" <| fun _ ->
        let leaf = { YamlNode.plain "x" with TrailingComment = Some (located " deep" 10001 1) }
        let tree = nestedSequence deepLevels leaf
        let text = Projection.presentation (single tree)
        Expect.equal (countOf text "\"style\":\"block\"") deepLevels "one collection entry for each level"
        Expect.isTrue (text.StartsWith """{"docCount":1,"docs":[{"styles":[{"path":"(10000)/0/0/0/0/0/0/0/0","role":"value","style":"plain"}],"collections":[{"path":"","role":"value","style":"block"},{"path":"/0","role":"value","style":"block"},""") "The root and its first child come first"
        // The collection at depth 64 has its full path, the one at depth 65 shows the depth.
        Expect.stringContains text ("{\"path\":\"" + String.replicate 64 "/0" + "\",\"role\":\"value\",\"style\":\"block\"}") "depth 64"
        Expect.stringContains text "{\"path\":\"(65)/0/0/0/0/0/0/0/0\",\"role\":\"value\",\"style\":\"block\"}" "depth 65"
        Expect.stringContains text "{\"path\":\"(9999)/0/0/0/0/0/0/0/0\",\"role\":\"value\",\"style\":\"block\"}" "the innermost collection"
        let slot = "root(10000)" + String.replicate 8 ".items[0].Node" + ".TrailingComment"
        let item = "{\"kind\":\"comment\",\"text\":\" deep\",\"line\":10001,\"col\":1,\"placement\":\"endOfLine\",\"slot\":\"" + slot + "\",\"path\":\"(10000)/0/0/0/0/0/0/0/0\",\"doc\":0}"
        Expect.isTrue (text.EndsWith ("\"trivia\":[" + item + "]}")) "the comment at the bottom"
        Expect.isTrue (text.Length < 100 * deepLevels) "the output is linear in the size of the tree"

    testCase "a deep tree with a comment on every level stays linear" <| fun _ ->
        let mutable node = YamlNode.plain "x"
        for level in 1 .. deepLevels do
            node <- YamlNode.withOpeningComment (comment (" c" + string level)) (YamlNode.flowSequence [ YamlNode.item node ])
        let text = Projection.presentation (single node)
        Expect.equal (countOf text "\"kind\":\"comment\"") deepLevels "every comment is listed"
        Expect.isTrue (text.Length < 500 * deepLevels) "the output is linear in the size of the tree"

    testCase "the corpus line of a deep tree is one line" <| fun _ ->
        let line = Projection.line "s" "deep" "h" (Projection.Outcome.Accepted (single (nestedSequence deepLevels (YamlNode.plain "x"))))
        Expect.isTrue (line.StartsWith """{"set":"s","id":"deep","sha256":"h","ok":true,"docCount":1,"maxDepth":10000,"docs":[""") "start"
        Expect.stringContains line ",\"presentation\":{\"docCount\":1,\"docs\":[" "presentation"
        Expect.equal (countOf line "\n") 0 "one line"
]

let tests_failure = testList "Failure projection and corpus lines" [
    testCase "the errors of a rejected input" <| fun _ ->
        Expect.equal (Projection.failure [ "UNCLOSED_QUOTE", 3 ]) """{"ok":false,"errors":[{"code":"UNCLOSED_QUOTE","line":3}]}""" "one error"
        Expect.equal
            (Projection.failure [ "BAD_INDENT", 2; "UNEXPECTED_TOKEN", 0 ])
            """{"ok":false,"errors":[{"code":"BAD_INDENT","line":2},{"code":"UNEXPECTED_TOKEN","line":0}]}"""
            "two errors, a line of 0 names no line"
        Expect.equal (Projection.failure []) """{"ok":false,"errors":[]}""" "no error"

    testCase "the line of an accepted input holds the data and the presentation" <| fun _ ->
        let expected =
            """{"set":"s","id":"i","sha256":"h","ok":true,"docCount":1,"maxDepth":0,"docs":[{"data":"""
            + strJson "x"
            + ""","maxDepth":0}],"presentation":"""
            + presentationOf """{"path":"","role":"value","style":"plain"}""" "" "" ""
            + "}"
        Expect.equal (Projection.line "s" "i" "h" (Projection.Outcome.Accepted (single (YamlNode.plain "x")))) expected "accepted"

    testCase "the line of a rejected input holds the errors" <| fun _ ->
        Expect.equal
            (Projection.line "s\"q" "i/d" "h" (Projection.Outcome.Rejected [ "BAD_INDENT", 2 ]))
            """{"set":"s\"q","id":"i/d","sha256":"h","ok":false,"errors":[{"code":"BAD_INDENT","line":2}]}"""
            "rejected, with an id that needs escaping"
]

let Main = testList "Projection" [
    tests_data
    tests_presentation
    tests_comments
    tests_deep
    tests_failure
]
