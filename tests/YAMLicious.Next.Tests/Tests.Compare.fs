module Tests.Compare

open Fable.Pyxpecto
open YAMLicious

let private field (key: string) (value: YamlNode) = YamlNode.entry (YamlNode.plain key) value

let private someSpan: SourceSpan =
    { Start = { Line = 3; Column = 2 }
      Finish = { Line = 3; Column = 9 } }

let private otherSpan: SourceSpan =
    { Start = { Line = 8; Column = 1 }
      Finish = { Line = 9; Column = 1 } }

/// The names of the slots that the template below can fill with a comment, one at a time.
let private slots =
    [ "entry-before"; "entry-question"; "entry-key-trailing"; "entry-before-colon"; "entry-colon"
      "entry-value-trailing"; "colon-following"; "blank"; "item-before"; "item-dash"; "item-trailing"
      "sequence-end"; "mapping-end"; "tag-after"; "anchor-after"; "header"; "flow-opening"
      "flow-closing"; "flow-end"; "alias-trailing" ]

/// A tree with one block mapping that reaches every place a comment can stand. All templates have the same
/// shape and differ only in the slot that holds the comment, so a comparison that confuses two slots fails.
/// With `spans` the template carries spans on every node, entry, item, comment and property placement.
let private templateWith (text: string) (spans: bool) (slot: string) : YamlNode =
    let span = if spans then Some someSpan else None
    let note: YamlComment = { Text = text; Span = span }
    let at name = if slot = name then Some note else None
    let trivia name = if slot = name then [ FullLineComment note ] else []
    let separation name =
        if slot = name then
            Separation.NewLine { Comment = Some note; Following = [] }
        else
            Separation.Inline
    let noProperties: NodeProperties = { Tag = None; Anchor = None }
    let make (kind: YamlNodeKind) (properties: NodeProperties) (layout: PropertyPlacement list) (trailing: YamlComment option) : YamlNode =
        { Kind = kind
          Properties = properties
          PropertyLayout = layout
          TrailingComment = trailing
          Span = span }
    let scalarKind (style: ScalarStyle) (text: string) (header: YamlComment option) : YamlNodeKind =
        YamlNodeKind.Scalar
            { Value = text
              Style = style
              HeaderComment = header }
    let plainNode (text: string) (trailing: YamlComment option) =
        make (scalarKind ScalarStyle.Plain text None) noProperties [] trailing
    let placement (property: PropertyName) (after: Separation) : PropertyPlacement =
        { Property = property
          After = after
          Span = span }
    let firstEntry: MappingEntry =
        { Before = trivia "entry-before"
          Question = Some (separation "entry-question")
          Key = plainNode "k1" (at "entry-key-trailing")
          BeforeColon = trivia "entry-before-colon"
          Colon = Some (separation "entry-colon")
          Value = plainNode "v1" (at "entry-value-trailing")
          Span = span }
    let tagged =
        make
            (scalarKind ScalarStyle.Plain "t" None)
            { Tag = Some (YamlTag.core "str"); Anchor = Some "a" }
            [ placement PropertyName.Tag (separation "tag-after"); placement PropertyName.Anchor (separation "anchor-after") ]
            (at "item-trailing")
    let blockScalar =
        make (scalarKind (ScalarStyle.Block (BlockScalarStyle.Literal, ChompingMode.Clip, None)) "text\n" (at "header")) noProperties [] None
    let flowItem: SequenceItem =
        { Before = []
          Dash = None
          Node = plainNode "f" None
          Span = span }
    let flow =
        make
            (YamlNodeKind.Sequence
                { Style = CollectionStyle.Flow
                  OpeningComment = at "flow-opening"
                  Items = [ flowItem ]
                  End = trivia "flow-end" })
            noProperties
            []
            (at "flow-closing")
    let alias = make (YamlNodeKind.Alias "x") noProperties [] (at "alias-trailing")
    let items: SequenceItem list =
        [ { Before = trivia "item-before"
            Dash = Some (separation "item-dash")
            Node = tagged
            Span = span }
          { Before = []
            Dash = Some Separation.Inline
            Node = blockScalar
            Span = span }
          { Before = []
            Dash = Some Separation.Inline
            Node = flow
            Span = span }
          { Before = []
            Dash = Some Separation.Inline
            Node = alias
            Span = span } ]
    let sequence =
        make
            (YamlNodeKind.Sequence
                { Style = CollectionStyle.Block
                  OpeningComment = None
                  Items = items
                  End = trivia "sequence-end" })
            noProperties
            []
            None
    let secondColon =
        if slot = "colon-following" then
            Separation.NewLine { Comment = None; Following = [ FullLineComment note ] }
        else
            Separation.Inline
    let secondEntry: MappingEntry =
        { Before = if slot = "blank" then [ BlankLine span ] else []
          Question = None
          Key = plainNode "k2" None
          BeforeColon = []
          Colon = Some secondColon
          Value = sequence
          Span = span }
    make
        (YamlNodeKind.Mapping
            { Style = CollectionStyle.Block
              OpeningComment = None
              Items = [ firstEntry; secondEntry ]
              End = trivia "mapping-end" })
        noProperties
        []
        None

let private template = templateWith " c"

let tests_spans = testList "Equality ignores spans" [
    testCase "a tree equals a rebuilt copy of itself" <| fun _ ->
        for slot in "none" :: slots do
            Expect.nodeEqual (template false slot) (template false slot) $"template {slot}"

    testCase "a tree with spans equals the same tree without spans" <| fun _ ->
        for slot in "none" :: slots do
            Expect.nodeEqual (template true slot) (template false slot) $"template {slot}, spans on the left"
            Expect.nodeEqual (template false slot) (template true slot) $"template {slot}, spans on the right"

    testCase "two different spans are equal" <| fun _ ->
        let a = { YamlNode.plain "a" with Span = Some someSpan }
        let b = { YamlNode.plain "a" with Span = Some otherSpan }
        Expect.nodeEqual a b "node spans"
        let commentA: YamlComment = { Text = " c"; Span = Some someSpan }
        let commentB: YamlComment = { Text = " c"; Span = Some otherSpan }
        Expect.nodeEqual { a with TrailingComment = Some commentA } { b with TrailingComment = Some commentB } "comment spans"

    testCase "the text of a comment counts" <| fun _ ->
        let a = YamlNode.plain "a" |> YamlNode.withTrailingComment (comment " one")
        let b = YamlNode.plain "a" |> YamlNode.withTrailingComment (comment " two")
        Expect.nodeNotEqual a b "text"
]

let tests_slots = testList "Equality sees every comment slot" [
    testCase "a comment in any slot makes a tree differ from the tree without it" <| fun _ ->
        let bare = template false "none"
        for slot in slots do
            Expect.nodeNotEqual (template false slot) bare $"slot {slot} against no comment"

    testCase "a comment in one slot differs from the same comment in every other slot" <| fun _ ->
        let trees = slots |> List.map (fun slot -> slot, template false slot)
        for (slotA, treeA) in trees do
            for (slotB, treeB) in trees do
                if slotA <> slotB then
                    Expect.nodeNotEqual treeA treeB $"slot {slotA} against slot {slotB}"

    testCase "the text of a comment counts in every slot" <| fun _ ->
        // The blank slot holds no comment.
        for slot in slots |> List.filter (fun slot -> slot <> "blank") do
            Expect.nodeNotEqual (templateWith " c" false slot) (templateWith " d" false slot) $"slot {slot}"
            Expect.nodeEqual (templateWith " d" false slot) (templateWith " d" true slot) $"slot {slot} with spans"

    testCase "a blank line is not a comment" <| fun _ ->
        Expect.nodeNotEqual (template false "blank") (template false "entry-before") "blank against comment"
        Expect.nodeNotEqual (template false "blank") (template false "none") "blank against nothing"
]

let tests_other_fields = testList "Equality sees the other fields" [
    testCase "the value, the style and the kind of a scalar count" <| fun _ ->
        Expect.nodeNotEqual (YamlNode.plain "a") (YamlNode.plain "b") "value"
        Expect.nodeNotEqual (YamlNode.plain "a") (YamlNode.scalar ScalarStyle.DoubleQuoted "a") "plain against double-quoted"
        Expect.nodeNotEqual (YamlNode.scalar ScalarStyle.SingleQuoted "a") (YamlNode.scalar ScalarStyle.DoubleQuoted "a") "single against double"
        Expect.nodeNotEqual (YamlNode.plain "a") (YamlNode.alias "a") "scalar against alias"
        Expect.nodeNotEqual (YamlNode.plain "a") (YamlNode.mapping []) "scalar against mapping"
        Expect.nodeNotEqual (YamlNode.mapping []) (YamlNode.sequence []) "mapping against sequence"

    testCase "the block scalar header counts" <| fun _ ->
        let block (style: BlockScalarStyle) (chomping: ChompingMode) (indentation: int option) =
            YamlNode.scalar (ScalarStyle.Block (style, chomping, indentation)) "a\n"
        let reference = block BlockScalarStyle.Literal ChompingMode.Clip None
        Expect.nodeEqual reference (block BlockScalarStyle.Literal ChompingMode.Clip None) "same header"
        Expect.nodeNotEqual reference (block BlockScalarStyle.Folded ChompingMode.Clip None) "folded"
        Expect.nodeNotEqual reference (block BlockScalarStyle.Literal ChompingMode.Keep None) "keep"
        Expect.nodeNotEqual reference (block BlockScalarStyle.Literal ChompingMode.Clip (Some 2)) "indentation indicator"

    testCase "the name of an alias counts" <| fun _ ->
        Expect.nodeEqual (YamlNode.alias "a") (YamlNode.alias "a") "same name"
        Expect.nodeNotEqual (YamlNode.alias "a") (YamlNode.alias "b") "other name"

    testCase "the tag, the anchor and the layout of the properties count" <| fun _ ->
        let bare = YamlNode.plain "a"
        Expect.nodeNotEqual bare (YamlNode.withTag (YamlTag.core "str") bare) "tag against none"
        Expect.nodeNotEqual (YamlNode.withTag (YamlTag.core "str") bare) (YamlNode.withTag (YamlTag.core "int") bare) "core tags"
        Expect.nodeNotEqual (YamlNode.withTag (YamlTag.local "str") bare) (YamlNode.withTag (YamlTag.core "str") bare) "local against core"
        Expect.nodeNotEqual (YamlNode.withTag YamlTag.NonSpecific bare) (YamlNode.withTag (YamlTag.local "x") bare) "non-specific against local"
        Expect.nodeNotEqual bare (YamlNode.withAnchor "x" bare) "anchor against none"
        Expect.nodeNotEqual (YamlNode.withAnchor "x" bare) (YamlNode.withAnchor "y" bare) "anchor names"
        let anchored = YamlNode.withAnchor "x" bare
        let onOwnLine = YamlNode.withPropertyComment PropertyName.Anchor (comment " c") anchored
        Expect.nodeNotEqual anchored onOwnLine "comment behind the anchor"
        let both = YamlNode.withTag (YamlTag.core "str") anchored
        let anchorFirst = { both with PropertyLayout = List.rev both.PropertyLayout }
        Expect.nodeNotEqual both anchorFirst "order of the placements"

    // A %TAG directive can bind the same identifier through another handle. The tree compares the identifier.
    testCase "a tag is the same tag whatever the split of its identifier" <| fun _ ->
        let whole = YamlTag.Named { Prefix = "tag:yaml.org,2002:"; Suffix = "str" }
        let split = YamlTag.Named { Prefix = "tag:yaml.org,"; Suffix = "2002:str" }
        Expect.nodeEqual (YamlNode.withTag whole (YamlNode.plain "a")) (YamlNode.withTag split (YamlNode.plain "a")) "same identifier"

    testCase "the order of the entries and the number of the entries count" <| fun _ ->
        let ab = YamlNode.mapping [ field "a" YamlNode.empty; field "b" YamlNode.empty ]
        let ba = YamlNode.mapping [ field "b" YamlNode.empty; field "a" YamlNode.empty ]
        let a = YamlNode.mapping [ field "a" YamlNode.empty ]
        Expect.nodeEqual ab (YamlNode.mapping [ field "a" YamlNode.empty; field "b" YamlNode.empty ]) "same entries"
        Expect.nodeNotEqual ab ba "order"
        Expect.nodeNotEqual ab a "fewer entries"
        Expect.nodeNotEqual a ab "more entries"

    testCase "the key and the value of an entry are not interchangeable" <| fun _ ->
        let kv = YamlNode.mapping [ YamlNode.entry (YamlNode.plain "a") (YamlNode.plain "b") ]
        let vk = YamlNode.mapping [ YamlNode.entry (YamlNode.plain "b") (YamlNode.plain "a") ]
        Expect.nodeNotEqual kv vk "swapped"

    testCase "duplicate keys count as two entries" <| fun _ ->
        let one = YamlNode.mapping [ field "a" (YamlNode.plain "1") ]
        let two = YamlNode.mapping [ field "a" (YamlNode.plain "1"); field "a" (YamlNode.plain "1") ]
        Expect.nodeNotEqual one two "duplicate"

    testCase "the style of a collection counts" <| fun _ ->
        let items = [ YamlNode.item (YamlNode.plain "1") ]
        Expect.nodeNotEqual (YamlNode.sequence items) (YamlNode.flowSequence items) "block against flow sequence"
        let entries = [ field "a" (YamlNode.plain "1") ]
        Expect.nodeNotEqual (YamlNode.mapping entries) (YamlNode.flowMapping entries) "block against flow mapping"

    testCase "the colon, the question mark and the dash of the items count" <| fun _ ->
        let plainEntry = field "a" (YamlNode.plain "1")
        let omitted = { plainEntry with Colon = None }
        Expect.nodeNotEqual (YamlNode.mapping [ plainEntry ]) (YamlNode.mapping [ omitted ]) "omitted colon"
        let explicitKey = { plainEntry with Question = Some Separation.Inline }
        Expect.nodeNotEqual (YamlNode.mapping [ plainEntry ]) (YamlNode.mapping [ explicitKey ]) "explicit key"
        let plainItem = YamlNode.item (YamlNode.plain "1")
        let blockSequenceOf (item: SequenceItem) =
            { YamlNode.sequence [ plainItem ] with
                Kind =
                    YamlNodeKind.Sequence
                        { Style = CollectionStyle.Block
                          OpeningComment = None
                          Items = [ item ]
                          End = [] } }
        let noDash = { plainItem with Dash = None }
        Expect.nodeNotEqual (blockSequenceOf plainItem) (blockSequenceOf noDash) "no dash"

    testCase "a difference deep in the tree is found" <| fun _ ->
        let build (leaf: string) =
            YamlNode.mapping [ field "a" (YamlNode.sequence [ YamlNode.item (YamlNode.mapping [ field "b" (YamlNode.plain leaf) ]) ]) ]
        Expect.nodeEqual (build "x") (build "x") "same leaf"
        Expect.nodeNotEqual (build "x") (build "y") "other leaf"
]

let private versionDirective (comment: YamlComment option) (span: SourceSpan option) : PreludeItem =
    Directive
        { Kind = YamlVersion (1, 2)
          TrailingComment = comment
          Span = span }

let private marker (comment: YamlComment option) (span: SourceSpan option) : DocumentMarker option =
    Some
        { TrailingComment = comment
          Span = span }

let tests_documents = testList "Document equality" [
    testCase "documents differ in each field and equal their own copy" <| fun _ ->
        let root = YamlNode.mapping [ field "k" (YamlNode.plain "v") ]
        let bare = YamlDocument.create root
        let variants: (string * YamlDocument) list =
            [ "directive", { bare with Prelude = [ versionDirective None None ] }
              "directive comment", { bare with Prelude = [ versionDirective (Some (comment " c")) None ] }
              "other version", { bare with Prelude = [ Directive { Kind = YamlVersion (1, 1); TrailingComment = None; Span = None } ] }
              "tag directive", { bare with Prelude = [ Directive { Kind = TagDirective ("!e!", "tag:example.com,2000:"); TrailingComment = None; Span = None } ] }
              "reserved directive", { bare with Prelude = [ Directive { Kind = ReservedDirective ("FOO", [ "bar" ]); TrailingComment = None; Span = None } ] }
              "prelude comment", { bare with Prelude = [ PreludeTrivia (FullLineComment (comment " c")) ] }
              "prelude blank line", { bare with Prelude = [ PreludeTrivia (BlankLine None) ] }
              "start marker", { bare with StartMarker = marker None None }
              "start marker comment", { bare with StartMarker = marker (Some (comment " c")) None }
              "before root", { bare with BeforeRoot = [ FullLineComment (comment " c") ] }
              "after root", { bare with AfterRoot = [ FullLineComment (comment " c") ] }
              "end marker", { bare with EndMarker = marker None None }
              "end marker comment", { bare with EndMarker = marker (Some (comment " c")) None }
              "other root", YamlDocument.create (YamlNode.plain "other") ]
        Expect.isTrue (YamlDocument.equalIgnoringSpans bare (YamlDocument.create root)) "The bare document equals its copy"
        for (name, variant) in variants do
            Expect.isFalse (YamlDocument.equalIgnoringSpans bare variant) $"{name} against the bare document"
            Expect.isTrue (YamlDocument.equalIgnoringSpans variant variant) $"{name} against itself"
        for (nameA, a) in variants do
            for (nameB, b) in variants do
                if nameA <> nameB then
                    Expect.isFalse (YamlDocument.equalIgnoringSpans a b) $"{nameA} against {nameB}"

    testCase "the spans of a document, its directives and its markers are ignored" <| fun _ ->
        let root = YamlNode.plain "a"
        let build (span: SourceSpan option) : YamlDocument =
            { Prelude = [ versionDirective (Some (comment " d")) span ]
              StartMarker = marker None span
              BeforeRoot = []
              Root = { root with Span = span }
              AfterRoot = []
              EndMarker = marker (Some (comment " e")) span
              Span = span }
        Expect.isTrue (YamlDocument.equalIgnoringSpans (build (Some someSpan)) (build None)) "with and without spans"
        Expect.isTrue (YamlDocument.equalIgnoringSpans (build (Some someSpan)) (build (Some otherSpan))) "other spans"
]

let tests_show = testList "show" [
    testCase "a scalar is one line" <| fun _ ->
        Expect.equal (YamlNode.show (YamlNode.plain "a")) "root: scalar plain \"a\"\n" "plain"
        Expect.equal (YamlNode.show (YamlNode.scalar ScalarStyle.DoubleQuoted "a b")) "root: scalar double \"a b\"\n" "double-quoted"
        Expect.equal (YamlNode.show (YamlNode.scalar ScalarStyle.SingleQuoted "")) "root: scalar single \"\"\n" "single-quoted"
        Expect.equal (YamlNode.show YamlNode.empty) "root: scalar plain \"\"\n" "empty"

    testCase "the text of a scalar is escaped" <| fun _ ->
        Expect.equal (YamlNode.show (YamlNode.plain "a\tb\"c\\d\r\ne")) "root: scalar plain \"a\\tb\\\"c\\\\d\\r\\ne\"\n" "escapes"

    testCase "a mapping shows its entries with their keys and values" <| fun _ ->
        let value =
            YamlNode.plain "v"
            |> YamlNode.withTag (YamlTag.core "str")
            |> YamlNode.withAnchor "a"
            |> YamlNode.withTrailingComment (comment " c")
        let expected =
            "root: mapping block\n"
            + "  entry\n"
            + "    key: scalar plain \"k\"\n"
            + "    value: scalar plain \"v\" tag=tag:yaml.org,2002:str anchor=&a layout=[tag:inline; anchor:inline] comment=#\" c\"\n"
        Expect.equal (YamlNode.show (YamlNode.mapping [ field "k" value ])) expected "mapping"

    testCase "a sequence shows its items" <| fun _ ->
        let expected =
            "root: sequence block\n"
            + "  item\n"
            + "    node: scalar plain \"a\"\n"
            + "  item\n"
            + "    node: alias *x\n"
        let sequence = YamlNode.sequence [ YamlNode.item (YamlNode.plain "a"); YamlNode.item (YamlNode.alias "x") ]
        Expect.equal (YamlNode.show sequence) expected "sequence"

    testCase "a flow collection shows its opening comment and its end trivia" <| fun _ ->
        let flow = YamlNode.flowSequence [ YamlNode.item (YamlNode.plain "1") ] |> YamlNode.withOpeningComment (comment " o")
        let content = (YamlNode.trySequence flow).Value
        let ended = { flow with Kind = YamlNodeKind.Sequence { content with End = [ BlankLine None; FullLineComment (comment " z") ] } }
        let expected =
            "root: sequence flow opening=#\" o\"\n"
            + "  item\n"
            + "    node: scalar plain \"1\"\n"
            + "  end=[blank; #\" z\"]\n"
        Expect.equal (YamlNode.show ended) expected "flow sequence"

    testCase "the layout of the properties and the header of a block scalar show" <| fun _ ->
        let anchored = YamlNode.plain "v" |> YamlNode.withAnchor "a" |> YamlNode.withPropertyComment PropertyName.Anchor (comment " c")
        Expect.equal (YamlNode.show anchored) "root: scalar plain \"v\" anchor=&a layout=[anchor:newline(#\" c\" [])]\n" "layout"
        let block =
            YamlNode.scalar (ScalarStyle.Block (BlockScalarStyle.Literal, ChompingMode.Keep, Some 2)) "a\n"
            |> YamlNode.withHeaderComment (comment " h")
        Expect.equal (YamlNode.show block) "root: scalar block(|2+) \"a\\n\" header=#\" h\"\n" "block scalar"
        let folded = YamlNode.scalar (ScalarStyle.Block (BlockScalarStyle.Folded, ChompingMode.Strip, None)) "a"
        Expect.equal (YamlNode.show folded) "root: scalar block(>-) \"a\"\n" "folded"

    testCase "an entry shows its question mark, its trivia and its colon" <| fun _ ->
        let lineBreak = Separation.NewLine { Comment = Some (comment " c"); Following = [ BlankLine None ] }
        let made =
            { field "k" (YamlNode.plain "v") with
                Before = [ FullLineComment (comment " b") ]
                Question = Some Separation.Inline
                BeforeColon = [ BlankLine None ]
                Colon = Some lineBreak }
        let expected =
            "root: mapping block\n"
            + "  entry before=[#\" b\"] question=inline before-colon=[blank] colon=newline(#\" c\" [blank])\n"
            + "    key: scalar plain \"k\"\n"
            + "    value: scalar plain \"v\"\n"
        Expect.equal (YamlNode.show (YamlNode.mapping [ made ])) expected "entry"
        let omitted = { field "k" YamlNode.empty with Colon = None }
        let shown = YamlNode.show (YamlNode.mapping [ omitted ])
        Expect.stringContains shown "entry colon=omitted\n" "omitted colon"

    testCase "the printout has no spans" <| fun _ ->
        let withSpans = { YamlNode.plain "a" with Span = Some someSpan }
        Expect.equal (YamlNode.show withSpans) (YamlNode.show (YamlNode.plain "a")) "spans stay out"
        Expect.equal (YamlNode.show (template true "none")) (YamlNode.show (template false "none")) "template"

    testCase "a document shows its prelude, its markers and its trivia" <| fun _ ->
        let document: YamlDocument =
            { Prelude = [ versionDirective (Some (comment " d")) None; PreludeTrivia (FullLineComment (comment " p")) ]
              StartMarker = marker None None
              BeforeRoot = [ BlankLine None ]
              Root = YamlNode.plain "a"
              AfterRoot = [ FullLineComment (comment " z") ]
              EndMarker = marker (Some (comment " e")) None
              Span = None }
        let expected =
            "document\n"
            + "  prelude: directive version 1.2 comment=#\" d\"\n"
            + "  prelude: #\" p\"\n"
            + "  start-marker\n"
            + "  before-root=[blank]\n"
            + "  root: scalar plain \"a\"\n"
            + "  after-root=[#\" z\"]\n"
            + "  end-marker comment=#\" e\"\n"
        Expect.equal (YamlDocument.show document) expected "document"

    testCase "anchor, alias and tag names are escaped like the text of a scalar" <| fun _ ->
        Expect.equal (YamlNode.show (YamlNode.alias "a\"b\\c")) "root: alias *a\\\"b\\\\c\n" "alias"
        let anchored = YamlNode.plain "v" |> YamlNode.withAnchor "a\"b"
        Expect.equal (YamlNode.show anchored) "root: scalar plain \"v\" anchor=&a\\\"b layout=[anchor:inline]\n" "anchor"
        let tagged = YamlNode.plain "v" |> YamlNode.withTag (YamlTag.local "t\"x")
        Expect.equal (YamlNode.show tagged) "root: scalar plain \"v\" tag=!t\\\"x layout=[tag:inline]\n" "tag"

    testCase "the failure message of nodeEqual names both trees" <| fun _ ->
        let message = errorOf (fun () -> Expect.nodeEqual (YamlNode.plain "a") (YamlNode.plain "b") "differ")
        Expect.stringContains message "differ" "message"
        Expect.stringContains message "scalar plain \"a\"" "actual"
        Expect.stringContains message "scalar plain \"b\"" "expected"
]

/// The depth of the trees in the stress tests. A recursive walk overflows the stack long before this depth
/// on .NET (1 MB main thread), on Node and on Python (limit 1000 frames).
let private depth = 10000

/// A sequence nested `levels` deep around one scalar, built from the inside out without recursion.
let private nestedSequence (levels: int) (leaf: string) : YamlNode =
    let mutable node = YamlNode.plain leaf
    for _ in 1 .. levels do
        node <- YamlNode.sequence [ YamlNode.item node ]
    node

/// A mapping nested `levels` deep as the value of its only entry, around one scalar.
let private nestedMapping (levels: int) (leaf: string) : YamlNode =
    let mutable node = YamlNode.plain leaf
    for _ in 1 .. levels do
        node <- YamlNode.mapping [ field "k" node ]
    node

/// Alternating mappings and sequences, with a span on every node, nested `levels` deep.
let private nestedMixed (levels: int) (leaf: string) : YamlNode =
    let mutable node = YamlNode.plain leaf
    for level in 1 .. levels do
        let wrapped =
            if level % 2 = 0 then
                YamlNode.mapping [ field "k" node ]
            else
                YamlNode.sequence [ YamlNode.item node ]
        node <- { wrapped with Span = Some someSpan }
    node

let private countLines (text: string) = text.Split('\n').Length - 1

let tests_depth = testList "Deep trees" [
    testCase "equality of two sequences nested 10000 levels deep" <| fun _ ->
        let a = nestedSequence depth "x"
        let b = nestedSequence depth "x"
        let c = nestedSequence depth "y"
        Expect.nodeEqual a b "equal trees"
        Expect.nodeNotEqual a c "The difference sits at the deepest level"

    testCase "equality of two mappings nested 10000 levels deep" <| fun _ ->
        let a = nestedMapping depth "x"
        Expect.nodeEqual a (nestedMapping depth "x") "equal trees"
        Expect.nodeNotEqual a (nestedMapping depth "y") "The difference sits at the deepest level"
        Expect.nodeNotEqual a (nestedMapping (depth - 1) "x") "A tree one level lower"

    testCase "equality of mixed trees ignores spans at every level" <| fun _ ->
        let a = nestedMixed depth "x"
        let unspanned = nestedMixed 3 "x"
        Expect.nodeEqual a (nestedMixed depth "x") "equal trees"
        Expect.nodeEqual unspanned { unspanned with Span = None } "spans"
        Expect.nodeNotEqual a (nestedMixed depth "y") "The difference sits at the deepest level"

    testCase "equality of a tree and a deep tree of another shape" <| fun _ ->
        Expect.nodeNotEqual (nestedSequence depth "x") (nestedMapping depth "x") "sequence against mapping"

    testCase "show prints a tree nested 10000 levels deep, one line per node, entry and item" <| fun _ ->
        let shown = YamlNode.show (nestedSequence depth "x")
        // The root, then an item line and a node line for each level.
        Expect.equal (countLines shown) (1 + 2 * depth) "line count"
        Expect.stringContains shown "[20000] node: scalar plain \"x\"\n" "The deepest line names its depth"
        Expect.isTrue (shown.StartsWith "root: sequence block\n  item\n    node: sequence block\n") "The first lines are indented"

    testCase "show of a mapping nested 10000 levels deep" <| fun _ ->
        let shown = YamlNode.show (nestedMapping depth "x")
        // The root, then an entry line, a key line and a value line for each level.
        Expect.equal (countLines shown) (1 + 3 * depth) "line count"
        Expect.stringContains shown "scalar plain \"x\"\n" "The leaf is printed"

    testCase "the printout of a deep tree stays linear in its size" <| fun _ ->
        // A line of the printout is at most about 100 characters. An indentation that grew with the depth would
        // print about 400 million characters for this tree.
        let shown = YamlNode.show (nestedSequence depth "x")
        Expect.isTrue (shown.Length < 100 * (1 + 2 * depth)) "The printout has a bounded line length"

    testCase "a deep document compares and prints" <| fun _ ->
        let a = YamlDocument.create (nestedSequence depth "x")
        let b = YamlDocument.create (nestedSequence depth "x")
        Expect.isTrue (YamlDocument.equalIgnoringSpans a b) "equal documents"
        Expect.equal (countLines (YamlDocument.show a)) (2 + 2 * depth) "document line count"

    testCase "a flat collection with many entries compares" <| fun _ ->
        let build (last: string) =
            YamlNode.mapping [ for i in 1 .. depth -> field (string i) (YamlNode.plain (if i = depth then last else "v")) ]
        Expect.nodeEqual (build "x") (build "x") "equal"
        Expect.nodeNotEqual (build "x") (build "y") "The last value differs"
]

let private markerOf (comment: YamlComment option) (span: SourceSpan option) : DocumentMarker =
    { TrailingComment = comment
      Span = span }

let tests_streams = testList "Stream equality and show" [
    testCase "streams differ in the kind, the order and the number of their parts" <| fun _ ->
        let document (text: string) = StreamPart.Document (YamlDocument.create (YamlNode.plain text))
        let parts: (string * StreamPart list) list =
            [ "none", []
              "one document", [ document "a" ]
              "other document", [ document "b" ]
              "two documents", [ document "a"; document "a" ]
              "trivia", [ StreamPart.Trivia (BlankLine None) ]
              "comment", [ StreamPart.Trivia (FullLineComment (comment " c")) ]
              "other comment", [ StreamPart.Trivia (FullLineComment (comment " d")) ]
              "extra end marker", [ StreamPart.ExtraEndMarker (markerOf None None) ]
              "extra end marker with comment", [ StreamPart.ExtraEndMarker (markerOf (Some (comment " c")) None) ]
              "document then trivia", [ document "a"; StreamPart.Trivia (BlankLine None) ]
              "trivia then document", [ StreamPart.Trivia (BlankLine None); document "a" ] ]
        for (nameA, a) in parts do
            for (nameB, b) in parts do
                let same = YamlStream.equalIgnoringSpans { Parts = a } { Parts = b }
                Expect.equal same (nameA = nameB) $"{nameA} against {nameB}"

    testCase "the spans of the parts are ignored" <| fun _ ->
        let build (span: SourceSpan option) : YamlStream =
            { Parts =
                [ StreamPart.Document { YamlDocument.create (YamlNode.plain "a") with Span = span }
                  StreamPart.Trivia (BlankLine span)
                  StreamPart.Trivia (FullLineComment { Text = " c"; Span = span })
                  StreamPart.ExtraEndMarker (markerOf None span) ] }
        Expect.isTrue (YamlStream.equalIgnoringSpans (build (Some someSpan)) (build None)) "with and without spans"
        Expect.isTrue (YamlStream.equalIgnoringSpans (build (Some someSpan)) (build (Some otherSpan))) "other spans"

    testCase "a stream with many parts compares and prints" <| fun _ ->
        let build (last: string) : YamlStream =
            { Parts = [ for i in 1 .. 2000 -> StreamPart.Document (YamlDocument.create (YamlNode.plain (if i = 2000 then last else "v"))) ] }
        Expect.isTrue (YamlStream.equalIgnoringSpans (build "x") (build "x")) "equal"
        Expect.isFalse (YamlStream.equalIgnoringSpans (build "x") (build "y")) "The last document differs"
        Expect.equal (countLines (YamlStream.show (build "x"))) (1 + 2 * 2000) "line count"

    testCase "show prints the parts in order" <| fun _ ->
        let stream: YamlStream =
            { Parts =
                [ StreamPart.Document (YamlDocument.create (YamlNode.plain "a"))
                  StreamPart.ExtraEndMarker (markerOf (Some (comment " e")) None)
                  StreamPart.Trivia (BlankLine None)
                  StreamPart.Trivia (FullLineComment (comment " t")) ] }
        let expected =
            "stream\n"
            + "document\n"
            + "  root: scalar plain \"a\"\n"
            + "extra-end-marker comment=#\" e\"\n"
            + "trivia: blank\n"
            + "trivia: #\" t\"\n"
        Expect.equal (YamlStream.show stream) expected "stream"
        Expect.equal (YamlStream.show { Parts = [] }) "stream\n" "An empty stream"
]

let Main = testList "Compare" [
    tests_spans
    tests_slots
    tests_other_fields
    tests_documents
    tests_streams
    tests_show
    tests_depth
]
