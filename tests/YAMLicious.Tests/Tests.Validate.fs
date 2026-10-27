module Tests.Validate

open Fable.Pyxpecto
open YAMLicious

let private field (key: string) (value: YamlNode) = YamlNode.entry (YamlNode.plain key) value

let private number (text: string) = YamlNode.plain text

let private lineComment (text: string) : Trivia = FullLineComment (comment text)

/// A line break behind an indicator with a comment in front of it and the given lines behind it.
let private commented (text: string) (following: Trivia list) : Separation =
    Separation.NewLine
        { Comment = Some (comment text)
          Following = following }

/// A line break behind an indicator without a comment.
let private broken (following: Trivia list) : Separation =
    Separation.NewLine
        { Comment = None
          Following = following }

let private placement (property: PropertyName) (after: Separation) : PropertyPlacement =
    { Property = property
      After = after
      Span = None }

let private blockLiteral (text: string) =
    YamlNode.scalar (ScalarStyle.Block (BlockScalarStyle.Literal, ChompingMode.Clip, None)) text

let private withEnd (lines: Trivia list) (node: YamlNode) : YamlNode =
    match node.Kind with
    | YamlNodeKind.Sequence content -> { node with Kind = YamlNodeKind.Sequence { content with End = lines } }
    | YamlNodeKind.Mapping content -> { node with Kind = YamlNodeKind.Mapping { content with End = lines } }
    | _ -> failwith "Only a collection has an end."

let private asCollectionOf (items: SequenceItem list) (style: CollectionStyle) : YamlNode =
    { YamlNode.sequence [ YamlNode.item YamlNode.empty ] with
        Kind =
            YamlNodeKind.Sequence
                { Style = style
                  OpeningComment = None
                  Items = items
                  End = [] } }

let private mappingOf (style: CollectionStyle) (entries: MappingEntry list) : YamlNode =
    { YamlNode.mapping [ field "k" YamlNode.empty ] with
        Kind =
            YamlNodeKind.Mapping
                { Style = style
                  OpeningComment = None
                  Items = entries
                  End = [] } }

let private documentOf (root: YamlNode) = YamlDocument.create root

let private scalarValue (node: YamlNode) : string = (YamlNode.tryScalar node).Value.Value

/// The slots that the problems name, which are the texts in front of the first colon and a space.
let private slotsOf (problems: string list) : string list =
    problems |> List.map (fun problem -> problem.Substring(0, problem.IndexOf ": "))

let private slotsOfDocument (root: YamlNode) : string list = slotsOf (Validate.document (documentOf root))

let private expectValid (root: YamlNode) (message: string) = Expect.equal (Validate.document (documentOf root)) [] message

let tests_node_checks = testList "Validate: nodes" [
    testCase "a tree built with the constructors is valid" <| fun _ ->
        let tree =
            YamlNode.mapping
                [ field "a" (YamlNode.plain "1" |> YamlNode.withTag (YamlTag.core "int") |> YamlNode.withAnchor "x")
                  field "b" (YamlNode.sequence [ YamlNode.item (YamlNode.alias "x"); YamlNode.item YamlNode.empty ])
                  field "c" (YamlNode.flowMapping [ field "d" (YamlNode.plain "2") ] |> YamlNode.withTrailingComment (comment " c"))
                  field "e" (blockLiteral "text\n" |> YamlNode.withHeaderComment (comment " h"))
                  field "f" (YamlNode.mapping [] |> YamlNode.withOpeningComment (comment " o")) ]
        expectValid tree "constructed tree"

    testCase "an alias has no properties" <| fun _ ->
        expectValid (YamlNode.alias "a") "alias"
        let tagged = { YamlNode.alias "a" with Properties = { Tag = Some (YamlTag.core "str"); Anchor = None } }
        Expect.equal (slotsOfDocument tagged) [ "root.PropertyLayout"; "root.Properties" ] "A tagged alias"
        let anchored = { YamlNode.alias "a" with Properties = { Tag = None; Anchor = Some "b" }; PropertyLayout = [ placement PropertyName.Anchor Separation.Inline ] }
        Expect.equal (slotsOfDocument anchored) [ "root.Properties" ] "An anchored alias with a layout"

    testCase "the layout names each present property once and nothing else" <| fun _ ->
        let tagged = YamlNode.plain "a" |> YamlNode.withTag (YamlTag.core "str")
        expectValid tagged "tag and layout"
        Expect.equal (slotsOfDocument { tagged with PropertyLayout = [] }) [ "root.PropertyLayout" ] "no placement for the tag"
        let twice = { tagged with PropertyLayout = tagged.PropertyLayout @ tagged.PropertyLayout }
        Expect.equal (slotsOfDocument twice) [ "root.PropertyLayout" ] "two placements for the tag"
        let absent = { tagged with PropertyLayout = tagged.PropertyLayout @ [ placement PropertyName.Anchor Separation.Inline ] }
        Expect.equal (slotsOfDocument absent) [ "root.PropertyLayout" ] "a placement for an anchor the node does not have"
        let both = tagged |> YamlNode.withAnchor "x"
        expectValid both "tag and anchor"
        Expect.equal (slotsOfDocument { both with PropertyLayout = List.rev both.PropertyLayout }) [] "The order of the placements is free"
        Expect.equal (slotsOfDocument { both with PropertyLayout = [ List.head both.PropertyLayout ] }) [ "root.PropertyLayout" ] "no placement for the anchor"

    testCase "a block collection has no trailing comment, a flow collection may have one" <| fun _ ->
        let note = Some (comment " c")
        expectValid (YamlNode.flowMapping [ field "a" YamlNode.empty ] |> YamlNode.withTrailingComment (comment " c")) "flow mapping"
        expectValid (YamlNode.flowSequence [ YamlNode.item YamlNode.empty ] |> YamlNode.withTrailingComment (comment " c")) "flow sequence"
        Expect.equal (slotsOfDocument { YamlNode.mapping [ field "a" YamlNode.empty ] with TrailingComment = note }) [ "root.TrailingComment" ] "block mapping"
        Expect.equal (slotsOfDocument { YamlNode.sequence [ YamlNode.item YamlNode.empty ] with TrailingComment = note }) [ "root.TrailingComment" ] "block sequence"

    testCase "a block scalar has a header comment and no trailing comment" <| fun _ ->
        expectValid (blockLiteral "a\n" |> YamlNode.withHeaderComment (comment " h")) "header comment"
        Expect.equal (slotsOfDocument { blockLiteral "a\n" with TrailingComment = Some (comment " c") }) [ "root.TrailingComment" ] "trailing comment"

    testCase "only a block scalar has a header comment" <| fun _ ->
        let content: YamlScalar = { Value = "a"; Style = ScalarStyle.Plain; HeaderComment = Some (comment " h"); Layout = None }
        Expect.equal (slotsOfDocument { YamlNode.plain "a" with Kind = YamlNodeKind.Scalar content }) [ "root.HeaderComment" ] "plain scalar"
        Expect.equal (slotsOfDocument { YamlNode.plain "a" with Kind = YamlNodeKind.Scalar { content with Style = ScalarStyle.DoubleQuoted } }) [ "root.HeaderComment" ] "double-quoted scalar"

    testCase "only a flow collection has an opening comment" <| fun _ ->
        let note = Some (comment " o")
        let block = YamlNode.mapping [ field "a" YamlNode.empty ]
        let content = (YamlNode.tryMapping block).Value
        Expect.equal (slotsOfDocument { block with Kind = YamlNodeKind.Mapping { content with OpeningComment = note } }) [ "root.OpeningComment" ] "block mapping"
        let blockSequence = YamlNode.sequence [ YamlNode.item YamlNode.empty ]
        let sequenceContent = (YamlNode.trySequence blockSequence).Value
        Expect.equal (slotsOfDocument { blockSequence with Kind = YamlNodeKind.Sequence { sequenceContent with OpeningComment = note } }) [ "root.OpeningComment" ] "block sequence"
        expectValid (YamlNode.flowMapping [ field "a" YamlNode.empty ] |> YamlNode.withOpeningComment (comment " o")) "flow mapping"

    testCase "an empty collection is a flow collection" <| fun _ ->
        expectValid (YamlNode.mapping []) "empty mapping"
        expectValid (YamlNode.sequence []) "empty sequence"
        Expect.equal (slotsOfDocument (mappingOf CollectionStyle.Block [])) [ "root" ] "empty block mapping"
        Expect.equal (slotsOfDocument (asCollectionOf [] CollectionStyle.Block)) [ "root" ] "empty block sequence"

    testCase "a block item has a dash and a flow item has none" <| fun _ ->
        let withDash = YamlNode.item (YamlNode.plain "a")
        let withoutDash = { withDash with Dash = None }
        expectValid (asCollectionOf [ withDash ] CollectionStyle.Block) "block item"
        expectValid (asCollectionOf [ withoutDash ] CollectionStyle.Flow) "flow item"
        Expect.equal (slotsOfDocument (asCollectionOf [ withDash; withoutDash ] CollectionStyle.Block)) [ "root.items[1].Dash" ] "block item without a dash"
        Expect.equal (slotsOfDocument (asCollectionOf [ withoutDash; withDash ] CollectionStyle.Flow)) [ "root.items[1].Dash" ] "flow item with a dash"

    testCase "a missing value has no trailing comment" <| fun _ ->
        let note = Some (comment " c")
        let missing = { YamlNode.empty with TrailingComment = note }
        Expect.equal (slotsOfDocument (YamlNode.mapping [ field "k" missing ])) [ "root.entries[0].Value.TrailingComment" ] "missing value"
        let emptyString = { YamlNode.scalar ScalarStyle.DoubleQuoted "" with TrailingComment = note }
        expectValid (YamlNode.mapping [ field "k" emptyString ]) "An empty double-quoted scalar is a value"
        let quotedWithTag = YamlNode.scalar ScalarStyle.DoubleQuoted "" |> YamlNode.withTag (YamlTag.core "str") |> YamlNode.withTrailingComment (comment " c")
        expectValid (YamlNode.mapping [ field "k" quotedWithTag ]) "A quoted scalar with a property has content"

    testCase "a node with properties and no content has a trailing comment only inside a flow collection" <| fun _ ->
        let note = Some (comment " c")
        let tagged = YamlNode.empty |> YamlNode.withTag (YamlTag.core "str")
        let anchored = YamlNode.empty |> YamlNode.withAnchor "a"
        let both = tagged |> YamlNode.withAnchor "a"
        for (name, node) in [ "tag", tagged; "anchor", anchored; "tag and anchor", both ] do
            let commented = { node with TrailingComment = note }
            Expect.equal (slotsOfDocument (YamlNode.mapping [ field "k" commented ])) [ "root.entries[0].Value.TrailingComment" ] (name + " as a block value")
            Expect.equal (slotsOfDocument (YamlNode.mapping [ YamlNode.entry commented (number "v") ])) [ "root.entries[0].Key.TrailingComment" ] (name + " as a block key")
            Expect.equal (slotsOfDocument (YamlNode.sequence [ YamlNode.item commented ])) [ "root.items[0].Node.TrailingComment" ] (name + " as a block item")
            Expect.equal (slotsOfDocument commented) [ "root.TrailingComment" ] (name + " as the root")
            expectValid (YamlNode.flowSequence [ YamlNode.item commented ]) (name + " as a flow item")
            expectValid (YamlNode.flowMapping [ field "k" commented ]) (name + " as a flow value")
            expectValid (YamlNode.flowMapping [ YamlNode.entry commented YamlNode.empty ]) (name + " as a flow key")
        let inFlow = YamlNode.flowSequence [ YamlNode.item { tagged with TrailingComment = note } ]
        expectValid (YamlNode.mapping [ field "k" inFlow ]) "a flow item in a block mapping"
        let deeper = YamlNode.flowSequence [ YamlNode.item (YamlNode.flowMapping [ field "k" { tagged with TrailingComment = note } ]) ]
        expectValid deeper "a flow value in a flow sequence"
        let lastProperty = if both.PropertyLayout.IsEmpty then PropertyName.Tag else (List.last both.PropertyLayout).Property
        expectValid (YamlNode.mapping [ field "k" (YamlNode.withPropertyComment lastProperty (comment " c") both) ]) "The comment stands behind the last property"
        expectValid (YamlNode.mapping [ field "k" (YamlNode.withPropertyComment PropertyName.Tag (comment " c") tagged) ]) "The comment stands behind the tag"
        let withContent = YamlNode.plain "v" |> YamlNode.withTag (YamlTag.core "str") |> YamlNode.withTrailingComment (comment " c")
        expectValid (YamlNode.mapping [ field "k" withContent ]) "A scalar with content has a trailing comment"
        let flow = YamlNode.flowSequence [] |> YamlNode.withTag (YamlTag.core "seq") |> YamlNode.withTrailingComment (comment " c")
        expectValid (YamlNode.mapping [ field "k" flow ]) "A flow collection with a property has content"

    testCase "a message names its rule" <| fun _ ->
        let problem = (Validate.document (documentOf (YamlNode.mapping [ field "k" { YamlNode.empty with TrailingComment = Some (comment " c") } ]))).Head
        Expect.stringContains problem "root.entries[0].Value.TrailingComment: " "slot"
        Expect.stringContains problem "(R1)" "rule"

    testCase "a path shows the slots down to the node" <| fun _ ->
        let note = Some (comment " c")
        let deep = YamlNode.mapping [ field "a" YamlNode.empty; field "b" (YamlNode.sequence [ YamlNode.item (YamlNode.mapping [ field "c" { YamlNode.empty with TrailingComment = note } ]) ]) ]
        Expect.equal (slotsOfDocument deep) [ "root.entries[1].Value.items[0].Node.entries[0].Value.TrailingComment" ] "nested slot"
        let keyed = YamlNode.mapping [ YamlNode.entry { YamlNode.empty with TrailingComment = note } (YamlNode.plain "v") ]
        Expect.equal (slotsOfDocument keyed) [ "root.entries[0].Key.TrailingComment" ] "key slot"
]

let tests_rule_checks = testList "Validate: separations, documents and streams" [
    testCase "the trivia behind an indicator and a block collection without a property go to the first entry" <| fun _ ->
        let entries = [ field "a" (number "1") ]
        let inner = YamlNode.mapping entries
        let late = { field "a" (number "1") with Before = [ lineComment " d" ] }
        let key = YamlNode.mapping [ { field "k" (YamlNode.mapping [ late ]) with Colon = Some (commented " c" []) } ]
        expectValid key "trivia in the entry"
        let wrongColon = YamlNode.mapping [ { field "k" inner with Colon = Some (commented " c" [ lineComment " d" ]) } ]
        Expect.equal (slotsOfDocument wrongColon) [ "root.entries[0].Colon.Following" ] "colon"
        let wrongQuestion = YamlNode.mapping [ { YamlNode.entry inner (number "v") with Question = Some (broken [ BlankLine None ]) } ]
        Expect.equal (slotsOfDocument wrongQuestion) [ "root.entries[0].Question.Following" ] "question mark"
        let wrongDash = YamlNode.sequence [ { YamlNode.item inner with Dash = Some (broken [ BlankLine None ]) } ]
        Expect.equal (slotsOfDocument wrongDash) [ "root.items[0].Dash.Following" ] "dash"

    testCase "the trivia behind an indicator may stand in Following when a token follows" <| fun _ ->
        let following = [ lineComment " d"; BlankLine None ]
        expectValid (YamlNode.mapping [ { field "k" (number "v") with Colon = Some (commented " c" following) } ]) "scalar"
        expectValid (YamlNode.mapping [ { field "k" (YamlNode.alias "a") with Colon = Some (broken following) } ]) "alias"
        expectValid (YamlNode.mapping [ { field "k" (YamlNode.flowSequence [ YamlNode.item (number "1") ]) with Colon = Some (broken following) } ]) "flow collection"
        let tagged = YamlNode.mapping [ field "b" (number "1") ] |> YamlNode.withTag (YamlTag.core "map")
        expectValid (YamlNode.mapping [ { field "k" tagged with Colon = Some (broken following) } ]) "block collection with a property"

    testCase "the trivia behind the last property and a block collection go to the first entry" <| fun _ ->
        let tagged = YamlNode.mapping [ { field "b" (number "1") with Before = [ lineComment " d" ] } ] |> YamlNode.withTag (YamlTag.core "map")
        let behindTag = { tagged with PropertyLayout = [ placement PropertyName.Tag (broken []) ] }
        expectValid behindTag "trivia in the entry"
        let wrong = { tagged with PropertyLayout = [ placement PropertyName.Tag (broken [ lineComment " d" ]) ] }
        Expect.equal (slotsOfDocument wrong) [ "root.PropertyLayout[0].After.Following" ] "after the last property"
        let both = tagged |> YamlNode.withAnchor "a"
        let betweenProperties = { both with PropertyLayout = [ placement PropertyName.Tag (broken [ lineComment " d" ]); placement PropertyName.Anchor (broken []) ] }
        expectValid betweenProperties "trivia between two properties"
        let scalar = YamlNode.plain "v" |> YamlNode.withTag (YamlTag.core "str")
        expectValid { scalar with PropertyLayout = [ placement PropertyName.Tag (broken [ lineComment " d" ]) ] } "trivia in front of a scalar"

    testCase "the trivia behind an indicator whose content is missing go to the next entry or item" <| fun _ ->
        let following = [ lineComment " d" ]
        let next = { field "j" (number "1") with Before = following }
        expectValid (YamlNode.mapping [ { field "k" YamlNode.empty with Colon = Some (commented " c" []) }; next ]) "colon, trivia in the next entry"
        Expect.equal
            (slotsOfDocument (YamlNode.mapping [ { field "k" YamlNode.empty with Colon = Some (commented " c" following) } ]))
            [ "root.entries[0].Colon.Following" ]
            "colon"
        let nextItem = { YamlNode.item (number "b") with Before = following }
        expectValid (YamlNode.sequence [ { YamlNode.item YamlNode.empty with Dash = Some (commented " c" []) }; nextItem ]) "dash, trivia in the next item"
        Expect.equal
            (slotsOfDocument (YamlNode.sequence [ { YamlNode.item YamlNode.empty with Dash = Some (commented " c" following) } ]))
            [ "root.items[0].Dash.Following" ]
            "dash"
        let missingKey = { YamlNode.entry YamlNode.empty (number "v") with Question = Some (commented " c" []); BeforeColon = following }
        expectValid (YamlNode.mapping [ missingKey ]) "question mark, trivia in BeforeColon"
        Expect.equal
            (slotsOfDocument (YamlNode.mapping [ { missingKey with Question = Some (commented " c" following); BeforeColon = [] } ]))
            [ "root.entries[0].Question.Following" ]
            "question mark"
        let tagged = YamlNode.empty |> YamlNode.withTag (YamlTag.core "str")
        expectValid (YamlNode.mapping [ { field "k" tagged with Colon = Some (broken following) } ]) "A property is a token, so the colon may hold Following"
        let behindTag = { tagged with PropertyLayout = [ placement PropertyName.Tag (commented " c" following) ] }
        Expect.equal
            (slotsOfDocument (YamlNode.mapping [ field "k" behindTag ]))
            [ "root.entries[0].Value.PropertyLayout[0].After.Following" ]
            "behind the last property of a node without content"
        expectValid (YamlNode.mapping [ field "k" { tagged with PropertyLayout = [ placement PropertyName.Tag (commented " c" []) ] }; next ]) "trivia in the next entry"

    testCase "the gap before a colon belongs to an explicit key or a key in a flow mapping" <| fun _ ->
        let gap = [ lineComment " c" ]
        let implicitKey = { field "a" (number "b") with BeforeColon = gap }
        Expect.equal (slotsOfDocument (mappingOf CollectionStyle.Block [ implicitKey ])) [ "root.entries[0].BeforeColon" ] "implicit key in a block mapping"
        expectValid (mappingOf CollectionStyle.Block [ { implicitKey with Question = Some Separation.Inline } ]) "explicit key"
        expectValid (mappingOf CollectionStyle.Flow [ implicitKey ]) "implicit key in a flow mapping"

    testCase "the trivia in front of a block collection root belong to its first entry" <| fun _ ->
        let entry = field "a" (number "1")
        let root = YamlNode.mapping [ entry ]
        let hasBefore = { YamlDocument.create (YamlNode.mapping [ { entry with Before = [ lineComment " c" ] } ]) with StartMarker = Some { TrailingComment = None; Span = None } }
        Expect.equal (Validate.document hasBefore) [] "trivia in the first entry"
        let wrong = { YamlDocument.create root with BeforeRoot = [ lineComment " c" ] }
        Expect.equal (slotsOf (Validate.document wrong)) [ "BeforeRoot" ] "BeforeRoot with a block collection"
        let wrongSequence = { YamlDocument.create (YamlNode.sequence [ YamlNode.item (number "1") ]) with BeforeRoot = [ BlankLine None ] }
        Expect.equal (slotsOf (Validate.document wrongSequence)) [ "BeforeRoot" ] "BeforeRoot with a block sequence"
        let scalar = { YamlDocument.create (number "v") with BeforeRoot = [ lineComment " c" ] }
        Expect.equal (Validate.document scalar) [] "BeforeRoot with a scalar"
        let flow = { YamlDocument.create (YamlNode.flowSequence []) with BeforeRoot = [ lineComment " c" ] }
        Expect.equal (Validate.document flow) [] "BeforeRoot with a flow collection"
        let tagged = { YamlDocument.create (YamlNode.withTag (YamlTag.core "map") root) with BeforeRoot = [ lineComment " c" ] }
        Expect.equal (Validate.document tagged) [] "BeforeRoot with a property in front of a block collection"

    testCase "the trivia behind a block collection or a missing root do not stand in AfterRoot" <| fun _ ->
        let root = YamlNode.mapping [ field "a" (number "1") ]
        let after = [ lineComment " c" ]
        Expect.equal (Validate.document { YamlDocument.create (withEnd after root) with AfterRoot = [] }) [] "End of the collection"
        Expect.equal (slotsOf (Validate.document { YamlDocument.create root with AfterRoot = after })) [ "AfterRoot" ] "AfterRoot with a block mapping"
        Expect.equal (slotsOf (Validate.document { YamlDocument.create (YamlNode.sequence [ YamlNode.item (number "1") ]) with AfterRoot = after })) [ "AfterRoot" ] "AfterRoot with a block sequence"
        Expect.equal (Validate.document { YamlDocument.create (number "v") with AfterRoot = after }) [] "AfterRoot with a scalar"
        Expect.equal (Validate.document { YamlDocument.create (YamlNode.flowMapping []) with AfterRoot = after }) [] "AfterRoot with a flow collection"
        Expect.equal (slotsOf (Validate.document { YamlDocument.create YamlNode.empty with AfterRoot = after })) [ "AfterRoot" ] "AfterRoot with a missing root"
        Expect.equal (Validate.document { YamlDocument.create YamlNode.empty with BeforeRoot = after }) [] "BeforeRoot with a missing root"

    testCase "trivia of the stream stand behind an extra end marker or at the start of the stream in front of one and nowhere else" <| fun _ ->
        let marker: DocumentMarker = { TrailingComment = None; Span = None }
        let document (text: string) = StreamPart.Document (YamlDocument.create (number text))
        let closed (text: string) = StreamPart.Document { YamlDocument.create (number text) with EndMarker = Some marker }
        let extra = StreamPart.ExtraEndMarker marker
        let trivia = StreamPart.Trivia (lineComment " t")
        let slotsOfParts (parts: StreamPart list) = slotsOf (Validate.stream { Parts = parts })
        Expect.equal (slotsOfParts []) [] "empty stream"
        Expect.equal (slotsOfParts [ trivia ]) [] "only trivia"
        Expect.equal (slotsOfParts [ trivia; extra ]) [] "trivia and an extra end marker without a document"
        // The trivia behind the `...` of a document belong to its AfterEndMarker.
        Expect.equal (slotsOfParts [ closed "a"; trivia ]) [ "Parts[1]" ] "trivia behind the end marker of the last document"
        Expect.equal (slotsOfParts [ closed "a"; trivia; trivia ]) [ "Parts[1]"; "Parts[2]" ] "a run of trivia behind the end marker"
        Expect.equal (slotsOfParts [ closed "a"; extra; trivia ]) [] "trivia behind an extra end marker"
        Expect.equal (slotsOfParts [ closed "a"; trivia; extra ]) [ "Parts[1]" ] "trivia between an end marker and an extra end marker"
        Expect.equal (slotsOfParts [ closed "a"; trivia; extra; trivia ]) [ "Parts[1]" ] "trivia on both sides of an extra end marker"
        Expect.equal (slotsOfParts [ document "a"; trivia ]) [ "Parts[1]" ] "trivia behind a document without a ..."
        Expect.equal (slotsOfParts [ trivia; document "a" ]) [ "Parts[0]" ] "trivia in front of a document"
        Expect.equal (slotsOfParts [ closed "a"; trivia; document "b" ]) [ "Parts[1]" ] "trivia between two documents"
        Expect.equal (slotsOfParts [ closed "a"; trivia; trivia; document "b" ]) [ "Parts[1]"; "Parts[2]" ] "a run of trivia in front of a document"
        Expect.equal (slotsOfParts [ closed "a"; trivia; extra; document "b" ]) [ "Parts[1]" ] "trivia between an end marker and an extra end marker, then a document"
        // By [211] a stream is l-document-prefix* (no document) l-document-suffix and then a bare document, so the trivia in front of a `...` that ends no document belong to no document.
        Expect.equal (slotsOfParts [ trivia; extra; document "a" ]) [] "trivia in front of an extra end marker at the start of a stream with a document"
        Expect.equal (slotsOfParts [ trivia; trivia; extra; trivia; extra; document "b" ]) [] "trivia in front of two extra end markers, then a document"
        Expect.equal (slotsOfParts [ trivia; extra; trivia; document "b" ]) [ "Parts[2]" ] "trivia behind the extra end marker and in front of the document stay invalid"
        Expect.equal (slotsOfParts [ trivia; extra; closed "a"; trivia; document "b" ]) [ "Parts[3]" ] "trivia in front of a document stay invalid when an extra end marker stands earlier"
        // A `...` behind a document without an end marker would end that document, so the stream has no extra end marker there and the trivia belong to the document.
        Expect.equal (slotsOfParts [ document "a"; trivia; extra ]) [ "Parts[1]"; "Parts[2]" ] "trivia between a document without a ... and an extra end marker"
        Expect.equal (slotsOfParts [ document "a"; trivia; trivia; extra ]) [ "Parts[1]"; "Parts[2]"; "Parts[3]" ] "a run of trivia between a document without a ... and an extra end marker"
        Expect.equal (slotsOfParts [ closed "a"; extra; trivia; extra ]) [] "trivia between two extra end markers behind a document"
        let withMarker = StreamPart.Document { YamlDocument.create (number "a") with StartMarker = Some marker }
        Expect.equal (slotsOfParts [ trivia; withMarker ]) [ "Parts[0]" ] "trivia in front of a document with a start marker"
        let problems = Validate.stream { Parts = [ trivia; document "a" ] }
        Expect.stringContains problems.Head "(R3)" "rule"
        Expect.stringContains (Validate.stream { Parts = [ closed "a"; trivia ] }).Head "AfterEndMarker" "The message names the slot"

    testCase "AfterEndMarker holds trivia behind the ... of a document that no document follows" <| fun _ ->
        let marker: DocumentMarker = { TrailingComment = None; Span = None }
        let after = [ lineComment " t"; Trivia.BlankLine None ]
        let closed = { YamlDocument.create (number "a") with EndMarker = Some marker; AfterEndMarker = after }
        let slotsOfParts (parts: StreamPart list) = slotsOf (Validate.stream { Parts = parts })
        Expect.equal (Validate.document closed) [] "behind the end marker of a single document"
        Expect.equal (slotsOfParts [ StreamPart.Document closed ]) [] "at the end of the stream"
        Expect.equal (slotsOfParts [ StreamPart.Document closed; StreamPart.ExtraEndMarker marker; StreamPart.Trivia (lineComment " u") ]) [] "in front of an extra end marker"
        let missing = { YamlDocument.create YamlNode.empty with StartMarker = Some marker; EndMarker = Some marker; AfterEndMarker = after }
        Expect.equal (Validate.document missing) [] "behind a document whose root is missing"
        Expect.equal (slotsOf (Validate.document { closed with EndMarker = None })) [ "AfterEndMarker" ] "without an end marker"
        let next = StreamPart.Document { YamlDocument.create (number "b") with StartMarker = Some marker }
        Expect.equal (slotsOfParts [ StreamPart.Document closed; next ]) [ "Parts[0].AfterEndMarker" ] "in front of a document"
        Expect.equal (slotsOf (Validate.document { closed with AfterEndMarker = [ lineComment " a\nb" ] })) [ "AfterEndMarker[0]" ] "the characters of a comment"

    // By [211] a `...` behind a document without an end marker is the end marker of that document, so the stream holds no extra end marker there.
    testCase "an extra end marker stands at the start of the stream or behind a ... and nowhere else" <| fun _ ->
        let marker: DocumentMarker = { TrailingComment = None; Span = None }
        let document (text: string) = StreamPart.Document (YamlDocument.create (number text))
        let closed (text: string) = StreamPart.Document { YamlDocument.create (number text) with EndMarker = Some marker }
        let extra = StreamPart.ExtraEndMarker marker
        let trivia = StreamPart.Trivia (lineComment " t")
        let slotsOfParts (parts: StreamPart list) = slotsOf (Validate.stream { Parts = parts })
        Expect.equal (slotsOfParts [ extra ]) [] "an extra end marker alone"
        Expect.equal (slotsOfParts [ extra; extra; document "a" ]) [] "two extra end markers in front of a document"
        Expect.equal (slotsOfParts [ closed "a"; extra ]) [] "an extra end marker behind a document with an end marker"
        Expect.equal (slotsOfParts [ closed "a"; extra; extra; document "b" ]) [] "two extra end markers between two documents"
        let commented = StreamPart.Document { YamlDocument.create (number "a") with EndMarker = Some marker; AfterEndMarker = [ lineComment " t" ] }
        Expect.equal (slotsOfParts [ commented; extra; document "b" ]) [] "trivia and an extra end marker between two documents"
        Expect.equal (slotsOfParts [ document "a"; extra ]) [ "Parts[1]" ] "an extra end marker behind a document without a ..."
        Expect.equal (slotsOfParts [ document "a"; extra; document "b" ]) [ "Parts[1]" ] "an extra end marker between two documents, the first without a ..."
        Expect.equal (slotsOfParts [ document "a"; extra; trivia ]) [ "Parts[1]" ] "trivia behind an extra end marker that follows a document without a ..."
        Expect.equal (slotsOfParts [ closed "a"; extra; document "b"; extra ]) [ "Parts[3]" ] "an extra end marker behind the second document, which has no ..."
        let problems = Validate.stream { Parts = [ document "a"; extra ] }
        Expect.stringContains problems.Head "(R3)" "rule"

    testCase "a document without a ... gives the trivia in front of the next document to that document" <| fun _ ->
        let marker: DocumentMarker = { TrailingComment = None; Span = None }
        let first = YamlDocument.create (number "a")
        let next = { YamlDocument.create (number "b") with StartMarker = Some marker; Prelude = [ PreludeTrivia (lineComment " c") ] }
        Expect.equal (Validate.stream { Parts = [ StreamPart.Document first; StreamPart.Document next ] }) [] "trivia in the Prelude of the next document"
        let stolen = { first with AfterRoot = [ lineComment " c" ] }
        Expect.equal
            (slotsOf (Validate.stream { Parts = [ StreamPart.Document stolen; StreamPart.Document { next with Prelude = [] } ] }))
            [ "Parts[0].AfterRoot" ]
            "trivia behind the root of the document before"
        let closed = { stolen with EndMarker = Some marker }
        Expect.equal (Validate.stream { Parts = [ StreamPart.Document closed; StreamPart.Document next ] }) [] "AfterRoot in front of a ..."
        Expect.equal (Validate.stream { Parts = [ StreamPart.Document stolen ] }) [] "AfterRoot at the end of the stream"
        Expect.equal (Validate.document stolen) [] "AfterRoot of a single document"

    testCase "the slots of a document in a stream start with its part" <| fun _ ->
        let bad = { YamlNode.empty with TrailingComment = Some (comment " c") }
        let stream: YamlStream =
            { Parts =
                [ StreamPart.Document (YamlDocument.create (number "a"))
                  StreamPart.Document { YamlDocument.create (YamlNode.mapping [ field "k" bad ]) with BeforeRoot = [ BlankLine None ] } ] }
        Expect.equal (slotsOf (Validate.stream stream)) [ "Parts[1].BeforeRoot"; "Parts[1].root.entries[0].Value.TrailingComment" ] "slots"

    testCase "a tree 10000 levels deep validates without a stack overflow and names a deep slot with its depth" <| fun _ ->
        let build (leaf: YamlNode) =
            let mutable node = leaf
            for _ in 1 .. 10000 do
                node <- YamlNode.sequence [ YamlNode.item node ]
            node
        Expect.equal (slotsOfDocument (build (number "x"))) [] "valid deep tree"
        let problems = slotsOfDocument (build { YamlNode.empty with TrailingComment = Some (comment " c") })
        Expect.equal problems.Length 1 "one problem"
        Expect.isTrue (problems.Head.StartsWith "root(10000).items[0].Node.items[0].Node") "The path shows the depth and the last segments"
        Expect.isTrue (problems.Head.EndsWith ".items[0].Node.TrailingComment") "The path ends at the slot"
]

/// The rule R1 to R8 tests build the tree that each rule asks for and validate it. The checks of the tree catch
/// a wrong placement where the tree alone shows it. Where the rule depends on columns or lines, the test only
/// shows where the parser puts the item.
let tests_rules = testList "Slot rules" [
    // yaml@2.6.1 reads `k: # c` followed by `  a: 1` as {"k":{"a":1}}, `- # c` followed by `  a` as ["a"], `? # c`
    // followed by `  a` and `: b` as {"a":"b"}, `k: &a # c` followed by `  b: 1` as {"k":{"b":1}}, `k: # c` as {"k":null}
    // and `k: !!str # c` as {"k":""}.
    testCase "R1: a comment behind an indicator without content goes to the separation of the indicator" <| fun _ ->
        let block = YamlNode.mapping [ field "a" (number "1") ]
        expectValid (YamlNode.mapping [ { field "k" block with Colon = Some (commented " c" []) } ]) "colon"
        expectValid (YamlNode.sequence [ { YamlNode.item (number "a") with Dash = Some (commented " c" []) } ]) "dash"
        expectValid (YamlNode.mapping [ { field "a" (number "b") with Question = Some (commented " c" []) } ]) "question mark"
        let anchored = YamlNode.mapping [ field "b" (number "1") ] |> YamlNode.withAnchor "a" |> YamlNode.withPropertyComment PropertyName.Anchor (comment " c")
        expectValid (YamlNode.mapping [ field "k" anchored ]) "property"
        let missing = YamlNode.mapping [ { field "k" YamlNode.empty with Colon = Some (commented " c" []) } ]
        expectValid missing "missing value"
        let wrong = YamlNode.mapping [ field "k" { YamlNode.empty with TrailingComment = Some (comment " c") } ]
        Expect.equal (slotsOfDocument wrong) [ "root.entries[0].Value.TrailingComment" ] "The comment on the missing value is wrong"
        // k: !!str # c
        let tagged = YamlNode.empty |> YamlNode.withTag (YamlTag.core "str")
        expectValid (YamlNode.mapping [ field "k" (YamlNode.withPropertyComment PropertyName.Tag (comment " c") tagged) ]) "property without content"
        let wrongProperty = YamlNode.mapping [ field "k" { tagged with TrailingComment = Some (comment " c") } ]
        Expect.equal (slotsOfDocument wrongProperty) [ "root.entries[0].Value.TrailingComment" ] "The comment on the node without content is wrong"

    // yaml@2.6.1 reads `k: # c`, `  # d` and `  a: 1` as {"k":{"a":1}}, `k: # c`, `  # d` and `  v` as {"k":"v"},
    // `k: !!map`, `  # d` and `  b: 1` as {"k":{"b":1}}, `k: !!str`, `  # d` and `  v` as {"k":"v"}, and `k: # c`, `# d`
    // and `j: 1` as {"k":null,"j":1}.
    testCase "R2: the trivia behind the line break go to the first entry of a block collection and to Following otherwise" <| fun _ ->
        let first = { field "a" (number "1") with Before = [ lineComment " d" ] }
        let block = YamlNode.mapping [ { field "k" (YamlNode.mapping [ first ]) with Colon = Some (commented " c" []) } ]
        expectValid block "block collection"
        let scalar = YamlNode.mapping [ { field "k" (number "v") with Colon = Some (commented " c" [ lineComment " d" ]) } ]
        expectValid scalar "scalar"
        let wrong = YamlNode.mapping [ { field "k" (YamlNode.mapping [ field "a" (number "1") ]) with Colon = Some (commented " c" [ lineComment " d" ]) } ]
        Expect.equal (slotsOfDocument wrong) [ "root.entries[0].Colon.Following" ] "The comment in Following is wrong"
        let behindTag = YamlNode.mapping [ { field "b" (number "1") with Before = [ lineComment " d" ] } ] |> YamlNode.withTag (YamlTag.core "map")
        expectValid (YamlNode.mapping [ field "k" { behindTag with PropertyLayout = [ placement PropertyName.Tag (broken []) ] } ]) "behind a property"
        // k: # c
        // # d
        // j: 1
        let missing = YamlNode.mapping [ { field "k" YamlNode.empty with Colon = Some (commented " c" []) }; { field "j" (number "1") with Before = [ lineComment " d" ] } ]
        expectValid missing "missing value, trivia in the next entry"
        let wrongMissing = YamlNode.mapping [ { field "k" YamlNode.empty with Colon = Some (commented " c" [ lineComment " d" ]) }; field "j" (number "1") ]
        Expect.equal (slotsOfDocument wrongMissing) [ "root.entries[0].Colon.Following" ] "The comment in Following is wrong when the value is missing"

    // yaml@2.6.1 reads `# before`, `---`, `# after` and `a: 1` as {"a":1}, `---`, `# d`, `v`, `# e` and `...` as "v", `---` and `# d`
    // as null, and `a`, `...`, `# c` and `b` as the two documents "a" and "b". It reads `a: 1` and `# c` as {"a":1}.
    testCase "R3: the trivia around the root go to Prelude, BeforeRoot, the first entry, AfterRoot or the stream" <| fun _ ->
        let marker: DocumentMarker option = Some { TrailingComment = None; Span = None }
        // # before
        // ---
        // # after
        // a: 1
        let block =
            { YamlDocument.create (YamlNode.mapping [ { field "a" (number "1") with Before = [ lineComment " after" ] } ]) with
                Prelude = [ PreludeTrivia (lineComment " before") ]
                StartMarker = marker }
        Expect.equal (Validate.document block) [] "block collection root"
        // ---
        // # d
        // v
        // # e
        // ...
        let scalar =
            { YamlDocument.create (number "v") with
                StartMarker = marker
                BeforeRoot = [ lineComment " d" ]
                AfterRoot = [ lineComment " e" ]
                EndMarker = marker }
        Expect.equal (Validate.document scalar) [] "scalar root"
        // ---
        // # d
        let missing = { YamlDocument.create YamlNode.empty with StartMarker = marker; BeforeRoot = [ lineComment " d" ] }
        Expect.equal (Validate.document missing) [] "missing root"
        // a
        // ...
        // # c
        // b
        let stream: YamlStream =
            { Parts =
                [ StreamPart.Document { YamlDocument.create (number "a") with EndMarker = marker }
                  StreamPart.Document { YamlDocument.create (number "b") with BeforeRoot = [ lineComment " c" ] } ] }
        Expect.equal (Validate.stream stream) [] "trivia behind the end marker and in front of the next document"
        let wrong: YamlStream =
            { Parts =
                [ StreamPart.Document { YamlDocument.create (number "a") with EndMarker = marker }
                  StreamPart.Trivia (lineComment " c")
                  StreamPart.Document (YamlDocument.create (number "b")) ] }
        Expect.equal (slotsOf (Validate.stream wrong)) [ "Parts[1]" ] "The comment in the stream is wrong"
        // # only
        Expect.equal (Validate.stream { Parts = [ StreamPart.Trivia (lineComment " only") ] }) [] "a stream without a document"

    // yaml@2.6.1 reads `a`, `# c`, `---` and `b` as the documents "a" and "b", and keeps the comment c in the document before,
    // where it is the comment of that document. The model gives the comment to the Prelude of the document that the `---` opens.
    // It reads `a`, `# c`, `...` and `b` as "a" and "b" with the comment c in front of the `...`, and `a`, `...`, `# c`, `---` and
    // `b` as "a" and "b" with the comment c in front of the second document. It reads `a`, `...` and `# t` as "a".
    testCase "R3: the trivia in front of a --- go to the Prelude of the document that it opens" <| fun _ ->
        let marker: DocumentMarker option = Some { TrailingComment = None; Span = None }
        let note = lineComment " c"
        let first = YamlDocument.create (number "a")
        let opened = { YamlDocument.create (number "b") with StartMarker = marker }
        // a
        // # c
        // ---
        // b
        let behindRoot: YamlStream =
            { Parts = [ StreamPart.Document first; StreamPart.Document { opened with Prelude = [ PreludeTrivia note ] } ] }
        Expect.equal (Validate.stream behindRoot) [] "Prelude of the next document"
        let stolen: YamlStream = { Parts = [ StreamPart.Document { first with AfterRoot = [ note ] }; StreamPart.Document opened ] }
        Expect.equal (slotsOf (Validate.stream stolen)) [ "Parts[0].AfterRoot" ] "AfterRoot of the document before is wrong"
        // a
        // # c
        // ...
        // b
        let beforeEnd: YamlStream =
            { Parts = [ StreamPart.Document { first with AfterRoot = [ note ]; EndMarker = marker }; StreamPart.Document (YamlDocument.create (number "b")) ] }
        Expect.equal (Validate.stream beforeEnd) [] "AfterRoot in front of a ..."
        // a
        // ...
        // # c
        // ---
        // b
        let behindEnd: YamlStream =
            { Parts = [ StreamPart.Document { first with EndMarker = marker }; StreamPart.Document { opened with Prelude = [ PreludeTrivia note ] } ] }
        Expect.equal (Validate.stream behindEnd) [] "Prelude behind a ..."
        // a
        // ...
        // # c
        // %YAML 1.2
        // ---
        // b
        let directive: YamlDirective = { Kind = YamlVersion (1, 2); TrailingComment = None; Span = None }
        let withDirective: YamlStream =
            { Parts =
                [ StreamPart.Document { first with EndMarker = marker }
                  StreamPart.Document { opened with Prelude = [ PreludeTrivia note; Directive directive ] } ] }
        Expect.equal (Validate.stream withDirective) [] "Prelude with a directive"
        // a
        // ...
        // # t
        let behindLast: YamlStream = { Parts = [ StreamPart.Document { first with EndMarker = marker; AfterEndMarker = [ lineComment " t" ] } ] }
        Expect.equal (Validate.stream behindLast) [] "trivia behind the last ..."
        let inStream: YamlStream = { Parts = [ StreamPart.Document { first with EndMarker = marker }; StreamPart.Trivia (lineComment " t") ] }
        Expect.equal (slotsOf (Validate.stream inStream)) [ "Parts[1]" ] "The trivia behind the last ... in the stream are wrong"
        // a
        // # t
        let noEnd: YamlStream = { Parts = [ StreamPart.Document { first with AfterRoot = [ lineComment " t" ] } ] }
        Expect.equal (Validate.stream noEnd) [] "AfterRoot at the end of the stream"
        let wrongEnd: YamlStream = { Parts = [ StreamPart.Document first; StreamPart.Trivia (lineComment " t") ] }
        Expect.equal (slotsOf (Validate.stream wrongEnd)) [ "Parts[1]" ] "trivia behind a document without a ... belong to AfterRoot"

    // yaml@2.6.1 reads `a:`, `  b: 1`, `# c` and `d: 2` as {"a":{"b":1},"d":2}. The comment stands between two nodes, so it
    // belongs to the entry `d`, although it stands at the column of `a`.
    testCase "R4: the trivia between two nodes belong to the Before of the next entry" <| fun _ ->
        let tree =
            YamlNode.mapping
                [ field "a" (YamlNode.mapping [ field "b" (number "1") ])
                  { field "d" (number "2") with Before = [ lineComment " c" ] } ]
        expectValid tree "comment in front of d"
        Expect.equal (YamlNode.entries tree).[1].Before [ lineComment " c" ] "Before of d"
        Expect.equal (YamlNode.entries (YamlNode.entries tree).[0].Value).[0].Before [] "Before of b"

    // yaml@2.6.1 reads `a:`, `  b: 1`, `  # c1`, an empty line, `# c2` and an empty line as {"a":{"b":1}}. The comment c1
    // stands at the column of b, so the End of the inner mapping holds it. The comment c2 stands at the column of a, so the End of
    // the root holds it. The empty line behind c1 goes with c2, and the empty line behind the last comment goes with c2.
    testCase "R4: the trivia behind the last node are split by column" <| fun _ ->
        let inner = YamlNode.mapping [ field "b" (number "1") ] |> withEnd [ lineComment " c1" ]
        let tree = YamlNode.mapping [ field "a" inner ] |> withEnd [ BlankLine None; lineComment " c2"; BlankLine None ]
        expectValid tree "split by column"
        Expect.equal (YamlNode.tryMapping (YamlNode.entries tree).[0].Value).Value.End [ lineComment " c1" ] "End of the inner mapping"
        Expect.equal (YamlNode.tryMapping tree).Value.End [ BlankLine None; lineComment " c2"; BlankLine None ] "End of the root"
        // a:
        //   b: 1
        // and an empty line: no comment follows, so the root holds the empty line.
        let onlyBlank = YamlNode.mapping [ field "a" (YamlNode.mapping [ field "b" (number "1") ]) ] |> withEnd [ BlankLine None ]
        expectValid onlyBlank "empty line without a comment"

    // yaml@2.6.1 reads `k: |`, `  text`, an empty line, `# c` and `j: 1` as {"k":"text\n","j":1}, and the same with `|+` as
    // {"k":"text\n\n","j":1}.
    testCase "R5: an empty line behind a block scalar is trivia under clip and strip and content under keep" <| fun _ ->
        let clip = YamlNode.mapping [ field "k" (blockLiteral "text\n"); { field "j" (number "1") with Before = [ BlankLine None; lineComment " c" ] } ]
        expectValid clip "clip"
        let keep =
            YamlNode.mapping
                [ field "k" (YamlNode.scalar (ScalarStyle.Block (BlockScalarStyle.Literal, ChompingMode.Keep, None)) "text\n\n")
                  { field "j" (number "1") with Before = [ lineComment " c" ] } ]
        expectValid keep "keep"
        Expect.equal (scalarValue (YamlNode.entries keep).[0].Value) "text\n\n" "The empty line is part of the value"

    // yaml@2.6.1 reads `[ # o`, `a, # c1`, `# t`, `b # c2`, `# e` and `] # z` as ["a","b"], and `{ a: 1, # c`, `# t` and `b # d` and
    // `}` as {"a":1,"b":null}. It reads `[!!str , # c` followed by ` b]` as ["","b"], `{a: !!str, # c` followed by
    // ` b: 1}` as {"a":"","b":1} and `{ !!str , # c` followed by ` b: 1 }` as {"":null,"b":1}.
    testCase "R6: the comments of a flow collection" <| fun _ ->
        let first = { YamlNode.item (number "a" |> YamlNode.withTrailingComment (comment " c1")) with Dash = None }
        let second = { YamlNode.item (number "b" |> YamlNode.withTrailingComment (comment " c2")) with Before = [ lineComment " t" ]; Dash = None }
        let sequence =
            YamlNode.flowSequence [ first; second ]
            |> YamlNode.withOpeningComment (comment " o")
            |> withEnd [ lineComment " e" ]
            |> YamlNode.withTrailingComment (comment " z")
        expectValid sequence "flow sequence"
        // The entry b has no value, so its comment stands on the key.
        let keyOnly =
            { YamlNode.entry (number "b" |> YamlNode.withTrailingComment (comment " d")) YamlNode.empty with
                Before = [ lineComment " t" ]
                Colon = None }
        let mapping = YamlNode.flowMapping [ field "a" (number "1" |> YamlNode.withTrailingComment (comment " c")); keyOnly ]
        expectValid mapping "flow mapping"
        let wrong = YamlNode.flowMapping [ { keyOnly with Value = { YamlNode.empty with TrailingComment = Some (comment " d") }; Key = number "b" } ]
        Expect.equal (slotsOfDocument wrong) [ "root.entries[0].Value.TrailingComment" ] "The comment on the missing value is wrong"
        // A node with properties and no content has the comment behind the comma as its trailing comment.
        let tagged = YamlNode.empty |> YamlNode.withTag (YamlTag.core "str") |> YamlNode.withTrailingComment (comment " c")
        // [!!str , # c
        //  b]
        let propertyItem = { YamlNode.item tagged with Dash = None }
        expectValid (YamlNode.flowSequence [ propertyItem; { YamlNode.item (number "b") with Dash = None } ]) "property without content as a flow item"
        // {a: !!str, # c
        //  b: 1}
        expectValid (YamlNode.flowMapping [ field "a" tagged; field "b" (number "1") ]) "property without content as a flow value"
        // { !!str , # c
        //   b: 1 }
        let propertyKey = { YamlNode.entry tagged YamlNode.empty with Colon = None }
        expectValid (YamlNode.flowMapping [ propertyKey; field "b" (number "1") ]) "property without content as a flow key"
        // k: !!str # c
        Expect.equal (slotsOfDocument (YamlNode.mapping [ field "k" tagged ])) [ "root.entries[0].Value.TrailingComment" ] "The block context has the comment behind the property"

    // yaml@2.6.1 reads `{ a`, `# c`, `: b }` as {"a":"b"}, `? a`, `# c` and `: b` as {"a":"b"}, and `? a`, `# d` and `n: 1` as
    // {"a":null,"n":1}.
    testCase "R7: the gap before a colon belongs to BeforeColon" <| fun _ ->
        let flow = mappingOf CollectionStyle.Flow [ { field "a" (number "b") with BeforeColon = [ lineComment " c" ] } ]
        expectValid flow "implicit key in a flow mapping"
        let explicitKey = mappingOf CollectionStyle.Block [ { field "a" (number "b") with Question = Some Separation.Inline; BeforeColon = [ lineComment " c" ] } ]
        expectValid explicitKey "explicit key"
        let wrong = mappingOf CollectionStyle.Block [ { field "a" (number "b") with BeforeColon = [ lineComment " c" ] } ]
        Expect.equal (slotsOfDocument wrong) [ "root.entries[0].BeforeColon" ] "A block mapping has no gap before the colon of an implicit key"
        // ? a
        // # d
        // n: 1
        let noColon = { YamlNode.entry (number "a") YamlNode.empty with Question = Some Separation.Inline; Colon = None }
        let next = { field "n" (number "1") with Before = [ lineComment " d" ] }
        expectValid (YamlNode.mapping [ noColon; next ]) "explicit key without a colon, trivia in the next entry"
        Expect.equal
            (slotsOfDocument (YamlNode.mapping [ { noColon with BeforeColon = [ lineComment " d" ] }; field "n" (number "1") ]))
            [ "root.entries[0].BeforeColon" ]
            "The trivia in BeforeColon are wrong when no colon follows"
        // { ? a
        // # d
        // , n: 1 }
        Expect.equal
            (slotsOfDocument (mappingOf CollectionStyle.Flow [ { noColon with BeforeColon = [ lineComment " d" ] }; field "n" (number "1") ]))
            [ "root.entries[0].BeforeColon" ]
            "A flow mapping entry without a colon has no gap either"
        expectValid (mappingOf CollectionStyle.Flow [ noColon; next ]) "flow mapping, trivia in the next entry"

    // yaml@2.6.1 reads `k: v # c` as {"k":"v"}, `- &a x` and `- *a # c` as ["x","x"], `k: [1] # c` as {"k":[1]} and `k: | # h` followed by
    // `  text` as {"k":"text\n"}.
    testCase "R8: a trailing comment stands on the last token and a block scalar takes a header comment" <| fun _ ->
        let note = comment " c"
        expectValid (YamlNode.mapping [ field "k" (number "v" |> YamlNode.withTrailingComment note) ]) "scalar"
        expectValid (YamlNode.sequence [ YamlNode.item (number "x" |> YamlNode.withAnchor "a"); YamlNode.item (YamlNode.alias "a" |> YamlNode.withTrailingComment note) ]) "alias"
        expectValid (YamlNode.mapping [ field "k" (YamlNode.flowSequence [ YamlNode.item (number "1") ] |> YamlNode.withTrailingComment note) ]) "flow closer"
        expectValid (YamlNode.mapping [ field "k" (blockLiteral "text\n" |> YamlNode.withHeaderComment note) ]) "header comment"
        Expect.equal (slotsOfDocument (YamlNode.mapping [ field "k" { blockLiteral "text\n" with TrailingComment = Some note } ])) [ "root.entries[0].Value.TrailingComment" ] "block scalar"
        Expect.equal (slotsOfDocument { YamlNode.mapping [ field "k" (number "v") ] with TrailingComment = Some note }) [ "root.TrailingComment" ] "block mapping"
]

/// The stream of two documents, the first one with the given root and end marker.
let private twoDocuments (root: YamlNode) (endMarker: DocumentMarker option) : YamlStream =
    { Parts =
        [ StreamPart.Document { documentOf root with EndMarker = endMarker }
          StreamPart.Document (documentOf (number "b")) ] }

let tests_end_rule = testList "Slot rules: the End of a block collection" [
    // yaml@2.6.1 reads `a:`, `  b: 1`, `  # c` as {"a":{"b":1}}. A comment run between two nodes goes to the Before of the next node, so
    // only the collections on the path to the last node of the document can have an End.
    testCase "R4: the End of a block collection stands on the path to the last node" <| fun _ ->
        let inner = YamlNode.mapping [ field "b" (number "1") ] |> withEnd [ lineComment " c" ]
        expectValid (YamlNode.mapping [ field "x" (number "0"); field "a" inner ]) "the value of the last entry"
        expectValid (withEnd [ lineComment " c" ] (YamlNode.mapping [ field "a" (number "1") ])) "the root"
        expectValid (YamlNode.sequence [ YamlNode.item (number "0"); YamlNode.item inner ]) "the node of the last item"
        let outer = YamlNode.mapping [ field "a" inner ] |> withEnd [ lineComment " d" ]
        expectValid outer "two collections on the path"

    testCase "R4: the End of a collection in front of another entry or item is wrong" <| fun _ ->
        let inner = YamlNode.mapping [ field "b" (number "1") ] |> withEnd [ lineComment " c" ]
        Expect.equal (slotsOfDocument (YamlNode.mapping [ field "a" inner; field "x" (number "0") ])) [ "root.entries[0].Value.End" ] "an entry follows"
        Expect.equal (slotsOfDocument (YamlNode.sequence [ YamlNode.item inner; YamlNode.item (number "0") ])) [ "root.items[0].Node.End" ] "an item follows"
        let deep = YamlNode.mapping [ field "a" (YamlNode.mapping [ field "b" inner; field "c" (number "0") ]) ]
        Expect.equal (slotsOfDocument deep) [ "root.entries[0].Value.entries[0].Value.End" ] "a nested entry follows"

    // yaml@2.6.1 reads `? - x`, `# c`, `: v` as {"x":"v"}, and the comment stands between the key and the colon (rule R7).
    testCase "R4: the End of an explicit key is wrong when a colon follows" <| fun _ ->
        let key = YamlNode.sequence [ YamlNode.item (number "x") ] |> withEnd [ lineComment " c" ]
        let withColon = YamlNode.mapping [ { YamlNode.entry key (number "v") with Question = Some Separation.Inline } ]
        Expect.equal (slotsOfDocument withColon) [ "root.entries[0].Key.End" ] "A colon follows the key"
        let withoutColon = YamlNode.mapping [ { YamlNode.entry key YamlNode.empty with Question = Some Separation.Inline; Colon = None } ]
        expectValid withoutColon "The key ends the text"

    // yaml@2.6.1 reads `a: 1`, `# c`, `---` and `b` as two documents, and the comment stands in front of the second document. With a `...`
    // between the documents the trivia stay behind the first one.
    testCase "R4: the End of the root is wrong in front of a document without an end marker" <| fun _ ->
        let root = YamlNode.mapping [ field "a" (number "1") ] |> withEnd [ lineComment " c" ]
        Expect.equal (Validate.stream (twoDocuments root None)) [ "Parts[0].root.End: the trivia behind a block collection belong to its End only when the collection is the last node of its document, and otherwise to the Before of the next entry or item (R4)" ] "no end marker"
        Expect.equal (Validate.stream (twoDocuments root (Some { TrailingComment = None; Span = None }))) [] "an end marker"
]

let private keepScalar (text: string) : YamlNode =
    YamlNode.scalar (ScalarStyle.Block (BlockScalarStyle.Literal, ChompingMode.Keep, None)) text

let private blankLine: Trivia = BlankLine None

let private noMarker: DocumentMarker = { TrailingComment = None; Span = None }

let tests_keep_rule = testList "Slot rules: a blank line behind a Keep block scalar" [
    // yaml@2.6.1 reads `a: |+`, `  x`, an empty line and `b: 1` as {"a":"x\n\n","b":1}, and `- |+`, `  x`, an empty line and `- y` as ["x\n\n","y"].
    // The empty line is part of the value ([165] to [169]), so a BlankLine in the Before of the next entry or item cannot come from a text.
    testCase "a blank line in the Before of the next entry or item is wrong" <| fun _ ->
        let keep = keepScalar "x\n"
        let next = { field "b" (number "1") with Before = [ blankLine ] }
        Expect.equal (slotsOfDocument (YamlNode.mapping [ field "a" keep; next ])) [ "root.entries[0].Value.Style" ] "the next entry"
        let nextItem = { YamlNode.item (number "y") with Before = [ blankLine ] }
        Expect.equal (slotsOfDocument (YamlNode.sequence [ YamlNode.item keep; nextItem ])) [ "root.items[0].Node.Style" ] "the next item"
        let problems = Validate.document (documentOf (YamlNode.mapping [ field "a" keep; next ]))
        Expect.stringContains problems.Head "Keep" "The chomping"
        Expect.stringContains problems.Head "[165]" "The production"
        Expect.equal (slotsOfDocument (YamlNode.mapping [ field "a" (keepScalar ""); next ])) [ "root.entries[0].Value.Style" ] "an empty value"

    // yaml@2.6.1 reads `a: |+`, `  x` and an empty line as {"a":"x\n\n"}. The parser of this library reads `a: |+`, `  x` and a line that holds a byte order mark as the value "x\n"
    // and a blank line in the End of the mapping, because the mark ends the body and the document ([202], [211]). Only trivia and markers follow in the End of the last collection.
    testCase "a blank line in the End of the last collection is valid" <| fun _ ->
        let keep = keepScalar "x\n"
        expectValid (withEnd [ blankLine ] (YamlNode.mapping [ field "a" keep ])) "a mapping"
        expectValid (withEnd [ blankLine ] (YamlNode.sequence [ YamlNode.item keep ])) "a sequence"
        // The End of the root is behind the last node of every collection on the path.
        expectValid (YamlNode.mapping [ field "a" (YamlNode.mapping [ field "b" keep ]) ] |> withEnd [ blankLine ]) "two levels deep"
        // A key without a colon ends the text like a value.
        let keyAlone = { field "k" YamlNode.empty with Question = Some Separation.Inline; Key = keep; Colon = None }
        expectValid (withEnd [ blankLine ] (YamlNode.mapping [ keyAlone ])) "an explicit key without a colon"

    // yaml@2.6.1 reads `a:`, `  b:`, `    c: |+`, `      x`, an empty line and `d: 1` as {"a":{"b":{"c":"x\n\n"}},"d":1}.
    testCase "a blank line behind the last line of a collection that is nested in other collections is wrong" <| fun _ ->
        let keep = keepScalar "x\n"
        let next = { field "d" (number "1") with Before = [ blankLine ] }
        let deep = YamlNode.mapping [ field "a" (YamlNode.mapping [ field "b" (YamlNode.mapping [ field "c" keep ]) ]); next ]
        Expect.equal (slotsOfDocument deep) [ "root.entries[0].Value.entries[0].Value.entries[0].Value.Style" ] "mappings"
        let mixed = YamlNode.mapping [ field "a" (YamlNode.sequence [ YamlNode.item (YamlNode.mapping [ field "b" keep ]) ]); next ]
        Expect.equal (slotsOfDocument mixed) [ "root.entries[0].Value.items[0].Node.entries[0].Value.Style" ] "a mapping in a sequence in a mapping"
        // A scalar in front of another entry of its own collection has that entry behind it, whatever follows the collection.
        let inner = YamlNode.mapping [ field "b" keep; field "c" (number "2") ]
        expectValid (YamlNode.mapping [ field "a" inner; next ]) "an entry of the same collection follows"

    // yaml@2.6.1 reads `? |+`, `  x`, an empty line and `: w` as {"x\n\n":"w"}, and `? |+`, `  x`, an empty line and `d: 1` as {"x\n\n":null,"d":1}.
    testCase "a blank line behind an explicit key is wrong in the gap in front of the colon and in the next entry" <| fun _ ->
        let keep = keepScalar "x\n"
        let withGap = { field "k" (number "w") with Question = Some Separation.Inline; Key = keep; BeforeColon = [ blankLine ] }
        Expect.equal (slotsOfDocument (YamlNode.mapping [ withGap ])) [ "root.entries[0].Key.Style" ] "the gap in front of the colon"
        let keyAlone = { field "k" YamlNode.empty with Question = Some Separation.Inline; Key = keep; Colon = None }
        let next = { field "d" (number "1") with Before = [ blankLine ] }
        Expect.equal (slotsOfDocument (YamlNode.mapping [ keyAlone; next ])) [ "root.entries[0].Key.Style" ] "no colon, the next entry"
        // The colon line follows a key that has a colon, so the Before of the next entry stands behind the value.
        let withColon = { field "k" (number "w") with Question = Some Separation.Inline; Key = keep }
        expectValid (YamlNode.mapping [ withColon; next ]) "the colon line follows the key"

    // yaml@2.6.1 reads `|+`, `  x` and an empty line as "x\n\n", and `|+`, `  x`, an empty line, `--- y` as ["x\n\n","y"]. The parser of this library reads a line that holds a tab
    // behind a root scalar, or a line that holds a byte order mark, as a blank line in AfterRoot or in the prelude of the next document.
    testCase "a blank line behind the root or in the prelude of the next document is valid" <| fun _ ->
        let keep = keepScalar "x\n"
        expectValid keep "a root scalar"
        Expect.equal (Validate.document { documentOf keep with AfterRoot = [ blankLine ] }) [] "AfterRoot"
        let second prelude = StreamPart.Document { documentOf (number "y") with Prelude = prelude; StartMarker = Some noMarker }
        let stream (root: YamlNode) (prelude: PreludeItem list) : YamlStream = { Parts = [ StreamPart.Document (documentOf root); second prelude ] }
        Expect.equal (Validate.stream (stream keep [ PreludeTrivia blankLine ])) [] "the prelude behind a root scalar"
        let collection = YamlNode.mapping [ field "a" keep ]
        Expect.equal (Validate.stream (stream collection [ PreludeTrivia blankLine ])) [] "the prelude behind a collection"
        // The writer puts `...` in front of a prelude with a directive, and a comment line ends the empty lines.
        let directive = PreludeItem.Directive { Kind = DirectiveKind.YamlVersion (1, 2); TrailingComment = None; Span = None }
        Expect.equal (Validate.stream (stream keep [ PreludeTrivia blankLine; directive ])) [] "a directive in the prelude"
        Expect.equal (Validate.stream (stream keep [ PreludeTrivia (lineComment " c"); PreludeTrivia blankLine ])) [] "a comment first"
        let closed = StreamPart.Document { documentOf keep with EndMarker = Some noMarker }
        Expect.equal (Validate.stream { Parts = [ closed; second [ PreludeTrivia blankLine ] ] }) [] "an end marker in between"

    // yaml@2.6.1 reads `a: |+`, `  x`, `# c`, an empty line and `b: 1` as {"a":"x\n","b":1}, and the same text with an empty line in front of the comment as {"a":"x\n\n","b":1}.
    // A comment line ends the empty lines of the scalar (l-trail-comments), so a blank line behind the comment line is trivia.
    testCase "a blank line behind a comment line, a Strip or Clip scalar and a scalar in a flow collection are valid" <| fun _ ->
        let keep = keepScalar "x\n"
        let behindComment = { field "b" (number "1") with Before = [ lineComment " c"; blankLine ] }
        expectValid (YamlNode.mapping [ field "a" keep; behindComment ]) "a comment line in between"
        expectValid (withEnd [ lineComment " c"; blankLine ] (YamlNode.mapping [ field "a" keep ])) "a comment line in the End"
        let next = { field "b" (number "1") with Before = [ blankLine ] }
        for chomping in [ ChompingMode.Strip; ChompingMode.Clip ] do
            let scalar = YamlNode.scalar (ScalarStyle.Block (BlockScalarStyle.Literal, chomping, None)) "x\n"
            expectValid (YamlNode.mapping [ field "a" scalar; next ]) (sprintf "the chomping %A" chomping)
        expectValid (YamlNode.mapping [ field "a" keep; field "b" (number "1") ]) "no trivia"
        expectValid (YamlNode.mapping [ field "a" (YamlNode.flowSequence [ YamlNode.item keep; { YamlNode.item (number "y") with Before = [ blankLine ] } ]) ]) "a flow collection writes another style"

    // The rule applies to a stored Keep that is the chomping written for the value, which is a value that can stand in a block scalar and is empty or ends in a line feed. A stored Keep on a
    // value without a final line feed, as the parser reads `a: |+` with the body `  abc` at the end of the input, is a style mismatch that the policies of the writer handle (PreferStored
    // writes Strip, RequireStored raises). The blank line behind it is trivia. yaml@2.6.1 reads the texts with a blank line behind the Keep scalar as the value with the empty lines.
    testCase "a stored Keep that does not fit the value leaves the blank line as trivia" <| fun _ ->
        let nextEntry = { field "b" (number "1") with Before = [ blankLine ] }
        let nextItem = { YamlNode.item (number "y") with Before = [ blankLine ] }
        let cases =
            [ "abc", false
              "abc\n", true
              "", true
              "abc\n\n", true
              "\n", true
              "a\nb", false
              "a\nb\n\n", true
              "a" + string (char 1) + "\n", false
              "a" + string (char 0xFEFF) + "\n", false ]
        for (value, rejected) in cases do
            let keep = keepScalar value
            let slots (expected: string) = if rejected then [ expected ] else []
            Expect.equal (slotsOfDocument (YamlNode.mapping [ field "a" keep; nextEntry ])) (slots "root.entries[0].Value.Style") (sprintf "the next entry after %A" value)
            Expect.equal (slotsOfDocument (YamlNode.sequence [ YamlNode.item keep; nextItem ])) (slots "root.items[0].Node.Style") (sprintf "the next item after %A" value)
            let withGap = { field "k" (number "w") with Question = Some Separation.Inline; Key = keep; BeforeColon = [ blankLine ] }
            Expect.equal (slotsOfDocument (YamlNode.mapping [ withGap ])) (slots "root.entries[0].Key.Style") (sprintf "the gap in front of the colon after %A" value)
        // The parser reads the body at the end of the input with the stored Keep, and the value has no final line feed.
        let parsed = parseOk "a: |+\n  abc"
        let scalar =
            match parsed.Parts with
            | [ StreamPart.Document document ] ->
                match document.Root.Kind with
                | YamlNodeKind.Mapping collection -> collection.Items.Head.Value
                | _ -> failwith "A mapping was expected"
            | _ -> failwith "One document was expected"
        Expect.equal (slotsOfDocument (YamlNode.mapping [ field "a" scalar; nextEntry ])) [] "the node that the parser produced"
]

let tests_end_run_rule = testList "Slot rules: the run of trivia behind the last node" [
    // The parser reads `a:`, `  b: c`, an empty line and `# d` at the first column as a root End of [blank, comment], because a blank line goes with the comment that follows it. It reads
    // `a:`, `  b: c`, `  # d` and an empty line as an End of the inner mapping with [comment, blank], because the blank lines behind the last comment stay with that comment.
    testCase "a nested End ends with a comment unless it holds the last comment of the run" <| fun _ ->
        let note = lineComment " d"
        let tree (inner: Trivia list) (outer: Trivia list) =
            withEnd outer (YamlNode.mapping [ field "a" (withEnd inner (YamlNode.mapping [ field "b" (number "c") ])) ])
        let wrongEnd = [ "root.entries[0].Value.End" ]
        Expect.equal (slotsOfDocument (tree [ blankLine ] [])) wrongEnd "a blank line alone in the inner End"
        Expect.equal (slotsOfDocument (tree [ blankLine ] [ note ])) wrongEnd "a blank line in the inner End and a comment in the root End"
        Expect.equal (slotsOfDocument (tree [ note; blankLine ] [ note ])) wrongEnd "the inner End ends with a blank line and the last comment stands behind it"
        Expect.equal (slotsOfDocument (tree [ blankLine; note; blankLine ] [ note ])) wrongEnd "a blank line before and behind a comment"
        Expect.equal (slotsOfDocument (tree [ note ] [ blankLine ])) [ "root.End" ] "a blank line in the root End behind the last comment of the inner End"
        Expect.equal (slotsOfDocument (tree [ note ] [ blankLine; note ])) [] "a comment in both Ends with a blank line in front of the second"
        expectValid (tree [] [ blankLine ]) "a blank line alone in the root End"
        expectValid (tree [] [ blankLine; note; blankLine ]) "blank lines around a comment in the root End"
        expectValid (tree [ note ] []) "a comment in the inner End"
        expectValid (tree [ note; blankLine ] []) "a blank line behind the last comment, in the inner End"
        expectValid (tree [ blankLine; note; blankLine ] []) "blank lines around the last comment, in the inner End"
        expectValid (tree [ blankLine; note ] [ note; blankLine ]) "the root End holds the last comment"

    // The same run along three levels and through a sequence. The problem names the End that breaks the rule.
    testCase "the rule follows the last node through sequences and several levels" <| fun _ ->
        let note = lineComment " d"
        let deep (innermost: Trivia list) (middle: Trivia list) (outer: Trivia list) =
            let inner = withEnd innermost (YamlNode.mapping [ field "c" (number "1") ])
            let middleNode = withEnd middle (YamlNode.sequence [ YamlNode.item inner ])
            withEnd outer (YamlNode.mapping [ field "a" middleNode ])
        Expect.equal (slotsOfDocument (deep [ blankLine ] [ note ] [])) [ "root.entries[0].Value.items[0].Node.End" ] "the innermost End has a blank line and a comment follows"
        Expect.equal (slotsOfDocument (deep [ note ] [ blankLine ] [])) [ "root.entries[0].Value.End" ] "the middle End holds a blank line behind the last comment"
        Expect.equal (slotsOfDocument (deep [] [] [ blankLine ])) [] "the root End holds the blank line"
        Expect.equal (slotsOfDocument (deep [] [ blankLine ] [])) [ "root.entries[0].Value.End" ] "the middle End holds a blank line and no comment stands in the run"
        Expect.equal (slotsOfDocument (deep [ note; blankLine ] [] [])) [] "the innermost End holds the comment and the blank line behind it"
        Expect.equal (slotsOfDocument (deep [ note ] [ note ] [ note ])) [] "a comment in every End"
        Expect.equal (slotsOfDocument (deep [ note ] [ blankLine; note ] [ blankLine; note ])) [] "a blank line in front of the comments of the outer Ends"

    // An End behind a collection that is not the last node of its document is wrong already, and the run rule adds no second problem.
    testCase "the run rule leaves a document that is followed by another one to the End rule" <| fun _ ->
        let nested = withEnd [ blankLine ] (YamlNode.mapping [ field "b" (number "c") ])
        let first = StreamPart.Document (documentOf (YamlNode.mapping [ field "a" nested ]))
        let second = StreamPart.Document { documentOf (number "y") with StartMarker = Some noMarker }
        Expect.equal (slotsOf (Validate.stream { Parts = [ first; second ] })) [ "Parts[0].root.entries[0].Value.End" ] "one problem"

    // yaml@2.6.1 rejects `!!` followed by a blank, and `!<>`.
    testCase "a named tag with an empty suffix is wrong" <| fun _ ->
        let tagged (tag: YamlTag) =
            { number "v" with
                Properties = { Tag = Some tag; Anchor = None }
                PropertyLayout = [ placement PropertyName.Tag Separation.Inline ] }
        for (label, name) in [ "core", { Prefix = YamlTag.coreNamespace; Suffix = "" }; "local", { Prefix = "!"; Suffix = "" }; "verbatim", { Prefix = ""; Suffix = "" } ] do
            Expect.equal (slotsOfDocument (tagged (YamlTag.Named name))) [ "root.Properties.Tag" ] (sprintf "the %s tag" label)
        expectValid (tagged (YamlTag.core "str")) "a core tag"
        expectValid (tagged (YamlTag.local "x")) "a local tag"
        expectValid (tagged (YamlTag.NonSpecific)) "the non-specific tag"
        expectValid (tagged (YamlTag.Named { Prefix = ""; Suffix = "tag:example.com,2000:x" })) "a verbatim tag"
]

let tests_line_break_rule = testList "Slot rules: a block collection starts on a later line" [
    // yaml@2.6.1 reads `k:` followed by `  - a` as {"k":["a"]} and rejects `k: - a` and `k: a: 1` (a block collection cannot start behind an
    // implicit key on its line).
    testCase "R1: the colon of an implicit entry has a line break in front of a block collection without a property" <| fun _ ->
        let sequence = YamlNode.sequence [ YamlNode.item (number "a") ]
        let mapping = YamlNode.mapping [ field "a" (number "1") ]
        let onColonLine (value: YamlNode) = { field "k" YamlNode.empty with Value = value }
        Expect.equal (slotsOfDocument (YamlNode.mapping [ onColonLine sequence ])) [ "root.entries[0].Colon" ] "sequence"
        Expect.equal (slotsOfDocument (YamlNode.mapping [ onColonLine mapping ])) [ "root.entries[0].Colon" ] "mapping"
        expectValid (YamlNode.mapping [ field "k" sequence; field "j" mapping ]) "entries that the constructor builds"
        // A flow collection, a scalar and a missing value stay on the line of the colon.
        expectValid (YamlNode.mapping [ onColonLine (YamlNode.flowSequence [ YamlNode.item (number "a") ]); onColonLine (number "v"); onColonLine YamlNode.empty ]) "inline values"

    // yaml@2.6.1 reads `- - a`, `- a: 1`, `? - a` followed by `: v` and `? a` followed by `: - b` without an error.
    testCase "R1: an item and an explicit entry may continue the line with a block collection" <| fun _ ->
        let sequence = YamlNode.sequence [ YamlNode.item (number "a") ]
        expectValid (YamlNode.sequence [ YamlNode.item sequence; YamlNode.item (YamlNode.mapping [ field "a" (number "1") ]) ]) "items"
        let explicitKey = { field "k" (number "v") with Question = Some Separation.Inline; Key = sequence }
        expectValid (YamlNode.mapping [ explicitKey ]) "explicit key"
        let explicitValue = { field "k" sequence with Question = Some Separation.Inline; Colon = Some Separation.Inline }
        expectValid (YamlNode.mapping [ explicitValue ]) "explicit value"

    // yaml@2.6.1 reads `k: !!seq` followed by `  - a` as {"k":["a"]} and rejects `k: !!seq - a`.
    testCase "R1: a line break follows the last property of a block collection" <| fun _ ->
        let sequence = YamlNode.sequence [ YamlNode.item (number "a") ]
        let tagged = sequence |> YamlNode.withTag (YamlTag.core "seq")
        expectValid (YamlNode.mapping [ field "k" tagged ]) "the constructors give the line break"
        expectValid (YamlNode.sequence [ YamlNode.item tagged ]) "an item"
        expectValid tagged "the root"
        let wrong = { tagged with PropertyLayout = [ placement PropertyName.Tag Separation.Inline ] }
        Expect.equal (slotsOfDocument wrong) [ "root.PropertyLayout[0].After" ] "the root"
        Expect.equal (slotsOfDocument (YamlNode.mapping [ field "k" wrong ])) [ "root.entries[0].Value.PropertyLayout[0].After" ] "an entry"
        Expect.equal (slotsOfDocument (YamlNode.sequence [ YamlNode.item wrong ])) [ "root.items[0].Node.PropertyLayout[0].After" ] "an item"
        // With two properties the line break stands behind the last one only.
        let both = tagged |> YamlNode.withAnchor "a"
        expectValid both "tag, then anchor"
        let swapped = { both with PropertyLayout = [ placement PropertyName.Tag (broken []); placement PropertyName.Anchor Separation.Inline ] }
        Expect.equal (slotsOfDocument swapped) [ "root.PropertyLayout[1].After" ] "the last property has no line break"
        // A flow collection, an empty collection and a scalar need no line break.
        expectValid (YamlNode.flowSequence [ YamlNode.item (number "a") ] |> YamlNode.withTag (YamlTag.core "seq")) "flow sequence"
        expectValid (YamlNode.sequence [] |> YamlNode.withTag (YamlTag.core "seq")) "empty sequence"

    // yaml@2.6.1 reads `- # c` followed by `  - a` as [["a"]] with the comment on the dash, and `- - a` as [["a"]]. A comment line in front of
    // the dash of the inner collection is trivia of the outer item, so the inner collection cannot hold it in front of its first item.
    testCase "R2: a block collection that continues the line of an indicator has no trivia in front of its first entry or item" <| fun _ ->
        let withComment = YamlNode.sequence [ { YamlNode.item (number "a") with Before = [ lineComment " c" ] } ]
        let mappingWithComment = YamlNode.mapping [ { field "a" (number "1") with Before = [ lineComment " c" ] } ]
        Expect.equal (slotsOfDocument (YamlNode.sequence [ YamlNode.item withComment ])) [ "root.items[0].Node.items[0].Before" ] "sequence behind a dash"
        Expect.equal (slotsOfDocument (YamlNode.sequence [ YamlNode.item mappingWithComment ])) [ "root.items[0].Node.entries[0].Before" ] "mapping behind a dash"
        let explicitKey = { field "k" (number "v") with Question = Some Separation.Inline; Key = withComment }
        Expect.equal (slotsOfDocument (YamlNode.mapping [ explicitKey ])) [ "root.entries[0].Key.items[0].Before" ] "behind a question mark"
        let explicitValue = { field "k" withComment with Question = Some Separation.Inline; Colon = Some Separation.Inline }
        Expect.equal (slotsOfDocument (YamlNode.mapping [ explicitValue ])) [ "root.entries[0].Value.items[0].Before" ] "behind an explicit colon"
        // A line break behind the indicator, a property in front of the collection and the colon of an implicit entry hold the trivia.
        expectValid (YamlNode.sequence [ { YamlNode.item withComment with Dash = Some (broken []) } ]) "dash with a line break"
        expectValid (YamlNode.mapping [ { explicitKey with Question = Some (broken []) } ]) "question mark with a line break"
        expectValid (YamlNode.mapping [ { explicitValue with Colon = Some (broken []) } ]) "explicit colon with a line break"
        expectValid (YamlNode.sequence [ YamlNode.item (withComment |> YamlNode.withTag (YamlTag.core "seq")) ]) "a property in front of the collection"
        expectValid (YamlNode.mapping [ field "k" withComment ]) "the value of an implicit entry"
]

// Characters that a test needs and a source file cannot show: built at run time, so that no editor or compiler changes them.
let private byteOrderMark = string (char 0xFEFF)
let private nextLine = string (char 0x85)
let private lineSeparator = string (char 0x2028)
let private loneSurrogate = string (char 0xD800)

let private directiveOf (kind: DirectiveKind) : PreludeItem =
    PreludeItem.Directive { Kind = kind; TrailingComment = None; Span = None }

/// The problems of a stream of one document whose prelude holds the items.
let private slotsOfPrelude (items: PreludeItem list) : string list =
    slotsOf (Validate.stream { Parts = [ StreamPart.Document { documentOf (number "a") with Prelude = items; StartMarker = Some { TrailingComment = None; Span = None } } ] })

let tests_character_rules = testList "Slot rules: comments, names, entries and directives" [
    // The text of a comment follows the `#` on the line of a comment, so it holds nb-char characters only ([27]): no line break, no control character, no byte order mark and no half of
    // a surrogate pair. yaml@2.6.1 reads a comment line with NEL, a line separator, a tab or an emoji as a comment, and a comment line with a byte order mark or a control character as an error.
    testCase "a comment holds nb-char characters only" <| fun _ ->
        for text in [ " a\nb"; " a\rb"; " a" + byteOrderMark; " a\001b"; " a" + loneSurrogate; " a\127b" ] do
            let bad = comment text
            Expect.equal (slotsOfDocument (YamlNode.plain "v" |> YamlNode.withTrailingComment bad)) [ "root.TrailingComment" ] (sprintf "TrailingComment %A" text)
            Expect.equal (slotsOfDocument (blockLiteral "x\n" |> YamlNode.withHeaderComment bad)) [ "root.HeaderComment" ] (sprintf "HeaderComment %A" text)
            Expect.equal (slotsOfDocument (YamlNode.flowSequence [] |> YamlNode.withOpeningComment bad)) [ "root.OpeningComment" ] (sprintf "OpeningComment %A" text)
        let bad = comment (" a" + byteOrderMark)
        let line = FullLineComment bad
        let entry = { field "a" (number "1") with Before = [ line ] }
        Expect.equal (slotsOfDocument (YamlNode.mapping [ entry ])) [ "root.entries[0].Before[0]" ] "Before of an entry"
        Expect.equal (slotsOfDocument (YamlNode.sequence [ { YamlNode.item (number "1") with Before = [ blankLine; line ] } ])) [ "root.items[0].Before[1]" ] "Before of an item"
        Expect.equal (slotsOfDocument (withEnd [ line ] (YamlNode.mapping [ field "a" (number "1") ]))) [ "root.End[0]" ] "End"
        let explicitKey = { field "k" (number "v") with Question = Some (commented " q" [ line ]); BeforeColon = [ line ]; Colon = Some (commented " c" [ line ]) }
        Expect.equal
            (slotsOfDocument (YamlNode.mapping [ explicitKey ]))
            [ "root.entries[0].Question.Following[0]"; "root.entries[0].BeforeColon[0]"; "root.entries[0].Colon.Following[0]" ]
            "Question, BeforeColon and Colon"
        Expect.equal (slotsOfDocument (YamlNode.sequence [ { YamlNode.item (number "1") with Dash = Some (commented (" a" + byteOrderMark) []) } ])) [ "root.items[0].Dash.Comment" ] "Dash"
        let tagged = YamlNode.withTag (YamlTag.core "str") YamlNode.empty |> fun node -> { node with PropertyLayout = [ placement PropertyName.Tag (commented (" a" + byteOrderMark) []) ] }
        Expect.equal (slotsOfDocument tagged) [ "root.PropertyLayout[0].After.Comment" ] "After of a property"
        Expect.equal (slotsOf (Validate.document { documentOf (number "v") with BeforeRoot = [ line ]; AfterRoot = [ line ] })) [ "BeforeRoot[0]"; "AfterRoot[0]" ] "BeforeRoot and AfterRoot"
        let marker: DocumentMarker = { TrailingComment = Some bad; Span = None }
        Expect.equal (slotsOf (Validate.document { documentOf (number "v") with StartMarker = Some marker; EndMarker = Some marker })) [ "StartMarker.TrailingComment"; "EndMarker.TrailingComment" ] "Markers"
        Expect.equal (slotsOf (Validate.stream { Parts = [ StreamPart.ExtraEndMarker marker; StreamPart.Trivia line ] })) [ "Parts[0].TrailingComment"; "Parts[1]" ] "ExtraEndMarker and StreamPart.Trivia"
        let directiveWithComment = PreludeItem.Directive { Kind = DirectiveKind.ReservedDirective ("FOO", []); TrailingComment = Some bad; Span = None }
        Expect.equal (slotsOfPrelude [ PreludeItem.PreludeTrivia line; directiveWithComment ]) [ "Parts[0].Prelude[0]"; "Parts[0].Prelude[1].TrailingComment" ] "Prelude"

    testCase "a comment may hold a tab, NEL, the line separator and an astral character" <| fun _ ->
        let text = " a\tb" + nextLine + lineSeparator + "\U0001F600"
        expectValid (YamlNode.plain "v" |> YamlNode.withTrailingComment (comment text)) "TrailingComment"
        expectValid (YamlNode.mapping [ { field "a" (number "1") with Before = [ FullLineComment (comment text) ] } ]) "Before"

    // The name behind `&` or `*` is one or more ns-anchor-char characters ([102]): no blank, no line break, no flow indicator, no control character and no byte order mark.
    // yaml@2.6.1 reads `&a` followed by a comma or a bracket as an anchor `a` and the rest as other tokens, so the name cannot hold them.
    testCase "an anchor or an alias name holds ns-anchor-char characters only" <| fun _ ->
        // The constructors reject an empty name, so the tests build the nodes as records.
        let anchored (name: string) = { number "v" with Properties = { Tag = None; Anchor = Some name }; PropertyLayout = [ placement PropertyName.Anchor Separation.Inline ] }
        let aliased (name: string) = { YamlNode.empty with Kind = YamlNodeKind.Alias name }
        for name in [ ""; "a b"; "a\tb"; "a,b"; "a[b"; "a]b"; "a{b"; "a}b"; "a\nb"; "a" + byteOrderMark + "b"; "a\001"; "a" + loneSurrogate ] do
            Expect.equal (slotsOfDocument (anchored name)) [ "root.Properties.Anchor" ] (sprintf "Anchor %A" name)
            Expect.equal (slotsOfDocument (aliased name)) [ "root" ] (sprintf "Alias %A" name)
        for name in [ "a"; "a-b_c"; "a:b"; string (char 0xE9); "a" + nextLine + "b"; "\U0001F600"; "a#b" ] do
            expectValid (anchored name) (sprintf "Anchor %A" name)
            expectValid (aliased name) (sprintf "Alias %A" name)

    // Model.fs says that the colon is omitted behind a key without a value only. A flow mapping entry that is a key alone and an explicit key without a value have no colon.
    testCase "an entry with a value has a colon" <| fun _ ->
        let noColon = { field "k" (number "v") with Colon = None }
        Expect.equal (slotsOfDocument (YamlNode.mapping [ noColon ])) [ "root.entries[0].Colon" ] "A block mapping"
        Expect.equal (slotsOfDocument (YamlNode.flowMapping [ noColon ])) [ "root.entries[0].Colon" ] "A flow mapping"
        let implicitAlone = { field "k" YamlNode.empty with Colon = None }
        Expect.equal (slotsOfDocument (YamlNode.mapping [ implicitAlone ])) [ "root.entries[0].Colon" ] "An implicit entry of a block mapping without a value"
        expectValid (YamlNode.flowMapping [ implicitAlone ]) "A flow key alone"
        expectValid (YamlNode.mapping [ { implicitAlone with Question = Some Separation.Inline } ]) "An explicit key without a value"

    testCase "a %YAML directive names a version 1.x and stands once" <| fun _ ->
        Expect.equal (slotsOfPrelude [ directiveOf (DirectiveKind.YamlVersion (1, 2)) ]) [] "1.2"
        Expect.equal (slotsOfPrelude [ directiveOf (DirectiveKind.YamlVersion (1, 1)) ]) [] "1.1"
        Expect.equal (slotsOfPrelude [ directiveOf (DirectiveKind.YamlVersion (2, 0)) ]) [ "Parts[0].Prelude[0].Kind" ] "2.0"
        Expect.equal (slotsOfPrelude [ directiveOf (DirectiveKind.YamlVersion (1, -1)) ]) [ "Parts[0].Prelude[0].Kind" ] "a negative minor number"
        Expect.equal (slotsOfPrelude [ directiveOf (DirectiveKind.YamlVersion (1, 2)); directiveOf (DirectiveKind.YamlVersion (1, 2)) ]) [ "Parts[0].Prelude[1].Kind" ] "twice"

    // yaml@2.6.1 reads `%TAG !e! tag:example.com,2000:` and rejects a handle without the exclamation marks and a second directive for the same handle.
    testCase "a %TAG directive has a handle and a prefix that the productions allow" <| fun _ ->
        let tag (handle: string) (prefix: string) = directiveOf (DirectiveKind.TagDirective (handle, prefix))
        for handle in [ "!"; "!!"; "!e!"; "!a-1!" ] do
            Expect.equal (slotsOfPrelude [ tag handle "tag:example.com,2000:" ]) [] (sprintf "Handle %s" handle)
        for handle in [ "e"; "!e"; "e!"; "!!!"; "!e e!"; "" ] do
            Expect.equal (slotsOfPrelude [ tag handle "tag:example.com,2000:" ]) [ "Parts[0].Prelude[0].Kind" ] (sprintf "Handle %s" handle)
        for prefix in [ ""; ",a"; "[a"; "a b"; "a%2"; "a" + byteOrderMark ] do
            Expect.equal (slotsOfPrelude [ tag "!e!" prefix ]) [ "Parts[0].Prelude[0].Kind" ] (sprintf "Prefix %A" prefix)
        for prefix in [ "!"; "!local-"; "tag:example.com,2000:"; "a%20b"; "a!b" ] do
            Expect.equal (slotsOfPrelude [ tag "!e!" prefix ]) [] (sprintf "Prefix %A" prefix)
        Expect.equal (slotsOfPrelude [ tag "!e!" "a"; tag "!e!" "b" ]) [ "Parts[0].Prelude[1].Kind" ] "a handle twice"
        Expect.equal (slotsOfPrelude [ tag "!e!" "a"; tag "!f!" "b" ]) [] "two handles"

    // A reserved directive is a name and parameters of ns-char characters ([82], [83]). A parameter that starts with a `#` would begin a comment, and `YAML` and `TAG` are no reserved names.
    testCase "a reserved directive has a name and parameters of ns-char characters" <| fun _ ->
        let reserved (name: string) (arguments: string list) = directiveOf (DirectiveKind.ReservedDirective (name, arguments))
        Expect.equal (slotsOfPrelude [ reserved "FOO" [ "a"; "b#c" ] ]) [] "valid"
        Expect.equal (slotsOfPrelude [ reserved "FOO" [] ]) [] "no parameter"
        for name in [ ""; "FOO BAR"; "F\tOO"; "FOO\n"; "YAML"; "TAG"; "F" + byteOrderMark ] do
            Expect.equal (slotsOfPrelude [ reserved name [] ]) [ "Parts[0].Prelude[0].Kind" ] (sprintf "Name %A" name)
        for argument in [ ""; "#c"; "a b"; "a\n"; "a\001"; "a" + byteOrderMark ] do
            Expect.equal (slotsOfPrelude [ reserved "FOO" [ argument ] ]) [ "Parts[0].Prelude[0].Kind" ] (sprintf "Parameter %A" argument)
]

let Main = testList "Validate" [
    tests_node_checks
    tests_rule_checks
    tests_rules
    tests_end_rule
    tests_keep_rule
    tests_end_run_rule
    tests_character_rules
    tests_line_break_rule
]
