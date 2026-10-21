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
        let content: YamlScalar = { Value = "a"; Style = ScalarStyle.Plain; HeaderComment = Some (comment " h") }
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
        let betweenProperties = { both with PropertyLayout = [ placement PropertyName.Tag (broken [ lineComment " d" ]); placement PropertyName.Anchor Separation.Inline ] }
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

    testCase "trivia of the stream stand behind a ... or at the start of the stream in front of an extra end marker and nowhere else" <| fun _ ->
        let marker: DocumentMarker = { TrailingComment = None; Span = None }
        let document (text: string) = StreamPart.Document (YamlDocument.create (number text))
        let closed (text: string) = StreamPart.Document { YamlDocument.create (number text) with EndMarker = Some marker }
        let extra = StreamPart.ExtraEndMarker marker
        let trivia = StreamPart.Trivia (lineComment " t")
        let slotsOfParts (parts: StreamPart list) = slotsOf (Validate.stream { Parts = parts })
        Expect.equal (slotsOfParts []) [] "empty stream"
        Expect.equal (slotsOfParts [ trivia ]) [] "only trivia"
        Expect.equal (slotsOfParts [ trivia; extra ]) [] "trivia and an extra end marker without a document"
        Expect.equal (slotsOfParts [ closed "a"; trivia ]) [] "trivia behind the end marker of the last document"
        Expect.equal (slotsOfParts [ closed "a"; trivia; trivia ]) [] "a run of trivia behind the end marker"
        Expect.equal (slotsOfParts [ closed "a"; extra; trivia ]) [] "trivia behind an extra end marker"
        Expect.equal (slotsOfParts [ document "a"; extra; trivia ]) [] "An extra end marker closes the document"
        Expect.equal (slotsOfParts [ closed "a"; trivia; extra ]) [] "trivia between an end marker and an extra end marker"
        Expect.equal (slotsOfParts [ closed "a"; trivia; extra; trivia ]) [] "trivia on both sides of an extra end marker"
        Expect.equal (slotsOfParts [ document "a"; trivia ]) [ "Parts[1]" ] "trivia behind a document without a ..."
        Expect.equal (slotsOfParts [ trivia; document "a" ]) [ "Parts[0]" ] "trivia in front of a document"
        Expect.equal (slotsOfParts [ closed "a"; trivia; document "b" ]) [ "Parts[1]" ] "trivia between two documents"
        Expect.equal (slotsOfParts [ closed "a"; trivia; trivia; document "b" ]) [ "Parts[1]"; "Parts[2]" ] "a run of trivia in front of a document"
        Expect.equal (slotsOfParts [ closed "a"; trivia; extra; document "b" ]) [] "trivia between an end marker and an extra end marker, then a document"
        // By [211] a stream is l-document-prefix* (no document) l-document-suffix and then a bare document, so the trivia in front of a `...` that ends no document belong to no document.
        Expect.equal (slotsOfParts [ trivia; extra; document "a" ]) [] "trivia in front of an extra end marker at the start of a stream with a document"
        Expect.equal (slotsOfParts [ trivia; trivia; extra; trivia; extra; document "b" ]) [] "trivia in front of two extra end markers, then a document"
        Expect.equal (slotsOfParts [ trivia; extra; trivia; document "b" ]) [ "Parts[2]" ] "trivia behind the extra end marker and in front of the document stay invalid"
        Expect.equal (slotsOfParts [ trivia; extra; closed "a"; trivia; document "b" ]) [ "Parts[3]" ] "trivia in front of a document stay invalid when an extra end marker stands earlier"
        // A `...` behind a document without an end marker would end that document, so the stream has no extra end marker there and the trivia belong to the document.
        Expect.equal (slotsOfParts [ document "a"; trivia; extra ]) [ "Parts[1]" ] "trivia between a document without a ... and an extra end marker"
        Expect.equal (slotsOfParts [ document "a"; trivia; trivia; extra ]) [ "Parts[1]"; "Parts[2]" ] "a run of trivia between a document without a ... and an extra end marker"
        Expect.equal (slotsOfParts [ closed "a"; extra; trivia; extra ]) [] "trivia between two extra end markers behind a document"
        let withMarker = StreamPart.Document { YamlDocument.create (number "a") with StartMarker = Some marker }
        Expect.equal (slotsOfParts [ trivia; withMarker ]) [ "Parts[0]" ] "trivia in front of a document with a start marker"
        let problems = Validate.stream { Parts = [ trivia; document "a" ] }
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
        let behindLast: YamlStream = { Parts = [ StreamPart.Document { first with EndMarker = marker }; StreamPart.Trivia (lineComment " t") ] }
        Expect.equal (Validate.stream behindLast) [] "trivia behind the last ..."
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

let Main = testList "Validate" [
    tests_node_checks
    tests_rule_checks
    tests_rules
]
