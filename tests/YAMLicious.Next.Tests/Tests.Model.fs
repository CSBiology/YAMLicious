module Tests.Model

open Fable.Pyxpecto
open YAMLicious

let private scalarOf (node: YamlNode) : YamlScalar =
    match YamlNode.tryScalar node with
    | Some content -> content
    | None -> failwith "The node is not a scalar."

let private placement (property: PropertyName) (after: Separation) : PropertyPlacement =
    { Property = property
      After = after
      Span = None }

let private lineBreak (text: string) : Separation =
    Separation.NewLine
        { Comment = Some (comment text)
          Following = [] }

let private field (key: string) (value: YamlNode) = YamlNode.entry (YamlNode.plain key) value

let private aliasName (node: YamlNode) : string option =
    match node.Kind with
    | YamlNodeKind.Alias name -> Some name
    | _ -> None

/// The only entry of a mapping node.
let private onlyEntry (node: YamlNode) : MappingEntry =
    match YamlNode.entries node with
    | [ single ] -> single
    | other -> failwith ("The mapping has " + string other.Length + " entries, not one.")

/// The only item of a sequence node.
let private onlyItem (node: YamlNode) : SequenceItem =
    match YamlNode.items node with
    | [ single ] -> single
    | other -> failwith ("The sequence has " + string other.Length + " items, not one.")

let tests_scalars = testList "Scalar constructors" [
    testCase "plain makes a plain scalar without properties, comments, layout and span" <| fun _ ->
        let node = YamlNode.plain "abc"
        let content = scalarOf node
        Expect.equal content.Value "abc" "value"
        Expect.equal content.Style ScalarStyle.Plain "style"
        Expect.equal content.HeaderComment None "no header comment"
        Expect.equal node.Properties { Tag = None; Anchor = None } "no properties"
        Expect.equal node.PropertyLayout [] "empty layout"
        Expect.equal node.TrailingComment None "no trailing comment"
        Expect.equal node.Span None "no span"

    testCase "scalar keeps the style that the caller names" <| fun _ ->
        for style in [ ScalarStyle.Plain; ScalarStyle.SingleQuoted; ScalarStyle.DoubleQuoted
                       ScalarStyle.Block (BlockScalarStyle.Literal, ChompingMode.Clip, None)
                       ScalarStyle.Block (BlockScalarStyle.Folded, ChompingMode.Keep, Some 2) ] do
            let content = scalarOf (YamlNode.scalar style "text")
            Expect.equal content.Style style "style"
            Expect.equal content.Value "text" "value"

    // yaml@2.6.1 reads `k:` as {"k":null}. The tree keeps the missing value as a node, and the core schema
    // decides that it means null.
    testCase "empty is a plain scalar with the text of nothing" <| fun _ ->
        let content = scalarOf YamlNode.empty
        Expect.equal content.Value "" "value"
        Expect.equal content.Style ScalarStyle.Plain "style"
        Expect.equal YamlNode.empty.Span None "A constructed node has no span"

    testCase "null and ~ stay plain scalars with their spelling" <| fun _ ->
        for text in [ "null"; "~"; "Null" ] do
            let content = scalarOf (YamlNode.plain text)
            Expect.equal content.Value text "The spelling stays"
            Expect.equal content.Style ScalarStyle.Plain "style"

    testCase "alias holds the anchor name and nothing else" <| fun _ ->
        let node = YamlNode.alias "base"
        Expect.equal (aliasName node) (Some "base") "kind"
        Expect.equal node.Properties { Tag = None; Anchor = None } "no properties"
        Expect.equal (YamlNode.tryScalar node |> Option.isNone) true "An alias is no scalar"
]

let tests_collections = testList "Collection constructors" [
    testCase "an empty mapping and an empty sequence are flow collections" <| fun _ ->
        Expect.equal (YamlNode.tryMapping (YamlNode.mapping [])).Value.Style CollectionStyle.Flow "{}"
        Expect.equal (YamlNode.trySequence (YamlNode.sequence [])).Value.Style CollectionStyle.Flow "[]"

    testCase "a mapping and a sequence with content are block collections" <| fun _ ->
        let mapping = YamlNode.mapping [ field "a" (YamlNode.plain "1") ]
        let sequence = YamlNode.sequence [ YamlNode.item (YamlNode.plain "1") ]
        Expect.equal (YamlNode.tryMapping mapping).Value.Style CollectionStyle.Block "mapping"
        Expect.equal (YamlNode.trySequence sequence).Value.Style CollectionStyle.Block "sequence"

    testCase "a collection has no opening comment and no end trivia" <| fun _ ->
        let content = (YamlNode.tryMapping (YamlNode.mapping [ field "a" YamlNode.empty ])).Value
        Expect.equal content.OpeningComment None "no opening comment"
        Expect.equal content.End [] "no end trivia"

    testCase "flowMapping and flowSequence make flow collections" <| fun _ ->
        let mapping = YamlNode.flowMapping [ field "a" (YamlNode.plain "1") ]
        Expect.equal (YamlNode.tryMapping mapping).Value.Style CollectionStyle.Flow "mapping"
        let sequence = YamlNode.flowSequence [ YamlNode.item (YamlNode.plain "1") ]
        Expect.equal (YamlNode.trySequence sequence).Value.Style CollectionStyle.Flow "sequence"

    testCase "a flow item has no dash" <| fun _ ->
        let sequence = YamlNode.flowSequence [ YamlNode.item (YamlNode.plain "1"); YamlNode.item (YamlNode.plain "2") ]
        for item in YamlNode.items sequence do
            Expect.equal item.Dash None "A flow item has no dash"

    testCase "entries and items keep their order" <| fun _ ->
        let mapping = YamlNode.mapping [ field "b" YamlNode.empty; field "a" YamlNode.empty; field "c" YamlNode.empty ]
        let keys = YamlNode.entries mapping |> List.map (fun e -> (scalarOf e.Key).Value)
        Expect.equal keys [ "b"; "a"; "c" ] "entry order"
        let sequence = YamlNode.sequence [ YamlNode.item (YamlNode.plain "z"); YamlNode.item (YamlNode.plain "y") ]
        let values = YamlNode.items sequence |> List.map (fun i -> (scalarOf i.Node).Value)
        Expect.equal values [ "z"; "y" ] "item order"

    // yaml@2.6.1 rejects a map with duplicate keys by default. The tree keeps both entries and leaves the
    // rejection to the decoder.
    testCase "duplicate keys stay in the list" <| fun _ ->
        let mapping = YamlNode.mapping [ field "a" (YamlNode.plain "1"); field "a" (YamlNode.plain "2") ]
        let values = YamlNode.entries mapping |> List.map (fun e -> (scalarOf e.Value).Value)
        Expect.equal values [ "1"; "2" ] "Both entries stay"

    testCase "entry has an inline colon and no explicit key marker" <| fun _ ->
        let made = field "k" (YamlNode.plain "v")
        Expect.equal made.Before [] "no trivia in front"
        Expect.equal made.Question None "no question mark"
        Expect.equal made.BeforeColon [] "no trivia in front of the colon"
        Expect.equal made.Colon (Some Separation.Inline) "inline colon"
        Expect.equal made.Span None "no span"

    testCase "item has an inline dash" <| fun _ ->
        let made = YamlNode.item (YamlNode.plain "v")
        Expect.equal made.Before [] "no trivia in front"
        Expect.equal made.Dash (Some Separation.Inline) "inline dash"
        Expect.equal made.Span None "no span"

    // yaml@2.6.1 reads a key that is a sequence, as in `? [a, b]`, as a key of the map.
    testCase "a key is a full node" <| fun _ ->
        let key = YamlNode.flowSequence [ YamlNode.item (YamlNode.plain "a") ]
        let mapping = YamlNode.mapping [ YamlNode.entry key (YamlNode.plain "v") ]
        Expect.equal (YamlNode.trySequence (onlyEntry mapping).Key |> Option.isSome) true "The key is a sequence"
        let aliasKey = YamlNode.mapping [ YamlNode.entry (YamlNode.alias "x") YamlNode.empty ]
        Expect.equal (aliasName (onlyEntry aliasKey).Key) (Some "x") "The key is an alias"
]

let tests_accessors = testList "Accessors" [
    testCase "tryScalar, tryMapping and trySequence answer for their own kind only" <| fun _ ->
        let scalar = YamlNode.plain "a"
        let mapping = YamlNode.mapping [ field "a" YamlNode.empty ]
        let sequence = YamlNode.sequence [ YamlNode.item YamlNode.empty ]
        let alias = YamlNode.alias "a"
        Expect.equal [ YamlNode.tryScalar scalar |> Option.isSome; YamlNode.tryScalar mapping |> Option.isSome
                       YamlNode.tryScalar sequence |> Option.isSome; YamlNode.tryScalar alias |> Option.isSome ]
            [ true; false; false; false ] "tryScalar"
        Expect.equal [ YamlNode.tryMapping scalar |> Option.isSome; YamlNode.tryMapping mapping |> Option.isSome
                       YamlNode.tryMapping sequence |> Option.isSome; YamlNode.tryMapping alias |> Option.isSome ]
            [ false; true; false; false ] "tryMapping"
        Expect.equal [ YamlNode.trySequence scalar |> Option.isSome; YamlNode.trySequence mapping |> Option.isSome
                       YamlNode.trySequence sequence |> Option.isSome; YamlNode.trySequence alias |> Option.isSome ]
            [ false; false; true; false ] "trySequence"

    testCase "entries and items name the kind of a node of another kind" <| fun _ ->
        Expect.equal (errorOf (fun () -> YamlNode.entries (YamlNode.plain "a") |> ignore)) "The node is a scalar, not a mapping." "entries of a scalar"
        Expect.equal (errorOf (fun () -> YamlNode.entries (YamlNode.sequence []) |> ignore)) "The node is a sequence, not a mapping." "entries of a sequence"
        Expect.equal (errorOf (fun () -> YamlNode.items (YamlNode.alias "a") |> ignore)) "The node is an alias, not a sequence." "items of an alias"
        Expect.equal (errorOf (fun () -> YamlNode.items (YamlNode.mapping []) |> ignore)) "The node is a mapping, not a sequence." "items of a mapping"

    testCase "properties gives the tag and the anchor" <| fun _ ->
        let node = YamlNode.plain "a" |> YamlNode.withTag (YamlTag.core "str") |> YamlNode.withAnchor "x"
        Expect.equal (YamlNode.properties node) { Tag = Some (YamlTag.core "str"); Anchor = Some "x" } "tag and anchor"
        Expect.equal (YamlNode.properties (YamlNode.plain "a")) { Tag = None; Anchor = None } "none"
]

let tests_properties = testList "Properties" [
    // yaml@2.6.1 reads `!!str a` with the tag tag:yaml.org,2002:str and `!x a` with the tag !x.
    testCase "withTag names the tag by its identifier" <| fun _ ->
        let core = YamlNode.plain "a" |> YamlNode.withTag (YamlTag.core "str")
        Expect.equal (YamlNode.properties core).Tag.Value (YamlTag.core "str") "core tag"
        Expect.equal (YamlTag.identifier (YamlTag.core "str")) "tag:yaml.org,2002:str" "core identifier"
        Expect.equal (YamlTag.identifier (YamlTag.local "x")) "!x" "local identifier"
        Expect.equal (YamlTag.identifier YamlTag.NonSpecific) "!" "non-specific identifier"

    testCase "a tag needs a name" <| fun _ ->
        Expect.equal (errorOf (fun () -> YamlTag.local "" |> ignore)) "A tag needs a name behind its handle." "empty local tag"
        Expect.equal (errorOf (fun () -> YamlTag.core "" |> ignore)) "A tag needs a name behind its handle." "empty core tag"
        Expect.notEqual (YamlTag.identifier YamlTag.NonSpecific) (YamlTag.identifier (YamlTag.local "x")) "The non-specific tag has its own identifier"

    testCase "withTag and withAnchor add one placement each, the tag first, with an inline separation" <| fun _ ->
        let node = YamlNode.plain "a" |> YamlNode.withAnchor "x" |> YamlNode.withTag (YamlTag.local "t")
        Expect.equal node.PropertyLayout [ placement PropertyName.Tag Separation.Inline; placement PropertyName.Anchor Separation.Inline ] "Tag first"
        Expect.equal (YamlNode.properties node) { Tag = Some (YamlTag.local "t"); Anchor = Some "x" } "Both properties are set"
        Expect.equal (YamlNode.plain "a" |> YamlNode.withAnchor "x").PropertyLayout [ placement PropertyName.Anchor Separation.Inline ] "Anchor alone"
        Expect.equal (YamlNode.plain "a").PropertyLayout [] "A node without properties has no layout"

    // yaml@2.6.1 reads `&a x`, `&a.b x`, `&a:b x` and a name with a non-ASCII letter with the whole name as the
    // anchor. It rejects `& x` with BAD_ALIAS (an anchor cannot be an empty string), and a flow indicator
    // ends a name, so `- &a,b x` and `- &a[b] x` are errors (MISSING_CHAR).
    testCase "alias and withAnchor reject an empty name and a character outside ns-anchor-char" <| fun _ ->
        for name in [ "a"; "a.b"; "a:b"; "-1"; "\u00e9"; "\U0001F600" ] do
            Expect.equal (YamlNode.withAnchor name (YamlNode.plain "x")).Properties.Anchor (Some name) $"anchor {name}"
            Expect.equal (aliasName (YamlNode.alias name)) (Some name) $"alias {name}"
        Expect.equal (errorOf (fun () -> YamlNode.alias "" |> ignore)) "The alias name must not be empty." "empty alias"
        Expect.equal (errorOf (fun () -> YamlNode.withAnchor "" (YamlNode.plain "x") |> ignore)) "The anchor name must not be empty." "empty anchor"
        for (index, name) in List.indexed [ "a b"; "a\tb"; "a\nb"; "a,b"; "a[b"; "a]b"; "a{b"; "a}b"; "a\u0001b"; "\uFEFF"; "a" + string (char 0xD83D) ] do
            Expect.equal
                (errorOf (fun () -> YamlNode.alias name |> ignore))
                ("The alias name " + name + " holds a character that a name cannot hold.")
                $"alias {index}"
            Expect.equal
                (errorOf (fun () -> YamlNode.withAnchor name (YamlNode.plain "x") |> ignore))
                ("The anchor name " + name + " holds a character that a name cannot hold.")
                $"anchor {index}"

    testCase "a second tag and a second anchor are errors" <| fun _ ->
        let tagged = YamlNode.plain "a" |> YamlNode.withTag (YamlTag.core "str")
        Expect.equal
            (errorOf (fun () -> YamlNode.withTag (YamlTag.core "int") tagged |> ignore))
            "A node cannot have two tags."
            "second tag"
        let anchored = YamlNode.plain "a" |> YamlNode.withAnchor "x"
        Expect.equal
            (errorOf (fun () -> YamlNode.withAnchor "y" anchored |> ignore))
            "A node cannot have two anchors."
            "second anchor"

    testCase "an alias takes no tag and no anchor" <| fun _ ->
        Expect.equal
            (errorOf (fun () -> YamlNode.withTag (YamlTag.core "str") (YamlNode.alias "x") |> ignore))
            "The alias *x cannot have a tag."
            "tag"
        Expect.equal
            (errorOf (fun () -> YamlNode.withAnchor "y" (YamlNode.alias "x") |> ignore))
            "The alias *x cannot have an anchor."
            "anchor"

    // yaml@2.6.1 reads `&a [x]` and `!!seq [x]` as the sequence with the anchor and the tag, so collections
    // carry properties like scalars do.
    testCase "collections take tags and anchors" <| fun _ ->
        let sequence = YamlNode.flowSequence [ YamlNode.item (YamlNode.plain "x") ] |> YamlNode.withAnchor "a" |> YamlNode.withTag (YamlTag.core "seq")
        Expect.equal (YamlNode.properties sequence) { Tag = Some (YamlTag.core "seq"); Anchor = Some "a" } "sequence"
        let mapping = YamlNode.mapping [ field "k" YamlNode.empty ] |> YamlNode.withAnchor "m"
        Expect.equal (YamlNode.properties mapping).Anchor (Some "m") "mapping"

    testCase "a tag on omitted content keeps the content empty" <| fun _ ->
        let node = YamlNode.empty |> YamlNode.withTag (YamlTag.core "str")
        Expect.equal (scalarOf node).Value "" "The value stays empty"
        Expect.equal (scalarOf node).Style ScalarStyle.Plain "The style stays plain"
        Expect.equal (YamlNode.properties node).Tag (Some (YamlTag.core "str")) "The tag is explicit"
]

let tests_comment_slots = testList "Comment slots" [
    // The tests here check what the constructors do with a comment. The slot rules R1 to R8 are in
    // Tests.Validate.fs, where each rule builds its tree and validates it.

    testCase "&a # c on a line of its own goes to the After of the anchor placement" <| fun _ ->
        let node = YamlNode.plain "v" |> YamlNode.withAnchor "a" |> YamlNode.withPropertyComment PropertyName.Anchor (comment " c")
        Expect.equal node.PropertyLayout [ placement PropertyName.Anchor (lineBreak " c") ] "layout"
        Expect.equal node.TrailingComment None "The node has no trailing comment"

    testCase "a tag and an anchor each have their own After" <| fun _ ->
        let node =
            YamlNode.plain "v"
            |> YamlNode.withTag (YamlTag.core "str")
            |> YamlNode.withAnchor "a"
            |> YamlNode.withPropertyComment PropertyName.Tag (comment " t")
        Expect.equal node.PropertyLayout [ placement PropertyName.Tag (lineBreak " t"); placement PropertyName.Anchor Separation.Inline ] "layout"
        let both = node |> YamlNode.withPropertyComment PropertyName.Anchor (comment " a")
        Expect.equal both.PropertyLayout [ placement PropertyName.Tag (lineBreak " t"); placement PropertyName.Anchor (lineBreak " a") ] "both"

    testCase "a property added behind a layout joins the layout in canonical order" <| fun _ ->
        let node = YamlNode.plain "v" |> YamlNode.withAnchor "a" |> YamlNode.withPropertyComment PropertyName.Anchor (comment " c") |> YamlNode.withTag (YamlTag.core "str")
        Expect.equal node.PropertyLayout [ placement PropertyName.Tag Separation.Inline; placement PropertyName.Anchor (lineBreak " c") ] "tag first"

    testCase "a property comment needs the property" <| fun _ ->
        Expect.equal
            (errorOf (fun () -> YamlNode.plain "v" |> YamlNode.withPropertyComment PropertyName.Tag (comment " c") |> ignore))
            "The node does not have the property that the comment belongs to."
            "no tag"

    testCase "| # c goes to the HeaderComment of the block scalar" <| fun _ ->
        let node = YamlNode.scalar (ScalarStyle.Block (BlockScalarStyle.Literal, ChompingMode.Clip, None)) "text\n" |> YamlNode.withHeaderComment (comment " c")
        Expect.equal (scalarOf node).HeaderComment (Some (comment " c")) "header"
        Expect.equal node.TrailingComment None "The node has no trailing comment"

    testCase "[ # c goes to the OpeningComment and ] # c to the TrailingComment of a flow collection" <| fun _ ->
        let node =
            YamlNode.flowSequence [ YamlNode.item (YamlNode.plain "a") ]
            |> YamlNode.withOpeningComment (comment " open")
            |> YamlNode.withTrailingComment (comment " close")
        Expect.equal (YamlNode.trySequence node).Value.OpeningComment (Some (comment " open")) "opening"
        Expect.equal node.TrailingComment (Some (comment " close")) "closing"
        let mapping = YamlNode.mapping [] |> YamlNode.withOpeningComment (comment " m")
        Expect.equal (YamlNode.tryMapping mapping).Value.OpeningComment (Some (comment " m")) "An empty mapping is a flow mapping"

    testCase "a comment in front of the closer of a flow collection goes to its End" <| fun _ ->
        let node = YamlNode.flowSequence [ YamlNode.item (YamlNode.plain "a") ]
        let content = (YamlNode.trySequence node).Value
        let ended = { node with Kind = YamlNodeKind.Sequence { content with End = [ FullLineComment (comment " before ]") ] } }
        Expect.equal (YamlNode.trySequence ended).Value.End [ FullLineComment (comment " before ]") ] "End"

    testCase "an alias may carry a trailing comment" <| fun _ ->
        let node = YamlNode.alias "x" |> YamlNode.withTrailingComment (comment " c")
        Expect.equal node.TrailingComment (Some (comment " c")) "alias"

    testCase "a later comment replaces an earlier comment of the same slot" <| fun _ ->
        let node = YamlNode.plain "v" |> YamlNode.withTrailingComment (comment " one") |> YamlNode.withTrailingComment (comment " two")
        Expect.equal node.TrailingComment (Some (comment " two")) "replaced"

    testCase "a trailing comment needs a node that ends in a closing token or in the value" <| fun _ ->
        Expect.equal
            (errorOf (fun () -> YamlNode.mapping [ field "k" YamlNode.empty ] |> YamlNode.withTrailingComment (comment " c") |> ignore))
            "A block mapping has no closing token for a trailing comment. Put the comment on an entry or the end of the collection."
            "block mapping"
        Expect.equal
            (errorOf (fun () -> YamlNode.sequence [ YamlNode.item YamlNode.empty ] |> YamlNode.withTrailingComment (comment " c") |> ignore))
            "A block sequence has no closing token for a trailing comment. Put the comment on an item or the end of the collection."
            "block sequence"
        Expect.equal
            (errorOf (fun () -> YamlNode.scalar (ScalarStyle.Block (BlockScalarStyle.Folded, ChompingMode.Strip, None)) "a" |> YamlNode.withTrailingComment (comment " c") |> ignore))
            "A block scalar has no place for a trailing comment. Use its header comment."
            "block scalar"
        for node in [ YamlNode.plain "a"; YamlNode.scalar ScalarStyle.DoubleQuoted "a"; YamlNode.mapping []; YamlNode.sequence [] ] do
            Expect.equal (YamlNode.withTrailingComment (comment " c") node).TrailingComment (Some (comment " c")) "accepted"

    testCase "a header comment needs a block scalar and an opening comment needs a flow collection" <| fun _ ->
        Expect.equal
            (errorOf (fun () -> YamlNode.plain "a" |> YamlNode.withHeaderComment (comment " c") |> ignore))
            "Only a block scalar has a header comment, not a scalar."
            "plain scalar"
        Expect.equal
            (errorOf (fun () -> YamlNode.sequence [ YamlNode.item YamlNode.empty ] |> YamlNode.withOpeningComment (comment " c") |> ignore))
            "Only a flow collection has an opening comment, not a sequence."
            "block sequence"
        Expect.equal
            (errorOf (fun () -> YamlNode.plain "a" |> YamlNode.withOpeningComment (comment " c") |> ignore))
            "Only a flow collection has an opening comment, not a scalar."
            "scalar"
]

let tests_documents = testList "Documents and streams" [
    testCase "YamlDocument.create holds only the root" <| fun _ ->
        let root = YamlNode.mapping [ field "k" (YamlNode.plain "v") ]
        let document = YamlDocument.create root
        Expect.equal document.Prelude.IsEmpty true "no prelude"
        Expect.equal document.StartMarker.IsNone true "no start marker"
        Expect.equal document.BeforeRoot [] "nothing in front of the root"
        Expect.equal document.AfterRoot [] "nothing behind the root"
        Expect.equal document.EndMarker.IsNone true "no end marker"
        Expect.equal document.Span None "no span"
        Expect.nodeEqual document.Root root "root"

    // A document without a node has an empty plain scalar as its root, so no document needs an option.
    testCase "a document without content has an empty root" <| fun _ ->
        let document = YamlDocument.create YamlNode.empty
        Expect.equal (scalarOf document.Root).Value "" "empty root"

    testCase "a stream holds documents, trivia and extra end markers in order" <| fun _ ->
        let marker: DocumentMarker = { TrailingComment = Some (comment " end"); Span = None }
        let stream =
            { Parts =
                [ StreamPart.Document (YamlDocument.create (YamlNode.plain "a"))
                  StreamPart.ExtraEndMarker marker
                  StreamPart.Trivia (BlankLine None)
                  StreamPart.Document (YamlDocument.create (YamlNode.plain "b")) ] }
        let names =
            stream.Parts
            |> List.map (fun part ->
                match part with
                | StreamPart.Document _ -> "document"
                | StreamPart.Trivia _ -> "trivia"
                | StreamPart.ExtraEndMarker _ -> "marker")
        Expect.equal names [ "document"; "marker"; "trivia"; "document" ] "order"
        Expect.equal ({ Parts = [] }: YamlStream).Parts.IsEmpty true "A stream may hold no document"

    testCase "directives keep their kind and comment" <| fun _ ->
        let directives: DirectiveKind list =
            [ YamlVersion (1, 2)
              TagDirective ("!e!", "tag:example.com,2000:")
              ReservedDirective ("FOO", [ "bar"; "baz" ]) ]
        let made = directives |> List.map (fun kind -> { Kind = kind; TrailingComment = None; Span = None }: YamlDirective)
        let document = { YamlDocument.create YamlNode.empty with Prelude = made |> List.map Directive }
        Expect.equal document.Prelude.Length 3 "three directives"
]

let Main = testList "Model" [
    tests_scalars
    tests_collections
    tests_accessors
    tests_properties
    tests_comment_slots
    tests_documents
]
