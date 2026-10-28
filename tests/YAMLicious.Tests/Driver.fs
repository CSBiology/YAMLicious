/// A small stand-in for the parser that drives the lexer with the contexts that the parser will pass. It
/// follows the block structure of simple inputs by the indicators it sees: the column of a dash, of a `?` or of
/// the key of a value is the parent indentation of the node behind it, and a flow collection takes the
/// parent indentation of the node that holds it. It is no parser and checks nothing about the structure.
[<AutoOpenAttribute>]
module internal Driver

open YAMLicious

/// The text of a lexeme for the comparison in the tests: `-` for a block entry, `s:value` for a plain
/// scalar and so on. A span is not part of it.
let show (lexeme: Lexeme) : string =
    let text (prefix: string) (value: string) = prefix + ":" + value.Replace("\n", "\\n").Replace("\t", "\\t")
    match lexeme with
    | Lexeme.StreamStart -> "stream-start"
    | Lexeme.StreamEnd -> "stream-end"
    | Lexeme.DocumentStart -> "---"
    | Lexeme.DocumentEnd -> "..."
    | Lexeme.ByteOrderMark -> "bom"
    | Lexeme.Directive (DirectiveKind.YamlVersion (major, minor)) -> sprintf "%%YAML %d.%d" major minor
    | Lexeme.Directive (DirectiveKind.TagDirective (handle, prefix)) -> sprintf "%%TAG %s %s" handle prefix
    | Lexeme.Directive (DirectiveKind.ReservedDirective (directiveName, arguments)) -> "%" + directiveName + " " + String.concat " " arguments
    | Lexeme.LineStart (indent, tabbed) -> if tabbed then sprintf "line:%d,tab" indent else sprintf "line:%d" indent
    | Lexeme.BlockEntry -> "-"
    | Lexeme.ExplicitKey -> "?"
    | Lexeme.Value -> ":"
    | Lexeme.FlowSequenceStart -> "["
    | Lexeme.FlowSequenceEnd -> "]"
    | Lexeme.FlowMappingStart -> "{"
    | Lexeme.FlowMappingEnd -> "}"
    | Lexeme.FlowEntry -> ","
    | Lexeme.Anchor anchorName -> "&" + anchorName
    | Lexeme.Alias aliasName -> "*" + aliasName
    | Lexeme.Tag (TagToken.Verbatim uri) -> "!<" + uri + ">"
    | Lexeme.Tag (TagToken.Shorthand (handle, suffix)) -> "tag:" + handle + "|" + suffix
    | Lexeme.Scalar scalar ->
        match scalar.Style with
        | ScalarStyle.Plain -> text "plain" scalar.Value
        | ScalarStyle.SingleQuoted -> text "single" scalar.Value
        | ScalarStyle.DoubleQuoted -> text "double" scalar.Value
        | ScalarStyle.Block (style, chomping, indicator) ->
            let styleText = if style = BlockScalarStyle.Literal then "literal" else "folded"
            let chompingText =
                match chomping with
                | ChompingMode.Strip -> "-"
                | ChompingMode.Clip -> ""
                | ChompingMode.Keep -> "+"
            let indicatorText = match indicator with Some digit -> string digit | None -> ""
            text (styleText + chompingText + indicatorText) scalar.Value
    | Lexeme.Comment (comment, ownLine) -> if ownLine then "#own:" + comment.Text else "#:" + comment.Text
    | Lexeme.BlankLine -> "blank"

/// What the driver returns for an input: the lexemes with their spans, and the error that stopped it.
type Lexed =
    { Items: Located<Lexeme> list
      Error: YamlError option }

/// The code and the message of an error for a failure message.
let describeError (error: YamlError) =
    sprintf "%s at %d:%d-%d:%d: %s" (YamlErrorCode.name error.Code) error.Span.Start.Line error.Span.Start.Column error.Span.Finish.Line error.Span.Finish.Column error.Message

/// Lexes the input from the start to the end of the text, choosing the context of every lexeme the way
/// the parser will. A lexeme that stands first after `---` has the parent indentation -1.
let lexWith (simpleKeys: bool) (input: string) : Lexed =
    let cursor = Cursor(input)
    let items = ResizeArray<Located<Lexeme>>()
    // The columns of the block nodes that are open, from the outermost to the innermost.
    let owners = ResizeArray<int>()
    // The parent indentation of each open flow collection.
    let flowOwners = ResizeArray<int>()
    let mutable entryColumn = -1
    let mutable expectEntry = true
    let mutable previousStartedLine = false
    let mutable finished = false
    let mutable error: YamlError option = None
    while not finished do
        let parent = if owners.Count > 0 then owners.[owners.Count - 1] else -1
        let context =
            if flowOwners.Count > 0 then
                { Grammar = GrammarContext.FlowIn
                  ParentIndent = flowOwners.[flowOwners.Count - 1]
                  SimpleKey = false }
            else
                { Grammar = GrammarContext.BlockOut
                  ParentIndent = parent
                  SimpleKey = simpleKeys }
        match Lexer.next context cursor with
        | Error failure ->
            error <- Some failure
            finished <- true
        | Ok lexeme ->
            items.Add lexeme
            let column = lexeme.Span.Start.Column - 1
            let startedLine = previousStartedLine
            previousStartedLine <- (match lexeme.Value with Lexeme.LineStart _ -> true | _ -> false)
            match lexeme.Value with
            | Lexeme.StreamEnd -> finished <- true
            | Lexeme.LineStart (indent, _) ->
                while owners.Count > 0 && owners.[owners.Count - 1] >= indent do
                    owners.RemoveAt(owners.Count - 1)
                expectEntry <- true
            | Lexeme.Comment _ -> ()
            | Lexeme.BlankLine -> ()
            | Lexeme.BlockEntry ->
                owners.Add column
                expectEntry <- true
            | Lexeme.ExplicitKey ->
                owners.Add column
                expectEntry <- true
            | Lexeme.Value ->
                if flowOwners.Count = 0 then
                    // A colon that starts its line is the explicit value indicator, which owns its node.
                    let owner = if startedLine then column else entryColumn
                    while owners.Count > 0 && owners.[owners.Count - 1] >= owner do
                        owners.RemoveAt(owners.Count - 1)
                    owners.Add owner
                expectEntry <- false
            | Lexeme.FlowSequenceStart ->
                flowOwners.Add(if flowOwners.Count > 0 then flowOwners.[flowOwners.Count - 1] else parent)
                if expectEntry && flowOwners.Count = 1 then entryColumn <- column
                expectEntry <- false
            | Lexeme.FlowMappingStart ->
                flowOwners.Add(if flowOwners.Count > 0 then flowOwners.[flowOwners.Count - 1] else parent)
                if expectEntry && flowOwners.Count = 1 then entryColumn <- column
                expectEntry <- false
            | Lexeme.FlowSequenceEnd -> if flowOwners.Count > 0 then flowOwners.RemoveAt(flowOwners.Count - 1)
            | Lexeme.FlowMappingEnd -> if flowOwners.Count > 0 then flowOwners.RemoveAt(flowOwners.Count - 1)
            | _ ->
                if expectEntry && flowOwners.Count = 0 then
                    entryColumn <- column
                expectEntry <- false
    { Items = List.ofSeq items
      Error = error }

/// The lexemes of the input as texts, without the stream start, the stream end and the line starts, and the
/// error of the lexer as a last text `error:<code>` when it stopped.
let lex (input: string) : string list =
    let result = lexWith false input
    let texts =
        result.Items
        |> List.map (fun item -> item.Value)
        |> List.filter (fun lexeme ->
            match lexeme with
            | Lexeme.StreamStart -> false
            | Lexeme.StreamEnd -> false
            | Lexeme.LineStart _ -> false
            | _ -> true)
        |> List.map show
    match result.Error with
    | Some failure -> texts @ [ "error:" + (YamlErrorCode.name failure.Code) ]
    | None -> texts

/// All lexemes including the line starts.
let lexFull (input: string) : string list =
    let result = lexWith false input
    let texts = result.Items |> List.map (fun item -> show item.Value)
    match result.Error with
    | Some failure -> texts @ [ "error:" + (YamlErrorCode.name failure.Code) ]
    | None -> texts

/// The first scalar of the input, with its span, or the error when the lexer fails before one.
let firstScalar (input: string) : Result<Located<YamlScalar>, YamlError> =
    let result = lexWith false input
    let found =
        result.Items
        |> List.tryPick (fun item ->
            match item.Value with
            | Lexeme.Scalar scalar -> Some { Value = scalar; Span = item.Span }
            | _ -> None)
    match found, result.Error with
    | Some scalar, _ -> Ok scalar
    | None, Some failure -> Error failure
    | None, None -> Error { Code = YamlErrorCode.UnexpectedContent; Message = "The input holds no scalar."; Span = { Start = { Line = 1; Column = 1 }; Finish = { Line = 1; Column = 1 } }; Related = [] }

/// The last scalar of the input. A key and its value are both scalars, so this is the value of `k: value`.
let lastScalar (input: string) : Result<Located<YamlScalar>, YamlError> =
    let result = lexWith false input
    let found =
        result.Items
        |> List.rev
        |> List.tryPick (fun item ->
            match item.Value with
            | Lexeme.Scalar scalar -> Some { Value = scalar; Span = item.Span }
            | _ -> None)
    match found, result.Error with
    | _, Some failure -> Error failure
    | Some scalar, None -> Ok scalar
    | None, None -> Error { Code = YamlErrorCode.UnexpectedContent; Message = "The input holds no scalar."; Span = { Start = { Line = 1; Column = 1 }; Finish = { Line = 1; Column = 1 } }; Related = [] }

/// The error that the lexer reports for the input, or None when it lexes the whole text.
let errorOfInput (input: string) : YamlError option = (lexWith false input).Error

/// The text of a span, as in `1:3-1:5`.
let showSpan (span: SourceSpan) =
    sprintf "%d:%d-%d:%d" span.Start.Line span.Start.Column span.Finish.Line span.Finish.Column

/// The lexemes with their spans as `text 1:1-1:4`, without the stream start, the stream end and the line
/// starts.
let lexSpans (input: string) : string list =
    (lexWith false input).Items
    |> List.filter (fun item ->
        match item.Value with
        | Lexeme.StreamStart -> false
        | Lexeme.StreamEnd -> false
        | Lexeme.LineStart _ -> false
        | _ -> true)
    |> List.map (fun item -> show item.Value + " " + showSpan item.Span)

/// The scalars of the input in their order, without a check of the error that may stop the lexer.
let scalarsOf (input: string) : YamlScalar list =
    (lexWith false input).Items
    |> List.choose (fun item ->
        match item.Value with
        | Lexeme.Scalar scalar -> Some scalar
        | _ -> None)

/// Lexes the whole input in one fixed context, which tests the context handling of the lexer without the
/// help of the driver. The result is shown like the one of `lex`.
let lexIn (grammar: GrammarContext) (parentIndent: int) (simpleKey: bool) (input: string) : string list =
    let cursor = Cursor(input)
    let context =
        { Grammar = grammar
          ParentIndent = parentIndent
          SimpleKey = simpleKey }
    let texts = ResizeArray<string>()
    let mutable finished = false
    while not finished do
        match Lexer.next context cursor with
        | Error failure ->
            texts.Add("error:" + (YamlErrorCode.name failure.Code))
            finished <- true
        | Ok lexeme ->
            match lexeme.Value with
            | Lexeme.StreamEnd -> finished <- true
            | Lexeme.StreamStart -> ()
            | Lexeme.LineStart _ -> ()
            | other -> texts.Add(show other)
    List.ofSeq texts

/// Lexes the whole input with the parent indentation 0, in the context BlockOut and, between the brackets of
/// a flow collection, in the context FlowIn. It counts the lexemes without keeping them, which is what the
/// timing test measures.
let countLexemes (input: string) : int =
    let cursor = Cursor(input)
    let mutable depth = 0
    let mutable count = 0
    let mutable finished = false
    while not finished do
        let context =
            { Grammar = (if depth > 0 then GrammarContext.FlowIn else GrammarContext.BlockOut)
              ParentIndent = 0
              SimpleKey = false }
        match Lexer.next context cursor with
        | Error failure -> failwith (describeError failure)
        | Ok lexeme ->
            count <- count + 1
            match lexeme.Value with
            | Lexeme.StreamEnd -> finished <- true
            | Lexeme.FlowSequenceStart -> depth <- depth + 1
            | Lexeme.FlowMappingStart -> depth <- depth + 1
            | Lexeme.FlowSequenceEnd -> depth <- depth - 1
            | Lexeme.FlowMappingEnd -> depth <- depth - 1
            | _ -> ()
    count

/// The code of the error that the lexer reports for the input, or an empty text.
let errorCodeOf (input: string) : string =
    match errorOfInput input with
    | Some failure -> (YamlErrorCode.name failure.Code)
    | None -> ""

/// The span of the error that the lexer reports for the input, as `1:3-1:5`, or an empty text.
let errorSpanOf (input: string) : string =
    match errorOfInput input with
    | Some failure -> showSpan failure.Span
    | None -> ""

// ---------------------------------------------------------------------------------------------
// The parser

/// Parses the text and checks the slot rules of the tree. A rejection or a broken rule fails the test.
let parseOk (input: string) : YamlStream =
    match Parser.parseStream input with
    | Ok stream ->
        let problems = Validate.stream stream
        if not problems.IsEmpty then
            failwithf "The tree of %A breaks the slot rules: %s" input (String.concat "; " problems)
        stream
    | Error error -> failwithf "The parser rejects %A: %s" input (describeError error)

/// The only document of the text.
let parseDocument (input: string) : YamlDocument =
    match (parseOk input).Parts with
    | [ StreamPart.Document document ] -> document
    | parts -> failwithf "%A holds %d parts, not one document" input parts.Length

/// The root node of the only document of the text.
let parseRoot (input: string) : YamlNode = (parseDocument input).Root

/// The error of the parser for the text, or a failure when the parser accepts it.
let parseError (input: string) : YamlError =
    match Parser.parseStream input with
    | Error error -> error
    | Ok _ -> failwithf "The parser accepts %A" input

/// The error of the parser as `code@line`, for example `mixed-collections@2`.
let parseErrorOf (input: string) : string =
    let error = parseError input
    YamlErrorCode.name error.Code + "@" + string error.Span.Start.Line

/// A plain scalar.
let sc (value: string) : YamlNode = YamlNode.plain value

/// A single-quoted scalar.
let sq (value: string) : YamlNode = YamlNode.scalar ScalarStyle.SingleQuoted value

/// A double-quoted scalar.
let dq (value: string) : YamlNode = YamlNode.scalar ScalarStyle.DoubleQuoted value

/// A literal block scalar with the chomping Clip.
let lit (value: string) : YamlNode = YamlNode.scalar (ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Clip, None)) value

/// A folded block scalar with the chomping Clip.
let fold (value: string) : YamlNode = YamlNode.scalar (ScalarStyle.Block(BlockScalarStyle.Folded, ChompingMode.Clip, None)) value

/// The node with a layout of folds into one space at the indexes of the spaces. A test names the indexes of the spaces
/// that the parser read from a line break.
let withFolds (indexes: int list) (node: YamlNode) : YamlNode =
    match node.Kind with
    | YamlNodeKind.Scalar content ->
        let layout = { Boundaries = [ for index in indexes -> { At = index; Kind = LayoutKind.Fold 0 } ] }
        { node with Kind = YamlNodeKind.Scalar { content with Layout = Some layout } }
    | _ -> failwith "A scalar was expected."

/// The indexes of the spaces that join the lines into one text.
let foldsOfLines (lines: string list) : int list =
    let mutable at = -1
    [ for line in lines do
          at <- at + 1 + line.Length
          yield at ]
    |> List.take (max 0 (List.length lines - 1))

/// A plain scalar that folding joined from the lines, with the layout of its line breaks.
let scLines (lines: string list) : YamlNode = withFolds (foldsOfLines lines) (sc (String.concat " " lines))

/// A single-quoted scalar that folding joined from the lines, with the layout of its line breaks.
let sqLines (lines: string list) : YamlNode = withFolds (foldsOfLines lines) (sq (String.concat " " lines))

/// A double-quoted scalar that folding joined from the lines, with the layout of its line breaks.
let dqLines (lines: string list) : YamlNode = withFolds (foldsOfLines lines) (dq (String.concat " " lines))

/// A block scalar with the given style, chomping and indentation indicator.
let blockScalar (style: BlockScalarStyle) (chomping: ChompingMode) (indicator: int option) (value: string) : YamlNode =
    YamlNode.scalar (ScalarStyle.Block(style, chomping, indicator)) value

/// A missing value.
let nothing: YamlNode = YamlNode.empty

/// An entry of a block mapping as the parser reads `key: value`. The constructor gives a block collection
/// without a property a line break behind the colon, because it cannot start on the line of its key.
let blockEntry (key: YamlNode) (value: YamlNode) : MappingEntry = YamlNode.entry key value

/// A block mapping with plain keys.
let mapOf (pairs: (string * YamlNode) list) : YamlNode =
    YamlNode.mapping [ for (key, value) in pairs -> blockEntry (YamlNode.plain key) value ]

/// A block mapping with keys that are nodes.
let mapOfNodes (pairs: (YamlNode * YamlNode) list) : YamlNode =
    YamlNode.mapping [ for (key, value) in pairs -> blockEntry key value ]

/// A block sequence.
let seqOf (nodes: YamlNode list) : YamlNode = YamlNode.sequence (List.map YamlNode.item nodes)

/// A document with a root and nothing else.
let docOf (root: YamlNode) : StreamPart = StreamPart.Document(YamlDocument.create root)

/// A comment line as trivia.
let note (text: string) : Trivia = Trivia.FullLineComment(YamlComment.create text)

/// A blank line as trivia.
let blank: Trivia = Trivia.BlankLine None

/// The separation behind an indicator when the content stands on a later line.
let newLine (commentText: string option) (following: Trivia list) : Separation =
    Separation.NewLine
        { Comment = commentText |> Option.map YamlComment.create
          Following = following }

/// An entry with trivia in front of it.
let entryBefore (before: Trivia list) (key: YamlNode) (value: YamlNode) : MappingEntry =
    { blockEntry key value with Before = before }

/// An item with trivia in front of it.
let itemBefore (before: Trivia list) (node: YamlNode) : SequenceItem =
    { YamlNode.item node with Before = before }

/// The tree with a comment behind its last token.
let withNote (text: string) (node: YamlNode) : YamlNode =
    YamlNode.withTrailingComment (YamlComment.create text) node

/// The block collection with the trivia at its end.
let withEnd (trivia: Trivia list) (node: YamlNode) : YamlNode =
    match node.Kind with
    | YamlNodeKind.Mapping collection -> { node with Kind = YamlNodeKind.Mapping { collection with End = trivia } }
    | YamlNodeKind.Sequence collection -> { node with Kind = YamlNodeKind.Sequence { collection with End = trivia } }
    | _ -> failwith "Only a collection has an end."

/// The collection with trivia in front of its first entry or item, as a blank line at the top of the text
/// gives the first entry of a root mapping (rules R3 and R4).
let withFirstBefore (trivia: Trivia list) (node: YamlNode) : YamlNode =
    match node.Kind with
    | YamlNodeKind.Mapping collection ->
        let first = { collection.Items.Head with Before = trivia }
        { node with Kind = YamlNodeKind.Mapping { collection with Items = first :: collection.Items.Tail } }
    | YamlNodeKind.Sequence collection ->
        let first = { collection.Items.Head with Before = trivia }
        { node with Kind = YamlNodeKind.Sequence { collection with Items = first :: collection.Items.Tail } }
    | _ -> failwith "Only a collection has a first entry."

/// The value of a tree as JSON text in the form of JSON.stringify of the value that yaml@2.6.1 builds: a mapping
/// is an object with the cooked text of its keys, a sequence is an array, an alias is the value of the node that
/// its anchor names, and a scalar is null, a Boolean, an integer or a string by the core schema. The walk recurses,
/// so it is meant for the small trees of the tests.
let valueJson (root: YamlNode) : string =
    let anchors = System.Collections.Generic.Dictionary<string, string>()
    let quote (text: string) =
        let sb = System.Text.StringBuilder()
        sb.Append('"') |> ignore
        for c in text do
            if c = '"' then sb.Append("\\\"") |> ignore
            elif c = '\\' then sb.Append("\\\\") |> ignore
            elif c = '\n' then sb.Append("\\n") |> ignore
            elif c = '\r' then sb.Append("\\r") |> ignore
            elif c = '\t' then sb.Append("\\t") |> ignore
            elif int c < 32 then sb.Append("\\u").Append((int c).ToString("x4")) |> ignore
            else sb.Append(c) |> ignore
        sb.Append('"') |> ignore
        sb.ToString()
    let rec visit (node: YamlNode) : string =
        let text =
            match node.Kind with
            | YamlNodeKind.Scalar scalar ->
                // The core schema takes the style and the explicit tag into account: a quoted scalar is a string
                // unless a core tag says otherwise, as in `!!int "2"`.
                if CoreSchema.isNull node.Properties.Tag scalar then
                    "null"
                else
                    match CoreSchema.tryParseBool node.Properties.Tag scalar with
                    | Some value -> if value then "true" else "false"
                    | None ->
                        match CoreSchema.tryParseInt64 node.Properties.Tag scalar with
                        | Some value -> string value
                        | None ->
                            match CoreSchema.tryParseFloat node.Properties.Tag scalar with
                            | Some value -> string value
                            | None -> quote scalar.Value
            | YamlNodeKind.Alias name -> anchors.[name]
            | YamlNodeKind.Sequence collection -> "[" + String.concat "," [ for item in collection.Items -> visit item.Node ] + "]"
            | YamlNodeKind.Mapping collection ->
                let keyText (key: YamlNode) =
                    match key.Kind with
                    | YamlNodeKind.Scalar scalar -> scalar.Value
                    | _ -> failwith "A key has to be a scalar here."
                "{" + String.concat "," [ for entry in collection.Items -> quote (keyText entry.Key) + ":" + visit entry.Value ] + "}"
        match node.Properties.Anchor with
        | Some name -> anchors.[name] <- text
        | None -> ()
        text
    visit root

/// Checks that the only document of the text has the root.
let expectRoot (input: string) (expected: YamlNode) =
    Expect.nodeEqual (parseRoot input) expected (sprintf "The tree of %A" input)

/// An entry whose value stands on a later line, as in `k:` followed by an indented scalar.
let entryOnNewLine (key: string) (value: YamlNode) : MappingEntry =
    { YamlNode.entry (sc key) value with Colon = Some(newLine None []) }

/// An item whose node stands on a later line, as in `-` followed by an indented mapping.
let itemOnNewLine (node: YamlNode) : SequenceItem =
    { YamlNode.item node with Dash = Some(newLine None []) }

/// An explicit entry: `? key` and `: value`, each indicator followed by its content on the same line. A block
/// collection continues the line of the colon, as in `: - b`.
let explicitEntry (key: YamlNode) (value: YamlNode) : MappingEntry =
    { YamlNode.entry key value with
        Question = Some Separation.Inline
        Colon = Some Separation.Inline }

/// An explicit key without the value indicator.
let explicitKeyOnly (key: YamlNode) : MappingEntry =
    { YamlNode.entry key nothing with
        Question = Some Separation.Inline
        Colon = None }

/// The node with a tag that stands on a line of its own, as in `k: !!seq` followed by the items.
let tagOnOwnLine (tag: YamlTag) (node: YamlNode) : YamlNode =
    { YamlNode.withTag tag node with
        PropertyLayout =
            [ { Property = PropertyName.Tag
                After = newLine None []
                Span = None } ] }

/// The node with an anchor that stands on a line of its own.
let anchorOnOwnLine (name: string) (node: YamlNode) : YamlNode =
    { YamlNode.withAnchor name node with
        PropertyLayout =
            [ { Property = PropertyName.Anchor
                After = newLine None []
                Span = None } ] }

/// The node with the layout of its properties replaced.
let withLayout (layout: PropertyPlacement list) (node: YamlNode) : YamlNode = { node with PropertyLayout = layout }

/// The placement of a property.
let placement (property: PropertyName) (after: Separation) : PropertyPlacement =
    { Property = property
      After = after
      Span = None }

/// Checks that the text is one document and that the document equals the expected one except for the spans.
let expectDocument (input: string) (expected: YamlDocument) =
    Expect.streamEqual (parseOk input) { Parts = [ StreamPart.Document expected ] } (sprintf "The document of %A" input)

/// A flow sequence of the nodes.
let flowSeq (nodes: YamlNode list) : YamlNode = YamlNode.flowSequence (List.map YamlNode.item nodes)

/// A flow mapping with plain keys.
let flowMap (pairs: (string * YamlNode) list) : YamlNode =
    YamlNode.flowMapping [ for (key, value) in pairs -> YamlNode.entry (YamlNode.plain key) value ]

/// The one-entry flow mapping that a flow pair such as `a: b` gives inside a flow sequence.
let flowPair (key: YamlNode) (value: YamlNode) : YamlNode = YamlNode.flowMapping [ YamlNode.entry key value ]

/// An entry of a flow mapping with trivia in front of it.
let flowEntryBefore (before: Trivia list) (key: YamlNode) (value: YamlNode) : MappingEntry = { YamlNode.entry key value with Before = before }

/// A key alone in a flow mapping: the value indicator is omitted and the value is missing.
let keyOnly (key: YamlNode) : MappingEntry = { YamlNode.entry key nothing with Colon = None }

/// An entry with the question mark and the separations behind the indicators.
let explicitFlowEntry (key: YamlNode) (value: YamlNode) : MappingEntry = { YamlNode.entry key value with Question = Some Separation.Inline }

/// The value of the only document of the text as JSON, from `valueJson`.
let valueOf (input: string) : string = valueJson (parseRoot input)

/// The values of the documents of the text as JSON from `valueJson`. A text with one document gives its value, any other count gives a JSON list.
/// A text that the parser rejects gives ERROR, the spelling that the expectations use for a text that yaml@2.6.1 rejects.
let yamlValueOf (input: string) : string =
    match Parser.parseStream input with
    | Error _ -> "ERROR"
    | Ok stream ->
        let problems = Validate.stream stream
        if not problems.IsEmpty then
            failwithf "The tree of %A breaks the slot rules: %s" input (String.concat "; " problems)
        let values =
            [ for part in stream.Parts do
                  match part with
                  | StreamPart.Document document -> yield valueJson document.Root
                  | _ -> () ]
        match values with
        | [ single ] -> single
        | _ -> "[" + String.concat "," values + "]"

/// Checks the rows of an old test, an input and the value that yaml@2.6.1 reads, and reports every difference together.
let expectYamlValues (rows: (string * string * string) list) =
    let differences =
        [ for (reference, input, expected) in rows do
              let actual = yamlValueOf input
              if actual <> expected then
                  yield sprintf "%s: %A reads as %s and not as %s" reference input actual expected ]
    if not differences.IsEmpty then
        failwith (String.concat "\n" differences)

// ---------------------------------------------------------------------------------------------
// The inputs of the scaling tests

/// A mapping whose first value is a plain scalar of the given number of continuation lines.
let plainContinuationInput (lines: int) =
    let sb = System.Text.StringBuilder()
    sb.Append("k: start\n") |> ignore
    for i in 0 .. lines - 1 do
        sb.Append(sprintf "  line %d\n" i) |> ignore
    sb.Append("j: 1\n") |> ignore
    sb.ToString()

/// A literal block scalar of the given number of lines in front of a sibling.
let blockBodyInput (lines: int) =
    let sb = System.Text.StringBuilder()
    sb.Append("a: |\n") |> ignore
    for i in 0 .. lines - 1 do
        sb.Append(sprintf "  line %d\n" i) |> ignore
    sb.Append("b: 1\n") |> ignore
    sb.ToString()

/// A run of comments at columns 0 to 8 with a blank line now and then, between a block scalar and its sibling.
let commentRunInput (comments: int) =
    let sb = System.Text.StringBuilder()
    sb.Append("root:\n  a: |\n    x\n") |> ignore
    for i in 0 .. comments - 1 do
        sb.Append(String.replicate (i % 5 * 2) " ").Append(sprintf "# c%d\n" i) |> ignore
        if i % 7 = 0 then sb.Append("\n") |> ignore
    sb.Append("  b: 1\nc: 2\n") |> ignore
    sb.ToString()

// The parts of the mixed inputs of the scaling tests. Each part is a text of one document, and `documentsOf` joins parts behind `---` lines. The count of a part is
// the number of its units or of the lines of its run.

/// The count of a part in a mixed input of `lines` lines, rounded. The share is in ten-thousandths of the lines, and a part has at least one unit.
let partOf (lines: int) (share: int) = max 1 (int ((int64 lines * int64 share + 5000L) / 10000L))

/// The parts as the documents of one text.
let documentsOf (parts: string list) : string = String.concat "---\n" parts

/// Block mappings and sequences in several nestings, with tags and anchors.
let entriesInput (count: int) =
    units count (fun i ->
        match i % 6 with
        | 0 -> sprintf "n%d:\n  a: 1\n  b: two words\n" i
        | 1 -> sprintf "s%d:\n- id: %d\n  name: tool\n- item %d\n" i i i
        | 2 -> sprintf "c%d:\n  - - nested %d\n    - x\n  - k: !!str &p%d v\n" i i i
        | 3 -> sprintf "t%d: !!str &t%d tagged %d\n" i i i
        | _ -> sprintf "key%d: value %d\n" i i)

/// Aliases to a mapping, aliases in flow collections, an anchor that a later anchor of the same name replaces, and an anchor in a flow mapping.
let aliasEntriesInput (count: int) =
    "base: &base\n  a: 1\n  b: [x, y]\n"
    + units count (fun i ->
        match i % 4 with
        | 0 -> sprintf "r%d: *base\n" i
        | 1 -> sprintf "k%d: [*base, &x%d v%d, *x%d]\n" i i i i
        | 2 -> sprintf "a%d: &a v%d\nb%d: *a\n" i i i
        | _ -> sprintf "q%d: {m: *base, n: &y%d [1, 2], o: *y%d}\n" i i i)

/// A flow sequence of `items` short items on one line.
let flowLineInput (items: int) =
    "k: [" + String.concat ", " [ for i in 0 .. items - 1 -> sprintf "item%d" i ] + "]\n"

/// A flow mapping with one entry on each line, with comments, quoted keys, pairs and explicit keys with properties.
let flowMappingInput (lines: int) =
    "k: {\n"
    + units lines (fun i ->
        (if i % 5 = 0 then sprintf "  # comment %d\n" i else "")
        + (if i % 3 = 0 then sprintf "  \"key%d\": [a, b: c], # trailing\n" i
           elif i % 7 = 1 then sprintf "  ? &a%d !!str key%d : !!str value,\n" i i
           else sprintf "  key%d: value %d,\n" i i))
    + "}\n"

/// A block sequence of flow collections that nest to a depth of 4. The depth tests cover deeper nesting.
let flowNestedInput (count: int) =
    units count (fun i -> sprintf "- [a, {b: [a, {b: v%d}]}]\n" i)

/// A flow sequence over many lines with a plain scalar of one word on each line, which is one scalar, and then a run of comments in front of the closer.
let flowLinesInput (lines: int) =
    "k: [\n" + units lines (fun i -> sprintf "  word%d\n" i) + units lines (fun i -> sprintf "# c%d\n" i) + "]\n"

/// A comment in front of every entry and a comment behind its value.
let interleavedCommentInput (lines: int) =
    units lines (fun i -> sprintf "# about %d\nk%d: v # trailing %d\n" i i i)

/// A run of comments behind a block scalar, and a run of comments at the end of a nested block, with a blank line among them.
let commentRunsInput (comments: int) =
    commentRunInput comments
    + "d:\n  e:\n    f: 1\n"
    + units comments (fun i ->
        match i % 3 with
        | 0 -> sprintf "      # comment %d\n" i
        | 1 -> "\n"
        | _ -> sprintf "  # comment %d\n" i)

/// A literal and a folded block scalar of `lines` lines each, with more indented lines, empty lines, lines of spaces and lines of a hash.
let blockScalarRunInput (lines: int) =
    let body (header: string) =
        header + "\n"
        + units lines (fun i ->
            match i % 7 with
            | 0 -> sprintf "  line %d\n" i
            | 1 -> sprintf "    deeper %d\n" i
            | 2 -> "\n"
            | 3 -> "   \n"
            | 4 -> sprintf "  # hash %d\n" i
            | _ -> sprintf "  line %d with 'quotes' and \"more\"\n" i)
    body "a: |+" + body "b: >"

/// Entries with a block scalar in every chomping, behind a header with a comment, with and without content, and a block scalar with a comment behind it.
let blockScalarUnitsInput (count: int) =
    units count (fun i ->
        match i % 10 with
        | 0 -> sprintf "h%d: | # c%d\n" i i
        | 1 -> sprintf "h%d: >- # c%d\n" i i
        | 2 -> sprintf "h%d: |+ # c%d\n" i i
        | 3 -> sprintf "h%d: >+ # c%d\n" i i
        | 4 -> sprintf "h%d: | # c%d\n  body %d\n" i i i
        | 5 -> sprintf "h%d: >- # c%d\n  body %d\n" i i i
        | 6 -> sprintf "h%d: |+ # c%d\n  body %d\n\n" i i i
        | 7 -> sprintf "h%d: >+ # c%d\n  body %d\n\n\n" i i i
        | 8 -> sprintf "k%d: |\n  line a %d\n  line b\n" i i
        | _ -> sprintf "k%d: |\n  x\n# c\n" i)

/// Quoted scalars with escapes and a comment behind them, a double-quoted scalar over three lines with an escaped line break, a single-quoted
/// scalar with a blank line, and plain scalars with quotes inside.
let quotedInput (count: int) =
    units count (fun i ->
        match i % 8 with
        | 0 -> sprintf "k%d: \"esc \\t \\u00e9 \\\"x\\\" \\U0001F600 %d\" # c%d\n" i i i
        | 1 -> sprintf "k%d: 'it''s %d'\n" i i
        | 2 -> sprintf "k%d: \"first %d\n  second \\\n  third\"\n" i i
        | 3 -> sprintf "k%d: 'a\n\n  b %d'\n" i i
        | 4 -> sprintf "k%d: it's a 'quoted word %d\n" i i
        | 5 -> sprintf "k%d: 3\" wide \"x %d\n" i i
        | 6 -> sprintf "k%d: rock 'n' roll %d\n" i i
        | _ -> sprintf "k%d: don't stop %d # c\n" i i)

/// A single-quoted and a double-quoted scalar over the given number of lines each, the double-quoted one with escaped line breaks.
let quotedRunInput (lines: int) =
    "a: 'w\n" + String.replicate lines "  word\n" + "  end'\nb: \"w\\n" + String.replicate lines "  word\\n" + "  end\"\n"

/// A plain scalar and a quoted scalar of `words` words on one line each.
let longLineInput (words: int) =
    "k: " + String.replicate words "word " + "\nj: \"" + String.replicate words "word " + "\"\n"

/// Runs of blank lines in the middle and at the end of nested mappings, behind a plain scalar and made of spaces.
let blankLinesInput (blanks: int) =
    "k: a\n" + String.replicate blanks "\n" + "j: b\n" + String.replicate blanks "  \n" + "i: c\n"
    + "a:\n  b: 1\n" + String.replicate blanks "\n" + "  c: 2\nd: 3\n" + String.replicate blanks "\n"

/// Documents with a block root and documents with a flow root on the marker line.
let documentsInput (count: int) =
    units count (fun i ->
        if i = 0 then "k: 0\n"
        elif i % 2 = 0 then sprintf "---\nk: %d\n" i
        else sprintf "--- [%d, {a: b}]\n" i)
