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
    | Lexeme.Directive (DirectiveKind.YamlVersion (major, minor)) -> sprintf "%%YAML %d.%d" major minor
    | Lexeme.Directive (DirectiveKind.TagDirective (handle, prefix)) -> sprintf "%%TAG %s %s" handle prefix
    | Lexeme.Directive (DirectiveKind.ReservedDirective (directiveName, arguments)) -> "%" + directiveName + " " + String.concat " " arguments
    | Lexeme.LineStart (indent, tabbed) -> if tabbed then sprintf "line:%d,tab" indent else sprintf "line:%d" indent
    | Lexeme.BlockEntry -> "-"
    | Lexeme.ExplicitKey -> "?"
    | Lexeme.Value adjacent -> if adjacent then ":adjacent" else ":"
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
    sprintf "%s at %d:%d-%d:%d: %s" error.Code error.Span.Start.Line error.Span.Start.Column error.Span.Finish.Line error.Span.Finish.Column error.Message

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
            | Lexeme.Value _ ->
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
    | Some failure -> texts @ [ "error:" + failure.Code ]
    | None -> texts

/// All lexemes including the line starts.
let lexFull (input: string) : string list =
    let result = lexWith false input
    let texts = result.Items |> List.map (fun item -> show item.Value)
    match result.Error with
    | Some failure -> texts @ [ "error:" + failure.Code ]
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
    | None, None -> Error { Code = "none"; Message = "The input holds no scalar."; Span = { Start = { Line = 1; Column = 1 }; Finish = { Line = 1; Column = 1 } }; Related = [] }

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
    | None, None -> Error { Code = "none"; Message = "The input holds no scalar."; Span = { Start = { Line = 1; Column = 1 }; Finish = { Line = 1; Column = 1 } }; Related = [] }

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
            texts.Add("error:" + failure.Code)
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
/// timing tests measure.
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
    | Some failure -> failure.Code
    | None -> ""

/// The span of the error that the lexer reports for the input, as `1:3-1:5`, or an empty text.
let errorSpanOf (input: string) : string =
    match errorOfInput input with
    | Some failure -> showSpan failure.Span
    | None -> ""
