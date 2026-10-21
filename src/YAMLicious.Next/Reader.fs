namespace YAMLicious

/// Raised by `Reader.read`, `Reader.readDocuments` and `Reader.readRoot` when the text is not valid YAML or
/// holds more than one document where `read` expects one. On .NET `Message` is the message of the error. Fable
/// compiles the override to a method that JavaScript and Python never call, so a caller on those targets reads
/// the payload.
exception YamlParseException of YamlError
    with
        override this.Message = this.Data0.Message

/// Reads text into documents. The parser keeps the whole presentation, so a reader that wants only the data
/// decodes the root of the document.
module Reader =

    let private origin: SourceSpan =
        { Start = { Line = 1; Column = 1 }
          Finish = { Line = 1; Column = 1 } }

    let private spanOf (document: YamlDocument) : SourceSpan =
        match document.Span with
        | Some span -> span
        | None -> origin

    /// Every document of the stream in order, without the trivia and the extra end markers between them.
    let documents (stream: YamlStream) : YamlDocument list =
        stream.Parts
        |> List.choose (fun part ->
            match part with
            | StreamPart.Document document -> Some document
            | _ -> None)

    /// The document that a text without any document stands for. Its root is missing. The trivia of the
    /// stream stay in `BeforeRoot` in their order, and the first end marker without a document closes it.
    let private emptyDocument (stream: YamlStream) : YamlDocument =
        let trivia =
            stream.Parts
            |> List.choose (fun part ->
                match part with
                | StreamPart.Trivia item -> Some item
                | _ -> None)
        let marker =
            stream.Parts
            |> List.tryPick (fun part ->
                match part with
                | StreamPart.ExtraEndMarker marker -> Some marker
                | _ -> None)
        { YamlDocument.create YamlNode.empty with
            BeforeRoot = trivia
            EndMarker = marker }

    /// Reads a text with any number of documents, also none. Each document keeps its prelude, its markers and
    /// the trivia around its root, and the stream keeps the trivia and the end markers that belong to no
    /// document.
    let tryReadDocuments (text: string) : Result<YamlStream, YamlError> = Parser.parseStream text

    let readDocuments (text: string) : YamlStream =
        match tryReadDocuments text with
        | Ok stream -> stream
        | Error error -> raise (YamlParseException error)

    /// Reads a text with one document. A text with several documents is an error with the code
    /// `MultipleDocuments`, whose span is the second document and whose related span is the first. A text
    /// without any document, such as an empty text or a text with comments only, gives a document with a
    /// missing root that keeps the comments in `BeforeRoot`. The trivia of the stream around a single
    /// document are not part of the document, and `readDocuments` keeps them.
    let tryRead (text: string) : Result<YamlDocument, YamlError> =
        match tryReadDocuments text with
        | Error error -> Error error
        | Ok stream ->
            match documents stream with
            | [] -> Ok (emptyDocument stream)
            | [ single ] -> Ok single
            | first :: second :: _ ->
                let place = spanOf second
                Error
                    { Code = YamlErrorCode.MultipleDocuments
                      Message =
                        "Line "
                        + string place.Start.Line
                        + ": The text holds more than one document. Use Reader.readDocuments to read all of them."
                      Span = place
                      Related = [ spanOf first ] }

    let read (text: string) : YamlDocument =
        match tryRead text with
        | Ok document -> document
        | Error error -> raise (YamlParseException error)

    /// The root of the only document of the text. It drops the markers, the directives and the comments
    /// around the root, so it suits a caller that decodes data.
    let readRoot (text: string) : YamlNode = (read text).Root
