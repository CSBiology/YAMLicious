namespace YAMLicious

/// Helpers for the rewrite of a tree in `Aliases`. The rewrite walks the tree with a stack on the heap, so a tree of
/// any depth is safe, and it rebuilds only the nodes that lie above a change.
module internal TreeWalk =

    let private zeroSpan: SourceSpan =
        { Start = { Line = 1; Column = 1 }
          Finish = { Line = 1; Column = 1 } }

    /// The error at a node. The message names the line of the node when the node has a span, as the messages of the
    /// parser do. A node that a program built has none, and its error points at the start of the text.
    let errorAt (code: YamlErrorCode) (message: string) (at: YamlNode) (related: SourceSpan option list) : YamlError =
        let found = related |> List.choose id
        match at.Span with
        | Some span ->
            { Code = code
              Message = "Line " + string span.Start.Line + ": " + message
              Span = span
              Related = found }
        | None ->
            { Code = code
              Message = message
              Span = zeroSpan
              Related = found }

    /// The nodes directly below a node in document order: the key and the value of each entry of a mapping, and the
    /// node of each item of a sequence.
    let children (node: YamlNode) : YamlNode[] =
        let found = ResizeArray<YamlNode>()
        match node.Kind with
        | YamlNodeKind.Mapping collection ->
            for entry in collection.Items do
                found.Add entry.Key
                found.Add entry.Value
        | YamlNodeKind.Sequence collection ->
            for item in collection.Items do
                found.Add item.Node
        | _ -> ()
        found.ToArray()

    /// The node with the nodes below it replaced. `child i` gives the new i-th node in the order of `children`.
    let withChildren (node: YamlNode) (child: int -> YamlNode) : YamlNode =
        match node.Kind with
        | YamlNodeKind.Mapping collection ->
            let entries =
                collection.Items
                |> List.mapi (fun i entry ->
                    { entry with
                        Key = child (2 * i)
                        Value = child (2 * i + 1) })
            { node with Kind = YamlNodeKind.Mapping { collection with Items = entries } }
        | YamlNodeKind.Sequence collection ->
            let items = collection.Items |> List.mapi (fun i item -> { item with Node = child i })
            { node with Kind = YamlNodeKind.Sequence { collection with Items = items } }
        | _ -> node

    let isFlowCollection (node: YamlNode) : bool =
        match node.Kind with
        | YamlNodeKind.Mapping collection -> collection.Style = CollectionStyle.Flow
        | YamlNodeKind.Sequence collection -> collection.Style = CollectionStyle.Flow
        | _ -> false

    /// The document with its root rewritten.
    let mapDocument (rewrite: YamlNode -> Result<YamlNode, YamlError>) (document: YamlDocument) : Result<YamlDocument, YamlError> =
        match rewrite document.Root with
        | Ok root -> Ok { document with Root = root }
        | Error error -> Error error

    /// The stream with the root of each document rewritten, up to the first error.
    let mapStream (rewrite: YamlNode -> Result<YamlNode, YamlError>) (stream: YamlStream) : Result<YamlStream, YamlError> =
        let parts = ResizeArray<StreamPart>()
        let mutable failure: YamlError option = None
        for part in stream.Parts do
            if failure.IsNone then
                match part with
                | StreamPart.Document document ->
                    match mapDocument rewrite document with
                    | Ok rewritten -> parts.Add(StreamPart.Document rewritten)
                    | Error error -> failure <- Some error
                | other -> parts.Add other
        match failure with
        | Some error -> Error error
        | None -> Ok { Parts = List.ofSeq parts }
