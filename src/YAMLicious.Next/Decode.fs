namespace YAMLicious

open System
open System.Collections.Generic
open System.Globalization

/// Reads a value of type 'T from a node. A decoder raises `YamlDecodeException` when the node does not fit,
/// and `Decode.tryDecode` turns that into a result.
type Decoder<'T> = YamlNode -> 'T

/// Why a decoder stopped and where. `Path` names the way from the node that the caller passed to the node that
/// failed, outermost segment first. A segment is the name of a field. A sequence item adds `[i]`, and a map or
/// dict entry adds `{key}`. `Span` is the range of the node that failed, and it is None for a node that a
/// program constructed.
type DecodeError =
    { Message: string
      Path: string list
      Span: SourceSpan option }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module DecodeError =

    /// The path as text, as in `steps[2].run`.
    let pathText (error: DecodeError) : string =
        let sb = System.Text.StringBuilder()
        let mutable first = true
        for segment in error.Path do
            if segment.StartsWith("[") || segment.StartsWith("{") then
                sb.Append(segment) |> ignore
            else
                if not first then sb.Append(".") |> ignore
                sb.Append(segment) |> ignore
            first <- false
        sb.ToString()

    /// The error as one line of text: the path, the message and the start of the span.
    let text (error: DecodeError) : string =
        let place =
            match error.Span with
            | Some span -> " (line " + string span.Start.Line + ", column " + string span.Start.Column + ")"
            | None -> ""
        if error.Path.IsEmpty then
            error.Message + place
        else
            "At " + pathText error + ": " + error.Message + place

/// Raised by a decoder. On .NET `Message` is `DecodeError.text` of the payload. Fable compiles the override to a
/// method that JavaScript and Python never call, so a caller on those targets reads the payload.
exception YamlDecodeException of DecodeError
    with
        override this.Message = DecodeError.text this.Data0

module internal DecodeSupport =

    let spanText (span: SourceSpan) : string =
        "line " + string span.Start.Line + ", column " + string span.Start.Column

    let private shorten (text: string) : string =
        let flat = text.Replace("\n", "\\n")
        if flat.Length > 40 then flat.Substring(0, 37) + "..." else flat

    /// A tag as the text shows it: `!!str` for the core namespace, `!local` and `!<uri>` otherwise.
    let tagText (tag: YamlTag) : string =
        match tag with
        | YamlTag.NonSpecific -> "!"
        | YamlTag.Named name ->
            if name.Prefix = YamlTag.coreNamespace then "!!" + name.Suffix
            elif name.Prefix = "!" then "!" + name.Suffix
            else "!<" + name.Prefix + name.Suffix + ">"

    /// What the node is, for a message that says what a decoder found.
    let describeNode (node: YamlNode) : string =
        match node.Kind with
        | YamlNodeKind.Mapping _ -> "a mapping"
        | YamlNodeKind.Sequence _ -> "a sequence"
        | YamlNodeKind.Alias name -> "the alias *" + name
        | YamlNodeKind.Scalar scalar ->
            if YamlNode.isMissingValue node then
                "a missing value"
            else
                let style =
                    match scalar.Style with
                    | ScalarStyle.Plain -> "plain"
                    | ScalarStyle.SingleQuoted -> "single-quoted"
                    | ScalarStyle.DoubleQuoted -> "double-quoted"
                    | ScalarStyle.Block (BlockScalarStyle.Literal, _, _) -> "literal block"
                    | ScalarStyle.Block (BlockScalarStyle.Folded, _, _) -> "folded block"
                let tag =
                    match node.Properties.Tag with
                    | Some explicit -> " with the tag " + tagText explicit
                    | None -> ""
                "the " + style + " scalar \"" + shorten scalar.Value + "\"" + tag

    let expected (what: string) (node: YamlNode) : string =
        "Expected " + what + ", found " + describeNode node + "."

    let failAt (message: string) (span: SourceSpan option) : 'T =
        raise (YamlDecodeException { Message = message; Path = []; Span = span })

    let fail (message: string) (node: YamlNode) : 'T = failAt message node.Span

    /// Aliases stay symbolic, so a decoder that reads a value rejects one and names it.
    let failAlias (name: string) (node: YamlNode) : 'T =
        let place =
            match node.Span with
            | Some span -> " at " + spanText span
            | None -> ""
        fail ("The alias *" + name + place + " cannot be read as a value. Aliases stay symbolic, so only Decode.alias and Decode.node accept them.") node

    let withSegment (segment: string) (error: DecodeError) : DecodeError =
        { error with Path = segment :: error.Path }

    /// Runs the decoder and adds the segment to the path of an error from below.
    let inSegment (segment: string) (decoder: Decoder<'T>) (node: YamlNode) : 'T =
        try
            decoder node
        with YamlDecodeException error ->
            raise (YamlDecodeException (withSegment segment error))

    /// Runs the decoder on the item with the index and adds `[index]` to the path of an error from below.
    /// The segment is built when an error happens only.
    let inItem (index: int) (decoder: Decoder<'T>) (node: YamlNode) : 'T =
        try
            decoder node
        with YamlDecodeException error ->
            raise (YamlDecodeException (withSegment ("[" + string index + "]") error))

    let keyLabel (key: YamlNode) : string =
        match key.Kind with
        | YamlNodeKind.Scalar scalar -> scalar.Value
        | YamlNodeKind.Alias name -> "*" + name
        | YamlNodeKind.Mapping _ -> "(mapping)"
        | YamlNodeKind.Sequence _ -> "(sequence)"

    /// Runs the decoder for the key or the value of the entry with the key node `key` and adds `{label}` to
    /// the path of an error from below.
    let inEntry (key: YamlNode) (decoder: Decoder<'T>) (node: YamlNode) : 'T =
        try
            decoder node
        with YamlDecodeException error ->
            raise (YamlDecodeException (withSegment ("{" + keyLabel key + "}") error))

    let scalarOf (what: string) (node: YamlNode) : YamlScalar =
        match node.Kind with
        | YamlNodeKind.Scalar scalar -> scalar
        | YamlNodeKind.Alias name -> failAlias name node
        | _ -> fail (expected what node) node

    /// Reads a scalar through one of the parsers of the core schema. The parser sees the explicit tag of the
    /// node next to the scalar.
    let typedScalar (what: string) (parse: YamlTag option -> YamlScalar -> 'T option) (node: YamlNode) : 'T =
        let scalar = scalarOf what node
        match parse node.Properties.Tag scalar with
        | Some value -> value
        | None -> fail (expected what node) node

    let isNullNode (node: YamlNode) : bool =
        match node.Kind with
        | YamlNodeKind.Scalar scalar -> CoreSchema.isNull node.Properties.Tag scalar
        | _ -> false

    let sequenceItems (what: string) (node: YamlNode) : SequenceItem list =
        match node.Kind with
        | YamlNodeKind.Sequence collection -> collection.Items
        | YamlNodeKind.Alias name -> failAlias name node
        | _ -> fail (expected what node) node

    let mappingEntries (what: string) (node: YamlNode) : MappingEntry list =
        match node.Kind with
        | YamlNodeKind.Mapping collection -> collection.Items
        | YamlNodeKind.Alias name -> failAlias name node
        | _ -> fail (expected what node) node

    /// Decodes every item of a sequence in order. One handler covers the whole loop, and the index of the failing
    /// item is all that it needs for the path.
    let decodeItems (what: string) (decoder: Decoder<'T>) (node: YamlNode) : ResizeArray<'T> =
        let result = ResizeArray<'T>()
        let mutable remaining = sequenceItems what node
        let mutable index = 0
        try
            while not remaining.IsEmpty do
                result.Add(decoder remaining.Head.Node)
                remaining <- remaining.Tail
                index <- index + 1
        with YamlDecodeException error ->
            raise (YamlDecodeException (withSegment ("[" + string index + "]") error))
        result

    let tupleNodes (size: int) (node: YamlNode) : YamlNode[] =
        let items = sequenceItems ("a sequence of " + string size + " items") node
        let count = List.length items
        if count <> size then
            fail ("Expected a sequence of " + string size + " items, found a sequence of " + string count + " items.") node
        else
            items |> List.map (fun item -> item.Node) |> List.toArray

    /// Decodes the entries of a mapping to pairs in source order. The key decoder gets the whole key node. Two
    /// entries whose decoded keys are equal are an error.
    let decodePairs (keyDecoder: Decoder<'K>) (valueDecoder: Decoder<'V>) (node: YamlNode) : ResizeArray<'K * 'V> =
        let pairs = ResizeArray<'K * 'V>()
        let seen = Dictionary<'K, YamlNode>()
        let mutable remaining = mappingEntries "a mapping" node
        while not remaining.IsEmpty do
            let entry = remaining.Head
            remaining <- remaining.Tail
            let key = inEntry entry.Key keyDecoder entry.Key
            match seen.TryGetValue key with
            | true, first ->
                let firstPlace =
                    match first.Span with
                    | Some span -> spanText span
                    | None -> "an earlier entry"
                let place =
                    match entry.Key.Span with
                    | Some span -> spanText span
                    | None -> "a later entry"
                fail ("The key " + keyLabel entry.Key + " occurs twice, at " + firstPlace + " and at " + place + ".") entry.Key
            | _ ->
                seen.[key] <- entry.Key
                pairs.Add((key, inEntry entry.Key valueDecoder entry.Value))
        pairs

    /// Reads the text of a scalar as a point in time through the parse of the target.
    let parseDateTime (text: string) : DateTime option =
#if FABLE_COMPILER
        match DateTime.TryParse text with
#else
        match DateTime.TryParse(text, CultureInfo.InvariantCulture, DateTimeStyles.RoundtripKind) with
#endif
        | true, value -> Some value
        | false, _ -> None

    let parseDateTimeOffset (text: string) : DateTimeOffset option =
#if FABLE_COMPILER
        match DateTimeOffset.TryParse text with
#else
        match DateTimeOffset.TryParse(text, CultureInfo.InvariantCulture, DateTimeStyles.None) with
#endif
        | true, value -> Some value
        | false, _ -> None

/// The decoders. A decoder reads the node and ignores its presentation: comments, anchors, spans and the
/// style of a collection. A scalar decoder follows the YAML 1.2 core schema with the explicit tag and the
/// style of the scalar, so `"1"` is a string for `int` and `!!int "1"` is an int. Every decoder except
/// `alias` and `node` rejects an alias.
[<RequireQualifiedAccess>]
module Decode =

    open DecodeSupport

    /// The getters of an object decoder.
    type IRequiredGetter =
        /// The value of the field. A mapping without the field is an error.
        abstract Field: string -> Decoder<'T> -> 'T

    type IOptionalGetter =
        /// The value of the field, or None when the field is missing or null.
        abstract Field: string -> Decoder<'T> -> 'T option

    type IMultipleOptionalGetter =
        /// The entries with these field names, in source order.
        abstract FieldList: string list -> MappingEntry list

    type IOverflowGetter =
        /// The entries without these field names, in source order.
        abstract FieldList: string list -> MappingEntry list

    type IGetters =
        abstract Required: IRequiredGetter
        abstract Optional: IOptionalGetter
        abstract MultipleOptional: IMultipleOptionalGetter
        abstract Overflow: IOverflowGetter
        /// Every entry of the mapping in source order.
        abstract Entries: MappingEntry list

    /// The getters of a mapping. The field names go into one index, so each lookup costs the same however
    /// many entries the mapping has. A key that is no scalar is an error, and so is a name that occurs twice.
    let private createGetters (mapping: YamlNode) : IGetters =
        let entryList = mappingEntries "a mapping" mapping
        let entries = List.toArray entryList
        let names: string[] = Array.zeroCreate entries.Length
        let index = Dictionary<string, int>()
        for i in 0 .. entries.Length - 1 do
            let key = entries.[i].Key
            match key.Kind with
            | YamlNodeKind.Scalar scalar ->
                match index.TryGetValue scalar.Value with
                | true, first ->
                    let firstPlace =
                        match entries.[first].Key.Span with
                        | Some span -> spanText span
                        | None -> "an earlier entry"
                    let place =
                        match key.Span with
                        | Some span -> spanText span
                        | None -> "a later entry"
                    fail ("The field " + scalar.Value + " occurs twice, at " + firstPlace + " and at " + place + ".") key
                | _ ->
                    index.[scalar.Value] <- i
                    names.[i] <- scalar.Value
            | YamlNodeKind.Alias name -> failAlias name key
            | _ -> fail (expected "a scalar as a field name" key) key
        let required =
            { new IRequiredGetter with
                member _.Field name decoder =
                    match index.TryGetValue name with
                    | true, i -> inSegment name decoder entries.[i].Value
                    | _ -> fail ("The mapping has no field " + name + ".") mapping }
        let optional =
            { new IOptionalGetter with
                member _.Field name decoder =
                    match index.TryGetValue name with
                    | true, i ->
                        let value = entries.[i].Value
                        if isNullNode value then None else Some (inSegment name decoder value)
                    | _ -> None }
        let multipleOptional =
            { new IMultipleOptionalGetter with
                member _.FieldList fieldNames =
                    let wanted = HashSet<string>(fieldNames)
                    [ for i in 0 .. entries.Length - 1 do
                          if wanted.Contains names.[i] then yield entries.[i] ] }
        let overflow =
            { new IOverflowGetter with
                member _.FieldList fieldNames =
                    let known = HashSet<string>(fieldNames)
                    [ for i in 0 .. entries.Length - 1 do
                          if not (known.Contains names.[i]) then yield entries.[i] ] }
        { new IGetters with
            member _.Required = required
            member _.Optional = optional
            member _.MultipleOptional = multipleOptional
            member _.Overflow = overflow
            member _.Entries = entryList }

    /// The text of any scalar, whatever its style and tag. That includes the text of a number, of a bool, of a
    /// null and of a missing value. A collection and an alias are errors.
    let string (node: YamlNode) : string = (scalarOf "a string" node).Value

    /// An int in 32 bits that the core schema reads from a plain scalar or from a scalar under `!!int`.
    let int (node: YamlNode) : int = typedScalar "an int" CoreSchema.tryParseInt node

    /// An int in 64 bits that the core schema reads from a plain scalar or from a scalar under `!!int`.
    let int64 (node: YamlNode) : int64 = typedScalar "an int64" CoreSchema.tryParseInt64 node

    /// A float that the core schema reads from a plain scalar or from a scalar under `!!float`. That includes
    /// the ints, `.inf`, `-.inf` and `.nan`.
    let float (node: YamlNode) : float = typedScalar "a float" CoreSchema.tryParseFloat node

    /// A bool that the core schema reads from a plain scalar or from a scalar under `!!bool`.
    let bool (node: YamlNode) : bool = typedScalar "a bool" CoreSchema.tryParseBool node

    /// A scalar of exactly one character, whatever its style and tag.
    let char (node: YamlNode) : char =
        let scalar = scalarOf "a char" node
        if scalar.Value.Length = 1 then scalar.Value.[0] else fail (expected "a char" node) node

    /// A point in time in the ISO 8601 form that `Encode.datetime` writes.
    let datetime (node: YamlNode) : DateTime =
        let scalar = scalarOf "a DateTime" node
        match parseDateTime scalar.Value with
        | Some value -> value
        | None -> fail (expected "a DateTime" node) node

    /// A point in time with an offset in the ISO 8601 form that `Encode.datetimeOffset` writes.
    let datetimeOffset (node: YamlNode) : DateTimeOffset =
        let scalar = scalarOf "a DateTimeOffset" node
        match parseDateTimeOffset scalar.Value with
        | Some value -> value
        | None -> fail (expected "a DateTimeOffset" node) node

    /// None for a null of the core schema: a plain `null`, `Null`, `NULL` or `~`, an empty plain scalar such
    /// as a missing value, and the same spellings under `!!null`. A quoted or block scalar is a string, so it is
    /// never null. A collection is never null. Anything else goes to the decoder.
    let option (decoder: Decoder<'T>) : Decoder<'T option> =
        fun node -> if isNullNode node then None else Some (decoder node)

    let list (decoder: Decoder<'T>) : Decoder<'T list> =
        fun node -> List.ofSeq (decodeItems "a sequence" decoder node)

    let array (decoder: Decoder<'T>) : Decoder<'T[]> =
        fun node -> (decodeItems "a sequence" decoder node).ToArray()

    /// Decodes every item at once, so a failing item raises in this call.
    let seq (decoder: Decoder<'T>) : Decoder<seq<'T>> =
        fun node -> decodeItems "a sequence" decoder node :> seq<'T>

    let resizearray (decoder: Decoder<'T>) : Decoder<ResizeArray<'T>> =
        fun node -> decodeItems "a sequence" decoder node

    /// A sequence whose items are scalars. An item that is a collection or an alias is an error.
    let values (decoder: Decoder<'T>) : Decoder<'T list> =
        let scalarItem (node: YamlNode) : 'T =
            match node.Kind with
            | YamlNodeKind.Scalar _ -> decoder node
            | YamlNodeKind.Alias name -> failAlias name node
            | _ -> fail (expected "a scalar" node) node
        fun node -> List.ofSeq (decodeItems "a sequence of scalars" scalarItem node)

    /// A sequence of exactly two items.
    let tuple2 (decoderA: Decoder<'A>) (decoderB: Decoder<'B>) : Decoder<'A * 'B> =
        fun node ->
            let nodes = tupleNodes 2 node
            let a = inItem 0 decoderA nodes.[0]
            let b = inItem 1 decoderB nodes.[1]
            (a, b)

    let tuple3 (decoderA: Decoder<'A>) (decoderB: Decoder<'B>) (decoderC: Decoder<'C>) : Decoder<'A * 'B * 'C> =
        fun node ->
            let nodes = tupleNodes 3 node
            let a = inItem 0 decoderA nodes.[0]
            let b = inItem 1 decoderB nodes.[1]
            let c = inItem 2 decoderC nodes.[2]
            (a, b, c)

    let tuple4
        (decoderA: Decoder<'A>)
        (decoderB: Decoder<'B>)
        (decoderC: Decoder<'C>)
        (decoderD: Decoder<'D>)
        : Decoder<'A * 'B * 'C * 'D> =
        fun node ->
            let nodes = tupleNodes 4 node
            let a = inItem 0 decoderA nodes.[0]
            let b = inItem 1 decoderB nodes.[1]
            let c = inItem 2 decoderC nodes.[2]
            let d = inItem 3 decoderD nodes.[3]
            (a, b, c, d)

    let tuple5
        (decoderA: Decoder<'A>)
        (decoderB: Decoder<'B>)
        (decoderC: Decoder<'C>)
        (decoderD: Decoder<'D>)
        (decoderE: Decoder<'E>)
        : Decoder<'A * 'B * 'C * 'D * 'E> =
        fun node ->
            let nodes = tupleNodes 5 node
            let a = inItem 0 decoderA nodes.[0]
            let b = inItem 1 decoderB nodes.[1]
            let c = inItem 2 decoderC nodes.[2]
            let d = inItem 3 decoderD nodes.[3]
            let e = inItem 4 decoderE nodes.[4]
            (a, b, c, d, e)

    let tuple6
        (decoderA: Decoder<'A>)
        (decoderB: Decoder<'B>)
        (decoderC: Decoder<'C>)
        (decoderD: Decoder<'D>)
        (decoderE: Decoder<'E>)
        (decoderF: Decoder<'F>)
        : Decoder<'A * 'B * 'C * 'D * 'E * 'F> =
        fun node ->
            let nodes = tupleNodes 6 node
            let a = inItem 0 decoderA nodes.[0]
            let b = inItem 1 decoderB nodes.[1]
            let c = inItem 2 decoderC nodes.[2]
            let d = inItem 3 decoderD nodes.[3]
            let e = inItem 4 decoderE nodes.[4]
            let f = inItem 5 decoderF nodes.[5]
            (a, b, c, d, e, f)

    let tuple7
        (decoderA: Decoder<'A>)
        (decoderB: Decoder<'B>)
        (decoderC: Decoder<'C>)
        (decoderD: Decoder<'D>)
        (decoderE: Decoder<'E>)
        (decoderF: Decoder<'F>)
        (decoderG: Decoder<'G>)
        : Decoder<'A * 'B * 'C * 'D * 'E * 'F * 'G> =
        fun node ->
            let nodes = tupleNodes 7 node
            let a = inItem 0 decoderA nodes.[0]
            let b = inItem 1 decoderB nodes.[1]
            let c = inItem 2 decoderC nodes.[2]
            let d = inItem 3 decoderD nodes.[3]
            let e = inItem 4 decoderE nodes.[4]
            let f = inItem 5 decoderF nodes.[5]
            let g = inItem 6 decoderG nodes.[6]
            (a, b, c, d, e, f, g)

    let tuple8
        (decoderA: Decoder<'A>)
        (decoderB: Decoder<'B>)
        (decoderC: Decoder<'C>)
        (decoderD: Decoder<'D>)
        (decoderE: Decoder<'E>)
        (decoderF: Decoder<'F>)
        (decoderG: Decoder<'G>)
        (decoderH: Decoder<'H>)
        : Decoder<'A * 'B * 'C * 'D * 'E * 'F * 'G * 'H> =
        fun node ->
            let nodes = tupleNodes 8 node
            let a = inItem 0 decoderA nodes.[0]
            let b = inItem 1 decoderB nodes.[1]
            let c = inItem 2 decoderC nodes.[2]
            let d = inItem 3 decoderD nodes.[3]
            let e = inItem 4 decoderE nodes.[4]
            let f = inItem 5 decoderF nodes.[5]
            let g = inItem 6 decoderG nodes.[6]
            let h = inItem 7 decoderH nodes.[7]
            (a, b, c, d, e, f, g, h)

    /// A mapping as a map. The key decoder gets the whole key node. Two entries with equal decoded keys are an
    /// error that names both.
    let map (keyDecoder: Decoder<'K>) (valueDecoder: Decoder<'V>) : Decoder<Map<'K, 'V>> =
        fun node -> Map.ofSeq (decodePairs keyDecoder valueDecoder node)

    /// A mapping as a dictionary with the entries in source order. The key decoder gets the whole key node. Two
    /// entries with equal decoded keys are an error that names both.
    let dict (keyDecoder: Decoder<'K>) (valueDecoder: Decoder<'V>) : Decoder<Dictionary<'K, 'V>> =
        fun node ->
            let result = Dictionary<'K, 'V>()
            for (key, value) in decodePairs keyDecoder valueDecoder node do
                result.[key] <- value
            result

    /// The entries of a mapping in source order, with their comments and spans.
    let entries (node: YamlNode) : MappingEntry list = mappingEntries "a mapping" node

    /// The node itself. It also accepts an alias.
    let node (node: YamlNode) : YamlNode = node

    /// The name of the anchor that an alias refers to.
    let alias (node: YamlNode) : string =
        match node.Kind with
        | YamlNodeKind.Alias name -> name
        | _ -> fail (expected "an alias" node) node

    /// Reads a mapping through getters. The field names must be scalars and unique. `get.Optional.Field`
    /// gives None for a missing field and for a null value. `get.Entries` shows the difference.
    let object (build: IGetters -> 'T) : Decoder<'T> = fun node -> build (createGetters node)

    /// Runs the decoder and returns the error as a value.
    let tryDecode (decoder: Decoder<'T>) (node: YamlNode) : Result<'T, DecodeError> =
        try
            Ok (decoder node)
        with YamlDecodeException error ->
            Error error
