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

    /// The error as one line of text. The path comes first when there is one, then the message and the start of
    /// its span.
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

/// The key of the index that finds duplicate keys. A null key and a not-a-number key are one key each on every
/// target. The Dictionary of .NET rejects a null key, and the Dictionary of Fable never finds a NaN again.
[<RequireQualifiedAccess>]
type internal PairKey<'K> =
    | Null
    | NotANumber
    | Value of 'K

module internal DecodeSupport =

    let spanText (span: SourceSpan) : string =
        "line " + string span.Start.Line + ", column " + string span.Start.Column

    let private shorten (text: string) : string =
        let flat = text.Replace("\n", "\\n")
        if flat.Length > 40 then flat.Substring(0, 37) + "..." else flat

    /// A tag as the text shows it. The core namespace gives `!!str` and a local tag gives `!local`. Any other
    /// tag shows as `!<uri>`.
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

    let private isNullTag (tag: YamlTag option) : bool =
        match tag with
        | Some explicit -> YamlTag.identifier explicit = YamlTag.coreNamespace + "null"
        | None -> false

    /// An explicit `!!null` needs a null of the core schema. Any other text under the tag is an error in every
    /// decoder, as `!!int foo` is an error for an int.
    let private checkNullTag (node: YamlNode) (scalar: YamlScalar) : unit =
        if isNullTag node.Properties.Tag && not (CoreSchema.isNull node.Properties.Tag scalar) then
            fail ("The tag !!null needs null, Null, NULL, ~ or an empty text, found \"" + shorten scalar.Value + "\".") node

    let scalarOf (what: string) (node: YamlNode) : YamlScalar =
        match node.Kind with
        | YamlNodeKind.Scalar scalar ->
            checkNullTag node scalar
            scalar
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
        | YamlNodeKind.Scalar scalar ->
            checkNullTag node scalar
            CoreSchema.isNull node.Properties.Tag scalar
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

    let private pairKey (key: 'K) : PairKey<'K> =
        let boxed = box key
        if isNull boxed then
            PairKey.Null
        else
            match boxed with
            | :? float as number when Double.IsNaN number -> PairKey.NotANumber
            | _ -> PairKey.Value key

    /// Decodes the entries of a mapping to pairs in source order. The key decoder gets the whole key node. Two
    /// entries whose decoded keys are equal are an error. A dictionary cannot hold a null key on .NET, so
    /// `allowNullKey` is false for it and a null key is an error on every target.
    let decodePairs
        (allowNullKey: bool)
        (keyDecoder: Decoder<'K>)
        (valueDecoder: Decoder<'V>)
        (node: YamlNode)
        : ResizeArray<'K * 'V> =
        let pairs = ResizeArray<'K * 'V>()
        let seen = Dictionary<PairKey<'K>, YamlNode>()
        let mutable remaining = mappingEntries "a mapping" node
        while not remaining.IsEmpty do
            let entry = remaining.Head
            remaining <- remaining.Tail
            let key = inEntry entry.Key keyDecoder entry.Key
            if not allowNullKey && isNull (box key) then
                fail ("The key " + keyLabel entry.Key + " decodes to null, which a dictionary cannot hold. Decode a map instead.") entry.Key
            match seen.TryGetValue (pairKey key) with
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
                seen.[pairKey key] <- entry.Key
                pairs.Add((key, inEntry entry.Key valueDecoder entry.Value))
        pairs

    /// The number that the `count` characters from `start` spell, or -1 when one of them is no digit or the text
    /// ends first.
    let private digitsAt (text: string) (start: int) (count: int) : int =
        if start + count > text.Length then
            -1
        else
            let mutable value = 0
            let mutable valid = true
            for i in start .. start + count - 1 do
                let c = text.[i]
                if c >= '0' && c <= '9' then
                    value <- value * 10 + (int c - int '0')
                else
                    valid <- false
            if valid then value else -1

    let private daysInMonth (year: int) (month: int) : int =
        if month = 2 then
            if (year % 4 = 0 && year % 100 <> 0) || year % 400 = 0 then 29 else 28
        elif month = 4 || month = 6 || month = 9 || month = 11 then
            30
        else
            31

    /// Checks a text against the forms of a timestamp: a date as `2018-10-01`, or a date and a time as
    /// `2018-10-01T11:12:55` with a `T` or a blank between them. The time has seconds and may have a fraction of
    /// one to seven digits. A zone may end it, which is `Z` or an offset of `+hh:mm` or `-hh:mm` up to 14:00. The
    /// date, the time and the zone must be possible values. The parse of a target reads far more than that, such as `1`, `2020`, `5` and
    /// `12:00`, so the check runs first and every target accepts the same texts.
    ///
    /// The result is the text in the one form that every target parses the same way: a `T` between the date and
    /// the time, and midnight behind a date alone. It is None when the text does not fit, or when `zoneRequired`
    /// is set and the text has no zone.
    let normalizeTimestamp (text: string) (zoneRequired: bool) : string option =
        let length = text.Length
        if length < 10 || text.[4] <> '-' || text.[7] <> '-' then
            None
        else
            let year = digitsAt text 0 4
            let month = digitsAt text 5 2
            let day = digitsAt text 8 2
            if year < 0 || month < 1 || month > 12 || day < 1 || day > daysInMonth year month then
                None
            elif length = 10 then
                if zoneRequired then None else Some (text + "T00:00:00")
            elif (text.[10] <> 'T' && text.[10] <> ' ') || length < 19 || text.[13] <> ':' || text.[16] <> ':' then
                None
            else
                let hour = digitsAt text 11 2
                let minute = digitsAt text 14 2
                let second = digitsAt text 17 2
                if hour < 0 || hour > 23 || minute < 0 || minute > 59 || second < 0 || second > 59 then
                    None
                else
                    let mutable index = 19
                    let mutable valid = true
                    if index < length && text.[index] = '.' then
                        index <- index + 1
                        let start = index
                        while index < length && text.[index] >= '0' && text.[index] <= '9' do
                            index <- index + 1
                        if index - start < 1 || index - start > 7 then
                            valid <- false
                    let mutable hasZone = false
                    if valid && index < length then
                        if text.[index] = 'Z' && index + 1 = length then
                            hasZone <- true
                        elif (text.[index] = '+' || text.[index] = '-') && index + 6 = length && text.[index + 3] = ':' then
                            let zoneHours = digitsAt text (index + 1) 2
                            let zoneMinutes = digitsAt text (index + 4) 2
                            if zoneHours >= 0 && zoneMinutes >= 0 && zoneMinutes <= 59 && zoneHours * 60 + zoneMinutes <= 840 then
                                hasZone <- true
                            else
                                valid <- false
                        else
                            valid <- false
                    if not valid || (zoneRequired && not hasZone) then
                        None
                    else
                        Some (text.Substring(0, 10) + "T" + text.Substring(11))

    /// Reads the text of a scalar as a point in time. The text goes through `normalizeTimestamp` before the parse
    /// of the target.
    let parseDateTime (text: string) : DateTime option =
        match normalizeTimestamp text false with
        | None -> None
        | Some normalized ->
#if FABLE_COMPILER
            match DateTime.TryParse normalized with
#else
            match DateTime.TryParse(normalized, CultureInfo.InvariantCulture, DateTimeStyles.RoundtripKind) with
#endif
            | true, value -> Some value
            | false, _ -> None

    /// Reads the text of a scalar as a point in time with an offset. A text without a zone has no offset, so it
    /// is no match.
    let parseDateTimeOffset (text: string) : DateTimeOffset option =
        match normalizeTimestamp text true with
        | None -> None
        | Some normalized ->
#if FABLE_COMPILER
            match DateTimeOffset.TryParse normalized with
#else
            match DateTimeOffset.TryParse(normalized, CultureInfo.InvariantCulture, DateTimeStyles.None) with
#endif
            | true, value -> Some value
            | false, _ -> None

/// The decoders. A decoder reads the node and ignores its presentation: comments, anchors, spans and the
/// style of a collection. A scalar decoder follows the YAML 1.2 core schema with the explicit tag and the
/// style of the scalar, so `"1"` is a string for `int` and `!!int "1"` is an int. An explicit `!!null` needs a
/// null spelling or an empty text, and any other text under it is an error in every decoder. Every decoder
/// except `alias` and `node` rejects an alias.
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
                        // The null check can raise too, for an explicit !!null with a text that is no null, so it runs inside the segment.
                        let orNone (value: YamlNode) = if isNullNode value then None else Some (decoder value)
                        inSegment name orNone entries.[i].Value
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

    /// A point in time. The text is a date, or a date and a time with a `T` or a blank between them. The time has
    /// seconds and may have a fraction of one to seven digits. A zone may end it, which is `Z` or an offset of
    /// `+hh:mm` or `-hh:mm` up to 14:00. A date alone is midnight. Any other text is an error on every target, also `1`, `2020`, `5` and
    /// `12:00`, which the parse of a target would read. A text with a zone gives the same instant on every target.
    /// .NET keeps the UTC kind of a `Z`. Fable gives the kind Local on Python and Unspecified on JavaScript.
    let datetime (node: YamlNode) : DateTime =
        let scalar = scalarOf "a DateTime" node
        match parseDateTime scalar.Value with
        | Some value -> value
        | None -> fail (expected "a DateTime" node) node

    /// A point in time with an offset. The text has the forms of `datetime` and needs a zone, so `2018-10-01` and
    /// a time without a zone are errors on every target. A text with a zone gives the same instant on every
    /// target. .NET and Python keep the offset of the text, and JavaScript gives the offset of the machine.
    let datetimeOffset (node: YamlNode) : DateTimeOffset =
        let scalar = scalarOf "a DateTimeOffset" node
        match parseDateTimeOffset scalar.Value with
        | Some value -> value
        | None -> fail (expected "a DateTimeOffset" node) node

    /// None for a null. Under an explicit `!!null` the tag decides: a null spelling or an empty text gives None,
    /// also in quotes, and any other text is an error. Without that tag only a plain scalar is null, with the text
    /// `null`, `Null`, `NULL` or `~` or with no text, as a missing value has. A quoted or block scalar is a string.
    /// A collection is never null. Anything else goes to the decoder.
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
    /// error that names both. Two null keys are equal, and so are two NaN keys.
    let map (keyDecoder: Decoder<'K>) (valueDecoder: Decoder<'V>) : Decoder<Map<'K, 'V>> =
        fun node -> Map.ofSeq (decodePairs true keyDecoder valueDecoder node)

    /// A mapping as a dictionary with the entries in source order. The key decoder gets the whole key node. Two
    /// entries with equal decoded keys are an error that names both, and a key that decodes to null is an
    /// error because a .NET dictionary cannot hold it. `map` takes a null key.
    let dict (keyDecoder: Decoder<'K>) (valueDecoder: Decoder<'V>) : Decoder<Dictionary<'K, 'V>> =
        fun node ->
            let result = Dictionary<'K, 'V>()
            for (key, value) in decodePairs false keyDecoder valueDecoder node do
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
