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

    /// What a null node is, for a message: a missing value, or the null with its text.
    let describeNull (node: YamlNode) : string =
        if YamlNode.isMissingValue node then
            "a missing value"
        else
            match node.Kind with
            | YamlNodeKind.Scalar scalar -> "the null \"" + shorten scalar.Value + "\""
            | _ -> "a null"

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

    let private nullTagMessage (text: string) : string =
        "The tag !!null needs null, Null, NULL, ~ or an empty text, found \"" + shorten text + "\"."

    /// An explicit `!!null` needs a null of the core schema. Any other text under the tag is an error in every
    /// decoder, as `!!int foo` is an error for an int.
    let private checkNullTag (node: YamlNode) (scalar: YamlScalar) : unit =
        if isNullTag node.Properties.Tag && not (CoreSchema.isNull node.Properties.Tag scalar) then
            fail (nullTagMessage scalar.Value) node

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

    /// What the node is when a decoder that needs a string rejects it. A plain scalar without a tag also names the
    /// tag that the core schema resolves it to, which a quoted spelling of the text would not have.
    let describeNonString (node: YamlNode) : string =
        match node.Kind with
        | YamlNodeKind.Scalar scalar when node.Properties.Tag.IsNone && scalar.Style = ScalarStyle.Plain ->
            if scalar.Value = "" then
                "a missing value"
            else
                let resolved =
                    match CoreSchema.resolveKind scalar.Value with
                    | CoreKind.Null -> "!!null"
                    | CoreKind.Bool -> "!!bool"
                    | CoreKind.Integer -> "!!int"
                    | CoreKind.Float -> "!!float"
                    | CoreKind.Str -> "!!str"
                describeNode node + ", which resolves to " + resolved
        | _ -> describeNode node

    /// The text of a scalar that the core schema reads as a string (`CoreSchema.isString`).
    let stringOf (what: string) (node: YamlNode) : string =
        let scalar = scalarOf what node
        if CoreSchema.isString node.Properties.Tag scalar then
            scalar.Value
        else
            fail ("Expected " + what + ", found " + describeNonString node + ".") node

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

    // ----------------------------------------------------------------------------------------------------------
    // The identity of a key
    //
    // A key node has a token, a string that two keys share exactly when they are equal YAML values (YAML 1.2.2,
    // section 3.2.1.3). The first character tells the kind. A string is "s" and its cooked text, a null is "n", a
    // bool is "btrue" or "bfalse", an int is "i" and its name in `CoreSchema.IntegerNames`, and a float is
    // "f" and the form of `CoreSchema.canonicalFloat`. A scalar under any other tag is "x", the length of the tag,
    // a colon, the tag and the cooked text. A collection is "q" for a sequence or "m" for a mapping, then its tag in the same
    // way and the numbers of the tokens of its children in an index that the check shares. A sequence lists them in
    // order and a mapping lists its pairs of key and value sorted by the number of the key, so the order of its
    // entries does not count. Every part has its length or its end, so no two values share a token.

    let private coreTag (suffix: string) : string = YamlTag.coreNamespace + suffix

    /// The state that the tokens of one check share. `Numbers` numbers the tokens of the children of collection
    /// keys, and `Integers` names the ints, so the token of an int costs time that grows with its length except
    /// for the one conversion that `CoreSchema.IntegerNames` describes.
    [<Sealed>]
    type KeyIndex() =
        member val Numbers = Dictionary<string, int>()
        member val Integers = CoreSchema.IntegerNames()

    let private scalarTagSuffixes = [ "str"; "null"; "bool"; "int"; "float" ]

    /// The place of a key, for the message of a repeated key.
    let private placeOf (key: YamlNode) (fallback: string) : string =
        match key.Span with
        | Some span -> "at " + spanText span
        | None -> fallback

    /// Fails at the second of two keys that are the same YAML value. The message names both spellings when they
    /// differ.
    let failRepeatedKey (first: YamlNode) (second: YamlNode) : 'T =
        let firstLabel = keyLabel first
        let secondLabel = keyLabel second
        let firstPlace = placeOf first "at an earlier entry"
        let place = placeOf second "at a later entry"
        let message =
            if firstLabel = secondLabel then
                (if secondLabel = "" then "The empty key" else "The key " + secondLabel) + " occurs twice, " + firstPlace + " and " + place + "."
            else
                let subject = if secondLabel = "" then "The empty key " else "The key " + secondLabel + " "
                let earlier = if firstLabel = "" then "the empty key " else "the key " + firstLabel + " "
                subject + place + " repeats " + earlier + firstPlace + "."
        fail message second

    /// The token of a scalar key. A tag of the core schema needs a text of its type.
    let private scalarToken (integers: CoreSchema.IntegerNames) (node: YamlNode) (scalar: YamlScalar) : string =
        let text = scalar.Value
        if CoreSchema.isString node.Properties.Tag scalar then
            "s" + text
        else
            let identifier =
                match node.Properties.Tag with
                | Some explicit -> YamlTag.identifier explicit
                | None ->
                    // A plain scalar without a tag that the core schema resolves to another type than a string.
                    match CoreSchema.resolveKind text with
                    | CoreKind.Null -> coreTag "null"
                    | CoreKind.Bool -> coreTag "bool"
                    | CoreKind.Integer -> coreTag "int"
                    | _ -> coreTag "float"
            let wrong (expected: string) : string =
                fail ("The tag !!" + identifier.Substring(YamlTag.coreNamespace.Length) + " needs " + expected + ", found \"" + shorten text + "\".") node
            if identifier = coreTag "null" then
                if CoreSchema.resolvesToNull text then "n" else fail (nullTagMessage text) node
            elif identifier = coreTag "bool" then
                match text with
                | "true" | "True" | "TRUE" -> "btrue"
                | "false" | "False" | "FALSE" -> "bfalse"
                | _ -> wrong "true, True, TRUE, false, False or FALSE"
            elif identifier = coreTag "int" then
                match integers.Name text with
                | Some name -> "i" + name
                | None -> wrong "an int of the core schema"
            elif identifier = coreTag "float" then
                match CoreSchema.canonicalFloat text with
                | Some form -> "f" + form
                | None -> wrong "a float of the core schema"
            elif identifier = coreTag "seq" then
                wrong "a sequence"
            elif identifier = coreTag "map" then
                wrong "a mapping"
            else
                "x" + string identifier.Length + ":" + identifier + text

    /// The tag of a collection in a token. Without a tag and under `!` a sequence has `!!seq` and a mapping has
    /// `!!map`. A tag of the core schema for another kind of node is an error.
    let private collectionTag (node: YamlNode) (own: string) : string =
        match node.Properties.Tag with
        | None
        | Some YamlTag.NonSpecific -> coreTag own
        | Some explicit ->
            let identifier = YamlTag.identifier explicit
            let found = if own = "seq" then "a sequence" else "a mapping"
            if identifier = coreTag "seq" && own <> "seq" then
                fail ("The tag " + tagText explicit + " needs a sequence, found " + found + ".") node
            elif identifier = coreTag "map" && own <> "map" then
                fail ("The tag " + tagText explicit + " needs a mapping, found " + found + ".") node
            elif scalarTagSuffixes |> List.exists (fun suffix -> identifier = coreTag suffix) then
                fail ("The tag " + tagText explicit + " needs a scalar, found " + found + ".") node
            else
                identifier

    [<RequireQualifiedAccess; NoEquality; NoComparison>]
    type private TokenStep =
        | Visit of YamlNode
        | FinishSequence of tag: string * count: int
        | FinishMapping of tag: string * keys: YamlNode[]

    /// The token of a key node of any depth. The walk keeps its own stack. A mapping inside the key that holds two
    /// equal keys is an error at the second one, and so is an alias.
    let private nodeToken (index: KeyIndex) (root: YamlNode) : string =
        let interned = index.Numbers
        let numberOf (token: string) : int =
            match interned.TryGetValue token with
            | true, number -> number
            | _ ->
                let number = interned.Count
                interned.[token] <- number
                number
        let steps = ResizeArray<TokenStep>()
        let tokens = ResizeArray<string>()
        steps.Add(TokenStep.Visit root)
        while steps.Count > 0 do
            let step = steps.[steps.Count - 1]
            steps.RemoveAt(steps.Count - 1)
            match step with
            | TokenStep.Visit node ->
                match node.Kind with
                | YamlNodeKind.Scalar scalar -> tokens.Add(scalarToken index.Integers node scalar)
                | YamlNodeKind.Alias name -> failAlias name node
                | YamlNodeKind.Sequence collection ->
                    let items = List.toArray collection.Items
                    steps.Add(TokenStep.FinishSequence(collectionTag node "seq", items.Length))
                    for i in items.Length - 1 .. -1 .. 0 do
                        steps.Add(TokenStep.Visit items.[i].Node)
                | YamlNodeKind.Mapping collection ->
                    let entries = List.toArray collection.Items
                    steps.Add(TokenStep.FinishMapping(collectionTag node "map", entries |> Array.map (fun entry -> entry.Key)))
                    for i in entries.Length - 1 .. -1 .. 0 do
                        steps.Add(TokenStep.Visit entries.[i].Value)
                        steps.Add(TokenStep.Visit entries.[i].Key)
            | TokenStep.FinishSequence (tag, count) ->
                let start = tokens.Count - count
                let sb = System.Text.StringBuilder()
                sb.Append("q").Append(string tag.Length).Append(":").Append(tag).Append("[") |> ignore
                for i in start .. tokens.Count - 1 do
                    if i > start then sb.Append(",") |> ignore
                    sb.Append(string (numberOf tokens.[i])) |> ignore
                tokens.RemoveRange(start, count)
                tokens.Add(sb.ToString())
            | TokenStep.FinishMapping (tag, keys) ->
                let start = tokens.Count - 2 * keys.Length
                let first = Dictionary<int, int>()
                let pairs: (int * int)[] = Array.zeroCreate keys.Length
                for i in 0 .. keys.Length - 1 do
                    let keyNumber = numberOf tokens.[start + 2 * i]
                    if first.ContainsKey keyNumber then
                        failRepeatedKey keys.[first.[keyNumber]] keys.[i]
                    first.[keyNumber] <- i
                    pairs.[i] <- (keyNumber, numberOf tokens.[start + 2 * i + 1])
                let sorted = pairs |> Array.sortBy fst
                let sb = System.Text.StringBuilder()
                sb.Append("m").Append(string tag.Length).Append(":").Append(tag).Append("{") |> ignore
                for i in 0 .. sorted.Length - 1 do
                    if i > 0 then sb.Append(",") |> ignore
                    sb.Append(string (fst sorted.[i])).Append(":").Append(string (snd sorted.[i])) |> ignore
                tokens.RemoveRange(start, 2 * keys.Length)
                tokens.Add(sb.ToString())
        tokens.[0]

    /// The tokens of the keys of the entries, in their order. Two keys with the same token are an error at the
    /// second one. A walk over many mappings can share the index.
    let keyTokens (index: KeyIndex) (entries: MappingEntry[]) : string[] =
        let tokens: string[] = Array.zeroCreate entries.Length
        let first = Dictionary<string, int>()
        for i in 0 .. entries.Length - 1 do
            let key = entries.[i].Key
            let token =
                match key.Kind with
                | YamlNodeKind.Scalar scalar -> scalarToken index.Integers key scalar
                | _ -> nodeToken index key
            if first.ContainsKey token then
                failRepeatedKey entries.[first.[token]].Key key
            first.[token] <- i
            tokens.[i] <- token
        tokens

    /// The text of a key whose token is a string, or None for any other key.
    let keyText (token: string) : string option =
        if token.[0] = 's' then Some (token.Substring 1) else None

    /// A place in the walk of `checkKeysBelow`. The segments of the path are built for an error only.
    [<NoEquality; NoComparison>]
    type KeyWalkPlace =
        { Parent: KeyWalkPlace option
          Key: YamlNode option
          Index: int }

    let private pathOf (place: KeyWalkPlace option) : string list =
        let mutable segments = []
        let mutable current = place
        while current.IsSome do
            let here = current.Value
            let segment =
                match here.Key with
                | Some key -> "{" + keyLabel key + "}"
                | None -> "[" + string here.Index + "]"
            segments <- segment :: segments
            current <- here.Parent
        segments

    /// Checks the keys of every mapping in the node, the node included, in the order of the text. The walk goes
    /// through the values and the items with its own stack, and the token of a collection key checks the mappings
    /// inside that key.
    let checkKeysBelow (root: YamlNode) : unit =
        let index = KeyIndex()
        let stack = ResizeArray<YamlNode * KeyWalkPlace option>()
        stack.Add((root, None))
        while stack.Count > 0 do
            let (node, place) = stack.[stack.Count - 1]
            stack.RemoveAt(stack.Count - 1)
            match node.Kind with
            | YamlNodeKind.Mapping collection ->
                let entries = List.toArray collection.Items
                try
                    keyTokens index entries |> ignore
                with YamlDecodeException error ->
                    raise (YamlDecodeException { error with Path = pathOf place @ error.Path })
                for i in entries.Length - 1 .. -1 .. 0 do
                    stack.Add((entries.[i].Value, Some { Parent = place; Key = Some entries.[i].Key; Index = i }))
            | YamlNodeKind.Sequence collection ->
                let items = List.toArray collection.Items
                for i in items.Length - 1 .. -1 .. 0 do
                    stack.Add((items.[i].Node, Some { Parent = place; Key = None; Index = i }))
            | _ -> ()

    let private pairKey (key: 'K) : PairKey<'K> =
        let boxed = box key
        if isNull boxed then
            PairKey.Null
        else
            match boxed with
            | :? float as number when Double.IsNaN number -> PairKey.NotANumber
            | _ -> PairKey.Value key

    /// Decodes the entries of a mapping to pairs in source order. The keys of the mapping have to be distinct YAML
    /// values (`keyTokens`) before the key decoder runs, and the key decoder gets the whole key node. Two distinct
    /// keys whose decoded keys are equal are an error too. A dictionary cannot hold a null key on .NET, so
    /// `allowNullKey` is false for it and a null key is an error on every target.
    let decodePairs
        (allowNullKey: bool)
        (keyDecoder: Decoder<'K>)
        (valueDecoder: Decoder<'V>)
        (node: YamlNode)
        : ResizeArray<'K * 'V> =
        let entries = List.toArray (mappingEntries "a mapping" node)
        keyTokens (KeyIndex()) entries |> ignore
        let pairs = ResizeArray<'K * 'V>()
        let seen = Dictionary<PairKey<'K>, YamlNode>()
        for entry in entries do
            let key = inEntry entry.Key keyDecoder entry.Key
            if not allowNullKey && isNull (box key) then
                fail ("The key " + keyLabel entry.Key + " decodes to null, which a dictionary cannot hold. Decode a map instead.") entry.Key
            match seen.TryGetValue (pairKey key) with
            | true, first ->
                let firstPlace = placeOf first "at an earlier entry"
                let place = placeOf entry.Key "at a later entry"
                fail
                    ("Distinct YAML keys decode to the same map key: " + describeNode first + " " + firstPlace + " and "
                     + describeNode entry.Key + " " + place + ".")
                    entry.Key
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

    /// Reads the fields that a mapping must have.
    type IRequiredGetter =
        /// The value of the field, read by the decoder. A mapping without the field is an error. An error of the
        /// decoder gets the name of the field in its path.
        abstract Field: string -> Decoder<'T> -> 'T

    /// Reads the fields that a mapping may leave out.
    type IOptionalGetter =
        /// The value of the field, read by the decoder, or None when the field is missing or null. The
        /// decoder never sees a null.
        abstract Field: string -> Decoder<'T> -> 'T option

    /// Collects the entries with known names without decoding them.
    type IMultipleOptionalGetter =
        /// The entries with these field names, in source order. A name that the mapping lacks adds nothing.
        abstract FieldList: string list -> MappingEntry list

    /// Collects the entries that the known names leave over, for the extension data of an object.
    type IOverflowGetter =
        /// The entries without these field names, in source order.
        abstract FieldList: string list -> MappingEntry list

    /// The getters that the builder of `Decode.object` receives.
    type IGetters =
        /// The fields that the mapping must have.
        abstract Required: IRequiredGetter
        /// The fields that the mapping may leave out.
        abstract Optional: IOptionalGetter
        /// The entries with known names, undecoded.
        abstract MultipleOptional: IMultipleOptionalGetter
        /// The entries that known names leave over, undecoded.
        abstract Overflow: IOverflowGetter
        /// Every entry of the mapping in source order. It shows a field with a null value, which
        /// `Optional.Field` reads as missing.
        abstract Entries: MappingEntry list

    /// The getters of a mapping. The keys have to be distinct YAML values (`uniqueKeys`) and strings by the rules
    /// of `string`. Distinct strings have distinct texts, so the field names go into one index, and each lookup
    /// costs the same however many entries the mapping has.
    let private createGetters (mapping: YamlNode) : IGetters =
        let entryList = mappingEntries "a mapping" mapping
        let entries = List.toArray entryList
        let tokens = keyTokens (KeyIndex()) entries
        let names: string[] = Array.zeroCreate entries.Length
        let index = Dictionary<string, int>()
        for i in 0 .. entries.Length - 1 do
            match keyText tokens.[i] with
            | Some name ->
                index.[name] <- i
                names.[i] <- name
            | None ->
                let key = entries.[i].Key
                fail ("Expected a string as a field name, found " + describeNonString key + ".") key
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

    /// A string of the YAML 1.2 core schema (`CoreSchema.isString`). The tag decides before the style. Under
    /// `!!str` and under the non-specific tag `!` the decoder reads the text of any scalar, and every other explicit
    /// tag is an error. Without a tag a quoted or block scalar is a string, and a plain scalar is one when the core
    /// schema resolves its text to a string. So a plain `1`, `true` or `null` and a missing value are errors, and
    /// `"1"`, `''`, `! 1` and `!!str 1` are strings. The error names the tag that a plain scalar resolves to. A
    /// collection and an alias are errors. `scalarText` reads the text of any scalar.
    let string (node: YamlNode) : string = stringOf "a string" node

    /// The cooked text of any scalar, whatever its style and tag. The text is `YamlScalar.Value` with its escapes
    /// and folds applied, and no spelling of the source. The spelling of a number or a bool stays as it is, and so
    /// does a null spelling. Empty content gives "". The decoder checks nothing against the schema, so it also reads
    /// a text that a tag rejects, such as `!!int foo` or `!!null foo`. A collection and an alias are errors.
    let scalarText (node: YamlNode) : string =
        match node.Kind with
        | YamlNodeKind.Scalar scalar -> scalar.Value
        | YamlNodeKind.Alias name -> failAlias name node
        | _ -> fail (expected "a scalar" node) node

    /// An int in 32 bits that the core schema reads from a plain scalar or from a scalar under `!!int`.
    let int (node: YamlNode) : int = typedScalar "an int" CoreSchema.tryParseInt node

    /// An int in 64 bits that the core schema reads from a plain scalar or from a scalar under `!!int`.
    let int64 (node: YamlNode) : int64 = typedScalar "an int64" CoreSchema.tryParseInt64 node

    /// A float that the core schema reads from a plain scalar or from a scalar under `!!float`. That includes
    /// the ints, `.inf`, `-.inf` and `.nan`.
    let float (node: YamlNode) : float = typedScalar "a float" CoreSchema.tryParseFloat node

    /// A bool that the core schema reads from a plain scalar or from a scalar under `!!bool`.
    let bool (node: YamlNode) : bool = typedScalar "a bool" CoreSchema.tryParseBool node

    /// A string of exactly one character by the rules of `string`. A quoted `"1"` is the char 1, and a plain `1`
    /// is an int and an error.
    let char (node: YamlNode) : char =
        let text = stringOf "a char" node
        if text.Length = 1 then text.[0] else fail (expected "a char" node) node

    /// A point in time. The text is a date, or a date and a time with a `T` or a blank between them. The time has
    /// seconds and may have a fraction of one to seven digits. A zone may end it, which is `Z` or an offset of
    /// `+hh:mm` or `-hh:mm` up to 14:00. A date alone is midnight. Any other text is an error on every target, also `1`, `2020`, `5` and
    /// `12:00`, which the parse of a target would read. A text with a zone gives the same instant on every target.
    /// .NET keeps the UTC kind of a `Z`. On Fable the kind depends on the time zone of the host.
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

    /// Rejects a node that is no value: a missing value, or a null of the core schema, which is a plain `null`,
    /// `Null`, `NULL` or `~`, or the same under `!!null`. The error is at the node. Any other node goes to the
    /// decoder, so a quoted `"null"` and `""` pass, and so does a collection. `Optional.Field` turns a null
    /// into None before its decoder runs, so `nonNull` belongs with `Required.Field`. There it gives one message
    /// for a null field in front of any decoder. `string` rejects a null as well, with the message of a string.
    let nonNull (decoder: Decoder<'T>) : Decoder<'T> =
        fun node ->
            if isNullNode node then
                fail ("Expected a value, found " + describeNull node + ".") node
            else
                decoder node

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

    /// A mapping as a map. The keys have to be distinct YAML values, as `uniqueKeys` checks them, and two equal keys
    /// are an error at the second one. Then the key decoder gets the whole key node. Two distinct YAML keys that
    /// decode to equal keys, such as `1` and `"1"` under `scalarText`, are the error "Distinct YAML keys decode to
    /// the same map key", which names both. Two decoded null keys are equal, and so are two decoded NaN keys.
    let map (keyDecoder: Decoder<'K>) (valueDecoder: Decoder<'V>) : Decoder<Map<'K, 'V>> =
        fun node -> Map.ofSeq (decodePairs true keyDecoder valueDecoder node)

    /// A mapping as a dictionary with the entries in source order. The keys follow the rules of `map`, and a key
    /// that decodes to null is an error because a .NET dictionary cannot hold it. `map` takes a null key. A NaN
    /// key stays in the dictionary, and .NET finds it again. The Dictionary of Fable never finds a NaN key on
    /// JavaScript and Python, so a program there reads it by enumerating the dictionary.
    let dict (keyDecoder: Decoder<'K>) (valueDecoder: Decoder<'V>) : Decoder<Dictionary<'K, 'V>> =
        fun node ->
            let result = Dictionary<'K, 'V>()
            for (key, value) in decodePairs false keyDecoder valueDecoder node do
                result.[key] <- value
            result

    /// The entries of a mapping in source order, with their comments and spans. The decoder returns every entry as
    /// written, repeated keys included. `uniqueKeys` checks the keys of a node, and `object` checks them as it decodes,
    /// like `map` and `dict`.
    let entries (node: YamlNode) : MappingEntry list = mappingEntries "a mapping" node

    /// The node itself. It also accepts an alias.
    let node (node: YamlNode) : YamlNode = node

    /// The name of the anchor that an alias refers to.
    let alias (node: YamlNode) : string =
        match node.Kind with
        | YamlNodeKind.Alias name -> name
        | _ -> fail (expected "an alias" node) node

    /// Reads a mapping through getters. The keys have to be distinct YAML values, as `uniqueKeys` checks them, and
    /// strings by the rules of `string`. A plain `1` or `true` as a key is an error at the key, and `"1"` is the
    /// field 1. `get.Optional.Field` gives None for a missing field and for a null value. `get.Entries` shows the
    /// difference. `entries`, `map` and `dict` read a mapping with other keys.
    let object (build: IGetters -> 'T) : Decoder<'T> = fun node -> build (createGetters node)

    /// Checks that no mapping in the node holds two equal keys, and then runs the decoder on the node. The check
    /// visits every mapping in the text of the node, the node included, also in the values that the decoder ignores
    /// and inside keys. The error stands at the second key with the path to its mapping, and its message names the
    /// place of the first key, and both spellings when they differ. A walk with its own stack makes a tree of any
    /// depth safe. The check belongs at the root, as in `Decode.uniqueKeys documentDecoder root`, because each call
    /// walks the whole node. `Decode.uniqueKeys Decode.node` checks a node alone.
    ///
    /// Two keys are equal when they are equal YAML values (YAML 1.2.2, section 3.2.1.3), whatever their style,
    /// anchor, comments and spans. A scalar compares its tag and its value, and a plain scalar without a tag has
    /// the tag that the core schema resolves. Strings compare by their cooked text. Every null spelling and empty
    /// content are one null, and the spellings of a bool are true or false. An int compares by its value in any
    /// radix and size, so `1`, `+1`, `01`, `0o1` and `0x1` are one key. A float compares by its exact decimal
    /// value, so `1.0`, `1.00`, `10e-1` and `0.1e1` are one key, every zero is one key with `-0.0` among them, and
    /// every spelling of `.nan` is one key. Two decimal texts that round to the same float stay two keys. The
    /// tags keep the types apart, so the int `1`, the float `1.0`, the bool `true` and the string `"1"` are four
    /// keys. A scalar under `!!null`, `!!bool`, `!!int` or `!!float` needs a text of that type, so `!!int nonsense`
    /// is an error and `!!float 1` is the float 1.0. Under any other tag a scalar compares by its tag and its cooked
    /// text. YAMLicious does not know other canonical forms, so two spellings of one value under a custom tag are
    /// not found as duplicates. A sequence key compares its tag and its items in order, and a mapping key compares
    /// its tag and its entries in any order. An alias as a key is an error, because aliases stay symbolic.
    /// `Aliases.expandNode` replaces them with copies first.
    let uniqueKeys (decoder: Decoder<'T>) : Decoder<'T> =
        fun node ->
            checkKeysBelow node
            decoder node

    /// Runs the decoder and returns the error as a value.
    let tryDecode (decoder: Decoder<'T>) (node: YamlNode) : Result<'T, DecodeError> =
        try
            Ok (decoder node)
        with YamlDecodeException error ->
            Error error
