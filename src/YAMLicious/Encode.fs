/// To YAML
[<RequireQualifiedAccessAttribute>]
module YAMLicious.Encode

open System
open System.Collections.Generic
open System.Globalization

open YAMLicious
open YAMLicious.YAMLiciousTypes

module Helper =

    let inline getUnionCaseName (x:'a) = 
        match Microsoft.FSharp.Reflection.FSharpValue.GetUnionFields(x, typeof<'a>) with
        | case, _ -> case.Name

/// The YAML 1.2.2 core schema, which decides what a plain scalar means.
module CoreSchema =

    // The regular expressions of the tag resolution of the core schema (YAML 1.2.2, section 10.3.2) for the
    // tags null, bool, int (decimal, octal and hexadecimal) and float (number, infinity and not-a-number).
    let private nullAlternative = "null|Null|NULL|~"
    let private boolAlternative = "true|True|TRUE|false|False|FALSE"
    let private decimalAlternative = "[-+]?[0-9]+"
    let private octalAlternative = "0o[0-7]+"
    let private hexadecimalAlternative = "0x[0-9a-fA-F]+"
    let private numberAlternative = "[-+]?(?:\\.[0-9]+|[0-9]+(?:\\.[0-9]*)?)(?:[eE][-+]?[0-9]+)?"
    let private infinityAlternative = "[-+]?\\.(?:inf|Inf|INF)"
    let private nanAlternative = "\\.(?:nan|NaN|NAN)"

    let private anchored (alternative: string) = "^(?:" + alternative + ")$"

    let private nonStringPattern =
        [
            nullAlternative
            boolAlternative
            decimalAlternative
            octalAlternative
            hexadecimalAlternative
            numberAlternative
            infinityAlternative
            nanAlternative
        ]
        |> String.concat "|"
        |> anchored

    let private nullPattern = anchored nullAlternative
    let private decimalPattern = anchored decimalAlternative
    let private octalPattern = anchored octalAlternative
    let private hexadecimalPattern = anchored hexadecimalAlternative
    let private numberPattern = anchored numberAlternative
    let private infinityPattern = anchored infinityAlternative
    let private nanPattern = anchored nanAlternative

    /// A text with a line break never resolves, so `$` cannot match in front of a final line break.
    let private matches (pattern: string) (text: string) =
        not (text.Contains("\n"))
        && System.Text.RegularExpressions.Regex.IsMatch(text, pattern)

    /// True when a reader of the core schema reads the text, written as a plain scalar, as null. The empty
    /// string counts, because an empty plain scalar is null.
    let resolvesToNull (text: string) =
        String.IsNullOrEmpty text || matches nullPattern text

    /// True when a reader of the core schema reads the text, written as a plain scalar, as null, a bool, an
    /// int or a float. YAML 1.2 does not resolve yes, no, on and off, so they are strings here although
    /// YAML 1.1 reads them as bools.
    let resolvesToNonString (text: string) =
        String.IsNullOrEmpty text || matches nonStringPattern text

    /// True when the scalar can have the type that `tag` names. A plain scalar has the type that its text
    /// says. A quoted or block scalar is a string. An explicit tag fixes the type, whatever the style.
    let private resolvesAs (tag: string) (content: YAMLContent) =
        match content.Tag with
        | Some explicit -> explicit = tag
        | None ->
            match content.Style with
            | None
            | Some ScalarStyle.Plain -> true
            | _ -> false

    /// True when a reader of the core schema reads the scalar as null. That is a plain scalar with the text
    /// null, Null, NULL, ~ or nothing, or a scalar under the tag !!null with such a text. A scalar under
    /// another tag, such as `!!str null`, is not null.
    let isNull (content: YAMLContent) =
        resolvesAs "tag:yaml.org,2002:null" content && resolvesToNull content.Value

    /// The bool that a reader of the core schema takes from the scalar, or None when the scalar is not a
    /// bool. Only true, True, TRUE, false, False and FALSE count. YAML 1.2 reads yes, no, on and off as strings.
    let tryParseBool (content: YAMLContent) : bool option =
        if resolvesAs "tag:yaml.org,2002:bool" content then
            match content.Value with
            | "true" | "True" | "TRUE" -> Some true
            | "false" | "False" | "FALSE" -> Some false
            | _ -> None
        else
            None

    /// The int64 that a reader of the core schema takes from the scalar, or None when the scalar is not an int
    /// or does not fit in 64 bits. The core schema sets no size limit for an int. A decimal int has an
    /// optional sign. An octal int starts with 0o and a hexadecimal int with 0x.
    let tryParseInt64 (content: YAMLContent) : int64 option =
        let text = content.Value
        let digitValue (c: char) =
            if c <= '9' then int c - int '0'
            elif c >= 'a' then int c - int 'a' + 10
            else int c - int 'A' + 10

        // A negative int counts down from zero and a positive int counts up, so that Int64.MinValue needs no
        // negation. Each step compares against the bound before it multiplies, so no value leaves the int64
        // range. Fable emulates int64 on JavaScript and Python, where a product beyond the range wraps.
        let sum (start: int) (radix: int) (negative: bool) : int64 option =
            let radix64 = int64 radix
            let lowestBeforeStep = Int64.MinValue / radix64
            let highestBeforeStep = Int64.MaxValue / radix64
            let mutable total = 0L
            let mutable index = start
            let mutable fits = true
            while fits && index < text.Length do
                let digit = int64 (digitValue text.[index])
                if negative then
                    if total < lowestBeforeStep || total * radix64 < Int64.MinValue + digit then
                        fits <- false
                    else
                        total <- total * radix64 - digit
                else
                    if total > highestBeforeStep || total * radix64 > Int64.MaxValue - digit then
                        fits <- false
                    else
                        total <- total * radix64 + digit
                index <- index + 1
            if fits then Some total else None

        if not (resolvesAs "tag:yaml.org,2002:int" content) then
            None
        elif matches decimalPattern text then
            sum (if text.[0] = '-' || text.[0] = '+' then 1 else 0) 10 (text.[0] = '-')
        elif matches octalPattern text then
            sum 2 8 false
        elif matches hexadecimalPattern text then
            sum 2 16 false
        else
            None

    /// The int that a reader of the core schema takes from the scalar, or None when the scalar is not an int
    /// or does not fit in 32 bits. It is the 32 bit view of `tryParseInt64`.
    let tryParseInt (content: YAMLContent) : int option =
        tryParseInt64 content
        |> Option.filter (fun number -> number >= int64 Int32.MinValue && number <= int64 Int32.MaxValue)
        |> Option.map int

    /// The float that a reader of the core schema takes from the scalar, or None when the scalar is not a
    /// decimal number, an infinity or not-a-number. A text that the number parse of the target accepts but
    /// the core schema does not, such as NaN or 0x1F, gives None.
    let tryParseFloat (content: YAMLContent) : float option =
        let text = content.Value
        if not (resolvesAs "tag:yaml.org,2002:float" content) then
            None
        elif matches infinityPattern text then
            Some (if text.[0] = '-' then Double.NegativeInfinity else Double.PositiveInfinity)
        elif matches nanPattern text then
            Some Double.NaN
        elif matches numberPattern text then
            #if FABLE_COMPILER
            match Double.TryParse text with // numberstyle and culture return not implemented warning
            #else
            match Double.TryParse(text, NumberStyles.Float, CultureInfo.InvariantCulture) with
            #endif
            | true, number -> Some number
            | false, _ -> None
        else
            None

    /// The text of a finite float. A whole number such as 1 gets ".0", because a reader of the core schema
    /// reads the text 1 as an int and 1.0 as a float. Every other text of a float already has a point or
    /// an exponent.
    let floatText (text: string) =
        if matches decimalPattern text then text + ".0" else text

    /// The content of a string scalar. Text that a core schema reader takes for null, a bool, an int or a
    /// float is double-quoted, so that it reads back as the string. Every other text has no style and the
    /// writer picks its form.
    let stringContent (text: string) =
        let style = if resolvesToNonString text then Some ScalarStyle.DoubleQuoted else None
        YAMLContent.create(text, ?style = style)

let int (value: int) = (YAMLContent.create(string value)) |> YAMLElement.Value

let int64 (value: int64) = (YAMLContent.create(string value)) |> YAMLElement.Value

#if FABLE_COMPILER_PYTHON
/// The Python repr of a float is the shortest text that reads back as the same number. It switches to an
/// exponent for large and small magnitudes (1e+21, 5e-324), where the Fable ToString writes every digit.
/// The Fable float64 has its own repr, so the number becomes a Python float first.
[<Fable.Core.Emit("repr(float($0))")>]
let private pyRepr (value: float) : string = Fable.Core.Util.nativeOnly
#endif

/// The core schema spells not-a-number as .nan and the infinities as .inf and -.inf, where ToString gives NaN
/// and Infinity. The JavaScript ToString writes negative zero as 0, so negative zero gets its own spelling.
/// A finite number is written in the shortest text that reads back as the same number. A whole number gets
/// ".0", because the core schema reads the text 1 as an int and 1.0 as a float. The exponent form depends on
/// the target: 1E+21 on .NET, 1e+21 on JavaScript and Python.
let float (value: float) : YAMLElement =
    let text =
        if Double.IsNaN value then ".nan"
        elif Double.IsPositiveInfinity value then ".inf"
        elif Double.IsNegativeInfinity value then "-.inf"
        elif value = 0.0 && BitConverter.DoubleToInt64Bits value < 0L then "-0.0"
        else
            #if FABLE_COMPILER_PYTHON
            CoreSchema.floatText (pyRepr value)
            #else
            #if FABLE_COMPILER_JAVASCRIPT || FABLE_COMPILER_TYPESCRIPT
            CoreSchema.floatText (value.ToString())
            #else
            CoreSchema.floatText (value.ToString("R", CultureInfo.InvariantCulture))
            #endif
            #endif
    YAMLContent.create text |> YAMLElement.Value

/// The core schema spells the booleans true and false. The .NET ToString gives True and False.
let bool (value: bool) = YAMLContent.create(if value then "true" else "false") |> YAMLElement.Value

let inline map (keyEncoder: 'a -> string) (valueEncoder: 'b -> YAMLElement) (value: Map<'a, 'b>) = 
    [
        for KeyValue (k, v) in value do 
            YAMLElement.Mapping (CoreSchema.stringContent (keyEncoder k), valueEncoder v) 
    ] 
    |> YAMLElement.Object

let inline dict (keyEncoder: 'a -> string) (valueEncoder: 'b -> YAMLElement) (value: Dictionary<'a, 'b>) = 
    [for KeyValue (k, v) in value do 
        YAMLElement.Mapping (CoreSchema.stringContent (keyEncoder k), valueEncoder v) 
    ] |> YAMLElement.Object

let datetime (value: DateTime) = (YAMLContent.create(value.ToString("O", CultureInfo.InvariantCulture))) |> YAMLElement.Value

let datetimeOffset (value: DateTimeOffset) = (YAMLContent.create(value.ToString("O", CultureInfo.InvariantCulture))) |> YAMLElement.Value

let inline option (enc: 'a -> YAMLElement) (value: 'a option) = 
    match value with
    | Some v -> enc v
    | None -> YAMLElement.Value <| YAMLContent.create YAML_NULL

let inline tuple2 (encA: 'a -> YAMLElement) (encB: 'b -> YAMLElement) (value: 'a * 'b) = YAMLElement.Sequence [encA (fst value); encB (snd value)]

let inline tuple3 (encA: 'a -> YAMLElement) (encB: 'b -> YAMLElement) (encC: 'c -> YAMLElement) (value: 'a * 'b * 'c) = 
    let a,b,c = value
    YAMLElement.Sequence [encA a; encB b; encC c]

let inline tuple4 (encA: 'a -> YAMLElement) (encB: 'b -> YAMLElement) (encC: 'c -> YAMLElement) (encD: 'd -> YAMLElement) (value: 'a * 'b * 'c * 'd) = 
    let a,b,c,d = value
    YAMLElement.Sequence [encA a; encB b; encC c; encD d]

let inline tuple5 (encA: 'a -> YAMLElement) (encB: 'b -> YAMLElement) (encC: 'c -> YAMLElement) (encD: 'd -> YAMLElement) (encE: 'e -> YAMLElement) (value: 'a * 'b * 'c * 'd * 'e) = 
    let a,b,c,d,e = value
    YAMLElement.Sequence [encA a; encB b; encC c; encD d; encE e]

let inline tuple6 
    (encA: 'a -> YAMLElement) 
    (encB: 'b -> YAMLElement) 
    (encC: 'c -> YAMLElement) 
    (encD: 'd -> YAMLElement) 
    (encE: 'e -> YAMLElement) 
    (encF: 'f -> YAMLElement) 
    (value: 'a * 'b * 'c * 'd * 'e * 'f) 
    = 
    let a,b,c,d,e,f = value
    YAMLElement.Sequence [encA a; encB b; encC c; encD d; encE e; encF f]

let inline tuple7 
    (encA: 'a -> YAMLElement) 
    (encB: 'b -> YAMLElement) 
    (encC: 'c -> YAMLElement) 
    (encD: 'd -> YAMLElement) 
    (encE: 'e -> YAMLElement) 
    (encF: 'f -> YAMLElement) 
    (encG: 'g -> YAMLElement) 
    (value: 'a * 'b * 'c * 'd * 'e * 'f * 'g) 
    = 
    let a,b,c,d,e,f,g = value
    YAMLElement.Sequence [encA a; encB b; encC c; encD d; encE e; encF f; encG g]

let inline tuple8 
    (encA: 'a -> YAMLElement) 
    (encB: 'b -> YAMLElement) 
    (encC: 'c -> YAMLElement) 
    (encD: 'd -> YAMLElement) 
    (encE: 'e -> YAMLElement) 
    (encF: 'f -> YAMLElement) 
    (encG: 'g -> YAMLElement) 
    (encH: 'h -> YAMLElement)
    (value: 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h)
    = 
    let a,b,c,d,e,f,g,h = value
    YAMLElement.Sequence [encA a; encB b; encC c; encD d; encE e; encF f; encG g; encH h]

/// A string node. Text that a core schema reader takes for null, a bool, an int or a float is double-quoted,
/// so that it reads back as the string. Every other string has no style and the writer picks its form.
let string (value: string) = CoreSchema.stringContent value |> YAMLElement.Value

let char (value: char) = string (value.ToString())

let seq (encoder: 'a -> YAMLElement) (s: seq<'a>) = s |> Seq.map encoder |> List.ofSeq |> YAMLElement.Sequence

let array (encoder: 'a -> YAMLElement) (arr: 'a []) = YAMLElement.Sequence (List.map encoder (List.ofArray arr))

let resizearray (encoder: 'a -> YAMLElement) (arr: ResizeArray<'a>) = YAMLElement.Sequence (List.map encoder (List.ofSeq arr))

let list (encoder: 'a -> YAMLElement) (l: 'a list) = YAMLElement.Sequence (List.map encoder l)

/// A sequence of string scalars, the counterpart of `Decode.values`. A block sequence is the only form in
/// which several scalars read back as several values. Siblings in an `Object` read back as one multiline
/// scalar. Each item goes through `string`, so a text such as `true` is double-quoted.
let values (encoder: 'a -> string) (values: 'a seq) = seq (encoder >> string) values

let inline object (objSeq: #seq<(string*YAMLElement)>) = 
    [
        for (key, v) in objSeq do 
            YAMLElement.Mapping (CoreSchema.stringContent key, v) 
    ]
    |> YAMLElement.Object

let inline withComment (comment: string) (ele: YAMLElement) = 
    match ele with
    | YAMLElement.Value v -> YAMLElement.Value {v with Comment = Some comment}
    | YAMLElement.Mapping (k, v) -> YAMLElement.Mapping ({k with Comment = Some comment}, v)
    | anyElse -> invalidArg "ele" (sprintf "Unsupported element type for `Encode.withComment`: %s" (Helper.getUnionCaseName anyElse))

let comment (string) = string |> YAMLElement.Comment

let nil = YAMLElement.Nil

/// Try to encode the given object using the given encoder, or return Encode.nil if the object is null
let tryInclude (name : string) (encoder : 'a -> YAMLElement) (value : 'a option) = 
    name,
    match value with
    | Some(o) -> encoder o
    | _ -> nil

let inline choose (kvs : (string * YAMLElement) list) = 
    kvs
    |> List.choose (fun (k,v) -> 
        if v = YAMLElement.Nil then None
        else Some (k,v)
    )

let write (whitespaces: int) (ele: YAMLElement) =
    Writer.write ele (Some (fun c -> {c with Whitespace = whitespaces}))