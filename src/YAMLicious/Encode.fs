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
    let private numberAlternative = "[-+]?(?:\\.[0-9]+|[0-9]+(?:\\.[0-9]*)?)(?:[eE][-+]?[0-9]+)?"
    let private infinityAlternative = "[-+]?\\.(?:inf|Inf|INF)"
    let private nanAlternative = "\\.(?:nan|NaN|NAN)"

    let private anchored (alternative: string) = "^(?:" + alternative + ")$"

    let private nonStringPattern =
        [
            nullAlternative
            "true|True|TRUE|false|False|FALSE"
            "[-+]?[0-9]+"
            "0o[0-7]+"
            "0x[0-9a-fA-F]+"
            numberAlternative
            infinityAlternative
            nanAlternative
        ]
        |> String.concat "|"
        |> anchored

    let private nullPattern = anchored nullAlternative
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

    /// The float that a reader of the core schema takes from the text, or None when the text is not a decimal
    /// number, an infinity or not-a-number. A text that the number parse of the target accepts but the core
    /// schema does not, such as NaN or 0x1F, gives None.
    let tryParseFloat (text: string) : float option =
        if matches infinityPattern text then
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

    /// The content of a string scalar. Text that a core schema reader takes for null, a bool, an int or a
    /// float is double-quoted, so that it reads back as the string. Every other text has no style and the
    /// writer picks its form.
    let stringContent (text: string) =
        let style = if resolvesToNonString text then Some ScalarStyle.DoubleQuoted else None
        YAMLContent.create(text, ?style = style)

let int (value: int) = (YAMLContent.create(string value)) |> YAMLElement.Value

/// The core schema spells not-a-number as .nan and the infinities as .inf and -.inf, where ToString gives NaN
/// and Infinity. The JavaScript ToString writes negative zero as 0, so negative zero gets its own spelling.
/// Every other text is a float of the core schema on all targets, for example 1E+21 on .NET and 1e+21 on
/// JavaScript and Python.
let float (value: float) : YAMLElement =
    let text =
        if Double.IsNaN value then ".nan"
        elif Double.IsPositiveInfinity value then ".inf"
        elif Double.IsNegativeInfinity value then "-.inf"
        elif value = 0.0 && BitConverter.DoubleToInt64Bits value < 0L then "-0.0"
        else
            #if FABLE_COMPILER_JAVASCRIPT || FABLE_COMPILER_TYPESCRIPT
            value.ToString()
            #else
            value.ToString("R", CultureInfo.InvariantCulture)
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

let values (encoder: 'a -> string) (values: 'a seq) = 
    values 
    |> Seq.map (encoder >> YAMLContent.create >> YAMLElement.Value) 
    |> List.ofSeq 
    |> YAMLElement.Object

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