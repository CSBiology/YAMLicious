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

    /// The plain scalars that the tag resolution of the core schema (YAML 1.2.2, section 10.3.2) does not
    /// leave as strings. The alternatives are the regular expressions of the spec for the tags null, bool,
    /// int (decimal, octal and hexadecimal) and float (number, infinity and not-a-number).
    let private nonStringPattern =
        let alternatives = [
            "null|Null|NULL|~"
            "true|True|TRUE|false|False|FALSE"
            "[-+]?[0-9]+"
            "0o[0-7]+"
            "0x[0-9a-fA-F]+"
            "[-+]?(?:\\.[0-9]+|[0-9]+(?:\\.[0-9]*)?)(?:[eE][-+]?[0-9]+)?"
            "[-+]?\\.(?:inf|Inf|INF)"
            "\\.(?:nan|NaN|NAN)"
        ]
        "^(?:" + String.concat "|" alternatives + ")$"

    /// True when a reader of the core schema reads the text, written as a plain scalar, as null, a bool, an
    /// int or a float. The empty string counts, because an empty plain scalar is null. YAML 1.2 does not
    /// resolve yes, no, on and off, so they are strings here although YAML 1.1 reads them as bools.
    /// A text with a line break never resolves, so `$` cannot match in front of a final line break.
    let resolvesToNonString (text: string) =
        String.IsNullOrEmpty text
        || (not (text.Contains("\n"))
            && System.Text.RegularExpressions.Regex.IsMatch(text, nonStringPattern))

let int (value: int) = (YAMLContent.create(string value)) |> YAMLElement.Value

#if FABLE_COMPILER_JAVASCRIPT || FABLE_COMPILER_TYPESCRIPT
let float (value: float) : YAMLElement = (YAMLContent.create(value.ToString())) |> YAMLElement.Value
#else 
let float (value: float) : YAMLElement = (YAMLContent.create(value.ToString("R", CultureInfo.InvariantCulture))) |> YAMLElement.Value
#endif
let char (value: char) = YAMLContent.create(string value) |> YAMLElement.Value

/// The core schema spells the booleans true and false. The .NET ToString gives True and False.
let bool (value: bool) = YAMLContent.create(if value then "true" else "false") |> YAMLElement.Value

let inline map (keyEncoder: 'a -> string) (valueEncoder: 'b -> YAMLElement) (value: Map<'a, 'b>) = 
    [
        for KeyValue (k, v) in value do 
            YAMLElement.Mapping (YAMLContent.create(keyEncoder k), valueEncoder v) 
    ] 
    |> YAMLElement.Object

let inline dict (keyEncoder: 'a -> string) (valueEncoder: 'b -> YAMLElement) (value: Dictionary<'a, 'b>) = 
    [for KeyValue (k, v) in value do 
        YAMLElement.Mapping (YAMLContent.create(keyEncoder k), valueEncoder v) 
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
let string (value: string) =
    let style = if CoreSchema.resolvesToNonString value then Some ScalarStyle.DoubleQuoted else None
    YAMLContent.create(value, ?style = style) |> YAMLElement.Value

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
            YAMLElement.Mapping (YAMLContent.create(key), v) 
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