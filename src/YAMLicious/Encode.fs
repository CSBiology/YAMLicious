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

let int (value: int) = (YAMLContent.create(string value)) |> YAMLElement.Value

let float (value: float) = (YAMLContent.create(string value)) |> YAMLElement.Value

let char (value: char) = YAMLContent.create(string value) |> YAMLElement.Value

let bool (value: bool) = YAMLContent.create(string value) |> YAMLElement.Value

let inline map (value: Map<'a, 'b>) (keyEncoder: 'a -> string) (valueEncoder: 'b -> YAMLElement) = 
    [
        for KeyValue (k, v) in value do 
            YAMLElement.Mapping (YAMLContent.create(keyEncoder k), valueEncoder v) 
    ] 
    |> YAMLElement.Object

let inline dict (value: Dictionary<'a, 'b>) (keyEncoder: 'a -> string) (valueEncoder: 'b -> YAMLElement) = [for KeyValue (k, v) in value do YAMLElement.Mapping (YAMLContent.create(keyEncoder k), valueEncoder v) ] |> YAMLElement.Object

let datetime (value: DateTime) = (YAMLContent.create(value.ToString("O", CultureInfo.InvariantCulture))) |> YAMLElement.Value

let datetimeOffset (value: DateTimeOffset) = (YAMLContent.create(value.ToString("O", CultureInfo.InvariantCulture))) |> YAMLElement.Value

let inline option (value: 'a option) (enc: 'a -> YAMLElement) = 
    match value with
    | Some v -> enc v
    | None -> YAMLElement.Value <| YAMLContent.create YAML_NULL

let inline tuple2 (value: 'a * 'b) (encA: 'a -> YAMLElement) (encB: 'b -> YAMLElement) = YAMLElement.Sequence [encA (fst value); encB (snd value)]

let inline tuple3 (value: 'a * 'b * 'c) (encA: 'a -> YAMLElement) (encB: 'b -> YAMLElement) (encC: 'c -> YAMLElement) = 
    let a,b,c = value
    YAMLElement.Sequence [encA a; encB b; encC c]

let inline tuple4 (value: 'a * 'b * 'c * 'd) (encA: 'a -> YAMLElement) (encB: 'b -> YAMLElement) (encC: 'c -> YAMLElement) (encD: 'd -> YAMLElement) = 
    let a,b,c,d = value
    YAMLElement.Sequence [encA a; encB b; encC c; encD d]

let inline tuple5 (value: 'a * 'b * 'c * 'd * 'e) (encA: 'a -> YAMLElement) (encB: 'b -> YAMLElement) (encC: 'c -> YAMLElement) (encD: 'd -> YAMLElement) (encE: 'e -> YAMLElement) = 
    let a,b,c,d,e = value
    YAMLElement.Sequence [encA a; encB b; encC c; encD d; encE e]

let inline tuple6 
    (value: 'a * 'b * 'c * 'd * 'e * 'f) 
    (encA: 'a -> YAMLElement) 
    (encB: 'b -> YAMLElement) 
    (encC: 'c -> YAMLElement) 
    (encD: 'd -> YAMLElement) 
    (encE: 'e -> YAMLElement) 
    (encF: 'f -> YAMLElement) 
    = 
    let a,b,c,d,e,f = value
    YAMLElement.Sequence [encA a; encB b; encC c; encD d; encE e; encF f]

let inline tuple7 
    (value: 'a * 'b * 'c * 'd * 'e * 'f * 'g) 
    (encA: 'a -> YAMLElement) 
    (encB: 'b -> YAMLElement) 
    (encC: 'c -> YAMLElement) 
    (encD: 'd -> YAMLElement) 
    (encE: 'e -> YAMLElement) 
    (encF: 'f -> YAMLElement) 
    (encG: 'g -> YAMLElement) 
    = 
    let a,b,c,d,e,f,g = value
    YAMLElement.Sequence [encA a; encB b; encC c; encD d; encE e; encF f; encG g]

let inline tuple8 
    (value: 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h) 
    (encA: 'a -> YAMLElement) 
    (encB: 'b -> YAMLElement) 
    (encC: 'c -> YAMLElement) 
    (encD: 'd -> YAMLElement) 
    (encE: 'e -> YAMLElement) 
    (encF: 'f -> YAMLElement) 
    (encG: 'g -> YAMLElement) 
    (encH: 'h -> YAMLElement) 
    = 
    let a,b,c,d,e,f,g,h = value
    YAMLElement.Sequence [encA a; encB b; encC c; encD d; encE e; encF f; encG g; encH h]

let string (value: string) = (YAMLContent.create(value)) |> YAMLElement.Value

let seq (s: seq<'a>) (encoder: 'a -> YAMLElement) = s |> Seq.map encoder |> List.ofSeq |> YAMLElement.Sequence

let array (arr: 'a []) (encoder: 'a -> YAMLElement) = YAMLElement.Sequence (List.map encoder (List.ofArray arr))

let resizearray (arr: ResizeArray<'a>) (encoder: 'a -> YAMLElement) = YAMLElement.Sequence (List.map encoder (List.ofSeq arr))

let list (l: 'a list) (encoder: 'a -> YAMLElement) = YAMLElement.Sequence (List.map encoder l)

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