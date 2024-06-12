[<RequireQualifiedAccessAttribute>]
module YAMLicious.Decode

open System
open System.Collections.Generic

open YAMLicious
open YAMLicious.YAMLiciousTypes

module Helper =

    let inline raiseInvalidArg (name: string) (message: string) wrongV = 
        invalidArg name (sprintf "%s: %A" message wrongV)

let int (value: YAMLElement) : int =
    match value with
    | YAMLElement.Value v -> 
        match System.Int32.TryParse v.Value with
        | true, int -> int
        | false, _ -> Helper.raiseInvalidArg "value" "Expected an int" v.Value
    | anyElse -> Helper.raiseInvalidArg "value" "Expected an int" anyElse

let float (value: YAMLElement) : float =
    match value with
    | YAMLElement.Value v ->
        match System.Double.TryParse v.Value with
        | true, float -> float
        | false, _ -> Helper.raiseInvalidArg "value" "Expected a float" v.Value
    | anyElse -> Helper.raiseInvalidArg "value" "Expected a float" anyElse

let char (value: YAMLElement) : char =
    match value with
    | YAMLElement.Value v -> 
        match System.Char.TryParse v.Value with
        | true, _ -> v.Value.[0]
        | _ -> Helper.raiseInvalidArg "value" "Expected a char" v.Value
    | anyElse -> Helper.raiseInvalidArg "value" "Expected a char" anyElse

let bool (value: YAMLElement) : bool =
    match value with
    | YAMLElement.Value v -> 
        match System.Boolean.TryParse v.Value with
        | true, bool -> bool
        | false, _ -> Helper.raiseInvalidArg "value" "Expected a bool" v.Value
    | anyElse -> Helper.raiseInvalidArg "value" "Expected a bool" anyElse

let map (value: YAMLElement) (keyDecoder: YAMLElement -> 'a) (valueDecoder: YAMLElement -> 'b) : Map<'a, 'b> =
    match value with
    | YAMLElement.Object v -> 
        v |> List.map (fun x -> (keyDecoder x, valueDecoder x)) |> Map.ofList
    | anyElse -> Helper.raiseInvalidArg "value" "Expected a map" anyElse

let dict (value: YAMLElement) (keyDecoder: YAMLElement -> 'a) (valueDecoder: YAMLElement -> 'b) : Dictionary<'a, 'b> =
    match value with
    | YAMLElement.Object v -> 
        v |> List.map (fun x -> (keyDecoder x, valueDecoder x)) |> Map |> Dictionary
    | anyElse -> Helper.raiseInvalidArg "value" "Expected a dictionary" anyElse

let datetime (value: YAMLElement) : DateTime =
    match value with
    | YAMLElement.Value v -> 
        match System.DateTime.TryParse(v.Value) with
        | true, dateTime -> dateTime
        | false, _ -> Helper.raiseInvalidArg "value" "Expected a DateTime" v.Value
    | anyElse -> Helper.raiseInvalidArg "value" "Expected a DateTime" anyElse

let datetimeOffset (value: YAMLElement) : DateTimeOffset =
    match value with
    | YAMLElement.Value v -> 
        match System.DateTimeOffset.TryParse(v.Value) with
        | true, dateTimeOffset -> dateTimeOffset
        | false, _ -> Helper.raiseInvalidArg "value" "Expected a DateTimeOffset" v.Value
    | anyElse -> Helper.raiseInvalidArg "value" "Expected a DateTimeOffset" anyElse

let option (value: YAMLElement) (decoder: YAMLElement -> 'a) : 'a option =
    match value with
    | YAMLElement.Value v -> 
        match v.Value with
        | YAML_NULL -> None
        | _ -> Some (decoder value)
    | anyElse -> Helper.raiseInvalidArg "value" "Expected an option" anyElse

let tuple2 (value: YAMLElement) (decoderA: YAMLElement -> 'a) (decoderB: YAMLElement -> 'b) : 'a * 'b =
    match value with
    | YAMLElement.Sequence [a; b] -> (decoderA a, decoderB b)
    | anyElse -> Helper.raiseInvalidArg "value" "Expected a tuple2" anyElse

let tuple3 (value: YAMLElement) (decoderA: YAMLElement -> 'a) (decoderB: YAMLElement -> 'b) (decoderC: YAMLElement -> 'c) : 'a * 'b * 'c =
    match value with
    | YAMLElement.Sequence [a; b; c] -> (decoderA a, decoderB b, decoderC c)
    | anyElse -> Helper.raiseInvalidArg "value" "Expected a tuple3" anyElse

let tuple4 
    (value: YAMLElement) 
    (decoderA: YAMLElement -> 'a) 
    (decoderB: YAMLElement -> 'b) 
    (decoderC: YAMLElement -> 'c) 
    (decoderD: YAMLElement -> 'd) 
    : 'a * 'b * 'c * 'd 
    =
    match value with
    | YAMLElement.Sequence [a; b; c; d] -> (decoderA a, decoderB b, decoderC c, decoderD d)
    | anyElse -> Helper.raiseInvalidArg "value" "Expected a tuple4" anyElse

let tuple5 
    (value: YAMLElement) 
    (decoderA: YAMLElement -> 'a) 
    (decoderB: YAMLElement -> 'b) 
    (decoderC: YAMLElement -> 'c) 
    (decoderD: YAMLElement -> 'd) 
    (decoderE: YAMLElement -> 'e) 
    : 'a * 'b * 'c * 'd * 'e 
    =
    match value with
    | YAMLElement.Sequence [a; b; c; d; e] -> (decoderA a, decoderB b, decoderC c, decoderD d, decoderE e)
    | anyElse -> Helper.raiseInvalidArg "value" "Expected a tuple5" anyElse

let tuple6
    (value: YAMLElement) 
    (decoderA: YAMLElement -> 'a) 
    (decoderB: YAMLElement -> 'b) 
    (decoderC: YAMLElement -> 'c) 
    (decoderD: YAMLElement -> 'd) 
    (decoderE: YAMLElement -> 'e) 
    (decoderF: YAMLElement -> 'f) 
    : 'a * 'b * 'c * 'd * 'e * 'f 
    =
    match value with
    | YAMLElement.Sequence [a; b; c; d; e; f] -> (decoderA a, decoderB b, decoderC c, decoderD d, decoderE e, decoderF f)
    | anyElse -> Helper.raiseInvalidArg "value" "Expected a tuple6" anyElse

let tuple7
    (value: YAMLElement) 
    (decoderA: YAMLElement -> 'a) 
    (decoderB: YAMLElement -> 'b) 
    (decoderC: YAMLElement -> 'c) 
    (decoderD: YAMLElement -> 'd) 
    (decoderE: YAMLElement -> 'e) 
    (decoderF: YAMLElement -> 'f) 
    (decoderG: YAMLElement -> 'g) 
    : 'a * 'b * 'c * 'd * 'e * 'f * 'g 
    =
    match value with
    | YAMLElement.Sequence [a; b; c; d; e; f; g] -> (decoderA a, decoderB b, decoderC c, decoderD d, decoderE e, decoderF f, decoderG g)
    | anyElse -> Helper.raiseInvalidArg "value" "Expected a tuple7" anyElse

let tuple8
    (value: YAMLElement) 
    (decoderA: YAMLElement -> 'a) 
    (decoderB: YAMLElement -> 'b) 
    (decoderC: YAMLElement -> 'c) 
    (decoderD: YAMLElement -> 'd) 
    (decoderE: YAMLElement -> 'e) 
    (decoderF: YAMLElement -> 'f) 
    (decoderG: YAMLElement -> 'g) 
    (decoderH: YAMLElement -> 'h) 
    : 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h 
    =
    match value with
    | YAMLElement.Sequence [a; b; c; d; e; f; g; h] -> (decoderA a, decoderB b, decoderC c, decoderD d, decoderE e, decoderF f, decoderG g, decoderH h)
    | anyElse -> Helper.raiseInvalidArg "value" "Expected a tuple8" anyElse

let string (value: YAMLElement) : string =
    match value with
    | YAMLElement.Value v -> v.Value
    | anyElse -> failwith "Expected a string"

let list (value: YAMLElement) (decoder: YAMLElement -> 'a) : 'a list =
    match value with
    | YAMLElement.Sequence v -> v |> List.map decoder
    | anyElse -> Helper.raiseInvalidArg "value" "Expected a list" anyElse

let array (value: YAMLElement) (decoder: YAMLElement -> 'a) : 'a [] =
    match value with
    | YAMLElement.Sequence v -> v |> List.map decoder |> List.toArray
    | anyElse -> Helper.raiseInvalidArg "value" "Expected an array" anyElse

let seq (value: YAMLElement) (decoder: YAMLElement -> 'a) : seq<'a> =
    match value with
    | YAMLElement.Sequence v -> v |> Seq.map decoder
    | anyElse -> Helper.raiseInvalidArg "value" "Expected a seq" anyElse

let resizearray (value: YAMLElement) (decoder: YAMLElement -> 'a) : ResizeArray<'a> =
    match value with
    | YAMLElement.Sequence v -> v |> List.map decoder |> ResizeArray
    | anyElse -> Helper.raiseInvalidArg "value" "Expected a resizearray" anyElse

let object (value: YAMLElement) (decoder: YAMLElement -> 'a) : 'a =
    failwith "TODO"