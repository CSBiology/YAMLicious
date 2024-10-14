/// From YAML
[<RequireQualifiedAccessAttribute>]
module YAMLicious.Decode

open System
open System.Collections.Generic

open YAMLicious
open YAMLicious.YAMLiciousTypes
open Fable.Core

[<AutoOpen>]
module Interop =

    [<Erase>]
    type IExports =
        abstract Number: obj

    [<ImportAll("numbers")>]
    let numbers: IExports = nativeOnly

    [<Global; Emit("list")>]
    let pyList: obj = nativeOnly

    [<Global; Emit("dict")>]
    let pyDict: obj = nativeOnly

    [<Global; Emit("int")>]
    let pyInt: obj = nativeOnly

    [<Global; Emit("bool")>]
    let pyBool: obj = nativeOnly

module Helper =

    let inline raiseInvalidArg (name: string) (message: string) wrongV = 
        invalidArg name (sprintf "%s: %A" message wrongV)

    //let isInt (value: string) =
    //    #if FABLE_COMPILER_JAVASCRIPT || FABLE_COMPILER_TYPESCRIPT
    //    JsInterop.isInt value
    //    #else
    //    match System.Int32.TryParse value with
    //    | true, _ -> true
    //    | false, _ -> false
    //    #endif

    //let isFloat (value: string) =
    //    #if FABLE_COMPILER_JAVASCRIPT || FABLE_COMPILER_TYPESCRIPT
    //    JsInterop.isInt value
    //    #else
    //    match System.Double.TryParse value with
    //    | true, _ -> true
    //    | false, _ -> false
    //    #endif

    let isString (value: string) =
        #if FABLE_COMPILER_JAVASCRIPT || FABLE_COMPILER_TYPESCRIPT
        JsInterop.isString value
        #endif
        #if FABLE_COMPILER_PYTHON
        YAMLicious.PyInterop.isString value
        #endif
        #if !FABLE_COMPILER
        match System.Char.TryParse value with
        | true, _ -> true
        | false, _ -> false
        #endif

open Helper
let int (value: YAMLElement) : int =
    match value with
    | YAMLElement.Value v | YAMLElement.Object [YAMLElement.Value v] -> 
        match System.Int32.TryParse v.Value with
        | true, v -> v
        | false, _ -> raiseInvalidArg "value" "Expected an int" v.Value
    | _ -> raiseInvalidArg "value" "Expected an YAMLElement.Value" value

let float (value: YAMLElement) : float =
    let Culture = System.Globalization.CultureInfo.InvariantCulture
    match value with
    | YAMLElement.Value v | YAMLElement.Object [YAMLElement.Value v] -> 
        #if FABLE_COMPILER
        match System.Double.TryParse(v.Value) with // numberstyle and culture return not implemented warning
        #else
        match System.Double.TryParse(v.Value, System.Globalization.NumberStyles.Float, Culture) with
        #endif
        | true, v -> float v
        | false, _ -> raiseInvalidArg "value" "Expected an int" v.Value
    | _ -> raiseInvalidArg "value" "Expected an YAMLElement.Value" value

let char (value: YAMLElement) : char =
    match value with
    | YAMLElement.Value v | YAMLElement.Object [YAMLElement.Value v] -> 
        match isString v.Value with
        | true -> v.Value.[0]
        | _ -> Helper.raiseInvalidArg "value" "Expected a char" v.Value
    | anyElse -> Helper.raiseInvalidArg "value" "Expected a char" anyElse

let bool (value: YAMLElement) : bool =
    match value with
    | YAMLElement.Value v | YAMLElement.Object [YAMLElement.Value v] -> 
        match System.Boolean.TryParse v.Value with
        | true, bool -> bool
        | false, _ -> Helper.raiseInvalidArg "value" "Expected a bool" v.Value
    | anyElse -> Helper.raiseInvalidArg "value" "Expected a bool" anyElse

let map (keyDecoder: string -> 'a) (valueDecoder: YAMLElement -> 'b) (value: YAMLElement) : Map<'a, 'b> =
    match value with
    | YAMLElement.Object v -> 
        v |> List.map (fun x -> 
            match x with
            | YAMLElement.Mapping (k, v) -> (keyDecoder k.Value, valueDecoder v)
            | anyElse -> Helper.raiseInvalidArg "value" "Expected a mapping" x
        ) |> Map.ofList
    | anyElse -> Helper.raiseInvalidArg "value" "Expected a map" anyElse

let dict (keyDecoder: string -> 'a) (valueDecoder: YAMLElement -> 'b) (value: YAMLElement) : Dictionary<'a, 'b> =
    match value with
    | YAMLElement.Object v -> 
        v |> List.map (fun x -> 
            match x with
            | YAMLElement.Mapping (k, v) -> (keyDecoder k.Value, valueDecoder v)
            | anyElse -> Helper.raiseInvalidArg "value" "Expected a mapping" anyElse
        ) |> Map |> Dictionary
    | anyElse -> Helper.raiseInvalidArg "value" "Expected a dictionary" anyElse

let datetime (value: YAMLElement) : DateTime =
    match value with
    | YAMLElement.Value v | YAMLElement.Object [YAMLElement.Value v] -> 
        match System.DateTime.TryParse(v.Value) with
        | true, dateTime -> dateTime
        | false, _ -> Helper.raiseInvalidArg "value" "Expected a DateTime" v.Value
    | anyElse -> Helper.raiseInvalidArg "value" "Expected a DateTime" anyElse

let datetimeOffset (value: YAMLElement) : DateTimeOffset =
    match value with
    | YAMLElement.Value v | YAMLElement.Object [YAMLElement.Value v] -> 
        match System.DateTimeOffset.TryParse(v.Value) with
        | true, dateTimeOffset -> dateTimeOffset
        | false, _ -> Helper.raiseInvalidArg "value" "Expected a DateTimeOffset" v.Value
    | anyElse -> Helper.raiseInvalidArg "value" "Expected a DateTimeOffset" anyElse

let option (decoder: YAMLElement -> 'a) (value: YAMLElement) : 'a option =
    match value with
    | YAMLElement.Value v | YAMLElement.Object [YAMLElement.Value v] -> 
        match v.Value with
        | YAML_NULL -> None
        | _ -> Some (decoder value)
    | anyElse -> Helper.raiseInvalidArg "value" "Expected an option" anyElse

let string (value: YAMLElement) : string =
    match value with
    | YAMLElement.Value v | YAMLElement.Object [YAMLElement.Value v] -> v.Value
    | anyElse -> Helper.raiseInvalidArg "value" "Expected a string" anyElse

let tuple2 (decoderA: YAMLElement -> 'a) (decoderB: YAMLElement -> 'b) (value: YAMLElement) : 'a * 'b =
    match value with
    | YAMLElement.Object [YAMLElement.Sequence [a; b]] | YAMLElement.Sequence [a; b] -> (decoderA a, decoderB b)
    | anyElse -> Helper.raiseInvalidArg "value" "Expected a tuple2" anyElse

let tuple3 (decoderA: YAMLElement -> 'a) (decoderB: YAMLElement -> 'b) (decoderC: YAMLElement -> 'c) (value: YAMLElement) : 'a * 'b * 'c =
    match value with
    | YAMLElement.Object [YAMLElement.Sequence [a; b; c]] | YAMLElement.Sequence [a; b; c] -> (decoderA a, decoderB b, decoderC c)
    | anyElse -> Helper.raiseInvalidArg "value" "Expected a tuple3" anyElse

let tuple4 
    (decoderA: YAMLElement -> 'a) 
    (decoderB: YAMLElement -> 'b) 
    (decoderC: YAMLElement -> 'c) 
    (decoderD: YAMLElement -> 'd) 
    (value: YAMLElement) 
    : 'a * 'b * 'c * 'd 
    =
    match value with
    | YAMLElement.Object [YAMLElement.Sequence [a; b; c; d]] | YAMLElement.Sequence [a; b; c; d] -> (decoderA a, decoderB b, decoderC c, decoderD d)
    | anyElse -> Helper.raiseInvalidArg "value" "Expected a tuple4" anyElse

let tuple5 
    (decoderA: YAMLElement -> 'a) 
    (decoderB: YAMLElement -> 'b) 
    (decoderC: YAMLElement -> 'c) 
    (decoderD: YAMLElement -> 'd) 
    (decoderE: YAMLElement -> 'e) 
    (value: YAMLElement) 
    : 'a * 'b * 'c * 'd * 'e 
    =
    match value with
    | YAMLElement.Object [YAMLElement.Sequence [a; b; c; d; e]] | YAMLElement.Sequence [a; b; c; d; e] -> (decoderA a, decoderB b, decoderC c, decoderD d, decoderE e)
    | anyElse -> Helper.raiseInvalidArg "value" "Expected a tuple5" anyElse

let tuple6
    (decoderA: YAMLElement -> 'a) 
    (decoderB: YAMLElement -> 'b) 
    (decoderC: YAMLElement -> 'c) 
    (decoderD: YAMLElement -> 'd) 
    (decoderE: YAMLElement -> 'e) 
    (decoderF: YAMLElement -> 'f) 
    (value: YAMLElement) 
    : 'a * 'b * 'c * 'd * 'e * 'f 
    =
    match value with
    | YAMLElement.Object [YAMLElement.Sequence [a; b; c; d; e; f]] 
    | YAMLElement.Sequence [a; b; c; d; e; f] -> 
        (decoderA a, decoderB b, decoderC c, decoderD d, decoderE e, decoderF f)
    | anyElse -> Helper.raiseInvalidArg "value" "Expected a tuple6" anyElse

let tuple7
    (decoderA: YAMLElement -> 'a) 
    (decoderB: YAMLElement -> 'b) 
    (decoderC: YAMLElement -> 'c) 
    (decoderD: YAMLElement -> 'd) 
    (decoderE: YAMLElement -> 'e) 
    (decoderF: YAMLElement -> 'f) 
    (decoderG: YAMLElement -> 'g) 
    (value: YAMLElement) 
    : 'a * 'b * 'c * 'd * 'e * 'f * 'g 
    =
    match value with
    | YAMLElement.Object [YAMLElement.Sequence [a; b; c; d; e; f; g]] 
    | YAMLElement.Sequence [a; b; c; d; e; f; g] -> 
        (decoderA a, decoderB b, decoderC c, decoderD d, decoderE e, decoderF f, decoderG g)
    | anyElse -> Helper.raiseInvalidArg "value" "Expected a tuple7" anyElse

let tuple8
    (decoderA: YAMLElement -> 'a) 
    (decoderB: YAMLElement -> 'b) 
    (decoderC: YAMLElement -> 'c) 
    (decoderD: YAMLElement -> 'd) 
    (decoderE: YAMLElement -> 'e) 
    (decoderF: YAMLElement -> 'f) 
    (decoderG: YAMLElement -> 'g) 
    (decoderH: YAMLElement -> 'h) 
    (value: YAMLElement) 
    : 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h 
    =
    match value with
    | YAMLElement.Object [YAMLElement.Sequence [a; b; c; d; e; f; g; h]] 
    | YAMLElement.Sequence [a; b; c; d; e; f; g; h] -> 
        (decoderA a, decoderB b, decoderC c, decoderD d, decoderE e, decoderF f, decoderG g, decoderH h)
    | anyElse -> Helper.raiseInvalidArg "value" "Expected a tuple8" anyElse

let list (decoder: YAMLElement -> 'a) (value: YAMLElement) : 'a list =
    match value with
    | YAMLElement.Sequence v | YAMLElement.Object [YAMLElement.Sequence v] -> 
        v |> List.map decoder
    | anyElse -> Helper.raiseInvalidArg "value" "Expected a list" anyElse

let array (decoder: YAMLElement -> 'a) (value: YAMLElement) : 'a [] =
    match value with
    | YAMLElement.Sequence v | YAMLElement.Object [YAMLElement.Sequence v] -> 
        v |> List.map decoder |> List.toArray
    | anyElse -> Helper.raiseInvalidArg "value" "Expected an array" anyElse

let seq (decoder: YAMLElement -> 'a) (value: YAMLElement) : seq<'a> =
    match value with
    | YAMLElement.Sequence v | YAMLElement.Object [YAMLElement.Sequence v] -> 
        v |> Seq.map decoder
    | anyElse -> Helper.raiseInvalidArg "value" "Expected a seq" anyElse

let resizearray (decoder: YAMLElement -> 'a) (value: YAMLElement) : ResizeArray<'a> =
    match value with
    | YAMLElement.Sequence v | YAMLElement.Object [YAMLElement.Sequence v] -> 
        v |> List.map decoder |> ResizeArray
    | anyElse -> Helper.raiseInvalidArg "value" "Expected a resizearray" anyElse

let values (decoder: YAMLElement -> 'a) (value: YAMLElement) : 'a list =
    match value with
    | YAMLElement.Sequence v | YAMLElement.Object [YAMLElement.Sequence v] | YAMLElement.Object v -> 
        v 
        |> List.map (function 
            | YAMLElement.Value _ as v -> decoder v 
            | anyElse -> Helper.raiseInvalidArg "value" "Expected a values" anyElse
        )
    | anyElse -> Helper.raiseInvalidArg "value" "Expected a values" anyElse

module ObjectHelper =

    type IRequiredGetter =
        abstract Field: string -> (YAMLElement -> 'a) -> 'a
        //abstract At: List<string> -> Decoder<'a> -> 'a
        //abstract Raw: Decoder<'a> -> 'a

    type IOptionalGetter =
        abstract Field: string -> (YAMLElement -> 'a) -> 'a option
        //abstract At: List<string> -> Decoder<'a> -> 'a option
        //abstract Raw: Decoder<'a> -> 'a option

    type IMultipleOptionalGetter =
        abstract FieldList: string list -> Dictionary<string, YAMLElement>

    type IOverflowGetter =
        abstract FieldList: string list -> Dictionary<string, YAMLElement>


open ObjectHelper

type IGetters =
    abstract Required:         IRequiredGetter
    abstract Optional:         IOptionalGetter
    abstract MultipleOptional: IMultipleOptionalGetter
    abstract Overflow:         IOverflowGetter

type Getter(ele: YAMLElement) =
    let required = 
        { new IRequiredGetter with 
            member _.Field fieldName dec =  
                match ele with
                | YAMLElement.Object v ->
                    //printfn "[GETTER] IsObject"
                    v 
                    |> List.tryFind (function 
                        | YAMLElement.Mapping (k, _) -> 
                            //printfn "[GETTER] IsMapping"
                            let equals = k.Value = fieldName
                            //printfn "[GETTER] Equals: %A" equals
                            equals
                        | _ -> false
                    )
                    |> Option.map (function 
                        | YAMLElement.Mapping (_, v) -> 
                            //printfn "[GETTER] Mapping: %A" v
                            dec v
                        | _ -> Helper.raiseInvalidArg "value" "Expected a mapping" ele
                    )
                    |> fun x ->
                        if x .IsNone then 
                            Helper.raiseInvalidArg "value" (sprintf "Field not found: %s" fieldName) ele
                        else x.Value
                | anyElse -> Helper.raiseInvalidArg "value" "Expected an object" anyElse
        }
    let optional =
        { new IOptionalGetter with 
            member this.Field (fieldName: string) (dec: YAMLElement -> 'a) = 
                match ele with
                | YAMLElement.Object v ->
                    v 
                    |> List.tryFind (function 
                        | YAMLElement.Mapping (k, _) -> k.Value = fieldName
                        | _ -> false
                    )
                    |> Option.map (function 
                        | YAMLElement.Mapping (_, v) -> dec v
                        | _ -> Helper.raiseInvalidArg "value" "Expected a mapping" ele
                    )
                | anyElse -> Helper.raiseInvalidArg "value" "Expected an object" anyElse
        }
    let multipleOptional =
        { new IMultipleOptionalGetter with 
            member this.FieldList (fieldNames: string list) = 
                match ele with
                | YAMLElement.Object v ->
                    let overflow =
                        v 
                        |> List.filter (function 
                            | YAMLElement.Mapping (k, _) -> fieldNames |> List.exists (fun x -> x = k.Value)
                            | _ -> false
                        )
                        |> List.map (function 
                            | YAMLElement.Mapping (k, v) -> (k.Value, v)
                            | _ -> Helper.raiseInvalidArg "value" "Expected a mapping" ele
                        )
                    let dict = new Dictionary<string, YAMLElement>()
                    overflow |> List.iter (fun (k, v) -> dict.Add(k, v))
                    dict
                | anyElse -> Helper.raiseInvalidArg "value" "Expected an object" anyElse
        }
    let overflow =
        { new IOverflowGetter with 
            member this.FieldList (fieldNames: string list) = 
                match ele with
                | YAMLElement.Object v ->
                    let overflow =
                        v 
                        |> List.filter (function 
                            | YAMLElement.Mapping (k, _) -> fieldNames |> List.exists (fun x -> x = k.Value) |> not
                            | _ -> false
                        )
                        |> List.map (function 
                            | YAMLElement.Mapping (k, v) -> (k.Value, v)
                            | _ -> Helper.raiseInvalidArg "value" "Expected a mapping" ele
                        )
                    let dict = new Dictionary<string, YAMLElement>()
                    overflow |> List.iter (fun (k, v) -> dict.Add(k, v))
                    dict
                | anyElse -> Helper.raiseInvalidArg "value" "Expected an object" anyElse
        }
    interface IGetters with
        member __.Required = required
        member __.Optional = optional
        member __.MultipleOptional     = multipleOptional
        member __.Overflow = overflow

let object (getter: IGetters -> 'a) (value: YAMLElement) : 'a =
    let getterObj = Getter(value) :> IGetters
    getter getterObj

let read (yaml: string) = 
    Reader.read yaml