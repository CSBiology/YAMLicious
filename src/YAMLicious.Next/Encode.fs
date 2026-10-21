namespace YAMLicious

open System
open System.Collections.Generic
open System.Globalization

/// Turns a value of type 'T into a node.
type Encoder<'T> = 'T -> YamlNode

module internal EncodeSupport =

#if FABLE_COMPILER_PYTHON
    /// The Python repr of a float is the shortest text that reads back as the same number. It switches to an
    /// exponent for large and small magnitudes (1e+21, 5e-324), where the Fable ToString writes every digit.
    /// The Fable float64 has its own repr, so the number becomes a Python float first.
    [<Fable.Core.Emit("repr(float($0))")>]
    let private pyRepr (value: float) : string = Fable.Core.Util.nativeOnly
#endif

    /// The core schema spells not-a-number as .nan and the infinities as .inf and -.inf, where ToString gives
    /// NaN and Infinity. The JavaScript ToString writes negative zero as 0, so negative zero gets its own
    /// spelling. A finite number is written in the shortest text that reads back as the same number. A whole
    /// number gets ".0", because the core schema reads the text 1 as an int and 1.0 as a float. The exponent
    /// form depends on the target: 1E+21 on .NET and 1e+21 on JavaScript and Python.
    let floatText (value: float) : string =
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

    let intText (value: int) : string = string value

    let int64Text (value: int64) : string = string value

/// The value encoders. They build scalars and block collections with the constructors of `YamlNode`, and an
/// empty collection becomes the flow collection `[]` or `{}`. A tree that needs comments, properties or other
/// styles takes them from the constructors of `YamlNode`.
[<RequireQualifiedAccess>]
module Encode =

    /// A plain scalar with the text `null`.
    let nullValue: YamlNode = YamlNode.plain "null"

    /// A plain scalar, so the number reads back as an int.
    let int (value: int) : YamlNode = YamlNode.plain (EncodeSupport.intText value)

    let int64 (value: int64) : YamlNode = YamlNode.plain (EncodeSupport.int64Text value)

    /// A plain scalar that reads back as a float in the core schema, also for `.nan`, `.inf`, `-.inf` and
    /// `-0.0`.
    let float (value: float) : YamlNode = YamlNode.plain (EncodeSupport.floatText value)

    /// The core schema spells the booleans true and false, where the .NET ToString gives True and False.
    let bool (value: bool) : YamlNode = YamlNode.plain (if value then "true" else "false")

    /// A string scalar. Text that a core schema reader takes for null, a bool, an int or a float is
    /// double-quoted, so that it reads back as the string. Every other text is plain, and the writer quotes
    /// it when it cannot stand plain.
    let string (value: string) : YamlNode =
        let content = CoreSchema.stringContent value
        YamlNode.scalar content.Style content.Value

    let char (value: char) : YamlNode = string (value.ToString())

    /// The round trip format of ISO 8601, as in `2018-10-01T11:12:55.0000000Z`. The digits of the fraction depend
    /// on the target, and `Decode.datetime` reads each form.
    let datetime (value: DateTime) : YamlNode = YamlNode.plain (value.ToString("O", CultureInfo.InvariantCulture))

    /// The round trip format of ISO 8601 with the offset, as in `2018-10-01T11:12:55.0000000+01:00`.
    let datetimeOffset (value: DateTimeOffset) : YamlNode =
        YamlNode.plain (value.ToString("O", CultureInfo.InvariantCulture))

    /// None is a plain `null`.
    let option (encoder: Encoder<'T>) : Encoder<'T option> =
        fun value ->
            match value with
            | Some inner -> encoder inner
            | None -> nullValue

    let seq (encoder: Encoder<'T>) : Encoder<seq<'T>> =
        fun items -> YamlNode.sequence [ for item in items -> YamlNode.item (encoder item) ]

    let list (encoder: Encoder<'T>) : Encoder<'T list> =
        fun items -> YamlNode.sequence (List.map (fun item -> YamlNode.item (encoder item)) items)

    let array (encoder: Encoder<'T>) : Encoder<'T[]> =
        fun items -> YamlNode.sequence [ for item in items -> YamlNode.item (encoder item) ]

    let resizearray (encoder: Encoder<'T>) : Encoder<ResizeArray<'T>> =
        fun items -> YamlNode.sequence [ for item in items -> YamlNode.item (encoder item) ]

    /// A block sequence of strings. Each item is `toText item` encoded by `string`, so a text such as `true` or `1`
    /// is double-quoted and `Decode.values Decode.string` reads the items back. `Decode.values Decode.int` rejects
    /// them. Use `list` with `int` for a sequence of ints.
    let values (toText: 'T -> string) : Encoder<seq<'T>> =
        fun items -> YamlNode.sequence [ for item in items -> YamlNode.item (string (toText item)) ]

    let tuple2 (encoderA: Encoder<'A>) (encoderB: Encoder<'B>) : Encoder<'A * 'B> =
        fun (a, b) -> YamlNode.sequence [ YamlNode.item (encoderA a); YamlNode.item (encoderB b) ]

    let tuple3 (encoderA: Encoder<'A>) (encoderB: Encoder<'B>) (encoderC: Encoder<'C>) : Encoder<'A * 'B * 'C> =
        fun (a, b, c) ->
            YamlNode.sequence [ YamlNode.item (encoderA a); YamlNode.item (encoderB b); YamlNode.item (encoderC c) ]

    let tuple4
        (encoderA: Encoder<'A>)
        (encoderB: Encoder<'B>)
        (encoderC: Encoder<'C>)
        (encoderD: Encoder<'D>)
        : Encoder<'A * 'B * 'C * 'D> =
        fun (a, b, c, d) ->
            YamlNode.sequence
                [ YamlNode.item (encoderA a)
                  YamlNode.item (encoderB b)
                  YamlNode.item (encoderC c)
                  YamlNode.item (encoderD d) ]

    let tuple5
        (encoderA: Encoder<'A>)
        (encoderB: Encoder<'B>)
        (encoderC: Encoder<'C>)
        (encoderD: Encoder<'D>)
        (encoderE: Encoder<'E>)
        : Encoder<'A * 'B * 'C * 'D * 'E> =
        fun (a, b, c, d, e) ->
            YamlNode.sequence
                [ YamlNode.item (encoderA a)
                  YamlNode.item (encoderB b)
                  YamlNode.item (encoderC c)
                  YamlNode.item (encoderD d)
                  YamlNode.item (encoderE e) ]

    let tuple6
        (encoderA: Encoder<'A>)
        (encoderB: Encoder<'B>)
        (encoderC: Encoder<'C>)
        (encoderD: Encoder<'D>)
        (encoderE: Encoder<'E>)
        (encoderF: Encoder<'F>)
        : Encoder<'A * 'B * 'C * 'D * 'E * 'F> =
        fun (a, b, c, d, e, f) ->
            YamlNode.sequence
                [ YamlNode.item (encoderA a)
                  YamlNode.item (encoderB b)
                  YamlNode.item (encoderC c)
                  YamlNode.item (encoderD d)
                  YamlNode.item (encoderE e)
                  YamlNode.item (encoderF f) ]

    let tuple7
        (encoderA: Encoder<'A>)
        (encoderB: Encoder<'B>)
        (encoderC: Encoder<'C>)
        (encoderD: Encoder<'D>)
        (encoderE: Encoder<'E>)
        (encoderF: Encoder<'F>)
        (encoderG: Encoder<'G>)
        : Encoder<'A * 'B * 'C * 'D * 'E * 'F * 'G> =
        fun (a, b, c, d, e, f, g) ->
            YamlNode.sequence
                [ YamlNode.item (encoderA a)
                  YamlNode.item (encoderB b)
                  YamlNode.item (encoderC c)
                  YamlNode.item (encoderD d)
                  YamlNode.item (encoderE e)
                  YamlNode.item (encoderF f)
                  YamlNode.item (encoderG g) ]

    let tuple8
        (encoderA: Encoder<'A>)
        (encoderB: Encoder<'B>)
        (encoderC: Encoder<'C>)
        (encoderD: Encoder<'D>)
        (encoderE: Encoder<'E>)
        (encoderF: Encoder<'F>)
        (encoderG: Encoder<'G>)
        (encoderH: Encoder<'H>)
        : Encoder<'A * 'B * 'C * 'D * 'E * 'F * 'G * 'H> =
        fun (a, b, c, d, e, f, g, h) ->
            YamlNode.sequence
                [ YamlNode.item (encoderA a)
                  YamlNode.item (encoderB b)
                  YamlNode.item (encoderC c)
                  YamlNode.item (encoderD d)
                  YamlNode.item (encoderE e)
                  YamlNode.item (encoderF f)
                  YamlNode.item (encoderG g)
                  YamlNode.item (encoderH h) ]

    /// A block mapping with the entries in the order of the map. The key encoder builds the key node.
    let map (keyEncoder: Encoder<'K>) (valueEncoder: Encoder<'V>) : Encoder<Map<'K, 'V>> =
        fun entries ->
            YamlNode.mapping [ for KeyValue (key, value) in entries -> YamlNode.entry (keyEncoder key) (valueEncoder value) ]

    /// A block mapping with the entries in the order of the dictionary. The key encoder builds the key node.
    let dict (keyEncoder: Encoder<'K>) (valueEncoder: Encoder<'V>) : Encoder<Dictionary<'K, 'V>> =
        fun entries ->
            YamlNode.mapping [ for KeyValue (key, value) in entries -> YamlNode.entry (keyEncoder key) (valueEncoder value) ]

    /// A block mapping with the fields in the given order. A field name goes through `string`, so a name such as
    /// `true` is double-quoted.
    let object (fields: seq<string * YamlNode>) : YamlNode =
        YamlNode.mapping [ for (name, value) in fields -> YamlNode.entry (string name) value ]

    /// The field for a value that may be missing, or None when it is. Combine the fields with `List.choose id`
    /// before they go to `object`.
    let optionalField (name: string) (encoder: Encoder<'T>) (value: 'T option) : (string * YamlNode) option =
        match value with
        | Some inner -> Some (name, encoder inner)
        | None -> None
