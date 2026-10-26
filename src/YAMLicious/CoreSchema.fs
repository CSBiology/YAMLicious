namespace YAMLicious

open System
open System.Globalization

/// The kind that the YAML 1.2.2 core schema (section 10.3.2) gives to the text of a plain scalar. `Integer`
/// has no size limit, so it covers an integer that no int64 and no float holds exactly.
[<RequireQualifiedAccess>]
type CoreKind =
    | Null
    | Bool
    | Integer
    | Float
    | Str

/// Why `CoreSchema.integerToFloat` gives no float.
[<RequireQualifiedAccess>]
type IntegerToFloatError =
    /// The text is no integer of the core schema.
    | NotAnInteger
    /// The integer rounds to a magnitude of 2^1024 or more, which no finite float holds.
    | BeyondFloatRange

/// The YAML 1.2.2 core schema, which decides what a plain scalar means.
///
/// The functions that read a scalar take the explicit tag of its node next to the scalar, because the tag
/// is a property of the node and not of the scalar. Pass `node.Properties.Tag`.
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
    let private boolPattern = anchored boolAlternative
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

    /// The radix of the digits of a core integer text (10, 8 or 16), or 0 when the text is no core integer.
    let private integerRadix (text: string) : int =
        if matches decimalPattern text then 10
        elif matches octalPattern text then 8
        elif matches hexadecimalPattern text then 16
        else 0

    let private digitValue (c: char) =
        if c <= '9' then int c - int '0'
        elif c >= 'a' then int c - int 'a' + 10
        else int c - int 'A' + 10

    /// True when a reader of the core schema reads the text, written as a plain scalar, as null. The empty
    /// string counts, because an empty plain scalar is null.
    let resolvesToNull (text: string) =
        String.IsNullOrEmpty text || matches nullPattern text

    /// True when a reader of the core schema reads the text, written as a plain scalar, as null, a bool, an
    /// int or a float. YAML 1.2 does not resolve yes, no, on and off, so they are strings here although
    /// YAML 1.1 reads them as bools.
    let resolvesToNonString (text: string) =
        String.IsNullOrEmpty text || matches nonStringPattern text

    /// The kind that the core schema gives the text when it is written as a plain scalar. The text decides
    /// alone, and its size does not matter. An integer of a thousand digits is an `Integer`, although
    /// `tryParseInt64` returns None for it as it does for a text that is no integer. An integer wins over a
    /// float, so `5` is an `Integer` and `5.0` is a `Float`. A `Float` also covers `.5`, `.inf`, `-.inf` and
    /// `.nan`. An integer has decimal digits with an optional sign, or an `0o` or `0x` prefix with octal or
    /// hexadecimal digits, and the prefixed forms take no sign. Texts that only YAML 1.1 resolves, such as
    /// `1_000`, `0b101`, `-0x10`, `1:20` and `yes`, are a `Str`.
    ///
    /// The function takes the text of a scalar that the caller already knows to be plain and untagged.
    /// A quoted or block scalar is a string whatever its text, and an explicit tag fixes the type, so the
    /// caller handles both before it asks. The readers that take the tag and the scalar, such as
    /// `tryParseBool`, apply those rules themselves. `resolveKind text <> CoreKind.Str` equals
    /// `resolvesToNonString text`.
    let resolveKind (text: string) : CoreKind =
        if resolvesToNull text then CoreKind.Null
        elif matches boolPattern text then CoreKind.Bool
        elif integerRadix text <> 0 then CoreKind.Integer
        elif matches numberPattern text || matches infinityPattern text || matches nanPattern text then CoreKind.Float
        else CoreKind.Str

    /// True when the scalar can have the type that the core tag `!!suffix` names. A plain scalar without a
    /// tag has the type that its text says. A quoted or block scalar is a string. An explicit tag fixes the
    /// type, whatever the style. The non-specific tag `!` makes a string, so it names no core type.
    /// The tag is compared by its full identifier, because a `%TAG` directive can split the same identifier
    /// into other prefixes and suffixes.
    let private resolvesAs (suffix: string) (tag: YamlTag option) (scalar: YamlScalar) =
        match tag with
        | Some explicit -> YamlTag.identifier explicit = YamlTag.coreNamespace + suffix
        | None ->
            match scalar.Style with
            | ScalarStyle.Plain -> true
            | _ -> false

    /// True when a reader of the core schema reads the scalar as null. That is a plain scalar with the text
    /// null, Null, NULL, ~ or nothing, or a scalar under the tag !!null with such a text. A scalar under
    /// another tag, such as `!!str null`, is not null.
    let isNull (tag: YamlTag option) (scalar: YamlScalar) =
        resolvesAs "null" tag scalar && resolvesToNull scalar.Value

    /// The bool that a reader of the core schema takes from the scalar, or None when the scalar is not a
    /// bool. Only true, True, TRUE, false, False and FALSE count. YAML 1.2 reads yes, no, on and off as strings.
    let tryParseBool (tag: YamlTag option) (scalar: YamlScalar) : bool option =
        if resolvesAs "bool" tag scalar then
            match scalar.Value with
            | "true" | "True" | "TRUE" -> Some true
            | "false" | "False" | "FALSE" -> Some false
            | _ -> None
        else
            None

    /// The int64 that a reader of the core schema takes from the scalar, or None when the scalar is not an int
    /// or does not fit in 64 bits. The core schema sets no size limit for an int. A decimal int has an
    /// optional sign. An octal int starts with 0o and a hexadecimal int with 0x.
    ///
    /// None says nothing about the reason. `resolveKind` tells a text that is no int from an int beyond 64
    /// bits, and `integerToFloat` converts an int of any size.
    let tryParseInt64 (tag: YamlTag option) (scalar: YamlScalar) : int64 option =
        let text = scalar.Value

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

        if not (resolvesAs "int" tag scalar) then
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
    let tryParseInt (tag: YamlTag option) (scalar: YamlScalar) : int option =
        tryParseInt64 tag scalar
        |> Option.filter (fun number -> number >= int64 Int32.MinValue && number <= int64 Int32.MaxValue)
        |> Option.map int

    /// The float that a reader of the core schema takes from the scalar, or None when the scalar is not a
    /// decimal number, an infinity or not-a-number. A text that the number parse of the target accepts but
    /// the core schema does not, such as NaN or 0x1F, gives None.
    ///
    /// A decimal int such as 7 gives 7.0, and the number parse of the target does the rounding. A decimal int
    /// beyond the float range gives an infinity, and `-0` gives -0.0. An int with a radix prefix, such as 0o17
    /// or 0x1F, gives None, because the float forms of the core schema have no prefix. `integerToFloat`
    /// converts an int of any radix and size with one rounding rule on every target. It reports an int beyond
    /// the float range and gives 0.0 for `-0`.
    let tryParseFloat (tag: YamlTag option) (scalar: YamlScalar) : float option =
        let text = scalar.Value
        if not (resolvesAs "float" tag scalar) then
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

    /// The float nearest to the integer that the text names, for a text that `resolveKind` reads as an
    /// `Integer`, whatever its radix and size. The conversion is exact, so a value halfway between two floats
    /// goes to the one with the even last bit (round to nearest even, as IEEE 754 does), and every target
    /// gives the same float. `-0` is the integer zero and gives 0.0 with a plus sign.
    ///
    /// A text that is no core integer is `NotAnInteger`. An integer from 2^1024 - 2^970 up in magnitude is
    /// `BeyondFloatRange`, because it rounds to 2^1024 or more. The function returns no infinity for it, since
    /// the text names a finite number. A caller that wants a range error raises it with the span of its node,
    /// which this function does not know.
    ///
    /// The text is a plain scalar with the caller's checks of style and tag already done, as for `resolveKind`.
    let integerToFloat (text: string) : Result<float, IntegerToFloatError> =
        let radix = integerRadix text
        if radix = 0 then
            Error IntegerToFloatError.NotAnInteger
        else
            // The integer is a little-endian array of 16-bit limbs held in int. A step multiplies each limb by
            // at most 16 and adds a carry below 16, so every product stays below 2^20 in the 32-bit int of
            // each target. 65 limbs hold 1040 bits. An integer that needs more is at least 2^1040, so the
            // loop stops at the first digit that needs more, and a long text of nonzero digits stops after a few
            // hundred of them.
            let maxLimbs = 65
            let limbs: int[] = Array.zeroCreate maxLimbs
            let mutable used = 0
            let mutable fits = true
            let mutable index = if radix = 10 then (if text.[0] = '-' || text.[0] = '+' then 1 else 0) else 2
            while fits && index < text.Length do
                let mutable carry = digitValue text.[index]
                for limb in 0 .. used - 1 do
                    let product = limbs.[limb] * radix + carry
                    limbs.[limb] <- product &&& 0xFFFF
                    carry <- product >>> 16
                if carry > 0 then
                    if used = maxLimbs then
                        fits <- false
                    else
                        limbs.[used] <- carry
                        used <- used + 1
                index <- index + 1

            if not fits then
                Error IntegerToFloatError.BeyondFloatRange
            else
                let bitAt (position: int) = (limbs.[position >>> 4] >>> (position &&& 15)) &&& 1
                let mutable topLimbBits = 0
                if used > 0 then
                    let mutable top = limbs.[used - 1]
                    while top > 0 do
                        topLimbBits <- topLimbBits + 1
                        top <- top >>> 1
                let bitLength = if used = 0 then 0 else (used - 1) * 16 + topLimbBits

                // The 53 leading bits are the significand. Up to 2^53 the float holds every integer, so the
                // loop adds up bits in a float exactly.
                let kept = if bitLength < 53 then bitLength else 53
                let dropped = bitLength - kept
                let mutable significand = 0.0
                for position = bitLength - 1 downto dropped do
                    significand <- significand * 2.0 + float (bitAt position)

                // Round to nearest even on the dropped bits: up when the first dropped bit is set and either a
                // later dropped bit is set or the last kept bit is odd.
                if dropped > 0 && bitAt (dropped - 1) = 1 then
                    let mutable laterBitSet = false
                    let mutable position = dropped - 2
                    while not laterBitSet && position >= 0 do
                        laterBitSet <- bitAt position = 1
                        position <- position - 1
                    if laterBitSet || bitAt dropped = 1 then
                        significand <- significand + 1.0

                // 2^53 - 1 shifted by 971 is the largest finite float. A rounded significand of 2^53 at that
                // shift, or any shift beyond it, is 2^1024 or more.
                if dropped > 971 || (dropped = 971 && significand = 9007199254740992.0) then
                    Error IntegerToFloatError.BeyondFloatRange
                else
                    // Doubling is exact, so the loop applies the power of two without a call to Math.Pow.
                    let mutable magnitude = significand
                    for _ in 1 .. dropped do
                        magnitude <- magnitude * 2.0
                    Ok (if radix = 10 && text.[0] = '-' && magnitude > 0.0 then -magnitude else magnitude)

    /// The text of a finite float. A whole number such as 1 gets ".0", because a reader of the core schema
    /// reads the text 1 as an int and 1.0 as a float. Every other text of a float already has a point or
    /// an exponent.
    let floatText (text: string) =
        if matches decimalPattern text then text + ".0" else text

    /// The scalar of a string. Text that a core schema reader takes for null, a bool, an int or a float is
    /// double-quoted, so that it reads back as the string. Every other text is plain. A constructed scalar has
    /// no span, so its plain style is a preference (see `ScalarStyle`), and the writer quotes a text that
    /// cannot stand plain, for example one that starts with an indicator or holds `: ` or ` #`.
    let stringContent (text: string) : YamlScalar =
        let style = if resolvesToNonString text then ScalarStyle.DoubleQuoted else ScalarStyle.Plain
        { Value = text
          Style = style
          HeaderComment = None
          Layout = None }
