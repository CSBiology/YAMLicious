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

    /// True when a reader of the core schema reads the scalar as a string. The tag decides before the style. The
    /// tag `!!str` and the non-specific tag `!` make a string of any text in any style, and every other explicit
    /// tag makes no string, a local or unknown tag included. Without a tag a quoted or block scalar is a string. A
    /// plain scalar without a tag is a string when `resolveKind` gives `Str` for its text, so an empty plain
    /// scalar is null and an integer of any size stays an `Integer`.
    let isString (tag: YamlTag option) (scalar: YamlScalar) =
        match tag with
        | Some YamlTag.NonSpecific -> true
        | Some explicit -> YamlTag.identifier explicit = YamlTag.coreNamespace + "str"
        | None ->
            match scalar.Style with
            | ScalarStyle.Plain ->
                // Every form of an int and a float starts with a sign, a point or a digit. Every form of null starts
                // with `~`, n or N, and every form of a bool with t, T, f or F. A text that starts with another
                // character is a string without a regular expression, which is the case of most keys and words.
                let text = scalar.Value
                if text.Length = 0 then false
                elif "+-.0123456789".IndexOf(text.[0]) >= 0 then resolveKind text = CoreKind.Str
                elif "~nN".IndexOf(text.[0]) >= 0 then not (resolvesToNull text)
                elif "tTfF".IndexOf(text.[0]) >= 0 then (tryParseBool None scalar).IsNone
                else true
            | _ -> true

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

    // The canonical forms below compare values for the identity of a mapping key. They work on decimal digits in
    // strings, so a number of any length gives the same text on every target and no native float takes part.

    /// The digits without their leading zeros, and "0" for digits that are all zero.
    let private withoutLeadingZeros (digits: string) : string =
        let mutable start = 0
        while start < digits.Length - 1 && digits.[start] = '0' do
            start <- start + 1
        digits.Substring start

    /// -1, 0 or 1 as the magnitude `a` is less than, equal to or greater than `b`. Both have no leading zeros.
    let private compareMagnitudes (a: string) (b: string) : int =
        if a.Length <> b.Length then
            compare a.Length b.Length
        else
            let mutable i = 0
            while i < a.Length && a.[i] = b.[i] do
                i <- i + 1
            if i = a.Length then 0 elif a.[i] < b.[i] then -1 else 1

    /// The text of digits that stand with the last digit first.
    let private fromLastDigit (digits: ResizeArray<int>) : string =
        let sb = System.Text.StringBuilder()
        for i in digits.Count - 1 .. -1 .. 0 do
            sb.Append(string digits.[i]) |> ignore
        withoutLeadingZeros (sb.ToString())

    let private addMagnitudes (a: string) (b: string) : string =
        let digits = ResizeArray<int>()
        let mutable i = a.Length - 1
        let mutable j = b.Length - 1
        let mutable carry = 0
        while i >= 0 || j >= 0 || carry > 0 do
            let sum = carry + (if i >= 0 then digitValue a.[i] else 0) + (if j >= 0 then digitValue b.[j] else 0)
            digits.Add(sum % 10)
            carry <- sum / 10
            i <- i - 1
            j <- j - 1
        fromLastDigit digits

    /// `larger` minus `smaller`, for magnitudes where `larger` is not less than `smaller`.
    let private subtractMagnitudes (larger: string) (smaller: string) : string =
        let digits = ResizeArray<int>()
        let mutable i = larger.Length - 1
        let mutable j = smaller.Length - 1
        let mutable borrow = 0
        while i >= 0 do
            let mutable difference = digitValue larger.[i] - borrow - (if j >= 0 then digitValue smaller.[j] else 0)
            if difference < 0 then
                difference <- difference + 10
                borrow <- 1
            else
                borrow <- 0
            digits.Add difference
            i <- i - 1
            j <- j - 1
        fromLastDigit digits

    /// The canonical decimal of a signed decimal plus a small int. Both and the result are in the form of
    /// `canonicalInteger`: a `-` in front of a negative number, no `+` and no leading zero.
    let private addToDecimal (canonical: string) (offset: int) : string =
        let negativeA = canonical.StartsWith("-")
        let magnitudeA = if negativeA then canonical.Substring 1 else canonical
        let negativeB = offset < 0
        let magnitudeB = string (if negativeB then -offset else offset)
        let negative, magnitude =
            if negativeA = negativeB then
                negativeA, addMagnitudes magnitudeA magnitudeB
            else
                match compareMagnitudes magnitudeA magnitudeB with
                | 0 -> false, "0"
                | 1 -> negativeA, subtractMagnitudes magnitudeA magnitudeB
                | _ -> negativeB, subtractMagnitudes magnitudeB magnitudeA
        if negative && magnitude <> "0" then "-" + magnitude else magnitude

    /// The integer of a core integer text as signed decimal digits: a `-` in front of a negative number, no `+`
    /// and no leading zero. Every zero is "0", and an octal or hexadecimal text converts exactly, so `1`, `+1`,
    /// `01`, `0o1` and `0x1` all give "1". The text has any length. None when `resolveKind` gives no `Integer`
    /// for the text.
    ///
    /// The conversion of an octal or hexadecimal text takes time that grows with the square of its length.
    let internal canonicalInteger (text: string) : string option =
        match integerRadix text with
        | 0 -> None
        | 10 ->
            let negative = text.[0] = '-'
            let start = if text.[0] = '-' || text.[0] = '+' then 1 else 0
            let magnitude = withoutLeadingZeros (text.Substring start)
            Some (if negative && magnitude <> "0" then "-" + magnitude else magnitude)
        | radix ->
            // Little-endian limbs of four decimal digits. A limb times 16 plus a carry below 16 stays below 2^18.
            let limbs = ResizeArray<int>()
            for index in 2 .. text.Length - 1 do
                let mutable carry = digitValue text.[index]
                for limb in 0 .. limbs.Count - 1 do
                    let product = limbs.[limb] * radix + carry
                    limbs.[limb] <- product % 10000
                    carry <- product / 10000
                while carry > 0 do
                    limbs.Add(carry % 10000)
                    carry <- carry / 10000
            if limbs.Count = 0 then
                Some "0"
            else
                let sb = System.Text.StringBuilder()
                sb.Append(string limbs.[limbs.Count - 1]) |> ignore
                for limb in limbs.Count - 2 .. -1 .. 0 do
                    let text = string limbs.[limb]
                    sb.Append(String.replicate (4 - text.Length) "0").Append(text) |> ignore
                Some (sb.ToString())

    /// The value of a core float text in one form per value. A finite number is its sign, its decimal digits
    /// without leading and trailing zeros, `e` and the exponent of the last digit, so `1.0`, `1.00`, `10e-1` and
    /// `0.1e1` all give "+1e0". Every zero, `-0.0` included, is "0". The infinities are "+inf" and "-inf", and
    /// every spelling of not-a-number is "nan". A decimal int such as `1` is a float text too and gives "+1e0". The
    /// form is exact for any count of digits and any exponent, so two texts give the same form exactly when they
    /// name the same decimal number, even when both round to the same float. None when the text is no float of
    /// the core schema, which includes the octal and hexadecimal ints.
    let internal canonicalFloat (text: string) : string option =
        if matches infinityPattern text then
            Some (if text.[0] = '-' then "-inf" else "+inf")
        elif matches nanPattern text then
            Some "nan"
        elif matches numberPattern text then
            let negative = text.[0] = '-'
            let start = if text.[0] = '-' || text.[0] = '+' then 1 else 0
            let mutable exponentAt = start
            while exponentAt < text.Length && text.[exponentAt] <> 'e' && text.[exponentAt] <> 'E' do
                exponentAt <- exponentAt + 1
            let mantissa = text.Substring(start, exponentAt - start)
            let point = mantissa.IndexOf('.')
            let integerDigits = if point < 0 then mantissa else mantissa.Substring(0, point)
            let fractionDigits = if point < 0 then "" else mantissa.Substring(point + 1)
            let digits = integerDigits + fractionDigits
            let mutable first = 0
            while first < digits.Length && digits.[first] = '0' do
                first <- first + 1
            if first = digits.Length then
                Some "0"
            else
                let mutable last = digits.Length
                while digits.[last - 1] = '0' do
                    last <- last - 1
                let exponent =
                    if exponentAt = text.Length then "0"
                    else (canonicalInteger (text.Substring(exponentAt + 1))).Value
                // The value is the digits times ten to the exponent minus the count of fraction digits. Each
                // trailing zero that goes adds one to that power.
                let shift = (digits.Length - last) - fractionDigits.Length
                Some ((if negative then "-" else "+") + digits.Substring(first, last - first) + "e" + addToDecimal exponent shift)
        else
            None

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
