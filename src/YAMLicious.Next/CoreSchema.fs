namespace YAMLicious

open System
open System.Globalization

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
    let tryParseInt64 (tag: YamlTag option) (scalar: YamlScalar) : int64 option =
        let text = scalar.Value
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
          HeaderComment = None }
