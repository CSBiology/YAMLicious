module internal YAMLicious.StringBuffer

/// Fable-compatible mutable string buffer using ResizeArray<char>.
/// Drop-in replacement for System.Text.StringBuilder that avoids
/// the set_Length operation which is not supported by Fable Python.
[<Sealed>]
type StringBuffer() =
    let chars = ResizeArray<char>()

    member _.Length = chars.Count

    member _.Item
        with get(i) = chars.[i]

    member this.Append(c: char) =
        chars.Add(c)
        this

    member this.Append(s: string) =
        for c in s do chars.Add(c)
        this

    member this.AppendLine(s: string) =
        for c in s do chars.Add(c)
        chars.Add('\n')
        this

    member _.TrimEndWhitespace() =
        while chars.Count > 0 && (chars.[chars.Count - 1] = '\t' || chars.[chars.Count - 1] = ' ') do
            chars.RemoveAt(chars.Count - 1)

    override _.ToString() =
        System.String(chars.ToArray())
