module YAMLicious.Escapes

open System
open System.Text

let private isBlank (c: char) = c = ' ' || c = '\t'

/// The offset of the first character at or behind `start` that is neither a space nor a tab.
let private skipBlanks (s: string) (start: int) =
    let mutable j = start
    while j < s.Length && isBlank s.[j] do
        j <- j + 1
    j

/// Reads the part of a fold that follows a line break. `start` is the offset behind the break. A line
/// that holds only blanks is an empty line, and the leading blanks of the line with content are dropped.
/// Returns the offset where that content starts (or the end of `s`) and the number of empty lines.
let private readFold (s: string) (start: int) =
    let mutable j = skipBlanks s start
    let mutable emptyLines = 0
    while j < s.Length && s.[j] = '\n' do
        emptyLines <- emptyLines + 1
        j <- skipBlanks s (j + 1)
    j, emptyLines

/// Decodes the interior of a double-quoted scalar (YAML 1.2.2, 7.3.1) in one pass. It resolves the
/// escapes and folds the lines (6.5). The scanner has already turned CRLF and CR into LF.
///
/// A line break folds together with the blanks around it. The blanks before the break and the leading
/// blanks of the next line go away, and the break becomes one space. When empty lines follow, each one
/// becomes a line feed and the break adds no space. An escaped line break adds nothing and keeps the
/// blanks before its backslash, and each empty line behind it becomes a line feed. A blank that an escape
/// produced is content, because only the literal run of blanks right in front of a line break is dropped.
let unescapeDoubleQuoted (s: string) : string =
    let sb = StringBuilder()
    let mutable i = 0
    while i < s.Length do
        let c = s.[i]
        if c = '\\' && i + 1 < s.Length then
            match s.[i + 1] with
            | '0' -> sb.Append('\u0000') |> ignore; i <- i + 2  // null
            | 'a' -> sb.Append('\u0007') |> ignore; i <- i + 2  // bell
            | 'b' -> sb.Append('\u0008') |> ignore; i <- i + 2  // backspace
            | 't' -> sb.Append('\t') |> ignore; i <- i + 2  // tab
            | 'n' -> sb.Append('\n') |> ignore; i <- i + 2  // line feed
            | 'v' -> sb.Append('\u000B') |> ignore; i <- i + 2  // vertical tab
            | 'f' -> sb.Append('\u000C') |> ignore; i <- i + 2  // form feed
            | 'r' -> sb.Append('\r') |> ignore; i <- i + 2  // carriage return
            | 'e' -> sb.Append('\u001B') |> ignore; i <- i + 2  // escape
            | ' ' -> sb.Append(' ') |> ignore; i <- i + 2  // space
            | '"' -> sb.Append('"') |> ignore; i <- i + 2  // double quote
            | '/' -> sb.Append('/') |> ignore; i <- i + 2  // slash (JSON compat)
            | '\\' -> sb.Append('\\') |> ignore; i <- i + 2  // backslash
            | 'N' -> sb.Append('\u0085') |> ignore; i <- i + 2  // next line
            | '_' -> sb.Append('\u00A0') |> ignore; i <- i + 2  // non-breaking space
            | 'L' -> sb.Append('\u2028') |> ignore; i <- i + 2  // line separator
            | 'P' -> sb.Append('\u2029') |> ignore; i <- i + 2  // paragraph separator
            | 'x' when i + 3 < s.Length ->  // 8-bit Unicode
                let hex = s.Substring(i + 2, 2)
                try
                    sb.Append(char (Convert.ToInt32(hex, 16))) |> ignore
                with _ ->
                    sb.Append('\\').Append('x').Append(hex) |> ignore
                i <- i + 4
            | 'u' when i + 5 < s.Length ->  // 16-bit Unicode
                let hex = s.Substring(i + 2, 4)
                try
                    sb.Append(char (Convert.ToInt32(hex, 16))) |> ignore
                with _ ->
                    sb.Append('\\').Append('u').Append(hex) |> ignore
                i <- i + 6
            | 'U' when i + 9 < s.Length ->  // 32-bit Unicode
                let hex = s.Substring(i + 2, 8)
                try
                    let codePoint = Convert.ToInt32(hex, 16)
                    #if FABLE_COMPILER_PYTHON
                    // Python's chr() supports full Unicode code points.
                    let utf32 = YAMLicious.PyInterop.fromCodePoint codePoint
                    sb.Append(utf32) |> ignore
                    #else
                    #if FABLE_COMPILER
                    // JS strings are UTF-16, so encode supplementary planes as surrogate pairs.
                    if codePoint >= 0x10000 then
                        let cp = codePoint - 0x10000
                        let high = 0xD800 + (cp / 0x400)
                        let low = 0xDC00 + (cp % 0x400)
                        sb.Append(char high).Append(char low) |> ignore
                    else
                        sb.Append(char codePoint) |> ignore
                    #else
                    sb.Append(Char.ConvertFromUtf32(codePoint)) |> ignore
                    #endif
                    #endif
                with _ ->
                    sb.Append('\\').Append('U').Append(hex) |> ignore
                i <- i + 10
            | '\n' ->
                let next, emptyLines = readFold s (i + 2)
                if emptyLines > 0 then
                    sb.Append(String.replicate emptyLines "\n") |> ignore
                i <- next
            | other ->
                // Unknown escape - keep the backslash and character
                sb.Append('\\').Append(other) |> ignore
                i <- i + 2
        elif c = '\n' then
            let next, emptyLines = readFold s (i + 1)
            sb.Append(if emptyLines = 0 then " " else String.replicate emptyLines "\n") |> ignore
            i <- next
        elif isBlank c then
            let next = skipBlanks s i
            // The blanks in front of a line break belong to the fold. Blanks anywhere else are content.
            if next >= s.Length || s.[next] <> '\n' then
                for k in i .. next - 1 do
                    sb.Append(s.[k]) |> ignore
            i <- next
        else
            sb.Append(c) |> ignore
            i <- i + 1
    sb.ToString()
