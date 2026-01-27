module YAMLicious.Escapes

open System
open System.Text

let unescapeDoubleQuoted (s: string) : string =
    let sb = new StringBuilder()
    let rec loop i =
        if i >= s.Length then ()
        elif s.[i] = '\\' && i + 1 < s.Length then
            match s.[i + 1] with
            | '0' -> sb.Append('\u0000') |> ignore; loop (i + 2)  // null
            | 'a' -> sb.Append('\u0007') |> ignore; loop (i + 2)  // bell
            | 'b' -> sb.Append('\u0008') |> ignore; loop (i + 2)  // backspace
            | 't' -> sb.Append('\t') |> ignore; loop (i + 2)  // tab
            | 'n' -> sb.Append('\n') |> ignore; loop (i + 2)  // line feed
            | 'v' -> sb.Append('\u000B') |> ignore; loop (i + 2)  // vertical tab
            | 'f' -> sb.Append('\u000C') |> ignore; loop (i + 2)  // form feed
            | 'r' -> sb.Append('\r') |> ignore; loop (i + 2)  // carriage return
            | 'e' -> sb.Append('\u001B') |> ignore; loop (i + 2)  // escape
            | ' ' -> sb.Append(' ') |> ignore; loop (i + 2)  // space
            | '"' -> sb.Append('"') |> ignore; loop (i + 2)  // double quote
            | '/' -> sb.Append('/') |> ignore; loop (i + 2)  // slash (JSON compat)
            | '\\' -> sb.Append('\\') |> ignore; loop (i + 2)  // backslash
            | 'N' -> sb.Append('\u0085') |> ignore; loop (i + 2)  // next line
            | '_' -> sb.Append('\u00A0') |> ignore; loop (i + 2)  // non-breaking space
            | 'L' -> sb.Append('\u2028') |> ignore; loop (i + 2)  // line separator
            | 'P' -> sb.Append('\u2029') |> ignore; loop (i + 2)  // paragraph separator
            | 'x' when i + 3 < s.Length ->  // 8-bit Unicode
                let hex = s.Substring(i + 2, 2)
                try
                    sb.Append(char (Convert.ToInt32(hex, 16))) |> ignore
                    loop (i + 4)
                with _ -> 
                    sb.Append('\\').Append('x').Append(hex) |> ignore
                    loop (i + 4)
            | 'u' when i + 5 < s.Length ->  // 16-bit Unicode
                let hex = s.Substring(i + 2, 4)
                try
                    sb.Append(char (Convert.ToInt32(hex, 16))) |> ignore
                    loop (i + 6)
                with _ -> 
                    sb.Append('\\').Append('u').Append(hex) |> ignore
                    loop (i + 6)
            | 'U' when i + 9 < s.Length ->  // 32-bit Unicode
                let hex = s.Substring(i + 2, 8)
                try
                    let codePoint = Convert.ToInt32(hex, 16)
                    #if FABLE_COMPILER
                    // For Fable, use a simple approach for high Unicode points
                    if codePoint >= 0x10000 then
                        // Surrogate pair encoding for characters outside BMP
                        let cp = codePoint - 0x10000
                        let high = 0xD800 + (cp / 0x400)
                        let low = 0xDC00 + (cp % 0x400)
                        sb.Append(char high).Append(char low) |> ignore
                    else
                        sb.Append(char codePoint) |> ignore
                    #else
                    sb.Append(Char.ConvertFromUtf32(codePoint)) |> ignore
                    #endif
                    loop (i + 10)
                with _ -> 
                    sb.Append('\\').Append('U').Append(hex) |> ignore
                    loop (i + 10)
            | '\n' -> loop (i + 2)  // escaped line break (continuation) - skip it
            | c -> 
                // Unknown escape - keep the backslash and character
                sb.Append('\\').Append(c) |> ignore
                loop (i + 2)
        else
            sb.Append(s.[i]) |> ignore
            loop (i + 1)
    loop 0
    sb.ToString()
