module YAMLicious.Syntax

open System
open YAMLicious.StringBuffer
open YAMLicious.YAMLiciousTypes

module Line =

    let normalizeNewlines (s: string) =
        s.Replace("\r\n", "\n").Replace("\r", "\n")

    let countLeadingSpaces (line: string) =
        line |> Seq.takeWhile (fun c -> c = ' ') |> Seq.length

    let isBlank (line: string) =
        line.Trim() = ""

module Placeholder =

    let private tryParseInt (s: string) =
        match Int32.TryParse s with
        | true, value -> Some value
        | false, _ -> None

    let tryParseComment (line: string) =
        let t = line.Trim()
        if t.StartsWith("<c f=") && t.EndsWith("/>") then
            let value = t.Substring(5, t.Length - 7)
            if value |> Seq.forall Char.IsDigit then
                tryParseInt value
            else
                None
        else
            None

    let isCommentOnlyLine (line: string) =
        tryParseComment line |> Option.isSome

    let splitTrailingComment (s: string) =
        let trimmed = s.TrimEnd()
        let start = trimmed.LastIndexOf("<c f=")
        if start >= 0 && trimmed.EndsWith("/>") then
            let valueStart = start + 5
            let valueLength = trimmed.Length - valueStart - 2
            if valueLength >= 1 then
                let value = trimmed.Substring(valueStart, valueLength)
                if value |> Seq.forall Char.IsDigit then
                    trimmed.Substring(0, start).TrimEnd(), tryParseInt value
                else
                    trimmed, None
            else
                trimmed, None
        else
            trimmed, None

module Document =

    let isMarker (marker: string) (line: string) =
        let trimmed = line.TrimStart()
        if not (trimmed.StartsWith(marker)) then
            false
        elif trimmed.Length = marker.Length then
            true
        else
            Char.IsWhiteSpace(trimmed.[marker.Length])

    let isStart line = isMarker "---" line

    let isEnd line = isMarker "..." line

    let isTopLevelMarker (markerCheck: string -> bool) (line: string) =
        Line.countLeadingSpaces line = 0 && markerCheck line

    let tryInlineContentAfterStartMarker (line: string) =
        let trimmed = line.TrimStart()
        if not (trimmed.StartsWith("---")) then
            None
        else
            let rest = trimmed.Substring(3).TrimStart()
            if String.IsNullOrWhiteSpace(rest) || rest.StartsWith("#") then
                None
            else
                Some rest

    let isDirectivePreludeLine (line: string) =
        let t = line.TrimStart()
        t = "" || t.StartsWith("%") || t.StartsWith("#")

    let isDirectivePreludeOnly (lines: string list) =
        let hasDirective =
            lines |> List.exists (fun line -> line.TrimStart().StartsWith("%"))

        hasDirective && (lines |> List.forall isDirectivePreludeLine)

module BlockScalar =

    type Header =
        { Style: BlockScalarStyle
          Indent: int option
          Chomp: ChompingMode }

    let parseHeader (header: string) =
        let h = header.Trim()
        if String.IsNullOrWhiteSpace h then
            None
        else
            let style =
                match h.[0] with
                | '|' -> Some BlockScalarStyle.Literal
                | '>' -> Some BlockScalarStyle.Folded
                | _ -> None

            match style with
            | None -> None
            | Some style ->
                let mutable indent: int option = None
                let mutable chomp = ChompingMode.Clip
                let mutable valid = true

                for i in 1 .. h.Length - 1 do
                    match h.[i] with
                    | c when c >= '1' && c <= '9' ->
                        if indent.IsSome then
                            valid <- false
                        else
                            indent <- Some (int (string c))
                    | '-' ->
                        if chomp <> ChompingMode.Clip then
                            valid <- false
                        else
                            chomp <- ChompingMode.Strip
                    | '+' ->
                        if chomp <> ChompingMode.Clip then
                            valid <- false
                        else
                            chomp <- ChompingMode.Keep
                    | _ ->
                        valid <- false

                if valid then
                    Some { Style = style; Indent = indent; Chomp = chomp }
                else
                    None

    let stripProperties (s: string) =
        let rec loop (current: string) =
            let c = current.TrimStart()
            if c.StartsWith("&") then
                let idx = c.IndexOf(' ')
                if idx < 0 then "" else loop (c.Substring(idx + 1))
            elif c.StartsWith("!<") then
                let idx = c.IndexOf('>')
                if idx < 0 then c else loop (c.Substring(idx + 1))
            elif c.StartsWith("!") && not (c.StartsWith("|")) && not (c.StartsWith(">")) then
                let idx = c.IndexOf(' ')
                if idx < 0 then "" else loop (c.Substring(idx + 1))
            else
                c

        loop s

    let headerTokenFromLine (line: string) =
        let trimmed = line.TrimStart()
        let afterDash =
            if trimmed.StartsWith("- ") then
                trimmed.Substring(2).TrimStart()
            else
                trimmed

        let candidate =
            let idx = afterDash.IndexOf(':')
            if idx >= 0 then afterDash.Substring(idx + 1).TrimStart() else afterDash

        stripProperties candidate
        |> fun s -> s.Split([|' '; '\t'|], StringSplitOptions.RemoveEmptyEntries)
        |> Array.tryHead

    let tryDetectHeaderIndent (line: string) =
        match headerTokenFromLine line with
        | Some token when parseHeader token |> Option.isSome ->
            Some (Line.countLeadingSpaces line)
        | _ ->
            None

    let isHeaderLine (line: string) =
        tryDetectHeaderIndent line |> Option.isSome

    let applyChomping (chomp: ChompingMode) (content: string) =
        match chomp with
        | ChompingMode.Strip -> content.TrimEnd([|'\r'; '\n'|])
        | ChompingMode.Keep -> content
        | ChompingMode.Clip ->
            let trimmed = content.TrimEnd([|'\r'; '\n'|])
            if content.Length > trimmed.Length then trimmed + "\n" else trimmed

    let stripIndent (indent: int) (line: string) =
        if line.Trim() = "" then
            ""
        else
            let available = Line.countLeadingSpaces line
            line.Substring(min indent available)

    let foldLines (lines: string list) =
        let isMoreIndented (line: string) =
            line.StartsWith(" ") || line.StartsWith("\t")

        let arr = lines |> List.toArray
        let sb = StringBuffer()

        for i in 0 .. arr.Length - 1 do
            let line = arr.[i]
            if line.Trim() = "" then
                sb.Append('\n') |> ignore
            else
                sb.Append(line) |> ignore
                if i < arr.Length - 1 then
                    let next = arr.[i + 1]
                    if next.Trim() = "" then
                        sb.Append('\n') |> ignore
                    elif isMoreIndented line || isMoreIndented next then
                        sb.Append('\n') |> ignore
                    else
                        sb.Append(' ') |> ignore

        sb.ToString()

    let deindentLines (headerIndent: int) (explicitIndent: int option) (lines: string list) =
        let contentIndent =
            match explicitIndent with
            | Some i -> headerIndent + i
            | None ->
                lines
                |> List.filter (fun l -> l.Trim() <> "")
                |> List.map Line.countLeadingSpaces
                |> function
                    | [] -> headerIndent
                    | indents -> indents |> List.min

        lines |> List.map (stripIndent contentIndent)

    let buildContent (style: BlockScalarStyle) (chomp: ChompingMode) (headerIndent: int) (explicitIndent: int option) (lines: string list) =
        let deindentedLines = deindentLines headerIndent explicitIndent lines
        let content =
            match style with
            | BlockScalarStyle.Literal ->
                if List.isEmpty deindentedLines then "" else String.Join("\n", deindentedLines) + "\n"
            | BlockScalarStyle.Folded ->
                let folded = foldLines deindentedLines
                if folded.EndsWith("\n") then folded else folded + "\n"

        applyChomping chomp content

module FlowTokens =

    type Token =
        | OpenBrace
        | CloseBrace
        | OpenBracket
        | CloseBracket
        | Colon
        | Comma
        | String of string
        | EOF

    let tokenize (input: string) : Token list =
        let rec parseString (chars: char list) (acc: char list) : char list * char list =
            match chars with
            | [] -> (List.rev acc, [])
            | '"'::rest -> (List.rev acc, rest)
            | '\\'::c::rest -> parseString rest (c::'\\'::acc)
            | c::rest -> parseString rest (c::acc)

        let consumePlaceholder (cs: char list) (acc: char list) =
            let rec loop (cs: char list) (acc: char list) =
                match cs with
                | '/'::'>'::rest -> (List.rev ('>'::'/'::acc), rest)
                | c::rest -> loop rest (c::acc)
                | [] -> (List.rev acc, [])
            loop cs acc

        let rec tokenizeChars (chars: char list) (acc: Token list) : Token list =
            match chars with
            | [] -> List.rev (Token.EOF::acc)
            | ' '::rest | '\n'::rest | '\r'::rest | '\t'::rest -> tokenizeChars rest acc
            | '{'::rest -> tokenizeChars rest (Token.OpenBrace::acc)
            | '}'::rest -> tokenizeChars rest (Token.CloseBrace::acc)
            | '['::rest -> tokenizeChars rest (Token.OpenBracket::acc)
            | ']'::rest -> tokenizeChars rest (Token.CloseBracket::acc)
            | ':'::rest -> tokenizeChars rest (Token.Colon::acc)
            | ','::rest -> tokenizeChars rest (Token.Comma::acc)
            | '"'::rest ->
                let (str, remaining) = parseString rest []
                let strValue = System.String(Array.ofList str)
                tokenizeChars remaining (Token.String strValue::acc)
            | chars ->
                let rec parseUnquoted (cs: char list) (acc: char list) =
                    match cs with
                    | [] -> (List.rev acc, [])
                    | '<'::'s'::' '::'f'::'='::rest ->
                        let (placeholder, remaining) = consumePlaceholder rest ['=';'f';' ';'s';'<']
                        let (restStr, final) = parseUnquoted remaining []
                        (placeholder @ restStr, final)
                    | '<'::'c'::' '::'f'::'='::rest ->
                        let (placeholder, remaining) = consumePlaceholder rest ['=';'f';' ';'c';'<']
                        let (restStr, final) = parseUnquoted remaining []
                        (placeholder @ restStr, final)
                    | c::rest when c = '{' || c = '}' || c = '[' || c = ']' || c = ':' || c = ',' || c = '\n' || c = '\r' || c = '\t' ->
                        (List.rev acc, cs)
                    | ' '::rest when acc.IsEmpty ->
                        parseUnquoted rest acc
                    | c::rest -> parseUnquoted rest (c::acc)

                let (str, remaining) = parseUnquoted chars []
                if str.IsEmpty then
                    tokenizeChars remaining acc
                else
                    let strValue = System.String(Array.ofList str)
                    tokenizeChars remaining (Token.String strValue::acc)

        tokenizeChars (List.ofSeq input) []
