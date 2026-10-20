module YAMLicious.Preprocessing

open System.Collections.Generic
open System.Text
open Syntax
open YAMLiciousTypes

let private isContentLine (line: string) =
    not (Line.isBlank line || Placeholder.isCommentOnlyLine line)

let private isFlowCloserLine (line: string) =
    let withoutComment, _ = Placeholder.splitTrailingComment line
    match withoutComment.Trim().TrimEnd(',') with
    | "]" | "}" -> true
    | _ -> false

let private withIndent (indent: int) (line: string) =
    System.String(' ', indent) + line.TrimStart()

/// Splits the body off a block scalar. The body holds every blank line and every line indented at
/// least as deep as the content indentation. It ends before the next nonblank line that is indented
/// less. Blank lines before that line stay in the body so that keep chomping can retain them.
let private takeBlockScalarBody (headerIndent: int) (indicator: int option) (lines: string list) =
    let contentIndent = BlockScalar.contentIndent headerIndent indicator lines
    let rec loop acc remaining =
        match remaining with
        | line :: rest when BlockScalar.isBodyLine contentIndent line ->
            loop (line :: acc) rest
        | _ ->
            List.rev acc, remaining

    loop [] lines

/// The indentation of the node that owns the last content line. A block scalar body belongs to
/// the node of its header, so a trailing comment cannot sit deeper than the header.
let private lastNodeIndent (lines: string list) =
    let rec loop (remaining: string list) (indent: int) =
        match remaining with
        | [] -> indent
        | line :: rest when not (isContentLine line) -> loop rest indent
        | line :: rest ->
            let lineIndent = Line.countLeadingSpaces line
            match BlockScalar.tryParseHeaderLine line with
            | Some header ->
                let _, afterBody = takeBlockScalarBody lineIndent header.Indent rest
                loop afterBody lineIndent
            | None -> loop rest lineIndent

    loop lines 0

/// A full-line comment belongs to the node that follows it. Giving it the indentation of the
/// next content line puts it into that node's block, so the indentation pass below treats
/// comments like any other line. Comments with no content after them stay in the last block, at
/// most as deep as the node of its final content line. A comment before a flow closer takes the
/// indentation of the previous entry when that entry is deeper than the closer, and the
/// indentation of the closer otherwise. Either way it stays inside the flow collection.
let private alignCommentLines (lines: string list) =
    let trailingIndent = lastNodeIndent lines

    let rec loop (reversed: string list) (nextContent: string option) (acc: string list) =
        match reversed with
        | [] -> acc
        | line :: rest when isContentLine line ->
            loop rest (Some line) (line :: acc)
        | line :: rest when Placeholder.isCommentOnlyLine line ->
            let aligned =
                match nextContent with
                | Some next when isFlowCloserLine next ->
                    let closerIndent = Line.countLeadingSpaces next
                    let previousIndent =
                        rest
                        |> List.tryFind isContentLine
                        |> Option.map Line.countLeadingSpaces
                    match previousIndent with
                    | Some p when p > closerIndent -> withIndent p line
                    | _ -> withIndent closerIndent line
                | Some next -> withIndent (Line.countLeadingSpaces next) line
                | None -> withIndent (min trailingIndent (Line.countLeadingSpaces line)) line
            loop rest nextContent (aligned :: acc)
        | line :: rest ->
            loop rest nextContent (line :: acc)

    loop (List.rev lines) None []

let private shouldStayInNestedBlock (currentIntendation: int) (line: string) (rest: string list) =
    if Line.isBlank line then
        match rest |> List.tryFind (Line.isBlank >> not) with
        | Some nextLine -> Line.countLeadingSpaces nextLine > currentIntendation
        | None -> true
    else
        Line.countLeadingSpaces line > currentIntendation

/// Removes exactly the header's indentation from a body line. Counting spaces keeps tabs and
/// all deeper indentation, which is content. A blank line shorter than the header loses only the
/// spaces it has.
let private stripBlockScalarIndent (headerIndent: int) (line: string) =
    line.Substring(min headerIndent (Line.countLeadingSpaces line))

/// Reads a block scalar body as one flat block. Its indentation beyond the header's is content,
/// so the structural pass neither nests nor interprets the lines. The Reader gets them as they are.
let private readBlockScalarBody (headerIndent: int) (indicator: int option) (lines: string list) =
    let body, remaining = takeBlockScalarBody headerIndent indicator lines
    let bodyLines = body |> List.map (fun bodyLine -> Line(stripBlockScalarIndent headerIndent bodyLine))
    Intendation bodyLines, remaining

/// Moves a line onto the reversed accumulator. A block scalar header takes its whole body along,
/// so an enclosing block never separates the header from the body or from its trailing blank lines.
let private pushLineWithBody (acc: string list) (line: string) (rest: string list) =
    match BlockScalar.tryParseHeaderLine line with
    | Some header ->
        let body, remaining = takeBlockScalarBody (Line.countLeadingSpaces line) header.Indent rest
        List.rev body @ (line :: acc), remaining
    | None ->
        line :: acc, rest

let private splitNestedBlock (currentIntendation: int) (lines: string list) =
    let rec loop acc remaining =
        match remaining with
        | line :: rest when shouldStayInNestedBlock currentIntendation line rest ->
            let nextAcc, nextRemaining = pushLineWithBody acc line rest
            loop nextAcc nextRemaining
        | _ ->
            List.rev acc, remaining

    loop [] lines


let write (rootElement: PreprocessorElement, fconfig: (Config -> Config) option) =
    let config =
        Config.init ()
        |> fun config -> if fconfig.IsSome then fconfig.Value config else config

    let sb = StringBuilder()

    let rec loop (current: PreprocessorElement) (sb: StringBuilder) (config: Config) =
        match current with
        | Line line -> sb.AppendLine(config.WhitespaceString + line) |> ignore
        | Intendation children ->
            let nextConfig = { config with Level = config.Level + 1 }

            for child in children do
                loop child sb nextConfig
        | Level children ->
            for child in children do
                loop child sb config
        | Nil -> ()

    loop rootElement sb config
    sb.ToString()

let read (yamlStr: string) =
    let content = Persil.pipeline yamlStr

    let stripIndent (indent: int) (line: string) =
        if indent <= 0 then line
        else
            let availableIndent = Line.countLeadingSpaces line
            if availableIndent >= indent then line.Substring(indent)
            else line.TrimStart()

    let rec loop (lines: string list) (currentIntendation: int) (acc: PreprocessorElement list) =
        let previousContentLine () =
            acc
            |> List.tryFind (function
                | Line line -> isContentLine line
                | _ -> true
            )

        let previousContentLineOpensBlock () =
            previousContentLine ()
            |> function
                | Some (Line line) ->
                    let trimmed = line.TrimEnd()
                    trimmed.EndsWith(":")
                    || trimmed.EndsWith("[")
                    || trimmed.EndsWith("{")
                | _ -> false

        let blockScalarHeaderBefore () =
            match previousContentLine () with
            | Some (Line line) -> BlockScalar.tryParseHeaderLine line
            | _ -> None

        match lines with
        | [] -> acc
        | line :: rest ->
            if Line.isBlank line then
                let nextIndentation =
                    rest
                    |> List.tryFind (Line.isBlank >> not)
                    |> Option.map Line.countLeadingSpaces

                let header =
                    match nextIndentation with
                    | Some nextIntendation when nextIntendation > currentIntendation -> blockScalarHeaderBefore ()
                    | _ -> None

                match header, nextIndentation with
                | Some header, _ ->
                    let bodyElement, remaining = readBlockScalarBody currentIntendation header.Indent lines
                    loop remaining currentIntendation (bodyElement :: acc)
                | None, Some nextIntendation when nextIntendation > currentIntendation && (List.isEmpty acc || previousContentLineOpensBlock ()) ->
                    let nextLevelLines, currentLevelLines = splitNestedBlock currentIntendation (line :: rest)

                    let children = loop nextLevelLines nextIntendation [] |> List.rev
                    loop currentLevelLines currentIntendation (Intendation children :: acc)
                | None, _ ->
                    loop rest currentIntendation (Line("") :: acc)
            else
                let nextIntendation = Line.countLeadingSpaces line

                if nextIntendation = currentIntendation then
                    let lineText = stripIndent currentIntendation line
                    let lineEle = Line(lineText)
                    loop rest currentIntendation (lineEle :: acc)
                else
                    let header =
                        if nextIntendation > currentIntendation then blockScalarHeaderBefore () else None

                    match header with
                    | Some header ->
                        let bodyElement, remaining = readBlockScalarBody currentIntendation header.Indent lines
                        loop remaining currentIntendation (bodyElement :: acc)
                    | None ->
                        // The block starts with its first line, which can itself be a header,
                        // so the recursion below processes that line too.
                        let firstReversed, afterFirst = pushLineWithBody [] line rest
                        let nextLevelLines, currentLevelLines = splitNestedBlock currentIntendation afterFirst

                        let children = loop (List.rev firstReversed @ nextLevelLines) nextIntendation [] |> List.rev
                        loop currentLevelLines currentIntendation (Intendation children :: acc)

    let lines = content.Lines |> List.ofArray |> alignCommentLines
    let ast = loop lines 0 [] |> List.rev |> Level

    { AST = ast
      StringMap = content.StringMap
      CommentMap = content.CommentMap
      YAMLVersion = content.YAMLVersion
      TagHandles = content.TagHandles }

let mkLine (line: string) = Line line

let mklLevel (children: #seq<PreprocessorElement>) = List.ofSeq children |> Level

let mkIntendation (children: #seq<PreprocessorElement>) = List.ofSeq children |> Intendation
