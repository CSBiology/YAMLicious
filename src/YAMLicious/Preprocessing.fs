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

/// A full-line comment belongs to the node that follows it. Giving it the indentation of the
/// next content line puts it into that node's block, so the indentation pass below treats
/// comments like any other line. Comments with no content after them stay in the last block,
/// at most as deep as its final content line. A comment before a flow closer takes the
/// indentation of the previous entry, which keeps it inside the flow collection.
let private alignCommentLines (lines: string list) =
    let lastContentIndent =
        lines
        |> List.tryFindBack isContentLine
        |> Option.map Line.countLeadingSpaces
        |> Option.defaultValue 0

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
                    | _ -> line
                | Some next -> withIndent (Line.countLeadingSpaces next) line
                | None -> withIndent (min lastContentIndent (Line.countLeadingSpaces line)) line
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

let private splitNestedBlock (currentIntendation: int) (lines: string list) =
    let rec loop acc remaining =
        match remaining with
        | line :: rest when shouldStayInNestedBlock currentIntendation line rest ->
            loop (line :: acc) rest
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

    let rec loop (lines: string list) (currentIntendation: int) (inBlockScalar: bool) (acc: PreprocessorElement list) =
        let previousContentLine () =
            acc
            |> List.tryFind (function
                | Line line -> isContentLine line
                | _ -> true
            )

        let canStartNestedBlockAfterPresentation () =
            previousContentLine ()
            |> function
                | Some (Line line) ->
                    let trimmed = line.TrimEnd()
                    trimmed.EndsWith(":")
                    || trimmed.EndsWith("[")
                    || trimmed.EndsWith("{")
                    || BlockScalar.isHeaderLine trimmed
                | _ -> false

        let nestedBlockScalarAfterPresentation () =
            match previousContentLine () with
            | Some (Line line) -> BlockScalar.isHeaderLine (line.TrimEnd())
            | _ -> false

        match lines with
        | [] -> acc
        | line :: rest ->
            if Line.isBlank line then
                let nextIndentedLine =
                    rest
                    |> List.tryFind (Line.isBlank >> not)

                match nextIndentedLine with
                | Some nextLine when Line.countLeadingSpaces nextLine > currentIntendation && (List.isEmpty acc || inBlockScalar || canStartNestedBlockAfterPresentation ()) ->
                    let nextIntendation = Line.countLeadingSpaces nextLine
                    let childInBlockScalar = inBlockScalar || nestedBlockScalarAfterPresentation ()
                    let nextLevelLines, currentLevelLines = splitNestedBlock currentIntendation (line :: rest)

                    let children = loop nextLevelLines nextIntendation childInBlockScalar [] |> List.rev
                    loop currentLevelLines currentIntendation inBlockScalar (Intendation children :: acc)
                | _ ->
                    loop rest currentIntendation inBlockScalar (Line("") :: acc)
            else
                let nextIntendation = Line.countLeadingSpaces line

                if nextIntendation = currentIntendation then
                    let lineText = stripIndent currentIntendation line
                    let lineEle = Line(lineText)
                    loop rest currentIntendation inBlockScalar (lineEle :: acc)
                else
                    let lineText =
                        stripIndent nextIntendation line
                    let lineEle = Line(lineText)
                    let childInBlockScalar = inBlockScalar || nestedBlockScalarAfterPresentation ()
                    let nextLevelLines, currentLevelLines = splitNestedBlock currentIntendation rest

                    let otherChildren = loop nextLevelLines nextIntendation childInBlockScalar [] |> List.rev
                    let children = lineEle :: otherChildren
                    loop currentLevelLines currentIntendation inBlockScalar (Intendation children :: acc)

    let lines = content.Lines |> List.ofArray |> alignCommentLines
    let ast = loop lines 0 false [] |> List.rev |> Level

    { AST = ast
      StringMap = content.StringMap
      CommentMap = content.CommentMap
      YAMLVersion = content.YAMLVersion
      TagHandles = content.TagHandles }

let mkLine (line: string) = Line line

let mklLevel (children: #seq<PreprocessorElement>) = List.ofSeq children |> Level

let mkIntendation (children: #seq<PreprocessorElement>) = List.ofSeq children |> Intendation
