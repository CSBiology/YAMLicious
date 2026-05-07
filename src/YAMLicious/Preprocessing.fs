module YAMLicious.Preprocessing

open YAMLicious.StringBuffer
open System.Collections.Generic
open System.Text.RegularExpressions
open YAMLiciousTypes

module ReadHelpers =
    let indentLevel (line: string) =
        line |> Seq.takeWhile (fun c -> c = ' ') |> Seq.length

let private isCommentOnlyLine (line: string) =
    Regex.IsMatch(line.Trim(), "^<c f=\d+/>$")

let private isPresentationOnlyLine (line: string) =
    line.Trim() = "" || isCommentOnlyLine line

let private isBlockScalarHeaderLine (line: string) =
    Regex.IsMatch(line.TrimEnd(), @":\s*[|>](?:[1-9][-+]?|[-+]?[1-9]?)?$")

let private tryFindContentLine (lines: string list) =
    lines
    |> List.tryFind (isPresentationOnlyLine >> not)

let private shouldStayInNestedBlock (currentIntendation: int) (line: string) (rest: string list) =
    if line.Trim() = "" then
        match rest |> List.tryFind (fun l -> l.Trim() <> "") with
        | Some nextLine -> ReadHelpers.indentLevel nextLine > currentIntendation
        | None -> true
    elif isCommentOnlyLine line then
        match tryFindContentLine rest with
        | Some nextLine -> ReadHelpers.indentLevel nextLine > currentIntendation
        | None -> true
    else
        ReadHelpers.indentLevel line > currentIntendation

let private splitNestedBlock (currentIntendation: int) (lines: string list) =
    let rec loop acc remaining =
        match remaining with
        | line :: rest when shouldStayInNestedBlock currentIntendation line rest ->
            loop (line :: acc) rest
        | _ ->
            List.rev acc, remaining

    loop [] lines

let private isDocumentMarker (marker: string) (line: string) =
    let trimmed = line.TrimStart()
    if not (trimmed.StartsWith(marker)) then
        false
    elif trimmed.Length = marker.Length then
        true
    else
        let next = trimmed.[marker.Length]
        System.Char.IsWhiteSpace(next)

let isDocumentStart (line: string) = isDocumentMarker "---" line

let isDocumentEnd (line: string) = isDocumentMarker "..." line

let write (rootElement: PreprocessorElement, fconfig: (Config -> Config) option) =
    let config =
        Config.init ()
        |> fun config -> if fconfig.IsSome then fconfig.Value config else config

    let sb = StringBuffer()

    let rec loop (current: PreprocessorElement) (sb: StringBuffer) (config: Config) =
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
            let availableIndent = ReadHelpers.indentLevel line
            if availableIndent >= indent then line.Substring(indent)
            else line.TrimStart()

    let rec loop (lines: string list) (currentIntendation: int) (acc: PreprocessorElement list) =
        let canStartNestedBlockAfterPresentation () =
            acc
            |> List.tryFind (function
                | Line line -> line.Trim() <> ""
                | _ -> true
            )
            |> function
                | Some (Line line) ->
                    let trimmed = line.TrimEnd()
                    trimmed.EndsWith(":") || isBlockScalarHeaderLine trimmed
                | _ -> false

        match lines with
        | [] -> acc
        | line :: rest ->
            let isEmptyLine = line.Trim() = ""
            let isCommentLine = isCommentOnlyLine line

            if isEmptyLine then
                let nextIndentedLine =
                    rest
                    |> List.tryFind (fun l -> l.Trim() <> "")

                match nextIndentedLine with
                | Some nextLine when ReadHelpers.indentLevel nextLine > currentIntendation ->
                    let nextIntendation = ReadHelpers.indentLevel nextLine
                    let nextLevelLines, currentLevelLines = splitNestedBlock currentIntendation (line :: rest)

                    let children = loop nextLevelLines nextIntendation [] |> List.rev
                    loop currentLevelLines currentIntendation (Intendation children :: acc)
                | _ ->
                    loop rest currentIntendation (Line("") :: acc)
            elif isCommentLine then
                let nextIndentedLine =
                    rest |> tryFindContentLine

                match nextIndentedLine with
                | Some nextLine when ReadHelpers.indentLevel nextLine > currentIntendation && canStartNestedBlockAfterPresentation () ->
                    let nextIntendation = ReadHelpers.indentLevel nextLine
                    let nextLevelLines, currentLevelLines = splitNestedBlock currentIntendation (line :: rest)

                    let children = loop nextLevelLines nextIntendation [] |> List.rev
                    loop currentLevelLines currentIntendation (Intendation children :: acc)
                | _ ->
                    let lineText = stripIndent currentIntendation line
                    let lineEle = Line(lineText)
                    loop rest currentIntendation (lineEle :: acc)
            else
                let nextIntendation = ReadHelpers.indentLevel line

                if nextIntendation = currentIntendation then
                    let lineText = stripIndent currentIntendation line
                    let lineEle = Line(lineText)
                    loop rest currentIntendation (lineEle :: acc)
                else
                    let lineText =
                        stripIndent nextIntendation line
                    let lineEle = Line(lineText)
                    let nextLevelLines, currentLevelLines = splitNestedBlock currentIntendation rest

                    let otherChildren = loop nextLevelLines nextIntendation [] |> List.rev
                    let children = lineEle :: otherChildren
                    loop currentLevelLines currentIntendation (Intendation children :: acc)

    let ast = loop (List.ofArray content.Lines) 0 [] |> List.rev |> Level

    { AST = ast
      StringMap = content.StringMap
      CommentMap = content.CommentMap
      YAMLVersion = content.YAMLVersion
      TagHandles = content.TagHandles }

let mkLine (line: string) = Line line

let mklLevel (children: #seq<PreprocessorElement>) = List.ofSeq children |> Level

let mkIntendation (children: #seq<PreprocessorElement>) = List.ofSeq children |> Intendation
