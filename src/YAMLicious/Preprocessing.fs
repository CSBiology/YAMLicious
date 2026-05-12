module YAMLicious.Preprocessing

open YAMLicious.StringBuffer
open System.Collections.Generic
open Syntax
open YAMLiciousTypes

let private isPresentationOnlyLine (line: string) =
    line.Trim() = "" || Placeholder.isCommentOnlyLine line

let private tryFindContentLine (lines: string list) =
    lines
    |> List.tryFind (isPresentationOnlyLine >> not)

let private shouldStayInNestedBlock (currentIntendation: int) (inBlockScalar: bool) (line: string) (rest: string list) =
    if line.Trim() = "" then
        if inBlockScalar then
            match rest |> List.tryFind (fun l -> l.Trim() <> "") with
            | Some nextLine -> Line.countLeadingSpaces nextLine > currentIntendation
            | None -> true
        else
            match rest |> List.tryFind (fun l -> l.Trim() <> "") with
            | Some nextLine when Placeholder.isCommentOnlyLine nextLine && Line.countLeadingSpaces nextLine <= currentIntendation ->
                match tryFindContentLine rest with
                | Some nextContentLine -> Line.countLeadingSpaces nextContentLine > currentIntendation
                | None -> true
            | Some nextLine -> Line.countLeadingSpaces nextLine > currentIntendation
            | None -> true
    elif Placeholder.isCommentOnlyLine line then
        if inBlockScalar then
            Line.countLeadingSpaces line > currentIntendation
        else
            Line.countLeadingSpaces line > currentIntendation
            ||
            match tryFindContentLine rest with
            | Some nextLine -> Line.countLeadingSpaces nextLine > currentIntendation
            | None -> true
    else
        Line.countLeadingSpaces line > currentIntendation

let private splitNestedBlock (currentIntendation: int) (inBlockScalar: bool) (lines: string list) =
    let rec loop acc remaining =
        match remaining with
        | line :: rest when shouldStayInNestedBlock currentIntendation inBlockScalar line rest ->
            loop (line :: acc) rest
        | _ ->
            List.rev acc, remaining

    loop [] lines


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
            let availableIndent = Line.countLeadingSpaces line
            if availableIndent >= indent then line.Substring(indent)
            else line.TrimStart()

    let rec loop (lines: string list) (currentIntendation: int) (inBlockScalar: bool) (acc: PreprocessorElement list) =
        let previousContentLine () =
            acc
            |> List.tryFind (function
                | Line line -> line.Trim() <> ""
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
            let isEmptyLine = line.Trim() = ""
            let isCommentLine = Placeholder.isCommentOnlyLine line

            if isEmptyLine then
                let nextIndentedLine =
                    rest
                    |> List.tryFind (fun l -> l.Trim() <> "")

                match nextIndentedLine with
                | Some nextLine when Line.countLeadingSpaces nextLine > currentIntendation && (List.isEmpty acc || inBlockScalar || canStartNestedBlockAfterPresentation ()) ->
                    let nextIntendation = Line.countLeadingSpaces nextLine
                    let childInBlockScalar = inBlockScalar || nestedBlockScalarAfterPresentation ()
                    let nextLevelLines, currentLevelLines = splitNestedBlock currentIntendation childInBlockScalar (line :: rest)

                    let children = loop nextLevelLines nextIntendation childInBlockScalar [] |> List.rev
                    loop currentLevelLines currentIntendation inBlockScalar (Intendation children :: acc)
                | _ ->
                    loop rest currentIntendation inBlockScalar (Line("") :: acc)
            elif isCommentLine then
                let nextIndentedLine =
                    rest |> tryFindContentLine

                match nextIndentedLine with
                | Some nextLine when Line.countLeadingSpaces nextLine > currentIntendation && canStartNestedBlockAfterPresentation () ->
                    let nextIntendation = Line.countLeadingSpaces nextLine
                    let childInBlockScalar = inBlockScalar || nestedBlockScalarAfterPresentation ()
                    let nextLevelLines, currentLevelLines = splitNestedBlock currentIntendation childInBlockScalar (line :: rest)

                    let children = loop nextLevelLines nextIntendation childInBlockScalar [] |> List.rev
                    loop currentLevelLines currentIntendation inBlockScalar (Intendation children :: acc)
                | _ ->
                    let lineText = stripIndent currentIntendation line
                    let lineEle = Line(lineText)
                    loop rest currentIntendation inBlockScalar (lineEle :: acc)
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
                    let nextLevelLines, currentLevelLines = splitNestedBlock currentIntendation childInBlockScalar rest

                    let otherChildren = loop nextLevelLines nextIntendation childInBlockScalar [] |> List.rev
                    let children = lineEle :: otherChildren
                    loop currentLevelLines currentIntendation inBlockScalar (Intendation children :: acc)

    let ast = loop (List.ofArray content.Lines) 0 false [] |> List.rev |> Level

    { AST = ast
      StringMap = content.StringMap
      CommentMap = content.CommentMap
      YAMLVersion = content.YAMLVersion
      TagHandles = content.TagHandles }

let mkLine (line: string) = Line line

let mklLevel (children: #seq<PreprocessorElement>) = List.ofSeq children |> Level

let mkIntendation (children: #seq<PreprocessorElement>) = List.ofSeq children |> Intendation
