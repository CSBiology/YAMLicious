module YAMLicious.Preprocessing

open YAMLicious.StringBuffer
open System.Collections.Generic
open YAMLiciousTypes

module ReadHelpers =
    let indentLevel (line: string) =
        line |> Seq.takeWhile (fun c -> c = ' ') |> Seq.length

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
        elif line.Length >= indent then line.Substring(indent)
        else line.TrimStart()

    let rec loop (lines: string list) (currentIntendation: int) (acc: PreprocessorElement list) =
        match lines with
        | [] -> acc
        | line :: rest ->
            let isEmptyLine = line.Trim() = ""

            if isEmptyLine then
                let nextIndentedLine =
                    rest
                    |> List.tryFind (fun l -> l.Trim() <> "")

                match nextIndentedLine with
                | Some nextLine when ReadHelpers.indentLevel nextLine > currentIntendation ->
                    let nextIntendation = ReadHelpers.indentLevel nextLine
                    let nextLevelLines =
                        line :: rest
                        |> List.takeWhile (fun l ->
                            let isEmpty = l.Trim() = ""
                            isEmpty || ReadHelpers.indentLevel l > currentIntendation
                        )

                    let currentLevelLines =
                        rest
                        |> List.skipWhile (fun l ->
                            let isEmpty = l.Trim() = ""
                            isEmpty || ReadHelpers.indentLevel l > currentIntendation
                        )

                    let children = loop nextLevelLines nextIntendation [] |> List.rev
                    loop currentLevelLines currentIntendation (Intendation children :: acc)
                | _ ->
                    loop rest currentIntendation (Line("") :: acc)
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
                    let nextLevelLines =
                        rest
                        |> List.takeWhile (fun l ->
                            let isEmpty = l.Trim() = ""
                            isEmpty || ReadHelpers.indentLevel l > currentIntendation
                        )

                    let currentLevelLines =
                        rest
                        |> List.skipWhile (fun l ->
                            let isEmpty = l.Trim() = ""
                            isEmpty || ReadHelpers.indentLevel l > currentIntendation
                        )

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
