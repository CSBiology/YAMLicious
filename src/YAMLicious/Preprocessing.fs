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
                    // Batch process same-level lines iteratively
                    let mutable remainingLines = lines
                    let mutable currentAcc = acc
                    let mutable continueSameLevel = true

                    while continueSameLevel && not (List.isEmpty remainingLines) do
                        match remainingLines with
                        | currentLine :: remaining ->
                            let currentIndent = ReadHelpers.indentLevel currentLine
                            let isEmpty = currentLine.Trim() = ""

                            if isEmpty then
                                let nextNonEmpty = remaining |> List.tryFind (fun l -> l.Trim() <> "")
                                match nextNonEmpty with
                                | Some nextLine when ReadHelpers.indentLevel nextLine = currentIntendation ->
                                    currentAcc <- Line("") :: currentAcc
                                    remainingLines <- remaining
                                | _ ->
                                    continueSameLevel <- false
                            elif currentIndent = currentIntendation then
                                let lineText = stripIndent currentIntendation currentLine
                                let lineEle = Line(lineText)
                                currentAcc <- lineEle :: currentAcc
                                remainingLines <- remaining
                            else
                                continueSameLevel <- false
                        | [] ->
                            continueSameLevel <- false

                    if List.isEmpty remainingLines then
                        currentAcc
                    else
                        loop remainingLines currentIntendation currentAcc
                else
                    // Different indentation - need to process item and its nested content
                    // Optimization: batch process multiple such items iteratively
                    let mutable currentRemainingLines = lines
                    let mutable currentBatchAcc = acc
                    let mutable shouldContinueBatch = true

                    while shouldContinueBatch && not (List.isEmpty currentRemainingLines) do
                        match currentRemainingLines with
                        | currentLine :: currentRest ->
                            let currentLineIndent = ReadHelpers.indentLevel currentLine
                            let isCurrentEmpty = currentLine.Trim() = ""

                            // Check if this line should be processed in this batch
                            // We only batch lines that start at currentIntendation level
                            if isCurrentEmpty || currentLineIndent < currentIntendation then
                                // Empty line or shallower - stop batching
                                shouldContinueBatch <- false
                            elif currentLineIndent = currentIntendation then
                                // This is at our level - should not happen in else branch initially
                                // but can happen after we've processed some items
                                shouldContinueBatch <- false
                            else
                                // currentLineIndent > currentIntendation
                                // Process this item and its nested content
                                let lineText = stripIndent currentLineIndent currentLine
                                let lineEle = Line(lineText)

                                let nextLevelLines =
                                    currentRest
                                    |> List.takeWhile (fun l ->
                                        let isEmpty = l.Trim() = ""
                                        isEmpty || ReadHelpers.indentLevel l > currentIntendation
                                    )

                                let afterThisItem =
                                    currentRest
                                    |> List.skipWhile (fun l ->
                                        let isEmpty = l.Trim() = ""
                                        isEmpty || ReadHelpers.indentLevel l > currentIntendation
                                    )

                                let otherChildren = loop nextLevelLines currentLineIndent [] |> List.rev
                                let children = lineEle :: otherChildren
                                currentBatchAcc <- Intendation children :: currentBatchAcc
                                currentRemainingLines <- afterThisItem
                        | [] ->
                            shouldContinueBatch <- false

                    // Continue processing any remaining lines
                    if List.isEmpty currentRemainingLines then
                        currentBatchAcc
                    else
                        loop currentRemainingLines currentIntendation currentBatchAcc

    let ast = loop (List.ofArray content.Lines) 0 [] |> List.rev |> Level

    { AST = ast
      StringMap = content.StringMap
      CommentMap = content.CommentMap
      YAMLVersion = content.YAMLVersion
      TagHandles = content.TagHandles }

let mkLine (line: string) = Line line

let mklLevel (children: #seq<PreprocessorElement>) = List.ofSeq children |> Level

let mkIntendation (children: #seq<PreprocessorElement>) = List.ofSeq children |> Intendation
