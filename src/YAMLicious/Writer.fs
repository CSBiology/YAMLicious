module YAMLicious.Writer

open YAMLicious.YAMLiciousTypes

module StyleVerifier =

    let private isInlineSimpleScalar (v: YAMLContent) =
        not (v.Value.Contains("\n"))
        && v.Comment.IsNone
        &&
        match v.Style with
        | None
        | Some ScalarStyle.Plain
        | Some ScalarStyle.SingleQuoted
        | Some ScalarStyle.DoubleQuoted -> true
        | Some (ScalarStyle.Block _) -> false

    let checkInlineSequence (ele: YAMLElement list) =
        ele
        |> Seq.forall (fun x ->
            match x with
            | YAMLElement.Value v -> isInlineSimpleScalar v
            | _ -> false
        )

module Formatting =

    let mkComment (comment: string) = "#" + comment
    let mkKey (key: string) = key + ":"
    let mkMinusLine (c: string) = "- " + c
    let private continuationPrefix = "  "

    let private mkTag (tag: string option) =
        match tag with
        | None -> ""
        | Some "!" -> "! "
        | Some s when s.StartsWith("!") && not (s.StartsWith("!!")) -> s + " "
        | Some s -> "!<" + s + "> "

    let private mkAnchor (anchor: string option) =
        anchor
        |> Option.map (fun s -> "&" + s + " ")
        |> Option.defaultValue ""

    let mkNodePrefix (content: YAMLContent) =
        mkTag content.Tag + mkAnchor content.Anchor

    let appendOptionalComment (comment: string option) (s: string) =
        s + (comment |> Option.map (fun c -> " " + mkComment c) |> Option.defaultValue "")

    let private appendComment (content: YAMLContent) (s: string) =
        appendOptionalComment content.Comment s

    let private escapeSingleQuoted (s: string) = s.Replace("'", "''")

    let private escapeDoubleQuoted (s: string) =
        s
            .Replace("\\", "\\\\")
            .Replace("\"", "\\\"")
            .Replace("\u0000", "\\0")
            .Replace("\u0007", "\\a")
            .Replace("\u0008", "\\b")
            .Replace("\t", "\\t")
            .Replace("\u000B", "\\v")
            .Replace("\u000C", "\\f")
            .Replace("\r", "\\r")
            .Replace("\u001B", "\\e")
            .Replace("\u0085", "\\N")
            .Replace("\u00A0", "\\_")
            .Replace("\u2028", "\\L")
            .Replace("\u2029", "\\P")
            .Replace("\n", "\\n")

    let private normalizeNewlines (s: string) =
        s.Replace("\r\n", "\n")

    let resolveBlockStyle (options: WriterOptions) (content: YAMLContent) =
        match options.PreserveScalarStyle, content.Style with
        | true, Some (ScalarStyle.Block (style, chomp, indent)) ->
            style, chomp, indent
        | _ ->
            options.MultilineFallbackStyle, options.MultilineFallbackChomping, None

    let private scalarToInlineText (options: WriterOptions) (content: YAMLContent) =
        let v = content.Value
        if options.PreserveScalarStyle then
            match content.Style with
            | Some ScalarStyle.SingleQuoted -> "'" + escapeSingleQuoted v + "'"
            | Some ScalarStyle.DoubleQuoted -> "\"" + escapeDoubleQuoted v + "\""
            | _ -> v
        else
            v

    let mkKeyContent (options: WriterOptions) (content: YAMLContent) =
        mkNodePrefix content + scalarToInlineText options content

    let mkMappingKey (options: WriterOptions) (key: YAMLContent) =
        mkKey (mkKeyContent options key)

    let shouldEmitBlockScalar (options: WriterOptions) (content: YAMLContent) =
        let hasMultiline = content.Value.Contains("\n")
        match options.PreserveScalarStyle, content.Style with
        | true, Some ScalarStyle.Plain -> false
        | true, Some (ScalarStyle.Block _) -> true
        | _ -> hasMultiline

    let shouldEmitPlainMultilineScalar (options: WriterOptions) (content: YAMLContent) =
        options.PreserveScalarStyle
        && content.Value.Contains("\n")
        &&
        match content.Style with
        | Some ScalarStyle.Plain -> true
        | _ -> false

    let mkInlineContent (options: WriterOptions) (content: YAMLContent) =
        let inlineScalar = scalarToInlineText options content
        let combined = mkNodePrefix content + inlineScalar
        appendComment content combined

    let mkInlineSequence (options: WriterOptions) (seq: YAMLElement list) =
        let content =
            seq
            |> List.map (fun x ->
                match x with
                | YAMLElement.Value v -> mkNodePrefix v + scalarToInlineText options v
                | _ -> failwith "Invalid sequence element"
            )
            |> String.concat ", "
        "[" + content + "]"

    let private mkBlockHeader (style: BlockScalarStyle) (chomp: ChompingMode) (indent: int option) =
        let styleChar =
            match style with
            | BlockScalarStyle.Literal -> "|"
            | BlockScalarStyle.Folded -> ">"
        let indentPart = indent |> Option.map string |> Option.defaultValue ""
        let chompPart =
            match chomp with
            | ChompingMode.Strip -> "-"
            | ChompingMode.Clip -> ""
            | ChompingMode.Keep -> "+"
        styleChar + indentPart + chompPart

    let private bodyTextForChomping (chomp: ChompingMode) (value: string) =
        let normalized = normalizeNewlines value
        match chomp with
        | ChompingMode.Strip ->
            normalized.TrimEnd([| '\n' |])
        | ChompingMode.Clip
        | ChompingMode.Keep ->
            if normalized.EndsWith("\n") then normalized.Substring(0, normalized.Length - 1) else normalized

    let private splitPreservingEmpty (s: string) =
        s.Split([| '\n' |], System.StringSplitOptions.None) |> Array.toList

    let private plainMultilineLines (content: YAMLContent) =
        normalizeNewlines content.Value |> splitPreservingEmpty

    let private mkPlainMultilineNode (mkHeader: string -> string) (lines: string list) =
        let firstLine, remainingLines =
            match lines with
            | firstLine :: rest -> firstLine, rest
            | [] -> "", []

        let continuationLines =
            remainingLines
            |> List.map (fun line -> PreprocessorElement.Line (continuationPrefix + line))

        PreprocessorElement.Level (
            PreprocessorElement.Line (mkHeader firstLine) :: continuationLines
        )

    let mkPlainMultilineMapping (options: WriterOptions) (key: YAMLContent) (content: YAMLContent) =
        let lines = plainMultilineLines content
        let mkHeader firstLine =
            mkMappingKey options key + " " + mkNodePrefix content + firstLine |> appendComment content
        mkPlainMultilineNode mkHeader lines

    let mkPlainMultilineSequenceItem (options: WriterOptions) (content: YAMLContent) =
        let lines = plainMultilineLines content
        let mkHeader firstLine =
            "- " + mkNodePrefix content + firstLine |> appendComment content
        mkPlainMultilineNode mkHeader lines

    let mkPlainMultilineRoot (options: WriterOptions) (content: YAMLContent) =
        let lines = plainMultilineLines content
        let mkHeader firstLine =
            mkNodePrefix content + firstLine |> appendComment content
        mkPlainMultilineNode mkHeader lines

    let tryLegacyPlainMultilineContent (items: YAMLElement list) =
        let isAllowedSegment (allowMetadata: bool) (content: YAMLContent) =
            let hasPlainCompatibleStyle =
                match content.Style with
                | None
                | Some ScalarStyle.Plain -> true
                | _ -> false

            let hasOnlyContinuationContent =
                content.Comment.IsNone
                && content.Anchor.IsNone
                && content.Tag.IsNone

            hasPlainCompatibleStyle
            && (allowMetadata || hasOnlyContinuationContent)

        let rec loop (allowMetadata: bool) (remaining: YAMLElement list) (acc: YAMLContent list) =
            match remaining with
            | [] ->
                acc |> List.rev |> Some
            | YAMLElement.Value content :: rest when isAllowedSegment allowMetadata content ->
                loop false rest (content :: acc)
            | _ ->
                None

        match loop true items [] with
        | Some (firstSegment :: _ :: _ as segments) ->
            let rawValue =
                segments
                |> List.map (fun segment -> segment.Value)
                |> String.concat "\n"

            Some { firstSegment with Value = rawValue; Style = Some ScalarStyle.Plain }
        | _ ->
            None

    let mkBlockScalarMapping (options: WriterOptions) (key: YAMLContent) (content: YAMLContent) =
        let style, chomp, indent = resolveBlockStyle options content
        let header = mkBlockHeader style chomp indent
        let bodyText = bodyTextForChomping chomp content.Value
        let lines = splitPreservingEmpty bodyText
        let headerLine = mkMappingKey options key + " " + mkNodePrefix content + header |> appendComment content
        PreprocessorElement.Level [
            PreprocessorElement.Line headerLine
            PreprocessorElement.Intendation (lines |> List.map PreprocessorElement.Line)
        ]

    let mkBlockScalarSequenceItem (options: WriterOptions) (content: YAMLContent) =
        let style, chomp, indent = resolveBlockStyle options content
        let header = mkBlockHeader style chomp indent
        let bodyText = bodyTextForChomping chomp content.Value
        let lines = splitPreservingEmpty bodyText
        let headerLine = "- " + mkNodePrefix content + header |> appendComment content
        PreprocessorElement.Level [
            PreprocessorElement.Line headerLine
            PreprocessorElement.Intendation (lines |> List.map PreprocessorElement.Line)
        ]

    let mkBlockScalarRoot (options: WriterOptions) (content: YAMLContent) =
        let style, chomp, indent = resolveBlockStyle options content
        let header = mkBlockHeader style chomp indent
        let bodyText = bodyTextForChomping chomp content.Value
        let lines = splitPreservingEmpty bodyText
        let headerLine = mkNodePrefix content + header |> appendComment content
        PreprocessorElement.Level [
            PreprocessorElement.Line headerLine
            PreprocessorElement.Intendation (lines |> List.map PreprocessorElement.Line)
        ]

let detokenizeWithOptions (options: WriterOptions) (ele: YAMLElement) =
    let rec loop (ele: YAMLElement) =
        match ele with
        | YAMLElement.Nil ->
            PreprocessorElement.Nil
        | YAMLElement.Mapping (key, v) ->
            match v with
            | _ when key.Comment.IsSome ->
                let header = Formatting.mkMappingKey options key |> Formatting.appendOptionalComment key.Comment
                PreprocessorElement.Level [
                    PreprocessorElement.Line header
                    PreprocessorElement.Intendation [
                        loop v
                    ]
                ]
            | YAMLElement.Object [YAMLElement.Value value] when Formatting.shouldEmitPlainMultilineScalar options value ->
                Formatting.mkPlainMultilineMapping options key value
            | YAMLElement.Object [YAMLElement.Value value] when Formatting.shouldEmitBlockScalar options value ->
                Formatting.mkBlockScalarMapping options key value
            | YAMLElement.Object [YAMLElement.Value value] ->
                let s = Formatting.mkMappingKey options key + " " + Formatting.mkInlineContent options value
                PreprocessorElement.Line s
            | YAMLElement.Object [YAMLElement.Alias a] ->
                PreprocessorElement.Line (Formatting.mkMappingKey options key + " *" + a)
            | YAMLElement.Object items when Formatting.tryLegacyPlainMultilineContent items |> Option.isSome ->
                let value = Formatting.tryLegacyPlainMultilineContent items |> Option.get
                Formatting.mkPlainMultilineMapping options key value
            | YAMLElement.Value value when Formatting.shouldEmitBlockScalar options value ->
                Formatting.mkBlockScalarMapping options key value
            | YAMLElement.Value value when Formatting.shouldEmitPlainMultilineScalar options value ->
                Formatting.mkPlainMultilineMapping options key value
            | YAMLElement.Value value ->
                let s = Formatting.mkMappingKey options key + " " + Formatting.mkInlineContent options value
                PreprocessorElement.Line s
            | YAMLElement.Sequence seq when StyleVerifier.checkInlineSequence seq ->
                let s = Formatting.mkMappingKey options key + " " + Formatting.mkInlineSequence options seq
                PreprocessorElement.Line s
            | anyElse ->
                PreprocessorElement.Level [
                    PreprocessorElement.Line (Formatting.mkMappingKey options key)
                    PreprocessorElement.Intendation [
                        loop anyElse
                    ]
                ]
        | YAMLElement.Object items when Formatting.tryLegacyPlainMultilineContent items |> Option.isSome ->
            let value = Formatting.tryLegacyPlainMultilineContent items |> Option.get
            Formatting.mkPlainMultilineRoot options value
        | YAMLElement.Value value when Formatting.shouldEmitPlainMultilineScalar options value ->
            Formatting.mkPlainMultilineRoot options value
        | YAMLElement.Value value when Formatting.shouldEmitBlockScalar options value ->
            Formatting.mkBlockScalarRoot options value
        | YAMLElement.Value value ->
            PreprocessorElement.Line (Formatting.mkInlineContent options value)
        | YAMLElement.Object seq ->
            PreprocessorElement.Level [
                for element in seq do
                    loop element
            ]
        | YAMLElement.Sequence seq ->
            PreprocessorElement.Level [
                for element in seq do
                    match element with
                    | YAMLElement.Object [YAMLElement.Value value] when Formatting.shouldEmitPlainMultilineScalar options value ->
                        Formatting.mkPlainMultilineSequenceItem options value
                    | YAMLElement.Object [YAMLElement.Value value] when Formatting.shouldEmitBlockScalar options value ->
                        Formatting.mkBlockScalarSequenceItem options value
                    | YAMLElement.Object [YAMLElement.Value value] ->
                        let s = Formatting.mkMinusLine (Formatting.mkInlineContent options value)
                        PreprocessorElement.Line s
                    | YAMLElement.Object [YAMLElement.Alias a] ->
                        PreprocessorElement.Line ("- *" + a)
                    | YAMLElement.Object items when Formatting.tryLegacyPlainMultilineContent items |> Option.isSome ->
                        let value = Formatting.tryLegacyPlainMultilineContent items |> Option.get
                        Formatting.mkPlainMultilineSequenceItem options value
                    | YAMLElement.Value value when Formatting.shouldEmitPlainMultilineScalar options value ->
                        Formatting.mkPlainMultilineSequenceItem options value
                    | YAMLElement.Value value when Formatting.shouldEmitBlockScalar options value ->
                        Formatting.mkBlockScalarSequenceItem options value
                    | YAMLElement.Value value ->
                        let s = Formatting.mkMinusLine (Formatting.mkInlineContent options value)
                        PreprocessorElement.Line s
                    | YAMLElement.Sequence nested when StyleVerifier.checkInlineSequence nested ->
                        let s = Formatting.mkMinusLine (Formatting.mkInlineSequence options nested)
                        PreprocessorElement.Line s
                    | anyElse ->
                        PreprocessorElement.Level [
                            PreprocessorElement.Line "-"
                            PreprocessorElement.Intendation [
                                loop anyElse
                            ]
                        ]
            ]
        | YAMLElement.Comment c ->
            PreprocessorElement.Line (Formatting.mkComment c)
        | YAMLElement.DocumentStart ->
            PreprocessorElement.Line "---"
        | YAMLElement.DocumentEnd ->
            PreprocessorElement.Line "..."
        | YAMLElement.Alias a ->
            PreprocessorElement.Line ("*" + a)
    loop ele

let detokenize (ele: YAMLElement) =
    detokenizeWithOptions WriterOptions.Default ele

let writeWithOptions (ele: YAMLElement) (writerOptions: WriterOptions) (fconfig: (Config -> Config) option) =
    let preprocessed = detokenizeWithOptions writerOptions ele
    Preprocessing.write (preprocessed, fconfig)

let write (ele: YAMLElement) (fconfig: (Config -> Config) option) =
    writeWithOptions ele WriterOptions.Default fconfig
