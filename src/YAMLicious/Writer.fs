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

    let private mkTag (tag: string option) =
        tag
        |> Option.map (fun s -> if s.StartsWith("<") then "!" + s + " " else "!" + s + " ")
        |> Option.defaultValue ""

    let private mkAnchor (anchor: string option) =
        anchor
        |> Option.map (fun s -> "&" + s + " ")
        |> Option.defaultValue ""

    let mkNodePrefix (content: YAMLContent) =
        mkTag content.Tag + mkAnchor content.Anchor

    let private appendComment (content: YAMLContent) (s: string) =
        s + (content.Comment |> Option.map (fun c -> " " + mkComment c) |> Option.defaultValue "")

    let private escapeSingleQuoted (s: string) = s.Replace("'", "''")

    let private escapeDoubleQuoted (s: string) =
        s
            .Replace("\\", "\\\\")
            .Replace("\"", "\\\"")
            .Replace("\t", "\\t")
            .Replace("\r", "\\r")
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

    let shouldEmitBlockScalar (options: WriterOptions) (content: YAMLContent) =
        let hasMultiline = content.Value.Contains("\n")
        match options.PreserveScalarStyle, content.Style with
        | true, Some (ScalarStyle.Block _) -> true
        | _ -> hasMultiline

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

    let mkBlockScalarMapping (options: WriterOptions) (key: string) (content: YAMLContent) =
        let style, chomp, indent = resolveBlockStyle options content
        let header = mkBlockHeader style chomp indent
        let bodyText = bodyTextForChomping chomp content.Value
        let lines = splitPreservingEmpty bodyText
        let headerLine = mkKey key + " " + mkNodePrefix content + header |> appendComment content
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

let detokenizeWithOptions (options: WriterOptions) (ele: YAMLElement) =
    let rec loop (ele: YAMLElement) =
        match ele with
        | YAMLElement.Nil ->
            PreprocessorElement.Nil
        | YAMLElement.Mapping (key, v) ->
            match v with
            | YAMLElement.Value value when Formatting.shouldEmitBlockScalar options value ->
                Formatting.mkBlockScalarMapping options key.Value value
            | YAMLElement.Value value ->
                let s = Formatting.mkKey key.Value + " " + Formatting.mkInlineContent options value
                PreprocessorElement.Line s
            | YAMLElement.Sequence seq when StyleVerifier.checkInlineSequence seq ->
                let s = Formatting.mkKey key.Value + " " + Formatting.mkInlineSequence options seq
                PreprocessorElement.Line s
            | anyElse ->
                PreprocessorElement.Level [
                    PreprocessorElement.Line (Formatting.mkKey (Formatting.mkInlineContent options key))
                    PreprocessorElement.Intendation [
                        loop anyElse
                    ]
                ]
        | YAMLElement.Value value when Formatting.shouldEmitBlockScalar options value ->
            let style, chomp, indent = Formatting.resolveBlockStyle options value
            let header =
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
            let bodyText =
                let normalized = value.Value.Replace("\r\n", "\n")
                match chomp with
                | ChompingMode.Strip -> normalized.TrimEnd([| '\n' |])
                | ChompingMode.Clip
                | ChompingMode.Keep ->
                    if normalized.EndsWith("\n") then normalized.Substring(0, normalized.Length - 1) else normalized
            let lines = bodyText.Split([| '\n' |], System.StringSplitOptions.None) |> Array.toList
            PreprocessorElement.Level [
                PreprocessorElement.Line header
                PreprocessorElement.Intendation (lines |> List.map PreprocessorElement.Line)
            ]
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
