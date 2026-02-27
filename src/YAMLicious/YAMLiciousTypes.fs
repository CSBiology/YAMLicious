module YAMLicious.YAMLiciousTypes

open YAMLicious.StringBuffer
open System.Collections.Generic

type Config =
    { Whitespace: int
      Level: int }

    static member init(?whitespace) : Config =
        { Whitespace = defaultArg whitespace 4
          Level = 0 }

    member this.WhitespaceString =
        String.init (this.Level * this.Whitespace) (fun _ -> " ")

type YAMLDirective = { Major: int; Minor: int }

type BlockScalarStyle =
    | Literal
    | Folded

type ChompingMode =
    | Strip
    | Clip
    | Keep

type ScalarStyle =
    | Plain
    | SingleQuoted
    | DoubleQuoted
    | Block of BlockScalarStyle * ChompingMode * int option

type QuotedStringKind =
    | SingleQuotedString
    | DoubleQuotedString

type StringMapEntry = {
    Value: string
    Kind: QuotedStringKind
}

type WriterOptions = {
    PreserveScalarStyle: bool
    MultilineFallbackStyle: BlockScalarStyle
    MultilineFallbackChomping: ChompingMode
} with
    static member Default =
        { PreserveScalarStyle = true
          MultilineFallbackStyle = BlockScalarStyle.Literal
          MultilineFallbackChomping = ChompingMode.Strip }

type Preprocessor = {
    AST: PreprocessorElement
    StringMap: Dictionary<int, StringMapEntry>
    CommentMap: Dictionary<int, string>
    YAMLVersion: YAMLDirective option
    TagHandles: Map<string, string> 
}

and PreprocessorElement =
    | Level of PreprocessorElement list
    | Intendation of PreprocessorElement list
    | Line of string
    /// This does not write anything
    | Nil 

    override this.ToString() =
        let sb = StringBuffer()
        let rec innerprint (next: PreprocessorElement) (level: int) =
            let indent = String.init (level*2) (fun _ -> " ")
            match next with
            | Line line -> sb.AppendLine(indent + $"Line \"{line}\"") |> ignore
            | Intendation children ->
                sb.AppendLine(indent + "Intendation [") |> ignore
                for child in children do
                    innerprint child (level+1)
                sb.AppendLine(indent + "]") |> ignore
            | Level children ->
                sb.AppendLine(indent + "Level [") |> ignore
                for child in children do
                    innerprint child (level+1)
                sb.AppendLine(indent + "]") |> ignore
            | Nil -> ()

        innerprint this 0
        sb.ToString()


type YAMLContent = { 
    Value: string
    Comment: string option
    Anchor: string option
    Tag: string option
    Style: ScalarStyle option
} with
    static member create(value, ?comment, ?anchor, ?tag, ?style) = 
        { Value = value; Comment = comment; Anchor = anchor; Tag = tag; Style = style }

[<RequireQualifiedAccess>]
type YAMLElement =
    | Mapping of YAMLContent * YAMLElement
    | Value of YAMLContent
    // A true Yaml sequence
    | Sequence of YAMLElement list
    /// Same intendation list of yaml elements
    ///
    /// Example1:
    ///
    /// MyKey1: MyValue1
    /// MyKey2: MyValue2
    | Object of YAMLElement list
    | Comment of string
    | DocumentStart
    | DocumentEnd
    | Alias of string
    // This does not write anything
    | Nil

[<Literal>]
let SequenceSquareDelimiter = ","

[<Literal>]
let NewLineChar = '\n'

[<Literal>]
let YAML_NULL = "null"

module YAMLElementNormalization =

    let withoutScalarStyleInContent (content: YAMLContent) =
        { content with Style = None }

    let rec withoutScalarStyle (element: YAMLElement) =
        match element with
        | YAMLElement.Mapping (key, value) ->
            YAMLElement.Mapping (withoutScalarStyleInContent key, withoutScalarStyle value)
        | YAMLElement.Value value ->
            YAMLElement.Value (withoutScalarStyleInContent value)
        | YAMLElement.Sequence items ->
            YAMLElement.Sequence (items |> List.map withoutScalarStyle)
        | YAMLElement.Object items ->
            YAMLElement.Object (items |> List.map withoutScalarStyle)
        | YAMLElement.Comment _
        | YAMLElement.DocumentStart
        | YAMLElement.DocumentEnd
        | YAMLElement.Alias _
        | YAMLElement.Nil ->
            element
