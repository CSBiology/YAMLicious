module YAMLicious.YAMLiciousTypes

open System.Text
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

type Preprocessor = {
    AST: PreprocessorElement
    StringMap: Dictionary<int, string>
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
        let sb = StringBuilder()
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
} with
    static member create(value, ?comment, ?anchor, ?tag) = 
        { Value = value; Comment = comment; Anchor = anchor; Tag = tag }

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