module YAMLicious.YAMLiciousTypes

/// This contains information about single token information, such as a key/value in key-value pair, a seq element or anything

open System.Text
open System.Collections.Generic

type Config = {
    Whitespace: int
    Level: int
} with
    static member init(?whitespace) : Config = {
        Whitespace = defaultArg whitespace 4
        Level = 0
    }
    member this.WhitespaceString =
        String.init (this.Level*this.Whitespace) (fun _ -> " ")

type Preprocessor = {
    AST: PreprocessorElement
    StringMap: Dictionary<int, string>
    CommentMap: Dictionary<int, string>
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
            | Nil ->
                ()
        innerprint this 0
        sb.ToString()


type YAMLContent = {
    Value: string
    Comment: string option
} with
    static member create (value, ?comment) = { Value = value; Comment = comment }

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
    // This does not write anything
    | Nil

[<Literal>]
let SequenceSquareDelimiter = ","

[<Literal>]
let NewLineChar = '\n'

[<Literal>]
let YAML_NULL = "null"