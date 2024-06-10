module YAMLicious.YAMLiciousTypes

/// This contains information about single token information, such as a key/value in key-value pair, a seq element or anything
type YAMLContent(value: string, ?comment: string) =
    member val Comment: string option = comment with get, set
    member val Value: string = value with get, set

[<RequireQualifiedAccess>]
type YAMLElement =
    | Mapping of YAMLContent * YAMLElement
    | Value of YAMLContent
    | SequenceElement of YAMLElement
    // A true Yaml sequence
    | Sequence of YAMLElement list
    /// Same intendation list of yaml elements
    ///
    /// Example1:
    ///
    /// MyKey1: MyValue1
    /// MyKey2: MyValue2
    | Level of YAMLElement list
    | Comment of string

[<Literal>]
let SequenceSquareDelimiter = ","

[<Literal>]
let NewLineChar = '\n'