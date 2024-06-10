module YAMLicious.YAMLiciousTypes

/// This contains information about single token information, such as a key/value in key-value pair, a seq element or anything
type YAMLContent(value: string) =
    member val Comment: string option = None with get, set
    member val Value: string = value with get, set

[<RequireQualifiedAccess>]
type YAMLElement =
    | Mapping of YAMLContent * YAMLElement
    | String of YAMLContent
    | Sequence of YAMLElement list