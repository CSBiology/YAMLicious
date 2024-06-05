module YAMLicious.Persil

open System
open System.Collections.Generic
open System.Text.RegularExpressions


[<Literal>]
let KeyValuePattern = 
    "^.*?: .*"

[<Literal>]
let ValuePattern = 
    "^(?!.*:)\S.*"

[<Literal>]
let SequencePattern = 
    "^-(?![\[\]])\s.*"

[<Literal>]
let NewLineChar = '\n'
