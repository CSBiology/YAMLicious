module YAMLicious.PatternMatcher

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

let matcher str =
    if Regex.IsMatch(str, KeyValuePattern) then
        "KeyValue"
    elif Regex.IsMatch(str, ValuePattern) then
        "Value"
    elif Regex.IsMatch(str, SequencePattern) then
        "Sequence"
    else
        "Unknown"