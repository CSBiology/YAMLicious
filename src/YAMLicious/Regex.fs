module YAMLicious.PatternMatcher

open System
open System.Collections.Generic
open System.Text.RegularExpressions
open YAMLicious.Persil
open YAMLicious.AST

[<Literal>]
let KeyValuePattern =
    #if FABLE_COMPILER_PYTHON
    "^(?P<key>[a-zA-Z0-9\s]+):\s*(?P<value>([a-zA-Z0-9-\s]+|<s f=\d+\/\>))?\s*?(?P<comment>\<c f=\d+\/\>)?$"
    #endif
    #if !FABLE_COMPILER
    "^(?<key>[a-zA-Z0-9\s]+):\s*(?<value>([a-zA-Z0-9-\s]+|<s f=\d+\/\>))?\s*?(?<comment>\<c f=\d+\/\>)?$"
    #endif

[<Literal>]
let SequenceValuePattern =
    #if FABLE_COMPILER_PYTHON
    "^-\s*(?P<value>([a-zA-Z0-9\s]+|<s f=\d+\/\>))?\s*?(?P<comment>\<c f=\d+\/\>)?$"
    #endif
    #if !FABLE_COMPILER
    "^-\s*(?<value>([a-zA-Z0-9\s]+|<s f=\d+\/\>))?\s*?(?<comment>\<c f=\d+\/\>)?$"
    #endif

[<Literal>]
let InlineSequencePattern =
    #if FABLE_COMPILER_PYTHON
    "^(?P<inlineSequence>\[.+\])\s*?(?P<comment>\<c f=\d+\/\>)?$"
    #endif
    #if !FABLE_COMPILER
    "^(?<inlineSequence>\[.+\])\s*?(?<comment>\<c f=\d+\/\>)?$"
    #endif

[<Literal>]
let SequenceOpenerPattern =
    #if FABLE_COMPILER_PYTHON
    "^\[\s*(?P<comment>\<c f=\d+\/\>)?$"
    #endif
    #if !FABLE_COMPILER
    "^\[\s*(?<comment>\<c f=\d+\/\>)?$"
    #endif

[<Literal>]
let SequenceCloserPattern =
    #if FABLE_COMPILER_PYTHON
    "^\]\s*(?P<comment>\<c f=\d+\/\>)?$"
    #endif
    #if !FABLE_COMPILER
    "^\]\s*(?<comment>\<c f=\d+\/\>)?$"
    #endif

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