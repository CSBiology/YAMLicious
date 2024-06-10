module YAMLicious.PatternMatcher

open System
open System.Collections.Generic
open System.Text.RegularExpressions
open YAMLicious.Persil
open YAMLicious.AST

[<Literal>]
let KeyPattern =
    #if FABLE_COMPILER_PYTHON
    "^(?P<key>[a-zA-Z0-9\s]+):\s*(\<c f=(?P<comment>\d+)\/\>)?$"
    #else
    "^(?<key>[a-zA-Z0-9\s]+):\s*(\<c f=(?<comment>\d+)\/\>)?$"
    #endif

[<Literal>]
let KeyValuePattern =
    #if FABLE_COMPILER_PYTHON
    "^(?P<key>[a-zA-Z0-9\s]+):\s*(?P<value>.*)$"
    #else
    "^(?<key>[a-zA-Z0-9\s]+):\s*(?<value>.*)$"
    #endif

[<Literal>]
let ValuePattern =
    #if FABLE_COMPILER_PYTHON
    "^(?P<value>([a-zA-Z0-9-\s]+|<s f=\d+\/\>))\s*?(\<c f=(?P<comment>\d+)\/\>)?$"
    #else
    "^(?<value>([a-zA-Z0-9-\s]+|<s f=\d+\/\>))\s*?(\<c f=(?<comment>\d+)\/\>)?$"
    #endif

[<Literal>]
let SequenceMinusPattern =
    #if FABLE_COMPILER_PYTHON
    "^-\s*(?P<value>.*)?$"
    #else
    "^-\s*(?<value>(.*)?$"
    #endif

[<Literal>]
let InlineSequencePattern =
    #if FABLE_COMPILER_PYTHON
    "^(?P<inlineSequence>\[.+\])\s*?(\<c f=(?P<comment>\d+)\/\>)?$"
    #else
    "^(?<inlineSequence>\[.+\])\s*?(\<c f=(?<comment>\d+)\/\>)?$"
    #endif

[<Literal>]
let SequenceOpenerPattern =
    #if FABLE_COMPILER_PYTHON
    "^\[\s*(\<c f=(?P<comment>\d+)\/\>)?$"
    #else
    "^\[\s*(\<c f=(?<comment>\d+)\/\>)?$"
    #endif

[<Literal>]
let SequenceCloserPattern =
    #if FABLE_COMPILER_PYTHON
    "^\]\s*(\<c f=(?P<comment>\d+)\/\>)?$"
    #else
    "^\]\s*(\<c f=(?<comment>\d+)\/\>)?$"
    #endif

[<Literal>]
let StringReplacementPattern =
    #if FABLE_COMPILER_PYTHON
    "\<s f=(?P<index>\d+)\/\>"
    #else
    "\<s f=(?<index>\d+)\/\>"
    #endif