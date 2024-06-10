module YAMLicious.PatternMatcher

open System
open System.Collections.Generic
open System.Text.RegularExpressions
open YAMLicious.Persil
open YAMLicious.AST

[<Literal>]
let CommentPattern =
    #if FABLE_COMPILER_PYTHON
    "\<c f=(?P<comment>\d+)\/\>"
    #endif
    #if FABLE_COMPILER_JAVASCRIPT || FABLE_COMPILER_TYPESCRIPT
    "<c f=(?<comment>\d+)\/>"
    #endif
    #if !FABLE_COMPILER
    "\<c f=(?<comment>\d+)\/\>"
    #endif

let KeyPattern =
    #if FABLE_COMPILER_PYTHON
    $"^(?P<key>[a-zA-Z0-9\s]+):\s*({CommentPattern})?$"
    #else
    $"^(?<key>[a-zA-Z0-9\s]+):\s*({CommentPattern})?$"
    #endif

[<Literal>]
let KeyValuePattern =
    #if FABLE_COMPILER_PYTHON
    "^(?P<key>[a-zA-Z0-9\s]+):\s*(?P<value>.*)$"
    #else
    "^(?<key>[a-zA-Z0-9\s]+):\s*(?<value>.*)$"
    #endif

let LineCommentPattern =
    $"^{CommentPattern}$"

let ValuePattern =
    #if FABLE_COMPILER_PYTHON
    $"^(?P<value>.*?)\s*?({CommentPattern})?$"
    #else
    $"^(?<value>.*?)\s*?({CommentPattern})?$"
    #endif

[<Literal>]
let SequenceMinusPattern =
    #if FABLE_COMPILER_PYTHON
    "^-\s*(?P<value>.*)?$"
    #else
    "^-\s*(?<value>.*)?$"
    #endif

let InlineSequencePattern =
    #if FABLE_COMPILER_PYTHON
    $"^\[(?P<inlineSequence>.+)\]\s*?({CommentPattern})?$"
    #else
    $"^\[(?<inlineSequence>.+)\]\s*?({CommentPattern})?$"
    #endif

let SequenceOpenerPattern =
    #if FABLE_COMPILER_PYTHON
    $"^\[\s*({CommentPattern})?$"
    #else
    $"^\[\s*({CommentPattern})?$"
    #endif

let SequenceCloserPattern =
    #if FABLE_COMPILER_PYTHON
    $"^\]\s*({CommentPattern})?$"
    #else
    $"^\]\s*({CommentPattern})?$"
    #endif

[<Literal>]
let StringReplacementPattern =
    #if FABLE_COMPILER_PYTHON
    "\<s f=(?P<index>\d+)\/\>"
    #else
    "\<s f=(?<index>\d+)\/\>"
    #endif