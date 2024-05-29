namespace YAMLicious

[<RequireQualifiedAccess>]
type YAMLElement =
    | Mapping of string * YAMLElement
    | String of string
    | Sequence of YAMLElement list
    | Comment of string

type YAMLAST =
    | Line of string
    | Level of YAMLAST list
    | Intendation of YAMLAST list

module Persil =

    open System
    open System.Collections.Generic
    open System.Text.RegularExpressions

    // Patterns:
    // https://regex101.com/r/V0AkIH/1

    [<Literal>]
    let StringMatchPattern = 
        #if FABLE_COMPILER_PYTHON
        "\"(?P<stringValue>.+)\\\""
        #endif
        #if FABLE_COMPILER_JAVASCRIPT
        "\"(?<stringValue>.+)\"" // fable sets /gu for js regex (unicode, not required to escape ")
        #endif
        #if !FABLE_COMPILER
        "\\\"(?<stringValue>.+)\\\"" // \\\" --> escaped \"
        #endif

    [<Literal>]
    let CommentMatchPattern = 
        #if FABLE_COMPILER_PYTHON
        "#(?P<comment>.*)$"
        #else
        "#(?<comment>.*)$"
        #endif

    let encodingCleanUp (s: string) =
        let replacementChar = '\n'
        //let newLineChars = "\f\u0085\u2028\u2029" |> Array.ofSeq
        let s1 = s.Replace("\r\n", string replacementChar)
        //let mutable index = s1.IndexOfAny(newLineChars)
        //if index = -1 then s1
        //else
        //    let sb = new System.Text.StringBuilder(s1)
        //    while index <> -1 do
        //        sb.Chars(index) <- replacementChar
        //        index <- s1.IndexOfAny(newLineChars, index + 1)
        //    sb.ToString()
        s1

    let stringCleanUp (s: string) =
        let mutable n = 0
        let dict = Dictionary<int, string>()
        let regex = Regex(StringMatchPattern)
        let matcheval = new MatchEvaluator(fun m ->
            let v = m.Groups.["stringValue"].Value
            let currentN = n
            n <- n + 1
            dict.Add(currentN, v)
            sprintf "</%i>" currentN
        )
        {|Content = regex.Replace(s, matcheval); StringMap = dict|}

    let commentCleanUp (s: string) =
        let regex = Regex(CommentMatchPattern)
        //let matcheval = new MatchEvaluator(fun m ->
        //    let v = m.Groups.["comment"].Value
        //    Comment v
        //)
        //regex.Replace(s, matcheval)
        0

module TestCases =

    [<Literal>]
    let KeyValue = """
My Key: My Value    
"""

    [<Literal>]
    let String = """
My Value
"""

    [<Literal>]
    let Sequence = """
- My Value 1
- My Value 2
- My Value 3
"""

    [<Literal>]
    let SequenceObjects = """
- My Value 1
  My Value 2
- My Value 3
"""

    [<Literal>]
    let SequenceImplicit = """
My Key:
  My Value1
  My Value2
  My Value3
"""
    [<Literal>]
    let SequenceMappings = """
-
  My Key1: My Value1
  My Key2: My Value2
  My Key3: My Value3
-
  My Key4: My Value4
  My Key5: My Value5
  My Key6: My Value6
"""

    [<Literal>]
    let SequenceOfSequences = """
- [v1, v2, v3]
- [v4, v5, v6]
- [v7, v8, v9]
""" 

    [<Literal>]
    let SequenceOfSequencesOfSequences = """
- 
  [v1, v2, v3]
- 
  [v4, v5, v6]
- 
  [v7, v8, v9]
"""
 
    [<Literal>]
    let SequenceSquare = """
[
  v1,
  v2,
  v3
]
"""
    
    let SequenceMappingsAST = 
        Level [
            Line "-"
            Intendation [
                Line "My Key1: My Value1"
                Line "My Key2: My Value2"
                Line "My Key3: My Value3"
            ]
            Line "-"
            Intendation [
                Line "My Key4: My Value4"
                Line "My Key5: My Value5"
                Line "My Key6: My Value6"
            ]
        ]

    [<Literal>]
    let LineComment = """
# This is a line comment    
"""

    [<Literal>]
    let InlineComment = """
My Key: # This is a comment
  My Value1 
  My Value2
  My Value3 # :::: This is also a comment
"""

    [<Literal>]
    let StringReplace = """
My Key: "[{Special character place # |}"
"""

    let StringReplaceClean = """
My Key: </1>
"""

    [<Literal>]
    let StringReplaceWithComment = """
My Key: "[{Special character place # |}" # A # in string is allowed!
"""

    [<Literal>]
    let StringReplaceWithCommentHashs = """
My Key: "[{Special character place # |}" ### A # in string is allowed!
"""