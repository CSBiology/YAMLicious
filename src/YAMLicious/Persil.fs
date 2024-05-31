module YAMLicious.Persil

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