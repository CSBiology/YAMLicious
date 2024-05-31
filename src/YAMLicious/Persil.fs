module YAMLicious.Persil

open System
open System.Collections.Generic
open System.Text.RegularExpressions

// Patterns:
// https://regex101.com/r/V0AkIH/1

[<Literal>]
let StringMatchPattern = 
    // (?<iscomment>#.*?)? checks if inside comment. if group is found do not parse as string
    #if FABLE_COMPILER_PYTHON
    "(?P<all>(?P<comment>#.*?)?\"(?P<stringValue>.+)\\\")"
    #endif
    #if FABLE_COMPILER_JAVASCRIPT
    "(?<all>(?<iscomment>#.*?)?\"(?<stringValue>.+)\")" // fable sets /gu for js regex (unicode, not required to escape ")
    #endif
    #if !FABLE_COMPILER
    "(?<all>(?<iscomment>#.*?)?\\\"(?<stringValue>.+)\\\")" // \\\" --> escaped \"
    #endif

[<Literal>]
let CommentMatchPattern = 
    #if FABLE_COMPILER_PYTHON
    "#(?P<comment>.*)"
    #else
    "#(?<comment>.*)"
    #endif

[<Literal>]
let NewLineChar = '\n'

let encodingCleanUp (s: string) =
    //let newLineChars = "\f\u0085\u2028\u2029" |> Array.ofSeq
    let s1 = s.Replace("\r\n", string NewLineChar)
    //let mutable index = s1.IndexOfAny(newLineChars)
    //if index = -1 then s1
    //else
    //    let sb = new System.Text.StringBuilder(s1)
    //    while index <> -1 do
    //        sb.Chars(index) <- replacementChar
    //        index <- s1.IndexOfAny(newLineChars, index + 1)
    //    sb.ToString()
    s1

let stringCleanUp (dict: Dictionary<int, string>) (s: string) =
    let mutable n = 0
    let regex = Regex(StringMatchPattern)
    printfn "StringCleanUp"
    let matcheval = new MatchEvaluator(fun m ->
        match m.Groups.["iscomment"].Success with
        | true ->
            m.Groups.["all"].Value
        | false ->
            let v = m.Groups.["stringValue"].Value
            let currentN = n
            n <- n + 1
            dict.Add(currentN, v)
            sprintf "<s f=%i/>" currentN
    )
    regex.Replace(s, matcheval)

let commentCleanUp (dict: Dictionary<int, string>) (s: string) =
    let mutable n = 0
    let regex = Regex(CommentMatchPattern)
    let matcheval = new MatchEvaluator(fun m ->
        let v = m.Groups.["comment"].Value
        let currentN = n
        n <- n + 1
        dict.Add(currentN, v)
        sprintf "<c f=%i/>" currentN
    )
    regex.Replace(s, matcheval)

let cut(yamlString: string) =
    yamlString.Split([|NewLineChar|], StringSplitOptions.RemoveEmptyEntries)

let pipeline (yamlString: string) =
    let stringMap = new Dictionary<int, string>()
    let commentMap = new Dictionary<int, string>()
    let lines =
        encodingCleanUp yamlString
        |> stringCleanUp stringMap
        |> commentCleanUp commentMap
        |> cut
    {|StringMap = stringMap; CommentMap = commentMap; Lines = lines|}