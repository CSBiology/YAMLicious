module Tests.CommentCleanUp

open Fable.Pyxpecto
open YAMLicious
open System.Collections.Generic

let Main = testList "CommentCleanUp" [
    testCase "full line" <| fun _ ->
        let dict = Dictionary<int, string>()
        let s = """
# This is a line comment
"""
        let actual = Persil.encodingCleanUp s |> Persil.commentCleanUp dict
        let expected = """
<c f=0/>
"""
        let expectedDict = Dictionary<int, string>(Map [0, " This is a line comment"])
        Expect.encodeEqual actual expected ""
        Expect.dictEqual dict expectedDict ""
    testCase "full line" <| fun _ ->
        let dict = Dictionary<int, string>()
        let s = """
My Key: # This is a comment
  My Value1
  <s f=0/>
  My Value3 # :::: "This is also a comment"
"""
        let actual = Persil.encodingCleanUp s |> Persil.commentCleanUp dict
        let expected = """
My Key: <c f=0/>
  My Value1
  <s f=0/>
  My Value3 <c f=1/>
"""
        let expectedDict = Dictionary<int, string>(Map [0, " This is a comment"; 1, " :::: \"This is also a comment\""])
        Expect.encodeEqual actual expected ""
        Expect.dictEqual dict expectedDict ""
]

