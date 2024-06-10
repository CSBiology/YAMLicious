module Tests.StringCleanUp

open Fable.Pyxpecto
open System.Collections.Generic
open YAMLicious

module private Examples =

    [<Literal>]
    let StringReplace = """
My Key: "[{Special character place # |}" # A # in string is allowed!
"""

    [<Literal>]
    let StringsReplace = """
My Key: "[{Special character place # |}" ### A # in string is allowed!
My Key2: "3" ### A # in string is allowed!
My Key2: "Lorem ipsum dolor et" ### A # in string is allowed!
My Key2: "Ehhhhh makarena" ### A # in string is allowed!
"""

let Main = testList "StringCleanUp" [
    testCase "single special char" <| fun () ->
        let stringMap = new Dictionary<int, string>()
        let actual = Persil.stringCleanUp stringMap Examples.StringReplace
        let expected = """
My Key: <s f=0/> # A # in string is allowed!
"""
        let expectedDict = Dictionary(Map[|0, "[{Special character place # |}"|])
        Expect.equal expected actual "content"
        Expect.dictEqual stringMap expectedDict "map"

    testCase "multiple comments" <| fun () ->
        let stringMap = new Dictionary<int, string>()
        let actual = Persil.stringCleanUp stringMap Examples.StringsReplace
        let expected = """
My Key: <s f=0/> ### A # in string is allowed!
My Key2: <s f=1/> ### A # in string is allowed!
My Key2: <s f=2/> ### A # in string is allowed!
My Key2: <s f=3/> ### A # in string is allowed!
"""
        let expectedDict = Dictionary(Map [|
            0, "[{Special character place # |}"; 
            1, "3"; 
            2, "Lorem ipsum dolor et"; 
            3, "Ehhhhh makarena"
        |])
        Expect.equal actual expected "content"
        Expect.dictEqual stringMap expectedDict "map"
]
