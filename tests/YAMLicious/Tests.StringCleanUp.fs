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
        let actual = Persil.stringCleanUp(Examples.StringReplace)
        let expected = """
My Key: </0> # A # in string is allowed!
"""
        let expectedDict = Dictionary(Map[|0, "[{Special character place # |}"|])
        let actualDict = actual.StringMap
        Expect.equal expected actual.Content "content"
        Expect.dictEqual actual.StringMap expectedDict "map"

    testCase "multiple comments" <| fun () ->
        let actual = Persil.stringCleanUp(Examples.StringsReplace)
        let expected = """
My Key: </0> ### A # in string is allowed!
My Key2: </1> ### A # in string is allowed!
My Key2: </2> ### A # in string is allowed!
My Key2: </3> ### A # in string is allowed!
"""
        let expectedDict = Dictionary(Map [|
            0, "[{Special character place # |}"; 
            1, "3"; 
            2, "Lorem ipsum dolor et"; 
            3, "Ehhhhh makarena"
        |])
        Expect.equal actual.Content expected "content"
        Expect.dictEqual actual.StringMap expectedDict "map"
]
