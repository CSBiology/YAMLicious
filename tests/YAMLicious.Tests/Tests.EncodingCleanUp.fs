module Tests.EncodingCleanUp

open Fable.Pyxpecto

module private Examples =

    [<Literal>]
    let TestString = "
Lorem ipsum dolor sit amet, \r\nconsetetur sadipscing elitr.
"

let Main = testList "EncodingCleanUp" [
    testCase "clean up line endings" <| fun () ->
        let actual = YAMLicious.Persil.encodingCleanUp(Examples.TestString)
        let expected = "
Lorem ipsum dolor sit amet, 
consetetur sadipscing elitr.
"
        Expect.encodeEqual actual expected ""
]