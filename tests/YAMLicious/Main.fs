open Fable.Pyxpecto

let all = testList "all" [
    Tests.YamlRead.Main
    Tests.EncodingCleanUp.Main
    Tests.StringCleanUp.Main
    Tests.CommentCleanUp.Main
]

[<EntryPoint>]
let main argv = Pyxpecto.runTests [||] all