open Fable.Pyxpecto

let all = testList "all" [
    Tests.EncodingCleanUp.Main
    Tests.StringCleanUp.Main
    Tests.CommentCleanUp.Main
    Tests.YamlRead.Main
    Tests.YamlMatch.Main
]

[<EntryPoint>]
let main argv = Pyxpecto.runTests [||] all