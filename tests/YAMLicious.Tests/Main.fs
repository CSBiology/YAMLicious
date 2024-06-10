open Fable.Pyxpecto

let all = testList "all" [
    Tests.EncodingCleanUp.Main
    Tests.StringCleanUp.Main
    Tests.CommentCleanUp.Main
    Tests.YamlGenericRead.Main
    Tests.YamlMatch.Main
    Tests.YamlRead.Main
]

[<EntryPoint>]
let main argv = Pyxpecto.runTests [||] all