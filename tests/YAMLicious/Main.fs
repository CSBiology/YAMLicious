open Fable.Pyxpecto

let all = testList "all" [
    Tests.YamlRead.Main
    Tests.EncodingCleanUp.Main
    Tests.StringCleanUp.Main
]

[<EntryPoint>]
let main argv = Pyxpecto.runTests [||] all