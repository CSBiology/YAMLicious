open Fable.Pyxpecto

let all =
    testList
        "all"
        [ Tests.Model.Main
          Tests.CoreSchema.Main
          Tests.Compare.Main
          Tests.Projection.Main ]

[<EntryPoint>]
let main argv = Pyxpecto.runTests [||] all
