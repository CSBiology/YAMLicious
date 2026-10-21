open Fable.Pyxpecto

let all =
    testList
        "all"
        [ Tests.Model.Main
          Tests.CoreSchema.Main
          Tests.Compare.Main
          Tests.Validate.Main
          Tests.Projection.Main
          Tests.Lexer.Main
          Tests.Scalars.Main ]

[<EntryPoint>]
let main argv = Pyxpecto.runTests [||] all
