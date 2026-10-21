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
          Tests.Scalars.Main
          Tests.Parser.Main
          Tests.Comments.Main
          Tests.Documents.Main
          Tests.Flow.Main
          Tests.Keys.Main
          Tests.Aliases.Main
          Tests.Decode.Main
          Tests.Encode.Main
          Tests.Reader.Main
          Tests.Writer.Main
          Tests.RoundTrip.Main ]

[<EntryPoint>]
let main argv = Pyxpecto.runTests [||] all
