open Fable.Pyxpecto

let all =
    testList
        "all"
        [ Tests.EncodingCleanUp.Main
          Tests.StringCleanUp.Main
          Tests.CommentCleanUp.Main
          Tests.FlowToBlock.Main
          Tests.YamlGenericRead.Main
          Tests.YamlMatch.Main
          Tests.YamlRead.Main
          Tests.YamlWrite.Main
          Tests.EncoderDecoder.Main
          Tests.Directives.Main
          Tests.ReaderDocuments.Main
          Tests.Escapes.Main
          Tests.BlockScalars.Main
          Tests.AdvancedFeatures.Main ]

[<EntryPoint>]
let main argv = Pyxpecto.runTests [||] all
