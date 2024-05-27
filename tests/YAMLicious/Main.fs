open Fable.Pyxpecto

let tests_test = testList "ensure" [
    testCase "testing1" <| fun _ ->
        Expect.equal 1 1 "Should be equal"
    testCase "Ensure ref" <| fun _ ->
        let _ = YAMLicious.Say.hello "World"
        Expect.pass()
]

let all = testList "all" [
    tests_test
]

[<EntryPoint>]
let main argv = Pyxpecto.runTests [||] all