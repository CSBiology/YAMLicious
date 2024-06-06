[<AutoOpenAttribute>]
module Util

module Expect =
    open Fable.Pyxpecto
    open System.Collections.Generic

    /// This is necessary because writing multiline string in vs community for the "expected" value will use \r\n.
    let encodeEqual (actual: string) (expected: string) message =
        let a = actual
        let e = expected.Replace("\r\n", "\n")
        Expect.equal a e message

    let dictEqual (actual: Dictionary<'a,'b>) (expected: Dictionary<'a,'b>) message =
        Expect.equal actual.Count expected.Count message
        let r1 = Array.zip (Array.ofSeq actual.Keys) (Array.ofSeq actual.Values)
        let r2 = Array.zip (Array.ofSeq expected.Keys) (Array.ofSeq expected.Values)
        let c = r1.Length
        for i in 0..c-1 do
            let (k1, v1) = r1.[i]
            let (k2, v2) = r2.[i]
            Expect.equal k1 k2 message
            Expect.equal v1 v2 message