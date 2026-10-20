[<AutoOpenAttribute>]
module Util

module Expect =
    open Fable.Pyxpecto
    open System.Collections.Generic
    open YAMLicious.YAMLiciousTypes

    /// Trims whitespace and normalizes lineendings to "\n"
    let trimEqual (actual: string) (expected: string) message =
        let a = actual.Trim().Replace("\r\n", "\n")
        let e = expected.Trim().Replace("\r\n", "\n")
        Expect.equal a e message

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
    
    let seqEqual (actual: #seq<'a>) (expected: #seq<'a>) message =

        let l1 = Seq.length actual

        Expect.equal l1 (Seq.length expected) message

        for i in 0 .. l1-1 do
            let v1 = Seq.item i actual
            let v2 = Seq.item i expected

            Expect.equal v1 v2 $"Item at index {i} is not equal."

    let yamlSemanticEqual (actual: YAMLElement) (expected: YAMLElement) message =
        let normalizedActual = YAMLElementNormalization.withoutScalarStyle actual
        let normalizedExpected = YAMLElementNormalization.withoutScalarStyle expected
        Expect.equal normalizedActual normalizedExpected message

/// The text of a scanner result: its emitted lines joined with a line break.
let tokenizedText (tokenized: YAMLicious.Persil.TokenizedYaml) =
    System.String.Join("\n", tokenized.Lines |> Array.map (fun line -> line.Text))

/// Runs the action repeatedly for a fixed time and returns the milliseconds of one run. Fable has no
/// Stopwatch, so the clock is the system time, and a window of 50 ms holds many runs of a short action.
let private sampleMilliseconds (action: unit -> unit) =
    let startTime = System.DateTime.UtcNow
    let mutable runs = 0
    let mutable elapsed = 0.0
    while elapsed < 50.0 do
        action ()
        runs <- runs + 1
        elapsed <- (System.DateTime.UtcNow - startTime).TotalMilliseconds
    elapsed / float runs

/// The lines of the small and the large input in the scaling tests. Fable Python runs far slower than
/// the other targets, so it keeps a shorter large input to hold the time of the suite down.
let private smallLines = 2000
#if FABLE_COMPILER_PYTHON
let private largeLines = 16000
#else
let private largeLines = 32000
#endif

/// Compares how the time of `operation` grows from the small to the large input with how the time of
/// `reference`, a linear pass, grows on the same inputs. A linear operation scores about 1 and a quadratic
/// one about the ratio of the two sizes. The reference absorbs the cost that a large input pays for the
/// garbage collector and the caches. Every round measures the four series one after the other, so a load
/// on the machine hits the operation and the reference alike. The first round warms up the code and is
/// dropped. The fastest of the five rounds after it keeps a pause of the garbage collector out of the result.
let expectLinear (name: string) (operation: string -> unit) (reference: string -> unit) (build: int -> string) =
    let smallInput = build smallLines
    let largeInput = build largeLines
    let actions: (unit -> unit)[] = [|
        fun () -> operation smallInput
        fun () -> operation largeInput
        fun () -> reference smallInput
        fun () -> reference largeInput
    |]
    let fastest = Array.create actions.Length infinity
    for round in 0 .. 5 do
        for i in 0 .. actions.Length - 1 do
            let milliseconds = sampleMilliseconds actions.[i]
            if round > 0 then fastest.[i] <- min fastest.[i] milliseconds
    let operationRatio = fastest.[1] / fastest.[0]
    let referenceRatio = fastest.[3] / fastest.[2]
    let normalized = operationRatio / referenceRatio
    // The limit is the geometric mean of 1 (linear) and the size ratio (quadratic), the middle on a log scale.
    let limit = sqrt (float largeLines / float smallLines)
    Fable.Pyxpecto.Expect.isTrue
        (normalized < limit)
        (sprintf "%s took %.3f ms for %d lines and %.3f ms for %d lines, a ratio of %.2f. The reference took %.3f ms and %.3f ms, a ratio of %.2f. Normalized: %.2f, limit: %.2f"
            name fastest.[0] smallLines fastest.[1] largeLines operationRatio fastest.[2] fastest.[3] referenceRatio normalized limit)
