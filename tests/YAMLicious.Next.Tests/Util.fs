[<AutoOpenAttribute>]
module Util

open YAMLicious

module Expect =
    open Fable.Pyxpecto

    /// Fails with the printouts of both trees when they differ. The printouts are built only on failure, so
    /// the check stays cheap on a deep tree.
    let nodeEqual (actual: YamlNode) (expected: YamlNode) (message: string) =
        if not (YamlNode.equalIgnoringSpans actual expected) then
            failwith (message + "\nActual:\n" + YamlNode.show actual + "Expected:\n" + YamlNode.show expected)

    /// Fails with the printout of the tree when the two trees are equal.
    let nodeNotEqual (actual: YamlNode) (expected: YamlNode) (message: string) =
        if YamlNode.equalIgnoringSpans actual expected then
            failwith (message + "\nBoth trees are:\n" + YamlNode.show actual)

    /// True when two floats have the same bits, so -0.0 differs from 0.0 and NaN equals NaN.
    let sameFloat (actual: float) (expected: float) (message: string) =
        let same =
            if System.Double.IsNaN expected then
                System.Double.IsNaN actual
            else
                System.BitConverter.DoubleToInt64Bits actual = System.BitConverter.DoubleToInt64Bits expected
        Expect.isTrue same (message + ": " + string actual + " is not " + string expected)

/// The message of the exception that the action throws, or an empty string when it succeeds.
let errorOf (action: unit -> unit) =
    try
        action ()
        ""
    with e ->
        e.Message

/// A comment with the given text, as in `comment " c"` for `# c`.
let comment (text: string) = YamlComment.create text

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
/// The test prints the milliseconds, so a log shows how much headroom the limit leaves.
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
    let report =
        sprintf "%s took %.3f ms for %d lines and %.3f ms for %d lines, a ratio of %.2f. The reference took %.3f ms and %.3f ms, a ratio of %.2f. Normalized: %.2f, limit: %.2f"
            name fastest.[0] smallLines fastest.[1] largeLines operationRatio fastest.[2] fastest.[3] referenceRatio normalized limit
    printfn "Timing: %s" report
    Fable.Pyxpecto.Expect.isTrue (normalized < limit) report
