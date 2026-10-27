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

    /// Fails with the printouts of both streams when they differ.
    let streamEqual (actual: YamlStream) (expected: YamlStream) (message: string) =
        if not (YamlStream.equalIgnoringSpans actual expected) then
            failwith (message + "\nActual:\n" + YamlStream.show actual + "Expected:\n" + YamlStream.show expected)

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

/// The texts that `piece` gives for 0 to `count` - 1, one behind the other.
let units (count: int) (piece: int -> string) : string =
    let sb = System.Text.StringBuilder()
    for i in 0 .. count - 1 do
        sb.Append(piece i) |> ignore
    sb.ToString()

/// The lines of the small input in the scaling tests. The only clock that Fable offers on every target is the system time, with a resolution
/// of a millisecond on JavaScript. So each target gets a small input whose single run takes at least 20 ms in each of the four tests, and the
/// counts differ with the speed of the target. A builder turns the lines into the counts of its parts. The large input has ten times as many lines.
#if FABLE_COMPILER_PYTHON
let private smallLines = 1300
#else
#if FABLE_COMPILER_JAVASCRIPT
let private smallLines = 40000
#else
let private smallLines = 56000
#endif
#endif

let private sizeRatio = 10

/// Compares how the time of `operation` grows from the small to the large input. Each input takes one run of the operation, timed once. A linear
/// operation scores about 1 and a quadratic one about the size ratio, and the limit is the geometric mean of the two, the middle on a log scale.
/// The builder runs outside the timed runs, so it can parse the input or build trees for the operation. On .NET a collection in front of
/// each run keeps the garbage and the young trees of the builders out of the time. The test prints the milliseconds, so a log shows how much
/// headroom the limit leaves.
let expectLinear (name: string) (operation: string -> unit) (build: int -> string) =
    let largeLines = smallLines * sizeRatio
    let smallInput = build smallLines
    let largeInput = build largeLines
    let time (input: string) =
#if !FABLE_COMPILER
        System.GC.Collect()
#endif
        let startTime = System.DateTime.UtcNow
        operation input
        (System.DateTime.UtcNow - startTime).TotalMilliseconds
    let small = time smallInput
    let large = time largeInput
    let normalized = large / small / float sizeRatio
    let limit = sqrt (float sizeRatio)
    let report =
        sprintf "%s took %.3f ms for %d lines and %.3f ms for %d lines, a ratio of %.2f. Normalized: %.2f, limit: %.2f"
            name small smallLines large largeLines (large / small) normalized limit
    printfn "Timing: %s" report
    Fable.Pyxpecto.Expect.isTrue (normalized < limit) report
