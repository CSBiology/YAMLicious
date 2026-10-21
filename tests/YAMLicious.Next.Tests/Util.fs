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
