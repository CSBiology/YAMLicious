[<RequireQualifiedAccessAttribute>]
module YAMLicious.PyInterop

#if FABLE_COMPILER_PYTHON
open Fable.Core.PyInterop

/// The string of the one code point `codePoint`. Python strings index by code point, so a code point above
/// U+FFFF is one character there.
let fromCodePoint (codePoint: int) : string =
    emitPyStatement codePoint "return chr($0)"
#endif
