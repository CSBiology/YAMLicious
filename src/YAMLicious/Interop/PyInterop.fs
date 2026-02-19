[<RequireQualifiedAccessAttribute>]
module YAMLicious.PyInterop

#if FABLE_COMPILER_PYTHON
open Fable.Core.PyInterop

let isString (s:string): bool =
    emitPyStatement s """return isinstance($0, str)"""

let fromCodePoint (codePoint: int) : string =
    emitPyStatement codePoint "return chr($0)"
#endif
