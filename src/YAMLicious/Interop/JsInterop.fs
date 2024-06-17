[<RequireQualifiedAccessAttribute>]
module YAMLicious.JsInterop

#if FABLE_COMPILER_JAVASCRIPT || FABLE_COMPILER_TYPESCRIPT
open Fable.Core.JsInterop

let isInt (s:string): bool =
    emitJsStatement (s) """if (typeof $0 != "string") return false // we only process strings!  
                return !isNaN($0) && // use type coercion to parse the _entirety_ of the string (`parseFloat` alone does not do this)...
                       !isNaN(parseFloat($0)) // ...and ensure strings of whitespace fail"""

let isString (s:string): bool =
    emitJsStatement s """return typeof $0 === 'string' || value instanceof String"""
#endif
