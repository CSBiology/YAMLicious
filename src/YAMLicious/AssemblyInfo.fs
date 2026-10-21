namespace YAMLicious

open System.Runtime.CompilerServices

// The lexer types are internal. The tests drive the lexer directly, so they see them.
[<assembly: InternalsVisibleTo("YAMLicious.Tests")>]
do ()
