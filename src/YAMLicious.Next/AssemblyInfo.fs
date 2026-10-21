namespace YAMLicious

open System.Runtime.CompilerServices

// The lexer types are internal. The tests of the candidate drive the lexer directly, so they see them.
[<assembly: InternalsVisibleTo("YAMLicious.Next.Tests")>]
do ()
