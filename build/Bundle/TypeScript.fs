module Bundle.TypeScript

open SimpleExec
open BlackFox.CommandLine
open System.IO
open Utils.Path.Operators

let private clean(jspath: string) =
    if Directory.Exists jspath then
        Directory.Delete(jspath, true)
    if Directory.Exists ProjectInfo.Packages.TS then
        Directory.Delete(ProjectInfo.Packages.TS, true)

let private transpileFSharp =
    CmdLine.empty
    |> CmdLine.appendRaw "fable"
    // |> CmdLine.appendIf isWatch "--watch"
    |> CmdLine.appendRaw (ProjectInfo.Projects.Main)
    |> CmdLine.appendPrefix "-o" ProjectInfo.Packages.JS
    |> CmdLine.appendRaw "--noCache"
    |> CmdLine.appendPrefix "--fableLib" "fable-library"
    |> CmdLine.appendRaw "--noReflection"
    |> CmdLine.toString

let private transpileTypeScript(jsPath) =
    CmdLine.empty
    |> CmdLine.appendRaw "tsc"
    |> CmdLine.appendPrefix "--outDir" jsPath
    |> CmdLine.appendRaw "--skipLibCheck"
    |> CmdLine.appendRaw "--noEmit"
    |> CmdLine.appendRaw "false"
    |> CmdLine.toString


let Main(jsDir: string) = 
    clean(jsDir)
    Command.Run("dotnet", transpileFSharp)