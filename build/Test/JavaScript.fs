module Build.Test.JavaScript

open SimpleExec
open BlackFox.CommandLine
open Utils.Path.Operators

[<LiteralAttribute>]
let private outDir = "js"
let private entryPoint = outDir </> "main.js"

let handle (args: string list) =
    let isWatch = args |> List.contains "--watch"

    let runArg =
        if isWatch then
            "--runWatch"
        else
            "--run"

    Command.Run(
        "dotnet",
        CmdLine.empty
        |> CmdLine.appendRaw "fable"
        |> CmdLine.appendPrefix "--outDir" outDir
        |> CmdLine.appendRaw "--noCache"
        |> CmdLine.appendIf isWatch "--watch"
        |> CmdLine.appendRaw runArg
        |> CmdLine.appendRaw "node"
        |> CmdLine.appendRaw entryPoint
        |> CmdLine.appendRaw "--silent"
        |> CmdLine.toString,
        workingDirectory = ProjectInfo.TestPaths.CoreDirectory
    )
