module Build.Test.Python

open SimpleExec
open BlackFox.CommandLine
open Utils.Path.Operators

let private outDir = ProjectInfo.TestPaths.CoreDirectory </> "py"
let private entryPoint = outDir </> "main.py"

let python = "uv run python"

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
        |> CmdLine.appendRaw ProjectInfo.TestPaths.CoreDirectory
        |> CmdLine.appendPrefix "--outDir" outDir
        |> CmdLine.appendPrefix "--lang" "python"
        |> CmdLine.appendRaw "--noCache"
        |> CmdLine.appendIf isWatch "--watch"
        |> CmdLine.appendRaw runArg
        |> CmdLine.appendRaw python
        |> CmdLine.appendRaw entryPoint
        |> CmdLine.appendRaw "--silent"
        |> CmdLine.toString
    )
