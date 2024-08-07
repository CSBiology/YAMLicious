﻿module Build.Main

// This is a basic help message, as the CLI parser is not a "real" CLI parser
// For now, it is enough as this is just a dev tool
let printHelp () =
    let helpText =
        """
Usage: dotnet run <command> [<args>]

Available commands:
    test                            Run the main tests suite
        Subcommands:
            f#                      Run f# tests in `Core`
            js                      Run the js transpiled tests
            js native               Run the native mocha tests
            py                      Run the py transpiled tests
            py native               Run the native pytest tests
            fable                   Run all tests in f#, js and py (Fable transpiled/not native only)

    bundle
        Subcommands:
            ts                      Bundle the TypeScript package
            py                      Bundle the Python package
            f#                      Bundle the F# package

    Publish
        Subcommands:
            pipeline                Run all tests, bundle all packages and publish all packages
            npm                     Publish the npm package
            pypi                    Publish the pypi package
            nuget                   Publish the nuget package

    index
        Subcommands:
            js                      Generate the js index file
            py                      Generate the py index file
"""

    printfn $"%s{helpText}"

[<EntryPoint>]
let main argv =
    let argv = argv |> Array.map (fun x -> x.ToLower()) |> Array.toList

    match argv with
    | "test" :: args ->
        match args with
        | "f#" :: args -> Test.FSharp.handle args
        | "js" :: "native" :: args -> 
            Test.JavaScript.handleNative args
        | "js" :: args -> Test.JavaScript.handle args
        | "py" :: "native" :: args -> Test.Python.handleNative args
        | "py" :: args -> Test.Python.handle args
        | "fable" :: args ->
            Test.FSharp.handle args
            Test.JavaScript.handle args
            Test.Python.handle []
        | [] | "all" :: _ -> 
            Test.FSharp.handle []
            Test.JavaScript.handle []
            Test.Python.handle []
            //Test.Python.handleNative args
            //Test.JavaScript.handleNative args
        | _ -> printHelp ()
    | "bundle" :: args ->
        match args with
        | "ts" :: _ -> 
            Bundle.TypeScript.Main(ProjectInfo.Packages.JS)
        | "py" :: _ ->
            Bundle.Python.Main(ProjectInfo.Packages.PY)
        | "f#" :: _ ->
            Bundle.Net.Main(ProjectInfo.Projects.Main, ProjectInfo.Packages.FSHARP)
        | _ -> printHelp ()
    | "publish" :: args ->
        match args with
        | "pipeline" :: _ ->
            // test
            Test.FSharp.handle []
            Test.JavaScript.handle []
            Test.Python.handle []
            Test.Python.handleNative args
            Test.JavaScript.handleNative args
            // bundle
            Bundle.TypeScript.Main(ProjectInfo.Packages.JS)
            Bundle.Python.Main(ProjectInfo.Packages.PY)
            Bundle.Net.Main(ProjectInfo.Projects.Main, ProjectInfo.Packages.FSHARP)
            // publish
            Publish.Npm.Main()
            Publish.PyPi.Main()
            Publish.Nuget.Main(ProjectInfo.Packages.FSHARP)
            Publish.Nuget.Main(ProjectInfo.Packages.CSHARP)
        | "npm" :: _ -> 
            Publish.Npm.Main()
        | "pypi" :: _ -> 
            Publish.PyPi.Main()
        | "nuget" :: _ -> 
            Test.FSharp.handle []
            // bundle
            Bundle.Net.Main(ProjectInfo.Projects.Main, ProjectInfo.Packages.FSHARP)
            Publish.Nuget.Main(ProjectInfo.Packages.FSHARP)
            //Publish.Nuget.Main(ProjectInfo.Packages.CSHARP)
        | _ -> printHelp ()
    | "codegen" :: _ ->
        printfn "STARTING CODEGEN..."
        printfn "ENDING CODEGEN..."
    | "index" :: args ->
        match args with
        | "js" :: _ -> 
            Index.JS.generate (ProjectInfo.TestPaths.JSNativeDirectory + ProjectInfo.ProjectName) false
        | "py" :: _ -> 
            Index.PY.generate (ProjectInfo.TestPaths.PyNativeDirectory + ProjectInfo.ProjectName) "index.py"
        | _ -> printHelp ()
    | _ -> printHelp ()

    0