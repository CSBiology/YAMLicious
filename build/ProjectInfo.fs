module ProjectInfo

open System.IO
open Utils
open Utils.Path
open Utils.Path.Operators

let root = Path.Resolve()

let Owner = "CSBiology"

[<Literal>]
let ProjectName = "YAMLicious"

/// This might require adjustment. If you adjust this also update pyproject.toml!
[<Literal>]
let ProjectNamePython = "YAMLicious"

/// This might require adjustment. If you adjust this also update package.json!
[<Literal>]
let ProjectNameJavaScript = "YAMLicious"

[<Literal>]
let Version = "1.0.0-alpha.2"

[<Literal>]
let PyprojectTOML = "pyproject.toml"

[<Literal>]
let PackageJSON = "package.json"

[<Literal>]
let README = "README.md"



module Keys =
    
    let [<Literal>] PyPi = "PYPI_KEY"
    let [<Literal>] NPM = "NPM_KEY" // not used currently
    let [<Literal>] Nuget = "NUGET_KEY"



module TestPaths =

    [<Literal>]
    let BaseDirectory = "tests"

    let CoreDirectory = BaseDirectory </> $"{ProjectName}.Tests"

    let JSNativeDirectory = BaseDirectory </> $"{ProjectName}.JavaScript.Tests"

    let PyNativeDirectory = BaseDirectory </> $"{ProjectName}.Python.Tests"

module Packages =
    [<Literal>]
    let PackageFolder = "./dist"
    let FSHARP = Path.Resolve(PackageFolder, "fsharp")
    let CSHARP = Path.Resolve(PackageFolder, "csharp")
    let JS = Path.Resolve(PackageFolder, "js")
    let TS = Path.Resolve(PackageFolder, "ts")
    let PY = Path.Resolve(PackageFolder, "py")


module Projects =

    let MainDir = $"src" </> ProjectName

    let Main = MainDir </> $"{ProjectName}.fsproj"
    let Tests = TestPaths.CoreDirectory </>  $"{ProjectName}.Tests.fsproj"


let getEnvVar key = System.Environment.GetEnvironmentVariable(key, System.EnvironmentVariableTarget.User)