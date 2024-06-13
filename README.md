# YAMLicious

YAMLicious is a small YAML reader/writer inspired by [Thoth.Json](https://github.com/thoth-org/Thoth.Json) syntax!

## Code examples

### F# Model

```fsharp
type Package = {
    Name: string
    Version: string option
}

type ArcValidation = {
    ArcSpecificationVersion: string
    Packages: Package list
}   
```

### YAML

```yaml
arc_specification: 2.0.0-draft
validation_packages:
  - name: package1
    version: 1.0.0
  - name: package2
    version: 2.0.0
  - name: package3
```

### Decode (from YAML)

```fsharp
// from yaml
let Example1 = 
    "arc_specification: 2.0.0-draft
validation_packages:
    - name: package1
    version: 1.0.0
    - name: package2
    version: 2.0.0
    - name: package3"

let packageEncoder : (YAMLicious.YAMLElement -> Package) = 
    Decode.object (fun get ->
        {
            Name = get.Required.Field "name" Decode.string
            Version = get.Optional.Field "version" Decode.string
        }
    )

let arcValidationDecoder : (YAMLicious.YAMLElement -> ArcValidation) =
    Decode.object (fun get ->
        {
            ArcSpecificationVersion = get.Required.Field "arc_specification" Decode.string
            Packages = get.Required.Field "validation_packages" (Decode.list packageEncoder)
        }
    )

let actual : ArcValidation = Examples.ValidationPackageTypes.string |> Decode.read |> arcValidationDecoder

```

### Encode (to YAML)

```fsharp
// to yaml
let Example2 = {
    ArcSpecificationVersion = "2.0.0-draft"
    Packages = [
        { Name = "package1"; Version = Some "1.0.0" }
        { Name = "package2"; Version = Some "2.0.0" }
        { Name = "package3"; Version = None }
    ]
}

let packageEncoder (pack: Package) = 
    [
        "name", Encode.string pack.Name
        Encode.tryInclude "version" Encode.string pack.Version 
    ]
    |> Encode.choose
    |> Encode.object

let arcValidationEncoder (arc: ArcValidation) = 
    Encode.object [
        "arc_specification", Encode.string arc.ArcSpecificationVersion
        "validation_packages", Encode.list packageEncoder arc.Packages
    ]

let actual : string = Encode.write 2 (arcValidationEncoder Example2)
```


---
## Local Development

### Requirements

Because this library targets multiple programming languages we need to support all of them:

- [nodejs and npm](https://nodejs.org/en/download)
    - verify with `node --version` (Tested with v20.10.0)
    - verify with `npm --version` (Tested with v9.2.0)
- [.NET SDK](https://dotnet.microsoft.com/en-us/download)
    - verify with `dotnet --version` (Tested with 8.0.205)
- [Python](https://www.python.org/downloads/)
    - verify with `py --version` (Tested with 3.11.9, known to work only for >=3.11)

### Setup

This needs to be done on a fresh download once. Paths for python venv executable might be different depending on the OS.

1. `dotnet tool restore`, Restore .NET tools (fable)
2. `npm i`, install js dependencies
3. `py -m venv ./.venv`, setup python virtual environment
4. `.\.venv\Scripts\Activate.ps1`, activate python virtual environment
5. install python dependencies
    1. `python -m pip install -U pip setuptools`
    2. `python -m pip install poetry`
    3. `python -m poetry install --no-root`

### Testing

First activate python virtual environment (`.\.venv\Scripts\Activate.ps1`).

`.\build.cmd test`

*or specify target*

`.\build.cmd test [f#, c#, js [native], py [native]]`

### Publish

Requires API keys for Nuget and PyPi. 

The following command will run all tests, bundle and then start publishing!

`.\build.cmd publish pipeline`

*or only publish specific targets, without test and bundle*

`.\build.cmd publish [npm, pypi, nuget]`
