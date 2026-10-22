# YAMLicious 

![NuGet Version](https://img.shields.io/nuget/v/YAMLicious?style=for-the-badge) ![NuGet Downloads](https://img.shields.io/nuget/dt/YAMLicious?style=for-the-badge)

YAMLicious is a YAML 1.2.2 reader and writer for F# with decoders and encoders in the style of [Thoth.Json](https://github.com/thoth-org/Thoth.Json). It runs on .NET, and [Fable](https://fable.io) compiles it to JavaScript and Python.

The reader builds a tree of nodes that keeps comments, blank lines, scalar and collection styles, tags, anchors and source spans. The writer prints the tree again with every comment and blank line in place.

> [!IMPORTANT]
> If you want to use YAMLicious in Python, please install [`fable-library`](https://pypi.org/project/fable-library/#history) via python dependency manager of your choice. YAMLicious depends on fable-library for core functionality.

## Installation

```
dotnet add package YAMLicious
```

The package carries the F# sources in its `fable/` folder, so a Fable project compiles them for JavaScript or Python.

## Usage

### Read a text

`Reader.readRoot` parses a text with one document and returns the root node. `Reader.read` returns the whole `YamlDocument`, and `Reader.readDocuments` returns a `YamlStream` for a text with any number of documents. The `tryRead` and `tryReadDocuments` variants return a `Result` with a `YamlError`, and the functions without `try` raise `YamlParseException`.

```fsharp
open YAMLicious

type Package = { Name: string; Version: string option }

type DocValidation =
    { FormatDefinitionVersion: string
      Packages: Package list }

let yaml =
    """# validation packages of a spec
arc_specification: 2.0.0-draft
validation_packages:
  - name: package1
    version: 1.0.0
  - name: package2 # needs a review
    version: 2.0.0
  - name: package3
"""

let root: YamlNode = Reader.readRoot yaml
```

### Decode

A `Decoder<'T>` is a function from `YamlNode` to `'T`. `Decode.object` reads a mapping through getters. `Decode.list` and `Decode.option` wrap another decoder.

```fsharp
let packageDecoder: Decoder<Package> =
    Decode.object (fun get ->
        { Name = get.Required.Field "name" Decode.string
          Version = get.Optional.Field "version" Decode.string })

let docValidationDecoder: Decoder<DocValidation> =
    Decode.object (fun get ->
        { FormatDefinitionVersion = get.Required.Field "arc_specification" Decode.string
          Packages = get.Required.Field "validation_packages" (Decode.list packageDecoder) })

let docValidation: DocValidation = docValidationDecoder root

printfn "%A" (docValidation.Packages |> List.map (fun package -> package.Name, package.Version))
```

```
[("package1", Some "1.0.0"); ("package2", Some "2.0.0"); ("package3", None)]
```

The scalar decoders follow the YAML 1.2 core schema. A plain `1` is an int and a quoted `"1"` is a string. `yes` is a string too, and `Decode.string` reads the text of any scalar.

### Errors with positions

Every `YamlError` has a stable `Code` and the `Span` of the offending text, and its message names the line. A decoder raises `YamlDecodeException` with a `DecodeError` that names the path to the failing node and its span. `Decode.tryDecode` returns the error as a value.

```fsharp
match Reader.tryRead "name: \"package1" with
| Ok _ -> ()
| Error error ->
    printfn "%s at %d:%d" (YamlErrorCode.name error.Code) error.Span.Start.Line error.Span.Start.Column

let withoutName = Reader.readRoot "arc_specification: 2.0.0-draft\nvalidation_packages:\n  - version: 1.0.0\n"

match Decode.tryDecode docValidationDecoder withoutName with
| Ok _ -> ()
| Error error -> printfn "%s" (DecodeError.text error)
```

```
unclosed-quote at 1:7
At validation_packages[0]: The mapping has no field name. (line 3, column 5)
```

### Encode and write

The encoders build nodes. `Encode.optionalField` returns `None` for a missing value, so combine the fields with `List.choose id`. `Writer.writeNode` turns a node into text.

```fsharp
let packageEncoder (package: Package) : YamlNode =
    [ Some("name", Encode.string package.Name)
      Encode.optionalField "version" Encode.string package.Version ]
    |> List.choose id
    |> Encode.object

let docValidationEncoder (doc: DocValidation) : YamlNode =
    Encode.object
        [ "arc_specification", Encode.string doc.FormatDefinitionVersion
          "validation_packages", Encode.list packageEncoder doc.Packages ]

let text: string = Writer.writeNode WriterOptions.defaults (docValidationEncoder docValidation)

printf "%s" text
```

```yaml
arc_specification: 2.0.0-draft
validation_packages:
  - name: package1
    version: 1.0.0
  - name: package2
    version: 2.0.0
  - name: package3
```

`WriterOptions.Indent` is the number of spaces for one level and is 2 by default. `NewLine` ends every line and is `"\n"` by default, and `Styles` is the style policy that the next section describes. `Writer.write` writes a `YamlDocument` and `Writer.writeDocuments` writes a `YamlStream`.

### Comments survive a round trip

A comment sits in one slot of the tree, so a document that you read and write again keeps its comments and blank lines.

```fsharp
let document: YamlDocument = Reader.read yaml

printfn "%b" (Writer.write WriterOptions.defaults document = yaml)
```

```
true
```

A scalar over several lines keeps its lines as well, because the tree holds the line breaks that folding turned into the text.

```fsharp
let wrapped = "description: >\n  Reads the input table and writes\n  one summary for each group.\n"

printfn "%b" (Writer.write WriterOptions.defaults (Reader.read wrapped) = wrapped)
```

```
true
```

A program that changes the text of such a scalar calls `YamlNode.withScalarValue`. The lines stay while each boundary still stands at the space or the line feeds that it replaced and the text next to a fold is no blank. Otherwise the writer writes the normal spelling.

To add a comment to a tree that you build, use `YamlNode.withTrailingComment` for a comment behind a value. The text of a comment is everything behind the `#`.

```fsharp
let note = YamlNode.withTrailingComment (YamlComment.create " checked") (YamlNode.plain "1.0.0")

printf "%s" (Writer.writeNode WriterOptions.defaults (Encode.object [ ("version", note) ]))
```

```yaml
version: 1.0.0 # checked
```

### Scalar styles

A node stores the style of its scalars and collections. A scalar that you construct states a preference, and the writer quotes a text that cannot stand plain. Examples are a text that starts with an indicator and a text that holds `: ` or ` #`. `Encode.string` double-quotes a text that the core schema reads as null, a bool, an int or a float.

The `Styles` option of `WriterOptions` decides what the writer does with the stored styles.

| `StylePolicy` | Behavior |
|---|---|
| `PreferStored` | Keeps a stored style that can express the value and picks the nearest valid style otherwise. This is the default. |
| `RequireStored` | Raises `YamlWriteException` where a stored style cannot express the value. |
| `Canonical` | Ignores the stored styles. Scalars are plain where that keeps the meaning and double-quoted otherwise, and collections are block collections. |

```fsharp
let literal = YamlNode.scalar (ScalarStyle.Block(BlockScalarStyle.Literal, ChompingMode.Clip, None)) "one\ntwo\n"
let tree = Encode.object [ ("text", literal) ]

printf "%s" (Writer.writeNode WriterOptions.defaults tree)
printf "%s" (Writer.writeNode { WriterOptions.defaults with Styles = Canonical } tree)
```

```yaml
text: |
  one
  two
text: "one\ntwo\n"
```

---
## Local development

### Requirements

Because this library targets multiple programming languages we need to support all of them:

- [nodejs and npm](https://nodejs.org/en/download)
    - verify with `node --version` (tested with v22.19.0)
    - verify with `npm --version` (tested with 11.6.1)
- [.NET SDK](https://dotnet.microsoft.com/en-us/download)
    - verify with `dotnet --version` (`global.json` asks for 10.0.101 and accepts a newer 10.x SDK, tested with 10.0.303)
- [uv](https://docs.astral.sh/uv/getting-started/installation/)
    - verify with `uv --version` (tested with 0.9.26)


### Setup

Run these commands once in a fresh clone.

1. `dotnet tool restore` restores the .NET tools (Fable).
2. `npm ci` installs the JavaScript dependencies from `package-lock.json`.
3. `uv python install` installs the Python version of `.python-version` when it is missing.
4. `uv sync --frozen` installs the Python dependencies from `uv.lock`.

### Testing

`.\build.cmd test` runs the tests of every target. Name a target to run one of them:

`.\build.cmd test [f#, js, py, fable]`

`f#` runs the .NET tests, and `js` and `py` run the Fable output on Node and on Python. `fable` runs all three. The Python run goes through `uv run`, so the virtual environment needs no activation.

### Publish

Requires API keys for Nuget and PyPi. 

The following command will run all tests, bundle and then start publishing!

`.\build.cmd publish pipeline`

*or only publish specific targets, without test and bundle*

`.\build.cmd publish [npm, pypi, nuget]`
