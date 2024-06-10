module YAMLicious.AST

open System.Text
open System.Collections.Generic

type Config = {
    Whitespace: int
    Level: int
} with
    static member init(?whitespace) : Config = {
        Whitespace = defaultArg whitespace 4
        Level = 0
    }
    member this.WhitespaceString =
        String.init (this.Level*this.Whitespace) (fun _ -> " ")
    
module ReadHelpers =
    let indentLevel (line: string) =
        line |> Seq.takeWhile (fun c -> c = ' ') |> Seq.length

type YAML = {
    AST: YAMLAST
    StringMap: Dictionary<int, string>
    CommentMap: Dictionary<int, string>
} 

and YAMLAST =
    | Level of YAMLAST list
    | Intendation of YAMLAST list
    | Line of string

    static member write(rootElement:YAMLAST, ?fconfig: Config -> Config) =
        let config = Config.init() |> fun config -> if fconfig.IsSome then fconfig.Value config else config
        let sb = new StringBuilder()
        let rec loop (current: YAMLAST) (sb: StringBuilder) (config: Config) =
            match current with
            | Line line ->
                sb.AppendLine(config.WhitespaceString+line) |> ignore
            | Intendation children ->
                let nextConfig = {config with Level = config.Level + 1}
                for child in children do
                    loop child sb nextConfig
            | Level children ->
                for child in children do
                    loop child sb config
        loop rootElement sb config
        sb.ToString()

    static member read(yamlStr: string) =
        let content = Persil.pipeline yamlStr
        let rec loop (lines: string list) (currentIntendation: int) (acc: YAMLAST list) =
            match lines with
            | [] -> acc
            | line::rest ->
                let lineEle = Line (line.Trim())
                let nextIntendation = ReadHelpers.indentLevel line
                if nextIntendation = currentIntendation then
                    loop rest currentIntendation (Line (line.Trim())::acc)
                else
                    let nextLevelLines = rest |> List.takeWhile (fun l -> ReadHelpers.indentLevel l > currentIntendation)
                    let currentLevelLines = rest |> List.skipWhile (fun l -> ReadHelpers.indentLevel l > currentIntendation)
                    let otherChildren = loop nextLevelLines nextIntendation [] |> List.rev
                    let children = lineEle::otherChildren
                    loop currentLevelLines currentIntendation (Intendation children::acc)
        let ast = 
            loop (List.ofArray content.Lines) 0 [] 
            |> List.rev
            |> Level
        {
            AST = ast
            StringMap = content.StringMap
            CommentMap = content.CommentMap
        }

    override this.ToString() =
        let sb = StringBuilder()
        let rec innerprint (next: YAMLAST) (level: int) =
            let indent = String.init (level*2) (fun _ -> " ")
            match next with
            | Line line -> sb.AppendLine(indent + $"Line \"{line}\"") |> ignore
            | Intendation children ->
                sb.AppendLine(indent + "Intendation [") |> ignore
                for child in children do
                    innerprint child (level+1)
                sb.AppendLine(indent + "]") |> ignore
            | Level children ->
                sb.AppendLine(indent + "Level [") |> ignore
                for child in children do
                    innerprint child (level+1)
                sb.AppendLine(indent + "]") |> ignore
        innerprint this 0
        sb.ToString()
            
        
let mkLine (line:string) = Line line

let mklLevel (children: #seq<YAMLAST>) = List.ofSeq children |> Level

let mkIntendation (children: #seq<YAMLAST>) = List.ofSeq children |> Intendation

let write rootElement = YAMLAST.write(rootElement)

type IYAMLConvertible =
    abstract ToYAMLAst: unit -> YAMLAST list