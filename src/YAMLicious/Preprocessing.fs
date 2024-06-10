module YAMLicious.Preprocessing

open System.Text
open System.Collections.Generic
open YAMLiciousTypes

module ReadHelpers =
    let indentLevel (line: string) =
        line |> Seq.takeWhile (fun c -> c = ' ') |> Seq.length
            
let write(rootElement:PreprocessorElement, fconfig: (Config -> Config) option) =
    let config = Config.init() |> fun config -> if fconfig.IsSome then fconfig.Value config else config
    let sb = new StringBuilder()
    let rec loop (current: PreprocessorElement) (sb: StringBuilder) (config: Config) =
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

let read(yamlStr: string) =
    let content = Persil.pipeline yamlStr
    let rec loop (lines: string list) (currentIntendation: int) (acc: PreprocessorElement list) =
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
        
let mkLine (line:string) = Line line

let mklLevel (children: #seq<PreprocessorElement>) = List.ofSeq children |> Level

let mkIntendation (children: #seq<PreprocessorElement>) = List.ofSeq children |> Intendation