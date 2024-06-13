module YAMLicious.Writer

open YAMLicious.YAMLiciousTypes

module StyleVerifier =
    
    let checkInlineSequence (ele: YAMLElement list) =
        ele 
        |> Seq.forall (fun x ->
            match x with
            | YAMLElement.Value {Value = v; Comment = None} ->
                true
            | _ -> false
        )

module Formatting =

    let mkComment (comment: string) = "#" +  comment
    let mkKey (key: string) = key + ":"
    let mkContent (content: YAMLContent) = content.Value + (content.Comment |> Option.map (fun s -> " " + (mkComment s)) |> Option.defaultValue "")
    let mkInlineSequence (seq: YAMLElement list) = 
        let content = 
            seq 
            |> List.map (fun x -> 
                match x with
                | YAMLElement.Value {Value = v; Comment = None} -> v
                | _ -> failwith "Invalid sequence element"
            )
            |> String.concat ", "
        "[" + content + "]"
    let mkMinusLine (c: string) = "- " + c

let detokenize (ele: YAMLElement) =
    let rec loop (ele: YAMLElement) =
        match ele with
        | YAMLElement.Nil ->
            PreprocessorElement.Nil
        | YAMLElement.Mapping (key, v) -> 
            match v with
            | YAMLElement.Value v -> // mykey: myvalue # 12313
                let s = Formatting.mkKey key.Value + " " + Formatting.mkContent v
                PreprocessorElement.Line s
            | YAMLElement.Sequence seq when StyleVerifier.checkInlineSequence seq -> // mykey: [v1, v2, v3]
                let s = Formatting.mkKey key.Value + " " + Formatting.mkInlineSequence seq
                PreprocessorElement.Line s
            | anyElse ->
                PreprocessorElement.Level [
                    PreprocessorElement.Line (Formatting.mkKey (Formatting.mkContent key))
                    PreprocessorElement.Intendation [
                        loop anyElse
                    ]
                ]
        | YAMLElement.Value (v) -> 
            PreprocessorElement.Line (Formatting.mkContent v)
        | YAMLElement.Object seq ->
            PreprocessorElement.Level [
                for ele in seq do
                    loop ele 
            ]
        | YAMLElement.Sequence seq -> 
            PreprocessorElement.Level [
                for ele in seq do
                    match ele with
                    | YAMLElement.Value v -> // mykey: myvalue # 12313
                        let s = Formatting.mkMinusLine (Formatting.mkContent v)
                        PreprocessorElement.Line s
                    | YAMLElement.Sequence seq when StyleVerifier.checkInlineSequence seq -> // - [v1, v2, v3]
                        let s = Formatting.mkMinusLine (Formatting.mkInlineSequence seq)
                        PreprocessorElement.Line s
                    | anyElse ->
                        PreprocessorElement.Level [
                            PreprocessorElement.Line "-"
                            PreprocessorElement.Intendation [
                                loop anyElse
                            ]
                        ]
            ]
        | YAMLElement.Comment (c) -> 
            PreprocessorElement.Line (Formatting.mkComment c)
    loop ele

let write (ele: YAMLElement) (fconfig: (Config -> Config) option) =
    let preprocessed = detokenize ele
    Preprocessing.write (preprocessed, fconfig)