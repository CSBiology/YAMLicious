module YAMLicious.FlowToBlock

open YAMLiciousTypes
open Preprocessing
open System.Collections.Generic
open System.Text.RegularExpressions
open Regex

type TransformContext = {
    BaseIndent: int           // Current indentation level (in spaces)
    IndentStep: int           // Spaces per indent level (default: 2)
    StringDict: Dictionary<int, StringMapEntry>  // For string placeholder lookup
}

let defaultContext stringDict = {
    BaseIndent = 0
    IndentStep = 2
    StringDict = stringDict
}

// Token types for flow-style parsing
type Token =
    | OpenBrace
    | CloseBrace
    | OpenBracket
    | CloseBracket
    | Colon
    | Comma
    | String of string
    | EOF

// Tokenize flow-style content into tokens
let tokenize (input: string) : Token list =
    let rec parseString (chars: char list) (acc: char list) : char list * char list =
        match chars with
        | [] -> (List.rev acc, [])
        | '"'::rest -> (List.rev acc, rest)
        | '\\'::c::rest -> parseString rest (c::'\\'::acc)
        | c::rest -> parseString rest (c::acc)

    let rec tokenizeChars (chars: char list) (acc: Token list) : Token list =
        match chars with
        | [] -> List.rev (Token.EOF::acc)
        | ' '::rest | '\n'::rest | '\r'::rest | '\t'::rest -> tokenizeChars rest acc
        | '{'::rest -> tokenizeChars rest (Token.OpenBrace::acc)
        | '}'::rest -> tokenizeChars rest (Token.CloseBrace::acc)
        | '['::rest -> tokenizeChars rest (Token.OpenBracket::acc)
        | ']'::rest -> tokenizeChars rest (Token.CloseBracket::acc)
        | ':'::rest -> tokenizeChars rest (Token.Colon::acc)
        | ','::rest -> tokenizeChars rest (Token.Comma::acc)
        | '"'::rest ->
            let (str, remaining) = parseString rest []
            let strValue = System.String(Array.ofList str)
            tokenizeChars remaining (Token.String strValue::acc)
        | chars ->
            // Parse unquoted string until delimiter
            let consumePlaceholder (cs: char list) (acc: char list) =
                let rec loop (cs: char list) (acc: char list) =
                    match cs with
                    | '/'::'>'::rest -> (List.rev ('>'::'/'::acc), rest)
                    | c::rest -> loop rest (c::acc)
                    | [] -> (List.rev acc, [])
                loop cs acc

            let rec parseUnquoted (cs: char list) (acc: char list) =
                match cs with
                | [] -> (List.rev acc, [])
                | '<'::'s'::' '::'f'::'='::rest ->
                    // String placeholder - consume it whole
                    let (placeholder, remaining) = consumePlaceholder rest ['=';'f';' ';'s';'<']
                    let (restStr, final) = parseUnquoted remaining []
                    (placeholder @ restStr, final)
                | '<'::'c'::' '::'f'::'='::rest ->
                    // Comment placeholder - consume it whole
                    let (placeholder, remaining) = consumePlaceholder rest ['=';'f';' ';'c';'<']
                    let (restStr, final) = parseUnquoted remaining []
                    (placeholder @ restStr, final)
                | c::rest when c = '{' || c = '}' || c = '[' || c = ']' || c = ':' || c = ',' || c = '\n' || c = '\r' || c = '\t' ->
                    (List.rev acc, cs)
                | ' '::rest when acc.IsEmpty ->
                    // Skip leading spaces
                    parseUnquoted rest acc
                | c::rest -> parseUnquoted rest (c::acc)
            
            let (str, remaining) = parseUnquoted chars []
            if str.IsEmpty then
                tokenizeChars remaining acc
            else
                let strValue = System.String(Array.ofList str)
                tokenizeChars remaining (Token.String strValue::acc)

    tokenizeChars (List.ofSeq input) []

// Convert tokens to block-style PreprocessorElement lines
let rec tokensToBlockElements (ctx: TransformContext) (tokens: Token list) : PreprocessorElement list * Token list =
    match tokens with
    | Token.OpenBrace::rest -> 
        objectToBlockElements ctx rest
    | Token.OpenBracket::rest -> 
        arrayToBlockElements ctx rest
    | Token.String key::Token.Colon::rest -> 
        // Top-level key-value pair (implicit object)
        let childCtx = { ctx with BaseIndent = ctx.BaseIndent + ctx.IndentStep }
        
        // Check what type of value follows
        match rest with
        | Token.OpenBrace::_ ->
            // This is an object value - always treat as complex nested structure
            let (valueElements, remaining) = tokensToBlockElements childCtx rest
            let keyLine = PreprocessorElement.Line (sprintf "%s:" key)
            let result = [keyLine; PreprocessorElement.Intendation valueElements]
            (result, remaining)
        | Token.OpenBracket::_ ->
            // This is an array value - always treat as complex nested structure  
            let (valueElements, remaining) = tokensToBlockElements childCtx rest
            let keyLine = PreprocessorElement.Line (sprintf "%s:" key)
            let result = [keyLine; PreprocessorElement.Intendation valueElements]
            (result, remaining)
        | _ ->
            // Other value types - use normal simple/complex logic
            let (valueElements, remaining) = tokensToBlockElements childCtx rest
            
            let keyValueElements =
                match valueElements with
                | [PreprocessorElement.Line simpleValue] ->
                    [PreprocessorElement.Line (sprintf "%s: %s" key simpleValue)]
                | [] ->
                    [PreprocessorElement.Line (sprintf "%s: {}" key)]
                | complexValue ->
                    let keyLine = PreprocessorElement.Line (sprintf "%s:" key)
                    [keyLine; PreprocessorElement.Intendation complexValue]
            
            (keyValueElements, remaining)
    | Token.String s::rest -> 
        ([PreprocessorElement.Line s], rest)
    | Token.EOF::_ -> 
        ([], tokens)
    | _ -> 
        failwith $"Unexpected token in tokensToBlockElements: {tokens}"

and objectToBlockElements (ctx: TransformContext) (tokens: Token list) : PreprocessorElement list * Token list =
    let childCtx = { ctx with BaseIndent = ctx.BaseIndent + ctx.IndentStep }
    
    let rec parseKeyValues (tokens: Token list) (acc: PreprocessorElement list) : PreprocessorElement list * Token list =
        match tokens with
        | Token.CloseBrace::rest -> 
            (List.rev acc, rest)
        | Token.Comma::rest -> 
            parseKeyValues rest acc
        | Token.EOF::_ -> 
            (List.rev acc, tokens)
        | Token.String key::Token.Colon::rest ->
            // Check what type of value follows  
            match rest with
            | Token.OpenBrace::_ ->
                // Object value - always treat as complex nested structure
                let (valueElements, remaining) = tokensToBlockElements childCtx rest
                let keyLine = PreprocessorElement.Line (sprintf "%s:" key)
                let keyValueElements = [keyLine; PreprocessorElement.Intendation valueElements]
                parseKeyValues remaining (List.rev keyValueElements @ acc)
            | Token.OpenBracket::_ ->
                // Array value - always treat as complex nested structure
                let (valueElements, remaining) = tokensToBlockElements childCtx rest
                let keyLine = PreprocessorElement.Line (sprintf "%s:" key)
                let keyValueElements = [keyLine; PreprocessorElement.Intendation valueElements]
                parseKeyValues remaining (List.rev keyValueElements @ acc)
            | _ ->
                // Other value types - use normal simple/complex logic
                let (valueElements, remaining) = tokensToBlockElements childCtx rest
                
                let keyValueElements =
                    match valueElements with
                    | [PreprocessorElement.Line simpleValue] ->
                        // Simple value - put on same line
                        [PreprocessorElement.Line (sprintf "%s: %s" key simpleValue)]
                    | [] ->
                        // Empty object {}
                        [PreprocessorElement.Line (sprintf "%s: {}" key)]
                    | complexValue ->
                        // Complex value - put on separate indented lines
                        let keyLine = PreprocessorElement.Line (sprintf "%s:" key)
                        [keyLine; PreprocessorElement.Intendation complexValue]
                
                parseKeyValues remaining (List.rev keyValueElements @ acc)
        | _ -> 
            failwith $"Expected key or close brace in object, got: {tokens}"
    
    parseKeyValues tokens []

and arrayToBlockElements (ctx: TransformContext) (tokens: Token list) : PreprocessorElement list * Token list =
    let childCtx = { ctx with BaseIndent = ctx.BaseIndent + ctx.IndentStep }
    
    let rec parseElements (tokens: Token list) (acc: PreprocessorElement list) : PreprocessorElement list * Token list =
        match tokens with
        | Token.CloseBracket::rest -> 
            (List.rev acc, rest)
        | Token.Comma::rest -> 
            parseElements rest acc
        | Token.EOF::_ -> 
            (List.rev acc, tokens)
        | _ ->
            // Parse the element
            let (elementLines, remaining) = tokensToBlockElements childCtx tokens
            
            // Format as list item  
            let itemElements =
                match elementLines with
                | [PreprocessorElement.Line singleValue] ->
                    // Simple value - put on same line with dash
                    [PreprocessorElement.Line (sprintf "- %s" singleValue)]
                | [] ->
                    // Empty object - put {} on same line
                    [PreprocessorElement.Line "- {}"]
                | complexValue ->
                    // Complex value - dash on its own line, then indented content
                    let dashLine = PreprocessorElement.Line "-"
                    [dashLine; PreprocessorElement.Intendation complexValue]
            
            parseElements remaining (List.rev itemElements @ acc)
    
    parseElements tokens []

/// Transform flow-style content string to block-style PreprocessorElements
let transformFlowContent (ctx: TransformContext) (content: string) : PreprocessorElement list =
    let tokens = tokenize content
    let (elements, _) = tokensToBlockElements ctx tokens
    elements

/// Transform a single PreprocessorElement that may contain flow-style
let rec transformElement (ctx: TransformContext) (element: PreprocessorElement) : PreprocessorElement list =
    match element with
    | PreprocessorElement.Line s ->
        // Check for KeyValue pattern with flow-style value: key: {...} or key: [...]
        // KeyValuePattern: ^(?<key>[^\{\[]+):\s+(?<value>.*)$
        let keyValueMatch = Regex.Match(s, Regex.KeyValuePattern)
        if keyValueMatch.Success then
            let key = keyValueMatch.Groups.["key"].Value
            let value = keyValueMatch.Groups.["value"].Value.Trim()
            
            // Check if value is flow-style (starts with { or [)
            if value.StartsWith("{") then
                let childCtx = { ctx with BaseIndent = ctx.BaseIndent + ctx.IndentStep }
                
                // For objects, check if there's a comment at the end: {...}<c f=N/>
                let objCommentMatch = Regex.Match(value, "^(\{.*\})\s*(<c f=(\d+)/>)?$")
                if objCommentMatch.Success then
                    let objContent = objCommentMatch.Groups.[1].Value
                    let commentGroup = objCommentMatch.Groups.[3]
                    
                    // Transform the flow-style value
                    let transformedValue = transformFlowContent childCtx objContent
                    
                    // Create key line and indented value
                    let keyLine = PreprocessorElement.Line (sprintf "%s:" key)
                    let indentedContent = PreprocessorElement.Intendation transformedValue
                    
                    // If there's a comment, add it before the indented content
                    if commentGroup.Success then
                        let commentId = int commentGroup.Value
                        let commentElement = PreprocessorElement.Line (sprintf "<c f=%s/>" commentGroup.Value)
                        [keyLine; PreprocessorElement.Intendation (commentElement :: transformedValue)]
                    else
                        [keyLine; indentedContent]
                else
                    // Fallback: transform the entire value
                    let transformedValue = transformFlowContent childCtx value
                    [PreprocessorElement.Line (sprintf "%s:" key); PreprocessorElement.Intendation transformedValue]
                    
            elif value.StartsWith("[") then
                let childCtx = { ctx with BaseIndent = ctx.BaseIndent + ctx.IndentStep }
                
                // For arrays, check if there's a comment at the end: [...] <c f=N/>
                let arrCommentMatch = Regex.Match(value, "^(\[.*\])\s*(<c f=(\d+)/>)?$")
                if arrCommentMatch.Success then
                    let arrContent = arrCommentMatch.Groups.[1].Value
                    let commentGroup = arrCommentMatch.Groups.[3]
                    
                    // Transform the flow-style value
                    let transformedValue = transformFlowContent childCtx arrContent
                    
                    // Create key line and indented value
                    let keyLine = PreprocessorElement.Line (sprintf "%s:" key)
                    
                    // If there's a comment, add it before the transformed content
                    if commentGroup.Success then
                        let commentElement = PreprocessorElement.Line (sprintf "<c f=%s/>" commentGroup.Value)
                        [keyLine; PreprocessorElement.Intendation (commentElement :: transformedValue)]
                    else
                        [keyLine; PreprocessorElement.Intendation transformedValue]
                else
                    // Fallback: transform the entire value
                    let transformedValue = transformFlowContent childCtx value
                    [PreprocessorElement.Line (sprintf "%s:" key); PreprocessorElement.Intendation transformedValue]
            else
                [element]  // Normal key-value, keep as-is
        else
            // Check if this line contains flow-style syntax
            // FlowStyleObject pattern: ^\{(?<inlineSequence>.*)\}\s*?(<c f=(?<comment>\d+)\/>)?$
            let inlineJsonMatch = Regex.Match(s, FlowStyleObjectPattern)
            if inlineJsonMatch.Success then
                let content = inlineJsonMatch.Groups.["inlineSequence"].Value
                if content.Trim() = "" then
                    // Empty object
                    [PreprocessorElement.Line "{}"]
                else
                    // Transform flow-style object to block-style
                    let transformed = transformFlowContent ctx ("{" + content + "}")
                    transformed
            else
                // FlowStyleArray pattern: ^\[(?<inlineSequence>.+)\]\s*?(<c f=(?<comment>\d+)\/>)?$
                let inlineSeqMatch = Regex.Match(s, FlowStyleArrayPattern)
                if inlineSeqMatch.Success then
                    let content = inlineSeqMatch.Groups.["inlineSequence"].Value
                    let commentGroup = inlineSeqMatch.Groups.["comment"]
                    
                    // Transform flow-style array to block-style
                    let transformed = transformFlowContent ctx ("[" + content + "]")
                    
                    // If there's a comment, add it as a comment element before the transformed content
                    if commentGroup.Success then
                        let commentId = int commentGroup.Value
                        let commentElement = PreprocessorElement.Line (sprintf "<c f=%d/>" commentId)
                        commentElement :: transformed
                    else
                        transformed
                else
                    [element]  // No flow-style detected, keep as-is
                
    | PreprocessorElement.Intendation children ->
        let transformed = transformElements ctx children
        [PreprocessorElement.Intendation transformed]
    | _ -> [element]

/// Transform an entire list of PreprocessorElements, expanding flow-style
and transformElements (ctx: TransformContext) (elements: PreprocessorElement list) : PreprocessorElement list =
        
    // First pass: look for JSONKeyOpener..JSONCloser patterns
    let rec processElements (elems: PreprocessorElement list) (acc: PreprocessorElement list) : PreprocessorElement list =
        match elems with
        | [] -> List.rev acc
        | PreprocessorElement.Line opener::PreprocessorElement.Intendation iList::PreprocessorElement.Line closer::rest ->
            // Check if this matches FlowStyleObjectOpener pattern: ^(?<key>[^\{\[]+):\s+\{\s*(<c f=(?<comment>\d+)\/>)?$
            let jsonOpenerMatch = Regex.Match(opener, FlowStyleObjectOpenerPattern)
            let jsonCloserMatch = Regex.Match(closer, FlowStyleObjectCloserPattern)
            if jsonOpenerMatch.Success && jsonCloserMatch.Success then
                // This is a multi-line flow object - transform it
                let key = jsonOpenerMatch.Groups.["key"].Value
                
                // Flatten the indentation to get the content
                let rec flattenLines (eles: PreprocessorElement list) : string list =
                    eles
                    |> List.collect (function
                        | PreprocessorElement.Line s -> [s.TrimEnd(',')]
                        | PreprocessorElement.Intendation children -> flattenLines children
                        | _ -> [])
                
                let jsonContent = flattenLines iList |> String.concat "\n"
                let fullJson = "{" + jsonContent + "}"
                
                // Transform the JSON content
                let childCtx = { ctx with BaseIndent = ctx.BaseIndent + ctx.IndentStep }
                let transformedContent = transformFlowContent childCtx fullJson
                
                // Create the key line and indented content
                let keyLine = PreprocessorElement.Line (sprintf "%s:" key)
                let indentedContent = PreprocessorElement.Intendation transformedContent
                
                processElements rest (indentedContent :: keyLine :: acc)
            else
                // Not a JSONKeyOpener pattern, process normally
                let transformed1 = transformElement ctx (PreprocessorElement.Line opener)
                let transformed2 = transformElement ctx (PreprocessorElement.Intendation iList)
                let transformed3 = transformElement ctx (PreprocessorElement.Line closer)
                processElements rest (List.rev transformed3 @ List.rev transformed2 @ List.rev transformed1 @ acc)
        | elem::rest ->
            let transformed = transformElement ctx elem
            processElements rest (List.rev transformed @ acc)
    
    processElements elements []
