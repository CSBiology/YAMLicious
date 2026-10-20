module YAMLicious.Preprocessing

open System.Collections.Generic
open System.Text
open Syntax
open YAMLiciousTypes

/// A block scalar header together with its body. The scanner decides where the body ends, so nothing
/// in this module measures the indentation of a body line again.
type private ScalarBlock =
    { Header: Persil.TokenLine
      /// The indentation of the header line, which places the scalar among its neighbors.
      Indent: int
      /// The indentation of the node that owns the scalar, as the shared header helper reports it. The
      /// body is read relative to it, and the content behind the scalar must not be indented deeper.
      NodeIndent: int
      /// The body lines exactly as the scanner copied them.
      Body: string list }

/// A piece of the input that the comment alignment and the indentation pass handle as a whole. A block
/// scalar is one unit, so neither pass ever sees one of its body lines.
[<RequireQualifiedAccess>]
type private InputUnit =
    | Code of Persil.TokenLine
    | Comment of Persil.TokenLine
    | Blank
    | Scalar of ScalarBlock

let private isFlowCloserLine (line: Persil.TokenLine) =
    match line.Text.Substring(0, line.CodeLength).Trim().TrimEnd(',') with
    | "]" | "}" -> true
    | _ -> false

let private withIndent (indent: int) (line: string) =
    System.String(' ', indent) + line.TrimStart()

/// The indentation of the node a unit starts. A block scalar counts by its header. A blank line starts
/// no node, so callers never ask for its indentation and the value here has no meaning.
let private indentOf (item: InputUnit) =
    match item with
    | InputUnit.Code line
    | InputUnit.Comment line -> Line.countLeadingSpaces line.Text
    | InputUnit.Scalar block -> block.Indent
    | InputUnit.Blank -> 0

let private scalarBlock (header: Persil.TokenLine) (body: string list) =
    let code = header.Text.Substring(0, header.CodeLength)
    match BlockScalar.tryParseHeaderCode code with
    | Some found ->
        { Header = header
          Indent = Line.countLeadingSpaces header.Text
          NodeIndent = found.Baseline
          Body = body }
    | None ->
        failwithf "Line %d starts a block scalar but holds no block scalar header: %s" header.SourceLine header.Text

/// A sequence entry puts its node behind the dash. In `- - a` the node starts at the second dash, and in
/// `- k: v` at the key. The line becomes one unit for each dash that stands left of the node, at the column
/// of the dash, and the unit of the node keeps its column and everything behind it, so no text moves. The
/// lines behind the entry then nest by their columns like the lines behind any other node.
let private splitEntryDashes (line: Persil.TokenLine) =
    match line.Kind with
    | Persil.LineKind.Code
    | Persil.LineKind.BlockHeader ->
        match LineValue.dashColumns line.Text with
        | [] -> [ line ]
        | columns ->
            let code = line.Text.Substring(0, line.CodeLength)
            let node = (LineValue.find code).Owner
            match columns |> List.filter (fun column -> column < node) with
            | [] -> [ line ]
            | dashes ->
                let nodeColumn = LineValue.skipBlanks code (List.last dashes + 1)
                [ for column in dashes do
                    yield
                        { line with
                            Text = System.String(' ', column) + "-"
                            Kind = Persil.LineKind.Code
                            BlockId = Persil.NoBlock
                            CommentId = Persil.NoComment
                            CodeLength = column + 1 }
                  yield { line with Text = System.String(' ', nodeColumn) + line.Text.Substring(nodeColumn) } ]
    | _ -> [ line ]

/// Joins every block scalar header with the body lines that the scanner assigned to it. The walk runs
/// from the last line to the first, so each body is complete when its header comes up and the units
/// come out in input order without a reversal.
let private groupUnits (tokenLines: Persil.TokenLine[]) =
    let lines = tokenLines |> Array.collect (splitEntryDashes >> Array.ofList)
    let mutable units = []
    let mutable body = []
    for index = lines.Length - 1 downto 0 do
        let line = lines.[index]
        if line.Kind = Persil.LineKind.BlockBody then
            body <- line.Text :: body
        elif line.BlockId <> Persil.NoBlock then
            units <- InputUnit.Scalar(scalarBlock line body) :: units
            body <- []
        elif line.Kind = Persil.LineKind.CommentOnly then
            units <- InputUnit.Comment line :: units
        elif line.Kind = Persil.LineKind.Blank then
            units <- InputUnit.Blank :: units
        else
            units <- InputUnit.Code line :: units
    units

/// Rejects content that follows a block scalar and sits deeper than the node that owns the scalar. The
/// scalar ended before such a line because the line is too shallow for the content, and no sibling or
/// parent can hold the line. The scanner already decided where each scalar ends, so a comment that
/// stands between the scalar and the line is not content. The message names the first such comment,
/// because that comment is the visible reason why the scalar ends. Without a comment the error says
/// only that the indentation is wrong. The check runs on the original indentation, before the
/// alignment moves any comment.
let private checkScalarBoundaries (comments: Dictionary<int, string>) (units: InputUnit list) =
    let check (scalar: ScalarBlock option) (comment: Persil.TokenLine option) (content: Persil.TokenLine) =
        match scalar with
        | Some block ->
            if Line.countLeadingSpaces content.Text > block.NodeIndent then
                match comment with
                | Some terminator ->
                    failwithf
                        "A comment line (#%s) ends a block scalar, so the more indented content after it is invalid. (Scalar line %d; comment line %d; content line %d.)"
                        comments.[terminator.CommentId] block.Header.SourceLine terminator.SourceLine content.SourceLine
                | None ->
                    failwithf
                        "A line that is more indented than the node of a block scalar, but not enough to continue the scalar, is invalid. (Scalar line %d; content line %d.)"
                        block.Header.SourceLine content.SourceLine
        | None -> ()

    // `scalar` is the block scalar that the next content unit follows. `comment` is the first comment
    // between them. The comments and blank lines after that first comment do not change the outcome.
    let rec loop (remaining: InputUnit list) (scalar: ScalarBlock option) (comment: Persil.TokenLine option) =
        match remaining with
        | [] -> ()
        | InputUnit.Blank :: rest -> loop rest scalar comment
        | InputUnit.Comment line :: rest ->
            if scalar.IsSome && comment.IsNone then loop rest scalar (Some line) else loop rest scalar comment
        | InputUnit.Code line :: rest ->
            check scalar comment line
            loop rest None None
        | InputUnit.Scalar block :: rest ->
            check scalar comment block.Header
            loop rest (Some block) None

    loop units None None

/// Sets the indentation of a comment. Any other unit stays as it is.
let private commentAt (indent: int) (item: InputUnit) =
    match item with
    | InputUnit.Comment line -> InputUnit.Comment { line with Text = withIndent indent line.Text; CodeLength = indent }
    | other -> other

/// The indentation for the comments in front of the unit `next`. A comment before a flow closer takes
/// the indentation of the previous node when that node is deeper than the closer, and the indentation
/// of the closer otherwise. Either way it stays inside the flow collection.
let private commentIndentBefore (next: InputUnit) (previousIndent: int) =
    match next with
    | InputUnit.Code line ->
        let indent = Line.countLeadingSpaces line.Text
        if isFlowCloserLine line && previousIndent > indent then previousIndent else indent
    | other -> indentOf other

/// Closes the levels that are deeper than `indent`. `levels` holds the indentations of the open levels,
/// innermost first, and each one is deeper than the level below it.
let rec private closeDeeperLevels (indent: int) (levels: int list) =
    match levels with
    | top :: below when top > indent -> closeDeeperLevels indent below
    | _ -> levels

/// A node closes the levels deeper than itself and opens a level of its own when it is deeper than
/// the innermost level that is left.
let private enterLevel (indent: int) (levels: int list) =
    match closeDeeperLevels indent levels with
    | top :: _ as openLevels when top = indent -> openLevels
    | openLevels -> indent :: openLevels

/// Aligns the comments that no node follows. `gap` holds the comments and blank lines in source order.
/// A comment takes the indentation of the deepest open level that is at most as deep as the comment,
/// and level 0 always qualifies. It closes the deeper levels for the comments behind it, so no comment
/// reopens a block that an earlier one has left. The result comes back newest first.
let private alignTrailingComments (gap: InputUnit list) (levels: int list) =
    let align (aligned: InputUnit list, openLevels: int list) (item: InputUnit) =
        match item with
        | InputUnit.Comment line ->
            let remaining = closeDeeperLevels (Line.countLeadingSpaces line.Text) openLevels
            let level = match remaining with top :: _ -> top | [] -> 0
            commentAt level item :: aligned, remaining
        | other -> other :: aligned, openLevels

    List.fold align ([], levels) gap |> fst

/// A full-line comment belongs to the node that follows it. Giving it the indentation of the next
/// node puts it into that node's block, so the indentation pass treats it like any other line. The
/// next node of a block scalar is its header, because the body belongs to the scalar and never holds
/// a comment. A comment with no node after it stays in the last block. It takes the deepest open level
/// that is at most as deep as the comment, so it never opens a level between two existing ones. A
/// block scalar opens its level by its header.
///
/// The pass walks forward and collects the comments and blank lines in front of each node, so each
/// unit costs a constant number of steps. Every node and every trailing comment closes the levels
/// deeper than itself, which makes the work on the open levels linear in total.
let private alignCommentLines (units: InputUnit list) =
    // `gap` holds the comments and blank lines since the last node, newest first. `aligned` holds the
    // result, newest first as well. `levels` holds the open levels, innermost first. The innermost level
    // is the indentation of the previous node, and the list is empty before the first node.
    let rec loop (remaining: InputUnit list) (gap: InputUnit list) (levels: int list) (aligned: InputUnit list) =
        match remaining with
        | [] ->
            List.rev (alignTrailingComments (List.rev gap) levels @ aligned)
        | (InputUnit.Comment _ | InputUnit.Blank as item) :: rest ->
            loop rest (item :: gap) levels aligned
        | item :: rest ->
            let previousIndent = match levels with top :: _ -> top | [] -> -1
            let moved =
                if List.isEmpty gap then gap
                else List.map (commentAt (commentIndentBefore item previousIndent)) gap
            loop rest [] (enterLevel (indentOf item) levels) (item :: (moved @ aligned))

    loop units [] [] []

/// Removes the indentation of a node line from the frame it sits in. A line that is less indented than
/// the frame loses only the spaces it has.
let private stripIndent (indent: int) (line: string) =
    if indent <= 0 then line
    else
        let availableIndent = Line.countLeadingSpaces line
        if availableIndent >= indent then line.Substring(indent)
        else line.TrimStart()

/// Turns a unit into elements on the reversed accumulator. A block scalar becomes its header line and
/// one flat body block without the indentation of the node that owns the scalar. The body is never
/// nested or interpreted, and its indentation beyond that of the owning node is content. An empty body
/// still yields a block, so that the Reader reads the header as a block scalar and not as a plain value.
let private lower (frameIndent: int) (item: InputUnit) (acc: PreprocessorElement list) =
    match item with
    | InputUnit.Scalar block ->
        let body = block.Body |> List.map (fun line -> Line(BlockScalar.stripIndent block.NodeIndent line))
        Intendation body :: Line(stripIndent frameIndent block.Header.Text) :: acc
    | InputUnit.Code line
    | InputUnit.Comment line -> Line(stripIndent frameIndent line.Text) :: acc
    | InputUnit.Blank -> Line("") :: acc

/// Counts the blank units at the start of the list and returns them with the units behind them.
let rec private skipBlanks (units: InputUnit list) (count: int) =
    match units with
    | InputUnit.Blank :: rest -> skipBlanks rest (count + 1)
    | _ -> count, units

/// Splits the units of the block that is nested deeper than `frameIndent` from the units behind it. A
/// run of blank lines belongs to the nested block when the unit behind the run is deeper too, or when
/// nothing follows. A run is judged once as a whole.
let private splitNestedBlock (frameIndent: int) (units: InputUnit list) =
    let rec loop (taken: InputUnit list) (remaining: InputUnit list) =
        match remaining with
        | [] -> List.rev taken, remaining
        | InputUnit.Blank :: _ ->
            let count, following = skipBlanks remaining 0
            let stays =
                match following with
                | next :: _ -> indentOf next > frameIndent
                | [] -> true
            if stays then loop (List.replicate count InputUnit.Blank @ taken) following
            else List.rev taken, remaining
        | item :: rest ->
            if indentOf item > frameIndent then loop (item :: taken) rest
            else List.rev taken, remaining

    loop [] units

/// True when the code of a line, without its comment, ends in a character that opens a block: the colon
/// of a key, or the bracket of a flow collection. A line with nothing behind its indicators but properties,
/// as in `k: !!str`, opens a block too, because the node behind the properties follows on a later line.
let private opensBlock (line: Persil.TokenLine) =
    let code = line.Text.Substring(0, line.CodeLength).TrimEnd()
    code.EndsWith("[") || code.EndsWith("{") || (LineValue.find code).Start >= code.Length

/// Builds the elements of one frame, the units that share the indentation `frameIndent`. A deeper unit
/// starts a nested frame, which becomes an `Intendation`. The elements come back in reverse order.
/// `lastCode` is the last code line of the frame when only comments and blank lines follow it, and
/// `None` otherwise.
let rec private buildFrame (units: InputUnit list) (frameIndent: int) (lastCode: Persil.TokenLine option) (acc: PreprocessorElement list) =
    match units with
    | [] -> acc
    | InputUnit.Blank :: _ ->
        let count, following = skipBlanks units 0
        let blankLines = List.replicate count (Line(""))
        match following with
        | next :: _ ->
            let nextIndent = indentOf next
            if nextIndent > frameIndent && (List.isEmpty acc || Option.exists opensBlock lastCode) then
                // The blank lines move into the block that follows them.
                let nested, remaining = splitNestedBlock frameIndent following
                let children = buildFrame nested nextIndent None blankLines |> List.rev
                buildFrame remaining frameIndent None (Intendation children :: acc)
            else
                buildFrame following frameIndent lastCode (blankLines @ acc)
        | [] ->
            blankLines @ acc
    | item :: rest ->
        let indent = indentOf item
        if indent = frameIndent then
            let lastCode =
                match item with
                | InputUnit.Code line -> Some line
                | InputUnit.Comment _ -> lastCode
                | InputUnit.Scalar _ | InputUnit.Blank -> None
            buildFrame rest frameIndent lastCode (lower frameIndent item acc)
        else
            let nested, remaining = splitNestedBlock frameIndent rest
            let children = buildFrame (item :: nested) indent None [] |> List.rev
            buildFrame remaining frameIndent None (Intendation children :: acc)


let write (rootElement: PreprocessorElement, fconfig: (Config -> Config) option) =
    let config =
        Config.init ()
        |> fun config -> if fconfig.IsSome then fconfig.Value config else config

    let sb = StringBuilder()

    let rec loop (current: PreprocessorElement) (sb: StringBuilder) (config: Config) =
        match current with
        | Line line -> sb.AppendLine(config.WhitespaceString + line) |> ignore
        | Intendation children ->
            let nextConfig = { config with Level = config.Level + 1 }

            for child in children do
                loop child sb nextConfig
        | Level children ->
            for child in children do
                loop child sb config
        | Nil -> ()

    loop rootElement sb config
    sb.ToString()

/// Builds the AST from lines that the scanner already tokenized. Reader.readDocuments passes the
/// documents of one tokenized input here, so no text is tokenized twice.
let internal readPrepared (content: Persil.PreparedYaml) =
    let units = groupUnits content.Lines
    checkScalarBoundaries content.CommentMap units
    let ast = buildFrame (alignCommentLines units) 0 None [] |> List.rev |> Level

    { AST = ast
      StringMap = content.StringMap
      CommentMap = content.CommentMap
      YAMLVersion = content.YAMLVersion
      TagHandles = content.TagHandles }

let read (yamlStr: string) =
    readPrepared (Persil.pipeline yamlStr)

let mkLine (line: string) = Line line

let mklLevel (children: #seq<PreprocessorElement>) = List.ofSeq children |> Level

let mkIntendation (children: #seq<PreprocessorElement>) = List.ofSeq children |> Intendation
