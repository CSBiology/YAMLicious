namespace YAMLicious

open System
open System.Collections.Generic
open System.Text

/// Paths to the places of a tree. A path is a chain of segments, and `materialize` joins them. The validator
/// spells the slots of a tree with it. The projection of the tests spells a second path, the keys, with the same
/// functions, so both cut a deep path in the same way.
module internal NodePath =

    /// The place of a node. A node deeper than `fullPathDepth` shows its depth and the last `tailSegments`
    /// segments only, so the size of the output stays linear in the size of the tree.
    type PathInfo =
        { Parent: PathInfo option
          Segment: string
          Depth: int }

    let fullPathDepth = 64
    let tailSegments = 8

    /// `head` stands in front of the first segment of a deep path, which does not reach the root. The slots
    /// use "root". The key path of the projection uses nothing.
    let materialize (path: PathInfo) (head: string) : string =
        let segments = ResizeArray<string>()
        let mutable current = Some path
        let mutable remaining = if path.Depth <= fullPathDepth then Int32.MaxValue else tailSegments
        while remaining > 0 && current.IsSome do
            let here = current.Value
            segments.Add(here.Segment)
            remaining <- remaining - 1
            current <- here.Parent
        let sb = StringBuilder()
        if path.Depth > fullPathDepth then
            sb.Append(head).Append("(").Append(string path.Depth).Append(")") |> ignore
        for i in segments.Count - 1 .. -1 .. 0 do
            sb.Append(segments.[i]) |> ignore
        sb.ToString()

    let slotOf (path: PathInfo) = materialize path "root"

    let child (parent: PathInfo) (segment: string) : PathInfo =
        { Parent = Some parent
          Segment = segment
          Depth = parent.Depth + 1 }

    let rootPath: PathInfo =
        { Parent = None
          Segment = "root"
          Depth = 0 }

/// Checks of a document or a stream against the invariants of the model and the slot rules R1 to R8 that
/// the tree alone can show. An empty list means that the tree is valid. Each message starts with the slot
/// that is wrong, written as the presentation projection writes slots, for example
/// `root.entries[2].Value.TrailingComment`. The slots of a document in a stream start with `Parts[i].`.
///
/// The checks, by the place of the rule:
///
/// - A node: an alias has no properties. `PropertyLayout` names each present property once and nothing
///   else. A block collection and a block scalar have no `TrailingComment` (R8). A missing value has none
///   either (R1), and neither has a node that has properties and no content unless it is an item of a flow
///   sequence or a key or value of a flow mapping entry (R1, R6). `HeaderComment` stands on block
///   scalars only, and `OpeningComment` on flow collections only. An empty collection is a flow collection.
/// - An item: a block item has a dash and a flow item has none.
/// - A separation behind an indicator (`?`, `:` or `-`) or behind the last property: `Following` is empty
///   when a block collection without a property follows or when the content is missing (R2). A block
///   collection starts on a later line than its last property, so that property has a line break behind it,
///   and the colon of an implicit entry has one when a block collection without a property is the value (R1).
///   Behind a dash, a question mark or an explicit colon with an inline separation such a collection continues
///   the line, and then no trivia stand in front of its first entry or item (R2, R4).
/// - An entry: the gap before the colon is empty when no colon follows and for an implicit key in a block
///   mapping (R7). An entry with a value has a colon, and so has an entry without a question mark in a block
///   mapping. Only a key without a value omits the colon.
/// - A comment: its text holds nb-char characters only ([27]), so no line break, control character, byte order mark
///   or lone surrogate half. This holds for every comment of the tree, the comments of the directives and the markers
///   included.
/// - A name: an anchor or an alias name is one or more ns-anchor-char characters ([102]).
/// - A tag: a named tag has a suffix of one or more characters ([98], [99]).
/// - A directive: a document has at most one `%YAML` directive and its version is 1.x. A tag handle is `!`, `!!` or
///   a word between two `!`, a document declares it once, and its prefix holds URI characters. A reserved directive
///   has a name of ns-char characters that is neither `YAML` nor `TAG`, and its parameters are ns-char words that
///   do not start with a `#`.
/// - A block collection: `End` is empty unless the collection is the last node of the text of its document
///   (R4). A trivia run between two nodes goes to the `Before` of the next one, and the run in front of a
///   following document without a `...` goes to its `Prelude`, so no other collection has an `End`. The Ends
///   along the last node of the text, the innermost first, hold one run of trivia, which the parser splits (R4):
///   a blank line goes with the comment that follows it, and the blank lines behind the last comment go to the
///   End of that comment. So the End of a nested collection ends with a comment unless it holds the last
///   comment, no End behind the one with the last comment holds anything, and a run without a comment goes to
///   the End of the root collection.
/// - A document: `BeforeRoot` is empty in front of a block collection without a property and `AfterRoot`
///   is empty behind a block collection, behind a missing root and in front of a document that follows
///   without a `...` between. `AfterEndMarker` is empty without an end marker and in front of a document that
///   follows (R3).
/// - A block scalar with the chomping Keep (R5): when the stored Keep is the chomping that the writer writes for
///   the value, no blank line follows its last line directly when more content follows, because l-chomped-empty
///   makes that line part of the value ([165] to [169]). The stored Keep is that chomping when the value can stand
///   in a block scalar and is empty or ends in a line feed. A stored Keep on a value without a final line feed is a
///   style mismatch that the policies of the writer handle, and a blank line behind it is trivia. The next line is
///   the `Before` of the next entry or item or the `BeforeColon` of an explicit key with a colon. A comment line in
///   between ends the empty lines of the scalar (l-trail-comments), so a blank line behind a comment line is trivia.
///   The exception is a blank line in the `End` of the last collection, in `AfterRoot` or in the prelude of the next
///   document. Only trivia and markers follow there, and the parser reads a line that holds a byte order mark as the
///   blank line that ends the scalar, and so a line that holds a tab behind a root scalar.
/// - A stream: `StreamPart.Trivia` does not stand in front of a document, and in a stream with a document
///   it stands behind an `ExtraEndMarker` or at the start of the stream in front of one only. The trivia behind
///   the `...` of a document belong to its `AfterEndMarker` (R3). An
///   `ExtraEndMarker` stands at the start of the stream or behind a part that ends with a `...`, because
///   behind a document without one the parser reads the `...` as the end marker of that document (R3).
///
/// A node has at most one tag and one anchor by its type, so no check is needed for that. The tree holds no
/// columns and no line numbers, so the parts of R4 and R6 that depend on them are not checked.
///
/// The walk keeps its own stack, so a tree of any depth is safe.
module Validate =

    let private isBlockCollection (node: YamlNode) =
        match node.Kind with
        | YamlNodeKind.Mapping collection -> collection.Style = CollectionStyle.Block
        | YamlNodeKind.Sequence collection -> collection.Style = CollectionStyle.Block
        | _ -> false

    /// A block collection without a property has no token of its own on the line of the indicator.
    let private isBareBlockCollection (node: YamlNode) =
        isBlockCollection node && node.Properties.Tag.IsNone && node.Properties.Anchor.IsNone

    let private isBlockScalar (scalar: YamlScalar) =
        match scalar.Style with
        | ScalarStyle.Block _ -> true
        | _ -> false

    let private followingOf (separation: Separation option) : Trivia list =
        match separation with
        | Some (Separation.NewLine lineBreak) -> lineBreak.Following
        | _ -> []

    /// True when the first of the trivia lines is a blank line.
    let internal startsWithBlankLine (lines: Trivia list) : bool =
        match lines with
        | Trivia.BlankLine _ :: _ -> true
        | _ -> false

    /// The lines that follow the root of the document in the text, up to the next marker: `AfterRoot`, then the
    /// trivia of the prelude of the next document when no `...` stands between. The writer prints a `...` in front of
    /// a prelude that holds a directive, so such a prelude adds no line.
    let internal linesBehindRoot (document: YamlDocument) (next: YamlDocument option) : Trivia list =
        if not document.AfterRoot.IsEmpty then
            document.AfterRoot
        elif document.EndMarker.IsSome then
            []
        else
            match next with
            | Some following ->
                let hasDirective =
                    following.Prelude
                    |> List.exists (fun item ->
                        match item with
                        | PreludeItem.Directive _ -> true
                        | PreludeItem.PreludeTrivia _ -> false)
                if hasDirective then
                    []
                else
                    [ for item in following.Prelude do
                          match item with
                          | PreludeItem.PreludeTrivia trivia -> yield trivia
                          | PreludeItem.Directive _ -> () ]
            | None -> []

    /// True when the first line that follows the root of the document in the text is a blank line.
    let internal blankLineFollowsRoot (document: YamlDocument) (next: YamlDocument option) : bool =
        startsWithBlankLine (linesBehindRoot document next)

    /// True when the first line behind the root of the document that is not blank is a comment line.
    let internal commentFollowsRoot (document: YamlDocument) (next: YamlDocument option) : bool =
        let mutable rest = linesBehindRoot document next
        while startsWithBlankLine rest do
            rest <- rest.Tail
        match rest with
        | Trivia.FullLineComment _ :: _ -> true
        | _ -> false

    // ------------------------------------------------------------------------------------------
    // Characters

    /// nb-char ([27]) with the tab: the printable characters of [1] without the line breaks and the byte order mark.
    /// It tells what a comment, a plain scalar or a block scalar can hold. A surrogate half is no character of its
    /// own, so `allChars` pairs the halves.
    let internal isNbChar (c: char) : bool =
        let code = int c
        code = 0x09
        || (code >= 0x20 && code <= 0x7E)
        || code = 0x85
        || (code >= 0xA0 && code <= 0xD7FF)
        || (code >= 0xE000 && code <= 0xFFFD && code <> 0xFEFF)
        || code >= 0x10000

    /// ns-char ([34]): an nb-char that is no blank.
    let internal isNsChar (c: char) : bool = isNbChar c && c <> ' ' && c <> '\t'

    let private isFlowIndicatorChar (c: char) = c = ',' || c = '[' || c = ']' || c = '{' || c = '}'

    /// ns-anchor-char ([102]): an ns-char that is no flow indicator.
    let internal isAnchorChar (c: char) : bool = isNsChar c && not (isFlowIndicatorChar c)

    /// True when every character of the text passes `accept`. A pair of surrogate halves counts as one character that
    /// passes, and a half without its partner fails.
    let internal allChars (accept: char -> bool) (text: string) : bool =
        let mutable ok = true
        let mutable i = 0
        while ok && i < text.Length do
            let code = int text.[i]
            if code >= 0xD800 && code <= 0xDBFF then
                if i + 1 < text.Length && int text.[i + 1] >= 0xDC00 && int text.[i + 1] <= 0xDFFF then i <- i + 2 else ok <- false
            elif code >= 0xDC00 && code <= 0xDFFF then
                ok <- false
            elif accept text.[i] then
                i <- i + 1
            else
                ok <- false
        ok

    /// True when the text can follow a `#` on a comment line: it holds nb-chars only.
    let internal isCommentText (text: string) : bool = allChars isNbChar text

    /// True when the text is a name behind `&` or `*`: one or more ns-anchor-chars.
    let internal isAnchorName (text: string) : bool = text.Length > 0 && allChars isAnchorChar text

    /// True when the text is a directive name or a directive parameter: one or more ns-chars ([82], [83]).
    let internal isDirectiveWord (text: string) : bool = text.Length > 0 && allChars isNsChar text

    // ------------------------------------------------------------------------------------------
    // The chomping of a block scalar

    let private isBlockChar (c: char) = c = '\n' || isNbChar c

    /// True when every character of the text except the line feed is an nb-char, so the text can stand in a plain
    /// scalar or in a block scalar.
    let internal isBlockText (text: string) : bool = allChars isBlockChar text

    /// The chomping that a block scalar gets for its value, and whether the stored chomping fits the value. The line
    /// feeds at the end of the value decide: none give Strip, one gives Clip, and several, or a value of line feeds
    /// only, give Keep. A value without a final line feed fits Clip and Keep only when the scalar ends the text
    /// (`endsText`), where the writer leaves out the last line break ([165] b-chomped-last takes the end of the input
    /// in place of a line feed). The result is the stored chomping when it fits and the one that the value needs
    /// otherwise.
    let internal blockChomping (value: string) (stored: ChompingMode) (endsText: bool) : ChompingMode * bool =
        let n = value.Length
        let mutable bodyEnd = n
        while bodyEnd > 0 && value.[bodyEnd - 1] = '\n' do
            bodyEnd <- bodyEnd - 1
        let breaks = n - bodyEnd
        let onlyBreaks = n > 0 && bodyEnd = 0
        let derived =
            if breaks = 0 then ChompingMode.Strip
            elif onlyBreaks then ChompingMode.Keep
            elif breaks = 1 then ChompingMode.Clip
            else ChompingMode.Keep
        let fits =
            match stored with
            | ChompingMode.Strip -> breaks = 0
            | ChompingMode.Clip -> n = 0 || (breaks = 1 && not onlyBreaks) || endsText
            | ChompingMode.Keep -> n = 0 || breaks >= 1 || endsText
        ((if fits then stored else derived), fits)

    /// The rule of the chomping Keep for a scalar that more text follows (R5). It applies to a stored Keep that is the
    /// chomping written for the value: the value can stand in a block scalar and the Keep fits it. The empty lines
    /// behind such a scalar belong to its value, so a blank trivia line directly behind it would change the value. A
    /// stored Keep that does not fit is a style mismatch that the policies of the writer handle, and a blank line behind
    /// it is trivia. The check of `Validate`, the planning of a block scalar and `Writer.endsWithKeptLines` use this rule.
    let internal storedKeepApplies (style: ScalarStyle) (value: string) : bool =
        match style with
        | ScalarStyle.Block (_, ChompingMode.Keep, _) -> isBlockText value && snd (blockChomping value ChompingMode.Keep false)
        | _ -> false

    let private keepText =
        "a blank line directly behind a block scalar with the stored chomping Keep belongs to the value of the scalar when more content follows and the Keep fits the value, so the trivia cannot hold it (l-chomped-empty, [165] to [169])"

    let private followingText =
        "the trivia belong to the Before of the first entry or item of the block collection behind the line break (R2)"

    let private missingText =
        "the content behind the indicator is missing, so the trivia belong to the Before of the next entry or item, or to BeforeColon behind a question mark when a colon follows (R2)"

    let private endText =
        "the trivia behind a block collection belong to its End only when the collection is the last node of its document, and otherwise to the Before of the next entry or item (R4)"

    let private startsLaterText =
        "a block collection starts on a later line than its last property, so a line break follows that property (R1)"

    let private colonText =
        "a block collection without a property starts on a later line than the colon of an implicit entry, so a line break follows the colon (R1)"

    let private compactText =
        "a block collection that continues the line of an indicator has no trivia in front of its first entry or item, because they would stand in front of the indicator (R2, R4)"

    let private commentText =
        "a comment holds a character that a comment line cannot hold: a line break, a control character, a byte order mark or half of a surrogate pair (nb-char, [27])"

    let private nameText =
        "a name behind & or * has one or more ns-anchor-char characters ([102]), so it holds no blank, line break, flow indicator, control character or byte order mark"

    let private colonValueText =
        "an entry with a value has a colon, because the value indicator is omitted only behind a key without a value"

    let private tagText =
        "a named tag has a suffix of one or more characters, because a shorthand tag is a handle followed by ns-tag-char characters ([99]) and a verbatim tag holds a URI ([98])"

    let private endBlankText =
        "a blank line goes with the comment that follows it, so the End of a nested block collection ends with a comment unless it holds the last comment behind the last node (R4)"

    let private endAfterText =
        "the lines behind the last comment of the trivia behind the last node stay in the End that holds that comment, and trivia without a comment stay in the End of the root collection (R4)"

    let private implicitColonText =
        "the entry of a block mapping without a question mark has a colon, because its key stands on one line with the colon"

    /// Reports a comment whose text a comment line cannot hold.
    let private checkComment (report: string -> string -> unit) (slot: string) (comment: YamlComment option) =
        match comment with
        | Some found when not (isCommentText found.Text) -> report slot commentText
        | _ -> ()

    /// Reports the comment lines of the trivia that a comment line cannot hold.
    let private checkTrivia (report: string -> string -> unit) (slot: string) (lines: Trivia list) =
        let mutable rest = lines
        let mutable index = 0
        while not rest.IsEmpty do
            match rest.Head with
            | Trivia.FullLineComment found -> checkComment report (slot + "[" + string index + "]") (Some found)
            | Trivia.BlankLine _ -> ()
            rest <- rest.Tail
            index <- index + 1

    /// Reports the comment of a separation and the comment lines behind it.
    let private checkSeparation (report: string -> string -> unit) (slot: string) (separation: Separation option) =
        match separation with
        | Some (Separation.NewLine lineBreak) ->
            checkComment report (slot + ".Comment") lineBreak.Comment
            checkTrivia report (slot + ".Following") lineBreak.Following
        | _ -> ()

    let private isWordChar (c: char) =
        (c >= '0' && c <= '9') || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '-'

    let private isHexChar (c: char) =
        (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

    let private isUriPunctuation (c: char) =
        c = '#' || c = ';' || c = '/' || c = '?' || c = ':' || c = '@' || c = '&' || c = '=' || c = '+' || c = '$'
        || c = '_' || c = '.' || c = '~' || c = '*' || c = '\'' || c = '(' || c = ')' || c = '!' || c = ',' || c = '['
        || c = ']'

    /// A tag handle ([89] to [92]): `!`, `!!` or one or more word characters between two `!`.
    let private isTagHandle (handle: string) : bool =
        if handle = "!" || handle = "!!" then
            true
        elif handle.Length >= 3 && handle.[0] = '!' && handle.[handle.Length - 1] = '!' then
            let mutable ok = true
            for i in 1 .. handle.Length - 2 do
                if not (isWordChar handle.[i]) then ok <- false
            ok
        else
            false

    /// The prefix of a %TAG directive ([93] to [95]): URI characters, a `%` followed by two hexadecimal digits, and no
    /// flow indicator at the start.
    let private isTagPrefix (prefix: string) : bool =
        let mutable ok = prefix.Length > 0 && not (prefix.[0] = ',' || prefix.[0] = '[' || prefix.[0] = ']')
        let mutable i = 0
        while ok && i < prefix.Length do
            let c = prefix.[i]
            if c = '%' then
                if i + 2 < prefix.Length && isHexChar prefix.[i + 1] && isHexChar prefix.[i + 2] then i <- i + 3 else ok <- false
            elif isWordChar c || isUriPunctuation c then
                i <- i + 1
            else
                ok <- false
        ok

    /// Reports the directives and the trivia of the prelude of a document. A document has at most one %YAML
    /// directive, the major version is 1, and a tag handle is declared once.
    let private checkPrelude (errors: ResizeArray<string>) (prefix: string) (prelude: PreludeItem list) =
        let handles = HashSet<string>()
        let mutable versions = 0
        let mutable index = 0
        for item in prelude do
            let slot = prefix + "Prelude[" + string index + "]"
            let report (suffix: string) (text: string) = errors.Add(slot + suffix + ": " + text)
            match item with
            | PreludeItem.PreludeTrivia (Trivia.FullLineComment found) -> checkComment report "" (Some found)
            | PreludeItem.PreludeTrivia (Trivia.BlankLine _) -> ()
            | PreludeItem.Directive directive ->
                checkComment report ".TrailingComment" directive.TrailingComment
                match directive.Kind with
                | DirectiveKind.YamlVersion (major, minor) ->
                    versions <- versions + 1
                    if versions > 1 then
                        report ".Kind" "a document has at most one %YAML directive"
                    if major <> 1 || minor < 0 || minor > 999999999 then
                        report ".Kind" "the %YAML directive names a version 1.x with a minor number of at most nine digits"
                | DirectiveKind.TagDirective (handle, tagPrefix) ->
                    if not (isTagHandle handle) then
                        report ".Kind" "a tag handle is !, !! or a word between two !"
                    elif not (handles.Add handle) then
                        report ".Kind" "a tag handle is declared once in a document"
                    if not (isTagPrefix tagPrefix) then
                        report ".Kind" "a tag prefix holds URI characters and no flow indicator at the start"
                | DirectiveKind.ReservedDirective (name, arguments) ->
                    if not (isDirectiveWord name) || name = "YAML" || name = "TAG" then
                        report ".Kind" "a reserved directive has a name of ns-char characters that is neither YAML nor TAG"
                    for argument in arguments do
                        if not (isDirectiveWord argument) || argument.[0] = '#' then
                            report ".Kind" "a parameter of a reserved directive has ns-char characters and does not start with a # that would begin a comment"
            index <- index + 1

    /// Reports the trivia in front of the first entry or item of a block collection without a property that continues
    /// the line of its indicator, as in `- - a`, `- a: 1` and `? - a`.
    let private checkCompact (report: string -> string -> unit) (slot: string) (content: YamlNode) =
        if isBareBlockCollection content then
            match content.Kind with
            | YamlNodeKind.Mapping collection ->
                match collection.Items with
                | first :: _ when not first.Before.IsEmpty -> report (slot + ".entries[0].Before") compactText
                | _ -> ()
            | YamlNodeKind.Sequence collection ->
                match collection.Items with
                | first :: _ when not first.Before.IsEmpty -> report (slot + ".items[0].Before") compactText
                | _ -> ()
            | _ -> ()

    /// Reports the trivia behind an indicator when the content behind it is a block collection without a
    /// property or is missing.
    let private checkFollowing (report: string -> string -> unit) (slot: string) (separation: Separation option) (content: YamlNode) =
        if not (followingOf separation).IsEmpty then
            if isBareBlockCollection content then
                report slot followingText
            elif YamlNode.isMissingValue content then
                report slot missingText

    /// What follows the last line of a node. `noBlank`: no blank line comes first. `blankThenContent`: a blank line comes
    /// first and more content follows it, namely the next entry or item, or the colon of an explicit key. `blankInTail`:
    /// a blank line comes first and only trivia and markers follow, as in the End of the last collection of a document,
    /// in AfterRoot and in the prelude of the next document.
    [<Literal>]
    let private noBlank = 0

    [<Literal>]
    let private blankThenContent = 1

    [<Literal>]
    let private blankInTail = 2

    let private walkNode (errors: ResizeArray<string>) (prefix: string) (canEnd: bool) (blankAfterRoot: int) (root: YamlNode) =
        // The first flag tells whether the node is an item or a key or value of a flow collection. The second tells
        // whether the node is the last node of the text of its document, where the trivia behind it can go to its End.
        // The number tells what follows the last line of the node, see `noBlank`.
        let stack = ResizeArray<YamlNode * NodePath.PathInfo * bool * bool * int>()
        stack.Add((root, NodePath.rootPath, false, canEnd, blankAfterRoot))
        while stack.Count > 0 do
            let last = stack.Count - 1
            let (node, path, inFlow, isLastNode, blankFollows) = stack.[last]
            stack.RemoveAt last
            // The slot of the node is built only for an error, so a tree without errors builds no path.
            let report (suffix: string) (text: string) =
                errors.Add(prefix + NodePath.slotOf path + suffix + ": " + text)

            let layoutCount (name: PropertyName) =
                node.PropertyLayout |> List.filter (fun placement -> placement.Property = name) |> List.length
            if layoutCount PropertyName.Tag <> (if node.Properties.Tag.IsSome then 1 else 0) then
                report ".PropertyLayout" "the layout names the tag once when the node has a tag and never otherwise"
            if layoutCount PropertyName.Anchor <> (if node.Properties.Anchor.IsSome then 1 else 0) then
                report ".PropertyLayout" "the layout names the anchor once when the node has an anchor and never otherwise"

            if (isBlockCollection node || YamlNode.isPropertyOnly node) && not node.PropertyLayout.IsEmpty then
                let placements = List.toArray node.PropertyLayout
                let lastIndex = placements.Length - 1
                if not (followingOf (Some placements.[lastIndex].After)).IsEmpty then
                    report
                        (".PropertyLayout[" + string lastIndex + "].After.Following")
                        (if isBlockCollection node then followingText else missingText)
                if isBlockCollection node && placements.[lastIndex].After = Separation.Inline then
                    report (".PropertyLayout[" + string lastIndex + "].After") startsLaterText

            checkComment report ".TrailingComment" node.TrailingComment
            match node.Properties.Anchor with
            | Some name when not (isAnchorName name) -> report ".Properties.Anchor" nameText
            | _ -> ()
            match node.Properties.Tag with
            | Some (YamlTag.Named name) when name.Suffix.Length = 0 -> report ".Properties.Tag" tagText
            | _ -> ()
            let mutable placementIndex = 0
            for placement in node.PropertyLayout do
                checkSeparation report (".PropertyLayout[" + string placementIndex + "].After") (Some placement.After)
                placementIndex <- placementIndex + 1

            match node.Kind with
            | YamlNodeKind.Alias name ->
                if node.Properties.Tag.IsSome || node.Properties.Anchor.IsSome || not node.PropertyLayout.IsEmpty then
                    report ".Properties" "an alias has no properties"
                if not (isAnchorName name) then
                    report "" nameText
            | YamlNodeKind.Scalar scalar ->
                checkComment report ".HeaderComment" scalar.HeaderComment
                if scalar.HeaderComment.IsSome && not (isBlockScalar scalar) then
                    report ".HeaderComment" "only a block scalar has a header comment (R8)"
                if node.TrailingComment.IsSome then
                    if isBlockScalar scalar then
                        report ".TrailingComment" "a block scalar takes the comment behind its header as HeaderComment (R8)"
                    elif YamlNode.isMissingValue node then
                        report ".TrailingComment" "a missing value has no trailing comment, the comment belongs to the separation behind the indicator (R1)"
                    elif YamlNode.isPropertyOnly node && not inFlow then
                        report ".TrailingComment" "a node with properties and no content has no trailing comment outside a flow collection, the comment belongs to the After of its last property (R1)"
                if blankFollows = blankThenContent && storedKeepApplies scalar.Style scalar.Value then
                    report ".Style" keepText
            | YamlNodeKind.Mapping collection ->
                checkComment report ".OpeningComment" collection.OpeningComment
                checkTrivia report ".End" collection.End
                if collection.Style = CollectionStyle.Block then
                    if node.TrailingComment.IsSome then
                        report ".TrailingComment" "a block mapping has no closing token to hang a trailing comment on (R8)"
                    if collection.OpeningComment.IsSome then
                        report ".OpeningComment" "only a flow collection has an opening comment"
                    if collection.Items.IsEmpty then
                        report "" "an empty collection is a flow collection"
                    if not collection.End.IsEmpty && not isLastNode then
                        report ".End" endText
                let entries = List.toArray collection.Items
                for i in 0 .. entries.Length - 1 do
                    let entry = entries.[i]
                    let at = ".entries[" + string i + "]"
                    checkTrivia report (at + ".Before") entry.Before
                    checkSeparation report (at + ".Question") entry.Question
                    checkTrivia report (at + ".BeforeColon") entry.BeforeColon
                    checkSeparation report (at + ".Colon") entry.Colon
                    if entry.Colon.IsNone && not (YamlNode.isMissingValue entry.Value) then
                        report (at + ".Colon") colonValueText
                    elif collection.Style = CollectionStyle.Block && entry.Question.IsNone && entry.Colon.IsNone then
                        report (at + ".Colon") implicitColonText
                    checkFollowing report (at + ".Question.Following") entry.Question entry.Key
                    checkFollowing report (at + ".Colon.Following") entry.Colon entry.Value
                    if collection.Style = CollectionStyle.Block && entry.Question.IsNone && entry.Colon = Some Separation.Inline && isBareBlockCollection entry.Value then
                        report (at + ".Colon") colonText
                    if collection.Style = CollectionStyle.Block && entry.Question = Some Separation.Inline then
                        checkCompact report (at + ".Key") entry.Key
                    if collection.Style = CollectionStyle.Block && entry.Question.IsSome && entry.Colon = Some Separation.Inline then
                        checkCompact report (at + ".Value") entry.Value
                    if entry.Colon.IsNone && not entry.BeforeColon.IsEmpty then
                        report (at + ".BeforeColon") "an entry without a colon has no gap before it, the trivia belong to the Before of the next entry or item (R7)"
                    elif collection.Style = CollectionStyle.Block && entry.Question.IsNone && not entry.BeforeColon.IsEmpty then
                        report (at + ".BeforeColon") "the key of an entry without a question mark stands on one line with its colon in a block mapping (R7)"
                let childInFlow = collection.Style = CollectionStyle.Flow
                for i in entries.Length - 1 .. -1 .. 0 do
                    let at = ".entries[" + string i + "]"
                    // The text of the last entry ends with its value, or with its key when no colon follows.
                    let lastEntry = isLastNode && not childInFlow && i = entries.Length - 1
                    // The first line behind the entry is the Before of the next entry, the End of the collection, or whatever follows the collection.
                    // A block scalar in a flow collection is written in another style, so the rule does not apply there.
                    let blankBehind =
                        if childInFlow then noBlank
                        elif i < entries.Length - 1 then (if startsWithBlankLine entries.[i + 1].Before then blankThenContent else noBlank)
                        elif not collection.End.IsEmpty then (if startsWithBlankLine collection.End then blankInTail else noBlank)
                        else blankFollows
                    // A key that a colon follows ends where the gap in front of the colon starts.
                    let blankBehindKey =
                        if entries.[i].Colon.IsSome then (if startsWithBlankLine entries.[i].BeforeColon then blankThenContent else noBlank)
                        else blankBehind
                    stack.Add((entries.[i].Value, NodePath.child path (at + ".Value"), childInFlow, lastEntry && entries.[i].Colon.IsSome, blankBehind))
                    stack.Add((entries.[i].Key, NodePath.child path (at + ".Key"), childInFlow, lastEntry && entries.[i].Colon.IsNone, blankBehindKey))
            | YamlNodeKind.Sequence collection ->
                checkComment report ".OpeningComment" collection.OpeningComment
                checkTrivia report ".End" collection.End
                if collection.Style = CollectionStyle.Block then
                    if node.TrailingComment.IsSome then
                        report ".TrailingComment" "a block sequence has no closing token to hang a trailing comment on (R8)"
                    if collection.OpeningComment.IsSome then
                        report ".OpeningComment" "only a flow collection has an opening comment"
                    if collection.Items.IsEmpty then
                        report "" "an empty collection is a flow collection"
                    if not collection.End.IsEmpty && not isLastNode then
                        report ".End" endText
                let items = List.toArray collection.Items
                for i in 0 .. items.Length - 1 do
                    let item = items.[i]
                    let at = ".items[" + string i + "]"
                    checkTrivia report (at + ".Before") item.Before
                    checkSeparation report (at + ".Dash") item.Dash
                    if collection.Style = CollectionStyle.Block && item.Dash.IsNone then
                        report (at + ".Dash") "an item of a block sequence has a dash"
                    elif collection.Style = CollectionStyle.Flow && item.Dash.IsSome then
                        report (at + ".Dash") "an item of a flow sequence has no dash"
                    checkFollowing report (at + ".Dash.Following") item.Dash item.Node
                    if collection.Style = CollectionStyle.Block && item.Dash = Some Separation.Inline then
                        checkCompact report (at + ".Node") item.Node
                let childInFlow = collection.Style = CollectionStyle.Flow
                for i in items.Length - 1 .. -1 .. 0 do
                    let lastItem = isLastNode && not childInFlow && i = items.Length - 1
                    let blankBehind =
                        if childInFlow then noBlank
                        elif i < items.Length - 1 then (if startsWithBlankLine items.[i + 1].Before then blankThenContent else noBlank)
                        elif not collection.End.IsEmpty then (if startsWithBlankLine collection.End then blankInTail else noBlank)
                        else blankFollows
                    stack.Add((items.[i].Node, NodePath.child path (".items[" + string i + "].Node"), childInFlow, lastItem, blankBehind))

    /// The last line of a trivia list is a comment line.
    let private endsWithComment (lines: Trivia list) : bool =
        match List.tryLast lines with
        | Some (Trivia.FullLineComment _) -> true
        | _ -> false

    let private hasComment (lines: Trivia list) : bool =
        lines
        |> List.exists (fun line ->
            match line with
            | Trivia.FullLineComment _ -> true
            | Trivia.BlankLine _ -> false)

    /// Rule R4 for the trivia behind the last node of a document, as far as the tree decides it. The Ends of the block
    /// collections along the last node of the text, the innermost first, hold one run of trivia. A blank line goes with
    /// the comment that follows it, and the blank lines behind the last comment go to the End of that comment. A run
    /// without a comment goes to the End of the root collection. So a nested End ends with a comment unless it holds the
    /// last comment, and no End behind the one with the last comment holds anything.
    let private checkEnds (errors: ResizeArray<string>) (prefix: string) (root: YamlNode) =
        // The Ends from the root inwards, with the places of the collections.
        let ends = ResizeArray<NodePath.PathInfo * Trivia list>()
        let mutable current = root
        let mutable path = NodePath.rootPath
        let mutable walking = true
        while walking do
            match current.Kind with
            | YamlNodeKind.Mapping collection when collection.Style = CollectionStyle.Block && not collection.Items.IsEmpty ->
                ends.Add((path, collection.End))
                let index = collection.Items.Length - 1
                let last = List.last collection.Items
                if last.Colon.IsSome then
                    current <- last.Value
                    path <- NodePath.child path (".entries[" + string index + "].Value")
                else
                    current <- last.Key
                    path <- NodePath.child path (".entries[" + string index + "].Key")
            | YamlNodeKind.Sequence collection when collection.Style = CollectionStyle.Block && not collection.Items.IsEmpty ->
                ends.Add((path, collection.End))
                let index = collection.Items.Length - 1
                current <- (List.last collection.Items).Node
                path <- NodePath.child path (".items[" + string index + "].Node")
            | _ -> walking <- false
        // The outermost End with a comment holds the last comment of the run.
        let mutable lastComment = -1
        for k in ends.Count - 1 .. -1 .. 0 do
            if hasComment (snd ends.[k]) then lastComment <- k
        for k in 0 .. ends.Count - 1 do
            let (at, lines) = ends.[k]
            if not lines.IsEmpty then
                let slot = prefix + NodePath.slotOf at + ".End: "
                if lastComment < 0 then
                    if k > 0 then errors.Add(slot + endAfterText)
                elif k < lastComment then
                    errors.Add(slot + endAfterText)
                elif k > lastComment && not (endsWithComment lines) then
                    errors.Add(slot + endBlankText)

    let private walkDocument (errors: ResizeArray<string>) (prefix: string) (next: YamlDocument option) (document: YamlDocument) =
        let root = document.Root
        let followedByDocument = next.IsSome
        let report (slot: string) (text: string) = errors.Add(prefix + slot + ": " + text)
        let markerComment (marker: DocumentMarker option) =
            match marker with
            | Some found -> found.TrailingComment
            | None -> None
        checkPrelude errors prefix document.Prelude
        checkComment report "StartMarker.TrailingComment" (markerComment document.StartMarker)
        checkComment report "EndMarker.TrailingComment" (markerComment document.EndMarker)
        checkTrivia report "BeforeRoot" document.BeforeRoot
        checkTrivia report "AfterRoot" document.AfterRoot
        checkTrivia report "AfterEndMarker" document.AfterEndMarker
        if not document.BeforeRoot.IsEmpty && isBareBlockCollection root then
            errors.Add(prefix + "BeforeRoot: the trivia belong to the Before of the first entry or item of the root collection (R3)")
        // Behind the root only trivia and markers follow, see `blankInTail`.
        let blankAfterRoot = if blankLineFollowsRoot document next then blankInTail else noBlank
        // The trivia behind the last node of a document go to the Prelude of the next document when no `...` stands between.
        let canEnd = not (followedByDocument && document.EndMarker.IsNone)
        walkNode errors prefix canEnd blankAfterRoot root
        if canEnd then checkEnds errors prefix root
        if not document.AfterRoot.IsEmpty then
            if isBlockCollection root then
                errors.Add(prefix + "AfterRoot: the trivia behind a block collection belong to its End (R3)")
            elif YamlNode.isMissingValue root then
                errors.Add(prefix + "AfterRoot: a document whose root is missing keeps all its trivia in BeforeRoot (R3)")
            elif followedByDocument && document.EndMarker.IsNone then
                errors.Add(prefix + "AfterRoot: the trivia in front of the next document belong to its Prelude when no `...` stands between (R3)")
        if not document.AfterEndMarker.IsEmpty then
            if document.EndMarker.IsNone then
                errors.Add(prefix + "AfterEndMarker: a document without a `...` keeps the trivia behind its root in AfterRoot or in the End of the root collection (R3)")
            elif followedByDocument then
                errors.Add(prefix + "AfterEndMarker: the trivia in front of the next document belong to that document (R3)")

    /// The problems of a document, in the order of the tree. The list is empty when the document is valid.
    let document (document: YamlDocument) : string list =
        let errors = ResizeArray<string>()
        walkDocument errors "" None document
        List.ofSeq errors

    /// The problems of a stream, in the order of its parts. The list is empty when the stream is valid.
    let stream (stream: YamlStream) : string list =
        let errors = ResizeArray<string>()
        let parts = List.toArray stream.Parts
        let hasDocument =
            parts
            |> Array.exists (fun part ->
                match part with
                | StreamPart.Document _ -> true
                | _ -> false)
        // The first part behind each part that is not trivia, so that a run of trivia parts looks at the part
        // behind the run.
        let following: StreamPart option[] = Array.create parts.Length None
        let mutable next: StreamPart option = None
        for i in parts.Length - 1 .. -1 .. 0 do
            following.[i] <- next
            match parts.[i] with
            | StreamPart.Trivia _ -> ()
            | other -> next <- Some other
        let endsWithMarker (part: StreamPart) =
            match part with
            | StreamPart.Document document -> document.EndMarker.IsSome
            | StreamPart.ExtraEndMarker _ -> true
            | StreamPart.Trivia _ -> false
        // The last part before the current one that is not trivia.
        let mutable previous: StreamPart option = None
        for i in 0 .. parts.Length - 1 do
            let at = "Parts[" + string i + "]"
            match parts.[i] with
            | StreamPart.Document document ->
                let nextDocument =
                    match following.[i] with
                    | Some (StreamPart.Document next) -> Some next
                    | _ -> None
                walkDocument errors (at + ".") nextDocument document
                previous <- Some parts.[i]
            | StreamPart.ExtraEndMarker marker ->
                checkComment (fun slot text -> errors.Add(at + "." + slot + ": " + text)) "TrailingComment" marker.TrailingComment
                // Behind a document without a `...` the marker would be the end marker of that document.
                match previous with
                | Some part when not (endsWithMarker part) ->
                    errors.Add(at + ": an extra end marker stands at the start of the stream or behind a `...`, and behind a document without an end marker it is the end marker of that document (R3)")
                | _ -> ()
                previous <- Some parts.[i]
            | StreamPart.Trivia trivia ->
                match trivia with
                | Trivia.FullLineComment found -> checkComment (fun slot text -> errors.Add(at + slot + ": " + text)) "" (Some found)
                | Trivia.BlankLine _ -> ()
                let beforeDocument =
                    match following.[i] with
                    | Some (StreamPart.Document _) -> true
                    | _ -> false
                let behindExtraEndMarker =
                    match previous with
                    | Some (StreamPart.ExtraEndMarker _) -> true
                    | _ -> false
                let behindEndMarker =
                    match previous with
                    | Some (StreamPart.Document document) -> document.EndMarker.IsSome
                    | _ -> false
                // A `...` that ends no document makes an ExtraEndMarker, and the trivia in front of it belong to no document
                // either, but only at the start of the stream. Behind a document without a `...` the marker would end that
                // document, and behind another extra marker `behindExtraEndMarker` has accepted the trivia already.
                let beforeExtraEndMarker =
                    match following.[i] with
                    | Some (StreamPart.ExtraEndMarker _) -> previous.IsNone
                    | _ -> false
                if beforeDocument then
                    errors.Add(at + ": trivia in front of a document belong to that document (R3)")
                elif behindEndMarker then
                    errors.Add(at + ": trivia behind the `...` of a document belong to its AfterEndMarker (R3)")
                elif hasDocument && not behindExtraEndMarker && not beforeExtraEndMarker then
                    errors.Add(at + ": trivia stand in the stream only behind an extra end marker or at the start of the stream in front of one, and otherwise they belong to a document (R3)")
        List.ofSeq errors
