namespace YAMLicious

/// A piece of a scalar between two of its line breaks. The text of the piece is `Value.[First .. Limit - 1]`.
/// `Behind` is the break behind the piece, and None for the last piece.
type internal LayoutPiece =
    { First: int
      Limit: int
      Behind: LayoutKind option }

/// The check of a layout against a value and a style, and the pieces that the writer replays.
///
/// A layout fits when the writer can write the lines again so that the reader gets the same value and the same
/// layout. The check looks at the value and the style only. It keeps the offsets of the boundaries, so it neither
/// shifts a boundary nor wraps the text. The writer adds the checks that depend on the place of the scalar, for example
/// that a plain scalar in a flow collection holds no flow indicator. The cost is linear in the length of the value and
/// the number of boundaries.
module internal Layout =

    let private isBlank (c: char) = c = ' ' || c = '\t'

    /// True when the index stands between the two halves of a surrogate pair. A piece that ends there would split a
    /// character. A Python string indexes by code point, so it has no such index.
    let private splitsPair (value: string) (index: int) =
        index > 0
        && index < value.Length
        && int value.[index - 1] >= 0xD800
        && int value.[index - 1] <= 0xDBFF
        && int value.[index] >= 0xDC00
        && int value.[index] <= 0xDFFF

    /// The number of characters of the value that the boundary replaces: the space of a fold without empty lines,
    /// and one line feed for each empty line otherwise.
    let consumed (kind: LayoutKind) : int =
        match kind with
        | LayoutKind.Fold 0 -> 1
        | LayoutKind.Fold lines -> lines
        | LayoutKind.EscapedBreak lines -> lines

    /// Cuts the value at the boundaries. Returns None when a boundary has a kind that the style cannot have, lies
    /// outside the value or inside the text that the previous boundary replaces, replaces text of another kind or
    /// splits a surrogate pair.
    let private cut (layout: ScalarLayout) (value: string) (emptyLinesOk: bool) (escapedOk: bool) : LayoutPiece[] option =
        let n = value.Length
        let pieces = ResizeArray<LayoutPiece>()
        let mutable start = 0
        let mutable ok = true
        let mutable rest = layout.Boundaries
        while ok && not rest.IsEmpty do
            let boundary = rest.Head
            let take = consumed boundary.Kind
            let kindOk =
                match boundary.Kind with
                | LayoutKind.Fold 0 -> true
                | LayoutKind.Fold lines -> emptyLinesOk && lines > 0
                | LayoutKind.EscapedBreak lines -> escapedOk && lines >= 0
            if not kindOk || boundary.At < start || boundary.At + take > n || splitsPair value boundary.At then
                ok <- false
            else
                match boundary.Kind with
                | LayoutKind.Fold 0 -> if value.[boundary.At] <> ' ' then ok <- false
                | _ ->
                    for i in boundary.At .. boundary.At + take - 1 do
                        if value.[i] <> '\n' then ok <- false
            if ok then
                pieces.Add { First = start; Limit = boundary.At; Behind = Some boundary.Kind }
                start <- boundary.At + take
                rest <- rest.Tail
        if ok then
            pieces.Add { First = start; Limit = n; Behind = None }
            Some(pieces.ToArray())
        else
            None

    let private isEmptyPiece (piece: LayoutPiece) = piece.Limit <= piece.First

    /// True when the piece starts with a character that a line of a folded scalar keeps: no blank and no line feed.
    let private startsWithText (value: string) (piece: LayoutPiece) =
        not (isEmptyPiece piece) && not (isBlank value.[piece.First]) && value.[piece.First] <> '\n'

    let private endsWithText (value: string) (piece: LayoutPiece) =
        not (isEmptyPiece piece) && not (isBlank value.[piece.Limit - 1]) && value.[piece.Limit - 1] <> '\n'

    /// A fold in a plain scalar and in single quotes turns into a space, so the pieces on both sides hold text that
    /// the reader keeps: a blank at the end of a line or at the start of the next one would go away, and a line feed
    /// would add an empty line. Single quotes have a first and a last piece that may be empty, because a quote can
    /// stand alone on a line.
    let private foldsOk (value: string) (pieces: LayoutPiece[]) (edgesMayBeEmpty: bool) : bool =
        let mutable ok = true
        for i in 0 .. pieces.Length - 2 do
            let left = pieces.[i]
            let right = pieces.[i + 1]
            if not (endsWithText value left || (edgesMayBeEmpty && i = 0 && isEmptyPiece left)) then ok <- false
            if not (startsWithText value right || (edgesMayBeEmpty && i + 1 = pieces.Length - 1 && isEmptyPiece right)) then ok <- false
        ok

    let private isEscapedBreak (kind: LayoutKind option) : bool =
        match kind with
        | Some (LayoutKind.EscapedBreak _) -> true
        | _ -> false

    /// A piece of a double-quoted scalar is empty when two breaks follow each other. A line that holds only a
    /// backslash is such a piece. After a fold or an escaped break the reader takes an empty line for line feeds, so
    /// only an escaped break may stand behind an empty piece. The first and the last piece may be empty.
    let private doubleOk (pieces: LayoutPiece[]) : bool =
        let mutable ok = true
        for i in 1 .. pieces.Length - 2 do
            if isEmptyPiece pieces.[i] && not (isEscapedBreak pieces.[i].Behind) then ok <- false
        ok

    /// A fold in a folded block scalar turns into a space only between two lines that start with text. The line in
    /// front of the fold may end in blanks, which are content, and the line behind it starts with a character that is
    /// no blank. A line that starts with a blank is more indented, and no fold happens next to it. The check looks at
    /// the start of the whole line in front of the fold, which a line feed of the value starts.
    let private foldedOk (value: string) (pieces: LayoutPiece[]) : bool =
        let mutable ok = true
        let mutable lineFirst = -1
        for i in 0 .. pieces.Length - 1 do
            let piece = pieces.[i]
            for k in piece.First .. piece.Limit - 1 do
                if value.[k] = '\n' then lineFirst <- -1
                elif lineFirst < 0 then lineFirst <- k
            match piece.Behind with
            | Some _ ->
                if isEmptyPiece piece || value.[piece.Limit - 1] = '\n' then ok <- false
                if lineFirst < 0 || isBlank value.[lineFirst] then ok <- false
                if not (startsWithText value pieces.[i + 1]) then ok <- false
            | None -> ()
        ok

    /// The pieces to replay when the layout fits the value in the style, and None otherwise. A literal block scalar
    /// has no layout.
    let tryPlan (layout: ScalarLayout) (value: string) (style: ScalarStyle) : LayoutPiece[] option =
        match style with
        | ScalarStyle.Plain ->
            match cut layout value false false with
            | Some pieces when foldsOk value pieces false -> Some pieces
            | _ -> None
        | ScalarStyle.SingleQuoted ->
            match cut layout value false false with
            | Some pieces when foldsOk value pieces true -> Some pieces
            | _ -> None
        | ScalarStyle.DoubleQuoted ->
            match cut layout value true true with
            | Some pieces when doubleOk pieces -> Some pieces
            | _ -> None
        | ScalarStyle.Block (BlockScalarStyle.Folded, _, _) ->
            match cut layout value false false with
            | Some pieces when foldedOk value pieces -> Some pieces
            | _ -> None
        | ScalarStyle.Block (BlockScalarStyle.Literal, _, _) -> None

    /// True when the layout fits the value in the style.
    let fits (layout: ScalarLayout) (value: string) (style: ScalarStyle) : bool = (tryPlan layout value style).IsSome
