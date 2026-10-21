module YAMLicious.Regex

[<Literal>]
let CommentPattern =
    #if FABLE_COMPILER_PYTHON
    "\<c f=(?P<comment>[0-9]+)\/\>"
    #endif
    #if FABLE_COMPILER_JAVASCRIPT || FABLE_COMPILER_TYPESCRIPT
    "<c f=(?<comment>[0-9]+)\/>"
    #endif
    #if !FABLE_COMPILER
    "\<c f=(?<comment>[0-9]+)\/\>"
    #endif

let AnchorPattern =
    #if FABLE_COMPILER_PYTHON
    "&(?P<anchor>[^\\s\\[\\]{},:]+)"
    #else
    "&(?<anchor>[^\\s\\[\\]{},:]+)"
    #endif

let AliasPattern =
    #if FABLE_COMPILER_PYTHON
    "^\\*(?P<alias>[^\\s\\[\\]{},:]+)$"
    #else
    "^\\*(?<alias>[^\\s\\[\\]{},:]+)$"
    #endif

let VerbatimTagPattern =
    #if FABLE_COMPILER_PYTHON
    "^!<(?P<tag>[^>]+)>"
    #else
    "^!<(?<tag>[^>]+)>"
    #endif

let KeyPattern =
    #if FABLE_COMPILER_PYTHON
    $"^(?P<key>(?:[!&]\S*\s+)*[^!&\s\{{\[].*?):\s*({CommentPattern})?$"
    #else
    $"^(?<key>(?:[!&]\S*\s+)*[^!&\s\\{{\\[].*?):\s*({CommentPattern})?$"
    #endif

let ExplicitKeyPattern = "^\\?\\s*$"

let ExplicitKeyWithValuePattern = "^\\?\\s+(.+)$"

let ExplicitValuePattern = "^:\\s*(.*)$"

[<Literal>]
let KeyValuePattern =
    #if FABLE_COMPILER_PYTHON
    "^(?P<key>(?:[!&]\S*\s+)*[^!&\s\{{\[].*?):\s+(?P<value>.*)$"
    #else
    "^(?<key>(?:[!&]\S*\s+)*[^!&\s\{{\[].*?):\s+(?<value>(.*))$"
    #endif

let LineCommentPattern =
    $"^{CommentPattern}$"

let ValuePattern =
    #if FABLE_COMPILER_PYTHON
    $"^(?P<value>.*?)\s*?({CommentPattern})?$"
    #else
    $"^(?<value>.*?)\s*?({CommentPattern})?$"
    #endif

[<Literal>]
let SequenceMinusPattern =
    #if FABLE_COMPILER_PYTHON
    "^-(\s+(?P<value>.*))?$"
    #else
    "^-(\s+(?<value>.*))?$"
    #endif

let FlowStyleArrayPattern =
    #if FABLE_COMPILER_PYTHON
    $"^\[(?P<inlineSequence>.*)\]\s*?({CommentPattern})?$"
    #else
    $"^\[(?<inlineSequence>.*)\]\s*?({CommentPattern})?$"
    #endif

// Backward compatibility alias
let InlineSequencePattern = FlowStyleArrayPattern

let FlowStyleObjectPattern =
    #if FABLE_COMPILER_PYTHON
    $"^\{{(?P<inlineSequence>.*)\}}\s*?({CommentPattern})?$"
    #else
    $"^\{{(?<inlineSequence>.*)\}}\s*?({CommentPattern})?$"
    #endif

// Backward compatibility alias
let InlineJSONPattern = FlowStyleObjectPattern

let SequenceOpenerPattern =
    #if FABLE_COMPILER_PYTHON
    $"^\[\s*({CommentPattern})?$"
    #else
    $"^\[\s*({CommentPattern})?$"
    #endif

let SequenceCloserPattern =
    #if FABLE_COMPILER_PYTHON
    $"^\]\s*({CommentPattern})?$"
    #else
    $"^\]\s*({CommentPattern})?$"
    #endif

let FlowStyleObjectOpenerPattern =
    #if FABLE_COMPILER_PYTHON
    $"^(?P<key>[^\{{\[]+):\s+\{{\s*({CommentPattern})?$"
    #else
    $"^(?<key>[^\{{\[]+):\s+\{{\s*({CommentPattern})?$"
    #endif

// Backward compatibility alias
let JSONOpenerPattern = FlowStyleObjectOpenerPattern

let FlowStyleObjectCloserPattern =
    #if FABLE_COMPILER_PYTHON
    $"^\}}\s*({CommentPattern})?$"
    #else
    $"^\}}\s*({CommentPattern})?$"
    #endif

// Backward compatibility alias
let JSONCloserPattern = FlowStyleObjectCloserPattern

[<Literal>]
let StringReplacementPattern =
    #if FABLE_COMPILER_PYTHON
    "\<s f=(?P<index>[0-9]+)\/\>"
    #endif
    #if FABLE_COMPILER_JAVASCRIPT || FABLE_COMPILER_TYPESCRIPT
    "<s f=(?<index>[0-9]+)\/>"
    #endif
    #if !FABLE_COMPILER
    "\<s f=(?<index>[0-9]+)\/\>"
    #endif
