module Tests.Flow

// The tests of the flow grammar: flow sequences and mappings, flow pairs, the forms of an entry, the layouts over several lines, plain scalars and
// properties inside a collection, the rejections, the depth and the time. The expected trees are built with the constructors and compared with
// YamlNode.equalIgnoringSpans. The comments of a flow collection and the slots of the rules R6, R7 and R8 stand in Tests.Comments.
//
// The yaml@2.6.1 value of each input stands in a comment above its test or in the tables, which a script produced from yaml@2.6.1 itself and not
// from the parser. The tables hold the inputs of the old tests that read a flow collection: Tests.FlowTokens (the inputs of the old tokenizer),
// Tests.YamlRead, earlier tokenizer tests and the N1 cases of Tests.Parser. The first column names the old test.
//
// Changes against the old tests, each from the new model or a ruling of the specification:
// - A flow collection or an alias as the key of an entry is valid now. The reader before 2.0.0 rejected both, because it kept scalar keys only (Tests.Keys).
// - `{?}` and `[?]` are errors: [142] and [150] ask for a separator behind the question mark, and a flow indicator is none. yaml@2.6.1 reads `{"":null}`.
// - The old tests that read the merged lines of the scanner stage have no counterpart. The parser reads the lines of a
//   flow collection directly, and the tests check the tree, the spans and the lines of the errors.
// - The old tests of the writer (Writer.Formatting.mkInlineSequence and the round trips) are in Tests.Writer and Tests.RoundTrip.

open Fable.Pyxpecto
open YAMLicious

let private flowValues : (string * string * string) list = [
    "FlowTokens simple object", "{key: value}", "{\"key\":\"value\"}"
    "FlowTokens simple array", "[a, b, c]", "[\"a\",\"b\",\"c\"]"
    "FlowTokens nested object", "{outer: {inner: value}}", "{\"outer\":{\"inner\":\"value\"}}"
    "FlowTokens string with spaces", "{key: Hello World}", "{\"key\":\"Hello World\"}"
    "FlowTokens double-quoted string with comma", "[\"a,b\", c]", "[\"a,b\",\"c\"]"
    "FlowTokens double-quoted string in object value", "{key: \"hello, world\"}", "{\"key\":\"hello, world\"}"
    "FlowTokens empty object", "{}", "{}"
    "FlowTokens empty array", "[]", "[]"
    "FlowTokens nested array inside object", "{key: [a, b]}", "{\"key\":[\"a\",\"b\"]}"
    "FlowTokens nested object inside array", "[{a: 1}, {b: 2}]", "[{\"a\":1},{\"b\":2}]"
    "FlowTokens colon without a blank behind it is part of a plain scalar", "[a:b, 12:45, http://x]", "[\"a:b\",\"12:45\",\"http://x\"]"
    "FlowTokens colon with a flow indicator or the end behind it separates", "{a:, b:}", "{\"a\":null,\"b\":null}"
    "FlowTokens colon with a flow indicator or the end behind it separates (pair)", "[a: b]", "[{\"a\":\"b\"}]"
    "FlowTokens properties, aliases and the question mark of an explicit key are tokens", "[!!str a, &x b, *x, ? c: d]", "[\"a\",\"b\",\"b\",{\"c\":\"d\"}]"
    "FlowTokens properties, aliases and the question mark of an explicit key are tokens (question mark)", "[?a, ? b]", "[\"?a\",{\"b\":null}]"
    "FlowTokens properties, aliases and the question mark of an explicit key are tokens (marks)", "[a ! b, a & b]", "[\"a ! b\",\"a & b\"]"
    "FlowTokens verbatim tags", "[!<tag:yaml.org,2002:str> a, !<tag:yaml.org,2002:str>, c]", "[\"a\",\"\",\"c\"]"
    "FlowTokens verbatim tag in front of a key", "{!<tag:yaml.org,2002:str> k: v}", "{\"k\":\"v\"}"
    "FlowTokens deeply nested structure (5+ levels)", "a: {b: {c: {d: {e: {f: value}}}}}", "{\"a\":{\"b\":{\"c\":{\"d\":{\"e\":{\"f\":\"value\"}}}}}}"
    "FlowTokens empty nested structures", "key: {a: {}, b: []}", "{\"key\":{\"a\":{},\"b\":[]}}"
    "FlowTokens mixed flow and block style", "outer:\n  flow: {a: 1, b: 2}\n  block:\n    - item1\n    - item2", "{\"outer\":{\"flow\":{\"a\":1,\"b\":2},\"block\":[\"item1\",\"item2\"]}}"
    "FlowTokens unicode in flow-style", "data: {name: \"\u65E5\u672C\u8A9E\", emoji: \"\uD83C\uDF89\"}", "{\"data\":{\"name\":\"\u65E5\u672C\u8A9E\",\"emoji\":\"\uD83C\uDF89\"}}"
    "FlowTokens array with nested objects in flow-style", "items: [{id: 1, name: first}, {id: 2, name: second}]", "{\"items\":[{\"id\":1,\"name\":\"first\"},{\"id\":2,\"name\":\"second\"}]}"
    "YamlRead KeyValue InlineSequence + Comment", "Say: [Hello, World] # 420 blaze it", "{\"Say\":[\"Hello\",\"World\"]}"
    "YamlRead KeyValue empty inline sequence stays empty sequence", "value: []", "{\"value\":[]}"
    "YamlRead SequenceofSequences", "\n- [v1, v2, v3]\n- [v4, v5, v6]\n- [v7, v8, v9]\n", "[[\"v1\",\"v2\",\"v3\"],[\"v4\",\"v5\",\"v6\"],[\"v7\",\"v8\",\"v9\"]]"
    "YamlRead MultilineSequenceSquare", "\n[\n  v1,\n  v2,\n  v3\n]\n", "[\"v1\",\"v2\",\"v3\"]"
    "YamlRead JSONMappingsInline", "\nMark McGwire: {hr: 65, avg: 0.278}\n", "{\"Mark McGwire\":{\"hr\":65,\"avg\":0.278}}"
    "YamlRead JSONMappingsMultiline", "\nSammy Sosa: {\n    hr: 63,\n    avg: 0.288,\n}", "{\"Sammy Sosa\":{\"hr\":63,\"avg\":0.288}}"
    "YamlRead NestedFlowStyleComplex", "requirements: {\n  DockerRequirement: {\n    dockerImageId: \"workstation1\",\n    dockerFile: { $include: \"SampleBoxContent/Dockerfile\" }\n  },\n  SubworkflowFeatureRequirement: {},\n  NetworkAccess: { networkAccess: true }\n}", "{\"requirements\":{\"DockerRequirement\":{\"dockerImageId\":\"workstation1\",\"dockerFile\":{\"$include\":\"SampleBoxContent/Dockerfile\"}},\"SubworkflowFeatureRequirement\":{},\"NetworkAccess\":{\"networkAccess\":true}}}"
    "YamlRead NestedFlowStyleCompact", "requirements: {\n  DockerRequirement: { dockerImageId: \"workstation1\", dockerFile: { $include: \"SampleBoxContent/Dockerfile\" } },\n  SubworkflowFeatureRequirement: {},\n  NetworkAccess: { networkAccess: true }\n}", "{\"requirements\":{\"DockerRequirement\":{\"dockerImageId\":\"workstation1\",\"dockerFile\":{\"$include\":\"SampleBoxContent/Dockerfile\"}},\"SubworkflowFeatureRequirement\":{},\"NetworkAccess\":{\"networkAccess\":true}}}"
    "YamlRead NestedFlowStyleInlineArrays", "requirements: {\n  InitialWorkDirRequirement: { listing: [{entryname: \"own\",entry: \"$(inputs.docDirectory)\", writable: true},{ entry: \"$(inputs.resultDirectory)\", writable: true }] },\n  EnvVarRequirement: { envDef: [{ envName: \"COLOR_DISABLE\", envValue: \"true\" },{ envName: \"TEST\", envValue: \"false\" }] },\n  SubworkflowFeatureRequirement: {}\n}", "{\"requirements\":{\"InitialWorkDirRequirement\":{\"listing\":[{\"entryname\":\"own\",\"entry\":\"$(inputs.docDirectory)\",\"writable\":true},{\"entry\":\"$(inputs.resultDirectory)\",\"writable\":true}]},\"EnvVarRequirement\":{\"envDef\":[{\"envName\":\"COLOR_DISABLE\",\"envValue\":\"true\"},{\"envName\":\"TEST\",\"envValue\":\"false\"}]},\"SubworkflowFeatureRequirement\":{}}}"
    "YamlRead Empty inline flow object", "key: {}", "{\"key\":{}}"
    "YamlRead Multiline flow sequence with trailing comment on value line", "data: [a, b] # after", "{\"data\":[\"a\",\"b\"]}"
    "YamlRead Multiline flow sequence as block sequence item parses structurally", "items:\n  - [\n      a,\n      b\n    ]", "{\"items\":[[\"a\",\"b\"]]}"
    "YamlRead Multiline flow object as block sequence item parses structurally", "items:\n  - {\n      a: b\n    }", "{\"items\":[{\"a\":\"b\"}]}"
    "YamlRead Root multiline flow object parses structurally", "{\n  a: b\n}", "{\"a\":\"b\"}"
    "YamlRead Comment inside multiline flow sequence stays a comment node", "data:\n  [\n    a, # one\n    b\n  ]", "{\"data\":[\"a\",\"b\"]}"
    "YamlRead Multiline flow object with closer inside mapping", "data:\n  k: {\n    a: 1\n  }", "{\"data\":{\"k\":{\"a\":1}}}"
    "YamlRead Multiline flow sequence accepts trailing comma and blank line", "arr:\n  [\n\n    a,\n    b,\n  ]", "{\"arr\":[\"a\",\"b\"]}"
    "YamlRead CWL-style inline flow object with nested flow value", "requirements: { LoadListingRequirement: { loadListing: \"no_listing\" } }", "{\"requirements\":{\"LoadListingRequirement\":{\"loadListing\":\"no_listing\"}}}"
    "YamlRead Inline flow object as value with space before brace", "key: {a: 1, b: 2}", "{\"key\":{\"a\":1,\"b\":2}}"
    "YamlRead Inline flow array as value", "key: [1, 2, 3]", "{\"key\":[1,2,3]}"
    "YamlRead Empty flow object as mapping value (CWL pattern)", "requirements:\n  InlineJavascriptRequirement: {}", "{\"requirements\":{\"InlineJavascriptRequirement\":{}}}"
    "YamlRead Empty flow array as mapping value", "outputs: {}", "{\"outputs\":{}}"
    "YamlRead Comment before a dedented flow closer stays in the flow collection", "data:\n  [\n    a,\n    b\n# last\n  ]\nz: 1", "{\"data\":[\"a\",\"b\"],\"z\":1}"
    "YamlRead Commented-out last entry of a multi-line flow sequence", "baseCommand:\n  [\n    python,\n#   old,\n  ]", "{\"baseCommand\":[\"python\"]}"
    "YamlRead Comment before a flow sequence closer that is not dedented stays in the flow collection (1)", "a:\n  - [\n    1,\n# c\n    ]\n", "{\"a\":[[1]]}"
    "YamlRead Comment before a flow sequence closer that is not dedented stays in the flow collection (2)", "a:\n  - [\n    1,\n  # c\n    ]\n", "{\"a\":[[1]]}"
    "YamlRead Comment before a flow mapping closer that is not dedented stays in the flow collection", "- {\n    x: 1,\n# c\n    }\n", "[{\"x\":1}]"
    "YamlRead A flow collection over several lines reads in every layout (1)", "data: [a, b,\n  c]", "{\"data\":[\"a\",\"b\",\"c\"]}"
    "YamlRead A flow collection over several lines reads in every layout (2)", "data: [[a, b, c],\n       [d, e, f],\n       [g, h, i]]", "{\"data\":[[\"a\",\"b\",\"c\"],[\"d\",\"e\",\"f\"],[\"g\",\"h\",\"i\"]]}"
    "YamlRead A flow collection over several lines reads in every layout (3)", "data:\n  [a, b,\n   c]", "{\"data\":[\"a\",\"b\",\"c\"]}"
    "YamlRead A flow collection over several lines reads in every layout (4)", "- [a, b,\n   c]", "[[\"a\",\"b\",\"c\"]]"
    "YamlRead A flow collection over several lines reads in every layout (5)", "data: {a: 1,\n  b: 2}", "{\"data\":{\"a\":1,\"b\":2}}"
    "YamlRead A flow collection over several lines reads in every layout (6)", "k: [\n  a,\n  b\n]", "{\"k\":[\"a\",\"b\"]}"
    "YamlRead A flow collection over several lines reads in every layout (7)", "expressionLib: [\n  \"function f() { return 1; }\",\n  \"var x = 'a';\"\n]", "{\"expressionLib\":[\"function f() { return 1; }\",\"var x = 'a';\"]}"
    "YamlRead A flow collection over several lines reads in every layout (8)", "k:\n    [\n      a\n  ]", "{\"k\":[\"a\"]}"
    "YamlRead A flow mapping entry without a value maps to nothing (1)", "- parameter: { 5 }", "[{\"parameter\":{\"5\":null}}]"
    "YamlRead A flow mapping entry without a value maps to nothing (2)", "C: [list, {p1: \"v_1\", ..., in: \"v_n\"}]", "{\"C\":[\"list\",{\"p1\":\"v_1\",\"...\":null,\"in\":\"v_n\"}]}"
    "YamlRead A flow mapping entry without a value maps to nothing (3)", "{k:}", "{\"k\":null}"
    "YamlRead A flow mapping entry without a value maps to nothing (4)", "{a, b: c}", "{\"a\":null,\"b\":\"c\"}"
    "YamlRead A flow mapping entry without a value maps to nothing (5)", "k: {a: 1,\n  b}", "{\"k\":{\"a\":1,\"b\":null}}"
    "YamlRead A pair in a flow sequence is a mapping with one entry (1)", "[a: b]", "[{\"a\":\"b\"}]"
    "YamlRead A pair in a flow sequence is a mapping with one entry (2)", "[a: b, c]", "[{\"a\":\"b\"},\"c\"]"
    "YamlRead A pair in a flow sequence is a mapping with one entry (3)", "[a: ]", "[{\"a\":null}]"
    "YamlRead A pair in a flow sequence is a mapping with one entry (4)", "[a: b,\n  c: d]", "[{\"a\":\"b\"},{\"c\":\"d\"}]"
    "YamlRead A pair in a flow sequence is a mapping with one entry (5)", "[a:b, 12:45]", "[\"a:b\",\"12:45\"]"
    "YamlRead A colon in a flow mapping separates a key only behind a blank, a flow indicator or a quote (1)", "{a:b, c: d}", "{\"a:b\":null,\"c\":\"d\"}"
    "YamlRead A colon in a flow mapping separates a key only behind a blank, a flow indicator or a quote (2)", "{a:\"b\"}", "{\"a:\\\"b\\\"\":null}"
    "YamlRead A colon in a flow mapping separates a key only behind a blank, a flow indicator or a quote (3)", "{\"a\":b, 'c':'d'}", "{\"a\":\"b\",\"c\":\"d\"}"
    "YamlRead A colon in a flow mapping separates a key only behind a blank, a flow indicator or a quote (4)", "{time: 12:45, url: http://x}", "{\"time\":\"12:45\",\"url\":\"http://x\"}"
    "YamlRead Comments at the start, in the middle and at the end of a flow sequence over several lines (1)", "k: [ # c1\n  a,\n  b\n]", "{\"k\":[\"a\",\"b\"]}"
    "YamlRead Comments at the start, in the middle and at the end of a flow sequence over several lines (2)", "k: [\n  a, # c2\n  b\n]", "{\"k\":[\"a\",\"b\"]}"
    "YamlRead Comments at the start, in the middle and at the end of a flow sequence over several lines (3)", "k: [\n  a,\n  b\n  # c3\n]", "{\"k\":[\"a\",\"b\"]}"
    "YamlRead Comments at the start, in the middle and at the end of a flow sequence over several lines (4)", "k: [\n  # c1\n  a,\n  # c2\n  b # c3\n]", "{\"k\":[\"a\",\"b\"]}"
    "YamlRead Comments at the start, in the middle and at the end of a flow sequence over several lines (5)", "k: [\n  \"a\", # c1\n  \"b\" # c2\n]", "{\"k\":[\"a\",\"b\"]}"
    "YamlRead Comments at the start, in the middle and at the end of a flow sequence over several lines (6)", "k: [\n  a\n] # c4", "{\"k\":[\"a\"]}"
    "YamlRead Comments in a flow mapping over several lines stay entries of the mapping (1)", "k: {\n  # c1\n  a: 1, # c2\n  b: 2\n  # c3\n}", "{\"k\":{\"a\":1,\"b\":2}}"
    "YamlRead Comments in a flow mapping over several lines stay entries of the mapping (2)", "k: {a: # c\n  1}", "{\"k\":{\"a\":1}}"
    "YamlRead Flow collections nested over several lines (1)", "[\n  [a, b,\n   c],\n  {d: e,\n   f: g}\n]", "[[\"a\",\"b\",\"c\"],{\"d\":\"e\",\"f\":\"g\"}]"
    "YamlRead Flow collections nested over several lines (2)", "k: [\n  [a],\n  {b: [c,\n    d]}\n]", "{\"k\":[[\"a\"],{\"b\":[\"c\",\"d\"]}]}"
    "YamlRead A flow collection behind properties reads as a collection (1)", "k: !!seq [\n  a\n]", "{\"k\":[\"a\"]}"
    "YamlRead A flow collection behind properties reads as a collection (2)", "k: &x {a: 1,\n  b: 2}", "{\"k\":{\"a\":1,\"b\":2}}"
    "YamlRead A flow collection behind properties reads as a collection (3)", "k: !!map {}", "{\"k\":{}}"
    "YamlRead A flow collection behind properties reads as a collection (4)", "- !!seq [a]", "[[\"a\"]]"
    "YamlRead A flow collection behind properties reads as a collection (5)", "!!seq [a,\n  b]", "[\"a\",\"b\"]"
    "YamlRead A plain scalar over several lines in a flow collection folds (1)", "[\n  a\n  b\n]", "[\"a b\"]"
    "YamlRead A plain scalar over several lines in a flow collection folds (2)", "[a\n\n b]", "[\"a\\nb\"]"
    "YamlRead A plain scalar over several lines in a flow collection folds (3)", "k: [\n  a\n  b,\n  c\n  d\n]", "{\"k\":[\"a b\",\"c d\"]}"
    "YamlRead A plain scalar over several lines in a flow collection folds (4)", "[a\n b, c]", "[\"a b\",\"c\"]"
    "YamlRead An expressionLib flow sequence of a CWL requirement reads with a trailing comma and comments (1)", "requirements:\n  - class: InlineJavascriptRequirement\n    expressionLib: [\n      \"function f(a){ return a; }\", # c1\n      \"function g(b){ return b; }\", # c2\n    ]\n", "{\"requirements\":[{\"class\":\"InlineJavascriptRequirement\",\"expressionLib\":[\"function f(a){ return a; }\",\"function g(b){ return b; }\"]}]}"
    "YamlRead An expressionLib flow sequence of a CWL requirement reads with a trailing comma and comments (2)", "requirements:\n  - class: InlineJavascriptRequirement\n    expressionLib: [\n      \"function f(a){ return a; }\",\n      \"function g(b){ return b; }\",\n    ]\n", "{\"requirements\":[{\"class\":\"InlineJavascriptRequirement\",\"expressionLib\":[\"function f(a){ return a; }\",\"function g(b){ return b; }\"]}]}"
    "YamlRead An expressionLib flow sequence of a CWL requirement reads with a trailing comma and comments (3)", "requirements:\n  - class: InlineJavascriptRequirement\n    expressionLib: [\n      # first\n      \"function f(a){ return a; }\",\n      # second\n      \"function g(b){ return b; }\"\n    ]\n", "{\"requirements\":[{\"class\":\"InlineJavascriptRequirement\",\"expressionLib\":[\"function f(a){ return a; }\",\"function g(b){ return b; }\"]}]}"
    "YamlRead Flow collections over several lines as explicit keys and values (value)", "? a\n: [\n  b\n]", "{\"a\":[\"b\"]}"
    "YamlRead Properties inside a flow collection belong to the node behind them (1)", "k: [!!int \"2\", &a \"x\"]", "{\"k\":[2,\"x\"]}"
    "YamlRead Properties inside a flow collection belong to the node behind them (2)", "k: {&a \"q\": !t \"v\"}", "{\"k\":{\"q\":\"v\"}}"
    "YamlRead Properties inside a flow collection belong to the node behind them (3)", "k: [!!str a\n  b]", "{\"k\":[\"a b\"]}"
    "YamlRead Properties inside a flow collection belong to the node behind them (4)", "k: [!!str , a]", "{\"k\":[\"\",\"a\"]}"
    "YamlRead Properties inside a flow collection belong to the node behind them (5)", "k: {a: &x}", "{\"k\":{\"a\":null}}"
    "YamlRead Properties inside a flow collection belong to the node behind them (6)", "%TAG !e! tag:example.com,2000:\n---\nk: [!e!foo a]", "{\"k\":[\"a\"]}"
    "YamlRead Properties inside a flow collection belong to the node behind them (7)", "k: [!!str # c\n  a]", "{\"k\":[\"a\"]}"
    "YamlRead A verbatim tag inside a flow collection is one tag (1)", "k: [!<tag:yaml.org,2002:str> a, c]", "{\"k\":[\"a\",\"c\"]}"
    "YamlRead A verbatim tag inside a flow collection is one tag (2)", "k: [!<tag:yaml.org,2002:str>, c]", "{\"k\":[\"\",\"c\"]}"
    "YamlRead A verbatim tag inside a flow collection is one tag (3)", "k: {!<tag:yaml.org,2002:str> k: v}", "{\"k\":{\"k\":\"v\"}}"
    "YamlRead Properties on a collection inside a flow collection follow the block rule", "k: [!!seq [a], !!map {b: c}]", "{\"k\":[[\"a\"],{\"b\":\"c\"}]}"
    "YamlRead An alias inside a flow collection is an alias (1)", "k: [&a x, *a, {k: *a}]", "{\"k\":[\"x\",\"x\",{\"k\":\"x\"}]}"
    "YamlRead An explicit key inside a flow collection (1)", "k: {? a: b}", "{\"k\":{\"a\":\"b\"}}"
    "YamlRead An explicit key inside a flow collection (2)", "k: [? a: b]", "{\"k\":[{\"a\":\"b\"}]}"
    "YamlRead An explicit key inside a flow collection (3)", "k: {? a}", "{\"k\":{\"a\":null}}"
    "YamlRead An explicit key inside a flow collection (4)", "{ ? foo :,\n  : bar }", "{\"foo\":null,\"\":\"bar\"}"
    "YamlRead An explicit key inside a flow collection (5)", "{\n? explicit: entry,\nimplicit: entry,\n?\n}", "{\"explicit\":\"entry\",\"implicit\":\"entry\",\"\":null}"
    "YamlRead An explicit key inside a flow collection (6)", "[? a : b, ? c]", "[{\"a\":\"b\"},{\"c\":null}]"
    "YamlRead An explicit key inside a flow collection (7)", "{? : b}", "{\"\":\"b\"}"
    "YamlRead An explicit key inside a flow collection (8)", "[ ? ]", "[{\"\":null}]"
    "YamlRead An explicit key inside a flow collection (9)", "{? !!str a : &x b}", "{\"a\":\"b\"}"
    "YamlRead An explicit key inside a flow collection (10)", "{?a: b}", "{\"?a\":\"b\"}"
    "YamlRead The key of an entry in a flow mapping or behind a question mark may span lines (1)", "{ matches\n% : 20 }", "{\"matches %\":20}"
    "YamlRead The key of an entry in a flow mapping or behind a question mark may span lines (2)", "[\n? foo\n bar : baz\n]", "[{\"foo bar\":\"baz\"}]"
    "YamlRead The key of an entry in a flow mapping or behind a question mark may span lines (3)", "{a\n b : c}", "{\"a b\":\"c\"}"
    "YamlRead The key of an entry in a flow mapping or behind a question mark may span lines (4)", "{? a\n b\n c}", "{\"a b c\":null}"
    "YamlRead The key of an entry in a flow mapping or behind a question mark may span lines (5)", "{a\n: b}", "{\"a\":\"b\"}"
    "YamlRead The key of an entry in a flow mapping or behind a question mark may span lines (6)", "k: {a\n  b : c}", "{\"k\":{\"a b\":\"c\"}}"
    "FlowTokens multiline flow content flattened", "a: 1,\nb: 2", "{\"a\":\"1,\",\"b\":2}"
    "FlowTokens string placeholder preservation", "{key: <s f=0/>}", "{\"key\":\"<s f=0/>\"}"
    "FlowTokens comment placeholder as separate token", "{key: <c f=1/>}", "{\"key\":\"<c f=1/>\"}"
    "FlowTokens comment placeholder ends a plain scalar and starts none (1)", "[a <c f=0/> b, <c f=1/> c]", "[\"a <c f=0/> b\",\"<c f=1/> c\"]"
    "FlowTokens comment placeholder ends a plain scalar and starts none (2)", "[<s f=0/> <c f=1/> ]", "[\"<s f=0/> <c f=1/>\"]"
    "FlowTokens text in front of a placeholder and the blanks around it stay in the token (1)", "[x <s f=0/> y, <s f=1/> tail]", "[\"x <s f=0/> y\",\"<s f=1/> tail\"]"
    "FlowTokens text in front of a placeholder and the blanks around it stay in the token (2)", "{a: x <c f=0/>}", "{\"a\":\"x <c f=0/>\"}"
    "FlowTokens colon directly behind a quoted scalar or a flow collection separates (1)", "{\"a\":b, \"c\" : c}", "{\"a\":\"b\",\"c\":\"c\"}"
    "FlowTokens properties, aliases and the question mark of an explicit key are tokens (keys)", "{&a \"p\": !t \"q\"}", "{\"p\":\"q\"}"
    "YamlRead Text in front of a placeholder inside a flow collection stays (1)", "k: [x <s f=0/> y]", "{\"k\":[\"x <s f=0/> y\"]}"
    "YamlRead Text in front of a placeholder inside a flow collection stays (2)", "k: {a: x <c f=0/>}", "{\"k\":{\"a\":\"x <c f=0/>\"}}"
    "YamlRead MultilineSequenceSquare accepts blank line after opener", "\n[\n\n  v1,\n  v2,\n  v3\n]\n", "[\"v1\",\"v2\",\"v3\"]"
    "YamlRead JSONMappingsMultiline accepts blank line after opener", "\nSammy Sosa: {\n\n    hr: 63,\n    avg: 0.288,\n}", "{\"Sammy Sosa\":{\"hr\":63,\"avg\":0.288}}"
    "YamlRead Multiline flow sequence in mapping context (1)", "data: [\n    a,\n    b\n  ]", "{\"data\":[\"a\",\"b\"]}"
    "YamlRead An empty sequence item reads as Nil and the empty flow mapping stays an empty object", "- {}\n- x", "[{},\"x\"]"
    "YamlRead InlineJavascriptRequirement empty flow object roundtrip", "InlineJavascriptRequirement: {}", "{\"InlineJavascriptRequirement\":{}}"
    "YamlRead CWL ExpressionTool roundtrip preserves block scalar (with a final line break)", "expression: >\n  ${ return { \"out\": name }; }\n", "{\"expression\":\"${ return { \\\"out\\\": name }; }\\n\"}"
    "Tokenizer quoted bars in a multiline flow collection are strings and the block after it opens", "a: [\n  '|', # one\n  \">\"\n]\nb: |\n  \"q\"\n", "{\"a\":[\"|\",\">\"],\"b\":\"\\\"q\\\"\\n\"}"
    "Tokenizer a multiline flow collection behind a tag reads its quotes and the block after it opens", "a: !!seq [\n  'b',\n  \"c\"\n]\nd: |\n  'x'\n", "{\"a\":[\"b\",\"c\"],\"d\":\"'x'\\n\"}"
    "Tokenizer a quote behind a bracket in a plain scalar (1)", "k: o[\"k\"]", "{\"k\":\"o[\\\"k\\\"]\"}"
    "Tokenizer a quote behind a bracket in a plain scalar (2)", "k: $(o['k'] || [1, 2])", "{\"k\":\"$(o['k'] || [1, 2])\"}"
    "Tokenizer a quote behind a bracket in a plain scalar (3)", "k: $(inputs[\"my-input\"].path)", "{\"k\":\"$(inputs[\\\"my-input\\\"].path)\"}"
    "Tokenizer a quote in a single-line flow collection opens a scalar (1)", "k: [\"a\", 'b']", "{\"k\":[\"a\",\"b\"]}"
    "Tokenizer a quote in a single-line flow collection opens a scalar (2)", "k: {\"a\": \"b\"}", "{\"k\":{\"a\":\"b\"}}"
    "Tokenizer a quote in a single-line flow collection opens a scalar (3)", "k: [x, \"y, z\"]", "{\"k\":[\"x\",\"y, z\"]}"
    "Tokenizer a quote in a single-line flow collection opens a scalar (4)", "- [\"a\", \"b\"]", "[[\"a\",\"b\"]]"
    "Tokenizer a quote behind a property at a node start opens a scalar", "k: !!str \"x\"", "{\"k\":\"x\"}"
    "Tokenizer a flow collection that spans lines keeps the quoted scalars on a line with two of them (1)", "k:\n  [\n    \"a\", \"b #c\"\n  ]", "{\"k\":[\"a\",\"b #c\"]}"
    "Tokenizer a flow collection that spans lines keeps the quoted scalars on a line with two of them (2)", "k: {\n  \"a\": 1, \"b #c\": 2\n}", "{\"k\":{\"a\":1,\"b #c\":2}}"
    "Tokenizer a flow collection that spans lines keeps the quoted scalars on a line with two of them (3)", "{\n  \"a\": 1, \"b #c\": 2\n}", "{\"a\":1,\"b #c\":2}"
    "Tokenizer a flow collection that spans lines keeps the quoted scalars on a line with two of them (4)", "k: {\n  \"a\": 1, \"b\\tc\": 2\n}", "{\"k\":{\"a\":1,\"b\\tc\":2}}"
    "Tokenizer a flow collection that spans lines keeps the quoted scalars on a line with two of them (5)", "- {\n  \"a\": 1, \"b\": 2\n  }", "[{\"a\":1,\"b\":2}]"
    "Tokenizer a flow collection over several lines becomes one line (1)", "data: [a, b,\n  c]", "{\"data\":[\"a\",\"b\",\"c\"]}"
    "Tokenizer a flow collection over several lines becomes one line (2)", "k: {a: 1,\n\n\n  b: 2}", "{\"k\":{\"a\":1,\"b\":2}}"
    "Tokenizer a flow collection over several lines becomes one line (3)", "[\n  [a, b],\n  [c]\n]", "[[\"a\",\"b\"],[\"c\"]]"
    "Tokenizer a flow collection nested in another one balances", "k: {a: [b]}", "{\"k\":{\"a\":[\"b\"]}}"
    "Tokenizer a balanced flow collection before a document marker is no error", "[a]\n---\n[b]", "[[\"a\"],[\"b\"]]"
    "Tokenizer a closer at the column of the owner (1)", "k: [\n  a\n]", "{\"k\":[\"a\"]}"
    "Tokenizer a closer at the column of the owner (2)", "k: [\n  a\n ]", "{\"k\":[\"a\"]}"
    "Tokenizer a closer at the column of the owner (3)", "[a,\nb]", "[\"a\",\"b\"]"
    "Tokenizer a closer at the column of the owner (4)", "- [\n a\n]", "[[\"a\"]]"
    "Tokenizer a closer at the column of the owner (5)", "k: [a,\n]", "{\"k\":[\"a\"]}"
    "Tokenizer a closer at the column of the owner (6)", "k: [ a,\n# c\n  b ]", "{\"k\":[\"a\",\"b\"]}"
    "Tokenizer a closer at the column of the owner (7)", "k:\n    [\n      a\n  ]", "{\"k\":[\"a\"]}"
    "Tokenizer a node in front of a flow collection (1)", "[\"a\"\n, b]", "[\"a\",\"b\"]"
    "Tokenizer a node in front of a flow collection (2)", "{\"a\"\n: b}", "{\"a\":\"b\"}"
    "Tokenizer a node in front of a flow collection (3)", "[a, [b], c: [d]]", "[\"a\",[\"b\"],{\"c\":[\"d\"]}]"
    "Tokenizer an entry of a flow collection can hold one colon (1)", "{a: b:c}", "{\"a\":\"b:c\"}"
    "Tokenizer an entry of a flow collection can hold one colon (2)", "[a: b, c: d]", "[{\"a\":\"b\"},{\"c\":\"d\"}]"
    "Tokenizer an entry of a flow collection can hold one colon (3)", "{a: [x: y], b: {c: d}}", "{\"a\":[{\"x\":\"y\"}],\"b\":{\"c\":\"d\"}}"
    "Tokenizer an alias may be followed by blanks, a comment, a comma or a closer (1)", "s: &a x\nl: [*a]", "{\"s\":\"x\",\"l\":[\"x\"]}"
    "Tokenizer a comment that ends a node inside a flow collection allows only a comma or a closer (1)", "[a, # c\n b]", "[\"a\",\"b\"]"
    "Tokenizer a comment that ends a node inside a flow collection allows only a comma or a closer (2)", "[a # c\n]", "[\"a\"]"
    "Tokenizer a question mark in a flow collection (1)", "{? a: b}", "{\"a\":\"b\"}"
    "Tokenizer a question mark in a flow collection (2)", "[? a : b]", "[{\"a\":\"b\"}]"
    "Tokenizer a question mark in a flow collection (3)", "{?a: b}", "{\"?a\":\"b\"}"
    "Tokenizer a question mark in a flow collection (4)", "[a, ? ]", "[\"a\",{\"\":null}]"
    "Tokenizer a trailing comma (1)", "[a,]", "[\"a\"]"
    "Tokenizer a trailing comma (2)", "{a: 1,}", "{\"a\":1}"
    "Tokenizer a trailing comma (3)", "{? ,}", "{\"\":null}"
    "Tokenizer a trailing comma (4)", "{a: , b}", "{\"a\":null,\"b\":null}"
    "Tokenizer a trailing comma (5)", "[!!str , a]", "[\"\",\"a\"]"
    "Tokenizer properties inside a flow collection are tokens of their own and stay in front of the folded scalar (1)", "k: [!!int \"2\", &a \"x\"]", "{\"k\":[2,\"x\"]}"
    "Tokenizer an indicator inside a plain scalar (1)", "k: a]", "{\"k\":\"a]\"}"
    "Tokenizer an indicator inside a plain scalar (2)", "k: a}", "{\"k\":\"a}\"}"
    "Tokenizer an indicator inside a plain scalar (3)", "k: a, b", "{\"k\":\"a, b\"}"
    "Tokenizer an indicator inside a plain scalar (4)", "k: -x", "{\"k\":\"-x\"}"
    "Tokenizer an indicator inside a plain scalar (5)", "k: ?x", "{\"k\":\"?x\"}"
    "Tokenizer an indicator inside a plain scalar (6)", "k: :x", "{\"k\":\":x\"}"
    "Tokenizer an indicator inside a plain scalar (7)", "[[]]", "[[]]"
    "Tokenizer an indicator inside a plain scalar (8)", "[-x]", "[\"-x\"]"
    "Tokenizer an indicator inside a plain scalar (9)", "[a, -1]", "[\"a\",-1]"
    "Tokenizer properties inside a flow collection are tokens of their own and stay in front of the folded scalar (2)", "k: [!!str a\n  b]", "{\"k\":[\"a b\"]}"
    "Tokenizer properties inside a flow collection are tokens of their own and stay in front of the folded scalar (3)", "k: {&a \"q\": !t \"v\"}", "{\"k\":{\"q\":\"v\"}}"
    "Tokenizer a colon in a flow collection separates a key only behind a blank, a flow indicator or a quote (1)", "{a:b}", "{\"a:b\":null}"
    "Tokenizer a colon in a flow collection separates a key only behind a blank, a flow indicator or a quote (2)", "[a:b, 12:45]", "[\"a:b\",\"12:45\"]"
    "Tokenizer a colon in a flow collection separates a key only behind a blank, a flow indicator or a quote (3)", "{\"a\":b}", "{\"a\":\"b\"}"
    "Tokenizer a colon in a flow collection separates a key only behind a blank, a flow indicator or a quote (4)", "{a: b}", "{\"a\":\"b\"}"
    "Tokenizer a colon in a flow collection separates a key only behind a blank, a flow indicator or a quote (5)", "{a:\"b\"}", "{\"a:\\\"b\\\"\":null}"
    "Tokenizer an unbalanced bracket at the start of a continuation line does not hide the blocks after it", "desc: see the list\n  [draft\nscript: |\n  echo 'hi'\nnext:\n  run: >\n    x # y\n", "{\"desc\":\"see the list [draft\",\"script\":\"echo 'hi'\\n\",\"next\":{\"run\":\"x # y\\n\"}}"
    "Tokenizer an unbalanced brace or a dash and bracket on a continuation line does not hide the block after it (1)", "desc: see the list\n  {bar\nscript: |\n  echo 'hi'\n", "{\"desc\":\"see the list {bar\",\"script\":\"echo 'hi'\\n\"}"
    "Tokenizer an unbalanced brace or a dash and bracket on a continuation line does not hide the block after it (2)", "desc: see the list\n  - [bar\nscript: |\n  echo 'hi'\n", "{\"desc\":\"see the list - [bar\",\"script\":\"echo 'hi'\\n\"}"
    "Tokenizer an unbalanced bracket in a sequence item does not hide the block after it", "- desc\n  [bar\n- |\n  echo 'hi'\n", "[\"desc [bar\",\"echo 'hi'\\n\"]"
    "Tokenizer brackets in plain text do not hide the block scalars after them", "a: foo [bar\nb: |\n  x\nc: x {y\nd: >\n  z\n", "{\"a\":\"foo [bar\",\"b\":\"x\\n\",\"c\":\"x {y\",\"d\":\"z\\n\"}"
    "Tokenizer a quote behind a comma in a plain scalar is text", "k: a, \"b\"", "{\"k\":\"a, \\\"b\\\"\"}"
    "Tokenizer a quote behind a brace in a plain scalar is text", "k: x {\"a\", \"b\"}", "{\"k\":\"x {\\\"a\\\", \\\"b\\\"}\"}"
    "Tokenizer an indicator character or a tag-like word inside a plain scalar starts no node (1)", "k: a- \"b\"", "{\"k\":\"a- \\\"b\\\"\"}"
    "Tokenizer an indicator character or a tag-like word inside a plain scalar starts no node (2)", "k: a? \"b\"", "{\"k\":\"a? \\\"b\\\"\"}"
    "Tokenizer an indicator character or a tag-like word inside a plain scalar starts no node (3)", "k: a:\"b\"", "{\"k\":\"a:\\\"b\\\"\"}"
    "Tokenizer an indicator character or a tag-like word inside a plain scalar starts no node (4)", "k: a !b \"c\"", "{\"k\":\"a !b \\\"c\\\"\"}"
    "Tokenizer a valid block scalar header and a bar or an angle bracket inside text are no error (1)", "k: >-\n  a\n", "{\"k\":\"a\"}"
    "Tokenizer a valid block scalar header and a bar or an angle bracket inside text are no error (2)", "- |\n  a\n", "[\"a\\n\"]"
    "Tokenizer a valid block scalar header and a bar or an angle bracket inside text are no error (3)", "k: |2\n   a\n", "{\"k\":\" a\\n\"}"
    "Tokenizer a valid block scalar header and a bar or an angle bracket inside text are no error (4)", "k: !!str |\n  a\n", "{\"k\":\"a\\n\"}"
    "Tokenizer a valid block scalar header and a bar or an angle bracket inside text are no error (5)", "? |\n  a\n: b\n", "{\"a\\n\":\"b\"}"
    "Tokenizer a valid block scalar header and a bar or an angle bracket inside text are no error (6)", "--- |\n  a\n", "\"a\\n\""
    "Tokenizer a valid block scalar header and a bar or an angle bracket inside text are no error (7)", "k: | # c\n  a\n", "{\"k\":\"a\\n\"}"
    "Tokenizer a valid block scalar header and a bar or an angle bracket inside text are no error (8)", "k: a | b", "{\"k\":\"a | b\"}"
    "Tokenizer a valid block scalar header and a bar or an angle bracket inside text are no error (9)", "k: a > b", "{\"k\":\"a > b\"}"
    "Tokenizer a valid block scalar header and a bar or an angle bracket inside text are no error (10)", "[a|b]", "[\"a|b\"]"
    "Tokenizer a valid block scalar header and a bar or an angle bracket inside text are no error (11)", "{a: x>y}", "{\"a\":\"x>y\"}"
    "Tokenizer a valid block scalar header and a bar or an angle bracket inside text are no error (12)", "[a, '|']", "[\"a\",\"|\"]"
    "Tokenizer a valid block scalar header and a bar or an angle bracket inside text are no error (13)", "k: \">\"", "{\"k\":\">\"}"
    "Tokenizer a colon inside a word, a quoted scalar or a comment, and a compact mapping are no error (1)", "k: a:b", "{\"k\":\"a:b\"}"
    "Tokenizer a colon inside a word, a quoted scalar or a comment, and a compact mapping are no error (2)", "k: http://x", "{\"k\":\"http://x\"}"
    "Tokenizer a colon inside a word, a quoted scalar or a comment, and a compact mapping are no error (3)", "k: \"a: b\"", "{\"k\":\"a: b\"}"
    "Tokenizer a colon inside a word, a quoted scalar or a comment, and a compact mapping are no error (4)", "k: 'a: b'", "{\"k\":\"a: b\"}"
    "Tokenizer a colon inside a word, a quoted scalar or a comment, and a compact mapping are no error (5)", "k: a #c: d", "{\"k\":\"a\"}"
    "Tokenizer a colon inside a word, a quoted scalar or a comment, and a compact mapping are no error (6)", "k: a :b", "{\"k\":\"a :b\"}"
    "Tokenizer a colon inside a word, a quoted scalar or a comment, and a compact mapping are no error (7)", "- k: v", "[{\"k\":\"v\"}]"
    "Tokenizer a colon inside a word, a quoted scalar or a comment, and a compact mapping are no error (8)", "- ? a", "[{\"a\":null}]"
    "Tokenizer a colon inside a word, a quoted scalar or a comment, and a compact mapping are no error (9)", "- - a: b", "[[{\"a\":\"b\"}]]"
    "Tokenizer a colon inside a word, a quoted scalar or a comment, and a compact mapping are no error (10)", "? a\n: b: c\n", "{\"a\":{\"b\":\"c\"}}"
    "Tokenizer a colon inside a word, a quoted scalar or a comment, and a compact mapping are no error (11)", "k: {a: b}", "{\"k\":{\"a\":\"b\"}}"
    "Tokenizer a colon inside a word, a quoted scalar or a comment, and a compact mapping are no error (12)", "- \"a\": b", "[{\"a\":\"b\"}]"
    "Tokenizer the lines of a plain scalar are one value that no stage reads as structure (1)", "k: a\n  - x", "{\"k\":\"a - x\"}"
    "Tokenizer the lines of a plain scalar are one value that no stage reads as structure (2)", "k: v\n  ? |\n  x", "{\"k\":\"v ? | x\"}"
    "Tokenizer the lines of a plain scalar are one value that no stage reads as structure (3)", "k: a\n  [x, \"y\"]", "{\"k\":\"a [x, \\\"y\\\"]\"}"
    "Tokenizer the lines of a plain scalar are one value that no stage reads as structure (4)", "- k: v\n      ? |\n- b", "[{\"k\":\"v ? |\"},\"b\"]"
    "Tokenizer a plain scalar can be the explicit key or the explicit value (1)", "? a\n  b\n: c", "{\"a b\":\"c\"}"
    "Tokenizer a plain scalar can be the explicit key or the explicit value (2)", "? key\n: a\n  b", "{\"key\":\"a b\"}"
    "Tokenizer tags and anchors before a header (1)", "k: !<tag:yaml.org,2002:str> |\n  x \"q\"\nm: \"n\"\n", "{\"k\":\"x \\\"q\\\"\\n\",\"m\":\"n\"}"
    "Tokenizer tags and anchors before a header (2)", "k: !!str |\n  x \"q\"\nm: \"n\"\n", "{\"k\":\"x \\\"q\\\"\\n\",\"m\":\"n\"}"
    "Tokenizer tags and anchors before a header (3)", "k: &a |\n  x \"q\"\nm: \"n\"\n", "{\"k\":\"x \\\"q\\\"\\n\",\"m\":\"n\"}"
    "Tokenizer tags and anchors before a header (4)", "k: &a !!str |\n  x \"q\"\nm: \"n\"\n", "{\"k\":\"x \\\"q\\\"\\n\",\"m\":\"n\"}"
    "Tokenizer a sequence entry with a verbatim tag and a header", "- !<tag:yaml.org,2002:str> |\n  x \"q\"\n- 'v'\n", "[\"x \\\"q\\\"\\n\",\"v\"]"
    "Tokenizer a quoted scalar behind a verbatim tag", "- !<tag:yaml.org,2002:str> 'v'\n", "[\"v\"]"
    "Tokenizer properties are whole tokens and a verbatim tag may contain colons (1)", "k: !<tag:yaml.org,2002:str> |\n  x\n", "{\"k\":\"x\\n\"}"
    "Tokenizer properties are whole tokens and a verbatim tag may contain colons (2)", "- !<tag:yaml.org,2002:str> |\n  x\n", "[\"x\\n\"]"
    "Tokenizer properties are whole tokens and a verbatim tag may contain colons (3)", "!<tag:yaml.org,2002:str> |\n  x\n", "\"x\\n\""
    "Tokenizer properties are whole tokens and a verbatim tag may contain colons (4)", "k: &a !!str |\n  x\n", "{\"k\":\"x\\n\"}"
    "Tokenizer properties are whole tokens and a verbatim tag may contain colons (5)", "&a |\n  x\n", "\"x\\n\""
    "Tokenizer properties are whole tokens and a verbatim tag may contain colons (6)", "!tag a: |\n  x\n", "{\"a\":\"x\\n\"}"
    "Tokenizer properties are whole tokens and a verbatim tag may contain colons (7)", "&a k: !<x:y> |\n  x\n", "{\"k\":\"x\\n\"}"
    "Tokenizer a quoted key with a hash and a colon opens a block", "\"a #b\": |\n  x # y\n  'z'\n\"c: d\": >\n  'e'\n", "{\"a #b\":\"x # y\\n'z'\\n\",\"c: d\":\"'e'\\n\"}"
    "N1-D02", "- \"q\"\n- [a]", "[\"q\",[\"a\"]]"
    "N1-D03", "k: [a] # c\n  # d\nj: 1", "{\"k\":[\"a\"],\"j\":1}"
    "N1-D27", "k: [a,\n  b]\nj: 1", "{\"k\":[\"a\",\"b\"],\"j\":1}"
]

let private cwlValues : (string * string * string) list = [
    "YamlRead CWL ExpressionTool join output roundtrip YAML (with a final line break)", "#!/usr/bin/env cwl-runner\n\ncwlVersion: v1.2\nclass: ExpressionTool\nid: \"T_join_out\"\nlabel: Gives back the joined folder called from \"analysis\", holding all given files and the subfolders.\nrequirements:\n  InlineJavascriptRequirement: {}\ninputs:\n  base_path:\n    type: Directory\n  item_single:\n    type: File?\n  item_array:\n    type: File[]?\n  container_single:\n    type: Directory?\n  container_array:\n    type: Directory[]?\n  relabel:\n    type: string?\noutputs:\n  joined_D:\n    type: Directory\n    doc: \"Merged results output folder\"\nexpression: >\n  ${ return (function() {\n    function trimItem(datum) {\n      var permittedKeys = ['class', 'basename', 'checksum', 'listing'];\n      var shortened = {};\n      for (var i = 0; i < permittedKeys.length; i++) {\n        var key = permittedKeys[i];\n        if (datum[key] !== undefined) shortened[key] = datum[key];\n      }\n      return shortened.class && shortened.basename ? shortened : null;\n      return name.replace(/\\.webp$/, \"\").replace(/\\.gif$/, \"\");\n    }\n\n    var collection = [];\n    if (inputs.container_single) collection.push(trimItem(inputs.container_single));\n    if (inputs.item_single) collection.push(trimItem(inputs.item_single));\n\n    return {\n      joined_D: { class: \"Directory\", basename: inputs.relabel || \"analysis\", listing: collection }\n    };\n  })(); }\n", "{\"cwlVersion\":\"v1.2\",\"class\":\"ExpressionTool\",\"id\":\"T_join_out\",\"label\":\"Gives back the joined folder called from \\\"analysis\\\", holding all given files and the subfolders.\",\"requirements\":{\"InlineJavascriptRequirement\":{}},\"inputs\":{\"base_path\":{\"type\":\"Directory\"},\"item_single\":{\"type\":\"File?\"},\"item_array\":{\"type\":\"File[]?\"},\"container_single\":{\"type\":\"Directory?\"},\"container_array\":{\"type\":\"Directory[]?\"},\"relabel\":{\"type\":\"string?\"}},\"outputs\":{\"joined_D\":{\"type\":\"Directory\",\"doc\":\"Merged results output folder\"}},\"expression\":\"${ return (function() {\\n  function trimItem(datum) {\\n    var permittedKeys = ['class', 'basename', 'checksum', 'listing'];\\n    var shortened = {};\\n    for (var i = 0; i < permittedKeys.length; i++) {\\n      var key = permittedKeys[i];\\n      if (datum[key] !== undefined) shortened[key] = datum[key];\\n    }\\n    return shortened.class && shortened.basename ? shortened : null;\\n    return name.replace(/\\\\.webp$/, \\\"\\\").replace(/\\\\.gif$/, \\\"\\\");\\n  }\\n\\n  var collection = [];\\n  if (inputs.container_single) collection.push(trimItem(inputs.container_single));\\n  if (inputs.item_single) collection.push(trimItem(inputs.item_single));\\n\\n  return {\\n    joined_D: { class: \\\"Directory\\\", basename: inputs.relabel || \\\"analysis\\\", listing: collection }\\n  };\\n})(); }\\n\"}"
]

let private expectError (input: string) (expected: string) =
    Expect.equal (parseErrorOf input) expected (sprintf "The error of %A" input)

/// The value of every document of the text as JSON, in the form of JSON.stringify of the value that yaml@2.6.1 builds: the value of the document for
/// one document and an array of the values for several.
let private streamValue (input: string) : string =
    let values =
        [ for part in (parseOk input).Parts do
              match part with
              | StreamPart.Document document -> yield valueJson document.Root
              | _ -> () ]
    match values with
    | [ single ] -> single
    | _ -> "[" + String.concat "," values + "]"

let private valueTests = testList "Flow values" [
    // The tables hold the inputs of the old tests and the value that yaml@2.6.1 reads for each. The value comes from yaml@2.6.1, and the parser gets no say in it.
    testCase "the flow inputs of the old tests read the value that yaml@2.6.1 reads" <| fun _ ->
        // The test collects the differences of all rows and reports them together.
        let differences =
            [ for (reference, input, expected) in flowValues @ cwlValues do
                  let actual = streamValue input
                  if actual <> expected then yield sprintf "%s: the value of %A is %s and not %s" reference input actual expected ]
        Expect.isEmpty differences (String.concat "\n" differences)
]

let private shapeTests = testList "Flow sequences and flow mappings" [
    // Tests.FlowTokens "empty object" and "empty array", Tests.YamlRead "Empty inline flow object". yaml@2.6.1: {}, [], {"key":{}}.
    testCase "empty collections are flow collections in every place" <| fun _ ->
        expectRoot "[]" (YamlNode.flowSequence [])
        expectRoot "{}" (YamlNode.flowMapping [])
        expectRoot "[ ]" (YamlNode.flowSequence [])
        expectRoot "{ \t}" (YamlNode.flowMapping [])
        expectRoot "k: {}" (mapOf [ "k", YamlNode.flowMapping [] ])
        expectRoot "- []" (seqOf [ YamlNode.flowSequence [] ])

    // Tests.FlowTokens "simple object", "simple array", "double-quoted string with comma". yaml@2.6.1: {"key":"value"}, ["a","b","c"], ["a,b","c"].
    testCase "a flow sequence and a flow mapping of scalars" <| fun _ ->
        expectRoot "[a, b, c]" (flowSeq [ sc "a"; sc "b"; sc "c" ])
        expectRoot "{key: value}" (flowMap [ "key", sc "value" ])
        expectRoot "[\"a,b\", c]" (flowSeq [ dq "a,b"; sc "c" ])
        expectRoot "['it''s', \"x\"]" (flowSeq [ sq "it's"; dq "x" ])
        expectRoot "{a: 1, b: two words}" (flowMap [ "a", sc "1"; "b", sc "two words" ])

    // Tests.FlowTokens "nested object", "nested array inside object", "nested object inside array". yaml@2.6.1 reads the nested values.
    testCase "collections nest in each other" <| fun _ ->
        expectRoot "{outer: {inner: value}}" (flowMap [ "outer", flowMap [ "inner", sc "value" ] ])
        expectRoot "{key: [a, b]}" (flowMap [ "key", flowSeq [ sc "a"; sc "b" ] ])
        expectRoot "[{a: 1}, {b: 2}]" (flowSeq [ flowMap [ "a", sc "1" ]; flowMap [ "b", sc "2" ] ])
        expectRoot "[[a, b], [c]]" (flowSeq [ flowSeq [ sc "a"; sc "b" ]; flowSeq [ sc "c" ] ])

    // Tests.YamlRead "Inline flow object as value", "Inline flow array as value", "KeyValue InlineSequence + Comment". A flow collection is a node
    // wherever a block node can stand.
    testCase "a flow collection is a node in every place of the block structure" <| fun _ ->
        expectRoot "k: [1, 2]" (mapOf [ "k", flowSeq [ sc "1"; sc "2" ] ])
        expectRoot "- {a: 1}\n- [b]" (seqOf [ flowMap [ "a", sc "1" ]; flowSeq [ sc "b" ] ])
        expectRoot "k:\n  [1, 2]" (YamlNode.mapping [ entryOnNewLine "k" (flowSeq [ sc "1"; sc "2" ]) ])
        expectRoot "? a\n: [b]" (YamlNode.mapping [ explicitEntry (sc "a") (flowSeq [ sc "b" ]) ])
        expectRoot "a:\n  b: {c: [d]}" (mapOf [ "a", mapOf [ "b", flowMap [ "c", flowSeq [ sc "d" ] ] ] ])
        let document = parseDocument "--- [a]"
        Expect.isTrue document.StartMarker.IsSome "The marker"
        Expect.nodeEqual document.Root (flowSeq [ sc "a" ]) "The root on the line of the marker"

    // Earlier tests "the lines behind a merged flow collection keep their source line numbers" and "text behind the closer stays on the merged line". The preprocessing before 2.0.0 merged the lines of a
    // collection, so it had to remember the physical lines. The parser reads the lines as they are, and the spans carry them. The span of a collection runs from its opener to its closer, and the
    // span of a missing value has no width.
    testCase "the spans of a flow collection and of its parts" <| fun _ ->
        let root = parseRoot "[a, {b: 1}]"
        Expect.equal (showSpan root.Span.Value) "1:1-1:12" "The sequence"
        let items = YamlNode.items root
        Expect.equal (items |> List.map (fun item -> showSpan item.Node.Span.Value)) [ "1:2-1:3"; "1:5-1:11" ] "The items"
        let multi = parseRoot "k: [\n  a,\n  b\n]"
        let value = (YamlNode.entries multi).Head.Value
        Expect.equal (showSpan value.Span.Value) "1:4-4:2" "A collection over four lines"
        let entry = (YamlNode.entries (parseRoot "{a, b: c}")).Head
        Expect.equal (showSpan entry.Value.Span.Value) "1:3-1:3" "The missing value of a key alone stands right behind the key"
]

let private pairTests = testList "Flow pairs" [
    // Tests.YamlRead "A pair in a flow sequence is a mapping with one entry". yaml@2.6.1: [{"a":"b"}], [{"a":"b"},"c"], [{"a":null}], [{"a":"b"},{"c":"d"}]
    // and ["a:b","12:45"]. [150] to [154] make `a: b` an entry of its own, which the model keeps as the one-entry flow mapping of `{a: b}`.
    testCase "a pair is a flow mapping with one entry" <| fun _ ->
        expectRoot "[a: b]" (flowSeq [ flowPair (sc "a") (sc "b") ])
        expectRoot "[a: b, c]" (flowSeq [ flowPair (sc "a") (sc "b"); sc "c" ])
        expectRoot "[a: ]" (flowSeq [ flowPair (sc "a") nothing ])
        expectRoot "[a: b,\n  c: d]" (flowSeq [ flowPair (sc "a") (sc "b"); flowPair (sc "c") (sc "d") ])
        expectRoot "[a:b, 12:45]" (flowSeq [ sc "a:b"; sc "12:45" ])
        Expect.nodeEqual (parseRoot "[a: b]") (parseRoot "[{a: b}]") "A pair and the mapping give the same tree"

    // Earlier test "a question mark in a flow collection is an explicit key only in front of a blank or a flow indicator" and Tests.YamlRead "An explicit key
    // inside a flow collection". yaml@2.6.1: [{"a":"b"}], [{"a":"b"},{"c":null}], [{"":null}].
    testCase "a pair can start with a question mark or with a colon" <| fun _ ->
        expectRoot "[? a : b]" (flowSeq [ YamlNode.flowMapping [ explicitFlowEntry (sc "a") (sc "b") ] ])
        expectRoot "[: b]" (flowSeq [ flowPair nothing (sc "b") ])
        expectRoot "[? a]" (flowSeq [ YamlNode.flowMapping [ { explicitFlowEntry (sc "a") nothing with Colon = None } ] ])
        expectRoot "[? ]" (flowSeq [ YamlNode.flowMapping [ { explicitFlowEntry nothing nothing with Colon = None } ] ])
        expectRoot "[? a : b, ? c]" (flowSeq [ YamlNode.flowMapping [ explicitFlowEntry (sc "a") (sc "b") ]; YamlNode.flowMapping [ { explicitFlowEntry (sc "c") nothing with Colon = None } ] ])
        // The key of a pair behind a question mark may span lines, because [143] ns-flow-map-explicit-entry has no limit on it.
        expectRoot "[\n? foo\n bar : baz\n]" (flowSeq [ YamlNode.flowMapping [ explicitFlowEntry (scLines [ "foo"; "bar" ]) (sc "baz") ] ])

    // [148] and [149]: a colon behind a quoted scalar or a flow collection takes its value without a blank, and blanks, comments and line breaks may stand
    // between the node and the colon. yaml@2.6.1: [{"a":"b"}], [{"a":"b"}], [{"a":"b"}] and [{"[ a ]":"b"}].
    testCase "a pair with a quoted key or a collection key takes its value without a blank" <| fun _ ->
        expectRoot "[\"a\":b]" (flowSeq [ flowPair (dq "a") (sc "b") ])
        expectRoot "[\"a\" :b]" (flowSeq [ flowPair (dq "a") (sc "b") ])
        expectRoot "['a':'b']" (flowSeq [ flowPair (sq "a") (sq "b") ])
        expectRoot "[[a]:b]" (flowSeq [ flowPair (flowSeq [ sc "a" ]) (sc "b") ])
        expectRoot "[{a: 1}:b]" (flowSeq [ flowPair (flowMap [ "a", sc "1" ]) (sc "b") ])
        expectRoot "[\"a\" : b]" (flowSeq [ flowPair (dq "a") (sc "b") ])

    // The span of a pair covers the key and the value.
    testCase "the span of a pair runs from its key to its value" <| fun _ ->
        let root = parseRoot "[x, a: b, ? c : d]"
        let items = YamlNode.items root
        Expect.equal (items |> List.map (fun item -> showSpan item.Node.Span.Value)) [ "1:2-1:3"; "1:5-1:9"; "1:11-1:18" ] "The items"
        let entry = (YamlNode.entries items.[1].Node).Head
        Expect.equal (showSpan entry.Key.Span.Value) "1:5-1:6" "The key"
        Expect.equal (showSpan entry.Span.Value) "1:5-1:9" "The entry"

    // Earlier test "the key of a pair in a flow sequence cannot span lines". yaml@2.6.1 rejects all three inputs with MULTILINE_IMPLICIT_KEY.
    // [154] ns-s-implicit-yaml-key: the key of a pair is on one line with its colon and has at most 1024 characters. The limit counts from the first
    // character of the key, its properties included.
    testCase "the key of a pair has the limits of an implicit key" <| fun _ ->
        let key (length: int) = String.replicate length "k"
        let accepted = parseRoot ("[" + key 1024 + ": v]")
        Expect.equal (YamlNode.items accepted).Length 1 "1024 characters"
        expectError ("[" + key 1025 + ": v]") "implicit-key-length@1"
        expectError ("[&a " + key 1022 + ": v]") "implicit-key-length@1"
        expectError "[a\n: b]" "implicit-key-multiline@1"
        expectError "[a\n b : c]" "implicit-key-multiline@1"
        expectError "k: [x,\n  a\n  b : c]" "implicit-key-multiline@2"
        // The limit counts code points, so a character beyond the BMP counts once on every target.
        let astral (length: int) = String.replicate length "\U0001F600"
        Expect.equal (YamlNode.items (parseRoot ("[" + astral 1024 + ": v]"))).Length 1 "1024 characters beyond the BMP"
        expectError ("[" + astral 1025 + ": v]") "implicit-key-length@1"
        // The key of an entry of a flow mapping has no such limit.
        Expect.equal (YamlNode.entries (parseRoot ("{" + key 1025 + ": v}"))).Length 1 "A long key in a mapping"
        Expect.equal (YamlNode.entries (parseRoot "{a\n b : c}")).Length 1 "A key over two lines in a mapping"
]

let private entryTests = testList "Entries of a flow mapping" [
    // Tests.YamlRead "A flow mapping entry without a value maps to nothing". yaml@2.6.1: {"k":null}, {"a":null,"b":"c"} and {"a":null,"b":"c"}.
    // The value indicator is omitted in `{a, b: c}`, so Colon is None, and the value is the empty plain scalar behind the key.
    testCase "a key alone has no colon and a missing value" <| fun _ ->
        expectRoot "{a, b: c}" (YamlNode.flowMapping [ keyOnly (sc "a"); YamlNode.entry (sc "b") (sc "c") ])
        expectRoot "{a}" (YamlNode.flowMapping [ keyOnly (sc "a") ])
        expectRoot "{k:}" (YamlNode.flowMapping [ YamlNode.entry (sc "k") nothing ])
        expectRoot "{a: , b}" (YamlNode.flowMapping [ YamlNode.entry (sc "a") nothing; keyOnly (sc "b") ])
        expectRoot "{? a}" (YamlNode.flowMapping [ { explicitFlowEntry (sc "a") nothing with Colon = None } ])

    // Tests.YamlRead "An explicit key inside a flow collection". yaml@2.6.1: {"a":"b"}, {"":"b"}, {"":null}, {"foo":null,"":"bar"} (spec 7.3) and
    // {"explicit":"entry","implicit":"entry","":null} (spec 7.16).
    testCase "explicit entries and entries with an empty key" <| fun _ ->
        expectRoot "{? a : b}" (YamlNode.flowMapping [ explicitFlowEntry (sc "a") (sc "b") ])
        expectRoot "{: b}" (YamlNode.flowMapping [ YamlNode.entry nothing (sc "b") ])
        expectRoot "{? : b}" (YamlNode.flowMapping [ explicitFlowEntry nothing (sc "b") ])
        expectRoot "{? }" (YamlNode.flowMapping [ { explicitFlowEntry nothing nothing with Colon = None } ])
        expectRoot
            "{ ? foo :,\n  : bar }"
            (YamlNode.flowMapping [ explicitFlowEntry (sc "foo") nothing; YamlNode.entry nothing (sc "bar") ])
        expectRoot
            "{\n? explicit: entry,\nimplicit: entry,\n?\n}"
            (YamlNode.flowMapping [
                explicitFlowEntry (sc "explicit") (sc "entry")
                YamlNode.entry (sc "implicit") (sc "entry")
                { explicitFlowEntry nothing nothing with Colon = None }
            ])

    // Tests.YamlRead "A colon in a flow mapping separates a key only behind a blank, a flow indicator or a quote". yaml@2.6.1: {"a:b":null,"c":"d"},
    // {"a:\"b\"":null}, {"a":"b","c":"d"} and {"time":"12:45","url":"http://x"}. [149] gives a JSON-like key its value without a blank.
    testCase "a colon separates a key behind a blank, a flow indicator or a quote" <| fun _ ->
        expectRoot "{a:b, c: d}" (YamlNode.flowMapping [ keyOnly (sc "a:b"); YamlNode.entry (sc "c") (sc "d") ])
        expectRoot "{a:\"b\"}" (YamlNode.flowMapping [ keyOnly (sc "a:\"b\"") ])
        expectRoot "{\"a\":b, 'c':'d'}" (YamlNode.flowMapping [ YamlNode.entry (dq "a") (sc "b"); YamlNode.entry (sq "c") (sq "d") ])
        expectRoot "{time: 12:45, url: http://x}" (flowMap [ "time", sc "12:45"; "url", sc "http://x" ])
        expectRoot "{\"a\" :b}" (YamlNode.flowMapping [ YamlNode.entry (dq "a") (sc "b") ])
        expectRoot "{[a]:b}" (YamlNode.flowMapping [ YamlNode.entry (flowSeq [ sc "a" ]) (sc "b") ])

    // Tests.YamlRead "The key of an entry in a flow mapping or behind a question mark may span lines". yaml@2.6.1: {"matches %":20} (spec 9.4), {"a b":"c"},
    // {"a b c":null} and {"a":"b"}.
    testCase "the key of an entry may span lines" <| fun _ ->
        expectRoot "{ matches\n% : 20 }" (YamlNode.flowMapping [ YamlNode.entry (scLines [ "matches"; "%" ]) (sc "20") ])
        expectRoot "{a\n b : c}" (YamlNode.flowMapping [ YamlNode.entry (scLines [ "a"; "b" ]) (sc "c") ])
        expectRoot "{? a\n b\n c}" (YamlNode.flowMapping [ { explicitFlowEntry (scLines [ "a"; "b"; "c" ]) nothing with Colon = None } ])
        expectRoot "{a\n: b}" (flowMap [ "a", sc "b" ])
        expectRoot "{\"a\"\n: b}" (YamlNode.flowMapping [ YamlNode.entry (dq "a") (sc "b") ])

    // [138] and [141] allow one comma behind the last entry. yaml@2.6.1: ["a"], {"a":1}, {"":null}, [{"a":"b"}] and {"a":null}.
    testCase "a comma behind the last entry is valid" <| fun _ ->
        expectRoot "[a, b, ]" (flowSeq [ sc "a"; sc "b" ])
        expectRoot "[a,]" (flowSeq [ sc "a" ])
        expectRoot "{a: 1, }" (flowMap [ "a", sc "1" ])
        expectRoot "{a,}" (YamlNode.flowMapping [ keyOnly (sc "a") ])
        expectRoot "[a: b,]" (flowSeq [ flowPair (sc "a") (sc "b") ])
        expectRoot "{? ,}" (YamlNode.flowMapping [ { explicitFlowEntry nothing nothing with Colon = None } ])
]

let private scalarTests = testList "Scalars inside a flow collection" [
    // Tests.YamlRead "A plain scalar over several lines in a flow collection folds". yaml@2.6.1: ["a b"], ["a\nb"], {"k":["a b","c d"]} and ["a b","c"].
    testCase "a plain scalar folds over the lines of a collection and ends at a flow indicator" <| fun _ ->
        expectRoot "[\n  a\n  b\n]" (flowSeq [ scLines [ "a"; "b" ] ])
        expectRoot "[a\n\n b]" (flowSeq [ sc "a\nb" ])
        expectRoot "[a\n b, c]" (flowSeq [ scLines [ "a"; "b" ]; sc "c" ])
        expectRoot "[a b]" (flowSeq [ sc "a b" ])
        expectRoot "[a,b]" (flowSeq [ sc "a"; sc "b" ])
        expectRoot "[a ,b]" (flowSeq [ sc "a"; sc "b" ])
        expectRoot "{a: b c}" (flowMap [ "a", sc "b c" ])

    // Earlier tests "an indicator inside a plain scalar or behind a closed flow collection is no error" and "a hash behind a blank starts a comment...".
    // yaml@2.6.1: {"k":"a]"}, {"k":"-x"}, {"k":":x"}, ["-x"], ["a",-1], ["a#b","c"].
    testCase "indicator characters inside a plain scalar are text" <| fun _ ->
        expectRoot "k: a]" (mapOf [ "k", sc "a]" ])
        expectRoot "k: a, b" (mapOf [ "k", sc "a, b" ])
        expectRoot "[-x]" (flowSeq [ sc "-x" ])
        expectRoot "[a, -1]" (flowSeq [ sc "a"; sc "-1" ])
        expectRoot "[:a, ?b]" (flowSeq [ sc ":a"; sc "?b" ])
        expectRoot "[a#b, c #d\n]" (flowSeq [ sc "a#b"; withNote "d" (sc "c") ])
        expectRoot "[a:b, 12:45, http://x]" (flowSeq [ sc "a:b"; sc "12:45"; sc "http://x" ])

    // Earlier tests "an unbalanced bracket or brace at the start of a continuation line does not hide the blocks after it" and "an unbalanced bracket in a sequence item does not hide the block scalar
    // after it" (the Reader variants of the table rows). A bracket in a plain scalar of the block structure is text, and the block scalars behind it open as usual.
    // Earlier test "a quote behind a bracket in a plain scalar is text". yaml@2.6.1: {"k":"o[\"k\"]"} and {"k":"$(inputs[\"my-input\"].path)"}.
    testCase "brackets and quotes inside a plain scalar of the block structure are text" <| fun _ ->
        expectRoot "k: o[\"k\"]" (mapOf [ "k", sc "o[\"k\"]" ])
        expectRoot "k: $(inputs[\"my-input\"].path)" (mapOf [ "k", sc "$(inputs[\"my-input\"].path)" ])
        expectRoot "k: $(o['k'] || [1, 2])" (mapOf [ "k", sc "$(o['k'] || [1, 2])" ])
]

let private endOfInputTests = testList "Block scalars with flow characters at the end of the input" [
    // Tests.YamlRead "CWL ExpressionTool roundtrip preserves block scalar" and "CWL ExpressionTool join output roundtrip YAML". yaml@2.6.1 reads the folded value with a line feed at its end:
    // {"expression":"${ return { \"out\": name }; }\n"}. The text has no line break at its end, and [165] b-chomped-last gives a clipped value no line feed then. That is the deliberate difference
    // of the lexer. The braces inside the scalar are text.
    testCase "a folded scalar that holds braces has no final line feed at the end of the input" <| fun _ ->
        Expect.equal (valueOf "expression: >\n  ${ return { \"out\": name }; }") "{\"expression\":\"${ return { \\\"out\\\": name }; }\"}" "Without a line break at the end"
        Expect.equal (valueOf "expression: >\n  ${ return { \"out\": name }; }\n") "{\"expression\":\"${ return { \\\"out\\\": name }; }\\n\"}" "With a line break at the end"
        expectRoot "InlineJavascriptRequirement: {}" (mapOf [ "InlineJavascriptRequirement", YamlNode.flowMapping [] ])
        // The long ExpressionTool of the old test has no line break at its end, and the table holds it with one. The value of yaml@2.6.1 loses only the final line feed of the expression.
        let (_, input, expected) = List.head cwlValues
        Expect.equal (valueOf (input.TrimEnd('\n'))) (expected.Replace("\\n\"}", "\"}")) "The long expression of the old test"
]

let private layoutTests = testList "Layouts over several lines" [
    // Tests.YamlRead "A flow collection over several lines reads in every layout" and the pattern "expressionLib". The closer may stand at the
    // column of the node that owns the collection. [69] and [80] ask for one more space, and the lexer takes a line of a flow collection at any indentation (see LexContext).
    // ruamel and yaml@2.6.1 read this layout of the outermost collection. The tests below cover the layouts that only ruamel reads.
    testCase "the closer of a collection may stand at the column of its owner" <| fun _ ->
        let expression = mapOf [ "expressionLib", flowSeq [ dq "function f() { return 1; }"; dq "var x = 'a';" ] ]
        expectRoot "expressionLib: [\n  \"function f() { return 1; }\",\n  \"var x = 'a';\"\n]" expression
        expectRoot "k: [\n  a\n]" (mapOf [ "k", flowSeq [ sc "a" ] ])
        expectRoot "k: [\n  a\n ]" (mapOf [ "k", flowSeq [ sc "a" ] ])
        expectRoot "- [\n a\n]" (seqOf [ flowSeq [ sc "a" ] ])
        expectRoot "k: {\n  a: 1\n}" (mapOf [ "k", flowMap [ "a", sc "1" ] ])
        expectRoot "k:\n    [\n      a\n  ]" (YamlNode.mapping [ entryOnNewLine "k" (flowSeq [ sc "a" ]) ])
        expectRoot "[a,\nb]" (flowSeq [ sc "a"; sc "b" ])
        // yaml@2.6.1 reads a closer behind a tab at the column 0 like a closer at the column of the owner.
        expectRoot "k: [\n\t ]" (mapOf [ "k", YamlNode.flowSequence [] ])

    // yaml@2.6.1 rejects every input with BAD_INDENT and related errors. ruamel reads the values below. [69] and [80] ask for the indentation of the owner plus one. The lexer takes a line
    // of a flow collection at any indentation, because the collection runs to its closer.
    testCase "the items of a collection may stand at the column of the owner or left of it" <| fun _ ->
        expectRoot "k: [\na,\nb\n]" (mapOf [ "k", flowSeq [ sc "a"; sc "b" ] ])
        expectRoot "- [a,\nb]\n- c" (seqOf [ flowSeq [ sc "a"; sc "b" ]; sc "c" ])
        expectRoot "top:\n  k: [\na,\nb\n]" (YamlNode.mapping [ entryOnNewLine "top" (mapOf [ "k", flowSeq [ sc "a"; sc "b" ] ]) ])
        for (input, expected) in
            [ "top:\n  k: [\n  a,\n  b\n  ]", "{\"top\":{\"k\":[\"a\",\"b\"]}}"
              "top:\n  k: [\n a,\n b\n ]", "{\"top\":{\"k\":[\"a\",\"b\"]}}"
              "- k: [\na,\nb\n]", "[{\"k\":[\"a\",\"b\"]}]"
              "k: {a: 1,\nb: 2}", "{\"k\":{\"a\":1,\"b\":2}}"
              "top:\n  k: {\na: 1\n}", "{\"top\":{\"k\":{\"a\":1}}}"
              "k: {\n  a:\n1\n}", "{\"k\":{\"a\":1}}"
              "k: [ a: b,\nc: d ]", "{\"k\":[{\"a\":\"b\"},{\"c\":\"d\"}]}"
              "top:\n  k: [ a\n, b ]", "{\"top\":{\"k\":[\"a\",\"b\"]}}" ] do
            Expect.equal (valueOf input) expected (sprintf "The value of %A" input)

    // yaml@2.6.1 rejects every input with BAD_INDENT and related errors. ruamel reads the values below. The lines of a plain scalar fold like the lines of a deeper scalar.
    testCase "a plain scalar in a collection goes on in a line at any indentation" <| fun _ ->
        expectRoot "k: [ a\nb ]" (mapOf [ "k", flowSeq [ scLines [ "a"; "b" ] ] ])
        for (input, expected) in
            [ "top:\n  k: [ a\n  b ]", "{\"top\":{\"k\":[\"a b\"]}}"
              "top:\n  k: [ a\nb ]", "{\"top\":{\"k\":[\"a b\"]}}"
              "top:\n  k: [ a\n b ]", "{\"top\":{\"k\":[\"a b\"]}}"
              "k: [ a\n\nb ]", "{\"k\":[\"a\\nb\"]}"
              "top:\n  k: [ a\n\nb, c ]", "{\"top\":{\"k\":[\"a\\nb\",\"c\"]}}"
              "k: [ a:\nb ]", "{\"k\":[{\"a\":\"b\"}]}"
              "k: { a: b\nc }", "{\"k\":{\"a\":\"b c\"}}"
              "k: [ a b\nc d, e\nf ]", "{\"k\":[\"a b c d\",\"e f\"]}" ] do
            Expect.equal (valueOf input) expected (sprintf "The value of %A" input)

    // yaml@2.6.1 rejects all but the last input (BAD_INDENT and related errors), which it reads like ruamel does. A nested closer, a closer of the outermost collection left of the owner and the
    // entries behind it read as they do with deeper lines.
    testCase "a closer may stand at the column of the owner or left of it" <| fun _ ->
        expectRoot "k: [\n  [a\n]\n]" (mapOf [ "k", flowSeq [ flowSeq [ sc "a" ] ] ])
        expectRoot "k: [a: b\n]" (mapOf [ "k", flowSeq [ flowPair (sc "a") (sc "b") ] ])
        for (input, expected) in
            [ "top:\n  k: [\n    [a\n  ]\n  ]", "{\"top\":{\"k\":[[\"a\"]]}}"
              "top:\n  k: [\n    [a\n]\n]", "{\"top\":{\"k\":[[\"a\"]]}}"
              "k: {\n  a: {b: 1\n}\n}", "{\"k\":{\"a\":{\"b\":1}}}"
              "k: [\n  [a: b\n]\n]", "{\"k\":[[{\"a\":\"b\"}]]}"
              "k: [\n  { a: 1\n}\n]", "{\"k\":[{\"a\":1}]}"
              "top:\n  k: [\n    a\n ]", "{\"top\":{\"k\":[\"a\"]}}"
              "top:\n  k: [ a,\n]", "{\"top\":{\"k\":[\"a\"]}}"
              "top:\n  k: [\n    a\n]\n  j: 1", "{\"top\":{\"k\":[\"a\"],\"j\":1}}"
              "top:\n  k: [\n    a\n]\nj: 1", "{\"top\":{\"k\":[\"a\"]},\"j\":1}"
              "- k: [\n    a\n]\n  j: 1", "[{\"k\":[\"a\"],\"j\":1}]"
              "k: [\n  a\n] # c\nj: 1", "{\"k\":[\"a\"],\"j\":1}" ] do
            Expect.equal (valueOf input) expected (sprintf "The value of %A" input)

    // yaml@2.6.1 rejects every input with BAD_INDENT and related errors, and ruamel reads the values below. [63] s-indent(n) counts spaces only, and a tab behind them is a separator ([66]),
    // so a flow line that starts with a tab is a line without indentation, and the lexer takes it like any other line. A line of tabs and spaces is a blank line.
    testCase "a flow line may start with a tab" <| fun _ ->
        for (input, expected) in
            [ "a: [\n\tb]", "{\"a\":[\"b\"]}"
              "k: [ a,\n\tb ]", "{\"k\":[\"a\",\"b\"]}"
              "top:\n  k: [ a,\n\tb ]", "{\"top\":{\"k\":[\"a\",\"b\"]}}"
              "top:\n  k: [ a,\n \tb ]", "{\"top\":{\"k\":[\"a\",\"b\"]}}"
              "top:\n  k: [ a\n\t]", "{\"top\":{\"k\":[\"a\"]}}"
              "k: [\n  { a: b\n  , c: d\n\t }\n  ]", "{\"k\":[{\"a\":\"b\",\"c\":\"d\"}]}"
              "a:\n  b: {\n    k: {x: 1\n  \t }\n  }", "{\"a\":{\"b\":{\"k\":{\"x\":1}}}}"
              "k: [ a,\n\t\nb ]", "{\"k\":[\"a\",\"b\"]}"
              "top:\n  k: [ a,\n  \t\n  b ]", "{\"top\":{\"k\":[\"a\",\"b\"]}}"
              "top:\n  k: [ a,\n\t# c\n  b ]", "{\"top\":{\"k\":[\"a\",\"b\"]}}" ] do
            Expect.equal (valueOf input) expected (sprintf "The value of %A" input)

    // yaml@2.6.1 and ruamel reject the first five inputs (ruamel with a ParserError) and read the last two as {"k":["a"]}. A tab is no indentation, so a plain scalar ends in front of a line
    // that starts with a tab behind too few spaces ([70] l-empty), and the next entry follows without a comma. A comment line ends a plain scalar as well.
    testCase "a plain scalar in a collection ends in front of a tab behind too few spaces and in front of a comment" <| fun _ ->
        expectError "k: [ a\n\tb ]" "missing-flow-comma@2"
        expectError "top:\n  k: [ a\n \tb ]" "missing-flow-comma@3"
        expectError "k: [ a\n\t\nb ]" "missing-flow-comma@3"
        expectError "k: [ a\n# c\nb ]" "missing-flow-comma@3"
        expectError "k: [ a #c\nb ]" "missing-flow-comma@2"
        Expect.equal (valueOf "k: [ a\n\t\n]") "{\"k\":[\"a\"]}" "A line of a tab and a closer behind it"
        Expect.equal (valueOf "k: [ a,\n\t\n]") "{\"k\":[\"a\"]}" "A line of a tab behind a comma"

    // yaml@2.6.1 rejects every input with BAD_INDENT and related errors. ruamel reads the values below. A comment line and the properties of a node stand at any indentation.
    testCase "comment lines and properties stand at any indentation inside a collection" <| fun _ ->
        for (input, expected) in
            [ "k: [ a,\n# c\nb ]", "{\"k\":[\"a\",\"b\"]}"
              "top:\n  k: [ a,\n# c\nb ]", "{\"top\":{\"k\":[\"a\",\"b\"]}}"
              "top:\n  k: [\n# c\na ]", "{\"top\":{\"k\":[\"a\"]}}"
              "k: [ &x\na ]", "{\"k\":[\"a\"]}"
              "k: [ !!str\na ]", "{\"k\":[\"a\"]}"
              "top:\n  k: [ &x\na, b ]", "{\"top\":{\"k\":[\"a\",\"b\"]}}"
              "? k\n: [ a,\nb ]", "{\"k\":[\"a\",\"b\"]}" ] do
            Expect.equal (valueOf input) expected (sprintf "The value of %A" input)

    // yaml@2.6.1 rejects every input with errors such as BAD_INDENT. ruamel rejects the first six inputs (a ParserError, or a ScannerError for the quoted scalar) and reads the others as text.
    // A marker is `---` or `...` at the column 0 with a blank or the end of the line behind it.
    testCase "a document marker at the start of a line is an error inside a collection" <| fun _ ->
        expectError "k: [ a,\n---\nb ]" "marker-in-flow@2"
        expectError "k: [ a,\n...\nb ]" "marker-in-flow@2"
        expectError "k: [ a,\n--- b ]" "marker-in-flow@2"
        expectError "k: { a: 1,\n---\nb: 2 }" "marker-in-flow@2"
        expectError "k: [ a\n---\nb ]" "marker-in-flow@2"
        expectError "k: [ \"a\n---\nb\" ]" "marker-in-quote@2"
        for (input, expected) in
            [ "k: [ a,\n---x ]", "{\"k\":[\"a\",\"---x\"]}"
              "k: [ a,\n---x,\nb ]", "{\"k\":[\"a\",\"---x\",\"b\"]}"
              "top:\n  k: [ a,\n  ---\n  b ]", "{\"top\":{\"k\":[\"a\",\"--- b\"]}}"
              "top:\n  k: [ a,\n  --- b ]", "{\"top\":{\"k\":[\"a\",\"--- b\"]}}" ] do
            Expect.equal (valueOf input) expected (sprintf "The value of %A" input)

    // yaml@2.6.1 and ruamel reject every input (ruamel with a ParserError, and with a ScannerError for the third). The indentation of a line decides none of them, so they stay errors.
    // An unclosed collection runs to the end of the text, so the lines behind it belong to the collection.
    testCase "the errors of a collection do not depend on the indentation of its lines" <| fun _ ->
        expectError "k: [ a,\n- b ]" "flow-dash@2"
        expectError "k: [ a, b ] x" "unexpected-content@1"
        expectError "[ a,\nb ]: v" "implicit-key-multiline@1"
        expectError "k: [ a,\nb" "unclosed-flow@2"
        expectError "k: [ a,\nb\nj: 1" "implicit-key-multiline@2"

    testCase "blank lines and comment lines stand anywhere inside a collection" <| fun _ ->
        expectRoot "k: [\n\n  a,\n\n  b\n\n]" (mapOf [ "k", withEnd [ blank ] (YamlNode.flowSequence [ itemBefore [ blank ] (sc "a"); itemBefore [ blank ] (sc "b") ]) ])
]

let private propertyTests = testList "Properties in a flow collection" [
    // Tests.YamlRead "A flow collection behind properties reads as a collection". yaml@2.6.1: {"k":["a"]}, {"k":{"a":1,"b":2}}, {"k":{}}, [["a"]], ["a","b"].
    // The core tag of a flow mapping is a property like any other here.
    // Tests.AdvancedFeatures "7.10 A core tag on the collection it names is dropped": the reader before 2.0.0 dropped the core tag of the collection that it names, because its model had no field for the
    // properties of a collection. The tree keeps the tag as a property of the collection, so the tests below expect it.
    testCase "tags and anchors on a flow collection" <| fun _ ->
        expectRoot "k: !!map {a: 1}" (mapOf [ "k", YamlNode.withTag (YamlTag.core "map") (flowMap [ "a", sc "1" ]) ])
        expectRoot "- &x [a]" (seqOf [ YamlNode.withAnchor "x" (flowSeq [ sc "a" ]) ])
        expectRoot "k: !!seq [\n  a\n]" (mapOf [ "k", YamlNode.withTag (YamlTag.core "seq") (flowSeq [ sc "a" ]) ])
        expectRoot "!!seq [a,\n  b]" (YamlNode.withTag (YamlTag.core "seq") (flowSeq [ sc "a"; sc "b" ]))
        expectRoot "k: !!map {}" (mapOf [ "k", YamlNode.withTag (YamlTag.core "map") (YamlNode.flowMapping []) ])
        expectRoot
            "&a !!seq [x]"
            (withLayout [ placement PropertyName.Anchor Separation.Inline; placement PropertyName.Tag Separation.Inline ] (YamlNode.withAnchor "a" (YamlNode.withTag (YamlTag.core "seq") (flowSeq [ sc "x" ]))))

    // The handle !e! of a %TAG directive expands the tag of a collection.
    testCase "a tag handle on a flow collection expands" <| fun _ ->
        let tag = YamlTag.Named { Prefix = "tag:e,2000:"; Suffix = "t" }
        let document = parseDocument "%TAG !e! tag:e,2000:\n---\n!e!t {}"
        Expect.nodeEqual document.Root (YamlNode.withTag tag (YamlNode.flowMapping [])) "The expanded tag"
        let inside = parseRoot "%TAG !e! tag:example.com,2000:\n---\nk: [!e!foo a]"
        let foo = YamlTag.Named { Prefix = "tag:example.com,2000:"; Suffix = "foo" }
        Expect.nodeEqual inside (mapOf [ "k", flowSeq [ YamlNode.withTag foo (sc "a") ] ]) "A tag handle inside a collection"

    // Tests.YamlRead "Properties on a collection inside a flow collection follow the block rule" and "Properties inside a flow collection belong to the node behind
    // them". yaml@2.6.1: {"k":[["a"],{"b":"c"}]}, {"k":[2,"x"]}, {"k":{"q":"v"}}, {"k":["a b"]} and {"k":["","a"]}.
    testCase "properties belong to the node behind them" <| fun _ ->
        let intTag = YamlTag.core "int"
        expectRoot
            "k: [!!seq [a], &m {b: c}]"
            (mapOf [ "k", flowSeq [ YamlNode.withTag (YamlTag.core "seq") (flowSeq [ sc "a" ]); YamlNode.withAnchor "m" (flowMap [ "b", sc "c" ]) ] ])
        expectRoot "k: [!!int \"2\", &a \"x\"]" (mapOf [ "k", flowSeq [ YamlNode.withTag intTag (dq "2"); YamlNode.withAnchor "a" (dq "x") ] ])
        expectRoot "k: {&a \"q\": !t \"v\"}" (mapOf [ "k", YamlNode.flowMapping [ YamlNode.entry (YamlNode.withAnchor "a" (dq "q")) (YamlNode.withTag (YamlTag.local "t") (dq "v")) ] ])
        expectRoot "k: [!!str a\n  b]" (mapOf [ "k", flowSeq [ YamlNode.withTag (YamlTag.core "str") (scLines [ "a"; "b" ]) ] ])
        expectRoot "k: [!!str , a]" (mapOf [ "k", flowSeq [ YamlNode.withTag (YamlTag.core "str") nothing; sc "a" ] ])
        expectRoot "{&a : b}" (YamlNode.flowMapping [ YamlNode.entry (YamlNode.withAnchor "a" nothing) (sc "b") ])
        expectRoot "k: {a: &x}" (mapOf [ "k", YamlNode.flowMapping [ YamlNode.entry (sc "a") (YamlNode.withAnchor "x" nothing) ] ])

    // Tests.FlowTokens "a verbatim tag is one property token", Tests.YamlRead "A verbatim tag inside a flow collection is one tag". yaml@2.6.1: ["a","c"],
    // ["","c"] and {"k":"v"}. The colon and the comma inside the tag do not split it.
    testCase "a verbatim tag is one tag" <| fun _ ->
        let str = YamlTag.core "str"
        expectRoot "[!<tag:yaml.org,2002:str> a, c]" (flowSeq [ YamlNode.withTag str (sc "a"); sc "c" ])
        expectRoot "[!<tag:yaml.org,2002:str>, c]" (flowSeq [ YamlNode.withTag str nothing; sc "c" ])
        expectRoot "{!<tag:yaml.org,2002:str> k: v}" (YamlNode.flowMapping [ YamlNode.entry (YamlNode.withTag str (sc "k")) (sc "v") ])

    // Properties on a collection in front of a key belong to the key. A property that stands on a line of its own in front of a flow collection of the
    // block structure belongs to the collection, or to the mapping that the collection opens as a key (the block split).
    testCase "own-line properties of a flow collection follow the block split" <| fun _ ->
        let keyed = parseRoot "&a [x]: 1"
        Expect.nodeEqual keyed (YamlNode.mapping [ YamlNode.entry (YamlNode.withAnchor "a" (flowSeq [ sc "x" ])) (sc "1") ]) "On the key line they belong to the key"
        let split = parseRoot "&a\n[x]: 1"
        Expect.nodeEqual
            split
            (anchorOnOwnLine "a" (YamlNode.mapping [ YamlNode.entry (flowSeq [ sc "x" ]) (sc "1") ]))
            "On a line of their own they belong to the mapping"
        let plain = parseRoot "k: &a\n  [x]"
        Expect.nodeEqual plain (mapOf [ "k", anchorOnOwnLine "a" (flowSeq [ sc "x" ]) ]) "A value on the next line keeps the anchor"

    // Earlier tests "tags and anchors before a header", "a sequence entry with a verbatim tag and a header" and "a quoted scalar behind a verbatim tag". The tag and the anchor belong to the
    // block scalar or the quoted scalar behind them. The values stand in the table.
    testCase "tags and anchors stand on a block scalar and on a quoted scalar" <| fun _ ->
        let literal = lit "x \"q\"\n"
        let tagged = withLayout [ placement PropertyName.Anchor Separation.Inline; placement PropertyName.Tag Separation.Inline ] (YamlNode.withTag (YamlTag.core "str") (YamlNode.withAnchor "a" literal))
        expectRoot "k: &a !!str |\n  x \"q\"\nm: \"n\"\n" (mapOf [ "k", tagged; "m", dq "n" ])
        expectRoot "k: !<tag:yaml.org,2002:str> |\n  x \"q\"\nm: \"n\"\n" (mapOf [ "k", YamlNode.withTag (YamlTag.core "str") literal; "m", dq "n" ])
        expectRoot "- !<tag:yaml.org,2002:str> |\n  x \"q\"\n- 'v'\n" (seqOf [ YamlNode.withTag (YamlTag.core "str") literal; sq "v" ])
        expectRoot "- !<tag:yaml.org,2002:str> 'v'\n" (seqOf [ YamlNode.withTag (YamlTag.core "str") (sq "v") ])

    // Earlier test "an anchor in a flow collection needs a name". yaml@2.6.1 rejects all three inputs with BAD_ALIAS.
    testCase "properties that cannot stand together or touch the node are errors" <| fun _ ->
        expectError "[!!str !!int a]" "duplicate-tag@1"
        expectError "[&a &b a]" "duplicate-anchor@1"
        expectError "k: [&a *b]" "alias-with-properties@1"
        expectError "k: !!seq[a]" "missing-separation@1"
        expectError "k: &x{a: 1}" "missing-separation@1"
        expectError "[!!str\"x\"]" "missing-separation@1"
        expectError "k: [&]" "empty-anchor@1"
        expectError "k: [& a]" "empty-anchor@1"
        expectError "k: {a: &}" "empty-anchor@1"
        expectError "[!x!y a]" "undeclared-tag-handle@1"
]

let private errorTests = testList "Flow errors" [
    // Earlier test "a comma behind a bracket or behind a comma is an error and a trailing comma is not". yaml@2.6.1 rejects all of these with UNEXPECTED_TOKEN.
    testCase "a comma with no entry in front of it is an error" <| fun _ ->
        expectError "[a,,b]" "empty-flow-entry@1"
        expectError "k: [,a]" "empty-flow-entry@1"
        expectError "{,a}" "empty-flow-entry@1"
        expectError "{a,,b}" "empty-flow-entry@1"
        expectError "[ , ]" "empty-flow-entry@1"
        expectError "[a, # c\n , b]" "empty-flow-entry@2"
        expectError "k: {\n  a: 1,\n  ,\n}" "empty-flow-entry@3"
        expectError "[a,\n\n,]" "empty-flow-entry@3"

    // Earlier tests "a node in front of a flow collection needs a comma or a colon behind it" and "an entry of a flow collection can hold only one colon".
    // yaml@2.6.1 rejects all of these with MISSING_CHAR, UNEXPECTED_TOKEN or BLOCK_IN_FLOW.
    testCase "two entries with no comma between them are an error" <| fun _ ->
        expectError "k: [a [b]]" "missing-flow-comma@1"
        expectError "k: [a {b}]" "missing-flow-comma@1"
        expectError "[a\n [b]]" "missing-flow-comma@2"
        expectError "k: [ function f(a){ return a; } ]" "missing-flow-comma@1"
        expectError "[\"a\" b]" "missing-flow-comma@1"
        expectError "{a: b c: d}" "missing-flow-comma@1"
        expectError "[a: b: c]" "missing-flow-comma@1"
        expectError "{a: b: c}" "missing-flow-comma@1"
        expectError "{\"a\": \"b\": c}" "missing-flow-comma@1"
        expectError "k: {a: 1,\n  b: 2: 3}" "missing-flow-comma@2"
        expectError "[a # c\n b]" "missing-flow-comma@2"
        expectError "[a [b], c]" "missing-flow-comma@1"

    // Earlier tests "a flow collection that is still open at the end or at a document marker is an error" and "a closer of the other kind is an error that
    // names the line of the opener". yaml@2.6.1 rejects all of these. The error stands where the text ends, and Related is the opener.
    // Tests.YamlRead "A flow collection that does not close over several lines is an error that names the opener" is the same rule for the Reader: the old message named the line of the opener, and the
    // new error stands at the end of the text with the opener as the related span.
    testCase "an unclosed collection is an error that names its opener" <| fun _ ->
        expectError "[a" "unclosed-flow@1"
        expectError "{a: 1" "unclosed-flow@1"
        expectError "k: [\n  x" "unclosed-flow@2"
        expectError "a: 1\nk: [\n  x\n" "unclosed-flow@4"
        expectError "[[a]" "unclosed-flow@1"
        expectError "[a,\n---\nk: v" "marker-in-flow@2"
        expectError "[a,\n...\n" "marker-in-flow@2"
        let error = parseError "a: 1\nk: [\n  {a: b\n"
        Expect.equal (error.Related |> List.map showSpan) [ "3:3-3:4" ] "The opener of the collection that is still open"
        Expect.stringContains error.Message "Line 4" "The message names the line"

    testCase "a closer that does not match its opener is an error that names the opener" <| fun _ ->
        expectError "k: [a}" "mismatched-flow-closer@1"
        expectError "k: {a]" "mismatched-flow-closer@1"
        expectError "a: 1\nk: [\n  {a: b\n  ]" "mismatched-flow-closer@4"
        expectError "[a: b}" "mismatched-flow-closer@1"
        expectError "[? a }" "mismatched-flow-closer@1"
        expectError "{a, ]" "mismatched-flow-closer@1"
        let error = parseError "k: [a,\n  b}"
        Expect.equal (error.Related |> List.map showSpan) [ "1:4-1:5" ] "The opener"

    testCase "a closer outside a flow collection is an error" <| fun _ ->
        expectError "]" "unopened-flow@1"
        expectError "}" "unopened-flow@1"
        expectError "a: ]" "unopened-flow@1"
        expectError "[a]]" "unopened-flow@1"
        expectError "{a: 1}}" "unopened-flow@1"
        expectError "k: [a]\n]" "unopened-flow@2"

    // Earlier test "text behind a flow collection is an error". yaml@2.6.1 rejects all five inputs with UNEXPECTED_TOKEN. A flow collection ends its node.
    testCase "text behind a flow collection is an error" <| fun _ ->
        expectError "k: [a] b" "unexpected-content@1"
        expectError "[a] [b]" "unexpected-content@1"
        expectError "k: [\n  a\n] x" "unexpected-content@3"
        expectError "k: [[a] b]" "missing-flow-comma@1"
        expectError "[a]\n[b]" "unexpected-content@2"
        expectError "k: [a]:b" "unexpected-content@1"

    // Earlier test "a question mark in a flow collection is an explicit key only in front of a blank or a flow indicator". The reader before 2.0.0 accepted a question
    // mark in front of a flow indicator (yaml@2.6.1 reads {"":null}). [142] and [150] ask for a separator behind the question mark, and a flow indicator is
    // none, so the question mark starts neither an explicit key nor a plain scalar.
    testCase "a question mark in front of a flow indicator is an error" <| fun _ ->
        expectError "[?]" "reserved-indicator@1"
        expectError "{?}" "reserved-indicator@1"
        expectError "[?, a]" "reserved-indicator@1"
        expectError "{?,}" "reserved-indicator@1"
        expectError "[?[a]]" "reserved-indicator@1"

    // Earlier test "a flow collection never holds a header". yaml@2.6.1 rejects the block scalar headers inside a collection.
    testCase "a block scalar cannot start inside a flow collection" <| fun _ ->
        expectError "a: [|" "block-scalar-position@1"
        expectError "[a, b: |" "block-scalar-position@1"
        expectError "{a: >" "block-scalar-position@1"
        expectError "a: { b: >" "block-scalar-position@1"

    testCase "a dash inside a flow collection is an error" <| fun _ ->
        expectError "[-]" "flow-dash@1"
        expectError "[a, - b]" "flow-dash@1"
        expectError "[a,\n- b]" "flow-dash@2"

    testCase "the message of a flow error names its line" <| fun _ ->
        Expect.stringContains (parseError "k: [a,,b]").Message "Line 1" "The message names the line"
        Expect.stringContains (parseError "[a,\n\n,]").Message "Line 3" "The message names the line of the comma"
]

let private depthTests = testList "Depth" [
    // The flow frames stand on the explicit stack of the parser, so a nesting of 10000 levels needs no deep call stack. Validate, equality, the projections
    // and the printout walk the tree with a stack of their own.
    testCase "10000 nested flow sequences parse without a stack overflow and work with every walker" <| fun _ ->
        let input = String.replicate 10000 "[" + String.replicate 10000 "]"
        let stream = parseOk input
        Expect.stringContains (Projection.data stream) "\"maxDepth\":10000" "The depth"
        Expect.isTrue (YamlStream.equalIgnoringSpans stream (parseOk input)) "Equality of two parses"
        Expect.isTrue ((YamlStream.show stream).Length > 0) "The printout"
        Expect.isTrue ((Projection.presentation stream).Length > 0) "The presentation projection"
        Expect.isEmpty (Validate.stream stream) "The slot rules"

    testCase "10000 nested flow mappings parse without a stack overflow and work with every walker" <| fun _ ->
        let input = String.replicate 10000 "{a: " + "x" + String.replicate 10000 "}"
        let stream = parseOk input
        Expect.stringContains (Projection.data stream) "\"maxDepth\":10000" "The depth"
        Expect.isTrue (YamlStream.equalIgnoringSpans stream (parseOk input)) "Equality of two parses"
        Expect.isTrue ((YamlStream.show stream).Length > 0) "The printout"
        Expect.isTrue ((Projection.presentation stream).Length > 0) "The presentation projection"
        Expect.isEmpty (Validate.stream stream) "The slot rules"

    testCase "10000 nested flow pairs and properties parse without a stack overflow" <| fun _ ->
        let input = String.replicate 10000 "[&a !!seq " + "x" + String.replicate 10000 "]"
        let stream = parseOk input
        Expect.stringContains (Projection.data stream) "\"maxDepth\":10000" "The depth"
        let pairs = String.replicate 5000 "[a: " + "x" + String.replicate 5000 "]"
        Expect.stringContains (Projection.data (parseOk pairs)) "\"maxDepth\":10000" "A pair counts as a mapping in a sequence"

    testCase "an unclosed nesting of 10000 levels is an error with the innermost opener" <| fun _ ->
        let error = parseError (String.replicate 10000 "[")
        Expect.equal (YamlErrorCode.name error.Code) "unclosed-flow" "The code"
        Expect.equal (error.Related |> List.map showSpan) [ "1:10000-1:10001" ] "The innermost opener"
]

// The inputs of the scaling tests. The reference is a single pass over the characters.

/// A flow sequence of `lines` short items on one line.
let private longSequenceInput (lines: int) =
    let sb = System.Text.StringBuilder()
    sb.Append("k: [") |> ignore
    for i in 0 .. lines - 1 do
        if i > 0 then sb.Append(", ") |> ignore
        sb.Append(sprintf "item%d" i) |> ignore
    sb.Append("]\n") |> ignore
    sb.ToString()

/// A flow mapping with one entry on each line, with comments, quoted keys and pairs.
let private longMappingInput (lines: int) =
    let sb = System.Text.StringBuilder()
    sb.Append("k: {\n") |> ignore
    for i in 0 .. lines - 1 do
        if i % 5 = 0 then sb.Append(sprintf "  # comment %d\n" i) |> ignore
        if i % 3 = 0 then sb.Append(sprintf "  \"key%d\": [a, b: c], # trailing\n" i) |> ignore
        elif i % 7 = 1 then sb.Append(sprintf "  ? &a%d !!str key%d : !!str value,\n" i i) |> ignore
        else sb.Append(sprintf "  key%d: value %d,\n" i i) |> ignore
    sb.Append("}\n") |> ignore
    sb.ToString()

/// Flow collections that nest to a depth of 4 and repeat. Deeper nesting is covered by the depth tests, and the scaling test would take minutes on Python.
let private nestedInput (lines: int) =
    let sb = System.Text.StringBuilder()
    for i in 0 .. lines - 1 do
        sb.Append("- ") |> ignore
        sb.Append(String.replicate 2 "[a, {b: ") |> ignore
        sb.Append(sprintf "v%d" i) |> ignore
        sb.Append(String.replicate 2 "}]") |> ignore
        sb.Append("\n") |> ignore
    sb.ToString()

/// Documents that hold a flow collection as the root.
let private flowDocumentInput (lines: int) =
    let sb = System.Text.StringBuilder()
    for i in 0 .. lines - 1 do
        sb.Append(sprintf "--- [%d, {a: b}]\n" i) |> ignore
    sb.ToString()

/// Plain scalars over many lines inside a flow sequence.
let private foldedInput (lines: int) =
    let sb = System.Text.StringBuilder()
    sb.Append("k: [\n") |> ignore
    for i in 0 .. lines - 1 do
        sb.Append(sprintf "  word%d\n" i) |> ignore
    sb.Append("]\n") |> ignore
    sb.ToString()

let private parseOnly (input: string) = Parser.parseStream input |> ignore

let private performanceTests = testList "Flow performance" [
    testCase "a long flow sequence on one line takes time linear in its length" <| fun _ ->
        expectLinear "The parser on a one-line flow sequence" parseOnly countLineBreaks longSequenceInput

    // Earlier tests "the read time grows linearly with the lines of a flow collection" and "the time grows linearly with the entries of a flow mapping with explicit keys and properties".
    testCase "a flow mapping over many lines takes time linear in the number of lines" <| fun _ ->
        expectLinear "The parser on a multi-line flow mapping" parseOnly countLineBreaks longMappingInput

    testCase "nested flow collections take time linear in the length of the input" <| fun _ ->
        expectLinear "The parser on nested flow collections" parseOnly countLineBreaks nestedInput

    testCase "a stream of documents with flow roots takes time linear in the number of documents" <| fun _ ->
        expectLinear "The parser on documents with flow roots" parseOnly countLineBreaks flowDocumentInput

    testCase "a plain scalar over many lines in a flow sequence takes time linear in the number of lines" <| fun _ ->
        expectLinear "The parser on a folded scalar in a flow sequence" parseOnly countLineBreaks foldedInput
]

let Main =
    testList "Flow" [
        valueTests
        shapeTests
        pairTests
        entryTests
        scalarTests
        endOfInputTests
        layoutTests
        propertyTests
        errorTests
        depthTests
        performanceTests
    ]
