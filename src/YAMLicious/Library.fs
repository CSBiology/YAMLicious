namespace YAMLicious

open YAMLiciousTypes
open AST

module TestCases =

    [<Literal>]
    let KeyValue = """
My Key: My Value    
"""

    [<Literal>]
    let String = """
My Value
"""

    [<Literal>]
    let Sequence = """
- My Value 1
- My Value 2
- My Value 3
"""

    [<Literal>]
    let SequenceObjects = """
- My Value 1
  My Value 2
- My Value 3
"""

    [<Literal>]
    let SequenceImplicit = """
My Key:
  My Value1
  My Value2
  My Value3
"""
    [<Literal>]
    let SequenceMappings = """
-
  My Key1: My Value1
  My Key2: My Value2
  My Key3: My Value3
-
  My Key4: My Value4
  My Key5: My Value5
  My Key6: My Value6
"""

    [<Literal>]
    let SequenceOfSequences = """
- [v1, v2, v3]
- [v4, v5, v6]
- [v7, v8, v9]
""" 

    [<Literal>]
    let SequenceOfSequences2 = """
- 
  [v1, v2, v3]
- 
  [v4, v5, v6]
- 
  [v7, v8, v9]
"""
 
    [<Literal>]
    let SequenceSquare = """
[
  v1,
  v2,
  v3
]
"""
    
    let SequenceMappingsAST = 
        Level [
            Line "-"
            Intendation [
                Line "My Key1: My Value1"
                Line "My Key2: My Value2"
                Line "My Key3: My Value3"
            ]
            Line "-"
            Intendation [
                Line "My Key4: My Value4"
                Line "My Key5: My Value5"
                Line "My Key6: My Value6"
            ]
        ]

    [<Literal>]
    let LineComment = """
# This is a line comment    
"""

    [<Literal>]
    let InlineComment = """
My Key: # This is a comment
  My Value1 
  My Value2
  My Value3 # :::: This is also a comment
"""

    [<Literal>]
    let StringReplace = """
My Key: "[{Special character place # |}"
"""

    let StringReplaceClean = """
My Key: </1>
"""

    [<Literal>]
    let StringReplaceWithComment = """
My Key: "[{Special character place # |}" # A # in string is allowed!
"""

    [<Literal>]
    let StringReplaceWithCommentHashs = """
My Key: "[{Special character place # |}" ### A # in string is allowed!
"""