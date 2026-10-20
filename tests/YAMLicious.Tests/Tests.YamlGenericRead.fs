module Tests.YamlGenericRead

open Fable.Pyxpecto
open YAMLicious.YAMLiciousTypes
open YAMLicious.Preprocessing

let Main = testList "YamlGenericRead" [
    testCase "Example KeyValue" <| fun _ ->
        let yaml = "My Key: My Value"
        let actual = read yaml
        let expected = Level [
            Line "My Key: My Value"
        ]
        Expect.equal actual.AST expected ""
    testCase "Example list" <| fun _ ->
        let yaml = """
- My Value 1
- My Value 2
- My Value 3
"""
        let actual = read yaml
        let expected = Level [
            Line "- My Value 1"
            Line "- My Value 2"
            Line "- My Value 3"
        ]
        Expect.equal actual.AST expected ""
    testCase "Example list shifted" <| fun _ ->
        let yaml = """
-
  My Key1: My Value1
  My Key2: My Value2
  My Key3: My Value3
-
  My Key4: My Value4
  My Key5: My Value5
  My Key6: My Value6
"""
        let actual = read yaml
        let expected = Level [
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
        Expect.equal actual.AST expected ""
    testCase "Example Mermaid" <| fun _ ->
        // yaml@2.6.1 reads the input as the plain scalar "classDiagram Animal <|-- Duck ... }". A line such as
        // `Animal : +int age` is invalid in it, because a plain scalar cannot hold a mapping key.
        let yaml = """
classDiagram
    Animal <|-- Duck
    Animal <|-- Fish
    Animal <|-- Zebra
    class Duck{
      +String beakColor
      +swim()
      +quack()
    }
    class Fish{
      -int sizeInFeet
      -canEat()
    }
    class Zebra{
      +bool is_wild
      +run()
    }
"""
        let actual = read yaml
        let expected = Level [ Line "<s f=0/>" ]
        Expect.equal actual.AST expected ""
        let lines =
            [ "classDiagram"; "Animal <|-- Duck"; "Animal <|-- Fish"; "Animal <|-- Zebra"
              "class Duck{"; "+String beakColor"; "+swim()"; "+quack()"; "}"
              "class Fish{"; "-int sizeInFeet"; "-canEat()"; "}"
              "class Zebra{"; "+bool is_wild"; "+run()"; "}" ]
        Expect.equal actual.StringMap.Count 1 "one string"
        Expect.equal actual.StringMap.[0] { Value = String.concat "\n" lines; Kind = QuotedStringKind.PlainScalar } "the lines of the scalar"
    testCase "Example Comment + String" <| fun _ ->
        // yaml@2.6.1: {"My Key":"My Value1 \"# This is not a comment!\" My Value3"}. The lines below the key
        // are one plain scalar, which the scanner moves into the string table. Its last line keeps the comment.
        let yaml = "
My Key: # This is a comment
  My Value1 
  \"# This is not a comment!\"
  My Value3 # :::: \"This is also a comment\""
        let actual = read yaml
        let expected = Level [
            Line "My Key: <c f=0/>"
            Intendation [
                Line "<s f=0/> <c f=1/>"
            ]
        ]
        let expectedCommentDict = new System.Collections.Generic.Dictionary<int, string>(Map [0, " This is a comment"; 1, " :::: \"This is also a comment\""])
        let expectedStringDict =
            new System.Collections.Generic.Dictionary<int, StringMapEntry>(
                Map [0, { Value = "My Value1\n\"# This is not a comment!\"\nMy Value3"; Kind = QuotedStringKind.PlainScalar }]
            )
        Expect.equal actual.AST expected "ast"
        Expect.dictEqual actual.CommentMap expectedCommentDict "comments"
        Expect.dictEqual actual.StringMap expectedStringDict "strings"

    testCase "Preprocessing read/write keeps stable structural indentation" <| fun _ ->
        let yaml = """a:
  b:
    c: d"""
        let pre = read yaml
        let written = write (pre.AST, None)
        let expected = """a:
    b:
        c: d"""
        let normalizeNewlines (s: string) = s.Replace("\r\n", "\n").Trim()
        Expect.equal (normalizeNewlines written) (normalizeNewlines expected) "read -> write should not duplicate indentation from parsed line payloads"
]
