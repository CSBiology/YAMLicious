module Tests.YamlRead

open Fable.Pyxpecto
open YAMLicious
open AST

let Main = testList "YamlRead" [
    testCase "Example KeyValue" <| fun _ ->
        let yaml = "My Key: My Value"
        let actual = YamlAST.read yaml
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
        let actual = YamlAST.read yaml
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
        let actual = YamlAST.read yaml
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
        let yaml = """
classDiagram
    Animal <|-- Duck
    Animal <|-- Fish
    Animal <|-- Zebra
    Animal : +int age
    Animal : +String gender
    Animal: +isMammal()
    Animal: +mate()
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
        let actual = YamlAST.read yaml
        let expected = Level [
            Line "classDiagram"
            Intendation [
                Line "Animal <|-- Duck"
                Line "Animal <|-- Fish"
                Line "Animal <|-- Zebra"
                Line "Animal : +int age"
                Line "Animal : +String gender"
                Line "Animal: +isMammal()"
                Line "Animal: +mate()"
                Line "class Duck{"
                Intendation [
                    Line "+String beakColor"
                    Line "+swim()"
                    Line "+quack()"
                ]
                Line "}"
                Line "class Fish{"
                Intendation [
                    Line "-int sizeInFeet"
                    Line "-canEat()"
                ]
                Line "}"
                Line "class Zebra{"
                Intendation [
                    Line "+bool is_wild"
                    Line "+run()"
                ]
                Line "}"
            ]
        ]
        Expect.equal actual.AST expected ""
    testCase "Example Comment + String" <| fun _ ->
        let yaml = "
My Key: # This is a comment
  My Value1 
  \"# This is not a comment!\"
  My Value3 # :::: \"This is also a comment\""
        printfn "---HERE---"
        let actual = YamlAST.read yaml
        let expected = Level [
            Line "My Key: <c f=0/>"
            Intendation [
                Line "My Value1"
                Line "<s f=0/>"
                Line "My Value3 <c f=1/>"
            ]
        ]
        let expectedCommentDict = new System.Collections.Generic.Dictionary<int, string>(Map [0, " This is a comment"; 1, " :::: \"This is also a comment\""])
        let expectedStringDict = new System.Collections.Generic.Dictionary<int, string>(Map [0, "# This is not a comment!"])
        Expect.equal actual.AST expected "ast"
        Expect.dictEqual actual.CommentMap expectedCommentDict "comments"
        Expect.dictEqual actual.StringMap expectedStringDict "strings"
]