module Tests.YamlRead

open Fable.Pyxpecto
open YAMLicious
open AST

let Main = testList "YamlRead" [
    testCase "Example 1" <| fun _ ->
        let yaml = "My Key: My Value"
        let actual = YamlAST.read yaml
        let expected = Level [
            Line "My Key: My Value"
        ]
        Expect.equal actual expected ""
    testCase "Example 2" <| fun _ ->
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
        Expect.equal actual expected ""
    testCase "Example 3" <| fun _ ->
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
        Expect.equal actual expected ""
    testCase "Example-Mermaid" <| fun _ ->
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
        Expect.equal actual expected ""
        
]