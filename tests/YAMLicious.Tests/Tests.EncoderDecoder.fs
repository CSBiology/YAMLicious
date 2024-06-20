module Tests.EncoderDecoder

module ValidationPackageTypes =

    type Package = {
        Name: string
        Version: string option
    }

    type ArcValidation = {
        ArcSpecificationVersion: string
        Packages: Package list
    }   
    
    type ArcValidationExtensionData = {
        ArcSpecificationVersion: string
        Packages: Package list
        ExtensionData: System.Collections.Generic.Dictionary<string, YAMLicious.YAMLiciousTypes.YAMLElement>
    }

open ValidationPackageTypes

module Examples =

    module ValidationPackageTypes =

        let string = "arc_specification: 2.0.0-draft
validation_packages:
  - name: package1
    version: 1.0.0
  - name: package2
    version: 2.0.0
  - name: package3"
        let stringOverflow = "arc_specification: 2.0.0-draft
validation_packages:
  - name: package1
    version: 1.0.0
  - name: package2
    version: 2.0.0
  - name: package3
author: TestAuthor
author_emails:
  - test1@email.com
  - test2@email.com"

        let type_ = {
            ArcSpecificationVersion = "2.0.0-draft"
            Packages = [
                { Name = "package1"; Version = Some "1.0.0" }
                { Name = "package2"; Version = Some "2.0.0" }
                { Name = "package3"; Version = None }
            ]
        }
        let typeOverflow_ = {
            ArcSpecificationVersion = "2.0.0-draft"
            Packages = [
                { Name = "package1"; Version = Some "1.0.0" }
                { Name = "package2"; Version = Some "2.0.0" }
                { Name = "package3"; Version = None }
            ]
            ExtensionData = 
                let dict = System.Collections.Generic.Dictionary<string, YAMLicious.YAMLiciousTypes.YAMLElement>()
                dict.Add("author", 
                    YAMLicious.YAMLiciousTypes.YAMLElement.Object [
                        YAMLicious.YAMLiciousTypes.YAMLElement.Value (
                            YAMLicious.YAMLiciousTypes.YAMLContent.create("TestAuthor")
                        )
                    ]
                )
                dict.Add("author_emails", 
                    YAMLicious.YAMLiciousTypes.YAMLElement.Object [
                        YAMLicious.YAMLiciousTypes.YAMLElement.Sequence [
                            YAMLicious.YAMLiciousTypes.YAMLElement.Object [
                                YAMLicious.YAMLiciousTypes.YAMLElement.Value (
                                    YAMLicious.YAMLiciousTypes.YAMLContent.create("test1@email.com")
                                )
                            ]
                            YAMLicious.YAMLiciousTypes.YAMLElement.Object [
                                YAMLicious.YAMLiciousTypes.YAMLElement.Value (
                                    YAMLicious.YAMLiciousTypes.YAMLContent.create("test2@email.com")
                                )
                            ]
                        ]
                    ]
                )
                dict
        }

open Fable.Pyxpecto
open YAMLicious

let tests_decode = testList "min decode" [
    testCase "integer" <| fun _ ->
        let ele = "23"
        let actual = ele |> Decode.read |> Decode.int
        let expected = 23
        Expect.equal actual expected ""
    testCase "float" <| fun  _ ->
        let ele = "123.45678"
        let actual = ele |> Decode.read |> Decode.float
        let expected = 123.45678
        Expect.floatClose Accuracy.medium actual expected ""
    testCase "char" <| fun _ ->
        let ele = "c"
        let actual = ele |> Decode.read |> Decode.char
        let expected = 'c'
        Expect.equal actual expected ""
    testCase "bool" <| fun _ ->
        let ele = "true"
        let actual = ele |> Decode.read |> Decode.bool
        let expected = true
        Expect.equal actual expected ""
    testCase "map" <| fun _ ->
        let ele = """my key 1: 1
my key 2: 2
my key 3: 3"""
        let actual = ele |> Decode.read |> Decode.map (id) (Decode.int)
        let expected = Map ["my key 1", 1; "my key 2", 2; "my key 3", 3]
        Expect.equal actual expected ""
    testCase "dict" <| fun _ ->
        let ele = """my key 1: 1
my key 2: 2
my key 3: 3"""
        let actual = ele |> Decode.read |> Decode.dict (id) (Decode.int)
        let expected = Map ["my key 1", 1; "my key 2", 2; "my key 3", 3] |> System.Collections.Generic.Dictionary
        Expect.dictEqual actual expected ""
    ptestCase "datetime" <| fun _ ->
        let ele = """2018-10-01T11:12:55.00Z"""
        let actual = ele |> Decode.read |> Decode.datetime
        let expected = System.DateTime(2018, 10, 1, 13, 12, 55)
        Expect.equal actual expected ""
    ptestCase "datetimeOffset" <| fun _ ->
        let ele = """2008-01-05 6:00:00"""
        let actual = ele |> Decode.read |> Decode.datetimeOffset
        let expected = System.DateTimeOffset(2008, 1, 5, 6, 0, 0, System.TimeSpan(1,0,0))
        Expect.equal actual expected ""
    testCase "some option" <| fun _ ->
        let ele = """42"""
        let actual = ele |> Decode.read |> Decode.option Decode.int
        let expected = Some 42
        Expect.equal actual expected ""
    testCase "none option" <| fun _ ->
        let ele = """null"""
        let actual = ele |> Decode.read |> Decode.option Decode.int
        let expected = None
        Expect.equal actual expected ""
    testCase "tuple 2" <| fun _ ->
        let ele = "[1, Hello World]"
        let actual = ele |> Decode.read |> Decode.tuple2 Decode.int Decode.string
        let expected = (1, "Hello World")
        Expect.equal actual expected ""
    testCase "tuple 8" <| fun _ ->
        let ele = "[1, Hello World, 3, Bye Bye, 5, true, false, 8]"
        let actual = 
            ele 
            |> Decode.read 
            |> Decode.tuple8 
                Decode.int 
                Decode.string 
                Decode.int 
                Decode.string 
                Decode.int 
                Decode.bool 
                Decode.bool 
                Decode.int
        let expected = (1, "Hello World", 3, "Bye Bye", 5, true, false, 8)
        Expect.equal actual expected ""
    testCase "list" <| fun _ ->
        let ele = """- 1
- 2
- 3"""
        let actual = ele |> Decode.read |> Decode.list Decode.int
        let expected = [1; 2; 3]
        Expect.equal actual expected ""
    testCase "array" <| fun _ ->
        let ele = """- 1
- 2
- 3"""
        let actual = ele |> Decode.read |> Decode.array Decode.int
        let expected = [|1; 2; 3|]
        Expect.equal actual expected ""
    testCase "seq" <| fun _ ->
        let ele = """- 1
- 2
- 3"""
        let actual = ele |> Decode.read |> Decode.seq Decode.int
        let expected = seq {1; 2; 3}
        Expect.seqEqual actual expected ""
    testCase "resizearray" <| fun _ ->
        let ele = """- 1
- 2
- 3"""
        let actual = ele |> Decode.read |> Decode.resizearray Decode.int
        let expected = ResizeArray([1; 2; 3])
        Expect.seqEqual actual expected ""
    testCase "object required" <| fun _ ->
        let ele = """myValue1: [1,2,3,4]
Hello World: 42
ByeBye World: oh no"""
        let actual = ele |> Decode.read |> Decode.object (fun get ->
            {|
                Field1 = get.Required.Field "myValue1" (Decode.list Decode.int)
                Field2 = get.Required.Field "Hello World" Decode.int
                Field3 = get.Required.Field "ByeBye World" Decode.string
            |}
        )
        let expected = {|
            Field1 = [1;2;3;4]
            Field2 = 42
            Field3 = "oh no"
        |}
        Expect.equal actual expected ""
    testCase "object optional" <| fun _ ->
        let ele = """myValue1: [1,2,3,4]
Hello World: 42"""
        let actual = ele |> Decode.read |> Decode.object (fun get ->
            {|
                Field1 = get.Optional.Field "myValue1" (Decode.list Decode.int)
                Field2 = get.Optional.Field "Hello World" Decode.int
                Field3 = get.Optional.Field "ByeBye World" Decode.string
            |}
        )
        let expected: {|Field1: int list option; Field2: int option; Field3: string option|} = {|
            Field1 = Some [1;2;3;4]
            Field2 = Some 42
            Field3 = None
        |}
        Expect.equal actual.Field1 expected.Field1 "field1"
        Expect.equal actual.Field2 expected.Field2 "field2"
        Expect.equal actual.Field3 expected.Field3 "field3"
    testCase "values string" <| fun _ ->
        let ele = """MyKey: 
    test1
    test2
    test3"""
        let actual = ele |> Decode.read |> Decode.object (fun get ->
            get.Required.Field "MyKey" (Decode.values Decode.string)
        )
        let expected = ["test1"; "test2"; "test3"]
        Expect.equal actual expected ""
    testCase "values bool" <| fun _ ->
        let ele = """MyKey: 
    true
    true
    false"""
        let actual = ele |> Decode.read |> Decode.object (fun get ->
            get.Required.Field "MyKey" (Decode.values Decode.bool)
        )
        let expected = [true; true; false]
        Expect.equal actual expected ""
]

let tests_validationPackages = testList "ValidationPackages" [
    testCase "encode" <| fun _ ->
        
        let packageEncoder (pack: Package) = 
            [
                "name", Encode.string pack.Name
                Encode.tryInclude "version" Encode.string pack.Version 
            ]
            |> Encode.choose
            |> Encode.object
        let arcValidationEncoder (arc: ArcValidation) = 
            Encode.object [
                "arc_specification", Encode.string arc.ArcSpecificationVersion
                "validation_packages", Encode.list packageEncoder arc.Packages
            ]
        let actual = Encode.write 2 (arcValidationEncoder Examples.ValidationPackageTypes.type_)
        let expected = "arc_specification: 2.0.0-draft
validation_packages:
  -
    name: package1
    version: 1.0.0
  -
    name: package2
    version: 2.0.0
  -
    name: package3"
        Expect.trimEqual actual expected ""
    testCase "decode" <| fun _ ->
        let packageEncoder = Decode.object (fun get ->
            {
                Name = get.Required.Field "name" Decode.string
                Version = get.Optional.Field "version" Decode.string
            }
        )
        let arcValidationDecoder =
            Decode.object (fun get ->
                {
                    ArcSpecificationVersion = get.Required.Field "arc_specification" Decode.string
                    Packages = get.Required.Field "validation_packages" (Decode.list packageEncoder)
                }
            )
        let actual = Examples.ValidationPackageTypes.string |> Decode.read |> arcValidationDecoder
        let expected = Examples.ValidationPackageTypes.type_
        Expect.equal actual expected ""       
    testCase "decode overflow" <| fun _ ->
        let packageEncoder = Decode.object (fun get ->
            {
                Name = get.Required.Field "name" Decode.string
                Version = get.Optional.Field "version" Decode.string
            }
        )
        let arcValidationDecoder =
            Decode.object (fun get ->
                {
                    ArcSpecificationVersion = get.Required.Field "arc_specification" Decode.string
                    Packages = get.Required.Field "validation_packages" (Decode.list packageEncoder)
                    ExtensionData = get.Overflow.FieldList ["author";"author_emails"]
                }
            )
        let actual = Examples.ValidationPackageTypes.stringOverflow |> Decode.read |> arcValidationDecoder
        let expected = Examples.ValidationPackageTypes.typeOverflow_
        Expect.equal actual.ArcSpecificationVersion expected.ArcSpecificationVersion ""
        Expect.equal actual.Packages expected.Packages ""
        Expect.dictEqual actual.ExtensionData expected.ExtensionData ""
    testCase "encode overflow" <| fun _ ->
        
        let packageEncoder (pack: Package) = 
            [
                "name", Encode.string pack.Name
                Encode.tryInclude "version" Encode.string pack.Version 
            ]
            |> Encode.choose
            |> Encode.object
        let arcValidationEncoder (arc: ArcValidationExtensionData) = 
            let extensionData =
                arc.ExtensionData.Keys
                |>Seq.map (fun key ->
                        key, arc.ExtensionData.[key]
                )
                |>List.ofSeq
            let typedData =
                [
                    "arc_specification", Encode.string arc.ArcSpecificationVersion
                    "validation_packages", Encode.list packageEncoder arc.Packages
                ]
            Encode.object (typedData@extensionData)
        let actual = Encode.write 2 (arcValidationEncoder Examples.ValidationPackageTypes.typeOverflow_)
        let expected = "arc_specification: 2.0.0-draft
validation_packages:
  -
    name: package1
    version: 1.0.0
  -
    name: package2
    version: 2.0.0
  -
    name: package3
author:
  TestAuthor
author_emails:
  -
    test1@email.com
  -
    test2@email.com"
        Expect.trimEqual actual expected ""
]

let Main = testList "EncoderDecoder" [
    tests_decode
    tests_validationPackages
]