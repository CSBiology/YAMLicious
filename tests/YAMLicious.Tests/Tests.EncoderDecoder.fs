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

        let type_ = {
            ArcSpecificationVersion = "2.0.0-draft"
            Packages = [
                { Name = "package1"; Version = Some "1.0.0" }
                { Name = "package2"; Version = Some "2.0.0" }
                { Name = "package3"; Version = None }
            ]
        }

open Fable.Pyxpecto
open YAMLicious


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
    //testCase "decode" <| fun _ ->
    //    let packageEncoder = Decode.object (fun get ->
    //        {
    //            Name = get.Required.Field "name" Decode.string
    //            Version = get.Optional.Field "version" Decode.string
    //        }
    //    )
    //    let arcValidationDecoder =
    //        Decode.object (fun get ->
    //            {
    //                ArcSpecificationVersion = get.Required.Field "arc_specification" Decode.string
    //                Packages = get.Required.Field "validation_packages" (Decode.list packageEncoder)
    //            }
    //        )
    //    let actual = Examples.ValidationPackageTypes.string |> Decode.read |> arcValidationDecoder
    //    let expected = Examples.ValidationPackageTypes.type_
    //    Expect.equal actual expected ""
    ftestCase "decode integer" <| fun _ ->
        let ele = "23"
        let actual = ele |> Decode.read |> Decode.int
        let expected = 23
        Expect.equal actual expected ""
]

let Main = testList "EncoderDecoder" [
    tests_validationPackages
]