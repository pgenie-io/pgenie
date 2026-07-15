let Contract = ./ContractV4.dhall

let Report = { path : List Text, message : Text }

let File = { path : Text, content : Text }

let Output =
      < Ok : { warnings : List Report, value : List File } | Err : Report >

let Config = { foo : Text }

in  Contract.module
      Config
      ( \(config : Optional Config) ->
        \(project : Contract.Project) ->
          Output.Ok
            { warnings = [] : List Report
            , value =
              [ { path = "custom-type-names.yaml"
                , content =
                    let renderCustomType =
                          \(customType : Contract.CustomType) ->
                            "- ${customType.name.inKebabCase}\n"

                    in  List/fold
                          Contract.CustomType
                          project.customTypes
                          Text
                          (\(customType : Contract.CustomType) -> \(rest : Text) -> renderCustomType customType ++ rest)
                          ""
                }
              ]
            }
      )
