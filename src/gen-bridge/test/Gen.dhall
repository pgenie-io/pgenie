let Contract = ./Contract.dhall

let Prelude =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/v23.1.0/Prelude/package.dhall
        sha256:931cbfae9d746c4611b07633ab1e547637ab4ba138b16bf65ef1b9ad66a60b7f

let Report = { path : List Text, message : Text }

let File = { path : Text, content : Text }

let Output =
      < Ok : { warnings : List Report, value : List File } | Err : Report >

let Config = { foo : Text, bar : Optional Natural }

in  Contract.module
      Config
      ( \(config : Optional Config) ->
        \(project : Contract.Project) ->
          Output.Ok
            { warnings = [] : List Report
            , value =
              [ { path = "output.yaml"
                , content =
                    merge
                      { None = "config: null"
                      , Some =
                          \(config : Config) ->
                            ''
                            config:
                              foo: ${config.foo}
                              bar: ${Prelude.Optional.fold
                                       Natural
                                       config.bar
                                       Text
                                       Natural/show
                                       "null"}
                            ''
                      }
                      config
                }
              ]
            }
      )
