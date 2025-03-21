module Test.Main exposing (main)

import Builder.Build
import Builder.Elm.Details
import Builder.Generate
import Builder.Http
import Extra.System.File
import Global
import Terminal.Command
import Terminal.Repl


main : Program () (Terminal.Repl.GlobalState ()) ()
main =
    Platform.worker
        { init =
            \_ ->
                ( Global.State
                    Extra.System.File.initialState
                    Builder.Http.initialState
                    Builder.Elm.Details.initialState
                    Builder.Build.initialState
                    Builder.Generate.initialState
                    Terminal.Command.initialState
                    Terminal.Repl.initialLocalState
                    ()
                , Cmd.none
                )
        , subscriptions = \_ -> Sub.none
        , update = \_ m -> ( m, Cmd.none )
        }
