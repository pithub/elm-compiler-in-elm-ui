module Main exposing (main)

import Browser
import Builder.Build as Build
import Builder.Elm.Details as Details
import Builder.Generate as Generate
import Extra.System.Config as Config
import Extra.System.Dir as Dir
import Extra.System.IO as IO
import Global
import Reactor.Index as Reactor
import Terminal.Command as Terminal
import Terminal.Repl as Repl


main : Program () Model Msg
main =
    Browser.document
        { init = IO.init initialModel initialIO
        , update = IO.update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    Reactor.State


initialModel : () -> Model
initialModel _ =
    Global.State
        Config.initialState
        Dir.initialState
        Details.initialState
        Build.initialState
        Generate.initialState
        Terminal.initialState
        Repl.initialLocalState
        Reactor.initialState



-- MSG


type alias Msg =
    IO.IO Model ()


initialIO : () -> Msg
initialIO _ =
    Reactor.initialIO (IO.return True)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    Reactor.view model
