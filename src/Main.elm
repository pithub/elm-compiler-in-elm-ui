module Main exposing (main)

import Browser
import Builder.Build as Build
import Builder.Elm.Details as Details
import Builder.Generate as Generate
import Builder.Http as Http
import Extra.System.File as SysFile
import Extra.System.IO as IO
import Global
import Reactor.Index as Reactor
import Terminal.Command as Terminal
import Terminal.Repl as Repl


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    Reactor.State


initialModel : Model
initialModel =
    Global.State
        SysFile.initialState
        Http.initialState
        Details.initialState
        Build.initialState
        Generate.initialState
        Terminal.initialState
        Repl.initialLocalState
        Reactor.initialState



-- INIT


init : () -> ( Model, Cmd Msg )
init () =
    update initialIO initialModel



-- UPDATE


type alias Msg =
    IO.IO Model ()


initialIO : Msg
initialIO =
    Reactor.initialIO (IO.return True)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg model of
        ( IO.Pure (), newModel ) ->
            ( newModel, Cmd.none )

        ( IO.ImpureCmd cmd, newModel ) ->
            ( newModel, cmd )

        ( IO.ImpureCont cont, newModel ) ->
            update (cont identity) newModel



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    Reactor.view model
