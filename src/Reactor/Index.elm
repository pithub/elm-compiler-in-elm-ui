module Reactor.Index exposing
    ( LocalState
    , State
    , initialIO
    , initialState
    , view
    )

import Browser
import Browser.Dom as Dom
import Builder.Build as Build
import Builder.Deps.Registry as Registry
import Builder.Elm.Details as Details
import Builder.Elm.Outline as Outline
import Builder.File as File
import Builder.Http as Http
import Builder.Reporting.Exit as Exit
import Builder.Reporting.Exit.Help exposing (Report)
import Builder.Stuff as Stuff
import Bytes exposing (Bytes)
import Bytes.Decode as BD
import Bytes.Encode as BE
import Compiler.Data.Name as Name
import Compiler.Data.NonEmptyList as NE
import Compiler.Elm.Package as Pkg
import Compiler.Elm.Version as V
import Elm.Error as Error
import Extra.Class.Applicative as Applicative
import Extra.System.File as SysFile exposing (FileName, FilePath)
import Extra.System.IO as IO
import Extra.Type.Either as Either exposing (Either(..))
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map
import Extra.Type.Set as Set
import Global
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Reactor.Errors as Errors
import Reactor.Index.Icon as Icon
import Reactor.Index.Navigator as Navigator
import Reactor.Index.Skeleton as Skeleton
import Reactor.Solver as Solver
import Task
import Terminal.Command as Terminal
import Terminal.Helpers as Helpers
import Terminal.Init
import Terminal.Install
import Terminal.Main
import Terminal.Make
import Terminal.Reactor
import Terminal.Repl
import Time



-- PUBLIC STATE


type alias State a h =
    Terminal.State a (LocalState a h) h


type LocalState a h
    = LocalState
        -- zone
        Time.Zone
        -- shown
        Shown
        -- htmlEnabled
        (IO a h Bool)


type Shown
    = ShowFile FilePath FileContents
    | ShowRepl Bool (Maybe String) (Maybe Terminal.Repl.InterpreterInput)
    | ShowError Error.Error
    | ShowNothing
    | ShowFileAndRepl FilePath String String Bool (Maybe String) (Maybe Terminal.Repl.InterpreterInput)


type FileContents
    = TextContents Bool String
    | HexContents String String String
    | ElmContents String


type ShowMode
    = AsText
    | AsHex
    | AsElm


initialState : LocalState a h
initialState =
    LocalState
        -- zone
        Time.utc
        -- shown
        ShowNothing
        -- htmlEnabled
        (IO.pure False)


lensZone =
    { getter = \(Global.State _ _ _ _ _ _ (LocalState x _ _) _) -> x
    , setter = \x (Global.State a b c d e f (LocalState _ bi ci) h) -> Global.State a b c d e f (LocalState x bi ci) h
    }


lensShown =
    { getter = \(Global.State _ _ _ _ _ _ (LocalState _ x _) _) -> x
    , setter = \x (Global.State a b c d e f (LocalState ai _ ci) h) -> Global.State a b c d e f (LocalState ai x ci) h
    }


lensHtmlEnabled =
    { getter = \(Global.State _ _ _ _ _ _ (LocalState _ _ x) _) -> x
    , setter = \x (Global.State a b c d e f (LocalState ai bi _) h) -> Global.State a b c d e f (LocalState ai bi x) h
    }



-- PRIVATE IO


type alias IO a h v =
    IO.IO (State a h) v


initialIO : IO a h Bool -> IO a h ()
initialIO htmlEnabled =
    IO.sequence
        [ setHtmlEnabled htmlEnabled
        , getTimeZone
        , commandLoop
        ]



-- TIME


getTimeZone : IO a h ()
getTimeZone =
    IO.bind
        (IO.liftCmd <| Task.perform identity Time.here)
        (IO.putLens lensZone)


timeString : Time.Zone -> Time.Posix -> String
timeString zone time =
    String.padLeft 2 '0' (String.fromInt (Time.toHour zone time))
        ++ ":"
        ++ String.padLeft 2 '0' (String.fromInt (Time.toMinute zone time))



-- HTML ENABLED


setHtmlEnabled : IO a h Bool -> IO a h ()
setHtmlEnabled =
    IO.putLens lensHtmlEnabled


isHtmlEnabled : IO a h Bool
isHtmlEnabled =
    IO.join (IO.getLens lensHtmlEnabled)



-- COMMAND LOOP


commandLoop : IO a h ()
commandLoop =
    IO.bind (IO.bind Terminal.getLine executeCommand) (\_ -> commandLoop)


executeCommand : String -> IO a h ()
executeCommand command =
    IO.sequence
        [ clearDisplay
        , if String.startsWith "d " command then
            createDirectory (String.dropLeft 2 command)

          else if command == "" || command == "h" then
            showHelp

          else if String.startsWith "m " command then
            mount (String.dropLeft 2 command |> String.split " ")

          else if String.startsWith "r " command then
            removeEntry (String.dropLeft 2 command)

          else if command == "rl" then
            loadRegistry

          else if command == "op" then
            parseOutline

          else if command == "ov" then
            validateOutline

          else if command == "dl" then
            loadDetails

          else if String.startsWith "bp " command then
            buildFromPaths (String.dropLeft 3 command)

          else if command == "e" then
            elmMain

          else if command == "ei" then
            elmInit

          else if String.startsWith "ei " command then
            elmInstall (String.dropLeft 3 command)

          else if String.startsWith "em " command then
            if String.dropLeft 4 command |> String.startsWith " " then
                elmMake (String.dropLeft 3 command |> String.left 1) (String.dropLeft 5 command)

            else
                elmMake "v" (String.dropLeft 3 command)

          else if command == "er" then
            IO.bind isHtmlEnabled <|
                elmRepl Terminal.Repl.Normal Nothing

          else if String.startsWith "er " command then
            IO.bind isHtmlEnabled <|
                elmRepl (Terminal.Repl.Module (String.dropLeft 3 command)) Nothing

          else if command == "l" then
            showLicense

          else
            createFile command
        ]


showCommandDuration : String -> IO a h ()
showCommandDuration command =
    IO.bind Terminal.getDurationSinceLastInput <|
        \maybeDuration ->
            case maybeDuration of
                Nothing ->
                    IO.noOp

                Just duration ->
                    Terminal.putLine <| command ++ " took " ++ String.fromInt duration ++ " ms"


clearDisplay : IO a h ()
clearDisplay =
    IO.sequence
        [ clearIO
        , hideShown
        ]


clearIO : IO a h ()
clearIO =
    IO.sequence
        [ Terminal.clearInput
        , Terminal.clearStdOut
        ]


showHelp : IO a h ()
showHelp =
    Terminal.putLine """Intermediate Steps (for the Demo)

rl - registry load 
op - outline parse 
ov - outline validate
dl - details load
bp - build from paths


Elm Commands

e - elm (show help message)
ei - elm init
ei <package> - elm install <package>
em <mode> <file> - elm make (mode: v - dev, d - debug, o - optimize, a - async)
er - elm repl
er <module> - elm repl in <module>


File System

<file> - create <file>
d <path> - create directory <path>
r <path> - remove <path>
m <mount> <target> - mount <mount> to <target>


Other

h - show help (this text)
l - show original license
"""


showLicense : IO a h ()
showLicense =
    Terminal.putLine """Copyright 2012-present Evan Czaplicki

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
"""



-- IDS


type alias IdRecord =
    { elmCodeId : String
    , elmResultId : String
    , replItemsId : String
    , replResultEvent : String
    , breakpointSuspendedEvent : String
    }


idRecord : IdRecord
idRecord =
    { elmCodeId = "elm-code"
    , elmResultId = "elm-result"
    , replItemsId = "repl-items"
    , replResultEvent = "repl-result"
    , breakpointSuspendedEvent = "breakpoint-suspended"
    }



-- FILE OPERATIONS


createDirectory : String -> IO a h ()
createDirectory string =
    IO.bind (toPath string) (SysFile.createDirectoryIfMissing True)


createFile : String -> IO a h ()
createFile string =
    IO.bind (toPath string) (\filePath -> SysFile.writeFile filePath emptyBytes)


emptyBytes : Bytes
emptyBytes =
    BE.encode (BE.sequence [])


getExtension : FilePath -> String
getExtension filePath =
    case SysFile.splitLastName filePath of
        ( _, name ) ->
            Tuple.second (SysFile.splitExtension name)


mount : TList String -> IO a h ()
mount strings =
    case strings of
        [ mountPoint, target ] ->
            IO.bind (toPath target) (SysFile.mount mountPoint)

        _ ->
            IO.noOp


removeEntry : String -> IO a h ()
removeEntry string =
    IO.bind (toPath string) <|
        \path ->
            IO.sequence
                [ SysFile.removeFile path
                , SysFile.removeDirectory path
                ]


saveFile : IO a h ()
saveFile =
    IO.bind (IO.getLens lensShown) <|
        \shown ->
            case shown of
                ShowFile name (TextContents _ text) ->
                    File.writeUtf8 name text

                _ ->
                    IO.noOp


toPath : String -> IO a h FilePath
toPath string =
    SysFile.makeAbsolute (SysFile.fromString string)



-- CURRENT WORKING DIRECTORY


pushDirectory : FileName -> IO a h ()
pushDirectory fileName =
    ifReplNotShown <|
        IO.sequence
            [ clearDisplay
            , SysFile.setCurrentDirectory (SysFile.fromString fileName)
            ]


changeCwd : TList FileName -> IO a h ()
changeCwd cwd =
    ifReplNotShown <|
        IO.sequence
            [ clearDisplay
            , IO.bind SysFile.getCurrentDirectory <|
                \current ->
                    if SysFile.toString current == "/" && cwd == [] then
                        SysFile.resetFileSystem

                    else
                        SysFile.setCurrentDirectory (cwdToPath cwd)
            ]


cwdToPath : TList FileName -> FilePath
cwdToPath cwd =
    SysFile.addNames rootPath (MList.reverse cwd)


rootPath : FilePath
rootPath =
    SysFile.fromString "/"



-- FILE DISPLAY


hideShown : IO a h ()
hideShown =
    setShown ShowNothing


setShown : Shown -> IO a h ()
setShown shown =
    IO.sequence
        [ IO.when (not (showsStdOut shown)) <| \() -> Terminal.clearStdOut
        , IO.putLens lensShown shown
        ]


modifyShown : (Shown -> Shown) -> IO a h ()
modifyShown f =
    IO.bind (IO.getLens lensShown) <|
        \shown ->
            setShown (f shown)


ifReplNotShown : IO a h () -> IO a h ()
ifReplNotShown io =
    IO.bind (IO.getLens lensShown) <|
        \shown ->
            if showsRepl shown then
                IO.sequence
                    [ IO.modifyLens lensShown (setReplFlashed True)
                    , IO.sleep 100
                    , IO.modifyLens lensShown (setReplFlashed False)
                    ]

            else
                io


showsRepl : Shown -> Bool
showsRepl shown =
    case shown of
        ShowFile _ _ ->
            False

        ShowRepl _ _ _ ->
            True

        ShowError _ ->
            False

        ShowNothing ->
            False

        ShowFileAndRepl _ _ _ _ _ _ ->
            True


setShownReplInput : Maybe Terminal.Repl.InterpreterInput -> IO a h ()
setShownReplInput maybeInterpreterInput =
    IO.modifyLens lensShown <|
        \shown ->
            case shown of
                ShowRepl flashed openedModule _ ->
                    ShowRepl flashed openedModule maybeInterpreterInput

                ShowFileAndRepl filePath contents openedModule flashed tag _ ->
                    ShowFileAndRepl filePath contents openedModule flashed tag maybeInterpreterInput

                _ ->
                    shown


showsStdOut : Shown -> Bool
showsStdOut shown =
    case shown of
        ShowFile _ _ ->
            False

        ShowRepl _ _ _ ->
            True

        ShowError _ ->
            False

        ShowNothing ->
            True

        ShowFileAndRepl _ _ _ _ _ _ ->
            True


showFile : FileName -> IO a h ()
showFile fileName =
    ifReplNotShown <|
        IO.bind (IO.liftA2 Tuple.pair (toPath fileName) (IO.getLens lensShown)) <|
            \( filePath, shown ) ->
                case shown of
                    ShowFile shownPath _ ->
                        if filePath == shownPath then
                            hideShown

                        else
                            showFileContents filePath (initialModeFor filePath)

                    _ ->
                        showFileContents filePath (initialModeFor filePath)


toggleShowMode : IO a h ()
toggleShowMode =
    IO.bind (IO.getLens lensShown) <|
        \shown ->
            case shown of
                ShowFile filePath (TextContents changed _) ->
                    IO.sequence
                        [ IO.when changed (\() -> saveFile)
                        , showFileContents filePath (specialModeFor filePath)
                        ]

                ShowFile filePath _ ->
                    showFileContents filePath AsText

                _ ->
                    IO.return ()


showFileContents : FilePath -> ShowMode -> IO a h ()
showFileContents filePath mode =
    IO.bind (getFileContents filePath mode) <|
        \contentResult ->
            case contentResult of
                Right contents ->
                    setShown <| ShowFile filePath contents

                Left error ->
                    showError (Exit.reactorToReport error)


getFileContents : FilePath -> ShowMode -> IO a h (Either Exit.Reactor FileContents)
getFileContents filePath mode =
    case mode of
        AsText ->
            File.readUtf8 filePath |> IO.fmap (TextContents False >> Right)

        AsHex ->
            SysFile.readFile filePath
                |> IO.fmap (Maybe.map bytesToHex)
                |> IO.fmap (Maybe.withDefault errorHex >> Right)

        AsElm ->
            getExecutableJavaScript filePath
                |> IO.fmap (Either.fmap ElmContents)


setFileContents : String -> IO a h ()
setFileContents contents =
    IO.modifyLens lensShown <|
        \shown ->
            case shown of
                ShowFile filePath (TextContents _ _) ->
                    ShowFile filePath (TextContents True contents)

                _ ->
                    shown


initialModeFor : FilePath -> ShowMode
initialModeFor filePath =
    if isEditable filePath then
        AsText

    else
        AsHex


specialModeFor : FilePath -> ShowMode
specialModeFor filePath =
    if isExecutable filePath then
        AsElm

    else
        AsHex


isEditable : FilePath -> Bool
isEditable filePath =
    Set.member (getExtension filePath) extensionsOfEditableFile


extensionsOfEditableFile : Set.Set String
extensionsOfEditableFile =
    Set.fromList [ "elm", "js", "json", "md", "txt" ]


isExecutable : FilePath -> Bool
isExecutable filePath =
    Set.member (getExtension filePath) extensionsOfExecutableFile


extensionsOfExecutableFile : Set.Set String
extensionsOfExecutableFile =
    Set.fromList [ "elm", "js" ]


showError : Report -> IO a h ()
showError report =
    setShown <| ShowError (Exit.toClient report)



-- JAVASCRIPT


getExecutableJavaScript : FilePath -> IO a h (Either Exit.Reactor String)
getExecutableJavaScript filePath =
    IO.rmap (getJavaScript filePath) <|
        Either.fmap <|
            \javaScript -> javaScript ++ startElmCommand idRecord.elmResultId filePath


getJavaScript : FilePath -> IO a h (Either Exit.Reactor String)
getJavaScript filePath =
    if getExtension filePath == "elm" then
        elmReactor filePath

    else
        IO.fmap Right <| File.readUtf8 filePath


startElmCommand : String -> FilePath -> String
startElmCommand elmResultId filePath =
    """
this.Elm.""" ++ toModuleMame filePath ++ """.init({
    node: document.getElementById('""" ++ elmResultId ++ """')
});
"""


toModuleMame : FilePath -> String
toModuleMame filePath =
    filePath
        |> SysFile.toString
        |> trimUntil "src/"
        |> String.replace ".elm" ""
        |> trimUntil "build/"
        |> String.replace ".js" ""
        |> String.replace "/" "."


trimUntil : String -> String -> String
trimUntil pattern string =
    case String.indexes pattern string of
        [] ->
            string

        index :: _ ->
            String.dropLeft (index + String.length pattern) string


valueOutputCode : String -> String
valueOutputCode replResultEvent =
    """
window.requestAnimationFrame(() =>
    this.dispatchEvent(new CustomEvent('""" ++ replResultEvent ++ """', {
        bubbles: true,
        cancelable: true,
        detail: { result: _result, force_quit_: force_quit_ }
    }))
);
"""


htmlOutputCode : Name.Name -> String
htmlOutputCode moduleName =
    """
this.Elm.""" ++ moduleName ++ """.init({
    node: this.parentNode.previousSibling.firstElementChild
});
this.removeAttribute('code');
"""



-- REGISTRY


loadRegistry : IO a h ()
loadRegistry =
    IO.bind Http.getManager <|
        \manager ->
            IO.bind Stuff.getPackageCache <|
                \cache ->
                    IO.bind (Registry.read cache) <|
                        \maybeRegistry ->
                            case maybeRegistry of
                                Nothing ->
                                    fetchRegistry manager cache

                                Just registry ->
                                    updateRegistry manager cache registry


fetchRegistry : Http.Manager -> Stuff.PackageCache -> IO a h ()
fetchRegistry manager cache =
    IO.sequence
        [ Terminal.clearPutLine "fetching registry"
        , IO.bind (Registry.fetch manager cache)
            showRegistryFetchResult
        ]


showRegistryFetchResult : Either Exit.RegistryProblem Registry.Registry -> IO a h ()
showRegistryFetchResult fetchResult =
    case fetchResult of
        Right (Registry.Registry count packages) ->
            Terminal.putLine <|
                ("fetched " ++ String.fromInt (Map.size packages) ++ " package names")
                    ++ (" / " ++ String.fromInt count ++ " versions")

        Left error ->
            showError <| Exit.toRegistryProblemReport "Error" error "Fetch"


updateRegistry : Http.Manager -> Stuff.PackageCache -> Registry.Registry -> IO a h ()
updateRegistry manager cache oldRegistry =
    IO.sequence
        [ Terminal.clearPutLine "updating registry"
        , IO.bind (Registry.update manager cache oldRegistry)
            (showRegistryUpdateResult oldRegistry)
        ]


showRegistryUpdateResult : Registry.Registry -> Either Exit.RegistryProblem Registry.Registry -> IO a h ()
showRegistryUpdateResult oldRegistry updateResult =
    case ( oldRegistry, updateResult ) of
        ( Registry.Registry oldCount oldPackages, Right (Registry.Registry count packages) ) ->
            Terminal.putLine <|
                ("updated " ++ String.fromInt (Map.size oldPackages) ++ " package names")
                    ++ (" / " ++ String.fromInt oldCount ++ " versions")
                    ++ (" with " ++ String.fromInt (Map.size packages - Map.size oldPackages) ++ " new names")
                    ++ (" / " ++ String.fromInt (count - oldCount) ++ " new versions")

        ( _, Left error ) ->
            showError <| Exit.toRegistryProblemReport "Error" error "Update"



-- STUFF


withRoot : (FilePath -> IO a h ()) -> IO a h ()
withRoot callback =
    IO.bind Stuff.findRoot <|
        \maybeRoot ->
            case maybeRoot of
                Nothing ->
                    showError <| Exit.makeToReport Exit.MakeNoOutline

                Just root ->
                    callback root



-- OUTLINE


parseOutline : IO a h ()
parseOutline =
    withRoot <|
        \root ->
            IO.sequence
                [ Terminal.clearPutLine "reading outline"
                , IO.bind (Outline.read root) showOutlineResult
                ]


showOutlineResult : Either Exit.Outline Outline.Outline -> IO a h ()
showOutlineResult readresult =
    case readresult of
        Right outline ->
            Terminal.putLine <| Debug.toString outline

        Left error ->
            showError <| Exit.toDetailsReport <| Exit.DetailsBadOutline error



-- SOLVER


validateOutline : IO a h ()
validateOutline =
    withRoot <|
        \root ->
            IO.sequence
                [ Terminal.clearPutLine "validating outline"
                , IO.bind (Solver.validate root) showValidateResult
                ]


showValidateResult : Either Exit.Details Details.ValidOutline -> IO a h ()
showValidateResult verifyResult =
    case verifyResult of
        Right validOutline ->
            Terminal.putLine <| Debug.toString validOutline

        Left error ->
            showError <| Exit.toDetailsReport error



-- DETAILS


loadDetails : IO a h ()
loadDetails =
    withRoot <|
        \root ->
            IO.sequence
                [ Terminal.clearPutLine "loading details"
                , IO.bind (Details.load root) showDetailsResult
                , showCommandDuration "loading details"
                ]


showDetailsResult : Either Exit.Details Details.Details -> IO a h ()
showDetailsResult loadresult =
    case loadresult of
        Right (Details.Details _ _ _ locals foreigns _) ->
            Terminal.putLine <|
                ("loaded " ++ String.fromInt (Map.size locals) ++ " locals")
                    ++ (", " ++ String.fromInt (Map.size foreigns) ++ " foreigns")

        Left error ->
            showError <| Exit.toDetailsReport error



-- BUILD


buildFromPaths : String -> IO a h ()
buildFromPaths string =
    withRoot <|
        \root ->
            IO.sequence
                [ Terminal.clearPutLine "building"
                , IO.bind (Details.load root) <|
                    \eitherDetails ->
                        case eitherDetails of
                            Left _ ->
                                showDetailsResult eitherDetails

                            Right details ->
                                IO.bind (toPath string) <|
                                    \path ->
                                        IO.bind
                                            (Build.fromPaths root details (NE.CList path []))
                                            showBuildResult
                ]


showBuildResult : Either Exit.BuildProblem Build.Artifacts -> IO a h ()
showBuildResult buildResult =
    case buildResult of
        Right (Build.Artifacts _ dependencies roots modules) ->
            Terminal.putLine <|
                ("built " ++ String.fromInt (MList.length <| NE.toList roots) ++ " roots")
                    ++ (", " ++ String.fromInt (MList.length modules) ++ " modules")
                    ++ (", " ++ String.fromInt (Map.size dependencies) ++ " dependencies")

        Left error ->
            showError <| Exit.toBuildProblemReport error



-- ELM


elmMain : IO a h ()
elmMain =
    Terminal.Main.runMain


elmInit : IO a h ()
elmInit =
    IO.bind Terminal.Init.run <|
        \result ->
            case result of
                Right () ->
                    IO.noOp

                Left error ->
                    showError <| Exit.initToReport error


elmInstall : String -> IO a h ()
elmInstall packageName =
    case Helpers.parsePackage packageName of
        Nothing ->
            Terminal.putLine "illegal package name"

        Just pkg ->
            withRoot <|
                \root ->
                    IO.bind (createBreakpointPackageIfNecessary packageName pkg) <|
                        \() ->
                            IO.bind (Terminal.Install.install root pkg) <|
                                \result ->
                                    case result of
                                        Right () ->
                                            showCommandDuration "elm install"

                                        Left error ->
                                            showError <| Exit.installToReport error


elmMake : String -> String -> IO a h ()
elmMake mode string =
    IO.bind (toPath string) <|
        \inputPath ->
            let
                ( debugFlag, optimizeFlag, asyncFlag ) =
                    case mode of
                        "d" ->
                            ( True, False, False )

                        "o" ->
                            ( False, True, False )

                        "a" ->
                            ( False, False, True )

                        _ ->
                            ( False, False, False )

                outputPath : FilePath
                outputPath =
                    SysFile.toString inputPath
                        |> String.replace "src/" "build/"
                        |> String.replace ".elm" ".js"
                        |> SysFile.fromString
            in
            withRoot <|
                \root ->
                    IO.bind (Terminal.Make.run root [ inputPath ] debugFlag optimizeFlag asyncFlag outputPath) <|
                        \result ->
                            case result of
                                Right () ->
                                    IO.noOp

                                Left error ->
                                    showError <| Exit.makeToReport error


elmReactor : FilePath -> IO a h (Either Exit.Reactor String)
elmReactor filePath =
    Terminal.Reactor.compile filePath



-- REPL


elmRepl : Terminal.Repl.Mode -> Maybe String -> Bool -> IO a h ()
elmRepl mode maybeTag htmlEnabled =
    IO.sequence
        [ modifyShown (addRepl mode maybeTag)
        , IO.bind (Terminal.Repl.run (Terminal.Repl.Flags interpreter mode htmlEnabled)) <|
            \result ->
                case result of
                    Right () ->
                        IO.sequence
                            [ clearIO
                            , modifyShown closeRepl
                            ]

                    Left error ->
                        showError <| Exit.replToReport error
        ]


addRepl : Terminal.Repl.Mode -> Maybe String -> Shown -> Shown
addRepl mode maybeTag shown =
    case ( mode, shown ) of
        ( Terminal.Repl.Breakpoint moduleName _ _, ShowFile filePath (ElmContents contents) ) ->
            ShowFileAndRepl filePath contents moduleName False maybeTag Nothing

        ( Terminal.Repl.Module moduleName, _ ) ->
            ShowRepl False (Just moduleName) Nothing

        _ ->
            ShowRepl False Nothing Nothing


setReplFlashed : Bool -> Shown -> Shown
setReplFlashed flashed shown =
    case shown of
        ShowRepl _ openedModule maybeInterpreterInput ->
            ShowRepl flashed openedModule maybeInterpreterInput

        ShowFileAndRepl filePath contents openedModule _ tag maybeInterpreterInput ->
            ShowFileAndRepl filePath contents openedModule flashed tag maybeInterpreterInput

        _ ->
            shown


closeRepl : Shown -> Shown
closeRepl shown =
    case shown of
        ShowFileAndRepl filePath contents _ _ _ _ ->
            ShowFile filePath (ElmContents contents)

        _ ->
            ShowNothing


interpreter : Terminal.Repl.Interpreter a (LocalState a h) h
interpreter input =
    IO.bindSequence
        [ IO.when (isHtmlInput input) (\() -> Terminal.putLine "")
        , setShownReplInput (Just input)
        , IO.sleep 10
        , IO.when (isHtmlInput input) (\() -> setShownReplInput Nothing)
        , jumpToBottom
        ]
        (IO.return Terminal.Repl.InterpreterSuccess)


isHtmlInput : Terminal.Repl.InterpreterInput -> Bool
isHtmlInput input =
    case input of
        Terminal.Repl.InterpretHtml _ _ ->
            True

        _ ->
            False


handleOutput : Json.Decode.Decoder (IO a h ())
handleOutput =
    Json.Decode.succeed
        (\output forceQuit ->
            if forceQuit then
                Terminal.setNextInput ":force_quit_"

            else
                IO.sequence
                    [ setShownReplInput Nothing
                    , Terminal.putLine output
                    ]
        )
        |> jsonAndMap (Json.Decode.at [ "detail", "result" ] Json.Decode.string)
        |> jsonAndMap (Json.Decode.at [ "detail", "force_quit_" ] Json.Decode.bool)


jumpToBottom : IO a h ()
jumpToBottom =
    Dom.getViewportOf idRecord.replItemsId
        |> Task.andThen (\info -> Dom.setViewportOf idRecord.replItemsId 0 info.scene.height)
        |> Task.attempt (\_ -> IO.noOp)
        |> IO.liftCmdIO



-- BREAKPOINTS


createBreakpointPackageIfNecessary : String -> Pkg.Name -> IO a h ()
createBreakpointPackageIfNecessary name pkg =
    if name == "elm/breakpoint" then
        createBreakpointPackage pkg

    else
        IO.noOp


createBreakpointPackage : Pkg.Name -> IO a h ()
createBreakpointPackage pkg =
    IO.bind Stuff.getPackageCache <|
        \cache ->
            let
                packagePath : FilePath
                packagePath =
                    Stuff.package cache pkg V.one

                srcPath =
                    SysFile.addName packagePath "src"

                kernelPath =
                    SysFile.addNames srcPath [ "Elm", "Kernel" ]
            in
            IO.sequence
                [ SysFile.createDirectoryIfMissing True kernelPath
                , File.writeUtf8 (SysFile.addName packagePath "elm.json") breakpointOutline
                , File.writeUtf8 (SysFile.addName srcPath "Breakpoint.elm") breakpointModule
                , File.writeUtf8
                    (SysFile.addName kernelPath "Breakpoint.js")
                    (breakpointKernel idRecord.breakpointSuspendedEvent)
                ]


breakpointOutline : String
breakpointOutline =
    """{
\t"type": "package",
\t"name": "elm/breakpoint",
\t"summary": "Experimental Breakpoint Support",
\t"license": "BSD-3-Clause",
\t"version": "1.0.0",
\t"exposed-modules": [
\t\t"Breakpoint"
\t],
\t"elm-version": "0.19.0 <= v < 0.20.0",
\t"dependencies": {
\t\t"elm/core": "1.0.0 <= v < 2.0.0"
\t},
\t"test-dependencies": {}
}"""


breakpointModule : String
breakpointModule =
    """module Breakpoint exposing
    ( Breakpoint
    , Suspend
    , arg
    , initRepl
    , isSuspended
    , name
    , new
    , resume
    , resumeWith
    , tag
    , unwrap
    )


import Basics exposing (..)
import Elm.Kernel.Breakpoint
import Maybe exposing (Maybe(..))
import String exposing (String)



type Breakpoint a f
    = Breakpoint String String (Suspend a -> f) (State a)


type alias Suspend a =
    String -> a -> a


type State a
    = Inactive
    | Suspended String a () (a -> a)


new : (Suspend a -> f) -> Breakpoint a f
new f =
    Breakpoint "" "" f Inactive


unwrap : Breakpoint a f -> f
unwrap ((Breakpoint _ _ f _) as breakpoint) =
    f (kernelSuspend breakpoint)


isSuspended : Breakpoint a f -> Bool
isSuspended (Breakpoint _ _ _ s) =
    s /= Inactive


name : Breakpoint a f -> String
name (Breakpoint _ n _ _) =
    n


tag : Breakpoint a f -> Maybe String
tag (Breakpoint _ _ _ s) =
    case s of
        Inactive ->
            Nothing

        Suspended t _ _ _ ->
            Just t


arg : Breakpoint a f -> Maybe a
arg (Breakpoint _ _ _ s) =
    case s of
        Inactive ->
            Nothing

        Suspended _ a _ _ ->
            Just a


resume : Breakpoint a f -> ()
resume bp =
    kernelResume bp Nothing


resumeWith : Breakpoint a f -> a -> ()
resumeWith bp a =
    kernelResume bp (Just a)


kernelSuspend : Breakpoint a f -> Suspend a
kernelSuspend =
    Elm.Kernel.Breakpoint.suspend


kernelResume : Breakpoint a f -> Maybe a -> ()
kernelResume =
    Elm.Kernel.Breakpoint.resume


initRepl : String -> Breakpoint a f -> Breakpoint a f
initRepl =
    Elm.Kernel.Breakpoint.initRepl
"""


breakpointKernel : String -> String
breakpointKernel suspendEvent =
    """/*

import Elm.Kernel.Debug exposing (log)
import Elm.Kernel.Utils exposing (Tuple0)
import Maybe exposing (Just, Nothing, withDefault)

*/


// BREAKPOINT

var _Breakpoint_state = {
\tcurrent__ASYNC: /* __Maybe_Nothing */ { $: 'Nothing' },
\tresume: _Breakpoint_resume_local
}

function _Breakpoint_get_state() {
\treturn _Breakpoint_state;
}

var _Breakpoint_Inactive = { $: 'Inactive' };

function _Breakpoint_Suspended(a, b, c, d) {
\treturn { $: 'Suspended', a: a, b: b, c: c, d: d };
}

var _Breakpoint_named = F3(function (module, name, bp) {
\tif (module !== '') bp.a = module;
\tif (name !== '') bp.b = name;
\treturn bp;
});

function _Breakpoint_suspend_sync(_bp, tag, a) {
\treturn A2(_Debug_log, tag, a);
}

async function _Breakpoint_suspend_async(bp, tag, a) {
\tvar s = bp.d;
\tif (s.$ !== 'Inactive') {
\t\tconsole.log('tried to suspend an already suspended breakpoint');
\t\treturn s.c;
\t} else if (_Breakpoint_state.current.$ !== 'Nothing') {
\t\tconsole.log('suspend while another breakpoint is suspended');
\t\treturn _Breakpoint_suspend_sync(bp, tag, a);
\t} else {
\t\tvar resume;
\t\tvar promise = new Promise(r => resume = r);
\t\tbp.d = _Breakpoint_Suspended(tag, a, promise, resume);
\t\t_Breakpoint_state.current = __Maybe_Just(bp);
\t\twindow.requestAnimationFrame(() =>
\t\t\tscope.dispatchEvent(new CustomEvent('""" ++ suspendEvent ++ """', {
\t\t\t\tbubbles: true,
\t\t\t\tcancelable: true,
\t\t\t\tdetail: { module: bp.a, name: bp.b, tag: tag }
\t\t\t}))
\t\t);
\t\treturn promise;
\t}
}

var _Breakpoint_suspend = F3(function (bp, tag, a) {
\tif ('current' in _Breakpoint_state) {
\t\treturn _Breakpoint_suspend_async(bp, tag, a);
\t} else {
\t\treturn _Breakpoint_suspend_sync(bp, tag, a);
\t}
});

function _Breakpoint_resume_local(bp, ma) {
\tvar s = bp.d;
\tif (s.$ === 'Inactive') {
\t\tconsole.log('tried to resume inactive breakpoint');
\t} else {
\t\tif (_Breakpoint_state.current.$ === 'Nothing') {
\t\t\tconsole.log('unexpected suspended breakpoint');
\t\t} else {
\t\t\t_Breakpoint_state.current = __Maybe_Nothing;
\t\t}
\t\tbp.d = _Breakpoint_Inactive;
\t\ts.d(A2(__Maybe_withDefault, s.b, ma));
\t}
\treturn __Utils_Tuple0;
}

var _Breakpoint_resume = F2(function (bp, ma) {
\treturn _Breakpoint_state.resume(bp, ma);
});

var _Breakpoint_initRepl = F2(function (id, _bp) {
\tvar node = document.getElementById(id);
\tif (node === null) {
\t\tconsole.log('could not find node with id ' + id);
\t\treturn _bp;
\t}
\tvar elm = node['Elm'];
\tif (elm === undefined) {
\t\tconsole.log('node doesn\\'t have an Elm property');
\t\treturn _bp;
\t}
\tvar get_state = elm['get_state'];
\tif (get_state === undefined) {
\t\tconsole.log('node doesn\\'t have an Elm.get_state property');
\t\treturn _bp;
\t}
\tvar remote_state = get_state();
\t_Breakpoint_state.resume = remote_state.resume;
\treturn A2(__Maybe_withDefault, _bp, remote_state.current);
});


// ASYNC PATCHES

async function _Platform_initialize_async(flagDecoder, args, init, update, subscriptions, stepperBuilder) {
\tvar result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
\t$elm$core$Result$isOk(result) || _Debug_crash(2 /**/, _Json_errorToString(result.a) /**/);
\tvar managers = {};
\tvar initPair = await init(result.a);
\tvar model = initPair.a;
\tvar stepper = stepperBuilder(sendToApp, model);
\tvar ports = _Platform_setupEffects(managers, sendToApp);

\tasync function sendToApp(msg, viewMetadata) {
\t\tvar pair = await A2(update, msg, model);
\t\tstepper(model = pair.a, viewMetadata);
\t\t_Platform_enqueueEffects(managers, pair.b, subscriptions(model));
\t}

\t_Platform_enqueueEffects(managers, initPair.b, subscriptions(model));

\treturn ports ? { ports: ports } : {};
}

var _VirtualDom_init_async = F4(function (virtualNode, flagDecoder, debugMetadata, args) {
\t// NOTE: this function needs _Platform_export available to work

\t/**_UNUSED/
\tvar node = args['node'];
\t//*/
\t/**/
\tvar node = args && args['node'] ? args['node'] : _Debug_crash(0);
\t//*/

\tnode.parentNode.replaceChild(
\t\t_VirtualDom_render_async(virtualNode, function () { }),
\t\tnode
\t);

\treturn {};
});

function _VirtualDom_render_async(vNode, eventNode) {
\tvar tag = vNode.$;

\tif (tag === 5) {
\t\treturn _VirtualDom_render_async(vNode.k || (vNode.k = vNode.m()), eventNode);
\t}

\tif (tag === 0) {
\t\treturn _VirtualDom_doc.createTextNode(vNode.a);
\t}

\tif (tag === 4) {
\t\tvar subNode = vNode.k;
\t\tvar tagger = vNode.j;

\t\twhile (subNode.$ === 4) {
\t\t\ttypeof tagger !== 'object'
\t\t\t\t? tagger = [tagger, subNode.j]
\t\t\t\t: tagger.push(subNode.j);

\t\t\tsubNode = subNode.k;
\t\t}

\t\tvar subEventRoot = { j: tagger, p: eventNode };
\t\tvar domNode = _VirtualDom_render_async(subNode, subEventRoot);
\t\tdomNode.elm_event_node_ref = subEventRoot;
\t\treturn domNode;
\t}

\tif (tag === 3) {
\t\tvar domNode = vNode.h(vNode.g);
\t\t_VirtualDom_applyFacts_async(domNode, eventNode, vNode.d);
\t\treturn domNode;
\t}

\t// at this point `tag` must be 1 or 2

\tvar domNode = vNode.f
\t\t? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
\t\t: _VirtualDom_doc.createElement(vNode.c);

\tif (_VirtualDom_divertHrefToApp && vNode.c == 'a') {
\t\tdomNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
\t}

\t_VirtualDom_applyFacts_async(domNode, eventNode, vNode.d);

\tfor (var kids = vNode.e, i = 0; i < kids.length; i++) {
\t\t_VirtualDom_appendChild(domNode, _VirtualDom_render_async(tag === 1 ? kids[i] : kids[i].b, eventNode));
\t}

\treturn domNode;
}

function _VirtualDom_applyFacts_async(domNode, eventNode, facts) {
\tfor (var key in facts) {
\t\tvar value = facts[key];

\t\tkey === 'a1'
\t\t\t? _VirtualDom_applyStyles(domNode, value)
\t\t\t:
\t\t\tkey === 'a0'
\t\t\t\t? _VirtualDom_applyEvents_async(domNode, eventNode, value)
\t\t\t\t:
\t\t\t\tkey === 'a3'
\t\t\t\t\t? _VirtualDom_applyAttrs(domNode, value)
\t\t\t\t\t:
\t\t\t\t\tkey === 'a4'
\t\t\t\t\t\t? _VirtualDom_applyAttrsNS(domNode, value)
\t\t\t\t\t\t:
\t\t\t\t\t\t((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
\t}
}

function _VirtualDom_applyEvents_async(domNode, eventNode, events) {
\tvar allCallbacks = domNode.elmFs || (domNode.elmFs = {});

\tfor (var key in events) {
\t\tvar newHandler = events[key];
\t\tvar oldCallback = allCallbacks[key];

\t\tif (!newHandler) {
\t\t\tdomNode.removeEventListener(key, oldCallback);
\t\t\tallCallbacks[key] = undefined;
\t\t\tcontinue;
\t\t}

\t\tif (oldCallback) {
\t\t\tvar oldHandler = oldCallback.q;
\t\t\tif (oldHandler.$ === newHandler.$) {
\t\t\t\toldCallback.q = newHandler;
\t\t\t\tcontinue;
\t\t\t}
\t\t\tdomNode.removeEventListener(key, oldCallback);
\t\t}

\t\toldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
\t\tdomNode.addEventListener(key, oldCallback,
\t\t\t_VirtualDom_passiveSupported
\t\t\t&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
\t\t);
\t\tallCallbacks[key] = oldCallback;
\t}
}

function _VirtualDom_makeCallback_async(eventNode, initialHandler) {
\tasync function callback(event) {
\t\tvar handler = callback.q;
\t\tvar result = _Json_runHelp(handler.a, event);

\t\tif (!$elm$core$Result$isOk(result)) {
\t\t\treturn;
\t\t}

\t\tvar tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

\t\t// 0 = Normal
\t\t// 1 = MayStopPropagation
\t\t// 2 = MayPreventDefault
\t\t// 3 = Custom

\t\tvar value = result.a;
\t\tvar message = !tag ? value : tag < 3 ? value.a : value.message;
\t\tvar stopPropagation = tag == 1 ? value.b : tag == 3 && value.stopPropagation;
\t\tvar currentEventNode = (
\t\t\tstopPropagation && event.stopPropagation(),
\t\t\t(tag == 2 ? value.b : tag == 3 && value.preventDefault) && event.preventDefault(),
\t\t\teventNode
\t\t);
\t\tvar tagger;
\t\tvar i;
\t\twhile (tagger = currentEventNode.j) {
\t\t\tif (typeof tagger == 'function') {
\t\t\t\tmessage = tagger(message);
\t\t\t}
\t\t\telse {
\t\t\t\tfor (var i = tagger.length; i--;) {
\t\t\t\t\tmessage = tagger[i](message);
\t\t\t\t}
\t\t\t}
\t\t\tcurrentEventNode = currentEventNode.p;
\t\t}
\t\tawait currentEventNode(message, stopPropagation); // stopPropagation implies isSync
\t}

\tcallback.q = initialHandler;

\treturn callback;
}

function _VirtualDom_applyPatches_async(rootDomNode, oldVirtualNode, patches, eventNode) {
\tif (patches.length === 0) {
\t\treturn rootDomNode;
\t}

\t_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
\treturn _VirtualDom_applyPatchesHelp_async(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp_async(rootDomNode, patches) {
\tfor (var i = 0; i < patches.length; i++) {
\t\tvar patch = patches[i];
\t\tvar localDomNode = patch.t
\t\tvar newNode = _VirtualDom_applyPatch_async(localDomNode, patch);
\t\tif (localDomNode === rootDomNode) {
\t\t\trootDomNode = newNode;
\t\t}
\t}
\treturn rootDomNode;
}

function _VirtualDom_applyPatch_async(domNode, patch) {
\tswitch (patch.$) {
\t\tcase 0:
\t\t\treturn _VirtualDom_applyPatchRedraw_async(domNode, patch.s, patch.u);

\t\tcase 4:
\t\t\t_VirtualDom_applyFacts_async(domNode, patch.u, patch.s);
\t\t\treturn domNode;

\t\tcase 3:
\t\t\tdomNode.replaceData(0, domNode.length, patch.s);
\t\t\treturn domNode;

\t\tcase 1:
\t\t\treturn _VirtualDom_applyPatchesHelp_async(domNode, patch.s);

\t\tcase 2:
\t\t\tif (domNode.elm_event_node_ref) {
\t\t\t\tdomNode.elm_event_node_ref.j = patch.s;
\t\t\t}
\t\t\telse {
\t\t\t\tdomNode.elm_event_node_ref = { j: patch.s, p: patch.u };
\t\t\t}
\t\t\treturn domNode;

\t\tcase 6:
\t\t\tvar data = patch.s;
\t\t\tfor (var i = 0; i < data.i; i++) {
\t\t\t\tdomNode.removeChild(domNode.childNodes[data.v]);
\t\t\t}
\t\t\treturn domNode;

\t\tcase 7:
\t\t\tvar data = patch.s;
\t\t\tvar kids = data.e;
\t\t\tvar i = data.v;
\t\t\tvar theEnd = domNode.childNodes[i];
\t\t\tfor (; i < kids.length; i++) {
\t\t\t\tdomNode.insertBefore(_VirtualDom_render_async(kids[i], patch.u), theEnd);
\t\t\t}
\t\t\treturn domNode;

\t\tcase 9:
\t\t\tvar data = patch.s;
\t\t\tif (!data) {
\t\t\t\tdomNode.parentNode.removeChild(domNode);
\t\t\t\treturn domNode;
\t\t\t}
\t\t\tvar entry = data.A;
\t\t\tif (typeof entry.r !== 'undefined') {
\t\t\t\tdomNode.parentNode.removeChild(domNode);
\t\t\t}
\t\t\tentry.s = _VirtualDom_applyPatchesHelp_async(domNode, data.w);
\t\t\treturn domNode;

\t\tcase 8:
\t\t\treturn _VirtualDom_applyPatchReorder_async(domNode, patch);

\t\tcase 5:
\t\t\treturn patch.s(domNode);

\t\tdefault:
\t\t\t_Debug_crash(10); // 'Ran into an unknown patch!'
\t}
}

function _VirtualDom_applyPatchRedraw_async(domNode, vNode, eventNode) {
\tvar parentNode = domNode.parentNode;
\tvar newNode = _VirtualDom_render_async(vNode, eventNode);

\tif (!newNode.elm_event_node_ref) {
\t\tnewNode.elm_event_node_ref = domNode.elm_event_node_ref;
\t}

\tif (parentNode && newNode !== domNode) {
\t\tparentNode.replaceChild(newNode, domNode);
\t}
\treturn newNode;
}

function _VirtualDom_applyPatchReorder_async(domNode, patch) {
\tvar data = patch.s;

\t// remove end inserts
\tvar frag = _VirtualDom_applyPatchReorderEndInsertsHelp_async(data.y, patch);

\t// removals
\tdomNode = _VirtualDom_applyPatchesHelp_async(domNode, data.w);

\t// inserts
\tvar inserts = data.x;
\tfor (var i = 0; i < inserts.length; i++) {
\t\tvar insert = inserts[i];
\t\tvar entry = insert.A;
\t\tvar node = entry.c === 2
\t\t\t? entry.s
\t\t\t: _VirtualDom_render_async(entry.z, patch.u);
\t\tdomNode.insertBefore(node, domNode.childNodes[insert.r]);
\t}

\t// add end inserts
\tif (frag) {
\t\t_VirtualDom_appendChild(domNode, frag);
\t}

\treturn domNode;
}

function _VirtualDom_applyPatchReorderEndInsertsHelp_async(endInserts, patch) {
\tif (!endInserts) {
\t\treturn;
\t}

\tvar frag = _VirtualDom_doc.createDocumentFragment();
\tfor (var i = 0; i < endInserts.length; i++) {
\t\tvar insert = endInserts[i];
\t\tvar entry = insert.A;
\t\t_VirtualDom_appendChild(frag, entry.c === 2
\t\t\t? entry.s
\t\t\t: _VirtualDom_render_async(entry.z, patch.u)
\t\t);
\t}
\treturn frag;
}

var _Browser_element_async = F4(function (impl, flagDecoder, debugMetadata, args) {
\treturn _Platform_initialize_async(
\t\tflagDecoder,
\t\targs,
\t\timpl.init,
\t\timpl.update,
\t\timpl.subscriptions,
\t\tfunction (sendToApp, initialModel) {
\t\t\tvar view = impl.view;
\t\t\t/**_UNUSED/
\t\t\tvar domNode = args['node'];
\t\t\t//*/
\t\t\t/**/
\t\t\tvar domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
\t\t\t//*/
\t\t\tvar currNode = _VirtualDom_virtualize(domNode);

\t\t\treturn _Browser_makeAnimator(initialModel, async function (model) {
\t\t\t\tvar nextNode = await view(model);
\t\t\t\tvar patches = _VirtualDom_diff(currNode, nextNode);
\t\t\t\tdomNode = _VirtualDom_applyPatches_async(domNode, currNode, patches, sendToApp);
\t\t\t\tcurrNode = nextNode;
\t\t\t});
\t\t}
\t);
});

var $elm$browser$Browser$sandbox_async = function (impl) {
\treturn _Browser_element_async(
\t\t{
\t\t\tinit: async function (_v0) {
\t\t\t\treturn _Utils_Tuple2(await impl.init, $elm$core$Platform$Cmd$none);
\t\t\t},
\t\t\tsubscriptions: function (_v1) {
\t\t\t\treturn $elm$core$Platform$Sub$none;
\t\t\t},
\t\t\tupdate: F2(
\t\t\t\tasync function (msg, model) {
\t\t\t\t\treturn _Utils_Tuple2(
\t\t\t\t\t\tawait A2(impl.update, msg, model),
\t\t\t\t\t\t$elm$core$Platform$Cmd$none);
\t\t\t\t}),
\t\t\tview: impl.view
\t\t});
};
"""


handleBreakpoint : Json.Decode.Decoder (IO a h ())
handleBreakpoint =
    Json.Decode.succeed
        (\moduleName bpName tag ->
            IO.sequence
                [ IO.sleep 100
                , Terminal.setNextInput "bpArg"
                , elmRepl
                    (Terminal.Repl.Breakpoint moduleName idRecord.elmCodeId bpName)
                    (Just tag)
                    True
                ]
        )
        |> jsonAndMap (Json.Decode.at [ "detail", "module" ] Json.Decode.string)
        |> jsonAndMap (Json.Decode.at [ "detail", "name" ] Json.Decode.string)
        |> jsonAndMap (Json.Decode.at [ "detail", "tag" ] Json.Decode.string)


jsonAndMap : Applicative.AndMap (Json.Decode.Decoder a) (Json.Decode.Decoder (a -> b)) (Json.Decode.Decoder b)
jsonAndMap =
    Json.Decode.map2 (|>)



-- VIEW HELPER


type alias File =
    { name : FileName
    , size : Int
    , time : String
    , runnable : Bool
    }


getRevCwd : State a h -> TList FileName
getRevCwd =
    SysFile.getCurrentDirectoryNamesPure


getDirsAndFiles : Time.Zone -> State a h -> ( TList File, TList File )
getDirsAndFiles zone state =
    SysFile.getCurrentDirectoryEntriesPure state <|
        \name size time ->
            { name = name
            , size = size
            , time = timeString zone time
            , runnable = isRunnable name
            }


isRunnable : FileName -> Bool
isRunnable fileName =
    String.endsWith ".elm" fileName



-- VIEW


view : State a h -> Browser.Document (IO a h ())
view state =
    let
        revCwd =
            getRevCwd state

        ( dirs, files ) =
            getDirsAndFiles (lensZone.getter state) state
    in
    { title = String.join "/" ("~" :: revCwd)
    , body =
        [ Html.header [ Html.Attributes.class "header" ] []
        , Html.div [ Html.Attributes.class "content" ]
            [ Navigator.view changeCwd "" revCwd
            , viewLeftColumn
                dirs
                files
                (Terminal.lensPrompt.getter state)
                (Terminal.lensInput.getter state)
            , viewRightColumn idRecord (lensShown.getter state) (Terminal.lensStdOut.getter state)
            , Html.div [ Html.Attributes.style "clear" "both" ] []
            ]
        ]
    }


viewLeftColumn : List File -> List File -> String -> String -> Html (IO a h ())
viewLeftColumn dirs files prefill command =
    Html.section [ Html.Attributes.class "left-column" ]
        [ viewCommand prefill command
        , viewFiles dirs files
        ]


viewRightColumn : IdRecord -> Shown -> TList Terminal.Output -> Html (IO a h ())
viewRightColumn ids shown stdOut =
    Html.section [ Html.Attributes.class "right-column" ] <|
        case shown of
            ShowFile filePath contents ->
                [ viewFileContents ids filePath contents (isEditable filePath) ]

            ShowRepl flashed openedModule maybeInterpreterInput ->
                [ viewRepl ids stdOut flashed openedModule Nothing maybeInterpreterInput ]

            ShowFileAndRepl filePath contents openedModule flashed maybeTag maybeInterpreterInput ->
                [ viewFileContents ids filePath (ElmContents contents) (isEditable filePath)
                , viewRepl ids stdOut flashed (Just openedModule) maybeTag maybeInterpreterInput
                ]

            ShowError error ->
                [ viewError error ]

            ShowNothing ->
                if MList.null stdOut then
                    []

                else
                    [ viewStdOut stdOut ]



-- VIEW COMMAND


viewCommand : String -> String -> Html (IO a h ())
viewCommand prefill command =
    Skeleton.box
        { title = "Command"
        , titleClick = Nothing
        , items =
            [ [ Html.form [ Html.Events.onSubmit Terminal.gotInput ]
                    [ Html.span [] [ Html.text prefill ]
                    , Html.input
                        [ Html.Attributes.value command
                        , Html.Events.onInput Terminal.setInput
                        ]
                        []
                    ]
              ]
            ]
        , footerClick = Nothing
        }



-- VIEW FILES


viewFiles : TList File -> TList File -> Html (IO a h ())
viewFiles dirs files =
    Skeleton.box
        { title = "File Navigation"
        , titleClick = Nothing
        , items =
            MList.map viewDir (MList.sortOn .name dirs)
                ++ MList.map viewFile (MList.sortOn .name files)
        , footerClick = Nothing
        }


viewDir : File -> TList (Html (IO a h ()))
viewDir { name, size, time } =
    [ Html.a [ Html.Events.onClick <| pushDirectory name ] [ Icon.folder, Html.text name ]
    , Html.text <| String.fromInt size ++ " " ++ time
    ]


viewFile : File -> TList (Html (IO a h ()))
viewFile { name, size, time } =
    [ Html.a [ Html.Events.onClick (showFile name) ] [ Icon.lookup name, Html.text name ]
    , Html.text <| String.fromInt size ++ " " ++ time
    ]



-- VIEW ERROR


viewError : Error.Error -> Html (IO a h ())
viewError error =
    Skeleton.styledBox
        { title = "Error"
        , titleClick = Just hideShown
        , titleFlashed = False
        , itemsStyle =
            [ Html.Attributes.style "background-color" "black"
            , Html.Attributes.style "border-radius" "0 0 5px 5px"
            ]
        , items = [ [ Errors.viewError error ] ]
        , footerClick = Nothing
        }



-- VIEW STDOUT


viewStdOut : TList Terminal.Output -> Html (IO a h ())
viewStdOut stdOut =
    Skeleton.box
        { title = "Output"
        , titleClick = Nothing
        , items = MList.map viewOutput (MList.reverse stdOut)
        , footerClick = Nothing
        }


viewOutput : Terminal.Output -> List (Html (IO a h ()))
viewOutput output =
    [ Html.div
        [ Html.Attributes.style "display" "inline-block"
        , Html.Attributes.style "font-family" "Source Code Pro"
        , Html.Attributes.style "font-size" "smaller"
        , Html.Attributes.style "white-space" "pre-wrap"
        ]
        [ Html.text (Terminal.getText output) ]
    ]



-- VIEW REPL


viewRepl : IdRecord -> TList Terminal.Output -> Bool -> Maybe String -> Maybe String -> Maybe Terminal.Repl.InterpreterInput -> Html (IO a h ())
viewRepl ids stdOut flashed openedModule maybeTag maybeInterpreterInput =
    Skeleton.styledBox
        { title = replTitle openedModule maybeTag
        , titleClick = Just (Terminal.setCurrentInput ":quit")
        , titleFlashed = flashed
        , itemsStyle = [ Html.Attributes.id ids.replItemsId ]
        , items = MList.reverse (viewInterpreterInput ids maybeInterpreterInput ++ MList.map viewOutput stdOut)
        , footerClick = Nothing
        }


replTitle : Maybe String -> Maybe String -> String
replTitle maybeModuleName maybeTag =
    case ( maybeModuleName, maybeTag ) of
        ( Nothing, Nothing ) ->
            "Repl"

        ( Just moduleName, Nothing ) ->
            "Repl in " ++ moduleName

        ( Nothing, Just tag ) ->
            "Breakpoint: " ++ tag

        ( Just moduleName, Just tag ) ->
            "Breakpoint in " ++ moduleName ++ ": " ++ tag


viewInterpreterInput : IdRecord -> Maybe Terminal.Repl.InterpreterInput -> TList (TList (Html (IO a h ())))
viewInterpreterInput ids maybeJavaScript =
    case maybeJavaScript of
        Nothing ->
            []

        Just (Terminal.Repl.InterpretValue javaScript) ->
            [ [ Html.node "elm-code"
                    [ Html.Attributes.attribute "code"
                        (javaScript ++ valueOutputCode ids.replResultEvent)
                    , Html.Events.on ids.replResultEvent handleOutput
                    ]
                    []
              ]
            ]

        Just (Terminal.Repl.InterpretHtml moduleName javaScript) ->
            [ [ Html.node "elm-code"
                    [ Html.Attributes.attribute "code"
                        (javaScript ++ htmlOutputCode moduleName)
                    ]
                    []
              ]
            ]

        Just (Terminal.Repl.ShowError error) ->
            [ [ Errors.viewError (Exit.toClient (Exit.replToReport error)) ] ]



-- VIEW FILE CONTENTS


viewFileContents : IdRecord -> FilePath -> FileContents -> Bool -> Html (IO a h ())
viewFileContents ids filePath contents editable =
    let
        ( _, fileName ) =
            SysFile.splitLastName filePath

        titleClick =
            if editable then
                Just toggleShowMode

            else
                Nothing
    in
    case contents of
        TextContents _ text ->
            Skeleton.box
                { title = fileName
                , titleClick = titleClick
                , items =
                    [ [ Html.textarea
                            [ Html.Attributes.class "edit-area"
                            , Html.Events.onInput setFileContents
                            ]
                            [ Html.text text ]
                      ]
                    ]
                , footerClick = Just saveFile
                }

        HexContents addresses hexes chars ->
            Skeleton.box
                { title = fileName
                , titleClick = titleClick
                , items =
                    [ [ Html.div [ Html.Attributes.class "hex-output" ]
                            [ Html.pre [] [ Html.text addresses ]
                            , Html.pre [] [ Html.text hexes ]
                            , Html.pre [] [ Html.text chars ]
                            ]
                      ]
                    ]
                , footerClick = Nothing
                }

        ElmContents code ->
            Skeleton.box
                { title = fileName
                , titleClick = titleClick
                , items =
                    [ [ Html.node "elm-code"
                            [ Html.Attributes.attribute "code" code
                            , Html.Attributes.id ids.elmCodeId
                            , Html.Events.on ids.breakpointSuspendedEvent handleBreakpoint
                            ]
                            [ Html.div [ Html.Attributes.id ids.elmResultId ] [] ]
                      ]
                    ]
                , footerClick = Nothing
                }



-- HEX VIEW


bytesToHex : Bytes.Bytes -> FileContents
bytesToHex bytes =
    let
        width =
            Bytes.width bytes

        lines =
            width // 16

        rest =
            width - lines * 16
    in
    if width > 0 then
        BD.decode (bytesToHexDecoder lines rest) bytes
            |> Maybe.withDefault errorHex

    else
        emptyHex


emptyHex : FileContents
emptyHex =
    HexContents "(empty)" "" ""


errorHex : FileContents
errorHex =
    HexContents "(error)" "" ""


bytesToHexDecoder : Int -> Int -> BD.Decoder FileContents
bytesToHexDecoder lines rest =
    BD.succeed linesAndRestToContents
        |> andMap (nTimes lines lineDecoder)
        |> andMap (restDecoder rest)


lineDecoder : BD.Decoder ( String, String )
lineDecoder =
    nTimes 16 BD.unsignedInt8
        |> BD.map wordsToLine


wordsToLine : List Int -> ( String, String )
wordsToLine words =
    ( wordsToHex words, wordsToChars words )


restDecoder : Int -> BD.Decoder (List ( String, String ))
restDecoder rest =
    if rest > 0 then
        nTimes rest BD.unsignedInt8
            |> BD.map (wordsToRest >> List.singleton)

    else
        BD.succeed []


wordsToRest : List Int -> ( String, String )
wordsToRest words =
    ( String.padRight 39 ' ' (wordsToHex words), wordsToChars words )


wordsToHex : List Int -> String
wordsToHex words =
    List.map wordToHex words |> String.join "" |> hexToGroups


wordToHex : Int -> String
wordToHex word =
    nibble (word // 16) ++ nibble (modBy 16 word)


nibble : Int -> String
nibble n =
    String.fromChar <|
        Char.fromCode <|
            if n > 9 then
                Char.toCode 'A' + n - 10

            else
                Char.toCode '0' + n


hexToGroups : String -> String
hexToGroups str =
    String.slice 0 4 str
        ++ (" " ++ String.slice 4 8 str)
        ++ (" " ++ String.slice 8 12 str)
        ++ (" " ++ String.slice 12 16 str)
        ++ (" " ++ String.slice 16 20 str)
        ++ (" " ++ String.slice 20 24 str)
        ++ (" " ++ String.slice 24 28 str)
        ++ (" " ++ String.slice 28 32 str)


wordsToChars : List Int -> String
wordsToChars words =
    words |> List.map wordToChar |> String.fromList


wordToChar : Int -> Char
wordToChar word =
    if word < 32 || word > 127 then
        '.'

    else
        Char.fromCode word


linesAndRestToContents : List ( String, String ) -> List ( String, String ) -> FileContents
linesAndRestToContents lines rest =
    let
        ( hexes, chars ) =
            MList.unzip (lines ++ rest)

        addresses =
            MList.range 0 (MList.length hexes - 1)
                |> MList.map lineNumberToAddress
    in
    HexContents
        (String.join "\n" addresses)
        (String.join "\n" hexes)
        (String.join "\n" chars)


lineNumberToAddress : Int -> String
lineNumberToAddress lineNumber =
    ""
        ++ wordToHex (modBy 0x0100 (lineNumber // 0x00100000))
        ++ wordToHex (modBy 0x0100 (lineNumber // 0x1000))
        ++ wordToHex (modBy 0x0100 (lineNumber // 0x10))
        ++ wordToHex (modBy 0x0100 (lineNumber * 0x10))



-- DECODER UTILS


andMap : BD.Decoder a -> BD.Decoder (a -> b) -> BD.Decoder b
andMap decoderA decoderF =
    BD.andThen
        (\f ->
            BD.map
                (\a -> f a)
                decoderA
        )
        decoderF


nTimes : Int -> BD.Decoder a -> BD.Decoder (List a)
nTimes count decoder =
    BD.loop ( count, [] )
        (\( n, ls ) ->
            if n > 0 then
                BD.map (\a -> BD.Loop ( n - 1, a :: ls )) decoder

            else
                BD.succeed (BD.Done <| List.reverse ls)
        )
