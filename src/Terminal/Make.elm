{- MANUALLY FORMATTED -}
module Terminal.Make exposing
  ( {-Flags(..)
  , Output(..)
  , ReportType(..)
  ,-} run, IO
  --, reportType
  --, output
  --, docsFile
  )


import Builder.Build as Build
import Builder.Elm.Details as Details
import Builder.File as File
import Builder.Generate as Generate
import Builder.Reporting.Exit as Exit
import Builder.Reporting.Task as Task
import Compiler.AST.Optimized as Opt
import Compiler.Data.NonEmptyList as NE
import Compiler.Elm.ModuleName as ModuleName
import Extra.System.File as SysFile exposing (FilePath)
import Extra.System.IO as IO
import Extra.Type.Either exposing (Either)
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Maybe as MMaybe
import Terminal.Command as Command



-- PRIVATE IO


type alias IO g h v =
  IO.IO (Command.State g h) v



-- RUN


type alias Task z g h v =
  Task.Task z (Command.State g h) Exit.Make v


{- NEW: async -}
run : FilePath -> TList FilePath -> Bool -> Bool -> Bool -> FilePath -> IO g h (Either Exit.Make ())
run root paths debug optimize async target =
  Task.run <|
  Task.bind (getMode debug optimize async) <| \desiredMode ->
  Task.bind (Task.eio Exit.MakeBadDetails (Details.load root)) <| \details ->
  case paths of
    [] ->
      Task.bind (getExposed details) <| \exposed ->
      buildExposed root details exposed

    p::ps ->
      Task.bind (buildPaths root details (NE.CList p ps)) <| \artifacts ->
      case getNoMains artifacts of
        [] ->
          Task.bind (toBuilder root details desiredMode artifacts) <| \builder ->
          generate target builder

        name::names ->
          Task.throw (Exit.MakeNonMainFilesIntoJavaScript name names)



-- GET INFORMATION


getMode : Bool -> Bool -> Bool -> Task z g h DesiredMode
getMode debug optimize async =
  case (debug, optimize, async) of
    (True , True , _    ) -> Task.throw Exit.MakeCannotOptimizeAndDebug
    (True , False, _    ) -> Task.return Debug
    (False, False, True ) -> Task.return Async
    (False, False, False) -> Task.return Dev
    (False, True , _    ) -> Task.return Prod


getExposed : Details.Details -> Task z g h (NE.TList ModuleName.Raw)
getExposed (Details.Details _ validOutline _ _ _ _) =
  case validOutline of
    Details.ValidApp _ ->
      Task.throw Exit.MakeAppNeedsFileNames

    Details.ValidPkg _ exposed _ ->
      case exposed of
        [] -> Task.throw Exit.MakePkgNeedsExposing
        m::ms -> Task.return (NE.CList m ms)



-- BUILD PROJECTS


buildExposed : FilePath -> Details.Details -> NE.TList ModuleName.Raw -> Task z g h ()
buildExposed root details exposed =
  let
    docsGoal = Build.ignoreDocs
  in
  Task.eio Exit.MakeCannotBuild <|
    Build.fromExposed root details docsGoal exposed


buildPaths : FilePath -> Details.Details -> NE.TList FilePath -> Task z g h Build.Artifacts
buildPaths root details paths =
  Task.eio Exit.MakeCannotBuild <|
    Build.fromPaths root details paths



-- GET MAINS


isMain : ModuleName.Raw -> Build.Module -> Bool
isMain targetName modul =
  case modul of
    Build.Fresh name _ (Opt.LocalGraph maybeMain _ _) ->
      MMaybe.isJust maybeMain && name == targetName

    Build.Cached name mainIsDefined _ ->
      mainIsDefined && name == targetName



-- GET MAINLESS


getNoMains : Build.Artifacts -> TList ModuleName.Raw
getNoMains (Build.Artifacts _ _ roots modules) =
  MMaybe.mapMaybe (getNoMain modules) (NE.toList roots)


getNoMain : TList Build.Module -> Build.Root -> Maybe ModuleName.Raw
getNoMain modules root =
  case root of
    Build.Inside name ->
      if MList.any (isMain name) modules
      then Nothing
      else Just name

    Build.Outside name (Opt.LocalGraph maybeMain _ _) ->
      case maybeMain of
        Just _  -> Nothing
        Nothing -> Just name



-- GENERATE


generate : FilePath -> String -> Task z g h ()
generate target builder =
  Task.io <|
    IO.bind (SysFile.createDirectoryIfMissing True (SysFile.dropLastName target)) <| \_ ->
    File.writeUtf8 target builder



-- TO BUILDER


type DesiredMode = Debug | Async | Dev | Prod


toBuilder : FilePath -> Details.Details -> DesiredMode -> Build.Artifacts -> Task z g h String
toBuilder root details desiredMode artifacts =
  Task.mapError Exit.MakeBadGenerate <|
    case desiredMode of
      Debug -> Generate.debug root details artifacts
      Async -> Generate.async root details artifacts
      Dev   -> Generate.dev   root details artifacts
      Prod  -> Generate.prod  root details artifacts
