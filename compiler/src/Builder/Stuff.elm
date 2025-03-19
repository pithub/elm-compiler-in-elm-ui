{- MANUALLY FORMATTED -}
module Builder.Stuff exposing
  ( details
  , interfaces
  , objects
  --, prepublishDir
  , elmi
  , elmo
  --, temp
  , findRoot
  --, withRootLock
  --, withRegistryLock
  , PackageCache
  , getPackageCache
  , registry
  , package
  , getReplCache
  , getElmHome
  )


import Compiler.Elm.ModuleName as ModuleName
import Compiler.Elm.Package as Pkg
import Compiler.Elm.Version as V
import Extra.System.File as SysFile exposing (FileName, FilePath)
import Extra.System.IO as IO


-- PRIVATE IO


type alias IO b c d e f g h v =
  IO.IO (SysFile.State b c d e f g h) v



-- PATHS


stuff : FilePath -> FilePath
stuff root =
  SysFile.addNames root [ "elm-stuff", compilerVersion ]


details : FilePath -> FilePath
details root =
  SysFile.addName (stuff root) "d.dat"


interfaces : FilePath -> FilePath
interfaces root =
  SysFile.addName (stuff root) "i.dat"


objects : FilePath -> FilePath
objects root =
  SysFile.addName (stuff root) "o.dat"


compilerVersion : FileName
compilerVersion =
  V.toChars V.compiler



-- ELMI and ELMO


elmi : FilePath -> ModuleName.Raw -> FilePath
elmi root name =
  toArtifactPath root name "elmi"


elmo : FilePath -> ModuleName.Raw -> FilePath
elmo root name =
  toArtifactPath root name "elmo"


toArtifactPath : FilePath -> ModuleName.Raw -> String -> FilePath
toArtifactPath root name ext =
  SysFile.addName (stuff root) (ModuleName.toHyphenName name ++ "." ++ ext)



-- ROOT


findRoot : IO b c d e f g h (Maybe FilePath)
findRoot =
  IO.bind SysFile.getCurrentDirectory <| \dir ->
  findRootHelp dir


findRootHelp : FilePath -> IO b c d e f g h (Maybe FilePath)
findRootHelp dirs =
  IO.bind (SysFile.doesFileExist (SysFile.addName dirs "elm.json")) <| \exists ->
  if exists
    then IO.return (Just dirs)
    else
      case SysFile.splitLastName dirs of
        ( _, "" ) ->
          IO.return Nothing

        ( parent, _ ) ->
          findRootHelp parent



-- PACKAGE CACHES


type PackageCache = PackageCache FilePath


getPackageCache : IO b c d e f g h PackageCache
getPackageCache =
  IO.fmap PackageCache <| getCacheDir "packages"


registry : PackageCache -> FilePath
registry (PackageCache dir) =
  SysFile.addName dir "registry.dat"


package : PackageCache -> Pkg.Name -> V.Version -> FilePath
package (PackageCache dir) name version =
  SysFile.addName (SysFile.combine dir (Pkg.toFilePath name)) (V.toChars version)



-- CACHE


getReplCache : IO b c d e f g h FilePath
getReplCache =
  getCacheDir "repl"


getCacheDir : FileName -> IO b c d e f g h FilePath
getCacheDir projectName =
  IO.bind getElmHome <| \home ->
  let root = SysFile.addNames home [ compilerVersion, projectName ] in
  IO.bind (SysFile.createDirectoryIfMissing True root) <| \_ ->
  IO.return root


getElmHome : IO b c d e f g h FilePath
getElmHome =
  SysFile.getAppUserDataDirectory "elm"
