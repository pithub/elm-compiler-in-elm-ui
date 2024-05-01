module Extra.System.File exposing
    ( Directory
    , Entry(..)
    , FileName
    , FilePath
    , FileSystem
    , State
    , addExtension
    , addName
    , addNames
    , combine
    , createDirectoryIfMissing
    , doesDirectoryExist
    , doesFileExist
    , dropLastName
    , fromString
    , getAppUserDataDirectory
    , getCurrentDirectory
    , getCurrentDirectoryEntriesPure
    , getCurrentDirectoryNamesPure
    , getModificationTime
    , getNames
    , initialState
    , isRelative
    , makeAbsolute
    , makeRelative
    , mount
    , readFile
    , removeDirectory
    , removeFile
    , resetFileSystem
    , setCurrentDirectory
    , splitExtension
    , splitLastName
    , toString
    , writeFile
    )

import Bytes exposing (Bytes)
import Extra.System.File.Remote as Remote
import Extra.System.IO as IO
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map
import Global
import Task
import Time



-- PUBLIC STATE


type alias State a c d e f g h =
    Global.State a FileSystem c d e f g h


initialState : FileSystem
initialState =
    FileSystem
        -- root
        Map.empty
        --cwd
        []


lensFileSystem =
    { getter = \(Global.State _ x _ _ _ _ _ _) -> x
    , setter = \x (Global.State a _ c d e f g h) -> Global.State a x c d e f g h
    }


lensRoot =
    { getter = \(Global.State _ (FileSystem x _) _ _ _ _ _ _) -> x
    , setter = \x (Global.State a (FileSystem _ bi) c d e f g h) -> Global.State a (FileSystem x bi) c d e f g h
    }


lensCwd =
    { getter = \(Global.State _ (FileSystem _ x) _ _ _ _ _ _) -> x
    , setter = \x (Global.State a (FileSystem ai _) c d e f g h) -> Global.State a (FileSystem ai x) c d e f g h
    }



-- PRIVATE IO


type alias IO a c d e f g h v =
    IO.IO (State a c d e f g h) v


resetFileSystem : IO a c d e f g h ()
resetFileSystem =
    IO.putLens lensFileSystem initialState



-- FILE PATHS


type alias FileName =
    String


type FilePath
    = Absolute (TList FileName)
    | Relative (TList FileName)


getNames : FilePath -> TList FileName
getNames path =
    case path of
        Absolute names ->
            names

        Relative names ->
            names


modifyNames : FilePath -> (TList FileName -> TList FileName) -> FilePath
modifyNames path f =
    case path of
        Absolute names ->
            Absolute (f names)

        Relative names ->
            Relative (f names)



-- FROM AND TO STRING


fromString : String -> FilePath
fromString string =
    if String.startsWith "/" string then
        fromStringHelper Absolute (String.dropLeft 1 string)

    else
        fromStringHelper Relative string


fromStringHelper : (TList FileName -> FilePath) -> String -> FilePath
fromStringHelper constructor string =
    string
        |> String.split "/"
        |> MList.filter ((/=) "")
        |> MList.reverse
        |> constructor


toString : FilePath -> String
toString path =
    case path of
        Absolute names ->
            "/" ++ String.join "/" (MList.reverse names)

        Relative [] ->
            "."

        Relative names ->
            String.join "/" (MList.reverse names)



-- STANDARD FUNCTIONS


addExtension : FilePath -> String -> FilePath
addExtension path extension =
    case splitLastName path of
        ( parent, name ) ->
            addName parent (name ++ "." ++ extension)


addName : FilePath -> FileName -> FilePath
addName path name =
    modifyNames path (\names -> name :: names)


addNames : FilePath -> TList FileName -> FilePath
addNames path names =
    MList.foldl addName path names


combine : FilePath -> FilePath -> FilePath
combine bPath aPath =
    case aPath of
        Absolute _ ->
            aPath

        Relative aNames ->
            modifyNames bPath (\bNames -> aNames ++ bNames)


dropLastName : FilePath -> FilePath
dropLastName path =
    Tuple.first (splitLastName path)


isRelative : FilePath -> Bool
isRelative path =
    case path of
        Absolute _ ->
            False

        Relative _ ->
            True


makeRelative : FilePath -> FilePath -> FilePath
makeRelative base path =
    case ( base, path ) of
        ( Absolute baseNames, Absolute pathNames ) ->
            Relative <| MList.reverse <| makeRelativeHelper (MList.reverse baseNames) (MList.reverse pathNames)

        _ ->
            path


makeRelativeHelper : TList FileName -> TList FileName -> TList FileName
makeRelativeHelper baseNames pathNames =
    case ( baseNames, pathNames ) of
        ( baseName :: baseRest, pathName :: pathRest ) ->
            if baseName == pathName then
                makeRelativeHelper baseRest pathRest

            else
                pathNames

        _ ->
            pathNames


splitExtension : FileName -> ( FileName, String )
splitExtension name =
    case MList.reverse (String.split "." name) of
        extension :: rest ->
            ( String.join "." (MList.reverse rest), extension )

        _ ->
            ( name, "" )


splitLastName : FilePath -> ( FilePath, FileName )
splitLastName path =
    case path of
        Absolute (name :: rest) ->
            ( Absolute rest, name )

        Relative (name :: rest) ->
            ( Relative rest, name )

        _ ->
            ( path, "" )



-- FILE SYSTEM


type FileSystem
    = FileSystem
        -- root
        Directory
        -- cwd
        (TList FileName)


type alias Directory =
    Map.Map FileName ( Time.Posix, Entry )


type Entry
    = FileEntry Bytes
    | DirectoryEntry Directory
    | MountedFileEntry String Int


createDirectoryIfMissing : Bool -> FilePath -> IO a c d e f g h ()
createDirectoryIfMissing createParents filePath =
    walkFileSystem createParents filePath <|
        \maybeNode now ->
            ( Maybe.andThen
                (\( directory, fileName, maybeEntry ) ->
                    case maybeEntry of
                        Nothing ->
                            Just (Map.insert fileName ( now, DirectoryEntry Map.empty ) directory)

                        _ ->
                            Nothing
                )
                maybeNode
            , ()
            )


doesDirectoryExist : FilePath -> IO a c d e f g h Bool
doesDirectoryExist filePath =
    walkFileSystem False filePath <|
        \maybeNode _ ->
            ( Nothing
            , case maybeNode of
                Just ( _, _, Just ( _, DirectoryEntry _ ) ) ->
                    True

                _ ->
                    False
            )


doesFileExist : FilePath -> IO a c d e f g h Bool
doesFileExist filePath =
    walkFileSystem False filePath <|
        \maybeNode _ ->
            ( Nothing
            , case maybeNode of
                Just ( _, _, Just ( _, FileEntry _ ) ) ->
                    True

                Just ( _, _, Just ( _, MountedFileEntry _ _ ) ) ->
                    True

                _ ->
                    False
            )


getAppUserDataDirectory : FileName -> IO a c d e f g h FilePath
getAppUserDataDirectory app =
    IO.return <| Absolute [ "." ++ app ]


getCurrentDirectory : IO a c d e f g h FilePath
getCurrentDirectory =
    IO.rmap IO.get (\s -> Absolute (lensCwd.getter s))


getModificationTime : FilePath -> IO a c d e f g h Time.Posix
getModificationTime path =
    walkFileSystem False path <|
        \maybeNode _ ->
            ( Nothing
            , case maybeNode of
                Just ( _, _, Just ( time, FileEntry _ ) ) ->
                    time

                Just ( _, _, Just ( time, MountedFileEntry _ _ ) ) ->
                    time

                _ ->
                    Time.millisToPosix 0
            )


makeAbsolute : FilePath -> IO a c d e f g h FilePath
makeAbsolute path =
    case path of
        Absolute _ ->
            IO.return path

        Relative _ ->
            IO.rmap getCurrentDirectory (\cwd -> combine cwd path)


mount : String -> FilePath -> IO a c d e f g h ()
mount mountPoint filePath =
    IO.bind (getRemoteTree mountPoint) <|
        \remoteTree ->
            walkFileSystem True filePath <|
                \maybeNode now ->
                    ( Maybe.andThen
                        (\( directory, fileName, maybeEntry ) ->
                            case maybeEntry of
                                Nothing ->
                                    Just (Map.insert fileName ( now, DirectoryEntry remoteTree ) directory)

                                _ ->
                                    Nothing
                        )
                        maybeNode
                    , ()
                    )


readFile : FilePath -> IO a c d e f g h (Maybe Bytes)
readFile filePath =
    IO.andThen getFileContent <|
        walkFileSystem False filePath <|
            \maybeNode _ ->
                ( Nothing
                , case maybeNode of
                    Just ( _, _, Just ( _, FileEntry contents ) ) ->
                        Just (BytesContent contents)

                    Just ( _, _, Just ( _, MountedFileEntry path _ ) ) ->
                        Just (MountPath path)

                    _ ->
                        Nothing
                )


type FileContent
    = BytesContent Bytes
    | MountPath String


getFileContent : Maybe FileContent -> IO a c d e f g h (Maybe Bytes)
getFileContent maybeContent =
    case maybeContent of
        Just (BytesContent bytes) ->
            IO.return (Just bytes)

        Just (MountPath path) ->
            Remote.getFile path

        Nothing ->
            IO.return Nothing


removeDirectory : FilePath -> IO a c d e f g h ()
removeDirectory filePath =
    walkFileSystem False filePath <|
        \maybeNode _ ->
            case maybeNode of
                Just ( directory, fileName, Just ( _, DirectoryEntry _ ) ) ->
                    ( Just (Map.delete fileName directory), () )

                _ ->
                    ( Nothing, () )


removeFile : FilePath -> IO a c d e f g h ()
removeFile filePath =
    walkFileSystem False filePath <|
        \maybeNode _ ->
            case maybeNode of
                Just ( directory, fileName, Just ( _, FileEntry _ ) ) ->
                    ( Just (Map.delete fileName directory), () )

                _ ->
                    ( Nothing, () )


setCurrentDirectory : FilePath -> IO a c d e f g h ()
setCurrentDirectory cwd =
    IO.bind (makeAbsolute cwd) <|
        \absolutePath ->
            IO.putLens lensCwd <| getNames absolutePath


writeFile : FilePath -> Bytes -> IO a c d e f g h ()
writeFile filePath contents =
    walkFileSystem False filePath <|
        \maybeNode now ->
            case maybeNode of
                Just ( _, _, Just ( _, DirectoryEntry _ ) ) ->
                    -- can't overwrite a directory
                    ( Nothing, () )

                Just ( directory, fileName, _ ) ->
                    ( Just (Map.insert fileName ( now, FileEntry contents ) directory), () )

                _ ->
                    ( Nothing, () )



-- PURE IO


getCurrentDirectoryNamesPure : State a c d e f g h -> TList FileName
getCurrentDirectoryNamesPure state =
    MList.reverse (lensCwd.getter state)


getCurrentDirectoryEntryPure : State a c d e f g h -> Directory
getCurrentDirectoryEntryPure state =
    let
        goto names directory =
            case names of
                [] ->
                    directory

                name :: rest ->
                    case Map.lookup name directory of
                        Just ( _, DirectoryEntry subDirectory ) ->
                            goto rest subDirectory

                        _ ->
                            directory
    in
    goto (getCurrentDirectoryNamesPure state) (lensRoot.getter state)


getCurrentDirectoryEntriesPure :
    State a c d e f g h
    -> (FileName -> Int -> Time.Posix -> z)
    -> ( TList z, TList z )
getCurrentDirectoryEntriesPure state f =
    Map.foldlWithKey
        (\( dirs, files ) name ( time, entry ) ->
            case entry of
                DirectoryEntry directory ->
                    ( f name (Map.size directory) time :: dirs, files )

                FileEntry bytes ->
                    ( dirs, f name (Bytes.width bytes) time :: files )

                MountedFileEntry _ size ->
                    ( dirs, f name size time :: files )
        )
        ( [], [] )
        (getCurrentDirectoryEntryPure state)



-- LOW LEVEL


walkFileSystem :
    Bool
    -> FilePath
    -> (Maybe ( Directory, FileName, Maybe ( Time.Posix, Entry ) ) -> Time.Posix -> ( Maybe Directory, v ))
    -> IO a c d e f g h v
walkFileSystem createDirectories filePath callback =
    IO.bind (IO.liftA2 Tuple.pair (makeAbsolute filePath) getTime) <|
        \( absolutePath, now ) ->
            IO.bind IO.get <|
                \s ->
                    let
                        root =
                            lensRoot.getter s

                        ( result, newRoot ) =
                            walkFileSystemPure createDirectories absolutePath now callback root
                    in
                    IO.rmap (IO.putLens lensRoot newRoot) (\_ -> result)


walkFileSystemPure :
    Bool
    -> FilePath
    -> Time.Posix
    -> (Maybe ( Directory, FileName, Maybe ( Time.Posix, Entry ) ) -> Time.Posix -> ( Maybe Directory, a ))
    -> (Directory -> ( a, Directory ))
walkFileSystemPure createDirectories filePath now callback root =
    let
        down :
            TList ( Directory, FileName )
            -> TList FileName
            -> Directory
            -> ( a, Directory )
        down visited fileNames directory =
            case fileNames of
                [] ->
                    -- only possible for root directory
                    finishWalk visited <| Just ( directory, "", Nothing )

                [ fileName ] ->
                    case Map.lookup fileName directory of
                        Just timeAndEntry ->
                            finishWalk visited <| Just ( directory, fileName, Just timeAndEntry )

                        _ ->
                            finishWalk visited <| Just ( directory, fileName, Nothing )

                fileName :: subFileNames ->
                    case Map.lookup fileName directory of
                        Just ( _, DirectoryEntry subDirectory ) ->
                            down (( directory, fileName ) :: visited) subFileNames subDirectory

                        _ ->
                            if createDirectories then
                                down (( directory, fileName ) :: visited) subFileNames Map.empty

                            else
                                finishWalk visited Nothing

        finishWalk :
            TList ( Directory, FileName )
            -> Maybe ( Directory, FileName, Maybe ( Time.Posix, Entry ) )
            -> ( a, Directory )
        finishWalk visited input =
            case callback input now of
                ( Just changedDirectory, result ) ->
                    ( result, up changedDirectory visited )

                ( Nothing, result ) ->
                    ( result, root )

        up : Directory -> TList ( Directory, FileName ) -> Directory
        up directory visited =
            MList.foldl
                (\changedDirectory ( parentDirectory, fileName ) ->
                    Map.insert fileName ( now, DirectoryEntry changedDirectory ) parentDirectory
                )
                directory
                visited
    in
    down [] (MList.reverse (getNames filePath)) root


getTime : IO a c d e f g h Time.Posix
getTime =
    IO.liftCmd <| Task.perform identity Time.now



-- REMOTE


getRemoteTree : String -> IO a c d e f g h Directory
getRemoteTree remotePath =
    IO.rmap (Remote.getTree remotePath) mapRemoteTree


mapRemoteTree : Remote.Directory -> Directory
mapRemoteTree remoteDirectory =
    Map.map mapRemoteEntry remoteDirectory


mapRemoteEntry : ( Int, Remote.Entry ) -> ( Time.Posix, Entry )
mapRemoteEntry ( time, entry ) =
    ( Time.millisToPosix time
    , case entry of
        Remote.FileEntry path size ->
            MountedFileEntry path size

        Remote.DirectoryEntry directory ->
            DirectoryEntry (mapRemoteTree directory)
    )
