module Extra.System.File.Remote exposing
    ( Directory
    , Entry(..)
    , getFile
    , getTree
    )

import Bytes exposing (Bytes)
import Extra.System.Http as Http
import Extra.System.IO as IO
import Extra.Type.Either exposing (Either(..))
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map
import Json.Decode as Json



-- READ DIRECTORY TREE


type alias Directory =
    Map.Map String ( Int, Entry )


type Entry
    = FileEntry String Int
    | DirectoryEntry Directory


getTree : String -> IO.IO s Directory
getTree remotePath =
    queryString remotePath <|
        \response ->
            case response of
                Left _ ->
                    IO.return Map.empty

                Right body ->
                    processBody remotePath body


processBody : String -> String -> IO.IO s Directory
processBody remotePath body =
    case Json.decodeString directoryDecoder body of
        Ok ( directories, files ) ->
            getDirectories remotePath directories
                |> IO.fmap (addFiles remotePath files)

        Err _ ->
            IO.return Map.empty



-- DIRECTORIES


getDirectories : String -> TList ( String, Int ) -> IO.IO s Directory
getDirectories remotePath dirs =
    Map.fromList dirs
        |> Map.traverseWithKey IO.pure IO.liftA2 (getDirectory remotePath)


getDirectory : String -> String -> Int -> IO.IO s ( Int, Entry )
getDirectory remotePath name time =
    getTree (remotePath ++ "/" ++ name)
        |> IO.fmap (\directory -> ( time, DirectoryEntry directory ))



-- FILES


addFiles : String -> TList ( String, Int, Int ) -> Directory -> Directory
addFiles remotePath files directory =
    MList.foldl (addFile remotePath) directory files


addFile : String -> Directory -> ( String, Int, Int ) -> Directory
addFile remotePath directory ( name, time, size ) =
    Map.insert name ( time, FileEntry (remotePath ++ "/" ++ name) size ) directory



-- JSON


directoryDecoder : Json.Decoder ( TList ( String, Int ), TList ( String, Int, Int ) )
directoryDecoder =
    Json.map2 Tuple.pair
        (Json.field "dirs" (Json.list dirEntryDecoder))
        (Json.field "files" (Json.list fileEntryDecoder))


dirEntryDecoder : Json.Decoder ( String, Int )
dirEntryDecoder =
    Json.map2 Tuple.pair
        (Json.field "name" Json.string)
        (Json.field "mtime" timeDecoder)


fileEntryDecoder : Json.Decoder ( String, Int, Int )
fileEntryDecoder =
    Json.map3 (\name time size -> ( name, time, size ))
        (Json.field "name" Json.string)
        (Json.field "mtime" timeDecoder)
        (Json.field "size" Json.int)


timeDecoder : Json.Decoder Int
timeDecoder =
    Json.map round Json.float



-- READ FILE


getFile : String -> IO.IO s (Maybe Bytes)
getFile filePath =
    queryBytes filePath <|
        \response ->
            case response of
                Left _ ->
                    IO.return Nothing

                Right body ->
                    IO.return (Just body)



-- HTTP


queryString : String -> (Either Http.Exception String -> IO.IO s a) -> IO.IO s a
queryString remotePath callback =
    query remotePath (\manager request -> Http.withStringResponse request manager callback)


queryBytes : String -> (Either Http.Exception Bytes -> IO.IO s a) -> IO.IO s a
queryBytes remotePath callback =
    query remotePath (\manager request -> Http.withBytesResponse request manager callback)


query : String -> (Http.Manager -> Http.Request -> IO.IO s a) -> IO.IO s a
query remotePath callback =
    IO.bind Http.directManager <|
        \manager ->
            IO.bind (Http.parseUrlThrow <| "/query/" ++ remotePath) (callback manager)
