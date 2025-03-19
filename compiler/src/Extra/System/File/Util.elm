module Extra.System.File.Util exposing
    ( AfterDirStep
    , BeforeDirStep
    , Directory
    , Entry
    , FileStep
    , InternalDirectory
    , InternalEntry(..)
    , Tree
    , getTreeError
    , getTree
    , requestBytes
    , requestString
    , sliceBytes
    )

import Bytes exposing (Bytes)
import Bytes.Decode
import Extra.System.Http as Http
import Extra.System.IO as IO exposing (IO)
import Extra.Type.Either exposing (Either)
import Extra.Type.Map as Map exposing (Map)
import Json.Decode
import Time



-- MOUNTED DIRECTORY TREE


type alias Tree s =
    ( Time.Posix, Directory s )


type alias Directory s =
    InternalDirectory (IO s (Maybe Bytes))


type alias InternalDirectory a =
    Map String ( Time.Posix, InternalEntry a )


type alias Entry s =
    InternalEntry (IO s (Maybe Bytes))


type InternalEntry a
    = UnreadFileEntry Int a
    | DirectoryEntry (InternalDirectory a)



-- BUILD DIRECTORY TREE


type alias FileStep s v =
    String -> Int -> v -> ( v, IO s (Maybe Bytes) )


type alias BeforeDirStep v =
    String -> v -> v


type alias AfterDirStep v =
    String -> v -> v -> v


getTree : FileStep s v -> BeforeDirStep v -> AfterDirStep v -> v -> String -> IO.IO s ( Time.Posix, Directory s )
getTree fileStep beforeDirStep afterDirStep v tree =
    case Json.Decode.decodeString directoryDecoder tree of
        Ok ( time, directory ) ->
            IO.return ( time, processUnreadFiles fileStep beforeDirStep afterDirStep v directory )

        Err _ ->
            getTreeError


getTreeError : IO s ( Time.Posix, InternalDirectory a )
getTreeError =
    IO.rmap IO.now (\time -> ( time, Map.empty ))



-- JSON TREE DECODER


type alias TmpDirectory =
    InternalDirectory ()


type alias TmpEntry =
    InternalEntry ()


directoryDecoder : Json.Decode.Decoder ( Time.Posix, TmpDirectory )
directoryDecoder =
    Json.Decode.map2 Tuple.pair
        (Json.Decode.index 1 timeDecoder)
        (Json.Decode.index 0 (Json.Decode.dict timeEntryDecoder))


timeEntryDecoder : Json.Decode.Decoder ( Time.Posix, TmpEntry )
timeEntryDecoder =
    Json.Decode.oneOf
        [ timeFileEntryDecoder
        , timeDirEntryDecoder
        ]


timeFileEntryDecoder : Json.Decode.Decoder ( Time.Posix, TmpEntry )
timeFileEntryDecoder =
    Json.Decode.map2 (\time size -> ( time, UnreadFileEntry size () ))
        (Json.Decode.index 1 timeDecoder)
        (Json.Decode.index 0 Json.Decode.int)


timeDirEntryDecoder : Json.Decode.Decoder ( Time.Posix, TmpEntry )
timeDirEntryDecoder =
    Json.Decode.map (\( time, directory ) -> ( time, DirectoryEntry directory ))
        (Json.Decode.lazy (\_ -> directoryDecoder))


timeDecoder : Json.Decode.Decoder Time.Posix
timeDecoder =
    Json.Decode.map Time.millisToPosix Json.Decode.int



-- PROCESS DIRECTORY TREE


processUnreadFiles : FileStep s v -> BeforeDirStep v -> AfterDirStep v -> v -> TmpDirectory -> Directory s
processUnreadFiles fileStep beforeDirStep afterDirStep v directory =
    Tuple.second (foldDirectory fileStep beforeDirStep afterDirStep v directory)


foldDirectory : FileStep s v -> BeforeDirStep v -> AfterDirStep v -> v -> TmpDirectory -> ( v, Directory s )
foldDirectory fileStep beforeDirStep afterDirStep v directory =
    Map.foldlWithKey (directoryFolder fileStep beforeDirStep afterDirStep) ( v, Map.empty ) directory


directoryFolder : FileStep s v -> BeforeDirStep v -> AfterDirStep v -> ( v, Directory s ) -> String -> ( Time.Posix, TmpEntry ) -> ( v, Directory s )
directoryFolder fileStep beforeDirStep afterDirStep ( v, directory ) name ( time, entry ) =
    case mapEntry fileStep beforeDirStep afterDirStep v name entry of
        ( newV, newEntry ) ->
            ( newV, Map.insert name ( time, newEntry ) directory )


mapEntry : FileStep s v -> BeforeDirStep v -> AfterDirStep v -> v -> String -> TmpEntry -> ( v, Entry s )
mapEntry fileStep beforeDirStep afterDirStep v name entry =
    case entry of
        UnreadFileEntry size () ->
            mapFileEntry fileStep v name size

        DirectoryEntry subDirectory ->
            mapDirEntry fileStep beforeDirStep afterDirStep v name subDirectory


mapFileEntry : FileStep s v -> v -> String -> Int -> ( v, Entry s )
mapFileEntry fileStep v name size =
    case fileStep name size v of
        ( newV, io ) ->
            ( newV, UnreadFileEntry size io )


mapDirEntry : FileStep s v -> BeforeDirStep v -> AfterDirStep v -> v -> String -> TmpDirectory -> ( v, Entry s )
mapDirEntry fileStep beforeDirStep afterDirStep v name subDirectory =
    case foldDirectory fileStep beforeDirStep afterDirStep (beforeDirStep name v) subDirectory of
        ( newV, newSubDirectory ) ->
            ( afterDirStep name v newV, DirectoryEntry newSubDirectory )



-- BYTES SLICES


sliceBytes : Int -> Int -> Bytes -> Maybe Bytes
sliceBytes offset length bytes =
    Bytes.Decode.decode (sliceDecoder offset length) bytes


sliceDecoder : Int -> Int -> Bytes.Decode.Decoder Bytes
sliceDecoder offset length =
    Bytes.Decode.andThen
        (\_ -> Bytes.Decode.bytes length)
        (Bytes.Decode.bytes offset)



-- HTTP REQUESTS


requestString : Maybe String -> String -> (Either Http.Exception String -> IO s a) -> IO s a
requestString prefix remotePath callback =
    performRequest prefix remotePath (\manager request -> Http.withStringResponse request manager callback)


requestBytes : Maybe String -> String -> (Either Http.Exception Bytes -> IO s a) -> IO s a
requestBytes prefix remotePath callback =
    performRequest prefix remotePath (\manager request -> Http.withBytesResponse request manager callback)


performRequest : Maybe String -> String -> (Http.Manager -> Http.Request -> IO s a) -> IO s a
performRequest prefix remotePath callback =
    IO.bind (Http.newManager prefix) <|
        \manager ->
            IO.bind (Http.parseUrlThrow remotePath) (callback manager)
