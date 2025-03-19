module Extra.System.File.Remote exposing (getTree)

import Bytes exposing (Bytes)
import Extra.System.File.Util as Util
import Extra.System.IO as IO exposing (IO)
import Extra.Type.Either exposing (Either(..))


getTree : Maybe String -> String -> IO.IO s (Util.Tree s)
getTree prefix remotePath =
    Util.requestString prefix remotePath <|
        \response ->
            case response of
                Right body ->
                    processBody prefix remotePath body

                Left _ ->
                    Util.getTreeError


processBody : Maybe String -> String -> String -> IO s (Util.Tree s)
processBody prefix remotePath body =
    Util.getTree (fileStep prefix) beforeDirStep afterDirStep remotePath body


type alias Acc =
    String


fileStep : Maybe String -> Util.FileStep s Acc
fileStep prefix name _ dirPath =
    ( dirPath, getFile prefix (dirPath ++ "/" ++ name) )


beforeDirStep : Util.BeforeDirStep Acc
beforeDirStep name dirPath =
    dirPath ++ "/" ++ name


afterDirStep : Util.AfterDirStep Acc
afterDirStep _ beforeFilePath _ =
    beforeFilePath


getFile : Maybe String -> String -> IO s (Maybe Bytes)
getFile prefix filePath =
    Util.requestBytes prefix filePath <|
        \response ->
            case response of
                Right body ->
                    IO.return (Just body)

                Left _ ->
                    IO.return Nothing
