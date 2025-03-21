module Extra.System.File.Static exposing (getTree)

import Bytes exposing (Bytes)
import Bytes.Decode
import Extra.System.File.Util as Util
import Extra.System.IO as IO exposing (IO)
import Extra.Type.Either exposing (Either(..))


getTree : Maybe String -> String -> IO s (Util.Tree s)
getTree prefix remotePath =
    Util.requestBytes prefix remotePath <|
        \response ->
            case response of
                Right body ->
                    processBodyBytes body

                Left _ ->
                    Util.getTreeError


processBodyBytes : Bytes -> IO s (Util.Tree s)
processBodyBytes body =
    case Bytes.Decode.decode treeBytesDecoder body of
        Just ( contentOffset, tree ) ->
            Util.getTree fileStep beforeDirStep afterDirStep ( contentOffset, body ) tree

        Nothing ->
            Util.getTreeError


treeBytesDecoder : Bytes.Decode.Decoder ( Int, String )
treeBytesDecoder =
    Bytes.Decode.andThen
        (\treeLength ->
            Bytes.Decode.map
                (\tree -> ( 4 + treeLength, tree ))
                (Bytes.Decode.string treeLength)
        )
        (Bytes.Decode.unsignedInt32 Bytes.BE)


type alias Acc =
    ( Int, Bytes )


fileStep : Util.FileStep s Acc
fileStep _ size ( offset, bytes ) =
    ( ( offset + size, bytes ), IO.return (Util.sliceBytes offset size bytes) )


beforeDirStep : Util.BeforeDirStep Acc
beforeDirStep _ acc =
    acc


afterDirStep : Util.AfterDirStep Acc
afterDirStep _ _ afterAcc =
    afterAcc
