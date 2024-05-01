module Extra.System.Http exposing
    ( Exception
    , Header
    , Manager
    , Method
    , Request
    , directManager
    , methodGet
    , methodPost
    , newManager
    , parseUrlThrow
    , urlEncodeVars
    , userAgent
    , withBytesResponse
    , withStringResponse
    )

import Bytes exposing (Bytes)
import Extra.System.IO as IO exposing (IO)
import Extra.Type.Either exposing (Either(..))
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map
import Http



-- MANAGER


type Manager
    = Direct
    | Proxied


newManager : IO s Manager
newManager =
    IO.return Proxied


directManager : IO s Manager
directManager =
    IO.return Direct


managedUrl : Manager -> String -> String
managedUrl manager url =
    case manager of
        Direct ->
            url

        Proxied ->
            "http://localhost:8088/proxy/" ++ url



-- HEADERS


type alias Header =
    Http.Header


userAgent : String -> Header
userAgent agent =
    Http.header "User-Agent" agent



-- REQUEST


type alias Method =
    String


methodGet : Method
methodGet =
    "GET"


methodPost : Method
methodPost =
    "POST"


type alias Request =
    { method : Method
    , headers : TList Header
    , url : String
    , body : Http.Body
    }


parseUrlThrow : String -> IO s Request
parseUrlThrow url =
    IO.return
        { method = methodGet
        , headers = []
        , url = url
        , body = Http.emptyBody
        }



-- RESPONSE


type alias Exception =
    Http.Error



-- IO


withStringResponse : Request -> Manager -> (Either Exception String -> IO s a) -> IO s a
withStringResponse request manager handler =
    Http.request
        { method = request.method
        , headers = request.headers
        , url = managedUrl manager request.url
        , body = request.body
        , expect = Http.expectString (mapHandler handler)
        , timeout = Nothing
        , tracker = Nothing
        }
        |> IO.liftCmdIO


withBytesResponse : Request -> Manager -> (Either Exception Bytes -> IO s a) -> IO s a
withBytesResponse request manager handler =
    Http.request
        { method = request.method
        , headers = request.headers
        , url = managedUrl manager request.url
        , body = request.body
        , expect = Http.expectBytesResponse (mapHandler handler) toResult
        , timeout = Nothing
        , tracker = Nothing
        }
        |> IO.liftCmdIO


mapHandler : (Either Exception a -> IO s b) -> Result Http.Error a -> IO s b
mapHandler handler result =
    handler <|
        case result of
            Ok a ->
                Right a

            Err error ->
                Left error


toResult : Http.Response Bytes -> Result Http.Error Bytes
toResult response =
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.BadStatus_ metadata _ ->
            Err (Http.BadStatus metadata.statusCode)

        Http.GoodStatus_ _ body ->
            Ok body



-- URL ENCODING


urlEncode : String -> String
urlEncode string =
    string |> String.toList |> MList.map urlEncodeChar |> String.join ""


urlEncodeVars : TList ( String, String ) -> String
urlEncodeVars vars =
    MList.map (\( key, value ) -> urlEncode key ++ "=" ++ urlEncode value) vars
        |> String.join "&"


urlEncodeChar : Char -> String
urlEncodeChar char =
    case Map.lookup char specialChars of
        Just replacement ->
            replacement

        Nothing ->
            String.fromChar char


specialChars : Map.Map Char String
specialChars =
    Map.fromList
        [ ( ' ', "%20" )
        , ( '!', "%21" )
        , ( '#', "%23" )
        , ( '$', "%24" )
        , ( '%', "%25" )
        , ( '&', "%26" )
        , ( '\'', "%27" )
        , ( '(', "%28" )
        , ( ')', "%29" )
        , ( '*', "%2A" )
        , ( '+', "%2B" )
        , ( ',', "%2C" )
        , ( '/', "%2F" )
        , ( ':', "%3A" )
        , ( ';', "%3B" )
        , ( '=', "%3D" )
        , ( '?', "%3F" )
        , ( '@', "%40" )
        , ( '[', "%5B" )
        , ( ']', "%5D" )
        ]
