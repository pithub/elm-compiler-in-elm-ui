module Compiler.Elm.ModuleName exposing
    ( Canonical(..)
    , Comparable
    , Raw
    , array
    , bCanonical
    , bComparable
    , bRaw
    , basics
    , char
    , cmd
    , comparison
    , debug
    , decoder
    , dict
    , encode
    , fromComparable
    , getModule
    , jsonDecode
    , jsonEncode
    , list
    , matrix4
    , maybe
    , platform
    , result
    , string
    , sub
    , texture
    , toChars
    , toComparable
    , toFileNames
    , toHyphenName
    , toPackage
    , toString
    , tuple
    , vector2
    , vector3
    , vector4
    , virtualDom
    , webgl
    )

import Compiler.Data.Name as Name
import Compiler.Data.Utf8 as Utf8
import Compiler.Elm.Package as Pkg
import Compiler.Json.Decode as D
import Compiler.Json.Encode as E
import Compiler.Parse.Primitives as P exposing (Col, Row)
import Compiler.Parse.Variable as Var
import Extra.Data.Binary as B
import Extra.System.File exposing (FileName)
import Extra.Type.List exposing (TList)



-- RAW


type alias Raw =
    Name.Name


toChars : Raw -> String
toChars =
    identity


toFileNames : Raw -> TList FileName
toFileNames name =
    String.split "." name


toHyphenName : Raw -> FileName
toHyphenName name =
    String.map
        (\c ->
            if c == '.' then
                '-'

            else
                c
        )
        name



-- JSON


encode : Raw -> E.Value
encode =
    E.name


decoder : D.Decoder z ( Row, Col ) Raw
decoder =
    D.customString parser Tuple.pair



-- PARSER


parser : P.Parser z ( Row, Col ) Raw
parser =
    P.Parser <|
        \(P.State src pos end indent row col) cok _ cerr eerr ->
            let
                ( isGood, newPos, newCol ) =
                    chompStart src pos end col
            in
            if isGood && newPos - pos < 256 then
                let
                    newState =
                        P.State src newPos end indent row newCol
                in
                cok (Utf8.fromPtr src pos newPos) newState

            else if col == newCol then
                eerr row newCol Tuple.pair

            else
                cerr row newCol Tuple.pair


chompStart : String -> Int -> Int -> Col -> ( Bool, Int, Col )
chompStart src pos end col =
    let
        width =
            Var.getUpperWidth src pos end
    in
    if width == 0 then
        ( False, pos, col )

    else
        chompInner src (pos + width) end (col + 1)


chompInner : String -> Int -> Int -> Col -> ( Bool, Int, Col )
chompInner src pos end col =
    if pos >= end then
        ( True, pos, col )

    else
        let
            word =
                P.unsafeIndex src pos

            width =
                Var.getInnerWidthHelp word
        in
        if width == 0 then
            if word == 0x2E {- . -} then
                chompStart src (pos + 1) end (col + 1)

            else
                ( True, pos, col )

        else
            chompInner src (pos + width) end (col + 1)



-- CANONICAL


type Canonical
    = Canonical
        --{ package : Pkg.Name
        --, module : Name.Name
        --}
        Pkg.Name
        Name.Name


getModule (Canonical _ name) =
    name


type alias Comparable =
    ( String, Pkg.Comparable )


toComparable : Canonical -> Comparable
toComparable (Canonical pkg name) =
    ( name, Pkg.toComparable pkg )


fromComparable : Comparable -> Canonical
fromComparable ( name, pkg ) =
    Canonical (Pkg.fromComparable pkg) name


comparison : Canonical -> Canonical -> Order
comparison can1 can2 =
    compare (toComparable can1) (toComparable can2)


toString : Canonical -> String
toString (Canonical pkg name) =
    Pkg.toString pkg ++ "/" ++ name


toPackage : Canonical -> Pkg.Name
toPackage (Canonical pkg _) =
    pkg



-- INSTANCES


bRaw : B.Binary Raw
bRaw =
    Name.bName


bCanonical : B.Binary Canonical
bCanonical =
    B.bin2 Canonical (\(Canonical a b) -> B.T2 a b) Pkg.bName Name.bName


bComparable : B.Binary Comparable
bComparable =
    B.iso toComparable fromComparable bCanonical



-- CORE


basics : Canonical
basics =
    Canonical Pkg.core Name.basics


char : Canonical
char =
    Canonical Pkg.core Name.char


string : Canonical
string =
    Canonical Pkg.core Name.string


maybe : Canonical
maybe =
    Canonical Pkg.core Name.maybe


result : Canonical
result =
    Canonical Pkg.core Name.result


list : Canonical
list =
    Canonical Pkg.core Name.list


array : Canonical
array =
    Canonical Pkg.core Name.array


dict : Canonical
dict =
    Canonical Pkg.core Name.dict


tuple : Canonical
tuple =
    Canonical Pkg.core Name.tuple


platform : Canonical
platform =
    Canonical Pkg.core Name.platform


cmd : Canonical
cmd =
    Canonical Pkg.core "Platform.Cmd"


sub : Canonical
sub =
    Canonical Pkg.core "Platform.Sub"


debug : Canonical
debug =
    Canonical Pkg.core Name.debug



-- HTML


virtualDom : Canonical
virtualDom =
    Canonical Pkg.virtualDom Name.virtualDom



-- JSON


jsonDecode : Canonical
jsonDecode =
    Canonical Pkg.json "Json.Decode"


jsonEncode : Canonical
jsonEncode =
    Canonical Pkg.json "Json.Encode"



-- WEBGL


webgl : Canonical
webgl =
    Canonical Pkg.webgl "WebGL"


texture : Canonical
texture =
    Canonical Pkg.webgl "WebGL.Texture"


vector2 : Canonical
vector2 =
    Canonical Pkg.linearAlgebra "Math.Vector2"


vector3 : Canonical
vector3 =
    Canonical Pkg.linearAlgebra "Math.Vector3"


vector4 : Canonical
vector4 =
    Canonical Pkg.linearAlgebra "Math.Vector4"


matrix4 : Canonical
matrix4 =
    Canonical Pkg.linearAlgebra "Math.Matrix4"
