module Compiler.AST.Utils.Shader exposing
    ( Source
    , Type(..)
    , Types(..)
    , bSource
    , fromChars
    , toJsStringBuilder
    )

import Compiler.Data.Name as Name
import Extra.Data.Binary as B
import Extra.Type.Map as Map



-- SOURCE


type Source
    = Source String



-- TYPES


type Types
    = Types {- attribute -} (Map.Map Name.Name Type) {- uniform -} (Map.Map Name.Name Type) {- varying -} (Map.Map Name.Name Type)


type Type
    = CInt
    | CFloat
    | V2
    | V3
    | V4
    | M4
    | Texture



-- TO BUILDER


toJsStringBuilder : Source -> String
toJsStringBuilder (Source src) =
  src



-- FROM CHARS


fromChars : String -> Source
fromChars chars =
    Source (escape chars)


escape : String -> String
escape chars =
    case String.uncons chars of
        Nothing ->
            ""

        Just ( c, cs ) ->
            if c == '\u{000D}' then
                escape cs

            else if c == '\n' then
                String.cons '\\' (String.cons 'n' (escape cs))

            else if c == '"' then
                String.cons '\\' (String.cons '"' (escape cs))

            else if c == '\'' then
                String.cons '\\' (String.cons '\'' (escape cs))

            else if c == '\\' then
                String.cons '\\' (String.cons '\\' (escape cs))

            else
                String.cons c (escape cs)



-- BINARY


bSource : B.Binary Source
bSource =
    B.bin1 Source (\(Source a) -> a) B.bString
