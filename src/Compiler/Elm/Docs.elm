{- MANUALLY FORMATTED -}
module Compiler.Elm.Docs exposing
  ( Documentation
  , Module(..)
  , fromModule
  --, Union(..)
  --, Alias(..)
  --, Value(..)
  --, Binop(..)
  --, Binop.Associativity(..)
  --, Binop.Precedence(..)
  --, Error(..)
  --, decoder
  , encode
  )


import Compiler.AST.Canonical as Can
import Compiler.AST.Utils.Binop as Binop
import Compiler.Elm.Compiler.Type as Type
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Data.Name as Name
import Compiler.Json.Encode as JE
import Compiler.Json.String as Json
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Docs as E
import Extra.Type.Either exposing (Either(..))
import Extra.Type.List exposing (TList)
import Extra.Type.Map as Map



-- DOCUMENTATION


type alias Documentation =
  Map.Map Name.Name Module


type Module =
  Module
    {- name -} Name.Name
    {- comment -} Comment
    {- unions -} (Map.Map Name.Name Union)
    {- aliases -} (Map.Map Name.Name Alias)
    {- values -} (Map.Map Name.Name Value)
    {- binops -} (Map.Map Name.Name Binop)

type alias Comment = Json.TString

type Alias = Alias Comment (TList Name.Name) Type.Type
type Union = Union Comment (TList Name.Name) (TList (Name.Name, TList Type.Type))
type Value = Value Comment Type.Type
type Binop = Binop Comment Type.Type Binop.Associativity Binop.Precedence



-- FROM MODULE


fromModule : Can.Module -> Either E.Error Module
fromModule _ =
  -- TODO: Compiler.Elm.Docs.fromModule
  Left (E.NoDocs A.zero)



-- JSON


encode : Documentation -> JE.Value
encode docs =
  JE.list encodeModule (Map.elems docs)


encodeModule : Module -> JE.Value
encodeModule (Module name comment unions aliases values binops) =
  JE.object <|
    [ ( "name", ModuleName.encode name )
    , ( "comment", JE.string comment )
    , ( "unions", JE.list encodeUnion (Map.toList unions) )
    , ( "aliases", JE.list encodeAlias (Map.toList aliases) )
    , ( "values", JE.list encodeValue (Map.toList values) )
    , ( "binops", JE.list encodeBinop (Map.toList binops) )
    ]



-- UNION JSON


encodeUnion : (Name.Name, Union) -> JE.Value
encodeUnion (name, Union comment args cases) =
  JE.object
    [ ( "name", JE.name name )
    , ( "comment", JE.string comment )
    , ( "args", JE.list JE.name args )
    , ( "cases", JE.list encodeCase cases )
    ]


encodeCase : ( Name.Name, TList Type.Type ) -> JE.Value
encodeCase ( tag, args ) =
  JE.list identity [ JE.name tag, JE.list Type.encode args ]



-- ALIAS JSON


encodeAlias : (Name.Name, Alias) -> JE.Value
encodeAlias ( name, Alias comment args tipe) =
  JE.object
    [ ( "name", JE.name name )
    , ( "comment", JE.string comment )
    , ( "args", JE.list JE.name args )
    , ( "type", Type.encode tipe )
    ]



-- VALUE JSON


encodeValue : (Name.Name, Value) -> JE.Value
encodeValue (name, Value comment tipe) =
  JE.object
    [ ( "name", JE.name name )
    , ( "comment", JE.string comment )
    , ( "type", Type.encode tipe )
    ]



-- BINOP JSON


encodeBinop : (Name.Name, Binop) -> JE.Value
encodeBinop (name, Binop comment tipe assoc prec) =
  JE.object
    [ ( "name", JE.name name )
    , ( "comment", JE.string comment )
    , ( "type", Type.encode tipe )
    , ( "associativity", encodeAssoc assoc )
    , ( "precedence", encodePrec prec )
    ]



-- ASSOCIATIVITY JSON


encodeAssoc : Binop.Associativity -> JE.Value
encodeAssoc assoc =
  case assoc of
    Binop.Left  -> JE.chars "left"
    Binop.Non   -> JE.chars "non"
    Binop.Right -> JE.chars "right"



-- PRECEDENCE JSON


encodePrec : Binop.Precedence -> JE.Value
encodePrec (Binop.Precedence n) =
  JE.int n
