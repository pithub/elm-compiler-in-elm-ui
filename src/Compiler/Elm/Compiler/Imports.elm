module Compiler.Elm.Compiler.Imports exposing (defaults)

import Compiler.AST.Source as Src
import Compiler.Data.Name as Name
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Reporting.Annotation as A
import Extra.Type.List exposing (TList)



-- DEFAULTS


defaults : TList Src.Import
defaults =
    [ import_ ModuleName.basics Nothing Src.Open
    , import_ ModuleName.debug Nothing closed
    , import_ ModuleName.list Nothing (operator "::")
    , import_ ModuleName.maybe Nothing (typeOpen Name.maybe)
    , import_ ModuleName.result Nothing (typeOpen Name.result)
    , import_ ModuleName.string Nothing (typeClosed Name.string)
    , import_ ModuleName.char Nothing (typeClosed Name.char)
    , import_ ModuleName.tuple Nothing closed
    , import_ ModuleName.platform Nothing (typeClosed Name.program)
    , import_ ModuleName.cmd (Just Name.cmd) (typeClosed Name.cmd)
    , import_ ModuleName.sub (Just Name.sub) (typeClosed Name.sub)
    ]


import_ : ModuleName.Canonical -> Maybe Name.Name -> Src.Exposing -> Src.Import
import_ (ModuleName.Canonical _ name) maybeAlias exposing_ =
    Src.Import (A.At A.zero name) maybeAlias exposing_



-- EXPOSING


closed : Src.Exposing
closed =
    Src.Explicit []


typeOpen : Name.Name -> Src.Exposing
typeOpen name =
    Src.Explicit [ Src.Upper (A.At A.zero name) (Src.Public A.zero) ]


typeClosed : Name.Name -> Src.Exposing
typeClosed name =
    Src.Explicit [ Src.Upper (A.At A.zero name) Src.Private ]


operator : Name.Name -> Src.Exposing
operator op =
    Src.Explicit [ Src.Operator A.zero op ]
