{- MANUALLY FORMATTED -}
module Compiler.Optimize.Names exposing
  ( Tracker
  , run
  , generate
  , registerKernel
  , registerGlobal
  , registerDebug
  , registerCtor
  , registerField
  , registerFieldDict
  , registerFieldList
  , fmap, pure, andMap, liftA2, return, bind, andThen
  )


import Compiler.AST.Canonical as Can
import Compiler.AST.Optimized as Opt
import Compiler.Data.Index as Index
import Compiler.Data.Name as Name
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Reporting.Annotation as A
import Extra.Class.Applicative as Applicative
import Extra.Class.Functor as Functor
import Extra.Class.Monad as Monad
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map
import Extra.Type.Set as Set
import Compiler.AST.Optimized exposing (toGlobalComparable)



-- GENERATOR


type Tracker z a =
  Tracker (
      Int
      -> Set.Set Opt.GlobalComparable
      -> Map.Map Name.Name Int
      -> (Int -> Set.Set Opt.GlobalComparable -> Map.Map Name.Name Int -> a -> z)
      -> z
  )


run : Tracker (Set.Set Opt.GlobalComparable, Map.Map Name.Name Int, a) a -> (Set.Set Opt.GlobalComparable, Map.Map Name.Name Int, a)
run (Tracker k) =
  k 0 Set.empty Map.empty
    (\_ deps fields value -> (deps, fields, value))


generate : Tracker z Name.Name
generate =
  Tracker <| \uid deps fields ok ->
    ok (uid + 1) deps fields (Name.fromVarIndex uid)


registerKernel : Name.Name -> a -> Tracker z a
registerKernel home value =
  Tracker <| \uid deps fields ok ->
    ok uid (Set.insert (Opt.toGlobalComparable <| Opt.toKernelGlobal home) deps) fields value


registerGlobal : ModuleName.Canonical -> Name.Name -> Tracker z Opt.Expr
registerGlobal home name =
  Tracker <| \uid deps fields ok ->
    let global = Opt.Global home name in
    ok uid (Set.insert (toGlobalComparable global) deps) fields (Opt.VarGlobal global)


registerDebug : Name.Name -> ModuleName.Canonical -> A.Region -> Tracker z Opt.Expr
registerDebug name home region =
  Tracker <| \uid deps fields ok ->
    let global = Opt.Global ModuleName.debug name in
    ok uid (Set.insert (toGlobalComparable global) deps) fields (Opt.VarDebug name home region Nothing)


registerCtor : ModuleName.Canonical -> Name.Name -> Index.ZeroBased -> Can.CtorOpts -> Tracker z Opt.Expr
registerCtor home name index opts =
  Tracker <| \uid deps fields ok ->
    let
      global = Opt.Global home name
      newDeps = Set.insert (toGlobalComparable global) deps
    in
    case opts of
      Can.Normal ->
        ok uid newDeps fields (Opt.VarGlobal global)

      Can.Enum ->
        ok uid newDeps fields <|
          let
            otherwise () = Opt.VarEnum global index
          in
          case name of
            "True"  -> if home == ModuleName.basics then Opt.CBool True  else otherwise ()
            "False" -> if home == ModuleName.basics then Opt.CBool False else otherwise ()
            _ -> otherwise ()

      Can.Unbox ->
        ok uid (Set.insert (toGlobalComparable identity_) newDeps) fields (Opt.VarBox global)


identity_ : Opt.Global
identity_ =
  Opt.Global ModuleName.basics Name.identity_


registerField : Name.Name -> a -> Tracker z a
registerField name value =
  Tracker <| \uid d fields ok ->
    ok uid d (Map.insertWith (+) name 1 fields) value


registerFieldDict : Map.Map Name.Name v -> a -> Tracker z a
registerFieldDict newFields value =
  Tracker <| \uid d fields ok ->
    ok uid d (Map.unionWith (+) fields (Map.map toOne newFields)) value


toOne : a -> Int
toOne _ = 1


registerFieldList : TList Name.Name -> a -> Tracker z a
registerFieldList names value =
  Tracker <| \uid deps fields ok ->
    ok uid deps (MList.foldr addOne fields names) value


addOne : Name.Name -> Map.Map Name.Name Int -> Map.Map Name.Name Int
addOne name fields =
  Map.insertWith (+) name 1 fields



-- INSTANCES


fmap : Functor.Fmap a (Tracker z a) b (Tracker z b)
fmap func (Tracker kv) =
    Tracker <| \n d f ok ->
      let
        ok1 n1 d1 f1 value =
          ok n1 d1 f1 (func value)
      in
      kv n d f ok1


pure : Applicative.Pure a (Tracker z a)
pure value =
    Tracker <| \n d f ok -> ok n d f value


andMap : Applicative.AndMap (Tracker z a) (Tracker z (a -> b)) (Tracker z b)
andMap (Tracker kv) (Tracker kf) =
    Tracker <| \n d f ok ->
      let
        ok1 n1 d1 f1 func =
          let
            ok2 n2 d2 f2 value =
              ok n2 d2 f2 (func value)
          in
          kv n1 d1 f1 ok2
      in
      kf n d f ok1


liftA2 : Applicative.LiftA2 a (Tracker z a) b (Tracker z b) c (Tracker z c)
liftA2 =
    Applicative.liftA2 fmap andMap


return : Monad.Return a (Tracker z a)
return =
    pure


bind : Monad.Bind a (Tracker z a) (Tracker z b)
bind (Tracker k) callback =
    Tracker <| \n d f ok ->
      let
        ok1 n1 d1 f1 a =
          case callback a of
            Tracker kb -> kb n1 d1 f1 ok
      in
      k n d f ok1


andThen : Monad.AndThen a (Tracker z a) (Tracker z b)
andThen =
    Monad.andThen bind
