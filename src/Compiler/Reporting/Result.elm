module Compiler.Reporting.Result exposing
    ( TResult(..)
    , andMap
    , andThen
    , bind
    , discard
    , fmap
    , foldM
    , liftA2
    , ok
    , pure
    , run
    , sequenceAMap
    , throw
    , traverseList
    , traverseMap
    , traverseWithKey
    , warn
    )

import Compiler.Data.OneOrMore as OneOrMore
import Compiler.Reporting.Warning as Warning
import Extra.Class.Applicative as Applicative
import Extra.Class.Functor as Functor
import Extra.Class.Monad as Monad
import Extra.Class.Traversable as Traversable
import Extra.Type.Either exposing (Either(..))
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map



-- RESULT


type TResult z info warnings error a
    = CResult
        (info
         -> warnings
         -> (info -> warnings -> OneOrMore.OneOrMore error -> z)
         -> (info -> warnings -> a -> z)
         -> z
        )


run : TResult ( TList w, Either (OneOrMore.OneOrMore e) a ) () (TList w) e a -> ( TList w, Either (OneOrMore.OneOrMore e) a )
run (CResult k) =
    k ()
        []
        (\() w e -> ( MList.reverse w, Left e ))
        (\() w a -> ( MList.reverse w, Right a ))



-- HELPERS


ok : a -> TResult z i w e a
ok a =
    CResult <|
        \i w _ good ->
            good i w a


warn : Warning.Warning -> TResult z i (TList Warning.Warning) e ()
warn warning =
    CResult <|
        \i warnings _ good ->
            good i (warning :: warnings) ()


throw : e -> TResult z i w e a
throw e =
    CResult <|
        \i w bad _ ->
            bad i w (OneOrMore.one e)



-- FUNCTOR


fmap : Functor.Fmap a (TResult z i w e a) b (TResult z i w e b)
fmap func (CResult k) =
    CResult <|
        \i w bad good ->
            let
                good1 i1 w1 value =
                    good i1 w1 (func value)
            in
            k i w bad good1



-- APPLICATIVE


pure : Applicative.Pure a (TResult z i w e a)
pure =
    ok


andMap : Applicative.AndMap (TResult z i w e a) (TResult z i w e (a -> b)) (TResult z i w e b)
andMap (CResult kv) (CResult kf) =
    CResult <|
        \i w bad good ->
            let
                bad1 i1 w1 e1 =
                    let
                        bad2 i2 w2 e2 =
                            bad i2 w2 (OneOrMore.more e1 e2)

                        good2 i2 w2 _ =
                            bad i2 w2 e1
                    in
                    kv i1 w1 bad2 good2

                good1 i1 w1 func =
                    let
                        bad2 i2 w2 e2 =
                            bad i2 w2 e2

                        good2 i2 w2 value =
                            good i2 w2 (func value)
                    in
                    kv i1 w1 bad2 good2
            in
            kf i w bad1 good1


liftA2 : Applicative.LiftA2 a (TResult z i w e a) b (TResult z i w e b) c (TResult z i w e c)
liftA2 =
    Applicative.liftA2 fmap andMap


discard : Applicative.DiscardFirst (TResult z i w e a) (TResult z i w e b)
discard =
    Applicative.discardFirst liftA2



-- MONAD


bind : Monad.Bind a (TResult z i w e a) (TResult z i w e b)
bind (CResult ka) callback =
    CResult <|
        \i w bad good ->
            let
                good1 i1 w1 a =
                    case callback a of
                        CResult kb ->
                            kb i1 w1 bad good
            in
            ka i w bad good1


andThen : Monad.AndThen a (TResult z i w e a) (TResult z i w e b)
andThen =
    Monad.andThen bind



-- FOLD


foldM : (b -> a -> TResult z i w e b) -> b -> TList a -> TResult z i w e b
foldM f z0 xs =
    let
        c x k z =
            bind (f z x) k
    in
    MList.foldr c pure xs z0



-- TRAVERSE


sequenceAMap : Traversable.SequenceA (Map.Map comparable (TResult z i w e a)) (TResult z i w e (Map.Map comparable a))
sequenceAMap =
    Map.sequenceA pure liftA2


traverseList : Traversable.Traverse a (TList a) (TResult z i w e b) (TResult z i w e (TList b))
traverseList =
    MList.traverse pure liftA2


traverseMap : Traversable.Traverse a (Map.Map comparable a) (TResult z i w e b) (TResult z i w e (Map.Map comparable b))
traverseMap =
    Map.traverse pure liftA2


traverseWithKey : (comparable -> a -> TResult z i w e b) -> Map.Map comparable a -> TResult z i w e (Map.Map comparable b)
traverseWithKey =
    Map.traverseWithKey pure liftA2
