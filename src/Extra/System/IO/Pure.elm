module Extra.System.IO.Pure exposing
    ( IO
    , andMap
    , andThen
    , bind
    , fmap
    , liftA2
    , liftM
    , liftM2
    , liftM3
    , liftS
    , performIO
    , pure
    , return
    , when
    )

-- pure IO implemented as the State monad with an explicit state type

import Extra.Class.Applicative as Applicative
import Extra.Class.Functor as Functor
import Extra.Class.Monad as Monad


type alias IO s a =
    s -> ( a, s )


andMap : Applicative.AndMap (IO s a) (IO s (a -> b)) (IO s b)
andMap =
    Applicative.andMap liftA2


andThen : Monad.AndThen a (IO s a) (IO s b)
andThen =
    Monad.andThen bind


bind : Monad.Bind a (IO s a) (IO s b)
bind ma callback s0 =
    let
        ( a, s1 ) =
            ma s0
    in
    callback a s1


fmap : Functor.Fmap a (IO s a) b (IO s b)
fmap f ma s0 =
    let
        ( a, s1 ) =
            ma s0
    in
    ( f a, s1 )


liftA2 : Applicative.LiftA2 a (IO s a) b (IO s b) c (IO s c)
liftA2 f ma mb s0 =
    let
        ( a, s1 ) =
            ma s0

        ( b, s2 ) =
            mb s1
    in
    ( f a b, s2 )


liftM : (a -> b) -> IO s a -> IO s b
liftM f ma =
    bind ma (\a -> return (f a))


liftM2 : (a -> b -> c) -> IO s a -> IO s b -> IO s c
liftM2 f ma mb =
    bind ma (\a -> bind mb (\b -> return (f a b)))


liftM3 : (a -> b -> c -> d) -> IO s a -> IO s b -> IO s c -> IO s d
liftM3 f ma mb mc =
    bind ma (\a -> bind mb (\b -> bind mc (\c -> return (f a b c))))


liftS : (s2 -> s1) -> (s1 -> s2 -> s2) -> IO s1 a -> IO s2 a
liftS getter setter state1 s2 =
    let
        ( a, s1 ) =
            state1 (getter s2)
    in
    ( a, setter s1 s2 )


performIO : IO s a -> s -> a
performIO ma s =
    Tuple.first (ma s)


pure : Applicative.Pure a (IO s a)
pure a s =
    ( a, s )


return : Monad.Return a (IO s a)
return =
    pure


when : Applicative.When (IO s ())
when =
    Applicative.when pure
