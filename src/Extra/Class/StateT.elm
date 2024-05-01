module Extra.Class.StateT exposing
    ( StateT
    , andMap
    , bind
    , evalStateT
    , fmap
    , gets
    , lift
    , liftA2
    , modify
    , pure
    , put
    , return
    )

import Extra.Class.Applicative as Applicative
import Extra.Class.Functor as Functor
import Extra.Class.Monad as Monad


type alias StateT s mas =
    s -> mas


andMap :
    Functor.Fmap ( a, s ) mas ( b, s ) mbs
    -> Monad.Bind ( a -> b, s ) mfs mbs
    -> Applicative.AndMap (StateT s mas) (StateT s mfs) (StateT s mbs)
andMap pFmap pBind sa sf s0 =
    pBind (sf s0) <|
        \( f, s1 ) ->
            pFmap (\( a, s2 ) -> ( f a, s2 )) (sa s1)


bind :
    Monad.Bind ( a, s ) mas mbs
    -> Monad.Bind a (StateT s mas) (StateT s mbs)
bind pBind sa callback s0 =
    pBind (sa s0) (\( a, s1 ) -> callback a s1)


evalStateT :
    Functor.Fmap ( a, s ) mas a ma
    -> (StateT s mas -> s -> ma)
evalStateT pFmap sa s =
    pFmap Tuple.first (sa s)


fmap :
    Functor.Fmap ( a, s ) mas ( b, s ) mbs
    -> Functor.Fmap a (StateT s mas) b (StateT s mbs)
fmap pFmap f sa s0 =
    pFmap (\( a, s1 ) -> ( f a, s1 )) (sa s0)


gets :
    Monad.Return ( a, s ) mas
    -> ((s -> a) -> StateT s mas)
gets pReturn f s =
    pReturn ( f s, s )


lift :
    Functor.Fmap a ma ( a, s ) mas
    -> (ma -> StateT s mas)
lift pFmap m s =
    pFmap (\a -> ( a, s )) m


liftA2 :
    Functor.Fmap ( a, s ) mas ( b -> c, s ) mfs
    -> Functor.Fmap ( b, s ) mbs ( c, s ) mcs
    -> Monad.Bind ( b -> c, s ) mfs mcs
    -> Applicative.LiftA2 a (StateT s mas) b (StateT s mbs) c (StateT s mcs)
liftA2 pFmap1 pFmap2 pBind =
    Applicative.liftA2 (fmap pFmap1) (andMap pFmap2 pBind)


modify :
    Monad.Return ( (), s ) mus
    -> ((s -> s) -> StateT s mus)
modify pReturn f s =
    pReturn ( (), f s )


pure :
    Monad.Return ( a, s ) mas
    -> Applicative.Pure a (StateT s mas)
pure pReturn a s =
    pReturn ( a, s )


put :
    Monad.Return ( (), s ) mus
    -> (s -> StateT s mus)
put pReturn s =
    state pReturn (\_ -> ( (), s ))


return :
    Monad.Return ( a, s ) mas
    -> Monad.Return a (StateT s mas)
return =
    pure


state :
    Monad.Return ( a, s ) mas
    -> ((s -> ( a, s )) -> StateT s mas)
state pReturn f s =
    pReturn (f s)
