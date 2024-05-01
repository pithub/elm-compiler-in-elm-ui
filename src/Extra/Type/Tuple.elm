module Extra.Type.Tuple exposing (mappend, mempty, traverseSecond)

import Extra.Class.Functor as Functor
import Extra.Class.Monoid as Monoid
import Extra.Class.Traversable as Traversable
import Time exposing (Weekday(..))


mappend : Monoid.Mappend a -> Monoid.Mappend b -> Monoid.Mappend ( a, b )
mappend pMappendA pMappendB ( a1, b1 ) ( a2, b2 ) =
    ( pMappendA a1 a2, pMappendB b1 b2 )


mempty : Monoid.Mempty a -> Monoid.Mempty b -> Monoid.Mempty ( a, b )
mempty ma mb =
    ( ma, mb )


traverseSecond :
    Functor.Fmap b fb ( x, b ) fxb
    -> Traversable.Traverse a ( x, a ) fb fxb
traverseSecond pFmap f ( x, a ) =
    pFmap (\b -> ( x, b )) (f a)
