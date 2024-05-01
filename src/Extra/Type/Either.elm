module Extra.Type.Either exposing
    ( Either(..)
    , andThen
    , bind
    , fmap
    , join
    , lefts
    , liftA2
    , mapLeft
    , pure
    , sequenceA
    , traverse
    )

import Extra.Class.Applicative as Applicative
import Extra.Class.Functor as Functor
import Extra.Class.Monad as Monad
import Extra.Class.Traversable as Traversable
import Extra.Type.List as MList exposing (TList)


type Either a b
    = Left a
    | Right b


andThen : Monad.AndThen a (Either x a) (Either x b)
andThen callback ea =
    bind ea callback


bind : Monad.Bind a (Either x a) (Either x b)
bind ea callback =
    case ea of
        Left x ->
            Left x

        Right a ->
            callback a


fmap : Functor.Fmap a (Either x a) b (Either x b)
fmap f ea =
    case ea of
        Left x ->
            Left x

        Right a ->
            Right (f a)


join : Either x (Either x a) -> Either x a
join eea =
    bind eea identity


lefts : TList (Either a b) -> TList a
lefts eithers =
    MList.foldr
        (\either acc ->
            case either of
                Left x ->
                    x :: acc

                Right _ ->
                    acc
        )
        []
        eithers


liftA2 : Applicative.LiftA2 a (Either x a) b (Either x b) c (Either x c)
liftA2 f ea eb =
    case ea of
        Left x ->
            Left x

        Right a ->
            case eb of
                Left x ->
                    Left x

                Right b ->
                    Right (f a b)


mapLeft : (x -> y) -> Either x a -> Either y a
mapLeft f ea =
    case ea of
        Left x ->
            Left (f x)

        Right a ->
            Right a


pure : Applicative.Pure a (Either x a)
pure =
    Right


sequenceA :
    Applicative.Pure (Either x a) fea
    -> Functor.Fmap a fa (Either x a) fea
    -> Traversable.SequenceA (Either x fa) fea
sequenceA pPure pFmap mfa =
    Traversable.sequenceA (traverse pPure pFmap) mfa


traverse :
    Applicative.Pure (Either x b) feb
    -> Functor.Fmap b fb (Either x b) feb
    -> Traversable.Traverse a (Either x a) fb feb
traverse pPure pFmap f ea =
    case ea of
        Left x ->
            pPure (Left x)

        Right a ->
            pFmap Right (f a)
