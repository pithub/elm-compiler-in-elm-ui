module Extra.Class.Monoid exposing
    ( Mappend
    , Mempty
    )


type alias Mempty a =
    a


type alias Mappend a =
    a -> a -> a
