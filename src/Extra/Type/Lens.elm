module Extra.Type.Lens exposing
    ( Lens
    , combine
    , modify
    )


type alias Lens whole part =
    { getter : whole -> part
    , setter : part -> whole -> whole
    }


combine : Lens a b -> Lens b c -> Lens a c
combine lens1 lens2 =
    { getter = \a -> lens2.getter (lens1.getter a)
    , setter = \c a -> lens1.setter (lens2.setter c (lens1.getter a)) a
    }


modify : Lens a b -> (b -> b) -> a -> a
modify lens f a =
    lens.setter (f (lens.getter a)) a
