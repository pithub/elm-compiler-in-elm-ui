module Extra.Class.Traversable exposing
    ( MapM
    , SequenceA
    , Traverse
    , sequenceA
    )


type alias MapM a ta fb ftb =
    Traverse a ta fb ftb


type alias Traverse a ta fb ftb =
    (a -> fb) -> ta -> ftb


type alias SequenceA tfa fta =
    tfa -> fta


sequenceA :
    Traverse fa tfa fa fta
    -> SequenceA tfa fta
sequenceA pTraverse =
    pTraverse identity
