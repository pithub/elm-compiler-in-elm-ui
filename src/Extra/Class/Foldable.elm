module Extra.Class.Foldable exposing
    ( All
    , Elem
    , Filter
    , Foldl
    , FoldlM
    , Foldr
    , FoldrM
    , ForM_
    , Length
    , MapM_
    , NotElem
    , Null
    , Sequence_
    , Traverse_
    , foldlM
    , foldrM
    , forM_
    , mapM_
    , sequence_
    , traverse_
    )

import Extra.Class.Applicative as Applicative
import Extra.Class.Monad as Monad


type alias FoldlM a ta b mb =
    (b -> a -> mb) -> b -> ta -> mb


foldlM :
    Foldr a ta (b -> mb)
    -> Monad.Return b mb
    -> Monad.Bind b mb mb
    -> FoldlM a ta b mb
foldlM pFoldr pReturn pBind f z0 xs =
    pFoldr (\x k z -> pBind (f z x) k) pReturn xs z0


type alias Foldr a ta b =
    (a -> b -> b) -> b -> ta -> b


type alias FoldrM a ta b mb =
    (a -> b -> mb) -> b -> ta -> mb


foldrM :
    Foldl a ta (b -> mb)
    -> Monad.Return b mb
    -> Monad.Bind b mb mb
    -> FoldrM a ta b mb
foldrM pFoldl pReturn pBind f z0 xs =
    pFoldl (\k x z -> pBind (f x z) k) pReturn xs z0


type alias Foldl a ta b =
    (b -> a -> b) -> b -> ta -> b


type alias ForM_ a ta mb mu =
    ta -> (a -> mb) -> mu


forM_ :
    Foldr a ta mu
    -> Monad.Return () mu
    -> Monad.Bind b mb mu
    -> ForM_ a ta mb mu
forM_ pFoldr pReturn pBind t f =
    mapM_ pFoldr pReturn pBind f t


type alias Elem a ta =
    a -> ta -> Bool


type alias MapM_ a ta mb mu =
    (a -> mb) -> ta -> mu


mapM_ :
    Foldr a ta mu
    -> Monad.Return () mu
    -> Monad.Bind b mb mu
    -> MapM_ a ta mb mu
mapM_ pFoldr pReturn pBind f t =
    pFoldr (\x k -> pBind (f x) (\_ -> k)) (pReturn ()) t


type alias NotElem a ta =
    a -> ta -> Bool


type alias Null ta =
    ta -> Bool


type alias Length ta =
    ta -> Int


type alias Traverse_ a ta fb fu =
    (a -> fb) -> ta -> fu


traverse_ :
    Applicative.Pure () fu
    -> Applicative.LiftA2 b fb () fu () fu
    -> Foldr a ta fu
    -> Traverse_ a ta fb fu
traverse_ pPure pLiftA2 pFoldr f t =
    pFoldr (pLiftA2 (\_ () -> ()) << f) (pPure ()) t


type alias Sequence_ tma mu =
    tma -> mu


sequence_ :
    Foldr mb tma mu
    -> Monad.Return () mu
    -> Monad.Bind b mb mu
    -> Sequence_ tma mu
sequence_ pFoldr pReturn pBind t =
    pFoldr (\m k -> pBind m (\_ -> k)) (pReturn ()) t


type alias All a ta =
    (a -> Bool) -> ta -> Bool


type alias Filter a ta =
    (a -> Bool) -> ta -> ta
