module Extra.Data.Graph exposing
    ( SCC(..)
    , stronglyConnComp
    )

-- Ported from https://hackage.haskell.org/package/containers-0.7/docs/Data-Graph.html

import Array
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Maybe as ME
import Extra.Type.Set as Set



-- STRONGLY CONNECTED COMPONENTS


type SCC vertex
    = AcyclicSCC vertex
    | CyclicSCC (TList vertex)


stronglyConnComp : TList ( node, comparable, TList comparable ) -> TList (SCC node)
stronglyConnComp edges0 =
    let
        get_node scc_ =
            case scc_ of
                AcyclicSCC ( n, _, _ ) ->
                    AcyclicSCC n

                CyclicSCC triples ->
                    CyclicSCC (MList.map (\( n, _, _ ) -> n) triples)
    in
    MList.map get_node (stronglyConnCompR edges0)


stronglyConnCompR :
    TList ( node, comparable, TList comparable )
    -- ^ The graph: a list of nodes uniquely identified by keys,
    -- with a list of keys of nodes this node has edges to.
    -- The out-list may contain keys that don't correspond to
    -- nodes of the graph; such edges are ignored.
    -> TList (SCC ( node, comparable, TList comparable )) -- ^ Reverse topologically sorted
stronglyConnCompR edges0 =
    case edges0 of
        [] ->
            []

        _ ->
            let
                ( graph, vertex_fn, _ ) =
                    graphFromEdges edges0

                forest =
                    scc graph

                decode (Node v ts) =
                    case ts of
                        [] ->
                            if mentions_itself v then
                                CyclicSCC [ vertex_fn v ]

                            else
                                AcyclicSCC (vertex_fn v)

                        _ ->
                            CyclicSCC (vertex_fn v :: MList.foldr dec [] ts)

                dec (Node v ts) vs =
                    vertex_fn v :: MList.foldr dec vs ts

                mentions_itself v =
                    MList.elem v (unsafeGet v graph)
            in
            MList.map decode forest



-- GRAPHS


type alias Vertex =
    -- | Abstract representation of vertices.
    Int


type alias Graph =
    -- | Adjacency list representation of a graph, mapping each vertex to its
    -- list of successors.
    Array.Array (TList Vertex)


type alias Bounds =
    ( Vertex, Vertex )


type alias Edge =
    ( Vertex, Vertex )


bounds : Graph -> Bounds
bounds g =
    ( 0, Array.length g - 1 )


vertices : Graph -> TList Vertex
vertices graph =
    MList.range 0 (Array.length graph - 1)


edges : Graph -> TList Edge
edges g =
    g |> Array.toIndexedList |> MList.concatMap (\( v, ws ) -> MList.map (\w -> ( v, w )) ws)


buildG : ( Vertex, Vertex ) -> TList Edge -> Graph
buildG ( start, end ) edges_ =
    MList.foldl (\a ( v, s ) -> Array.set v (s :: unsafeGet v a) a) (Array.repeat (end - start + 1) []) edges_


transposeG : Graph -> Graph
transposeG g =
    buildG (bounds g) (reverseE g)


reverseE : Graph -> TList Edge
reverseE g =
    g |> edges |> MList.map (\( v, w ) -> ( w, v ))


graphFromEdges :
    TList ( node, comparable, TList comparable )
    ->
        ( Graph
        , Vertex -> ( node, comparable, TList comparable )
        , comparable -> Maybe Vertex
        )
graphFromEdges edges0 =
    let
        max_v : Int
        max_v =
            MList.length edges0 - 1

        sorted_edges =
            MList.sortBy lt edges0

        graph =
            sorted_edges |> MList.map (\( _, _, ks ) -> ME.mapMaybe key_vertex ks) |> Array.fromList

        key_map =
            sorted_edges |> MList.map (\( _, k, _ ) -> k) |> Array.fromList

        vertex_map =
            sorted_edges |> Array.fromList

        lt ( _, k1, _ ) ( _, k2, _ ) =
            compare k1 k2

        -- key_vertex : key -> Maybe Vertex
        --  returns Nothing for non-interesting vertices
        key_vertex k =
            let
                findVertex a b =
                    if a > b then
                        Nothing

                    else
                        let
                            mid =
                                a + (b - a) // 2
                        in
                        case compare k (unsafeGet mid key_map) of
                            LT ->
                                findVertex a (mid - 1)

                            EQ ->
                                Just mid

                            GT ->
                                findVertex (mid + 1) b
            in
            findVertex 0 max_v
    in
    ( graph, \v -> unsafeGet v vertex_map, key_vertex )


unsafeGet : Int -> Array.Array a -> a
unsafeGet index array =
    case Array.get index array of
        Just value ->
            value

        Nothing ->
            Debug.todo "Array.get failed"



-- DEPTH FIRST SEARCH


dff : Graph -> TList (Tree Vertex)
dff g =
    dfs g (vertices g)


dfs : Graph -> TList Vertex -> TList (Tree Vertex)
dfs g vs0 =
    let
        go : Set.Set Vertex -> TList Vertex -> ( Set.Set Vertex, TList (Tree Vertex) )
        go visited vertexes =
            case vertexes of
                [] ->
                    ( visited, [] )

                v :: vs ->
                    if Set.member v visited then
                        go visited vs

                    else
                        let
                            ( visited1, as_ ) =
                                go (Set.insert v visited) (unsafeGet v g)

                            ( visited2, bs_ ) =
                                go visited1 vs
                        in
                        ( visited2, Node v as_ :: bs_ )
    in
    go Set.empty vs0 |> Tuple.second



-- ALGORITHMS


postorder : Tree a -> TList a -> TList a
postorder (Node a ts) =
    postorderF ts << (::) a


postorderF : TList (Tree a) -> TList a -> TList a
postorderF ts =
    MList.foldr (<<) identity <| MList.map postorder ts


postOrd : Graph -> TList Vertex
postOrd g =
    postorderF (dff g) []


scc : Graph -> TList (Tree Vertex)
scc g =
    dfs g (MList.reverse (postOrd (transposeG g)))



-- TREE


type Tree a
    = Node a (TList (Tree a))
