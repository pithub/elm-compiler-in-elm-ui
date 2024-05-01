module Compiler.Parse.Primitives exposing
    ( Col
    , Parser(..)
    , Row
    , Snippet(..)
    , State(..)
    , addChomped
    , addEnd
    , addLocation
    , bind
    , do
    , do1
    , doAndBind
    , doN
    , fmap
    , fromByteString
    , fromSnippet
    , getCharWidth
    , getPosition
    , inContext
    , isWord
    , oneOf
    , oneOfWithFallback
    , return
    , rmap
    , specialize
    , unsafeIndex
    , withBacksetIndent
    , withIndent
    , word1
    , word2
    )

-- TODO Explore possible optimizations (for example in the Keyword module, but also general String handling)

import Compiler.Reporting.Annotation as A
import Extra.Type.Either exposing (Either(..))
import Extra.Type.List exposing (TList)



-- PARSER


type Parser z x a
    = Parser
        (State
         -> (a -> State -> z) -- consumed ok
         -> (a -> State -> z) -- empty ok
         -> (Row -> Col -> (Row -> Col -> x) -> z) -- consumed err
         -> (Row -> Col -> (Row -> Col -> x) -> z) -- empty err
         -> z
        )


type State
    = State
        --{ src : String
        --, pos : Int
        --, end : Int
        --, indent : Int
        --, row : Row
        --, col : Col
        --}
        String
        Int
        Int
        Int
        Row
        Col


type alias Row =
    Int


type alias Col =
    Int



-- FUNCTOR


fmap : (a -> b) -> Parser z x a -> Parser z x b
fmap f (Parser parser) =
    Parser <|
        \state cok eok cerr eerr ->
            let
                cok_ a s =
                    cok (f a) s

                eok_ a s =
                    eok (f a) s
            in
            parser state cok_ eok_ cerr eerr


{- NEW: rmap -}
rmap : Parser z x a -> (a -> b) -> Parser z x b
rmap p f =
    fmap f p



-- ONE OF


oneOf : (Row -> Col -> x) -> TList (Parser z x a) -> Parser z x a
oneOf toError parsers =
    Parser <|
        \state cok eok cerr eerr ->
            oneOfHelp state cok eok cerr eerr toError parsers


oneOfHelp :
    State
    -> (a -> State -> z)
    -> (a -> State -> z)
    -> (Row -> Col -> (Row -> Col -> x) -> z)
    -> (Row -> Col -> (Row -> Col -> x) -> z)
    -> (Row -> Col -> x)
    -> TList (Parser z x a)
    -> z
oneOfHelp state cok eok cerr eerr toError parsers =
    case parsers of
        (Parser parser) :: parsers_ ->
            let
                eerr_ _ _ _ =
                    oneOfHelp state cok eok cerr eerr toError parsers_
            in
            parser state cok eok cerr eerr_

        [] ->
            let
                (State _ _ _ _ row col) =
                    state
            in
            eerr row col toError



-- ONE OF WITH FALLBACK


oneOfWithFallback : TList (Parser z x a) -> a -> Parser z x a
oneOfWithFallback parsers fallback =
    Parser <|
        \state cok eok cerr _ ->
            oowfHelp state cok eok cerr parsers fallback


oowfHelp :
    State
    -> (a -> State -> z)
    -> (a -> State -> z)
    -> (Row -> Col -> (Row -> Col -> x) -> z)
    -> TList (Parser z x a)
    -> a
    -> z
oowfHelp state cok eok cerr parsers fallback =
    case parsers of
        [] ->
            eok fallback state

        (Parser parser) :: parsers_ ->
            let
                eerr_ _ _ _ =
                    oowfHelp state cok eok cerr parsers_ fallback
            in
            parser state cok eok cerr eerr_



-- MONAD


return : a -> Parser z x a
return value =
    Parser <|
        \state _ eok _ _ ->
            eok value state


bind : Parser z x a -> (a -> Parser z x b) -> Parser z x b
bind (Parser parserA) callback =
    Parser <|
        \state cok eok cerr eerr ->
            let
                cok_ a s =
                    case callback a of
                        Parser parserB ->
                            parserB s cok cok cerr cerr

                eok_ a s =
                    case callback a of
                        Parser parserB ->
                            parserB s cok eok cerr eerr
            in
            parserA state cok_ eok_ cerr eerr


doAndBind : TList (Parser z x ()) -> Parser z x a -> (a -> Parser z x b) -> Parser z x b
doAndBind parsers parserA callback =
    doN parsers (bind parserA callback)


doN : TList (Parser z x ()) -> Parser z x a -> Parser z x a
doN parsers parser2 =
    let
        combine : TList (Parser z x ()) -> Parser z x ()
        combine parsers_ =
            case parsers_ of
                [] ->
                    return ()

                [ parser ] ->
                    parser

                parser :: remaining ->
                    do1 parser (combine remaining)
    in
    do1 (combine parsers) parser2


do1 : Parser z x a -> Parser z x b -> Parser z x b
do1 parser1 parser2 =
    bind parser1 (\_ -> parser2)


do : Parser z x a -> Parser z x ()
do parser =
    fmap (\_ -> ()) parser



-- FROM BYTESTRING


fromByteString : Parser (Either x a) x a -> (Row -> Col -> x) -> String -> Either x a
fromByteString (Parser parser) toBadEnd string =
    let
        toOk_ =
            toOk toBadEnd

        end =
            String.length string
    in
    parser (State string 0 end 0 1 1) toOk_ toOk_ toErr toErr


toOk : (Row -> Col -> x) -> a -> State -> Either x a
toOk toBadEnd a (State _ pos end _ row col) =
    if pos == end then
        Right a

    else
        Left (toBadEnd row col)


toErr : Row -> Col -> (Row -> Col -> x) -> Either x a
toErr row col toError =
    Left (toError row col)



-- FROM SNIPPET


type Snippet
    = Snippet
        --{ src   : String
        --, offset : Int
        --, length : Int
        --, offRow : Row
        --, offCol : Col
        --}
        String
        Int
        Int
        Row
        Col


fromSnippet : Parser (Either x a) x a -> (Row -> Col -> x) -> Snippet -> Either x a
fromSnippet (Parser parser) toBadEnd (Snippet fptr offset length row col) =
    let
        toOk_ =
            toOk toBadEnd

        pos =
            offset

        end =
            pos + length
    in
    parser (State fptr pos end 0 row col) toOk_ toOk_ toErr toErr



-- POSITION


getPosition : Parser z x A.Position
getPosition =
    Parser <|
        \((State _ _ _ _ row col) as state) _ eok _ _ ->
            eok (A.Position row col) state


addLocation : Parser z x a -> Parser z x (A.Located a)
addLocation (Parser parser) =
    Parser <|
        \((State _ _ _ _ sr sc) as state) cok eok cerr eerr ->
            let
                cok_ a ((State _ _ _ _ er ec) as s) =
                    cok (A.At (A.Region (A.Position sr sc) (A.Position er ec)) a) s

                eok_ a ((State _ _ _ _ er ec) as s) =
                    eok (A.At (A.Region (A.Position sr sc) (A.Position er ec)) a) s
            in
            parser state cok_ eok_ cerr eerr


addEnd : A.Position -> a -> Parser z x (A.Located a)
addEnd start value =
    Parser <|
        \((State _ _ _ _ row col) as state) _ eok _ _ ->
            eok (A.at start (A.Position row col) value) state



-- CHOMPED


addChomped : Parser z x a -> Parser z x ( a, String )
addChomped (Parser parser) =
    {- NEW: addChomped -}
    Parser <|
        \((State src so _ _ _ _) as state) cok eok cerr eerr ->
            let
                cok_ a ((State _ eo _ _ _ _) as s) =
                    cok ( a, String.slice so eo src ) s

                eok_ a ((State _ eo _ _ _ _) as s) =
                    eok ( a, String.slice so eo src ) s
            in
            parser state cok_ eok_ cerr eerr



-- INDENT


withIndent : Parser z x a -> Parser z x a
withIndent (Parser parser) =
    Parser <|
        \(State src pos end oldIndent row col) cok eok cerr eerr ->
            let
                cok_ a (State s p e _ r c) =
                    cok a (State s p e oldIndent r c)

                eok_ a (State s p e _ r c) =
                    eok a (State s p e oldIndent r c)
            in
            parser (State src pos end col row col) cok_ eok_ cerr eerr


withBacksetIndent : Int -> Parser z x a -> Parser z x a
withBacksetIndent backset (Parser parser) =
    Parser <|
        \(State src pos end oldIndent row col) cok eok cerr eerr ->
            let
                cok_ a (State s p e _ r c) =
                    cok a (State s p e oldIndent r c)

                eok_ a (State s p e _ r c) =
                    eok a (State s p e oldIndent r c)
            in
            parser (State src pos end (col - backset) row col) cok_ eok_ cerr eerr



-- CONTEXT


inContext : (x -> Row -> Col -> y) -> Parser z y start -> Parser z x a -> Parser z y a
inContext addContext (Parser parserStart) (Parser parserA) =
    Parser <|
        \((State _ _ _ _ row col) as state) cok eok cerr eerr ->
            let
                cerrA r c tx =
                    cerr row col (addContext (tx r c))

                eerrA r c tx =
                    eerr row col (addContext (tx r c))

                cokS _ s =
                    parserA s cok cok cerrA cerrA

                eokS _ s =
                    parserA s cok eok cerrA eerrA
            in
            parserStart state cokS eokS cerr eerr


specialize : (x -> Row -> Col -> y) -> Parser z x a -> Parser z y a
specialize addContext (Parser parser) =
    Parser <|
        \((State _ _ _ _ row col) as state) cok eok cerr eerr ->
            let
                cerr_ r c tx =
                    cerr row col (addContext (tx r c))

                eerr_ r c tx =
                    eerr row col (addContext (tx r c))
            in
            parser state cok eok cerr_ eerr_



-- SYMBOLS


word1 : Int -> (Row -> Col -> x) -> Parser z x ()
word1 word toError =
    Parser <|
        \(State src pos end indent row col) cok _ _ eerr ->
            if pos < end && unsafeIndex src pos == word then
                let
                    newState =
                        State src (pos + 1) end indent row (col + 1)
                in
                cok () newState

            else
                eerr row col toError


word2 : Int -> Int -> (Row -> Col -> x) -> Parser z x ()
word2 w1 w2 toError =
    Parser <|
        \(State src pos end indent row col) cok _ _ eerr ->
            let
                pos1 =
                    pos + 1
            in
            if pos1 < end && unsafeIndex src pos == w1 && unsafeIndex src pos1 == w2 then
                let
                    newState =
                        State src (pos + 2) end indent row (col + 2)
                in
                cok () newState

            else
                eerr row col toError



-- LOW-LEVEL CHECKS


unsafeIndex : String -> Int -> Int
unsafeIndex src pos =
    src
        |> String.dropLeft pos
        |> String.uncons
        |> Maybe.map (Tuple.first >> Char.toCode)
        |> Maybe.withDefault 0


isWord : String -> Int -> Int -> Int -> Bool
isWord src pos end word =
    pos < end && unsafeIndex src pos == word


getCharWidth : Int -> Int
getCharWidth word =
    if word > 0xFFFF then
        2

    else
        1
