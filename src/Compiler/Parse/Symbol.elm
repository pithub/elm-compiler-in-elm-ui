module Compiler.Parse.Symbol exposing
    ( BadOperator(..)
    , isBinopChar
    , operator
    )

import Compiler.Data.Name as Name
import Compiler.Parse.Primitives as P
import Extra.Type.List as MList
import Extra.Type.Set as Set exposing (Set)



-- OPERATOR


type BadOperator
    = BadDot
    | BadPipe
    | BadArrow
    | BadEquals
    | BadHasType


operator : (P.Row -> P.Col -> x) -> (BadOperator -> P.Row -> P.Col -> x) -> P.Parser z x Name.Name
operator toExpectation toError =
    P.Parser <|
        \(P.State src pos end indent row col) cok _ cerr eerr ->
            let
                newPos =
                    chompOps src pos end
            in
            if pos == newPos then
                eerr row col toExpectation

            else
                case String.slice pos newPos src of
                    "." ->
                        eerr row col (toError BadDot)

                    "|" ->
                        cerr row col (toError BadPipe)

                    "->" ->
                        cerr row col (toError BadArrow)

                    "=" ->
                        cerr row col (toError BadEquals)

                    ":" ->
                        cerr row col (toError BadHasType)

                    op ->
                        let
                            newCol =
                                col + (newPos - pos)

                            newState =
                                P.State src newPos end indent row newCol
                        in
                        cok op newState


chompOps : String -> Int -> Int -> Int
chompOps src pos end =
    if pos < end && isBinopCharHelp (P.unsafeIndex src pos) then
        chompOps src (pos + 1) end

    else
        pos


isBinopCharHelp : Int -> Bool
isBinopCharHelp word =
    word < 128 && Set.member word binopCharSet


binopCharSet : Set Int
binopCharSet =
    Set.fromList (MList.map Char.toCode (String.toList "+-/*=.<>:&|^?%!"))


isBinopChar : Char -> Bool
isBinopChar char =
    isBinopCharHelp (Char.toCode char)
