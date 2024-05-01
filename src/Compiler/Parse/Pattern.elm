module Compiler.Parse.Pattern exposing
    ( expression
    , term
    )

import Compiler.AST.Source as Src
import Compiler.Data.Name as Name
import Compiler.Data.Utf8 as Utf8
import Compiler.Parse.Keyword as Keyword
import Compiler.Parse.Number as Number
import Compiler.Parse.Primitives as P
import Compiler.Parse.Space as Space
import Compiler.Parse.String as MString
import Compiler.Parse.Variable as Var
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Syntax as E
import Extra.Type.List as MList exposing (TList)



-- TERM


term : P.Parser z E.Pattern Src.Pattern
term =
    P.bind P.getPosition <|
        \start ->
            P.oneOf E.PStart
                [ record start
                , tuple start
                , list start
                , termHelp start
                ]


termHelp : A.Position -> P.Parser z E.Pattern Src.Pattern
termHelp start =
    P.oneOf E.PStart
        [ P.do1 wildcard <|
            P.addEnd start Src.PAnything
        , P.bind (Var.lower E.PStart) <|
            \name ->
                P.addEnd start (Src.PVar name)
        , P.bind (Var.foreignUpper E.PStart) <|
            \upper ->
                P.bind P.getPosition <|
                    \end ->
                        let
                            region =
                                A.Region start end
                        in
                        P.return <|
                            A.at start end <|
                                case upper of
                                    Var.Unqualified name ->
                                        Src.PCtor region name []

                                    Var.Qualified home name ->
                                        Src.PCtorQual region home name []
        , P.bind (Number.number E.PStart E.PNumber) <|
            \number ->
                P.bind P.getPosition <|
                    \end ->
                        case number of
                            Number.CInt int ->
                                P.return (A.at start end (Src.PInt int))

                            Number.CFloat float ->
                                P.Parser <|
                                    \(P.State _ _ _ _ row col) _ _ cerr _ ->
                                        let
                                            width =
                                                Utf8.size float
                                        in
                                        cerr row (col - width) (E.PFloat width)
        , P.bind (MString.string E.PStart E.PString) <|
            \str ->
                P.addEnd start (Src.PStr str)
        , P.bind (MString.character E.PStart E.PChar) <|
            \chr ->
                P.addEnd start (Src.PChr chr)
        ]



-- WILDCARD


wildcard : P.Parser z E.Pattern ()
wildcard =
    P.Parser <|
        \(P.State src pos end indent row col) cok _ cerr eerr ->
            if pos == end || P.unsafeIndex src pos /= 0x5F {- _ -} then
                eerr row col E.PStart

            else
                let
                    newPos =
                        pos + 1

                    newCol =
                        col + 1
                in
                if Var.getInnerWidth src newPos end > 0 then
                    let
                        ( badPos, badCol ) =
                            Var.chompInnerChars src newPos end newCol
                    in
                    cerr row col (E.PWildcardNotVar (Name.fromPtr src pos badPos) (badCol - col))

                else
                    let
                        newState =
                            P.State src newPos end indent row newCol
                    in
                    cok () newState



-- RECORDS


record : A.Position -> P.Parser z E.Pattern Src.Pattern
record start =
    P.inContext E.PRecord (P.word1 0x7B {- { -} E.PStart) <|
        P.do1 (Space.chompAndCheckIndent E.PRecordSpace E.PRecordIndentOpen) <|
            P.oneOf E.PRecordOpen
                [ P.bind (P.addLocation (Var.lower E.PRecordField)) <|
                    \var ->
                        P.do1 (Space.chompAndCheckIndent E.PRecordSpace E.PRecordIndentEnd) <|
                            recordHelp start [ var ]
                , P.do1 (P.word1 0x7D {- } -} E.PRecordEnd) <|
                    P.addEnd start (Src.PRecord [])
                ]


recordHelp : A.Position -> TList (A.Located Name.Name) -> P.Parser z E.PRecord Src.Pattern
recordHelp start vars =
    P.oneOf E.PRecordEnd
        [ P.doAndBind
            [ P.do (P.word1 0x2C {- , -} E.PRecordEnd)
            , P.do (Space.chompAndCheckIndent E.PRecordSpace E.PRecordIndentField)
            ]
            (P.addLocation (Var.lower E.PRecordField))
          <|
            \var ->
                P.do1 (Space.chompAndCheckIndent E.PRecordSpace E.PRecordIndentEnd) <|
                    recordHelp start (var :: vars)
        , P.do1 (P.word1 0x7D {- } -} E.PRecordEnd) <|
            P.addEnd start (Src.PRecord (MList.reverse vars))
        ]



-- TUPLES


tuple : A.Position -> P.Parser z E.Pattern Src.Pattern
tuple start =
    P.inContext E.PTuple (P.word1 0x28 {- ( -} E.PStart) <|
        P.do1 (Space.chompAndCheckIndent E.PTupleSpace E.PTupleIndentExpr1) <|
            P.oneOf E.PTupleOpen
                [ P.bind (P.specialize E.PTupleExpr expression) <|
                    \( pattern, end ) ->
                        P.do1 (Space.checkIndent end E.PTupleIndentEnd) <|
                            tupleHelp start pattern []
                , P.do1 (P.word1 0x29 {- ) -} E.PTupleEnd) <|
                    P.addEnd start Src.PUnit
                ]


tupleHelp : A.Position -> Src.Pattern -> TList Src.Pattern -> P.Parser z E.PTuple Src.Pattern
tupleHelp start firstPattern revPatterns =
    P.oneOf E.PTupleEnd
        [ P.doAndBind
            [ P.do (P.word1 0x2C {- , -} E.PTupleEnd)
            , P.do (Space.chompAndCheckIndent E.PTupleSpace E.PTupleIndentExprN)
            ]
            (P.specialize E.PTupleExpr expression)
          <|
            \( pattern, end ) ->
                P.do1 (Space.checkIndent end E.PTupleIndentEnd) <|
                    tupleHelp start firstPattern (pattern :: revPatterns)
        , P.do1 (P.word1 0x29 {- ) -} E.PTupleEnd) <|
            case MList.reverse revPatterns of
                [] ->
                    P.return firstPattern

                secondPattern :: otherPatterns ->
                    P.addEnd start (Src.PTuple firstPattern secondPattern otherPatterns)
        ]



-- LIST


list : A.Position -> P.Parser z E.Pattern Src.Pattern
list start =
    P.inContext E.PList (P.word1 0x5B {- [ -} E.PStart) <|
        P.do1 (Space.chompAndCheckIndent E.PListSpace E.PListIndentOpen) <|
            P.oneOf E.PListOpen
                [ P.bind (P.specialize E.PListExpr expression) <|
                    \( pattern, end ) ->
                        P.do1 (Space.checkIndent end E.PListIndentEnd) <|
                            listHelp start [ pattern ]
                , P.do1 (P.word1 0x5D {- ] -} E.PListEnd) <|
                    P.addEnd start (Src.PList [])
                ]


listHelp : A.Position -> TList Src.Pattern -> P.Parser z E.PList Src.Pattern
listHelp start patterns =
    P.oneOf E.PListEnd
        [ P.doAndBind
            [ P.do (P.word1 0x2C {- , -} E.PListEnd)
            , P.do (Space.chompAndCheckIndent E.PListSpace E.PListIndentExpr)
            ]
            (P.specialize E.PListExpr expression)
          <|
            \( pattern, end ) ->
                P.do1 (Space.checkIndent end E.PListIndentEnd) <|
                    listHelp start (pattern :: patterns)
        , P.do1 (P.word1 0x5D {- ] -} E.PListEnd) <|
            P.addEnd start (Src.PList (MList.reverse patterns))
        ]



-- EXPRESSION


expression : P.Parser z E.Pattern ( Src.Pattern, A.Position )
expression =
    P.bind P.getPosition <|
        \start ->
            P.bind exprPart <|
                \ePart ->
                    exprHelp start [] ePart


exprHelp : A.Position -> TList Src.Pattern -> ( Src.Pattern, A.Position ) -> P.Parser z E.Pattern ( Src.Pattern, A.Position )
exprHelp start revPatterns ( pattern, end ) =
    P.oneOfWithFallback
        [ P.doAndBind
            [ P.do (Space.checkIndent end E.PIndentStart)
            , P.do (P.word2 0x3A 0x3A {- :: -} E.PStart)
            , P.do (Space.chompAndCheckIndent E.PSpace E.PIndentStart)
            ]
            exprPart
          <|
            \ePart ->
                exprHelp start (pattern :: revPatterns) ePart
        , P.doAndBind
            [ P.do (Space.checkIndent end E.PIndentStart)
            , P.do (Keyword.as_ E.PStart)
            , P.do (Space.chompAndCheckIndent E.PSpace E.PIndentAlias)
            ]
            P.getPosition
          <|
            \nameStart ->
                P.bind (Var.lower E.PAlias) <|
                    \name ->
                        P.bind P.getPosition <|
                            \newEnd ->
                                P.do1 (Space.chomp E.PSpace) <|
                                    let
                                        alias =
                                            A.at nameStart newEnd name
                                    in
                                    P.return
                                        ( A.at start newEnd (Src.PAlias (MList.foldl cons pattern revPatterns) alias)
                                        , newEnd
                                        )
        ]
        ( MList.foldl cons pattern revPatterns
        , end
        )


cons : Src.Pattern -> Src.Pattern -> Src.Pattern
cons tl hd =
    A.merge hd tl (Src.PCons hd tl)



-- EXPRESSION PART


exprPart : P.Parser z E.Pattern ( Src.Pattern, A.Position )
exprPart =
    P.oneOf E.PStart
        [ P.bind P.getPosition <|
            \start ->
                P.bind (Var.foreignUpper E.PStart) <|
                    \upper ->
                        P.bind P.getPosition <|
                            \end ->
                                exprTermHelp (A.Region start end) upper start []
        , P.bind term <|
            \((A.At (A.Region _ end) _) as eterm) ->
                P.do1 (Space.chomp E.PSpace) <|
                    P.return ( eterm, end )
        ]


exprTermHelp : A.Region -> Var.Upper -> A.Position -> TList Src.Pattern -> P.Parser z E.Pattern ( Src.Pattern, A.Position )
exprTermHelp region upper start revArgs =
    P.bind P.getPosition <|
        \end ->
            P.do1 (Space.chomp E.PSpace) <|
                P.oneOfWithFallback
                    [ P.doAndBind
                        [ P.do (Space.checkIndent end E.PIndentStart)
                        ]
                        term
                      <|
                        \arg ->
                            exprTermHelp region upper start (arg :: revArgs)
                    ]
                    ( A.at start end <|
                        case upper of
                            Var.Unqualified name ->
                                Src.PCtor region name (MList.reverse revArgs)

                            Var.Qualified home name ->
                                Src.PCtorQual region home name (MList.reverse revArgs)
                    , end
                    )
