module Compiler.Parse.Type exposing
    ( expression
    , variant
    )

import Compiler.AST.Source as Src
import Compiler.Data.Name as Name
import Compiler.Parse.Primitives as P
import Compiler.Parse.Space as Space
import Compiler.Parse.Variable as Var
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Syntax as E
import Extra.Type.List as MList exposing (TList)



-- TYPE TERMS


term : P.Parser z E.Type Src.Type
term =
    P.bind P.getPosition <|
        \start ->
            P.oneOf E.TStart
                [ -- types with no arguments (Int, Float, etc.)
                  P.bind (Var.foreignUpper E.TStart) <|
                    \upper ->
                        P.bind P.getPosition <|
                            \end ->
                                let
                                    region =
                                        A.Region start end
                                in
                                P.return <|
                                    A.At region <|
                                        case upper of
                                            Var.Unqualified name ->
                                                Src.TType region name []

                                            Var.Qualified home name ->
                                                Src.TTypeQual region home name []
                , -- type variables
                  P.bind (Var.lower E.TStart) <|
                    \var ->
                        P.addEnd start (Src.TVar var)
                , -- tuples
                  P.inContext E.TTuple (P.word1 0x28 {- ( -} E.TStart) <|
                    P.oneOf E.TTupleOpen
                        [ P.do1 (P.word1 0x29 {- ) -} E.TTupleOpen) <|
                            P.addEnd start Src.TUnit
                        , P.doAndBind
                            [ P.do (Space.chompAndCheckIndent E.TTupleSpace E.TTupleIndentType1)
                            ]
                            (P.specialize E.TTupleType expression)
                          <|
                            \( tipe, end ) ->
                                P.do1 (Space.checkIndent end E.TTupleIndentEnd) <|
                                    chompTupleEnd start tipe []
                        ]
                , -- records
                  P.inContext E.TRecord (P.word1 0x7B {- { -} E.TStart) <|
                    P.do1 (Space.chompAndCheckIndent E.TRecordSpace E.TRecordIndentOpen) <|
                        P.oneOf E.TRecordOpen
                            [ P.do1 (P.word1 0x7D {- } -} E.TRecordEnd) <|
                                P.addEnd start (Src.TRecord [] Nothing)
                            , P.bind (P.addLocation (Var.lower E.TRecordField)) <|
                                \name ->
                                    P.do1 (Space.chompAndCheckIndent E.TRecordSpace E.TRecordIndentColon) <|
                                        P.oneOf E.TRecordColon
                                            [ P.doAndBind
                                                [ P.do (P.word1 0x7C {- | -} E.TRecordColon)
                                                , P.do (Space.chompAndCheckIndent E.TRecordSpace E.TRecordIndentField)
                                                ]
                                                chompField
                                              <|
                                                \field ->
                                                    P.bind (chompRecordEnd [ field ]) <|
                                                        \fields ->
                                                            P.addEnd start (Src.TRecord fields (Just name))
                                            , P.doAndBind
                                                [ P.do (P.word1 0x3A {- : -} E.TRecordColon)
                                                , P.do (Space.chompAndCheckIndent E.TRecordSpace E.TRecordIndentType)
                                                ]
                                                (P.specialize E.TRecordType expression)
                                              <|
                                                \( tipe, end ) ->
                                                    P.doAndBind
                                                        [ P.do (Space.checkIndent end E.TRecordIndentEnd)
                                                        ]
                                                        (chompRecordEnd [ ( name, tipe ) ])
                                                    <|
                                                        \fields ->
                                                            P.addEnd start (Src.TRecord fields Nothing)
                                            ]
                            ]
                ]



-- TYPE EXPRESSIONS


expression : P.Parser z E.Type ( Src.Type, A.Position )
expression =
    P.bind P.getPosition <|
        \start ->
            P.bind
                (P.oneOf E.TStart
                    [ app start
                    , P.bind term <|
                        \eterm ->
                            P.bind P.getPosition <|
                                \end ->
                                    P.do1 (Space.chomp E.TSpace) <|
                                        P.return ( eterm, end )
                    ]
                )
            <|
                \(( tipe1, end1 ) as term1) ->
                    P.oneOfWithFallback
                        [ P.doAndBind
                            [ P.do (Space.checkIndent end1 E.TIndentStart) -- should never trigger
                            , -- -> could just be another type instead
                              P.do (P.word2 0x2D 0x3E E.TStart)
                            , P.do (Space.chompAndCheckIndent E.TSpace E.TIndentStart)
                            ]
                            expression
                          <|
                            \( tipe2, end2 ) ->
                                let
                                    tipe =
                                        A.at start end2 (Src.TLambda tipe1 tipe2)
                                in
                                P.return ( tipe, end2 )
                        ]
                        term1



-- TYPE CONSTRUCTORS


app : A.Position -> P.Parser z E.Type ( Src.Type, A.Position )
app start =
    P.bind (Var.foreignUpper E.TStart) <|
        \upper ->
            P.bind P.getPosition <|
                \upperEnd ->
                    P.doAndBind
                        [ P.do (Space.chomp E.TSpace)
                        ]
                        (chompArgs [] upperEnd)
                    <|
                        \( args, end ) ->
                            let
                                region =
                                    A.Region start upperEnd

                                tipe =
                                    case upper of
                                        Var.Unqualified name ->
                                            Src.TType region name args

                                        Var.Qualified home name ->
                                            Src.TTypeQual region home name args
                            in
                            P.return ( A.at start end tipe, end )


chompArgs : TList Src.Type -> A.Position -> P.Parser z E.Type ( TList Src.Type, A.Position )
chompArgs args end =
    P.oneOfWithFallback
        [ P.doAndBind
            [ P.do (Space.checkIndent end E.TIndentStart)
            ]
            term
          <|
            \arg ->
                P.bind P.getPosition <|
                    \newEnd ->
                        P.do1 (Space.chomp E.TSpace) <|
                            chompArgs (arg :: args) newEnd
        ]
        ( MList.reverse args, end )



-- TUPLES


chompTupleEnd : A.Position -> Src.Type -> TList Src.Type -> P.Parser z E.TTuple Src.Type
chompTupleEnd start firstType revTypes =
    P.oneOf E.TTupleEnd
        [ P.doAndBind
            [ P.do (P.word1 0x2C {- , -} E.TTupleEnd)
            , P.do (Space.chompAndCheckIndent E.TTupleSpace E.TTupleIndentTypeN)
            ]
            (P.specialize E.TTupleType expression)
          <|
            \( tipe, end ) ->
                P.do1 (Space.checkIndent end E.TTupleIndentEnd) <|
                    chompTupleEnd start firstType (tipe :: revTypes)
        , P.do1 (P.word1 0x29 {- ) -} E.TTupleEnd) <|
            case MList.reverse revTypes of
                [] ->
                    P.return firstType

                secondType :: otherTypes ->
                    P.addEnd start (Src.TTuple firstType secondType otherTypes)
        ]



-- RECORD


type alias Field =
    ( A.Located Name.Name, Src.Type )


chompRecordEnd : TList Field -> P.Parser z E.TRecord (TList Field)
chompRecordEnd fields =
    P.oneOf E.TRecordEnd
        [ P.doAndBind
            [ P.do (P.word1 0x2C {- , -} E.TRecordEnd)
            , P.do (Space.chompAndCheckIndent E.TRecordSpace E.TRecordIndentField)
            ]
            chompField
          <|
            \field ->
                chompRecordEnd (field :: fields)
        , P.do1 (P.word1 0x7D {- } -} E.TRecordEnd) <|
            P.return (MList.reverse fields)
        ]


chompField : P.Parser z E.TRecord Field
chompField =
    P.bind (P.addLocation (Var.lower E.TRecordField)) <|
        \name ->
            P.doAndBind
                [ P.do (Space.chompAndCheckIndent E.TRecordSpace E.TRecordIndentColon)
                , P.do (P.word1 0x3A {- : -} E.TRecordColon)
                , P.do (Space.chompAndCheckIndent E.TRecordSpace E.TRecordIndentType)
                ]
                (P.specialize E.TRecordType expression)
            <|
                \( tipe, end ) ->
                    P.do1 (Space.checkIndent end E.TRecordIndentEnd) <|
                        P.return ( name, tipe )



-- VARIANT


variant : P.Parser z E.CustomType ( ( A.Located Name.Name, TList Src.Type ), A.Position )
variant =
    P.bind (P.addLocation (Var.upper E.CT_Variant)) <|
        \((A.At (A.Region _ nameEnd) _) as name) ->
            P.doAndBind
                [ P.do (Space.chomp E.CT_Space)
                ]
                (P.specialize E.CT_VariantArg (chompArgs [] nameEnd))
            <|
                \( args, end ) ->
                    P.return ( ( name, args ), end )
