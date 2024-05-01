module Compiler.Parse.Expression exposing (expression)

import Compiler.AST.Source as Src
import Compiler.Data.Name as Name
import Compiler.Parse.Keyword as Keyword
import Compiler.Parse.Number as Number
import Compiler.Parse.Pattern as Pattern
import Compiler.Parse.Primitives as P
import Compiler.Parse.Shader as Shader
import Compiler.Parse.Space as Space
import Compiler.Parse.String as MString
import Compiler.Parse.Symbol as Symbol
import Compiler.Parse.Type as Type
import Compiler.Parse.Variable as Var
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Syntax as E
import Extra.Type.List as MList exposing (TList)



-- TERMS


term : P.Parser z E.Expr Src.Expr
term =
    P.bind P.getPosition <|
        \start ->
            P.oneOf E.Start
                [ P.bind (variable start) (accessible start)
                , string start
                , number start
                , Shader.shader start
                , list start
                , P.bind (record start) (accessible start)
                , P.bind (tuple start) (accessible start)
                , accessor start
                , character start
                ]


string : A.Position -> P.Parser z E.Expr Src.Expr
string start =
    P.bind (MString.string E.Start E.CString) <|
        \str ->
            P.addEnd start (Src.Str str)


character : A.Position -> P.Parser z E.Expr Src.Expr
character start =
    P.bind (MString.character E.Start E.CChar) <|
        \chr ->
            P.addEnd start (Src.Chr chr)


number : A.Position -> P.Parser z E.Expr Src.Expr
number start =
    P.bind (Number.number E.Start E.Number) <|
        \nmbr ->
            P.addEnd start <|
                case nmbr of
                    Number.CInt int ->
                        Src.CInt int

                    Number.CFloat float ->
                        Src.CFloat float


accessor : A.Position -> P.Parser z E.Expr Src.Expr
accessor start =
    P.doAndBind
        [ P.do (P.word1 0x2E {- . -} E.Dot)
        ]
        (Var.lower E.Access)
    <|
        \field ->
            P.addEnd start (Src.Accessor field)


variable : A.Position -> P.Parser z E.Expr Src.Expr
variable start =
    P.bind (Var.foreignAlpha E.Start) <|
        \var ->
            P.addEnd start var


accessible : A.Position -> Src.Expr -> P.Parser z E.Expr Src.Expr
accessible start expr =
    P.oneOfWithFallback
        [ P.doAndBind
            [ P.do (P.word1 0x2E {- . -} E.Dot)
            ]
            P.getPosition
          <|
            \pos ->
                P.bind (Var.lower E.Access) <|
                    \field ->
                        P.bind P.getPosition <|
                            \end ->
                                accessible start <|
                                    A.at start end (Src.Access expr (A.at pos end field))
        ]
        expr



-- LISTS


list : A.Position -> P.Parser e E.Expr Src.Expr
list start =
    P.inContext E.CList (P.word1 0x5B {- [ -} E.Start) <|
        P.do1 (Space.chompAndCheckIndent E.ListSpace E.ListIndentOpen) <|
            P.oneOf E.ListOpen
                [ P.bind (P.specialize E.ListExpr expression) <|
                    \( entry, end ) ->
                        P.do1 (Space.checkIndent end E.ListIndentEnd) <|
                            chompListEnd start [ entry ]
                , P.do1 (P.word1 0x5D {- ] -} E.ListOpen) <|
                    P.addEnd start (Src.CList [])
                ]


chompListEnd : A.Position -> TList Src.Expr -> P.Parser z E.TList Src.Expr
chompListEnd start entries =
    P.oneOf E.ListEnd
        [ P.doAndBind
            [ P.do (P.word1 0x2C {- , -} E.ListEnd)
            , P.do (Space.chompAndCheckIndent E.ListSpace E.ListIndentExpr)
            ]
            (P.specialize E.ListExpr expression)
          <|
            \( entry, end ) ->
                P.do1 (Space.checkIndent end E.ListIndentEnd) <|
                    chompListEnd start (entry :: entries)
        , P.do1 (P.word1 0x5D {- ] -} E.ListEnd) <|
            P.addEnd start (Src.CList (MList.reverse entries))
        ]



-- TUPLES


tuple : A.Position -> P.Parser z E.Expr Src.Expr
tuple ((A.Position row col) as start) =
    P.inContext E.Tuple (P.word1 0x28 {- ( -} E.Start) <|
        P.bind P.getPosition <|
            \before ->
                P.doAndBind
                    [ P.do (Space.chompAndCheckIndent E.TupleSpace E.TupleIndentExpr1)
                    ]
                    P.getPosition
                <|
                    \after ->
                        if before /= after then
                            P.bind (P.specialize E.TupleExpr expression) <|
                                \( entry, end ) ->
                                    P.do1 (Space.checkIndent end E.TupleIndentEnd) <|
                                        chompTupleEnd start entry []

                        else
                            P.oneOf E.TupleIndentExpr1
                                [ P.bind (Symbol.operator E.TupleIndentExpr1 E.TupleOperatorReserved) <|
                                    \op ->
                                        if op == "-" then
                                            P.oneOf E.TupleOperatorClose
                                                [ P.do1 (P.word1 0x29 {- ) -} E.TupleOperatorClose) <|
                                                    P.addEnd start (Src.Op op)
                                                , P.bind
                                                    (P.specialize E.TupleExpr <|
                                                        P.bind term <|
                                                            \((A.At (A.Region _ end) _) as negatedExpr) ->
                                                                P.do1 (Space.chomp E.Space) <|
                                                                    let
                                                                        exprStart =
                                                                            A.Position row (col + 2)

                                                                        expr =
                                                                            A.at exprStart end (Src.Negate negatedExpr)
                                                                    in
                                                                    chompExprEnd exprStart (State [] expr [] end)
                                                    )
                                                  <|
                                                    \( entry, end ) ->
                                                        P.do1 (Space.checkIndent end E.TupleIndentEnd) <|
                                                            chompTupleEnd start entry []
                                                ]

                                        else
                                            P.do1 (P.word1 0x29 {- ) -} E.TupleOperatorClose) <|
                                                P.addEnd start (Src.Op op)
                                , P.do1 (P.word1 0x29 {- ) -} E.TupleIndentExpr1) <|
                                    P.addEnd start Src.Unit
                                , P.bind (P.specialize E.TupleExpr expression) <|
                                    \( entry, end ) ->
                                        P.do1 (Space.checkIndent end E.TupleIndentEnd) <|
                                            chompTupleEnd start entry []
                                ]


chompTupleEnd : A.Position -> Src.Expr -> TList Src.Expr -> P.Parser z E.Tuple Src.Expr
chompTupleEnd start firstExpr revExprs =
    P.oneOf E.TupleEnd
        [ P.doAndBind
            [ P.do (P.word1 0x2C {- , -} E.TupleEnd)
            , P.do (Space.chompAndCheckIndent E.TupleSpace E.TupleIndentExprN)
            ]
            (P.specialize E.TupleExpr expression)
          <|
            \( entry, end ) ->
                P.do1 (Space.checkIndent end E.TupleIndentEnd) <|
                    chompTupleEnd start firstExpr (entry :: revExprs)
        , P.do1 (P.word1 0x29 {- ) -} E.TupleEnd) <|
            case MList.reverse revExprs of
                [] ->
                    P.return firstExpr

                secondExpr :: otherExprs ->
                    P.addEnd start (Src.Tuple firstExpr secondExpr otherExprs)
        ]



-- RECORDS


record : A.Position -> P.Parser z E.Expr Src.Expr
record start =
    P.inContext E.Record (P.word1 0x7B {- { -} E.Start) <|
        P.do1 (Space.chompAndCheckIndent E.RecordSpace E.RecordIndentOpen) <|
            P.oneOf E.RecordOpen
                [ P.do1 (P.word1 0x7D {- } -} E.RecordOpen) <|
                    P.addEnd start (Src.Record [])
                , P.bind (P.addLocation (Var.lower E.RecordField)) <|
                    \starter ->
                        P.do1 (Space.chompAndCheckIndent E.RecordSpace E.RecordIndentEquals) <|
                            P.oneOf E.RecordEquals
                                [ P.doAndBind
                                    [ P.do (P.word1 0x7C {- | -} E.RecordEquals)
                                    , P.do (Space.chompAndCheckIndent E.RecordSpace E.RecordIndentField)
                                    ]
                                    chompField
                                  <|
                                    \firstField ->
                                        P.bind (chompFields [ firstField ]) <|
                                            \fields ->
                                                P.addEnd start (Src.Update starter fields)
                                , P.doAndBind
                                    [ P.do (P.word1 0x3D {- = -} E.RecordEquals)
                                    , P.do (Space.chompAndCheckIndent E.RecordSpace E.RecordIndentExpr)
                                    ]
                                    (P.specialize E.RecordExpr expression)
                                  <|
                                    \( value, end ) ->
                                        P.doAndBind
                                            [ P.do (Space.checkIndent end E.RecordIndentEnd)
                                            ]
                                            (chompFields [ ( starter, value ) ])
                                        <|
                                            \fields ->
                                                P.addEnd start (Src.Record fields)
                                ]
                ]


type alias Field =
    ( A.Located Name.Name, Src.Expr )


chompFields : TList Field -> P.Parser z E.Record (TList Field)
chompFields fields =
    P.oneOf E.RecordEnd
        [ P.doAndBind
            [ P.do (P.word1 0x2C {- , -} E.RecordEnd)
            , P.do (Space.chompAndCheckIndent E.RecordSpace E.RecordIndentField)
            ]
            chompField
          <|
            \f ->
                chompFields (f :: fields)
        , P.do1 (P.word1 0x7D {- } -} E.RecordEnd) <|
            P.return (MList.reverse fields)
        ]


chompField : P.Parser z E.Record Field
chompField =
    P.bind (P.addLocation (Var.lower E.RecordField)) <|
        \key ->
            P.doAndBind
                [ P.do (Space.chompAndCheckIndent E.RecordSpace E.RecordIndentEquals)
                , P.do (P.word1 0x3D {- = -} E.RecordEquals)
                , P.do (Space.chompAndCheckIndent E.RecordSpace E.RecordIndentExpr)
                ]
                (P.specialize E.RecordExpr expression)
            <|
                \( value, end ) ->
                    P.do1 (Space.checkIndent end E.RecordIndentEnd) <|
                        P.return ( key, value )



-- EXPRESSIONS


expression : P.Parser z E.Expr ( Src.Expr, A.Position )
expression =
    P.bind P.getPosition <|
        \start ->
            P.oneOf E.Start
                [ let_ start
                , if_ start
                , case_ start
                , function start
                , P.bind (possiblyNegativeTerm start) <|
                    \expr ->
                        P.bind P.getPosition <|
                            \end ->
                                P.do1 (Space.chomp E.Space) <|
                                    chompExprEnd start (State [] expr [] end)
                ]


type State
    = State
        --{ ops  : (List_ (Src.Expr, A.Located Name.Name))
        --, expr : Src.Expr
        --, args : (List_ Src.Expr)
        --, end  : A.Position
        --}
        (TList ( Src.Expr, A.Located Name.Name ))
        Src.Expr
        (TList Src.Expr)
        A.Position


chompExprEnd : A.Position -> State -> P.Parser z E.Expr ( Src.Expr, A.Position )
chompExprEnd start (State ops expr args end) =
    P.oneOfWithFallback
        [ -- argument
          P.doAndBind
            [ P.do (Space.checkIndent end E.Start)
            ]
            term
          <|
            \arg ->
                P.bind P.getPosition <|
                    \newEnd ->
                        P.do1 (Space.chomp E.Space) <|
                            chompExprEnd start (State ops expr (arg :: args) newEnd)
        , -- operator
          P.do1 (Space.checkIndent end E.Start) <|
            P.bind (P.addLocation (Symbol.operator E.Start E.OperatorReserved)) <|
                \((A.At (A.Region opStart opEnd) opName) as op) ->
                    P.doAndBind
                        [ P.do (Space.chompAndCheckIndent E.Space (E.IndentOperatorRight opName))
                        ]
                        P.getPosition
                    <|
                        \newStart ->
                            if "-" == opName && end /= opStart && opEnd == newStart then
                                -- negative terms
                                P.bind term <|
                                    \negatedExpr ->
                                        P.bind P.getPosition <|
                                            \newEnd ->
                                                P.do1 (Space.chomp E.Space) <|
                                                    let
                                                        arg =
                                                            A.at opStart newEnd (Src.Negate negatedExpr)
                                                    in
                                                    chompExprEnd start (State ops expr (arg :: args) newEnd)

                            else
                                let
                                    err =
                                        E.OperatorRight opName
                                in
                                P.oneOf err
                                    [ -- term
                                      P.bind (possiblyNegativeTerm newStart) <|
                                        \newExpr ->
                                            P.bind P.getPosition <|
                                                \newEnd ->
                                                    P.do1 (Space.chomp E.Space) <|
                                                        let
                                                            newOps =
                                                                ( toCall expr args, op ) :: ops
                                                        in
                                                        chompExprEnd start (State newOps newExpr [] newEnd)
                                    , -- final term
                                      P.bind
                                        (P.oneOf err
                                            [ let_ newStart
                                            , case_ newStart
                                            , if_ newStart
                                            , function newStart
                                            ]
                                        )
                                      <|
                                        \( newLast, newEnd ) ->
                                            let
                                                newOps =
                                                    ( toCall expr args, op ) :: ops

                                                finalExpr =
                                                    Src.Binops (MList.reverse newOps) newLast
                                            in
                                            P.return ( A.at start newEnd finalExpr, newEnd )
                                    ]
        ]
        -- done
        (case ops of
            [] ->
                ( toCall expr args
                , end
                )

            _ ->
                ( A.at start end (Src.Binops (MList.reverse ops) (toCall expr args))
                , end
                )
        )


possiblyNegativeTerm : A.Position -> P.Parser z E.Expr Src.Expr
possiblyNegativeTerm start =
    P.oneOf E.Start
        [ P.doAndBind
            [ P.do
                (P.word1 0x2D
                    {---}
                    E.Start
                )
            ]
            term
          <|
            \expr ->
                P.addEnd start (Src.Negate expr)
        , term
        ]


toCall : Src.Expr -> TList Src.Expr -> Src.Expr
toCall func revArgs =
    case revArgs of
        [] ->
            func

        lastArg :: _ ->
            A.merge func lastArg (Src.Call func (MList.reverse revArgs))



-- IF EXPRESSION


if_ : A.Position -> P.Parser z E.Expr ( Src.Expr, A.Position )
if_ start =
    P.inContext E.If (Keyword.if_ E.Start) <|
        chompIfEnd start []


chompIfEnd : A.Position -> TList ( Src.Expr, Src.Expr ) -> P.Parser z E.If ( Src.Expr, A.Position )
chompIfEnd start branches =
    P.doAndBind
        [ P.do (Space.chompAndCheckIndent E.IfSpace E.IfIndentCondition)
        ]
        (P.specialize E.IfCondition expression)
    <|
        \( condition, condEnd ) ->
            P.doAndBind
                [ P.do (Space.checkIndent condEnd E.IfIndentThen)
                , P.do (Keyword.then_ E.IfThen)
                , P.do (Space.chompAndCheckIndent E.IfSpace E.IfIndentThenBranch)
                ]
                (P.specialize E.IfThenBranch expression)
            <|
                \( thenBranch, thenEnd ) ->
                    P.doN
                        [ P.do (Space.checkIndent thenEnd E.IfIndentElse)
                        , P.do (Keyword.else_ E.IfElse)
                        , P.do (Space.chompAndCheckIndent E.IfSpace E.IfIndentElseBranch)
                        ]
                    <|
                        let
                            newBranches =
                                ( condition, thenBranch ) :: branches
                        in
                        P.oneOf E.IfElseBranchStart
                            [ P.do1 (Keyword.if_ E.IfElseBranchStart) <|
                                chompIfEnd start newBranches
                            , P.bind (P.specialize E.IfElseBranch expression) <|
                                \( elseBranch, elseEnd ) ->
                                    let
                                        ifExpr =
                                            Src.If (MList.reverse newBranches) elseBranch
                                    in
                                    P.return ( A.at start elseEnd ifExpr, elseEnd )
                            ]



-- LAMBDA EXPRESSION


function : A.Position -> P.Parser z E.Expr ( Src.Expr, A.Position )
function start =
    P.inContext E.Func (P.word1 0x5C {- \ -} E.Start) <|
        P.doAndBind
            [ P.do (Space.chompAndCheckIndent E.FuncSpace E.FuncIndentArg)
            ]
            (P.specialize E.FuncArg Pattern.term)
        <|
            \arg ->
                P.doAndBind
                    [ P.do (Space.chompAndCheckIndent E.FuncSpace E.FuncIndentArrow)
                    ]
                    (chompArgs [ arg ])
                <|
                    \revArgs ->
                        P.doAndBind
                            [ P.do (Space.chompAndCheckIndent E.FuncSpace E.FuncIndentBody)
                            ]
                            (P.specialize E.FuncBody expression)
                        <|
                            \( body, end ) ->
                                let
                                    funcExpr =
                                        Src.Lambda (MList.reverse revArgs) body
                                in
                                P.return ( A.at start end funcExpr, end )


chompArgs : TList Src.Pattern -> P.Parser t E.Func (TList Src.Pattern)
chompArgs revArgs =
    P.oneOf E.FuncArrow
        [ P.bind (P.specialize E.FuncArg Pattern.term) <|
            \arg ->
                P.do1 (Space.chompAndCheckIndent E.FuncSpace E.FuncIndentArrow) <|
                    chompArgs (arg :: revArgs)
        , P.do1 (P.word2 0x2D 0x3E E.FuncArrow) <|
            {- -> -} P.return revArgs
        ]



-- CASE EXPRESSIONS


case_ : A.Position -> P.Parser z E.Expr ( Src.Expr, A.Position )
case_ start =
    P.inContext E.Case (Keyword.case_ E.Start) <|
        P.doAndBind
            [ P.do (Space.chompAndCheckIndent E.CaseSpace E.CaseIndentExpr)
            ]
            (P.specialize E.CaseExpr expression)
        <|
            \( expr, exprEnd ) ->
                P.doN
                    [ P.do (Space.checkIndent exprEnd E.CaseIndentOf)
                    , P.do (Keyword.of_ E.CaseOf)
                    , P.do (Space.chompAndCheckIndent E.CaseSpace E.CaseIndentPattern)
                    ]
                <|
                    P.withIndent <|
                        P.bind chompBranch <|
                            \( firstBranch, firstEnd ) ->
                                P.bind (chompCaseEnd [ firstBranch ] firstEnd) <|
                                    \( branches, end ) ->
                                        P.return
                                            ( A.at start end (Src.Case expr branches)
                                            , end
                                            )


chompBranch : P.Parser z E.Case ( ( Src.Pattern, Src.Expr ), A.Position )
chompBranch =
    P.bind (P.specialize E.CasePattern Pattern.expression) <|
        \( pattern, patternEnd ) ->
            P.doAndBind
                [ P.do (Space.checkIndent patternEnd E.CaseIndentArrow)
                , P.do
                    (P.word2 0x2D
                        0x3E
                        {-->-}
                        E.CaseArrow
                    )
                , P.do (Space.chompAndCheckIndent E.CaseSpace E.CaseIndentBranch)
                ]
                (P.specialize E.CaseBranch expression)
            <|
                \( branchExpr, end ) ->
                    P.return ( ( pattern, branchExpr ), end )


chompCaseEnd : TList ( Src.Pattern, Src.Expr ) -> A.Position -> P.Parser z E.Case ( TList ( Src.Pattern, Src.Expr ), A.Position )
chompCaseEnd branches end =
    P.oneOfWithFallback
        [ P.doAndBind
            [ P.do (Space.checkAligned E.CasePatternAlignment)
            ]
            chompBranch
          <|
            \( branch, newEnd ) ->
                chompCaseEnd (branch :: branches) newEnd
        ]
        ( MList.reverse branches, end )



-- LET EXPRESSION


let_ : A.Position -> P.Parser z E.Expr ( Src.Expr, A.Position )
let_ start =
    P.inContext E.Let (Keyword.let_ E.Start) <|
        P.bind
            (P.withBacksetIndent 3 <|
                P.do1 (Space.chompAndCheckIndent E.LetSpace E.LetIndentDef) <|
                    P.withIndent <|
                        P.bind chompLetDef <|
                            \( def, end ) ->
                                chompLetDefs [ def ] end
            )
        <|
            \( defs, defsEnd ) ->
                P.doAndBind
                    [ P.do (Space.checkIndent defsEnd E.LetIndentIn)
                    , P.do (Keyword.in_ E.LetIn)
                    , P.do (Space.chompAndCheckIndent E.LetSpace E.LetIndentBody)
                    ]
                    (P.specialize E.LetBody expression)
                <|
                    \( body, end ) ->
                        P.return
                            ( A.at start end (Src.Let defs body)
                            , end
                            )


chompLetDefs : TList (A.Located Src.Def) -> A.Position -> P.Parser z E.Let ( TList (A.Located Src.Def), A.Position )
chompLetDefs revDefs end =
    P.oneOfWithFallback
        [ P.doAndBind
            [ P.do (Space.checkAligned E.LetDefAlignment)
            ]
            chompLetDef
          <|
            \( def, newEnd ) ->
                chompLetDefs (def :: revDefs) newEnd
        ]
        ( MList.reverse revDefs, end )



-- LET DEFINITIONS


chompLetDef : P.Parser z E.Let ( A.Located Src.Def, A.Position )
chompLetDef =
    P.oneOf E.LetDefName
        [ definition
        , destructure
        ]



-- DEFINITION


definition : P.Parser z E.Let ( A.Located Src.Def, A.Position )
definition =
    P.bind (P.addLocation (Var.lower E.LetDefName)) <|
        \((A.At (A.Region start _) name) as aname) ->
            P.specialize (E.LetDef name) <|
                P.do1 (Space.chompAndCheckIndent E.DefSpace E.DefIndentEquals) <|
                    P.oneOf E.DefEquals
                        [ P.doAndBind
                            [ P.do (P.word1 0x3A {- : -} E.DefEquals)
                            , P.do (Space.chompAndCheckIndent E.DefSpace E.DefIndentType)
                            ]
                            (P.specialize E.DefType Type.expression)
                          <|
                            \( tipe, _ ) ->
                                P.doAndBind
                                    [ P.do (Space.checkAligned E.DefAlignment)
                                    ]
                                    (chompMatchingName name)
                                <|
                                    \defName ->
                                        P.do1 (Space.chompAndCheckIndent E.DefSpace E.DefIndentEquals) <|
                                            chompDefArgsAndBody start defName (Just tipe) []
                        , chompDefArgsAndBody start aname Nothing []
                        ]


chompDefArgsAndBody : A.Position -> A.Located Name.Name -> Maybe Src.Type -> TList Src.Pattern -> P.Parser z E.Def ( A.Located Src.Def, A.Position )
chompDefArgsAndBody start name tipe revArgs =
    P.oneOf E.DefEquals
        [ P.bind (P.specialize E.DefArg Pattern.term) <|
            \arg ->
                P.do1 (Space.chompAndCheckIndent E.DefSpace E.DefIndentEquals) <|
                    chompDefArgsAndBody start name tipe (arg :: revArgs)
        , P.doAndBind
            [ P.do (P.word1 0x3D {- = -} E.DefEquals)
            , P.do (Space.chompAndCheckIndent E.DefSpace E.DefIndentBody)
            ]
            (P.specialize E.DefBody expression)
          <|
            \( body, end ) ->
                P.return
                    ( A.at start end (Src.Define name (MList.reverse revArgs) body tipe)
                    , end
                    )
        ]


chompMatchingName : Name.Name -> P.Parser z E.Def (A.Located Name.Name)
chompMatchingName expectedName =
    let
        (P.Parser parserL) =
            Var.lower E.DefNameRepeat
    in
    P.Parser <|
        \((P.State _ _ _ _ sr sc) as state) cok eok cerr eerr ->
            let
                cokL name ((P.State _ _ _ _ er ec) as newState) =
                    if expectedName == name then
                        cok (A.At (A.Region (A.Position sr sc) (A.Position er ec)) name) newState

                    else
                        cerr sr sc (E.DefNameMatch name)

                eokL name ((P.State _ _ _ _ er ec) as newState) =
                    if expectedName == name then
                        eok (A.At (A.Region (A.Position sr sc) (A.Position er ec)) name) newState

                    else
                        eerr sr sc (E.DefNameMatch name)
            in
            parserL state cokL eokL cerr eerr



-- DESTRUCTURE


destructure : P.Parser z E.Let ( A.Located Src.Def, A.Position )
destructure =
    P.specialize E.LetDestruct <|
        P.bind P.getPosition <|
            \start ->
                P.bind (P.specialize E.DestructPattern Pattern.term) <|
                    \pattern ->
                        P.doAndBind
                            [ P.do (Space.chompAndCheckIndent E.DestructSpace E.DestructIndentEquals)
                            , P.do (P.word1 0x3D {- = -} E.DestructEquals)
                            , P.do (Space.chompAndCheckIndent E.DestructSpace E.DestructIndentBody)
                            ]
                            (P.specialize E.DestructBody expression)
                        <|
                            \( expr, end ) ->
                                P.return ( A.at start end (Src.Destruct pattern expr), end )
