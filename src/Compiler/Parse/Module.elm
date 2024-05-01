module Compiler.Parse.Module exposing
    ( ProjectType(..)
    , chompImport
    , chompImports
    , fromByteString
    , isKernel
    --
    , Header(..)
    , Effects(..)
    , chompHeader
    )

import Compiler.AST.Source as Src
import Compiler.Data.Name as Name
import Compiler.Elm.Compiler.Imports as Imports
import Compiler.Elm.Package as Pkg
import Compiler.Parse.Declaration as Decl
import Compiler.Parse.Keyword as Keyword
import Compiler.Parse.Primitives as P
import Compiler.Parse.Space as Space
import Compiler.Parse.Symbol as Symbol
import Compiler.Parse.Variable as Var
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Syntax as E
import Extra.Type.Either as Either exposing (Either(..))
import Extra.Type.List as MList exposing (TList)



-- FROM BYTE STRING


fromByteString : ProjectType -> String -> Either E.Error Src.Module
fromByteString projectType source =
    case P.fromByteString (chompModule projectType) E.ModuleBadEnd source of
        Right modul ->
            checkModule projectType modul

        Left err ->
            Left (E.ParseError err)



-- PROJECT TYPE


type ProjectType
    = Package Pkg.Name
    | Application


isCore : ProjectType -> Bool
isCore projectType =
    case projectType of
        Package pkg ->
            pkg == Pkg.core

        Application ->
            False


isKernel : ProjectType -> Bool
isKernel projectType =
    case projectType of
        Package pkg ->
            Pkg.isKernel pkg

        Application ->
            False



-- MODULE


type Module
    = Module
        --{ header : Maybe Header
        --, imports : List_ Src.Import
        --, infixes : List_ (A.Located Src.Infix)
        --, decls : List_ Decl.Decl
        --}
        (Maybe Header)
        (TList Src.Import)
        (TList (A.Located Src.Infix))
        (TList Decl.Decl)


chompModule : ProjectType -> P.Parser z E.Module Module
chompModule projectType =
    P.bind chompHeader <|
        \header ->
            P.bind
                (chompImports
                    (if isCore projectType then
                        []

                     else
                        Imports.defaults
                    )
                )
            <|
                \imports ->
                    P.bind
                        (if isKernel projectType then
                            chompInfixes []

                         else
                            P.return []
                        )
                    <|
                        \infixes ->
                            P.bind (P.specialize E.Declarations (chompDecls [])) <|
                                \decls ->
                                    P.return (Module header imports infixes decls)



-- CHECK MODULE


checkModule : ProjectType -> Module -> Either E.Error Src.Module
checkModule projectType (Module maybeHeader imports infixes decls) =
    let
        ( ( values, unions ), ( aliases, ports ) ) =
            categorizeDecls [] [] [] [] decls
    in
    case maybeHeader of
        Just (Header name effects exports docs) ->
            Either.fmap (Src.Module (Just name) exports (toDocs docs decls) imports values unions aliases infixes) <|
                checkEffects projectType ports effects

        Nothing ->
            Right <|
                Src.Module Nothing (A.At A.one Src.Open) (Src.NoDocs A.one) imports values unions aliases infixes <|
                    case ports of
                        [] ->
                            Src.NoEffects

                        _ :: _ ->
                            Src.Ports ports


checkEffects : ProjectType -> TList Src.Port -> Effects -> Either E.Error Src.Effects
checkEffects projectType ports effects =
    case effects of
        NoEffects region ->
            case ports of
                [] ->
                    Right Src.NoEffects

                (Src.Port name _) :: _ ->
                    case projectType of
                        Package _ ->
                            Left (E.NoPortsInPackage name)

                        Application ->
                            Left (E.UnexpectedPort region)

        Ports region ->
            case projectType of
                Package _ ->
                    Left (E.NoPortModulesInPackage region)

                Application ->
                    case ports of
                        [] ->
                            Left (E.NoPorts region)

                        _ :: _ ->
                            Right (Src.Ports ports)

        Manager region manager ->
            if isKernel projectType then
                case ports of
                    [] ->
                        Right (Src.Manager region manager)

                    _ :: _ ->
                        Left (E.UnexpectedPort region)

            else
                Left (E.NoEffectsOutsideKernel region)


categorizeDecls : TList (A.Located Src.Value) -> TList (A.Located Src.Union) -> TList (A.Located Src.Alias) -> TList Src.Port -> TList Decl.Decl -> ( ( TList (A.Located Src.Value), TList (A.Located Src.Union) ), ( TList (A.Located Src.Alias), TList Src.Port ) )
categorizeDecls values unions aliases ports decls =
    case decls of
        [] ->
            ( ( values, unions ), ( aliases, ports ) )

        decl :: otherDecls ->
            case decl of
                Decl.Value _ value ->
                    categorizeDecls (value :: values) unions aliases ports otherDecls

                Decl.Union _ union ->
                    categorizeDecls values (union :: unions) aliases ports otherDecls

                Decl.Alias _ alias ->
                    categorizeDecls values unions (alias :: aliases) ports otherDecls

                Decl.Port _ port_ ->
                    categorizeDecls values unions aliases (port_ :: ports) otherDecls



-- TO DOCS


toDocs : Either A.Region Src.Comment -> TList Decl.Decl -> Src.Docs
toDocs comment decls =
    case comment of
        Right overview ->
            Src.YesDocs overview (getComments decls [])

        Left region ->
            Src.NoDocs region


getComments : TList Decl.Decl -> TList ( Name.Name, Src.Comment ) -> TList ( Name.Name, Src.Comment )
getComments decls comments =
    case decls of
        [] ->
            comments

        decl :: otherDecls ->
            case decl of
                Decl.Value c (A.At _ (Src.Value n _ _ _)) ->
                    getComments otherDecls (addComment c n comments)

                Decl.Union c (A.At _ (Src.Union n _ _)) ->
                    getComments otherDecls (addComment c n comments)

                Decl.Alias c (A.At _ (Src.Alias n _ _)) ->
                    getComments otherDecls (addComment c n comments)

                Decl.Port c (Src.Port n _) ->
                    getComments otherDecls (addComment c n comments)


addComment : Maybe Src.Comment -> A.Located Name.Name -> TList ( Name.Name, Src.Comment ) -> TList ( Name.Name, Src.Comment )
addComment maybeComment (A.At _ name) comments =
    case maybeComment of
        Just comment ->
            ( name, comment ) :: comments

        Nothing ->
            comments



-- FRESH LINES


freshLine : (P.Row -> P.Col -> E.Module) -> P.Parser z E.Module ()
freshLine toFreshLineError =
    P.do1 (Space.chomp E.ModuleSpace) <|
        Space.checkFreshLine toFreshLineError



-- CHOMP DECLARATIONS


chompDecls : TList Decl.Decl -> P.Parser z E.Decl (TList Decl.Decl)
chompDecls decls =
    P.bind Decl.declaration <|
        \( decl, _ ) ->
            P.oneOfWithFallback
                [ P.do1 (Space.checkFreshLine E.DeclStart) <|
                    chompDecls (decl :: decls)
                ]
                (MList.reverse (decl :: decls))


chompInfixes : TList (A.Located Src.Infix) -> P.Parser z E.Module (TList (A.Located Src.Infix))
chompInfixes infixes =
    P.oneOfWithFallback
        [ P.bind Decl.infix_ <|
            \binop ->
                chompInfixes (binop :: infixes)
        ]
        infixes



-- MODULE DOC COMMENT


chompModuleDocCommentSpace : P.Parser z E.Module (Either A.Region Src.Comment)
chompModuleDocCommentSpace =
    P.bind (P.addLocation (freshLine E.FreshLine)) <|
        \(A.At region ()) ->
            P.oneOfWithFallback
                [ P.bind (Space.docComment E.ImportStart E.ModuleSpace) <|
                    \docComment ->
                        P.doN
                            [ P.do (Space.chomp E.ModuleSpace)
                            , P.do (Space.checkFreshLine E.FreshLine)
                            ]
                        <|
                            P.return (Right docComment)
                ]
                (Left region)



-- HEADER


type Header
    = Header (A.Located Name.Name) Effects (A.Located Src.Exposing) (Either A.Region Src.Comment)


type Effects
    = NoEffects A.Region
    | Ports A.Region
    | Manager A.Region Src.Manager


chompHeader : P.Parser z E.Module (Maybe Header)
chompHeader =
    P.doAndBind
        [ P.do (freshLine E.FreshLine)
        ]
        P.getPosition
    <|
        \start ->
            P.oneOfWithFallback
                [ -- module MyThing exposing (..)
                  P.doAndBind
                    [ P.do (Keyword.module_ E.ModuleProblem)
                    ]
                    P.getPosition
                  <|
                    \effectEnd ->
                        P.doAndBind
                            [ P.do (Space.chompAndCheckIndent E.ModuleSpace E.ModuleProblem)
                            ]
                            (P.addLocation (Var.moduleName E.ModuleName))
                        <|
                            \name ->
                                P.doAndBind
                                    [ P.do (Space.chompAndCheckIndent E.ModuleSpace E.ModuleProblem)
                                    , P.do (Keyword.exposing_ E.ModuleProblem)
                                    , P.do (Space.chompAndCheckIndent E.ModuleSpace E.ModuleProblem)
                                    ]
                                    (P.addLocation (P.specialize E.ModuleExposing exposing_))
                                <|
                                    \exports ->
                                        P.bind chompModuleDocCommentSpace <|
                                            \comment ->
                                                P.return <|
                                                    Just <|
                                                        Header name (NoEffects (A.Region start effectEnd)) exports comment
                , -- port module MyThing exposing (..)
                  P.doAndBind
                    [ P.do (Keyword.port_ E.PortModuleProblem)
                    , P.do (Space.chompAndCheckIndent E.ModuleSpace E.PortModuleProblem)
                    , P.do (Keyword.module_ E.PortModuleProblem)
                    ]
                    P.getPosition
                  <|
                    \effectEnd ->
                        P.doAndBind
                            [ P.do (Space.chompAndCheckIndent E.ModuleSpace E.PortModuleProblem)
                            ]
                            (P.addLocation (Var.moduleName E.PortModuleName))
                        <|
                            \name ->
                                P.doAndBind
                                    [ P.do (Space.chompAndCheckIndent E.ModuleSpace E.PortModuleProblem)
                                    , P.do (Keyword.exposing_ E.PortModuleProblem)
                                    , P.do (Space.chompAndCheckIndent E.ModuleSpace E.PortModuleProblem)
                                    ]
                                    (P.addLocation (P.specialize E.PortModuleExposing exposing_))
                                <|
                                    \exports ->
                                        P.bind chompModuleDocCommentSpace <|
                                            \comment ->
                                                P.return <|
                                                    Just <|
                                                        Header name (Ports (A.Region start effectEnd)) exports comment
                , -- effect module MyThing where { command = MyCmd } exposing (..)
                  P.doAndBind
                    [ P.do (Keyword.effect_ E.Effect)
                    , P.do (Space.chompAndCheckIndent E.ModuleSpace E.Effect)
                    , P.do (Keyword.module_ E.Effect)
                    ]
                    P.getPosition
                  <|
                    \effectEnd ->
                        P.doAndBind
                            [ P.do (Space.chompAndCheckIndent E.ModuleSpace E.Effect)
                            ]
                            (P.addLocation (Var.moduleName E.ModuleName))
                        <|
                            \name ->
                                P.doAndBind
                                    [ P.do (Space.chompAndCheckIndent E.ModuleSpace E.Effect)
                                    , P.do (Keyword.where_ E.Effect)
                                    , P.do (Space.chompAndCheckIndent E.ModuleSpace E.Effect)
                                    ]
                                    chompManager
                                <|
                                    \manager ->
                                        P.doAndBind
                                            [ P.do (Space.chompAndCheckIndent E.ModuleSpace E.Effect)
                                            , P.do (Keyword.exposing_ E.Effect)
                                            , P.do (Space.chompAndCheckIndent E.ModuleSpace E.Effect)
                                            ]
                                            (P.addLocation (P.specialize (always E.Effect) exposing_))
                                        <|
                                            \exports ->
                                                P.bind chompModuleDocCommentSpace <|
                                                    \comment ->
                                                        P.return <|
                                                            Just <|
                                                                Header name (Manager (A.Region start effectEnd) manager) exports comment
                ]
                -- default header
                Nothing


chompManager : P.Parser z E.Module Src.Manager
chompManager =
    P.doN
        [ P.do (P.word1 0x7B {- { -} E.Effect)
        , P.do spaces_em
        ]
    <|
        P.oneOf E.Effect
            [ P.bind chompCommand <|
                \cmd ->
                    P.do1 spaces_em <|
                        P.oneOf E.Effect
                            [ P.doN
                                [ P.do (P.word1 0x7D {- } -} E.Effect)
                                , P.do spaces_em
                                ]
                              <|
                                P.return (Src.CCmd cmd)
                            , P.doAndBind
                                [ P.do (P.word1 0x2C {- , -} E.Effect)
                                , P.do spaces_em
                                ]
                                chompSubscription
                              <|
                                \sub ->
                                    P.doN
                                        [ P.do spaces_em
                                        , P.do (P.word1 0x7D {- } -} E.Effect)
                                        , P.do spaces_em
                                        ]
                                    <|
                                        P.return (Src.Fx cmd sub)
                            ]
            , P.bind chompSubscription <|
                \sub ->
                    P.do1 spaces_em <|
                        P.oneOf E.Effect
                            [ P.doN
                                [ P.do (P.word1 0x7D {- } -} E.Effect)
                                , P.do spaces_em
                                ]
                              <|
                                P.return (Src.CSub sub)
                            , P.doAndBind
                                [ P.do (P.word1 0x2C {- , -} E.Effect)
                                , P.do spaces_em
                                ]
                                chompCommand
                              <|
                                \cmd ->
                                    P.doN
                                        [ P.do spaces_em
                                        , P.do (P.word1 0x7D {- } -} E.Effect)
                                        , P.do spaces_em
                                        ]
                                    <|
                                        P.return (Src.Fx cmd sub)
                            ]
            ]


chompCommand : P.Parser z E.Module (A.Located Name.Name)
chompCommand =
    P.doN
        [ P.do (Keyword.command_ E.Effect)
        , P.do spaces_em
        , P.do (P.word1 0x3D {- = -} E.Effect)
        , P.do spaces_em
        ]
    <|
        P.addLocation (Var.upper E.Effect)


chompSubscription : P.Parser z E.Module (A.Located Name.Name)
chompSubscription =
    P.doN
        [ P.do (Keyword.subscription_ E.Effect)
        , P.do spaces_em
        , P.do (P.word1 0x3D {- = -} E.Effect)
        , P.do spaces_em
        ]
    <|
        P.addLocation (Var.upper E.Effect)


spaces_em : P.Parser z E.Module ()
spaces_em =
    Space.chompAndCheckIndent E.ModuleSpace E.Effect



-- IMPORTS


chompImports : TList Src.Import -> P.Parser z E.Module (TList Src.Import)
chompImports is =
    P.oneOfWithFallback
        [ P.bind chompImport <|
            \i ->
                chompImports (i :: is)
        ]
        (MList.reverse is)


chompImport : P.Parser z E.Module Src.Import
chompImport =
    P.doAndBind
        [ P.do (Keyword.import_ E.ImportStart)
        , P.do (Space.chompAndCheckIndent E.ModuleSpace E.ImportIndentName)
        ]
        (P.addLocation (Var.moduleName E.ImportName))
    <|
        \((A.At (A.Region _ end) _) as name) ->
            P.do1 (Space.chomp E.ModuleSpace) <|
                P.oneOf E.ImportEnd
                    [ P.do1 (Space.checkFreshLine E.ImportEnd) <|
                        P.return <|
                            Src.Import name Nothing (Src.Explicit [])
                    , P.do1 (Space.checkIndent end E.ImportEnd) <|
                        P.oneOf E.ImportAs
                            [ chompAs name
                            , chompExposing name Nothing
                            ]
                    ]


chompAs : A.Located Name.Name -> P.Parser z E.Module Src.Import
chompAs name =
    P.doAndBind
        [ P.do (Keyword.as_ E.ImportAs)
        , P.do (Space.chompAndCheckIndent E.ModuleSpace E.ImportIndentAlias)
        ]
        (Var.upper E.ImportAlias)
    <|
        \alias_ ->
            P.bind P.getPosition <|
                \end ->
                    P.do1 (Space.chomp E.ModuleSpace) <|
                        P.oneOf E.ImportEnd <|
                            [ P.do1 (Space.checkFreshLine E.ImportEnd) <|
                                P.return <|
                                    Src.Import name (Just alias_) (Src.Explicit [])
                            , P.do1 (Space.checkIndent end E.ImportEnd) <|
                                chompExposing name (Just alias_)
                            ]


chompExposing : A.Located Name.Name -> Maybe Name.Name -> P.Parser z E.Module Src.Import
chompExposing name maybeAlias =
    P.doAndBind
        [ P.do (Keyword.exposing_ E.ImportExposing)
        , P.do (Space.chompAndCheckIndent E.ModuleSpace E.ImportIndentExposingList)
        ]
        (P.specialize E.ImportExposingList exposing_)
    <|
        \exposed ->
            P.do1 (freshLine E.ImportEnd) <|
                P.return <|
                    Src.Import name maybeAlias exposed



-- LISTING


exposing_ : P.Parser z E.Exposing Src.Exposing
exposing_ =
    P.doN
        [ P.do (P.word1 0x28 {- ( -} E.ExposingStart)
        , P.do (Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentValue)
        ]
    <|
        P.oneOf E.ExposingValue
            [ P.doN
                [ P.do (P.word2 0x2E 0x2E {- .. -} E.ExposingValue)
                , P.do (Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentEnd)
                , P.do (P.word1 0x29 {- ) -} E.ExposingEnd)
                ]
              <|
                P.return Src.Open
            , P.bind chompExposed <|
                \exposed ->
                    P.do1 (Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentEnd) <|
                        exposingHelp [ exposed ]
            ]


exposingHelp : TList Src.Exposed -> P.Parser z E.Exposing Src.Exposing
exposingHelp revExposed =
    P.oneOf E.ExposingEnd
        [ P.doAndBind
            [ P.do (P.word1 0x2C {- , -} E.ExposingEnd)
            , P.do (Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentValue)
            ]
            chompExposed
          <|
            \exposed ->
                P.do1 (Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentEnd) <|
                    exposingHelp (exposed :: revExposed)
        , P.do1 (P.word1 0x29 {- ) -} E.ExposingEnd) <|
            P.return (Src.Explicit (MList.reverse revExposed))
        ]


chompExposed : P.Parser z E.Exposing Src.Exposed
chompExposed =
    P.bind P.getPosition <|
        \start ->
            P.oneOf E.ExposingValue
                [ P.bind (Var.lower E.ExposingValue) <|
                    \name ->
                        P.bind P.getPosition <|
                            \end ->
                                P.return <| Src.Lower <| A.at start end name
                , P.do1 (P.word1 0x28 {- ( -} E.ExposingValue) <|
                    P.bind (Symbol.operator E.ExposingOperator E.ExposingOperatorReserved) <|
                        \op ->
                            P.do1 (P.word1 0x29 {- ) -} E.ExposingOperatorRightParen) <|
                                P.bind P.getPosition <|
                                    \end ->
                                        P.return <| Src.Operator (A.Region start end) op
                , P.bind (Var.upper E.ExposingValue) <|
                    \name ->
                        P.bind P.getPosition <|
                            \end ->
                                P.do1 (Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentEnd) <|
                                    P.fmap (Src.Upper (A.at start end name)) privacy
                ]


privacy : P.Parser z E.Exposing Src.Privacy
privacy =
    P.oneOfWithFallback
        [ P.doAndBind
            [ P.do (P.word1 0x28 {- ( -} E.ExposingTypePrivacy)
            , P.do (Space.chompAndCheckIndent E.ExposingSpace E.ExposingTypePrivacy)
            ]
            P.getPosition
          <|
            \start ->
                P.do1 (P.word2 0x2E 0x2E {- .. -} E.ExposingTypePrivacy) <|
                    P.bind P.getPosition <|
                        \end ->
                            P.doN
                                [ P.do (Space.chompAndCheckIndent E.ExposingSpace E.ExposingTypePrivacy)
                                , P.do (P.word1 0x29 {- ) -} E.ExposingTypePrivacy)
                                ]
                            <|
                                P.return <|
                                    Src.Public (A.Region start end)
        ]
        Src.Private
