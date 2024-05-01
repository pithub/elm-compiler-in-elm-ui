module Compiler.Canonicalize.Effects exposing
    ( canonicalize
    , checkPayload
    )

import Compiler.AST.Canonical as Can
import Compiler.AST.Source as Src
import Compiler.AST.Utils.Type as UType
import Compiler.Canonicalize.Environment as Env
import Compiler.Canonicalize.Type as Type
import Compiler.Data.Name as Name
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Canonicalize as Error
import Compiler.Reporting.Result as MResult
import Extra.Type.Either as Either exposing (Either(..))
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map



-- RESULT


type alias TResult z i w a =
    MResult.TResult z i w Error.Error a



-- CANONICALIZE


canonicalize :
    Env.Env
    -> TList (A.Located Src.Value)
    -> Map.Map Name.Name union
    -> Src.Effects
    -> TResult z i w Can.Effects
canonicalize env values unions effects =
    case effects of
        Src.NoEffects ->
            MResult.ok Can.NoEffects

        Src.Ports ports ->
            MResult.bind (MResult.traverseList (canonicalizePort env) ports) <|
                \pairs ->
                    MResult.pure <| Can.Ports (Map.fromList pairs)

        Src.Manager region manager ->
            let
                dict =
                    Map.fromList (MList.map toNameRegion values)
            in
            MResult.ok Can.Manager
                |> MResult.andMap (verifyManager region dict "init")
                |> MResult.andMap (verifyManager region dict "onEffects")
                |> MResult.andMap (verifyManager region dict "onSelfMsg")
                |> MResult.andMap
                    (case manager of
                        Src.CCmd cmdType ->
                            MResult.ok Can.CCmd
                                |> MResult.andMap (verifyEffectType cmdType unions)
                                |> MResult.discard (verifyManager region dict "cmdMap")

                        Src.CSub subType ->
                            MResult.ok Can.CSub
                                |> MResult.andMap (verifyEffectType subType unions)
                                |> MResult.discard (verifyManager region dict "subMap")

                        Src.Fx cmdType subType ->
                            MResult.ok Can.Fx
                                |> MResult.andMap (verifyEffectType cmdType unions)
                                |> MResult.andMap (verifyEffectType subType unions)
                                |> MResult.discard (verifyManager region dict "cmdMap")
                                |> MResult.discard (verifyManager region dict "subMap")
                    )



-- CANONICALIZE PORT


canonicalizePort : Env.Env -> Src.Port -> TResult z i w ( Name.Name, Can.Port )
canonicalizePort env (Src.Port (A.At region portName) tipe) =
    MResult.bind (Type.toAnnotation env tipe) <|
        \(Can.Forall freeVars ctipe) ->
            case MList.reverse (UType.delambda (UType.deepDealias ctipe)) of
                (Can.TType home name [ msg ]) :: revArgs ->
                    if home == ModuleName.cmd && name == Name.cmd then
                        case revArgs of
                            [] ->
                                MResult.throw (Error.PortTypeInvalid region portName Error.CmdNoArg)

                            [ outgoingType ] ->
                                case msg of
                                    Can.TVar _ ->
                                        case checkPayload outgoingType of
                                            Right () ->
                                                MResult.ok ( portName, Can.Outgoing freeVars outgoingType ctipe )

                                            Left ( badType, err ) ->
                                                MResult.throw (Error.PortPayloadInvalid region portName badType err)

                                    _ ->
                                        MResult.throw (Error.PortTypeInvalid region portName Error.CmdBadMsg)

                            _ ->
                                MResult.throw (Error.PortTypeInvalid region portName (Error.CmdExtraArgs (MList.length revArgs)))

                    else if home == ModuleName.sub && name == Name.sub then
                        case revArgs of
                            [ Can.TLambda incomingType (Can.TVar msg1) ] ->
                                case msg of
                                    Can.TVar msg2 ->
                                        if msg1 == msg2 then
                                            case checkPayload incomingType of
                                                Right () ->
                                                    MResult.ok ( portName, Can.Incoming freeVars incomingType ctipe )

                                                Left ( badType, err ) ->
                                                    MResult.throw (Error.PortPayloadInvalid region portName badType err)

                                        else
                                            MResult.throw (Error.PortTypeInvalid region portName Error.SubBad)

                                    _ ->
                                        MResult.throw (Error.PortTypeInvalid region portName Error.SubBad)

                            _ ->
                                MResult.throw (Error.PortTypeInvalid region portName Error.SubBad)

                    else
                        MResult.throw (Error.PortTypeInvalid region portName Error.NotCmdOrSub)

                _ ->
                    MResult.throw (Error.PortTypeInvalid region portName Error.NotCmdOrSub)



-- VERIFY MANAGER


verifyEffectType : A.Located Name.Name -> Map.Map Name.Name a -> TResult z i w Name.Name
verifyEffectType (A.At region name) unions =
    if Map.member name unions then
        MResult.ok name

    else
        MResult.throw (Error.EffectNotFound region name)


toNameRegion : A.Located Src.Value -> ( Name.Name, A.Region )
toNameRegion (A.At _ (Src.Value (A.At region name) _ _ _)) =
    ( name, region )


verifyManager : A.Region -> Map.Map Name.Name A.Region -> Name.Name -> TResult z i w A.Region
verifyManager tagRegion values name =
    case Map.lookup name values of
        Just region ->
            MResult.ok region

        Nothing ->
            MResult.throw (Error.EffectFunctionNotFound tagRegion name)



-- CHECK PAYLOAD TYPES


checkPayload : Can.Type -> Either ( Can.Type, Error.InvalidPayload ) ()
checkPayload tipe =
    case tipe of
        Can.TAlias _ _ args aliasedType ->
            checkPayload (UType.dealias args aliasedType)

        Can.TType home name args ->
            case args of
                [] ->
                    if isJson home name then
                        Right ()

                    else if isString home name then
                        Right ()

                    else if isIntFloatBool home name then
                        Right ()

                    else
                        Left ( tipe, Error.UnsupportedType name )

                [ arg ] ->
                    if isList home name then
                        checkPayload arg

                    else if isMaybe home name then
                        checkPayload arg

                    else if isArray home name then
                        checkPayload arg

                    else
                        Left ( tipe, Error.UnsupportedType name )

                _ ->
                    Left ( tipe, Error.UnsupportedType name )

        Can.TUnit ->
            Right ()

        Can.TTuple a b maybeC ->
            Either.bind (checkPayload a) <|
                \() ->
                    Either.bind (checkPayload b) <|
                        \() ->
                            case maybeC of
                                Nothing ->
                                    Right ()

                                Just c ->
                                    checkPayload c

        Can.TVar name ->
            Left ( tipe, Error.TypeVariable name )

        Can.TLambda _ _ ->
            Left ( tipe, Error.Function )

        Can.TRecord _ (Just _) ->
            Left ( tipe, Error.ExtendedRecord )

        Can.TRecord fields Nothing ->
            Map.traverse_ Right Either.liftA2 checkFieldPayload fields


checkFieldPayload : Can.FieldType -> Either ( Can.Type, Error.InvalidPayload ) ()
checkFieldPayload (Can.FieldType _ tipe) =
    checkPayload tipe


isIntFloatBool : ModuleName.Canonical -> Name.Name -> Bool
isIntFloatBool home name =
    (home == ModuleName.basics) && (name == Name.int || name == Name.float || name == Name.bool)


isString : ModuleName.Canonical -> Name.Name -> Bool
isString home name =
    (home == ModuleName.string) && (name == Name.string)


isJson : ModuleName.Canonical -> Name.Name -> Bool
isJson home name =
    (home == ModuleName.jsonEncode) && (name == Name.value)


isList : ModuleName.Canonical -> Name.Name -> Bool
isList home name =
    (home == ModuleName.list) && (name == Name.list)


isMaybe : ModuleName.Canonical -> Name.Name -> Bool
isMaybe home name =
    (home == ModuleName.maybe) && (name == Name.maybe)


isArray : ModuleName.Canonical -> Name.Name -> Bool
isArray home name =
    (home == ModuleName.array) && (name == Name.array)
