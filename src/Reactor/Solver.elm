module Reactor.Solver exposing (validate)

import Builder.Deps.Registry as Registry
import Builder.Deps.Solver as Solver
import Builder.Elm.Details as Details
import Builder.Elm.Outline as Outline
import Builder.Http as Http
import Builder.Reporting.Exit as Exit
import Builder.Reporting.Task as Task
import Builder.Stuff as Stuff
import Compiler.Elm.Constraint as Con
import Compiler.Elm.Package as Pkg
import Compiler.Elm.Version as V
import Extra.System.File exposing (FilePath)
import Extra.System.IO as IO
import Extra.Type.Either exposing (Either)
import Extra.Type.Map as Map


type alias IO a d e f g h v =
    IO.IO (Details.State a d e f g h) v


validate : FilePath -> IO a d e f g h (Either Exit.Details Details.ValidOutline)
validate root =
    Task.run (validateTask root)


validateTask : FilePath -> Task (Either Exit.Details Details.ValidOutline) a d e f g h Details.ValidOutline
validateTask root =
    Task.bind (initEnv root) <|
        \( env, outline ) ->
            case outline of
                Outline.Pkg pkg ->
                    verifyPkg env pkg

                Outline.App app ->
                    verifyApp env app


solverEnv : Task z a d e f g h Solver.Env
solverEnv =
    Task.eio Exit.DetailsCannotGetRegistry Solver.initEnv


readOutline : FilePath -> Task z a d e f g h Outline.Outline
readOutline root =
    Task.eio Exit.DetailsBadOutline (Outline.read root)



-- ENV


type Env
    = Env {- root -} FilePath {- cache -} Stuff.PackageCache {- manager -} Http.Manager {- connection -} Solver.Connection {- registry -} Registry.Registry


initEnv : FilePath -> Task z a d e f g h ( Env, Outline.Outline )
initEnv root =
    Task.bind solverEnv <|
        \(Solver.Env cache manager connection registry) ->
            Task.bind (readOutline root) <|
                \outline ->
                    Task.return <| ( Env root cache manager connection registry, outline )



-- VERIFY PROJECT


type alias Task z a d e f g h v =
    Task.Task z (Details.State a d e f g h) Exit.Details v


verifyPkg : Env -> Outline.PkgOutline -> Task z a d e f g h Details.ValidOutline
verifyPkg env (Outline.PkgOutline pkg _ _ _ exposed direct testDirect elm) =
    if Con.goodElm elm then
        Task.bind (Task.andThen (verifyConstraints env) <| union noDups direct testDirect) <|
            \solution ->
                let
                    exposedList =
                        Outline.flattenExposed exposed

                    exactDeps =
                        Map.map (\(Solver.Details v _) -> v) solution
                in
                Task.return (Details.ValidPkg pkg exposedList exactDeps)

    else
        Task.throw <| Exit.DetailsBadElmInPkg elm


verifyApp : Env -> Outline.AppOutline -> Task z a d e f g h Details.ValidOutline
verifyApp env ((Outline.AppOutline elmVersion srcDirs _ _ _ _) as outline) =
    if elmVersion == V.compiler then
        Task.bind (checkAppDeps outline) <|
            \stated ->
                Task.bind (verifyConstraints env (Map.map Con.exactly stated)) <|
                    \actual ->
                        if Map.size stated == Map.size actual then
                            Task.return (Details.ValidApp srcDirs)

                        else
                            Task.throw <| Exit.DetailsHandEditedDependencies

    else
        Task.throw <| Exit.DetailsBadElmInAppOutline elmVersion


checkAppDeps : Outline.AppOutline -> Task z a d e f g h (Map.Map Pkg.Comparable V.Version)
checkAppDeps (Outline.AppOutline _ _ direct indirect testDirect testIndirect) =
    Task.bind (union allowEqualDups indirect testDirect) <|
        \x ->
            Task.bind (union noDups direct testIndirect) <|
                \y ->
                    union noDups x y



-- VERIFY CONSTRAINTS


verifyConstraints : Env -> Map.Map Pkg.Comparable Con.Constraint -> Task z a d e f g h (Map.Map Pkg.Comparable Solver.Details)
verifyConstraints (Env _ cache _ connection registry) constraints =
    Task.bind (Task.io <| Solver.verify cache connection registry constraints) <|
        \result ->
            case result of
                Solver.Ok details ->
                    Task.return details

                Solver.NoSolution ->
                    Task.throw <| Exit.DetailsNoSolution

                Solver.NoOfflineSolution ->
                    Task.throw <| Exit.DetailsNoOfflineSolution

                Solver.Err exit ->
                    Task.throw <| Exit.DetailsSolverProblem exit



-- UNION


union : (comparable -> v -> v -> Task z a d e f g h v) -> Map.Map comparable v -> Map.Map comparable v -> Task z a d e f g h (Map.Map comparable v)
union tieBreaker deps1 deps2 =
    Map.mergeA Task.pure Task.liftA2 (Map.preserveMissing Task.pure) (Map.preserveMissing Task.pure) (Map.zipWithAMatched Task.fmap tieBreaker) deps1 deps2


noDups : comparable -> v -> v -> Task z a d e f g h v
noDups _ _ _ =
    Task.throw Exit.DetailsHandEditedDependencies


allowEqualDups : comparable -> v -> v -> Task z a d e f g h v
allowEqualDups _ v1 v2 =
    if v1 == v2 then
        Task.return v1

    else
        Task.throw Exit.DetailsHandEditedDependencies
