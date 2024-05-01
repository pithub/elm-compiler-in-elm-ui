module Compiler.Reporting.Warning exposing
    ( Context(..)
    , Warning(..)
    )

import Compiler.AST.Canonical as Can
import Compiler.Data.Name as Name
import Compiler.Reporting.Annotation as A



-- ALL POSSIBLE WARNINGS


type Warning
    = -- TODO UnusedImport can be removed
      UnusedImport A.Region Name.Name
    | UnusedVariable A.Region Context Name.Name
    | MissingTypeAnnotation A.Region Name.Name Can.Type


type Context
    = Def
    | Pattern
