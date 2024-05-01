{- MANUALLY FORMATTED -}
module Compiler.Reporting.Error.Docs exposing
  ( Error(..)
  --, SyntaxProblem(..)
  --, NameProblem(..)
  --, DefProblem(..)
  , toReports
  )

import Compiler.Data.NonEmptyList as NE
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Doc exposing (d)
import Compiler.Reporting.Render.Code as Code
import Compiler.Reporting.Report as Report



type Error
  = NoDocs A.Region



-- TO REPORTS


toReports : Code.Source -> Error -> NE.TList Report.Report
toReports _ _ =
  NE.CList (Report.Report "" A.zero [] (d"")) []
