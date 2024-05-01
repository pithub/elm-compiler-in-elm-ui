module Compiler.Reporting.Report exposing (Report(..))

import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Doc as D
import Extra.Type.List exposing (TList)



-- BUILD REPORTS


type Report
    = Report
        --{ title : String
        --, region : A.Region
        --, sgstns : List_ String
        --, message : D.Doc
        --}
        String
        A.Region
        (TList String)
        D.Doc
