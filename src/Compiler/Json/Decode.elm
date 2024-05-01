{- MANUALLY FORMATTED -}
module Compiler.Json.Decode exposing
  ( fromByteString
  , Decoder, fmap, pure, andMap, return, bind, andThen
  , string
  , customString
  --, bool
  --, int
  , list
  , nonEmptyList
  --, pair
  ----
  , KeyDecoder(..)
  , dict
  , pairs
  , field
  ----
  , oneOf
  , failure
  , mapError
  --
  , Error(..)
  , Problem(..)
  , DecodeExpectation(..)
  , ParseError(..)
  , StringProblem(..)
  )


import Compiler.Data.NonEmptyList as NE
import Compiler.Json.String as Json
import Compiler.Parse.Keyword as K
import Compiler.Parse.Primitives as P exposing (Row, Col)
import Compiler.Reporting.Annotation as A
import Extra.Type.Either exposing (Either(..))
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map
import Extra.Class.Functor as Functor
import Extra.Class.Applicative as Applicative
import Extra.Class.Monad as Monad



-- RUNNERS


fromByteString : Decoder (Either (Error x) a) x a -> String -> Either (Error x) a
fromByteString (Decoder decode) src =
  case P.fromByteString pFile BadEnd src of
    Right ast ->
      decode ast Right (Left << DecodeProblem src)

    Left problem ->
      Left (ParseProblem src problem)



-- DECODERS


type Decoder z x a =
  Decoder
  (
      AST
      -> (a -> z)
      -> (Problem x -> z)
      -> z
  )



-- ERRORS


type Error x
  = DecodeProblem String (Problem x)
  | ParseProblem String ParseError



-- DECODE PROBLEMS


type Problem x
  = Field String (Problem x)
  | Index Int (Problem x)
  | OneOf (Problem x) (TList (Problem x))
  | Failure A.Region x
  | Expecting A.Region DecodeExpectation


type DecodeExpectation
  = TObject
  | TArray
  | TString
  | TBool
  | TInt
  | TObjectWith String
  | TArrayPair Int



-- INSTANCES


fmap : Functor.Fmap a (Decoder z x a) b (Decoder z x b)
fmap func (Decoder decodeA) =
    Decoder <| \ast ok err ->
      let
        ok_ a = ok (func a)
      in
      decodeA ast ok_ err


pure : Applicative.Pure a (Decoder z x a)
pure = return

andMap : Applicative.AndMap (Decoder z x a) (Decoder z x (a -> b)) (Decoder z x b)
andMap (Decoder decodeArg) (Decoder decodeFunc) =
    Decoder <| \ast ok err ->
      let
        okF func =
          let
            okA arg = ok (func arg)
          in
          decodeArg ast okA err
      in
      decodeFunc ast okF err


return : Monad.Return a (Decoder z x a)
return a =
    Decoder <| \_ ok _ ->
      ok a

bind : Monad.Bind a (Decoder z x a) (Decoder z x b)
bind (Decoder decodeA) callback =
    Decoder <| \ast ok err ->
      let
        ok_ a =
          case callback a of
            Decoder decodeB -> decodeB ast ok err
      in
      decodeA ast ok_ err

andThen : Monad.AndThen a (Decoder z x a) (Decoder z x b)
andThen = Monad.andThen bind



-- STRINGS


string : Decoder z x Json.TString
string =
  Decoder <| \(A.At region ast) ok err ->
    case ast of
      String snippet ->
        ok (Json.fromSnippet snippet)

      _ ->
        err (Expecting region TString)


customString : P.Parser (Either x a) x a -> (Row -> Col -> x) -> Decoder z x a
customString parser toBadEnd =
  Decoder <| \(A.At region ast) ok err ->
    case ast of
      String snippet ->
        case P.fromSnippet parser toBadEnd snippet of
          Right a -> ok a
          Left  x -> err (Failure region x)

      _ ->
        err (Expecting region TString)



-- LISTS


list : Decoder z x a -> Decoder z x (TList a)
list decoder =
  Decoder <| \(A.At region ast) ok err ->
    case ast of
      Array asts ->
        listHelp decoder ok err 0 asts []

      _ ->
        err (Expecting region TArray)


listHelp : Decoder b x a -> (TList a -> b) -> (Problem x -> b) -> Int -> TList AST -> TList a -> b
listHelp ((Decoder decodeA) as decoder) ok err i asts revs =
  case asts of
    [] ->
      ok (MList.reverse revs)

    ast::asts_ ->
      let
        ok_ value = listHelp decoder ok err (i+1) asts_ (value::revs)
        err_ prob = err (Index i prob)
      in
      decodeA ast ok_ err_



-- NON-EMPTY LISTS


nonEmptyList : Decoder z x a -> x -> Decoder z x (NE.TList a)
nonEmptyList decoder x =
      bind (list decoder) <| \values ->
      case values of
        v::vs -> return (NE.CList v vs)
        []   -> failure x



-- OBJECTS


type KeyDecoder z x comparable =
  KeyDecoder (P.Parser z x comparable) (Row -> Col -> x)


dict : KeyDecoder (Either x comparable) x comparable -> Decoder z x a -> Decoder z x (Map.Map comparable a)
dict keyDecoder valueDecoder =
  fmap Map.fromList <| pairs keyDecoder valueDecoder


pairs : KeyDecoder (Either x comparable) x comparable -> Decoder z x a -> Decoder z x (TList (comparable, a))
pairs keyDecoder valueDecoder =
  Decoder <| \(A.At region ast) ok err ->
    case ast of
      Object kvs ->
        pairsHelp keyDecoder valueDecoder ok err kvs []

      _ ->
        err (Expecting region TObject)


pairsHelp : KeyDecoder (Either x comparable) x comparable -> Decoder z x a -> (TList (comparable, a) -> z) -> (Problem x -> z) -> TList (P.Snippet, AST) -> TList (comparable, a) -> z
pairsHelp ((KeyDecoder keyParser toBadEnd) as keyDecoder) ((Decoder decodeA) as valueDecoder) ok err kvs revs =
  case kvs of
    [] ->
      ok (MList.reverse revs)

    (snippet, ast) :: kvs_ ->
      case P.fromSnippet keyParser toBadEnd snippet of
        Left x ->
          err (Failure (snippetToRegion snippet) x)

        Right key ->
          let
            ok_ value = pairsHelp keyDecoder valueDecoder ok err kvs_ ((key,value)::revs)
            err_ prob =
              let (P.Snippet fptr off len _ _) = snippet in
              err (Field (String.slice off (off + len) fptr) prob)
          in
          decodeA ast ok_ err_


snippetToRegion : P.Snippet -> A.Region
snippetToRegion (P.Snippet _ _ len row col) =
  A.Region (A.Position row col) (A.Position row (col + len))



-- FIELDS


field : String -> Decoder z x a -> Decoder z x a
field key (Decoder decodeA) =
  Decoder <| \(A.At region ast) ok err ->
    case ast of
      Object kvs ->
        case findField key kvs of
          Just value ->
            let
              err_ prob =
                err (Field key prob)
            in
            decodeA value ok err_

          Nothing ->
            err (Expecting region (TObjectWith key))

      _ ->
        err (Expecting region TObject)


findField : String -> TList (P.Snippet, AST) -> Maybe AST
findField key pairs_ =
  case pairs_ of
    [] ->
      Nothing

    (P.Snippet src off len _ _, value) :: remainingPairs ->
      if key == String.slice off (off + len) src
      then Just value
      else findField key remainingPairs



-- ONE OF


oneOf : TList (Decoder z x a) -> Decoder z x a
oneOf decoders =
  Decoder <| \ast ok err ->
    case decoders of
      Decoder decodeA :: decoders_ ->
        let
          err_ e =
            oneOfHelp ast ok err decoders_ e []
        in
        decodeA ast ok err_

      [] ->
        Debug.todo "Ran into (Json.Decode.oneOf [])"


oneOfHelp : AST -> (a -> b) -> (Problem x -> b) -> TList (Decoder b x a) -> Problem x -> TList (Problem x) -> b
oneOfHelp ast ok err decoders p ps =
  case decoders of
    Decoder decodeA :: decoders_ ->
      let
        err_ p_ =
          oneOfHelp ast ok err decoders_ p_ (p::ps)
      in
      decodeA ast ok err_

    [] ->
      err (oneOfError [] p ps)


oneOfError : TList (Problem x) -> Problem x -> TList (Problem x) -> Problem x
oneOfError problems prob ps =
  case ps of
    [] ->
      OneOf prob problems

    p::ps_ ->
      oneOfError (prob::problems) p ps_



-- FAILURE


failure : x -> Decoder z x a
failure x =
  Decoder <| \(A.At region _) _ err ->
    err (Failure region x)



-- ERRORS


mapError : (x -> y) -> Decoder z x a -> Decoder z y a
mapError func (Decoder decodeA) =
  Decoder <| \ast ok err ->
    let
      err_ prob = err (mapErrorHelp func prob)
    in
    decodeA ast ok err_


mapErrorHelp : (x -> y) -> Problem x -> Problem y
mapErrorHelp func problem =
  case problem of
    Field k p     -> Field k (mapErrorHelp func p)
    Index i p     -> Index i (mapErrorHelp func p)
    OneOf p ps    -> OneOf (mapErrorHelp func p) (MList.map (mapErrorHelp func) ps)
    Failure r x   -> Failure r (func x)
    Expecting r e -> Expecting r e



-- AST


type alias AST =
  A.Located AST_


type AST_
  = Array (TList AST)
  | Object (TList (P.Snippet, AST))
  | String P.Snippet
  | Int Int
  | TRUE
  | FALSE
  | NULL



-- PARSE


type alias Parser z a =
  P.Parser z ParseError a


type ParseError
  = Start Row Col
  | ObjectField Row Col
  | ObjectColon Row Col
  | ObjectEnd Row Col
  | ArrayEnd Row Col
  | StringProblem StringProblem Row Col
  | NoLeadingZeros Row Col
  | NoFloats Row Col
  | BadEnd Row Col

--  PIndex Int ParseError Row Col
--  PField Json.String ParseError Row Col


type StringProblem
  = BadStringEnd
  | BadStringControlChar
  | BadStringEscapeChar
  | BadStringEscapeHex



-- PARSE AST


pFile : Parser z AST
pFile =
      P.bind spaces <| \_ ->
      P.bind pValue <| \value ->
      P.bind spaces <| \_ ->
      P.return value


pValue : Parser z AST
pValue =
  P.addLocation <|
  P.oneOf Start
    [ P.fmap String <| pString Start
    , pObject
    , pArray
    , pInt
    , P.bind (K.k4 0x74 0x72 0x75 0x65      Start) <| \_ -> P.return TRUE
    , P.bind (K.k5 0x66 0x61 0x6C 0x73 0x65 Start) <| \_ -> P.return FALSE
    , P.bind (K.k4 0x6E 0x75 0x6C 0x6C      Start) <| \_ -> P.return NULL
    ]



-- OBJECT


pObject : Parser z AST_
pObject =
      P.bind (P.word1 0x7B {- { -} Start) <| \_ ->
      P.bind spaces <| \_ ->
      P.oneOf ObjectField
        [     P.bind (pField) <| \entry ->
              P.bind spaces <| \_ ->
              pObjectHelp [entry]
        ,     P.bind (P.word1 0x7D {-}-} ObjectEnd) <| \_ ->
              P.return (Object [])
        ]


pObjectHelp : TList (P.Snippet, AST) -> Parser z AST_
pObjectHelp revEntries =
  P.oneOf ObjectEnd
    [
          P.bind (P.word1 0x2C {-,-} ObjectEnd) <| \_ ->
          P.bind spaces <| \_ ->
          P.bind pField <| \entry ->
          P.bind spaces <| \_ ->
          pObjectHelp (entry::revEntries)
    ,
          P.bind (P.word1 0x7D {-}-} ObjectEnd) <| \_ ->
          P.return (Object (MList.reverse revEntries))
    ]


pField : Parser z (P.Snippet, AST)
pField =
      P.bind (pString ObjectField) <| \key ->
      P.bind spaces <| \_ ->
      P.bind (P.word1 0x3A {-:-} ObjectColon) <| \_ ->
      P.bind spaces <| \_ ->
      P.bind (pValue) <| \value ->
      P.return (key, value)



-- ARRAY


pArray : Parser z AST_
pArray =
      P.bind (P.word1 0x5B {-[-} Start) <| \_ ->
      P.bind spaces <| \_ ->
      P.oneOf Start
        [     P.bind (pValue) <| \entry ->
              P.bind spaces <| \_ ->
              pArrayHelp 1 [entry]
        ,     P.bind (P.word1 0x5D {-]-} ArrayEnd) <| \_ ->
              P.return (Array [])
        ]


pArrayHelp : Int -> TList AST -> Parser z AST_
pArrayHelp len revEntries =
  P.oneOf ArrayEnd
    [
          P.bind (P.word1 0x2C {-,-} ArrayEnd) <| \_ ->
          P.bind spaces <| \_ ->
          P.bind pValue <| \entry ->
          P.bind spaces <| \_ ->
          pArrayHelp (len + 1) (entry::revEntries)
    ,
          P.bind (P.word1 0x5D {-]-} ArrayEnd) <| \_ ->
          P.return (Array (MList.reverse revEntries))
    ]



-- STRING


pString : (Row -> Col -> ParseError) -> Parser z P.Snippet
pString start =
  P.Parser <| \(P.State src pos end indent row col) cok _ cerr eerr ->
    if pos < end && P.unsafeIndex src pos == 0x22 {-"-} then

      let
        pos1 = pos + 1
        col1 = col + 1

        ((status, newPos), (newRow, newCol)) =
          pStringHelp src pos1 end row col1
      in
      case status of
        GoodString ->
          let
            off = pos1
            len = newPos - pos1 - 1
            snp = P.Snippet src off len row col1
            newState = P.State src newPos end indent newRow newCol
          in
          cok snp newState

        BadString problem ->
          cerr newRow newCol (StringProblem problem)

    else
      eerr row col start


type StringStatus
  = GoodString
  | BadString StringProblem


pStringHelp : String -> Int -> Int -> Row -> Col -> ((StringStatus, Int), (Row, Col))
pStringHelp src pos end row col =
  if pos >= end then
    ((BadString BadStringEnd, pos), (row, col))

  else
    case P.unsafeIndex src pos of
      0x22 {-"-} ->
        ((GoodString, pos + 1), (row, col + 1))

      0x0A {-\n-} ->
        ((BadString BadStringEnd, pos), (row, col))

      0x5C {-\-} ->
        let pos1 = pos + 1 in
        if pos1 >= end then
          ((BadString BadStringEnd, pos1), (row + 1, col))
        else
          case P.unsafeIndex src pos1 of
            0x22 {-"-} -> pStringHelp src (pos + 2) end row (col + 2)
            0x5C {-\-} -> pStringHelp src (pos + 2) end row (col + 2)
            0x2F {-/-} -> pStringHelp src (pos + 2) end row (col + 2)
            0x62 {-b-} -> pStringHelp src (pos + 2) end row (col + 2)
            0x66 {-f-} -> pStringHelp src (pos + 2) end row (col + 2)
            0x6E {-n-} -> pStringHelp src (pos + 2) end row (col + 2)
            0x72 {-r-} -> pStringHelp src (pos + 2) end row (col + 2)
            0x74 {-t-} -> pStringHelp src (pos + 2) end row (col + 2)
            0x75 {-u-} ->
              let pos6 = pos + 6 in
              if pos6 <= end
                && isHex (P.unsafeIndex src (pos + 2))
                && isHex (P.unsafeIndex src (pos + 3))
                && isHex (P.unsafeIndex src (pos + 4))
                && isHex (P.unsafeIndex src (pos + 5))
              then
                pStringHelp src pos6 end row (col + 6)
              else
                ((BadString BadStringEscapeHex, pos), (row, col))

            _ ->
              ((BadString BadStringEscapeChar, pos), (row, col))

      word ->
        if word < 0x20 then
          ((BadString BadStringControlChar, pos), (row, col))
        else
          let newPos = pos + (P.getCharWidth word) in
          pStringHelp src newPos end row (col + 1)


isHex : Int -> Bool
isHex word =
     0x30 {-0-} <= word && word <= 0x39 {-9-}
  || 0x61 {-a-} <= word && word <= 0x66 {-f-}
  || 0x41 {-A-} <= word && word <= 0x46 {-F-}



-- SPACES


spaces : Parser z ()
spaces =
  P.Parser <| \((P.State src pos end indent row col) as state) cok eok _ _ ->
    let
      (newPos, newRow, newCol) =
        eatSpaces src pos end row col
    in
    if pos == newPos then
      eok () state
    else
      let
        newState =
          P.State src newPos end indent newRow newCol
      in
      cok () newState


eatSpaces : String -> Int -> Int -> Row -> Col -> (Int, Row, Col)
eatSpaces src pos end row col =
  if pos >= end then
    (pos, row, col)

  else
    case P.unsafeIndex src pos of
      0x20 {-  -} -> eatSpaces src (pos + 1) end row (col + 1)
      0x09 {-\t-} -> eatSpaces src (pos + 1) end row (col + 1)
      0x0A {-\n-} -> eatSpaces src (pos + 1) end (row + 1) 1
      0x0D {-\r-} -> eatSpaces src (pos + 1) end row col
      _ ->
        (pos, row, col)



-- INTS


pInt : Parser z AST_
pInt =
  P.Parser <| \(P.State src pos end indent row col) cok _ cerr eerr ->
    if pos >= end then
      eerr row col Start

    else
      let word = P.unsafeIndex src pos in
      if not (isDecimalDigit word) then
        eerr row col Start

      else if word == 0x30 {-0-} then

        let
          pos1 = pos + 1
          newState = P.State src pos1 end indent row (col + 1)
        in
        if pos1 < end then
          let word1 = P.unsafeIndex src pos1 in
          if isDecimalDigit word1 then
            cerr row (col + 1) NoLeadingZeros
          else if word1 == 0x2E {-.-} then
            cerr row (col + 1) NoFloats
          else
            cok (Int 0) newState
        else
          cok (Int 0) newState

      else
        let
          (status, n, newPos) =
            chompInt src (pos + 1) end (word - 0x30 {-0-})

          len = newPos - pos
        in
        case status of
          GoodInt ->
            let
              newState =
                P.State src newPos end indent row (col + len)
            in
            cok (Int n) newState

          BadIntEnd ->
            cerr row (col + len) NoFloats


type IntStatus = GoodInt | BadIntEnd


chompInt : String -> Int -> Int -> Int -> (IntStatus, Int, Int)
chompInt src pos end n =
  if pos < end then
    let word = P.unsafeIndex src pos in
    if isDecimalDigit word then
      let m = 10 * n + word - 0x30 {-0-} in
      chompInt src (pos + 1) end m
    else if word == 0x2E {-.-} || word == 0x65 {-e-} || word == 0x45 {-E-} then
      (BadIntEnd, n, pos)
    else
      (GoodInt, n, pos)

  else
    (GoodInt, n, pos)


isDecimalDigit : Int -> Bool
isDecimalDigit word =
  word <= 0x39 {-9-} && word >= 0x30 {-0-}
