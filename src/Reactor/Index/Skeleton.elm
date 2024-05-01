module Reactor.Index.Skeleton exposing
    ( BoxArgs
    , StyledBoxArgs
    , box
    , styledBox
    )

import Html exposing (Attribute, Html, a, div, text)
import Html.Attributes exposing (class, style, title)
import Html.Events exposing (onClick)
import Reactor.Index.Icon as Icon



-- VIEW BOXES


type alias BoxArgs msg =
    { title : String
    , titleClick : Maybe msg
    , items : List (List (Html msg))
    , footerClick : Maybe msg
    }


type alias StyledBoxArgs msg =
    { title : String
    , titleClick : Maybe msg
    , titleFlashed : Bool
    , itemsStyle : List (Attribute msg)
    , items : List (List (Html msg))
    , footerClick : Maybe msg
    }


box : BoxArgs msg -> Html msg
box { title, titleClick, items, footerClick } =
    boxWithItems title titleClick False footerClick <|
        realItems [] items


styledBox : StyledBoxArgs msg -> Html msg
styledBox { title, titleClick, titleFlashed, itemsStyle, items, footerClick } =
    boxWithItems title titleClick titleFlashed footerClick <|
        realItems itemsStyle items


boxWithItems : String -> Maybe msg -> Bool -> Maybe msg -> Html msg -> Html msg
boxWithItems boxTitle titleClick titleFlashed footerClick items =
    div [ class "box" ] <|
        [ boxHeader boxTitle titleClick titleFlashed
        , items
        , boxFooter footerClick
        ]


titleClickAttributes : Maybe msg -> List (Attribute msg)
titleClickAttributes titleClick =
    case titleClick of
        Just clickHandler ->
            [ style "cursor" "pointer", onClick clickHandler ]

        Nothing ->
            []


boxHeader : String -> Maybe msg -> Bool -> Html msg
boxHeader boxTitle titleClick titleFlashed =
    div (class (titleClass titleFlashed) :: titleClickAttributes titleClick) [ text boxTitle ]


titleClass : Bool -> String
titleClass flashed =
    if flashed then
        "box-header-flashed"

    else
        "box-header"


realItems : List (Attribute msg) -> List (List (Html msg)) -> Html msg
realItems itemsStyle items =
    div (class "box-items" :: itemsStyle) <| List.map (div [ class "box-item" ]) items


boxFooter : Maybe msg -> Html msg
boxFooter footerClick =
    case footerClick of
        Just clickHandler ->
            a [ title "Save", onClick clickHandler ]
                [ div [ class "box-footer" ] [ Icon.package ] ]

        Nothing ->
            text ""
