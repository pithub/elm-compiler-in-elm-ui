module Reactor.Index.Navigator exposing (view)

import Html exposing (Html, a, div, span, text)
import Html.Attributes exposing (style, title)
import Html.Events exposing (onClick)
import Reactor.Index.Icon as Icon



-- VIEW


view : (List String -> msg) -> String -> List String -> Html msg
view cmd root dirs =
    div
        [ style "font-size" "2em"
        , style "padding" "20px 0"
        , style "display" "flex"
        , style "align-items" "center"
        , style "height" "40px"
        ]
        (makeLinks cmd root dirs [] [])


makeLinks : (List String -> msg) -> String -> List String -> List String -> List (Html msg) -> List (Html msg)
makeLinks cmd root dirs oldPath revAnchors =
    case dirs of
        dir :: otherDirs ->
            let
                newPath =
                    dir :: oldPath

                anchor =
                    a [ onClick (cmd newPath) ] [ text dir ]
            in
            makeLinks cmd root otherDirs newPath (anchor :: revAnchors)

        [] ->
            let
                home =
                    a [ onClick (cmd []), title root, style "display" "inherit" ] [ Icon.home ]
            in
            case revAnchors of
                [] ->
                    [ home ]

                lastAnchor :: otherRevAnchors ->
                    home :: slash :: List.foldl addSlash [ lastAnchor ] otherRevAnchors


addSlash : Html msg -> List (Html msg) -> List (Html msg)
addSlash front back =
    front :: slash :: back


slash : Html msg
slash =
    span [ style "padding" "0 8px" ] [ text "/" ]
