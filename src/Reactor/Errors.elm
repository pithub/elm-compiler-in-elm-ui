module Reactor.Errors exposing (viewError)

-- Extracted from https://github.com/elm/compiler/blob/master/reactor/src/Errors.elm

import Elm.Error
import Extra.Type.List as MList exposing (TList)
import Html
import Html.Attributes
import String



-- VIEW


viewError : Elm.Error.Error -> Html.Html msg
viewError error =
    Html.div
        [ Html.Attributes.style "display" "inline-block"
        , Html.Attributes.style "color" "rgb(233, 235, 235)"
        , Html.Attributes.style "background-color" "black"
        , Html.Attributes.style "font-family" "'Source Code Pro', monospace"
        , Html.Attributes.style "font-size" "smaller"
        , Html.Attributes.style "white-space" "pre-wrap"
        , Html.Attributes.style "padding" "1em"
        ]
        (viewErrorHelp error)


viewErrorHelp : Elm.Error.Error -> TList (Html.Html msg)
viewErrorHelp error =
    case error of
        Elm.Error.GeneralProblem { path, title, message } ->
            viewHeader title path :: viewMessage message

        Elm.Error.ModuleProblems badModules ->
            viewBadModules badModules



-- VIEW HEADER


viewHeader : String -> Maybe String -> Html.Html msg
viewHeader title maybeFilePath =
    let
        left =
            "-- " ++ title ++ " "

        right =
            case maybeFilePath of
                Nothing ->
                    ""

                Just filePath ->
                    " " ++ filePath
    in
    Html.span
        [ Html.Attributes.style "color" "rgb(51,187,200)" ]
        [ Html.text (fill left right ++ "\n\n") ]


fill : String -> String -> String
fill left right =
    left ++ String.repeat (80 - String.length left - String.length right) "-" ++ right



-- VIEW BAD MODULES


viewBadModules : TList Elm.Error.BadModule -> TList (Html.Html msg)
viewBadModules badModules =
    case badModules of
        [] ->
            []

        [ badModule ] ->
            [ viewBadModule badModule ]

        a :: b :: cs ->
            viewBadModule a :: viewSeparator a.name b.name :: viewBadModules (b :: cs)


viewBadModule : Elm.Error.BadModule -> Html.Html msg
viewBadModule { path, problems } =
    Html.span [] (MList.map (viewProblem path) problems)


viewProblem : String -> Elm.Error.Problem -> Html.Html msg
viewProblem filePath problem =
    Html.span [] (viewHeader problem.title (Just filePath) :: viewMessage problem.message)


viewSeparator : String -> String -> Html.Html msg
viewSeparator before after =
    Html.span
        [ Html.Attributes.style "color" "rgb(211,56,211)" ]
        [ Html.text <|
            String.padLeft 80 ' ' (before ++ "  ↑    ")
                ++ "\n"
                ++ "====o======================================================================o====\n"
                ++ "    ↓  "
                ++ after
                ++ "\n\n\n"
        ]



-- VIEW MESSAGE


viewMessage : TList Elm.Error.Chunk -> TList (Html.Html msg)
viewMessage chunks =
    case chunks of
        [] ->
            [ Html.text "\n\n" ]

        chunk :: others ->
            let
                htmlChunk =
                    case chunk of
                        Elm.Error.Unstyled string ->
                            Html.text string

                        Elm.Error.Styled style string ->
                            Html.span (styleToAttrs style) [ Html.text string ]
            in
            htmlChunk :: viewMessage others


styleToAttrs : Elm.Error.Style -> TList (Html.Attribute msg)
styleToAttrs { bold, underline, color } =
    addBold bold <| addUnderline underline <| addColor color []


addBold : Bool -> TList (Html.Attribute msg) -> TList (Html.Attribute msg)
addBold bool attrs =
    if bool then
        Html.Attributes.style "font-weight" "bold" :: attrs

    else
        attrs


addUnderline : Bool -> TList (Html.Attribute msg) -> TList (Html.Attribute msg)
addUnderline bool attrs =
    if bool then
        Html.Attributes.style "text-decoration" "underline" :: attrs

    else
        attrs


addColor : Maybe Elm.Error.Color -> TList (Html.Attribute msg) -> TList (Html.Attribute msg)
addColor maybeColor attrs =
    case maybeColor of
        Nothing ->
            attrs

        Just color ->
            Html.Attributes.style "color" (colorToCss color) :: attrs


colorToCss : Elm.Error.Color -> String
colorToCss color =
    case color of
        Elm.Error.Red ->
            "rgb(194,54,33)"

        Elm.Error.RED ->
            "rgb(252,57,31)"

        Elm.Error.Magenta ->
            "rgb(211,56,211)"

        Elm.Error.MAGENTA ->
            "rgb(249,53,248)"

        Elm.Error.Yellow ->
            "rgb(173,173,39)"

        Elm.Error.YELLOW ->
            "rgb(234,236,35)"

        Elm.Error.Green ->
            "rgb(37,188,36)"

        Elm.Error.GREEN ->
            "rgb(49,231,34)"

        Elm.Error.Cyan ->
            "rgb(51,187,200)"

        Elm.Error.CYAN ->
            "rgb(20,240,240)"

        Elm.Error.Blue ->
            "rgb(73,46,225)"

        Elm.Error.BLUE ->
            "rgb(88,51,255)"

        Elm.Error.White ->
            "rgb(203,204,205)"

        Elm.Error.WHITE ->
            "rgb(233,235,235)"

        Elm.Error.Black ->
            "rgb(0,0,0)"

        Elm.Error.BLACK ->
            "rgb(129,131,131)"
