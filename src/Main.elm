module Main exposing (..)

import Array exposing (Array)
import Browser
import ColorPicker as Picker exposing (Color)
import Dict exposing (Dict)
import Html exposing (Html, a, button, div, hr, img, input, label, li, option, select, span, table, td, text, tr, ul)
import Html.Attributes exposing (class, for, href, id, src, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Patterns as Patterns exposing (Pattern, PatternType(..))
import Random as Random
import Random.List exposing (shuffle)
import Task as Task


type View
    = SeePattern
    | CreatePattern



---- MODEL ----


type alias Model =
    { selectedPattern : Pattern
    , pickers : Array Picker.State
    , repeatsX : Int
    , repeatsY : Int
    , currentView : View
    }


init : ( Model, Cmd Msg )
init =
    ( { selectedPattern = Patterns.dano
      , pickers = Array.repeat 6 Picker.init
      , repeatsX = 10
      , repeatsY = 4
      , currentView = SeePattern
      }
    , Task.perform (always <| GenerateRandomColors) (Task.succeed ())
    )



---- UPDE ----


type Msg
    = PickerChanged Int Picker.State
    | SetPattern String
    | GenerateRandomColors
    | ColorsGenerated (List Color)
    | SetRepeatsX Int
    | SetRepeatsY Int
    | SetView View


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PickerChanged number state ->
            let
                newPickers =
                    model.pickers |> Array.set number state
            in
            ( { model | pickers = newPickers }, Cmd.none )

        SetPattern name ->
            let
                pattern =
                    case name of
                        "Dartlo" ->
                            Patterns.dartlo

                        "Dano" ->
                            Patterns.dano

                        "Alazani" ->
                            Patterns.alazani

                        "Shenako" ->
                            Patterns.shenako

                        _ ->
                            Patterns.dano
            in
            ( { model | selectedPattern = pattern }, Cmd.none )

        SetRepeatsX repeats ->
            ( { model | repeatsX = repeats }, Cmd.none )

        SetRepeatsY repeats ->
            ( { model | repeatsY = repeats }, Cmd.none )

        GenerateRandomColors ->
            ( model, Random.generate ColorsGenerated (shuffle Picker.availableColors) )

        ColorsGenerated randomColors ->
            let
                applyColor picker color =
                    { picker | selectedColor = color }

                pickers =
                    List.map2 applyColor (model.pickers |> Array.toList) (randomColors |> List.take 6)
            in
            ( { model | pickers = pickers |> Array.fromList }
            , Cmd.none
            )

        SetView newView ->
            ( { model | currentView = newView }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "d-flex flex-nowrap" ]
        [ viewMenu model
        , case model.currentView of
            CreatePattern ->
                div [] []

            SeePattern ->
                viewPattern model
        ]


viewMenu : Model -> Html Msg
viewMenu model =
    let
        pickerView x =
            Picker.view (PickerChanged x) (model.pickers |> Array.get x |> Maybe.withDefault Picker.init)

        generatePickers =
            List.range 0 5 |> List.map pickerView
    in
    div
        [ id "menu"
        , class "d-flex flex-column flex-shrink-0 p-3 bg-light"
        ]
        [ a
            [ href "/"
            , class "mb-3 mb-md-0"
            ]
            [ img
                [ src "logo.webp"
                ]
                []
            ]
        , ul
            [ class "nav nav-pills flex-column mb-auto"
            ]
            [ li [] [ viewPatternSelector ]
            , li [] generatePickers
            , li [] [ button [ type_ "button", class "btn btn-link", onClick GenerateRandomColors ] [ "Couleurs al??atoires" |> text ] ]
            , li [] [ button [ type_ "button", class "btn btn-link", onClick (SetView CreatePattern) ] [ "Cr??er un motif" |> text ] ]
            , li [] [ viewRepeats model model.repeatsX SetRepeatsX "horizontales" ]
            , li [] [ viewRepeats model model.repeatsY SetRepeatsY "verticales" ]
            ]
        ]


viewRepeats : Model -> Int -> (Int -> Msg) -> String -> Html Msg
viewRepeats model repeats msg txt =
    case model.selectedPattern.type_ of
        Repeats ->
            div [ class "repeats" ]
                [ label [ for "repeats", class "form-label" ] [ "R??p??titions " ++ txt ++ " (" ++ (repeats |> String.fromInt) ++ ")" |> text ]
                , input
                    [ onInput (String.toInt >> Maybe.withDefault 0 >> msg)
                    , type_ "range"
                    , class "form-range"
                    , id "repeats"
                    , value (repeats |> String.fromInt)
                    , Html.Attributes.min "1"
                    , Html.Attributes.max "20"
                    ]
                    []
                ]

        Long ->
            div [] []


viewPatternSelector : Html Msg
viewPatternSelector =
    let
        optionView pattern =
            option
                [ value pattern.name ]
                [ pattern.name |> text ]
    in
    div []
        [ select
            [ onInput SetPattern
            , class "form-select"
            ]
            (Patterns.availablePatterns |> List.map optionView)
        ]


viewPattern : Model -> Html Msg
viewPattern { selectedPattern, pickers, repeatsX, repeatsY } =
    let
        colors =
            pickers |> Array.map .selectedColor |> Array.toList

        patternContent =
            selectedPattern.content

        colorMatch =
            List.map2 Tuple.pair (" xo/cl" |> String.toList) colors |> Dict.fromList

        colorFromChar char =
            colorMatch |> Dict.get char |> Maybe.withDefault Picker.defaultColor |> .color

        displayPatternChar char =
            td [ style "background-color" (colorFromChar char) ] [ text " " ]

        displayPatternLine chars =
            tr [] (chars |> List.map displayPatternChar)

        patternWithHorizontalRepetition =
            case selectedPattern.type_ of
                Patterns.Repeats ->
                    patternContent |> List.map (\x -> List.repeat repeatsX x |> List.concat)

                Patterns.Long ->
                    patternContent

        patternWithVerticalRepetition =
            case selectedPattern.type_ of
                Patterns.Long ->
                    patternWithHorizontalRepetition

                Patterns.Repeats ->
                    patternWithHorizontalRepetition |> List.repeat repeatsY |> List.concat
    in
    div [ class "container" ]
        [ table [] (patternWithVerticalRepetition |> List.map displayPatternLine)
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
