module ColorPicker exposing (Color, State, availableColors, defaultColor, init, view)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)


type alias Color =
    { name : String
    , color : String
    , textColor : String
    }


defaultColor =
    Color "Noir" "#000000" "#fff"


availableColors : List Color
availableColors =
    [ Color "Noir" "#000000" "#fff"
    , Color "Blanc" "#ffffff" "#000"
    , Color "Marron" "#582900" "#fff"
    , Color "Gris" "#909090" "#000"
    , Color "Bleu Roi" "#318CE7" "#000"
    , Color "Rouge" "#E71837" "#000"
    , Color "Jaune D'or" "#EFD807" "#000"
    , Color "Vert" "#49B675" "#000"
    , Color "Orange" "#FC9303" "#000"
    , Color "Brique" "#C8927F" "#000"
    , Color "Turquoise" "#06B8B9" "#000"
    , Color "Vieux Rose" "#D6B3BA" "#000"
    , Color "Framboise" "#C72C48" "#000"
    , Color "Prune" "#654856" "#fff"
    , Color "Vert Sapin" "#849c82" "#000"
    , Color "Jaune Citron" "#f7ff3c" "#000"
    , Color "Bleu Canard" "#048B9A" "#000"
    , Color "Bordeaux" "#6d071a" "#fff"
    , Color "Nature" "#d3d1c6" "#000"
    , Color "Bleu Électrique" "#2c75ff" "#000"
    ]


type alias State =
    { colors : List Color
    , selectedColor : Color
    , open : Bool
    }


init : State
init =
    { colors = availableColors
    , selectedColor = defaultColor
    , open = False
    }


handleEvent : (State -> msg) -> Msg -> State -> msg
handleEvent toMsg msg =
    update msg >> toMsg


type Msg
    = SetColor Color
    | Open
    | Close


update : Msg -> State -> State
update msg state =
    case msg of
        SetColor color ->
            { state | selectedColor = color, open = False }

        Open ->
            { state | open = True }

        Close ->
            { state | open = False }


view : (State -> msg) -> State -> Html msg
view toMsg state =
    let
        viewColor ({ name, color, textColor } as option) =
            div
                [ class "color"
                , style "background-color" color
                , style "color" textColor
                , onClick <| handleEvent toMsg (SetColor option) state
                ]
                [ name |> text ]
    in
    if state.open then
        div [ class "colorpicker open" ] (state.colors |> List.map viewColor)

    else
        div [ class "colorpicker closed" ]
            [ div
                [ class "color-box"
                , onClick <| handleEvent toMsg Open state
                , style "background-color" state.selectedColor.color
                , style "color" state.selectedColor.textColor
                ]
                []
            , div [ class "color-name" ] [ text state.selectedColor.name ]
            ]
