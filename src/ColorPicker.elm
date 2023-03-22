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
    , Color "Jaune D'or" "#FFB807" "#000"
    , Color "Jaune Citron" "#F9FC05" "#000"
    , Color "Brique" "#C43102" "#000"
    , Color "Vert" "#1B5F02" "#fff"
    , Color "Vert Sapin" "#081C00" "#fff"
    , Color "Vert emeraude" "#055229" "#fff"
    , Color "Vert menthe à l'eau" "#20C459" "#000"
    , Color "Marron" "#461701" "#fff"
    , Color "Vieux Rose" "#FEC9C9" "#000"
    , Color "Rouge sang" "#B90202" "#000"
    , Color "Rouge" "#F41717" "#000"
    , Color "Framboise" "#D20137" "#000"
    , Color "Orange" "#FC5A0F" "#000"
    , Color "Gris" "#5B4F4A" "#000"
    , Color "Prune" "#630512" "#fff"
    , Color "Ocre" "#C98817" "#fff"
    , Color "Bleu Roi" "#02013A" "#fff"
    , Color "Bleu Canard" "#083A65" "#fff"
    , Color "Bleu Électrique" "#0690F0" "#000"
    , Color "Turquoise" "#299B7D" "#000"
    , Color "Framboise" "#C72C48" "#000"
    , Color "Nature" "#BAB6A6" "#000"
    , Color "Taupe" "#81423A" "#000"
    , Color "Bordeaux" "#4B0212" "#fff"
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


view : (State -> msg) -> State -> Char -> Bool -> Html msg
view toMsg state char displayChar =
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
            , let
                colorChar =
                    if displayChar == True then
                        " (" ++ (char |> String.fromChar) ++ ")"

                    else
                        ""
              in
              div [ class "color-name" ] [ state.selectedColor.name ++ colorChar |> text ]
            ]
