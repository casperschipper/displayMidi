module Piet exposing (pick, wrap)

import Color exposing (Color, rgb255)
import List.Extra as LE


pietRed : Color
pietRed =
    rgb255 220 64 42


pietBlue : Color
pietBlue =
    rgb255 0 62 114


pietYellow : Color
pietYellow =
    rgb255 241 191 76


pietBlack : Color
pietBlack =
    rgb255 13 17 9


pietWhite : Color
pietWhite =
    rgb255 182 190 199


allColors =
    [ pietRed, pietYellow, pietBlue, pietYellow, pietBlack]


pick : Float -> Color
pick x =
    let
        n =
            List.length allColors

        i =
            toFloat n * x |> floor
    in
    LE.getAt i allColors |> Maybe.withDefault pietBlack


wrap : Int -> Color
wrap i =
    let
        idx =
            modBy (List.length allColors) i
    in
    LE.getAt idx allColors |> Maybe.withDefault pietBlack
