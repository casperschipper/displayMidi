module Main exposing
    ( ColoredChar(..)
    , Config(..)
    , Evt(..)
    , Line(..)
    , MidiEvent(..)
    , Model(..)
    , Msg(..)
    , Pitch(..)
    , Screen(..)
    , Time(..)
    , Velo(..)
    , asHtml
    , decodeCsv
    , decoder
    , displayEvts
    , displayLines
    , fromData
    , init
    , lineToHtml
    , main
    , midiEventToString
    , noteToLine
    , numberToDigits
    , subscriptions
    , toString
    , update
    , veloToColor
    , view
    , white
    , whiteLine
    , withColor
    )

import Browser
import Color exposing (Color, rgba)
import Csv.Decode as Decode exposing (Decoder)
import Html exposing (Html, br, div, p, pre, span, text)
import Html.Attributes as Attr exposing (style)
import Http
import List.Extra as LE


type Model
    = Loading
    | Loaded String
    | Parsed (Html Msg)
    | Failed Http.Error
    | CSVError String


type Msg
    = GotMidiCSV (Result Http.Error String)


type Pitch
    = Pitch Int


type Velo
    = Velo Int


type Time
    = Time Int


type MidiEvent
    = NoteOn Pitch Velo
    | NoteOff Pitch Velo
    | Ignore


type Evt
    = Evt Time MidiEvent


type ColoredChar
    = ColoredChar (Maybe Color) Char


type Line
    = Line (List ColoredChar)


type Screen
    = Screen (List Line)


type Config
    = Config Int



-- Display


numberToDigits : Int -> List Char
numberToDigits x =
    let
        aux y =
            case y of
                0 ->
                    []

                n ->
                    modBy 10 n :: aux (n // 10)
    in
    aux x |> List.reverse |> List.map (\d -> d + 48 |> Char.fromCode)


white =
    ColoredChar Nothing ' '


whiteLine : Config -> Line
whiteLine (Config w) =
    List.repeat w white |> Line


displayEvts : Config -> List Evt -> List Line
displayEvts cfg evts =
    let
        isNow now (Evt (Time t) _) =
            now >= t

        getMidiEvt (Evt _ e) =
            e

        fold t ( result, tail ) =
            case tail of
                [] ->
                    ( result, [] )

                x :: xs ->
                    if isNow t x then
                        let
                            line =
                                getMidiEvt x |> noteToLine cfg
                        in
                        ( line :: result, xs )

                    else
                        ( whiteLine cfg :: result, tail )

        clock =
            List.range 0 5000 |> List.map (\x -> x * 10)
    in
    List.foldr fold ( [], evts ) clock |> Tuple.first


noteToLine : Config -> MidiEvent -> Line
noteToLine (Config w) note =
    case note of
        NoteOn (Pitch p) velo ->
            let
                noteChars =
                    numberToDigits p |> withColor (veloToColor velo)

                trailing =
                    w - List.length noteChars - p
            in
            List.repeat p white ++ noteChars ++ List.repeat trailing white |> Line

        NoteOff (Pitch p) velo ->
            let
                noteChars =
                    numberToDigits p |> withColor (veloToColor velo)

                trailing =
                    w - List.length noteChars - p
            in
            List.repeat p white ++ noteChars ++ List.repeat trailing white |> Line

        Ignore ->
            whiteLine (Config w)


displayLines : List Line -> Html Msg
displayLines lines =
    lines
        |> List.map lineToHtml
        |> LE.intercalate [ br [] [] ]
        |> div []


veloToColor : Velo -> Color
veloToColor (Velo v) =
    let
        flt =
            toFloat v / 128.0
    in
    Color.rgba flt flt flt 1.0


withColor : Color -> List Char -> List ColoredChar
withColor c =
    List.map (\char -> ColoredChar (Just c) char)


lineToHtml : Line -> List (Html Msg)
lineToHtml (Line lst) =
    lst |> List.map asHtml


asHtml : ColoredChar -> Html Msg
asHtml (ColoredChar color char) =
    case color of
        Nothing ->
            text (String.fromChar char)

        Just clr ->
            span [ style "color" (Color.toCssString clr) ] [ text (String.fromChar char) ]


toString : Evt -> String
toString (Evt (Time time) evt) =
    String.join "-" [ String.fromInt time, midiEventToString evt ]


midiEventToString : MidiEvent -> String
midiEventToString m =
    case m of
        NoteOn (Pitch p) (Velo v) ->
            String.join " " [ "Note on -", String.fromInt p, String.fromInt v ]

        NoteOff (Pitch p) (Velo v) ->
            String.join " " [ "Note off -", String.fromInt p, String.fromInt v ]

        Ignore ->
            "-?-"


fromData : Int -> String -> Int -> Int -> Evt
fromData time status pitch velo =
    let
        m =
            case status of
                "Note_on_c" ->
                    NoteOn (Pitch pitch) (Velo velo)

                "Note_off_c" ->
                    NoteOff (Pitch pitch) (Velo velo)

                _ ->
                    Ignore
    in
    Evt (Time time) m


decoder : Decoder Evt
decoder =
    let
        pipe =
            Decode.pipeline

        field =
            Decode.field

        string =
            Decode.string

        int =
            Decode.int
    in
    Decode.into
        fromData
        |> pipe (field "time" int)
        |> pipe (field "status" string)
        |> pipe (field "pitch" int)
        |> pipe (field "velo" int)


decodeCsv : String -> Result Decode.Error (List Evt)
decodeCsv csv =
    Decode.decodeCsv Decode.FieldNamesFromFirstRow decoder csv


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions _ =
    Sub.none


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , Http.get
        { url = "melody-berlin-train.txt"
        , expect = Http.expectString GotMidiCSV
        }
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotMidiCSV res ->
            case res of
                Ok str ->
                    let
                        _ =
                            Debug.log "str" str

                        m =
                            case decodeCsv str of
                                Ok lst ->
                                    Parsed (lst |> displayEvts (Config 255) |> displayLines)

                                Err err ->
                                    CSVError <| Decode.errorToString err
                    in
                    ( m, Cmd.none )

                Err e ->
                    ( Failed e, Cmd.none )


view : Model -> Html Msg
view model =
    case model of
        Loaded str ->
            div [ style "font-family" "monospace" ]
                [ pre [] [ text str ]
                ]

        Loading ->
            text "loading.."

        Failed e ->
            let
                _ =
                    Debug.log (Debug.toString e)
            in
            text "error !"

        CSVError s ->
            text s

        Parsed html ->
            div [ style "font-family" "monospace" ]
                [ html
                ]
