module Main exposing
    ( Config(..)
    , DisplayChar(..)
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
import Piet
import Random exposing (Generator)
import Random.Char
import Random.Extra as RX
import Random.List as RL


type Model
    = Loading
    | Loaded String
    | Generated ( Html Msg, Maybe (Html Msg) )
    | Failed Http.Error
    | WrongOrder
    | CSVError String

      

type Msg
    = GotMidiCSV (Result Http.Error String)
    | GotRandomized (Html Msg)
    | GotRandomizedCode (Html Msg)
    | GotCodeTxt (Result Http.Error String)


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


getMidiEvt (Evt _ e) =
    e


type DisplayChar
    = ColoredChar Color Char
    | FillerChar
    | RndColorChar Char


type Line
    = Line (List DisplayChar)


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


setTransperancy : Float -> Color -> Color
setTransperancy t c =
    let
        color =
            Color.toRgba c
    in
    Color.rgba color.red color.green color.blue t


randomColoredAndChar : Generator ( Color, Char )
randomColoredAndChar =
    let
        randChar =
            generatorOfString "(12345678()[]{}+-*~          )"

        rflt =
            Random.float 0.95 1.0

        randColor =
            Random.int 0 5 |> Random.map (Piet.wrap >> setTransperancy 0.2)
    in
    Random.map2 Tuple.pair randColor randChar


randomColor : Generator Color
randomColor =
    Random.float 0.1 0.5 |> Random.map (\c -> Color.rgb (c * 0.2) 0.0 (c * 2.0))


generatorOfString : String -> Generator Char
generatorOfString str =
    str |> String.toList |> RL.choose |> Random.map (Tuple.first >> Maybe.withDefault ' ')


white =
    FillerChar


whiteLine : Config -> Line
whiteLine (Config w) =
    List.repeat w white |> Line


displayEvtsWithoutTime : Config -> List Evt -> List Line
displayEvtsWithoutTime cfg evts =
    evts |> List.indexedMap (\i v -> v |> getMidiEvt |> noteToLine cfg i)


displayEvts : Config -> List Evt -> List Line
displayEvts cfg evts =
    let
        isNow now (Evt (Time t) _) =
            now >= t

        fold t ( result, tail ) =
            case tail of
                [] ->
                    ( result, [] )

                x :: xs ->
                    if isNow t x then
                        let
                            line =
                                getMidiEvt x |> noteToLine cfg 0
                        in
                        ( line :: result, xs )

                    else
                        ( whiteLine cfg :: result, tail )

        clock =
            List.range 0 5000 |> List.map (\x -> x * 10)
    in
    List.foldr fold ( [], evts ) clock |> Tuple.first


offset =
    35


noteToLine : Config -> Int -> MidiEvent -> Line
noteToLine (Config w) index note =
    case note of
        NoteOn (Pitch p) velo ->
            let
                noteChars =
                    numberToDigits p |> withColor (Piet.wrap (index * 1))

                trailing =
                    w - List.length noteChars - (p - offset)
            in
            List.repeat (p - offset) white ++ noteChars ++ List.repeat trailing white |> Line

        NoteOff (Pitch p) velo ->
            let
                noteChars =
                    numberToDigits p |> withColor (Piet.wrap (index * 1))

                trailing =
                    w - List.length noteChars - p
            in
            List.repeat p white ++ noteChars ++ List.repeat trailing white |> Line

        Ignore ->
            whiteLine (Config w)


displayLines : List Line -> Generator (Html Msg)
displayLines lines =
    lines
        |> List.map lineToHtml
        |> RX.sequence
        |> Random.map
            (LE.transpose
                >> LE.intercalate [ br [] [] ]
                >> div []
            )


displayCodeLines : List Line -> Generator (Html Msg)
displayCodeLines lines =
    lines
        |> List.map lineToHtml
        |> RX.sequence
        |> Random.map (LE.intercalate [ br [] [] ] >> div [])


veloToColor : Velo -> Color
veloToColor (Velo v) =
    let
        flt =
            toFloat v / 128.0
    in
    Piet.pick flt


withColor : Color -> List Char -> List DisplayChar
withColor c =
    List.map (\char -> ColoredChar c char)


lineToHtml : Line -> Generator (List (Html Msg))
lineToHtml (Line lst) =
    lst |> List.map asHtml |> RX.sequence


asHtml : DisplayChar -> Generator (Html Msg)
asHtml dchar =
    let
        render color char bg =
            span
                [ style "color" (Color.toCssString color)
                , style "background-color" (Color.toCssString bg)
                ]
                [ text (String.fromChar char) ]
    in
    case dchar of
        FillerChar ->
            Random.constant <|
                span
                    []
                    [ text " " ]

        ColoredChar color char ->
            Random.constant <| render color char color

        RndColorChar char ->
            randomColor |> Random.map (\rndColor -> render Color.white char (rndColor |> setTransperancy 0.9))


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
    let
        cmd =
            Http.get
                { url = "17-02-2021.txt"
                , expect = Http.expectString GotMidiCSV
                }
    in
    ( Loading
    , cmd
    )


codeHtmlGenerator : Config -> String -> Generator (Html Msg)
codeHtmlGenerator (Config w) str =
    let
        unf seed =
            case seed of
                [] ->
                    Nothing

                lst ->
                    Just ( List.take w lst, List.drop w lst )

        chars =
            String.toList str
                |> List.filter (\c -> List.member c (String.toList " -.,+*~()"))
                |> List.map
                    (\c ->
                        case c of
                            '\n' ->
                                '\t'

                            other ->
                                other
                    )
                |> List.map RndColorChar

        lines =
            LE.unfoldr unf chars |> List.map Line
    in
    displayCodeLines lines


config =
    Config 70


setCodeHtml : Html Msg -> ( Html Msg, Maybe (Html Msg) ) -> ( Html Msg, Maybe (Html Msg) )
setCodeHtml codeHtml ( midi, _ ) =
    ( midi, Just codeHtml )


setMidiHtml : Html Msg -> ( Html Msg, Maybe (Html Msg) ) -> ( Html Msg, Maybe (Html Msg) )
setMidiHtml midiHtml ( _, mCode ) =
    ( midiHtml, mCode )


getCodeCmd : Cmd Msg
getCodeCmd =
    Http.get
        { url = "code.txt"
        , expect = Http.expectString GotCodeTxt
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotMidiCSV res ->
            case res of
                Ok str ->
                    case decodeCsv str of
                        Ok lst ->
                            let
                                _ = Debug.log "gen" lst
                                    
                                gen =
                                    lst |> List.take 300 |> displayEvtsWithoutTime config |> displayLines
                            in
                            ( model, Random.generate GotRandomized gen )

                        Err err ->
                            ( CSVError <| Decode.errorToString err, Cmd.none )

                Err e ->
                    ( Failed e, Cmd.none )

        GotRandomized html ->
            ( Generated ( html, Nothing ), getCodeCmd )

        GotRandomizedCode html ->
            case model of
                Generated mdl ->
                    ( Generated (setCodeHtml html mdl), Cmd.none )

                _ ->
                    ( WrongOrder, Cmd.none )

        GotCodeTxt res ->
            case res of
                Ok txt ->
                    let
                        genCode =
                            txt |> codeHtmlGenerator (Config 300)
                    in
                    ( model, Random.generate GotRandomizedCode genCode )

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

        Generated ( html, mCodeHtml ) ->
            let
                layer fz htmlContent =
                    pre
                        [ style "position" "absolute"
                        , style "top" "0"
                        , style "left" "0"
                        , style "font-family" "monospace"
                        , style "font-size" fz
                        ]
                        [ htmlContent
                        ]
            in
            case mCodeHtml of
                Just codeHtml ->
                    div [style "background-color" "black"] [ layer "1em" html, layer "1em" codeHtml ]

                Nothing ->
                    div [] [ layer "1em" html ]

        WrongOrder ->
            text "wrong order"

{-
🚲
bicycle
Unicode: U+1F6B2, UTF-8: F0 9F 9A B2

-}
