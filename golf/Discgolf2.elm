module Discgolf2 exposing (..)

import Html
import Html.Events as Events
import Html.Attributes as Attrs
import Svg exposing (..)
import Svg.Attributes as SvgAttrs


type alias Records =
    { par : List Int, shots : List Int }


type AppState
    = Enterpar Records
    | ShotsAndScore Records Int
    | Giveoption Records
    | Summary Records


initialState =
    Enterpar { par = [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], shots = [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ] }


viewEnterpar : Records -> Html.Html AppEvent
viewEnterpar records =
    Html.div [ Attrs.style [ ( "background-color", "#38FF18" ), ( "font-size", "20pt" ), ( "font-family", "Impact" ), ( "padding", "20px" ) ] ]
        ([ Html.p [] [ Html.text "Please enter the par for each hole" ] ]
            ++ (List.indexedMap
                    (\index par1 ->
                        Html.p []
                            [ Html.text ("Hole" ++ " " ++ toString (index + 1))
                            , Html.text (" ")
                            , Html.input [ Events.onInput (\s -> Storepar index (Result.withDefault 0 (String.toInt (s)))), Attrs.value (toString (par1)) ] []
                            ]
                    )
                    records.par
               )
            ++ [ Html.button
                    [ Events.onClick (Submit1)
                    ]
                    [ Html.text "Submit 1" ]
               ]
        )


viewShotsAndScore : Records -> Int -> Html.Html AppEvent
viewShotsAndScore records x =
    Html.div [ Attrs.style [ ( "background-color", "#38FF18" ), ( "font-size", "20pt" ), ( "font-family", "Impact" ), ( "padding", "20px" ) ] ]
        ([ Html.p [] [ Html.text "Please enter the shots for this hole, then click Ok. The score will be simultaneously shown and stored." ] ]
            ++ [ Html.p [] [ Html.text ("Hole" ++ " " ++ toString (x) ++ "," ++ "         " ++ "Par" ++ " " ++ toString (getone records.par x)) ]
               ]
            ++ [ Html.p []
                    [ Html.text ("Shots")
                    , Html.text (" ")
                    , Html.input [ Events.onInput (\s -> Storeshots x (Result.withDefault 0 (String.toInt (s)))), Attrs.value (toString (getone records.shots x)) ] []
                    , Html.text ("Scores")
                    , Html.text (" ")
                    , Html.text (toString (getone records.shots x - getone records.par x))
                    ]
               ]
            ++ [ Html.button
                    [ Events.onClick (Ok) ]
                    [ Html.text "Ok" ]
               ]
        )


viewGiveoption : Html.Html AppEvent
viewGiveoption =
    Html.div [ Attrs.style [ ( "background-color", "#38FF18" ), ( "font-size", "20pt" ), ( "font-family", "Impact" ), ( "padding", "20px" ) ] ]
        ([ Html.p [] [ Html.text "If you want to play another round, click continue. If you want to see how you do, click summarize" ] ]
            ++ [ Html.button
                    [ Events.onClick (Continue)
                    ]
                    [ Html.text "Continue" ]
               ]
            ++ [ Html.button
                    [ Events.onClick (Summarize)
                    ]
                    [ Html.text "Summarize" ]
               ]
        )


getone : List Int -> Int -> Int
getone list i =
    case list of
        [] ->
            0

        h :: t ->
            if i == 1 then
                h
            else
                getone t (i - 1)


type AppEvent
    = Submit1
    | Ok
    | Continue
    | Summarize
    | Storepar Int Int
    | Storeshots Int Int


view : AppState -> Html.Html AppEvent
view state =
    case state of
        Enterpar records ->
            viewEnterpar records

        ShotsAndScore records x ->
            viewShotsAndScore records x

        Giveoption records ->
            viewGiveoption

        Summary records ->
            viewSummary records


update : AppEvent -> AppState -> AppState
update event state =
    case event of
        Ok ->
            case state of
                ShotsAndScore r i ->
                    if i == 18 then
                        Giveoption r
                    else
                        ShotsAndScore r (i + 1)

                _ ->
                    state

        Submit1 ->
            case state of
                Enterpar r ->
                    ShotsAndScore r 1

                _ ->
                    state

        Continue ->
            case state of
                Giveoption r ->
                    ShotsAndScore r 1

                _ ->
                    state

        Storepar index hisinput ->
            case state of
                Enterpar r ->
                    Enterpar { par = replace index hisinput r.par, shots = r.shots }

                _ ->
                    state

        Storeshots index hisinput ->
            case state of
                ShotsAndScore r i ->
                    ShotsAndScore { par = r.par, shots = replace (index - 1) hisinput r.shots } i

                _ ->
                    state

        Summarize ->
            case state of
                Giveoption r ->
                    Summary r

                _ ->
                    state


replace : Int -> a -> List a -> List a
replace int item list =
    case list of
        [] ->
            []

        h :: t ->
            if int == 0 then
                item :: t
            else
                h :: replace (int - 1) item t


flexRowStyle : Html.Attribute msg
flexRowStyle =
    Attrs.style
        [ ( "display", "flex" )
        , ( "flex-direction", "row" )
        ]


flexColumnStyle : Html.Attribute msg
flexColumnStyle =
    Attrs.style
        [ ( "display", "flex" )
        , ( "flex-direction", "column" )
        ]


tableCellStyle : Html.Attribute msg
tableCellStyle =
    Attrs.style
        [ ( "width", "100px" )
        , ( "height", "5em" ) --em is spacing based on font size
        , ( "text-align", "center" )
        , ( "border-left-style", "solid" )
        ]


row : Records -> Html.Html msg
row records =
    Html.div [ flexColumnStyle ]
        [ Html.div [ flexRowStyle ]
            [ Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text ("Round 1 Shots")
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 1))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 2))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 3))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 4))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 5))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 6))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 7))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 8))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 9))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 10))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 11))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 12))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 13))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 14))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 15))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 16))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 17))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 18))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 1 + getone records.shots 2 + getone records.shots 3 + getone records.shots 4 + getone records.shots 5 + getone records.shots 6 + getone records.shots 7 + getone records.shots 8 + getone records.shots 9 + getone records.shots 10 + getone records.shots 11 + getone records.shots 12 + getone records.shots 13 + getone records.shots 14 + getone records.shots 15 + getone records.shots 16 + getone records.shots 17 + getone records.shots 18))
                ]

            --, total for 18 shots
            ]
        , Html.div
            [ flexRowStyle ]
            [ Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text ("Round 1 Scores")
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 1 - getone records.par 1))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 2 - getone records.par 2))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 3 - getone records.par 3))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 4 - getone records.par 4))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 5 - getone records.par 5))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 6 - getone records.par 6))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 7 - getone records.par 7))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 8 - getone records.par 8))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 9 - getone records.par 9))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 10 - getone records.par 10))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 11 - getone records.par 11))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 12 - getone records.par 12))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 13 - getone records.par 13))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 14 - getone records.par 14))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 15 - getone records.par 15))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 16 - getone records.par 16))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 17 - getone records.par 17))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 18 - getone records.par 18))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString ((getone records.shots 1 - getone records.par 1) + (getone records.shots 2 - getone records.par 2) + (getone records.shots 3 - getone records.par 3) + (getone records.shots 4 - getone records.par 4) + (getone records.shots 5 - getone records.par 5) + (getone records.shots 6 - getone records.par 6) + (getone records.shots 7 - getone records.par 7) + (getone records.shots 8 - getone records.par 8) + (getone records.shots 9 - getone records.par 9) + (getone records.shots 10 - getone records.par 10) + (getone records.shots 11 - getone records.par 11) + (getone records.shots 12 - getone records.par 12) + (getone records.shots 13 - getone records.par 13) + (getone records.shots 14 - getone records.par 14) + (getone records.shots 15 - getone records.par 15) + (getone records.shots 16 - getone records.par 16) + (getone records.shots 17 - getone records.par 17) + (getone records.shots 18 - getone records.par 18)))
                ]

            --, Total for 18 scores
            ]
        , Html.div
            [ flexRowStyle ]
            [ Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text ("Average Shots")
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 1))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 2))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 3))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 4))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 5))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 6))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 7))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 8))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 9))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 10))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 11))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 12))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 13))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 14))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 15))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 16))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 17))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 18))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString ((getone records.shots 1 + getone records.shots 2 + getone records.shots 3 + getone records.shots 4 + getone records.shots 5 + getone records.shots 6 + getone records.shots 7 + getone records.shots 8 + getone records.shots 9 + getone records.shots 10 + getone records.shots 11 + getone records.shots 12 + getone records.shots 13 + getone records.shots 14 + getone records.shots 15 + getone records.shots 16 + getone records.shots 17 + getone records.shots 18) // 18))
                ]

            --, Same thing, but for all 18 rounds
            ]
        , Html.div
            [ flexRowStyle ]
            [ Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text ("Average Scores")
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 1 - getone records.par 1))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 2 - getone records.par 2))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 3 - getone records.par 3))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 4 - getone records.par 4))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 5 - getone records.par 5))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 6 - getone records.par 6))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 7 - getone records.par 7))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 8 - getone records.par 8))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 9 - getone records.par 9))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 10 - getone records.par 10))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 11 - getone records.par 11))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 12 - getone records.par 12))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 13 - getone records.par 13))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 14 - getone records.par 14))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 15 - getone records.par 15))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 16 - getone records.par 16))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 17 - getone records.par 17))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString (getone records.shots 18 - getone records.par 18))
                ]
            , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ]
                [ Html.text (toString ((getone records.shots 1 - getone records.par 1) + (getone records.shots 2 - getone records.par 2) + (getone records.shots 3 - getone records.par 3) + (getone records.shots 4 - getone records.par 4) + (getone records.shots 5 - getone records.par 5) + (getone records.shots 6 - getone records.par 6) + (getone records.shots 7 - getone records.par 7) + (getone records.shots 8 - getone records.par 8) + (getone records.shots 9 - getone records.par 9) + (getone records.shots 10 - getone records.par 10) + (getone records.shots 11 - getone records.par 11) + (getone records.shots 12 - getone records.par 12) + (getone records.shots 13 - getone records.par 13) + (getone records.shots 14 - getone records.par 14) + (getone records.shots 15 - getone records.par 15) + (getone records.shots 16 - getone records.par 16) + (getone records.shots 17 - getone records.par 17) + (getone records.shots 18 - getone records.par 18) // 18))
                ]

            --, Same thing but for all 18 (round)
            ]
        ]



-- When I go back from Continue to play the second round, we should store another set of 18 holes into another record.


header : Html.Html msg
header =
    Html.div [ flexRowStyle ]
        [ Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ] [ Html.text " " ]
        , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ] [ Html.text "H 1" ]
        , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ] [ Html.text "H 2" ]
        , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ] [ Html.text "H 3" ]
        , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ] [ Html.text "H 4" ]
        , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ] [ Html.text "H 5" ]
        , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ] [ Html.text "H 6" ]
        , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ] [ Html.text "H 7" ]
        , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ] [ Html.text "H 8" ]
        , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ] [ Html.text "H 9" ]
        , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ] [ Html.text "H 10" ]
        , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ] [ Html.text "H 11" ]
        , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ] [ Html.text "H 12" ]
        , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ] [ Html.text "H 13" ]
        , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ] [ Html.text "H 14" ]
        , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ] [ Html.text "H 15" ]
        , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ] [ Html.text "H 16" ]
        , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ] [ Html.text "H 17" ]
        , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ] [ Html.text "H 18" ]
        , Html.div [ tableCellStyle, Attrs.style [ ( "border-bottom-style", "solid" ) ] ] [ Html.text "Round" ]
        ]


viewSummary : Records -> Html.Html AppEvent
viewSummary records =
    Html.div [ flexColumnStyle, Attrs.style [ ( "background-color", "#38FF18" ), ( "font-size", "20pt" ), ( "font-family", "Impact" ), ( "padding", "12px" ) ] ]
        --(:: (List.map row records)
        [ header, (row records) ]


main =
    Html.beginnerProgram { model = initialState, view = view, update = update }
