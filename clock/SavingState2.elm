port module SavingState2 exposing (..)

import Html
import Time
import Date
import Time.DateTime as DateTime exposing (DateTime, dateTime)
import Html.Attributes as Attrs
import Html.Events as Events


type alias AppState =
    { secondsNow : Time.Time
    , now : Date.Date
    , year : Int
    , month : Int
    , day : Int
    , hour : Int
    , minute : Int
    , second : Int
    }


type alias StoreState =
    { year : Int
    , month : Int
    , day : Int
    , hour : Int
    , minute : Int
    , second : Int
    }


appToStore : AppState -> StoreState
appToStore state =
    { year = state.year, month = state.month, day = state.day, hour = state.hour, minute = state.minute, second = state.second }


type AppEvent
    = Tick Time.Time
    | ChangeYear Int
    | ChangeMonth Int
    | ChangeDay Int
    | ChangeHour Int
    | ChangeMinute Int
    | ChangeSecond Int


charToDiv : Char -> Html.Html AppEvent
charToDiv c =
    case c of
        '1' ->
            Html.div [ binarydivsblack ] []

        _ ->
            Html.div [ binarydivswhite ] []


view : AppState -> Html.Html AppEvent
view state =
    let
        s =
            round (DateTime.toTimestamp (DateTime.dateTime { year = state.year, month = state.month, day = state.day, hour = state.hour, minute = state.minute, second = state.second, millisecond = 0 })) // 1000
    in
        Html.div [ container ]
            [ Html.h1 [] [ Html.text "Binary Clock" ]
            , Html.p [] [ Html.text (toString (round (state.secondsNow) - s - 14400)) ]
            , Html.p [] [ Html.text (numToBinary (round (state.secondsNow) - s - 14400)) ]
            , Html.text
                ("Current time:"
                    ++ " "
                    ++ toString (Date.year state.now)
                    ++ " "
                    ++ toString (Date.month state.now)
                    ++ "."
                    ++ toString (Date.day state.now)
                    ++ " "
                    ++ toString (Date.dayOfWeek state.now)
                    ++ " "
                    ++ normalhour (Date.hour state.now)
                    ++ ":"
                    ++ specialcases (Date.minute state.now)
                    ++ ":"
                    ++ specialcases (Date.second state.now)
                )
            , Html.p []
                [ Html.p [] [ Html.text "Birth time (please enter integers): " ]
                , Html.p []
                    [ Html.text "Year: "
                    , Html.input [ Events.onInput (\s -> ChangeYear (Result.withDefault 0 (String.toInt s))), Attrs.value (toString (state.year)) ] []
                    ]
                , Html.p []
                    [ Html.text "Month: "
                    , Html.input [ Events.onInput (\s -> ChangeMonth (Result.withDefault 0 (String.toInt s))), Attrs.value (toString (state.month)) ] []
                    ]
                , Html.p []
                    [ Html.text "Day: "
                    , Html.input [ Events.onInput (\s -> ChangeDay (Result.withDefault 0 (String.toInt s))), Attrs.value (toString (state.day)) ] []
                    ]
                , Html.p []
                    [ Html.text "Hour: "
                    , Html.input [ Events.onInput (\s -> ChangeHour (Result.withDefault 0 (String.toInt s))), Attrs.value (toString (state.hour)) ] []
                    ]
                , Html.p []
                    [ Html.text "Minute: "
                    , Html.input [ Events.onInput (\s -> ChangeMinute (Result.withDefault 0 (String.toInt s))), Attrs.value (toString (state.minute)) ] []
                    ]
                , Html.p []
                    [ Html.text "Second: "
                    , Html.input [ Events.onInput (\s -> ChangeSecond (Result.withDefault 0 (String.toInt s))), Attrs.value (toString (state.second)) ] []
                    ]
                ]
            , Html.div [ contents ]
                [ Html.h2 [ Attrs.style [ ( "color", "brown" ) ] ] [ Html.text "Binary Representation of the Number of Seconds You've Been Born" ]
                , Html.div [ divsformat ]
                    (List.map charToDiv (String.toList (numToBinary (round (state.secondsNow) - s - 14400))))
                ]
            ]


port storeState : StoreState -> Cmd msg


update : AppEvent -> AppState -> AppState
update event state =
    case event of
        Tick currentTime ->
            { state | now = Date.fromTime currentTime, secondsNow = toFloat (round (currentTime) // 1000) }

        ChangeYear newYear ->
            { state | year = newYear }

        ChangeMonth newMonth ->
            { state | month = newMonth }

        ChangeDay newDay ->
            { state | day = newDay }

        ChangeHour newHour ->
            { state | hour = newHour }

        ChangeMinute newMinute ->
            { state | minute = newMinute }

        ChangeSecond newSecond ->
            { state | second = newSecond }


updateAndSave : AppEvent -> AppState -> ( AppState, Cmd AppEvent )
updateAndSave event state =
    let
        newState =
            update event state
    in
        ( newState, storeState (appToStore newState) )


initFunction : Maybe StoreState -> AppState
initFunction maybeString =
    case maybeString of
        Just state ->
            { year = state.year, month = state.month, day = state.day, hour = state.hour, minute = state.minute, second = state.second, secondsNow = 0, now = Date.fromTime 0 }

        Nothing ->
            { year = 0, month = 0, day = 0, hour = 0, minute = 0, second = 0, secondsNow = 0, now = Date.fromTime 0 }


subscriptions : AppState -> Sub AppEvent
subscriptions state =
    Time.every Time.second (\t -> Tick t)


specialcases : Int -> String
specialcases t =
    if t < 10 then
        "0" ++ toString t
    else
        toString t


normalhour : Int -> String
normalhour t =
    if t > 12 then
        toString (t - 12)
    else
        toString t


numToBinary : Int -> String
numToBinary x =
    case x of
        0 ->
            ""

        a ->
            if a % 2 == 1 then
                numToBinary (a // 2) ++ "1"
            else
                numToBinary (a // 2) ++ "0"


main =
    Html.programWithFlags
        { init =
            (\s ->
                let
                    newState =
                        initFunction s
                in
                    ( newState, storeState (appToStore newState) )
            )
        , update = updateAndSave
        , subscriptions = subscriptions
        , view = view
        }


container : Html.Attribute msg
container =
    Attrs.style
        [ ( "padding", "20px" )
        , ( "margin", "20px" )
        , ( "border-style", "solid" )
        , ( "border-color", "black" )
        , ( "border-width", "2px" )
        , ( "background-color", "#faebd7" )
        , ( "display", "flex" )
        , ( "flex-direction", "column" )
        , ( "justify-content", "end" )
        , ( "height", "700px" )
        , ( "width", "1300px" )
        ]


contents : Html.Attribute msg
contents =
    Attrs.style
        [ ( "padding", "20px" )
        , ( "margin", "20px" )
        , ( "border-style", "solid" )
        , ( "border-color", "black" )
        , ( "border-width", "2px" )
        , ( "background-color", "#ffa07a" )
        , ( "display", "flex" )
        , ( "flex-direction", "column" )
        , ( "justify-content", "end" )
        , ( "height", "200px" )
        , ( "width", "1200px" )
        ]


divsformat : Html.Attribute msg
divsformat =
    Attrs.style
        [ ( "padding", "20px" )
        , ( "background-color", "#ffa07a" )
        , ( "display", "flex" )
        , ( "flex-direction", "row" )
        , ( "justify-content", "end" )
        , ( "height", "100px" )
        , ( "width", "1100px" )
        ]


binarydivswhite : Html.Attribute msg
binarydivswhite =
    Attrs.style
        [ ( "padding", "5px" )
        , ( "margin", "5px" )
        , ( "border-style", "solid" )
        , ( "border-color", "black" )
        , ( "border-width", "1px" )
        , ( "background-color", "#ffffff" )
        , ( "display", "flex" )
        , ( "flex-direction", "row" )
        , ( "justify-content", "end" )
        , ( "height", "15px" )
        , ( "width", "15px" )
        ]


binarydivsblack : Html.Attribute msg
binarydivsblack =
    Attrs.style
        [ ( "padding", "5px" )
        , ( "margin", "5px" )
        , ( "border-style", "solid" )
        , ( "border-color", "black" )
        , ( "border-width", "1px" )
        , ( "background-color", "#000000" )
        , ( "display", "flex" )
        , ( "flex-direction", "row" )
        , ( "justify-content", "end" )
        , ( "height", "15px" )
        , ( "width", "15px" )
        ]
