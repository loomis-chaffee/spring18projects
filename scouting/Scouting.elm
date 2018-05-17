{- Made by: Kohl Reddy, Dzhangir Bayandarov
   CL Computer Science
   Project name: "Robotics scouting app"
   Teachers: Alex Ozdemir, Hudson Harper

   Brief description: The project is design for the Loomis Chaffee Robotics Team to keep track of the
   rankings of the opposing teams based on a criteria inputed by the members of the team.
-}


port module Scouting exposing (..)

import Debug
import Html
import Html.Events as Events
import Html.Attributes as Attrs


type alias AppState =
    { autonomous : List ( String, Int )
    , teleop : List ( String, Int )
    , endgame : List ( String, Int )
    , currentPage : Page
    , evaluation : Evaluation
    , event_name : String
    , rankings : List ( List Evaluation, String )
    }


type alias Evaluation =
    { interview : Bool
    , autonomousScore : List Int
    , teleopScore : List Int
    , endgameScore : List Int
    , comments : String
    }


type Page
    = CriteriaPage
    | RankingsPage
    | NewSchoolsPage String
    | PerformanceEvaluationPage String
    | InterviewEvaluationPage String
    | ReportPage String


type alias StorableState =
    { autonomous : List ( String, Int )
    , teleop : List ( String, Int )
    , endgame : List ( String, Int )
    , evaluation : Evaluation
    , event_name : String
    , criteriaPage : Bool
    , newSchoolsPage : Maybe String
    , evaluationPage : Maybe String
    , rankingsPage : Bool
    , rankings : List ( List Evaluation, String )
    , reportPage : Maybe String
    }


initFunction : Maybe StorableState -> AppState
initFunction maybeString =
    case maybeString of
        Just state ->
            decode state

        Nothing ->
            { autonomous = [ ( "add your criteria here", 0 ) ]
            , teleop = [ ( "add your criteria here", 0 ) ]
            , endgame = [ ( "add your criteria here", 0 ) ]
            , currentPage = CriteriaPage
            , rankings = []
            , evaluation = { autonomousScore = [], teleopScore = [], endgameScore = [], interview = False, comments = "" }
            , event_name = "2018"
            }


type AppEvent
    = NewAutonomous
    | AddSchool String
    | AddEval Evaluation String
    | EditComments String
    | NewTeleOp
    | NewEndGame
    | EditSchoolName String
    | EditAutonomous Int String
    | EditAutonomousMaxScore Int Int
    | EditTeleOp Int String
    | EditTeleOpMaxScore Int Int
    | EditEndGame Int String
    | EditEndGameMaxScore Int Int
    | RemoveAutonomous Int
    | RemoveTeleOp Int
    | RemoveEndGame Int
    | NewEventName String
    | ChangetoRankings
    | ChangetoInterviewEval String
    | ChangetoPerformanceEval String
    | ChangetoNewSchoolsPage
    | EditAutonomousScore Int Int
    | EditTeleOpScore Int Int
    | EditEndGameScore Int Int
    | DeleteSchool Int
    | SeeFullReport String


zip : List a -> List b -> List ( a, b )
zip alist blist =
    case ( alist, blist ) of
        ( a :: as_, b :: bs ) ->
            ( a, b ) :: zip as_ bs

        _ ->
            []


addToBack : a -> List a -> List a
addToBack a list =
    case list of
        [] ->
            [ a ]

        h :: t ->
            h :: addToBack a t


delete : Int -> List a -> List a
delete index list =
    case list of
        [] ->
            []

        h :: t ->
            if index == 0 then
                t
            else
                h :: delete (index - 1) t


replaceFirst : Int -> a -> List ( a, b ) -> List ( a, b )
replaceFirst index item list =
    case list of
        [] ->
            []

        h :: t ->
            if index == 0 then
                let
                    ( f, s ) =
                        h
                in
                    ( item, s ) :: t
            else
                h :: replaceFirst (index - 1) item t


stringToMaybeInt : String -> Int
stringToMaybeInt s =
    Maybe.withDefault 0 (Result.toMaybe (String.toInt s))


replace : Int -> a -> List a -> List a
replace index item list =
    case list of
        [] ->
            []

        h :: t ->
            if index == 0 then
                item :: t
            else
                h :: replace (index - 1) item t


replaceSecond : Int -> b -> List ( a, b ) -> List ( a, b )
replaceSecond index item list =
    case list of
        [] ->
            []

        h :: t ->
            if index == 0 then
                let
                    ( f, s ) =
                        h
                in
                    ( f, item ) :: t
            else
                h :: replaceSecond (index - 1) item t


totalEvalScore : Evaluation -> Int
totalEvalScore evaluation =
    (List.sum evaluation.autonomousScore) + (List.sum evaluation.teleopScore) + (List.sum evaluation.endgameScore)


finder : b -> List ( List a, b ) -> List a
finder item list =
    case list of
        [] ->
            []

        h :: t ->
            if
                (let
                    ( a, b ) =
                        h
                 in
                    b
                )
                    == item
            then
                let
                    ( a, item ) =
                        h
                in
                    a
            else
                finder item t


findAndAdd : b -> a -> List ( List a, b ) -> List ( List a, b )
findAndAdd schoolName eval list =
    case list of
        [] ->
            []

        h :: t ->
            if
                (let
                    ( a, b ) =
                        h
                 in
                    b
                )
                    == schoolName
            then
                ( addToBack eval
                    (let
                        ( a, b ) =
                            h
                     in
                        a
                    )
                , schoolName
                )
                    :: t
            else
                h :: findAndAdd schoolName eval t


avgScore : List Evaluation -> Int
avgScore evals =
    case evals of
        [] ->
            0

        h :: t ->
            (List.sum (List.map (totalEvalScore) (h :: t)) // (List.length (h :: t)))


rank : ( List Evaluation, String ) -> Int
rank school =
    let
        ( interviews, performances ) =
            List.partition (isInterview)
                (let
                    ( evals, _ ) =
                        school
                 in
                    evals
                )
    in
        (avgScore performances) - (avgScore interviews)


isInterview : Evaluation -> Bool
isInterview eval =
    if eval.interview == True then
        True
    else
        False


update : AppEvent -> AppState -> AppState
update event state =
    Debug.log ("state")
        (case event of
            NewAutonomous ->
                { autonomous = addToBack ( "", 0 ) state.autonomous
                , teleop = state.teleop
                , endgame = state.endgame
                , currentPage = state.currentPage
                , event_name = state.event_name
                , evaluation = state.evaluation
                , rankings = state.rankings
                }

            NewTeleOp ->
                { autonomous = state.autonomous
                , teleop = addToBack ( "", 0 ) state.teleop
                , endgame = state.endgame
                , currentPage = state.currentPage
                , event_name = state.event_name
                , evaluation = state.evaluation
                , rankings = state.rankings
                }

            NewEndGame ->
                { autonomous = state.autonomous
                , teleop = state.teleop
                , endgame = addToBack ( "", 0 ) state.endgame
                , currentPage = state.currentPage
                , event_name = state.event_name
                , evaluation = state.evaluation
                , rankings = state.rankings
                }

            EditAutonomous index text ->
                { autonomous = replaceFirst index text state.autonomous
                , teleop = state.teleop
                , endgame = state.endgame
                , currentPage = state.currentPage
                , event_name = state.event_name
                , evaluation = state.evaluation
                , rankings = state.rankings
                }

            EditAutonomousScore index text ->
                { autonomous = state.autonomous
                , teleop = state.teleop
                , endgame = state.endgame
                , currentPage = state.currentPage
                , event_name = state.event_name
                , evaluation =
                    { interview = state.evaluation.interview
                    , autonomousScore = replace index text state.evaluation.autonomousScore
                    , teleopScore = state.evaluation.teleopScore
                    , endgameScore = state.evaluation.endgameScore
                    , comments = state.evaluation.comments
                    }
                , rankings = state.rankings
                }

            EditTeleOp index text ->
                { autonomous = state.autonomous
                , teleop = replaceFirst index text state.teleop
                , endgame = state.endgame
                , currentPage = state.currentPage
                , event_name = state.event_name
                , evaluation = state.evaluation
                , rankings = state.rankings
                }

            EditTeleOpScore index text ->
                { autonomous = state.autonomous
                , teleop = state.teleop
                , endgame = state.endgame
                , currentPage = state.currentPage
                , event_name = state.event_name
                , evaluation =
                    { interview = state.evaluation.interview
                    , autonomousScore = state.evaluation.autonomousScore
                    , teleopScore = replace index text state.evaluation.teleopScore
                    , endgameScore = state.evaluation.endgameScore
                    , comments = state.evaluation.comments
                    }
                , rankings = state.rankings
                }

            EditEndGame index text ->
                { autonomous = state.autonomous
                , teleop = state.teleop
                , endgame = replaceFirst index text state.endgame
                , currentPage = state.currentPage
                , event_name = state.event_name
                , evaluation = state.evaluation
                , rankings = state.rankings
                }

            EditEndGameScore index text ->
                { autonomous = state.autonomous
                , teleop = state.teleop
                , endgame = state.endgame
                , currentPage = state.currentPage
                , event_name = state.event_name
                , evaluation =
                    { interview = state.evaluation.interview
                    , autonomousScore = state.evaluation.autonomousScore
                    , teleopScore = state.evaluation.teleopScore
                    , endgameScore = replace index text state.evaluation.endgameScore
                    , comments = state.evaluation.comments
                    }
                , rankings = state.rankings
                }

            RemoveAutonomous index ->
                { autonomous = delete index state.autonomous
                , teleop = state.teleop
                , endgame = state.endgame
                , currentPage = state.currentPage
                , event_name = state.event_name
                , evaluation = state.evaluation
                , rankings = state.rankings
                }

            RemoveTeleOp index ->
                { autonomous = state.autonomous
                , teleop = delete index state.teleop
                , endgame = state.endgame
                , currentPage = state.currentPage
                , event_name = state.event_name
                , evaluation = state.evaluation
                , rankings = state.rankings
                }

            RemoveEndGame index ->
                { autonomous = state.autonomous
                , teleop = state.teleop
                , endgame = delete index state.endgame
                , currentPage = state.currentPage
                , event_name = state.event_name
                , evaluation = state.evaluation
                , rankings = state.rankings
                }

            NewEventName s ->
                { autonomous = state.autonomous
                , teleop = state.teleop
                , endgame = state.endgame
                , event_name = s
                , currentPage = state.currentPage
                , evaluation = state.evaluation
                , rankings = state.rankings
                }

            ChangetoRankings ->
                { autonomous = state.autonomous
                , teleop = state.teleop
                , endgame = state.endgame
                , event_name = state.event_name
                , currentPage = RankingsPage
                , evaluation = state.evaluation
                , rankings = state.rankings
                }

            ChangetoInterviewEval school ->
                { autonomous = state.autonomous
                , teleop = state.teleop
                , endgame = state.endgame
                , event_name = state.event_name
                , currentPage = InterviewEvaluationPage school
                , evaluation =
                    { interview = True
                    , autonomousScore = List.map (\a -> 0) (state.autonomous)
                    , teleopScore = List.map (\a -> 0) (state.teleop)
                    , endgameScore = List.map (\a -> 0) (state.endgame)
                    , comments = state.evaluation.comments
                    }
                , rankings = state.rankings
                }

            ChangetoPerformanceEval school ->
                { autonomous = state.autonomous
                , teleop = state.teleop
                , endgame = state.endgame
                , event_name = state.event_name
                , currentPage = PerformanceEvaluationPage school
                , evaluation =
                    { interview = False
                    , autonomousScore = List.map (\a -> 0) (state.autonomous)
                    , teleopScore = List.map (\a -> 0) (state.teleop)
                    , endgameScore = List.map (\a -> 0) (state.endgame)
                    , comments = state.evaluation.comments
                    }
                , rankings = state.rankings
                }

            ChangetoNewSchoolsPage ->
                { autonomous = state.autonomous
                , teleop = state.teleop
                , endgame = state.endgame
                , event_name = state.event_name
                , currentPage = NewSchoolsPage ""
                , evaluation = state.evaluation
                , rankings = state.rankings
                }

            EditAutonomousMaxScore index score ->
                { autonomous = replaceSecond index score state.autonomous
                , teleop = state.teleop
                , endgame = state.endgame
                , event_name = state.event_name
                , currentPage = state.currentPage
                , evaluation = state.evaluation
                , rankings = state.rankings
                }

            EditTeleOpMaxScore index score ->
                { autonomous = state.autonomous
                , teleop = replaceSecond index score state.teleop
                , endgame = state.endgame
                , event_name = state.event_name
                , currentPage = state.currentPage
                , evaluation = state.evaluation
                , rankings = state.rankings
                }

            EditEndGameMaxScore index score ->
                { autonomous = state.autonomous
                , teleop = state.teleop
                , endgame = replaceSecond index score state.endgame
                , event_name = state.event_name
                , currentPage = state.currentPage
                , evaluation = state.evaluation
                , rankings = state.rankings
                }

            AddSchool school ->
                { autonomous = state.autonomous
                , teleop = state.teleop
                , endgame = state.endgame
                , event_name = state.event_name
                , currentPage = RankingsPage
                , rankings = addToBack ( [], school ) (state.rankings)
                , evaluation = state.evaluation
                }

            AddEval evaluation school ->
                { autonomous = state.autonomous
                , teleop = state.teleop
                , endgame = state.endgame
                , event_name = state.event_name
                , currentPage = RankingsPage
                , rankings = findAndAdd school evaluation state.rankings
                , evaluation = state.evaluation
                }

            EditComments string ->
                { autonomous = state.autonomous
                , teleop = state.teleop
                , endgame = state.endgame
                , event_name = state.event_name
                , currentPage = state.currentPage
                , rankings = state.rankings
                , evaluation =
                    { interview = state.evaluation.interview
                    , autonomousScore = state.evaluation.autonomousScore
                    , teleopScore = state.evaluation.teleopScore
                    , endgameScore = state.evaluation.endgameScore
                    , comments = string
                    }
                }

            EditSchoolName school ->
                { autonomous = state.autonomous
                , teleop = state.teleop
                , endgame = state.endgame
                , event_name = state.event_name
                , currentPage = NewSchoolsPage school
                , rankings = state.rankings
                , evaluation = state.evaluation
                }

            DeleteSchool index ->
                { autonomous = state.autonomous
                , teleop = state.teleop
                , endgame = state.endgame
                , event_name = state.event_name
                , currentPage = state.currentPage
                , rankings = delete index state.rankings
                , evaluation = state.evaluation
                }

            SeeFullReport school ->
                { autonomous = state.autonomous
                , teleop = state.teleop
                , endgame = state.endgame
                , event_name = state.event_name
                , currentPage = ReportPage school
                , rankings = state.rankings
                , evaluation = state.evaluation
                }
        )


port storeState : StorableState -> Cmd msg


updateAndSave : AppEvent -> AppState -> ( AppState, Cmd AppEvent )
updateAndSave event state =
    let
        newState =
            update event state
    in
        ( newState, storeState (encode newState) )


subscriptions : AppState -> Sub AppEvent
subscriptions state =
    Sub.none


view : AppState -> Html.Html AppEvent
view state =
    case state.currentPage of
        CriteriaPage ->
            Html.div [ Attrs.style [ ( "color", "blue" ) ] ]
                [ Html.span [] [ Html.text "Event name: " ]
                , Html.input [ Events.onInput (\s -> NewEventName s) ] []
                , Html.div []
                    [ Html.p [] [ Html.text ("Autonomous") ]
                    , Html.ul []
                        (List.indexedMap
                            (\index ( criteria, maxscore ) ->
                                Html.li []
                                    [ Html.input [ Events.onInput (\s -> EditAutonomous index s), Attrs.value criteria ] []
                                    , Html.text " max score: "
                                    , Html.input
                                        [ Events.onInput (\s -> EditAutonomousMaxScore index (stringToMaybeInt s))
                                        , Attrs.value (toString maxscore)
                                        , Attrs.type_ "number"
                                        , Attrs.name "quantity"
                                        , Attrs.min "0"
                                        , Attrs.max "1000"
                                        ]
                                        []
                                    , Html.button [ Events.onClick (RemoveAutonomous index) ] [ Html.text "-" ]
                                    ]
                            )
                            state.autonomous
                        )
                    , Html.button [ Events.onClick NewAutonomous ]
                        [ Html.text ("New Item") ]
                    ]
                , Html.div []
                    [ Html.p [] [ Html.text ("Tele-op") ]
                    , Html.ul []
                        (List.indexedMap
                            (\index ( criteria, maxscore ) ->
                                Html.li []
                                    [ Html.input [ Events.onInput (\s -> EditTeleOp index s), Attrs.value criteria ] []
                                    , Html.text " max score: "
                                    , Html.input
                                        [ Events.onInput (\s -> EditTeleOpMaxScore index (stringToMaybeInt s))
                                        , Attrs.value (toString maxscore)
                                        , Attrs.type_ "number"
                                        , Attrs.name "quantity"
                                        , Attrs.min "0"
                                        , Attrs.max "1000"
                                        ]
                                        []
                                    , Html.button [ Events.onClick (RemoveTeleOp index) ] [ Html.text "-" ]
                                    ]
                            )
                            state.teleop
                        )
                    , Html.button [ Events.onClick NewTeleOp ]
                        [ Html.text ("New Item") ]
                    ]
                , Html.div []
                    [ Html.p [] [ Html.text ("End Game") ]
                    , Html.ul []
                        (List.indexedMap
                            (\index ( criteria, maxscore ) ->
                                Html.li []
                                    [ Html.input [ Events.onInput (\s -> EditEndGame index s), Attrs.value criteria ] []
                                    , Html.text " max score: "
                                    , Html.input
                                        [ Events.onInput (\s -> EditEndGameMaxScore index (stringToMaybeInt s))
                                        , Attrs.value (toString maxscore)
                                        , Attrs.type_ "number"
                                        , Attrs.name "quantity"
                                        , Attrs.min "0"
                                        , Attrs.max "1000"
                                        ]
                                        []
                                    , Html.button [ Events.onClick (RemoveEndGame index) ] [ Html.text "-" ]
                                    ]
                            )
                            state.endgame
                        )
                    , Html.button [ Events.onClick NewEndGame ]
                        [ Html.text ("New Item") ]
                    ]
                , Html.button
                    [ Events.onClick (ChangetoNewSchoolsPage) ]
                    [ Html.text "Submit Criteria" ]
                , Html.p
                    [ Attrs.style [ ( "color", "red" ) ] ]
                    [ Html.text "**WARNING: You will not be able to alter criteria after submitting**" ]
                ]

        NewSchoolsPage schoolName ->
            Html.div []
                [ Html.text "School Name: "
                , Html.input [ Events.onInput (\s -> EditSchoolName s), Attrs.value schoolName ] []
                , Html.button [ Events.onClick (AddSchool schoolName) ] [ Html.text "Create" ]
                , Html.button [ Events.onClick ChangetoRankings ] [ Html.text "Cancel" ]
                ]

        InterviewEvaluationPage school ->
            Html.div
                []
                [ Html.div
                    []
                    [ Html.p [] [ Html.text "Autonomous" ]
                    , Html.ul []
                        (List.indexedMap
                            (\index ( ( criteria, maxscore ), score ) ->
                                Html.li []
                                    [ Html.span [] [ Html.text criteria ]
                                    , Html.text " "
                                    , Html.input
                                        [ Events.onInput (\s -> EditAutonomousScore index (stringToMaybeInt s))
                                        , Attrs.value (toString (Debug.log ("score") (score)))
                                        , Attrs.type_ "number"
                                        , Attrs.name "quantity"
                                        , Attrs.min "0"
                                        , Attrs.max "1000"
                                        ]
                                        []
                                    , Html.text "/"
                                    , Html.span [] [ Html.text (toString maxscore) ]
                                    ]
                            )
                            (zip state.autonomous state.evaluation.autonomousScore)
                        )
                    ]
                , Html.div []
                    [ Html.p [] [ Html.text "Tele-Op" ]
                    , Html.ul []
                        (List.indexedMap
                            (\index ( ( criteria, maxscore ), score ) ->
                                Html.li []
                                    [ Html.span [] [ Html.text criteria ]
                                    , Html.text " "
                                    , Html.input
                                        [ Events.onInput (\s -> EditTeleOpScore index (stringToMaybeInt s))
                                        , Attrs.value (toString score)
                                        , Attrs.type_ "number"
                                        , Attrs.name "quantity"
                                        , Attrs.min "0"
                                        , Attrs.max "1000"
                                        ]
                                        []
                                    , Html.text "/"
                                    , Html.span [] [ Html.text (toString maxscore) ]
                                    ]
                            )
                            (zip state.teleop state.evaluation.teleopScore)
                        )
                    ]
                , Html.div []
                    [ Html.p [] [ Html.text "End Game" ]
                    , Html.ul []
                        (List.indexedMap
                            (\index ( ( criteria, maxscore ), score ) ->
                                Html.li []
                                    [ Html.span [] [ Html.text criteria ]
                                    , Html.text " "
                                    , Html.input
                                        [ Events.onInput (\s -> EditEndGameScore index (stringToMaybeInt s))
                                        , Attrs.value (toString score)
                                        , Attrs.type_ "number"
                                        , Attrs.name "quantity"
                                        , Attrs.min "0"
                                        , Attrs.max "1000"
                                        ]
                                        []
                                    , Html.text "/"
                                    , Html.span [] [ Html.text (toString maxscore) ]
                                    ]
                            )
                            (zip state.endgame state.evaluation.endgameScore)
                        )
                    ]
                , Html.input [ Events.onInput (\s -> EditComments s) ] []
                , Html.button
                    [ Events.onClick (ChangetoRankings) ]
                    [ Html.text "Cancel" ]
                , Html.button [ Events.onClick (AddEval state.evaluation school) ] [ Html.text "Submit Evaluation" ]
                ]

        PerformanceEvaluationPage school ->
            Html.div
                []
                [ Html.div
                    []
                    [ Html.p [] [ Html.text "Autonomous" ]
                    , Html.ul []
                        (List.indexedMap
                            (\index ( ( criteria, maxscore ), score ) ->
                                Html.li []
                                    [ Html.span [] [ Html.text criteria ]
                                    , Html.text " "
                                    , Html.input
                                        [ Events.onInput (\s -> EditAutonomousScore index (stringToMaybeInt s))
                                        , Attrs.value (toString score)
                                        , Attrs.type_ "number"
                                        , Attrs.name "quantity"
                                        , Attrs.min "0"
                                        , Attrs.max "1000"
                                        ]
                                        []
                                    , Html.text "/"
                                    , Html.span [] [ Html.text (toString maxscore) ]
                                    ]
                            )
                            (zip state.autonomous state.evaluation.autonomousScore)
                        )
                    ]
                , Html.div []
                    [ Html.p [] [ Html.text "Tele-Op" ]
                    , Html.ul []
                        (List.indexedMap
                            (\index ( ( criteria, maxscore ), score ) ->
                                Html.li []
                                    [ Html.span [] [ Html.text criteria ]
                                    , Html.text " "
                                    , Html.input
                                        [ Events.onInput (\s -> EditTeleOpScore index (stringToMaybeInt s))
                                        , Attrs.value (toString score)
                                        , Attrs.type_ "number"
                                        , Attrs.name "quantity"
                                        , Attrs.min "0"
                                        , Attrs.max "1000"
                                        ]
                                        []
                                    , Html.text "/"
                                    , Html.span [] [ Html.text (toString maxscore) ]
                                    ]
                            )
                            (zip state.teleop state.evaluation.teleopScore)
                        )
                    ]
                , Html.div []
                    [ Html.p [] [ Html.text "End Game" ]
                    , Html.ul []
                        (List.indexedMap
                            (\index ( ( criteria, maxscore ), score ) ->
                                Html.li []
                                    [ Html.span [] [ Html.text criteria ]
                                    , Html.text " "
                                    , Html.input
                                        [ Events.onInput (\s -> EditEndGameScore index (stringToMaybeInt s))
                                        , Attrs.value (toString score)
                                        , Attrs.type_ "number"
                                        , Attrs.name "quantity"
                                        , Attrs.min "0"
                                        , Attrs.max "1000"
                                        ]
                                        []
                                    , Html.text "/"
                                    , Html.span [] [ Html.text (toString maxscore) ]
                                    ]
                            )
                            (zip state.endgame state.evaluation.endgameScore)
                        )
                    ]
                , Html.input [ Events.onInput (\s -> EditComments s) ] []
                , Html.button
                    [ Events.onClick (ChangetoRankings) ]
                    [ Html.text "Cancel" ]
                , Html.button [ Events.onClick (AddEval state.evaluation school) ] [ Html.text "Submit Evaluation" ]
                ]

        RankingsPage ->
            Html.div
                []
                [ Html.text ("Rankings " ++ state.event_name)
                , Html.ul []
                    (List.indexedMap
                        (\index ( evals, schoolName ) ->
                            Html.li []
                                [ Html.text (toString schoolName)
                                , Html.button [ Events.onClick (ChangetoInterviewEval schoolName) ] [ Html.text "Add Interview" ]
                                , Html.button [ Events.onClick (ChangetoPerformanceEval schoolName) ] [ Html.text "Add Performance" ]
                                , Html.button [ Events.onClick (DeleteSchool index) ] [ Html.p [ Attrs.style [ ( "Color", "Red" ) ] ] [ Html.text "Delete" ] ]
                                , Html.button [ Events.onClick (SeeFullReport schoolName) ] [ Html.text "See Full Report" ]
                                ]
                        )
                        (List.sortBy (rank)
                            (state.rankings)
                        )
                    )
                , Html.button [ Events.onClick ChangetoNewSchoolsPage ] [ Html.text "Add School" ]
                ]

        ReportPage schoolName ->
            let
                ( interviews, performances ) =
                    List.partition (isInterview) (finder schoolName state.rankings)
            in
                Html.div [ Attrs.style [ ( "display", "flex" ), ( "flex-direction", "row" ) ] ]
                    [ Html.text ("School: " ++ schoolName ++ " : ")
                    , Html.div
                        [ Attrs.style [ ( "border-color", "red" ) ] ]
                        [ Html.text " Interview"
                        , Html.ol [] (List.indexedMap (\index1 interview -> Html.li [] [ viewEvaluation state interview ]) interviews)
                        ]
                    , Html.div [ Attrs.style [ ( "border-color", "red " ) ] ]
                        [ Html.text " Performances"
                        , Html.ol [] (List.indexedMap (\index2 performance -> Html.li [] [ viewEvaluation state performance ]) performances)
                        ]
                    , Html.button [ Events.onClick (ChangetoRankings) ] [ Html.text "Back to Rankings " ]
                    ]


viewEvaluation : AppState -> Evaluation -> Html.Html AppEvent
viewEvaluation state evaluation =
    Html.div
        []
        [ Html.div []
            [ Html.p [] [ Html.text "Autonomous" ]
            , Html.ul []
                (List.indexedMap
                    (\index ( ( criteria, maxscore ), score ) ->
                        Html.li []
                            [ Html.span [] [ Html.text criteria ]
                            , Html.text " "
                            , Html.span [] [ Html.text (toString score) ]
                            , Html.text "/"
                            , Html.span [] [ Html.text (toString maxscore) ]
                            ]
                    )
                    (zip state.autonomous evaluation.autonomousScore)
                )
            ]
        , Html.div []
            [ Html.p [] [ Html.text "Tele-Op" ]
            , Html.ul []
                (List.indexedMap
                    (\index ( ( criteria, maxscore ), score ) ->
                        Html.li []
                            [ Html.span [] [ Html.text criteria ]
                            , Html.text " "
                            , Html.span [] [ Html.text (toString score) ]
                            , Html.text "/"
                            , Html.span [] [ Html.text (toString maxscore) ]
                            ]
                    )
                    (zip state.teleop evaluation.teleopScore)
                )
            ]
        , Html.div []
            [ Html.p [] [ Html.text "End Game" ]
            , Html.ul []
                (List.indexedMap
                    (\index ( ( criteria, maxscore ), score ) ->
                        Html.li []
                            [ Html.span [] [ Html.text criteria ]
                            , Html.text " "
                            , Html.span [] [ Html.text (toString score) ]
                            , Html.text "/"
                            , Html.span [] [ Html.text (toString maxscore) ]
                            ]
                    )
                    (zip state.endgame evaluation.endgameScore)
                )
            ]
        , Html.text (evaluation.comments)
        ]


main =
    Html.programWithFlags
        { init =
            (\s ->
                let
                    newState =
                        initFunction s
                in
                    ( newState, storeState (encode newState) )
            )
        , update = updateAndSave
        , subscriptions = subscriptions
        , view = view
        }


encode : AppState -> StorableState
encode state =
    case state.currentPage of
        CriteriaPage ->
            { autonomous = state.autonomous
            , teleop = state.teleop
            , endgame = state.endgame
            , criteriaPage = True
            , newSchoolsPage = Nothing
            , evaluationPage = Nothing
            , rankingsPage = False
            , rankings = state.rankings
            , evaluation = state.evaluation
            , event_name = state.event_name
            , reportPage = Nothing
            }

        RankingsPage ->
            { autonomous = state.autonomous
            , teleop = state.teleop
            , endgame = state.endgame
            , criteriaPage = False
            , newSchoolsPage = Nothing
            , evaluationPage = Nothing
            , rankingsPage = True
            , rankings = state.rankings
            , evaluation = state.evaluation
            , event_name = state.event_name
            , reportPage = Nothing
            }

        NewSchoolsPage schoolName ->
            { autonomous = state.autonomous
            , teleop = state.teleop
            , endgame = state.endgame
            , criteriaPage = False
            , newSchoolsPage = Just schoolName
            , evaluationPage = Nothing
            , rankingsPage = False
            , rankings = state.rankings
            , evaluation = state.evaluation
            , event_name = state.event_name
            , reportPage = Nothing
            }

        InterviewEvaluationPage schoolName ->
            { autonomous = state.autonomous
            , teleop = state.teleop
            , endgame = state.endgame
            , criteriaPage = False
            , newSchoolsPage = Nothing
            , evaluationPage = Just schoolName
            , rankingsPage = False
            , rankings = state.rankings
            , evaluation = state.evaluation
            , event_name = state.event_name
            , reportPage = Nothing
            }

        PerformanceEvaluationPage schoolName ->
            { autonomous = state.autonomous
            , teleop = state.teleop
            , endgame = state.endgame
            , criteriaPage = False
            , newSchoolsPage = Nothing
            , evaluationPage = Just schoolName
            , rankingsPage = False
            , rankings = state.rankings
            , evaluation = state.evaluation
            , event_name = state.event_name
            , reportPage = Nothing
            }

        ReportPage schoolName ->
            { autonomous = state.autonomous
            , teleop = state.teleop
            , endgame = state.endgame
            , criteriaPage = False
            , newSchoolsPage = Nothing
            , evaluationPage = Nothing
            , rankingsPage = False
            , rankings = state.rankings
            , evaluation = state.evaluation
            , event_name = state.event_name
            , reportPage = Just schoolName
            }


decode : StorableState -> AppState
decode state =
    if state.criteriaPage then
        { autonomous = state.autonomous
        , teleop = state.teleop
        , endgame = state.endgame
        , currentPage = CriteriaPage
        , evaluation = state.evaluation
        , event_name = state.event_name
        , rankings = state.rankings
        }
    else if state.rankingsPage then
        { autonomous = state.autonomous
        , teleop = state.teleop
        , endgame = state.endgame
        , currentPage = RankingsPage
        , evaluation = state.evaluation
        , event_name = state.event_name
        , rankings = state.rankings
        }
    else if state.reportPage == Just "" then
        case state.reportPage of
            Just schoolName ->
                { autonomous = state.autonomous
                , teleop = state.teleop
                , endgame = state.endgame
                , currentPage = ReportPage schoolName
                , evaluation = state.evaluation
                , event_name = state.event_name
                , rankings = state.rankings
                }

            Nothing ->
                { autonomous = state.autonomous
                , teleop = state.teleop
                , endgame = state.endgame
                , currentPage = RankingsPage
                , evaluation = state.evaluation
                , event_name = state.event_name
                , rankings = state.rankings
                }
    else
        case state.newSchoolsPage of
            Just schoolName ->
                { autonomous = state.autonomous
                , teleop = state.teleop
                , endgame = state.endgame
                , currentPage = NewSchoolsPage schoolName
                , evaluation = state.evaluation
                , event_name = state.event_name
                , rankings = state.rankings
                }

            Nothing ->
                case state.evaluationPage of
                    Just schoolName ->
                        { autonomous = state.autonomous
                        , teleop = state.teleop
                        , endgame = state.endgame
                        , currentPage = InterviewEvaluationPage schoolName
                        , evaluation = state.evaluation
                        , event_name = state.event_name
                        , rankings = state.rankings
                        }

                    Nothing ->
                        { autonomous = state.autonomous
                        , teleop = state.teleop
                        , endgame = state.endgame
                        , currentPage = RankingsPage
                        , evaluation = state.evaluation
                        , event_name = state.event_name
                        , rankings = state.rankings
                        }
