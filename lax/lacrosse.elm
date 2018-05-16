module Lacrosse exposing (..)

import Html
import Html.Attributes as Attrs
import Html.Events as Events
import Json.Decode


type alias AppState =
    { attack : List ( String, String, Bool )
    , midfield : List ( String, String, Bool )
    , defense : List ( String, String, Bool )
    }


type alias Schedule =
    { attacker : List ( String, List ( Float, Float ) )
    , midfielder : List ( String, List ( Float, Float ) )
    , defender : List ( String, List ( Float, Float ) )
    }


initialState =
    { attack = [ ( "", "Average", False ) ]
    , midfield = [ ( "", "Average", False ) ]
    , defense = [ ( "", "Average", False ) ]
    }


schedule =
    { attacker = [ ( "Andrew", [ ( 0, 1.0 ), ( 2.5, 3 ), ( 3.5, 5 ), ( 7, 11 ), ( 15, 22 ) ] ), ( "Sage", [ ( 1.0, 2.5 ), ( 3, 4.5 ) ] ) ]
    , midfielder = [ ( "Rick", [ ( 0, 1.0 ), ( 2, 4.5 ) ] ), ( "Julia", [ ( 1.0, 2 ), ( 4.5, 5 ) ] ) ]
    , defender = [ ( "Michelle", [ ( 0, 1.0 ), ( 2, 2.5 ) ] ), ( "Sophie", [ ( 1.0, 2 ), ( 2.5, 4.5 ) ] ) ]
    }


type AppEvent
    = NewDefender
    | NewAttacker
    | NewMidfielder
    | DeleteTaskA Int
    | DeleteTaskM Int
    | DeleteTaskD Int
    | EditNameA Int String
    | EditAbilityA Int String
    | EditStarterA Int Bool
    | EditNameM Int String
    | EditAbilityM Int String
    | EditStarterM Int Bool
    | EditNameD Int String
    | EditAbilityD Int String
    | EditStarterD Int Bool


addToBack : a -> List a -> List a
addToBack item list =
    list ++ [ item ]


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


replaceName : Int -> a -> List ( a, a, b ) -> List ( a, a, b )
replaceName index item list =
    case list of
        [] ->
            []

        h :: t ->
            if index == 0 then
                let
                    ( f, s, th ) =
                        h
                in
                    ( item, s, th ) :: t
            else
                h :: replaceName (index - 1) item t


replaceAbility : Int -> a -> List ( a, a, b ) -> List ( a, a, b )
replaceAbility index item list =
    case list of
        [] ->
            []

        h :: t ->
            if index == 0 then
                let
                    ( f, s, th ) =
                        h
                in
                    ( f, item, th ) :: t
            else
                h :: replaceAbility (index - 1) item t


replaceStarter : Int -> b -> List ( a, a, b ) -> List ( a, a, b )
replaceStarter index item list =
    case list of
        [] ->
            []

        h :: t ->
            if index == 0 then
                let
                    ( f, s, th ) =
                        h
                in
                    ( f, s, item ) :: t
            else
                h :: replaceStarter (index - 1) item t


midfield : List ( String, String, Bool ) -> List ( String, List ( Float, Float ) )
midfield list =
    if List.length list == 3 then
        List.map minPlayers list
    else if allEqual list then
        finishBlocks (attackDefenseEqualPlayerTimes (reorderStarter list) (attackDefenseBlocks (reorderStarter list)))
    else
        finishBlocks (removeLevel (transferBlocks (attackDefenseUnequalPlayerTimes (reorderLevel (reorderStarter list)) (attackDefenseBlocks (reorderLevel (reorderStarter list)))) (attackDefenseBlocks (reorderLevel (reorderStarter list)))))


attackAndDefense : List ( String, String, Bool ) -> List ( String, List ( Float, Float ) )
attackAndDefense list =
    if List.length list == 4 then
        List.map minPlayers list
    else if allEqual list then
        finishBlocks (midfieldEqualPlayerTimes (reorderStarter list) (midfieldBlocks (reorderStarter list)))
    else
        finishBlocks (removeLevel (transferBlocks (midfieldUnequalPlayerTimes (reorderLevel (reorderStarter list)) (midfieldBlocks (reorderLevel (reorderStarter list)))) (midfieldBlocks (reorderLevel (reorderStarter list)))))


minPlayers : ( String, String, Bool ) -> ( String, List ( Float, Float ) )
minPlayers player =
    let
        ( x, y, z ) =
            player
    in
        ( x, [ ( 0, 25 ) ] )


allEqual : List ( String, String, Bool ) -> Bool
allEqual list =
    case list of
        [] ->
            False

        [ ( _, "Average", _ ) ] ->
            True

        h :: t ->
            let
                ( x, y, z ) =
                    h
            in
                if y /= "Average" then
                    False
                else
                    allEqual t


starter : ( String, String, Bool ) -> Bool
starter player =
    let
        ( x, y, z ) =
            player
    in
        if z == True then
            True
        else
            False


reorderStarter : List ( String, String, Bool ) -> List ( String, String, Bool )
reorderStarter list =
    case list of
        [] ->
            []

        h :: t ->
            if starter h == True then
                h :: reorderStarter t
            else
                reorderStarter t ++ [ h ]


level : ( String, String, Bool ) -> String
level player =
    let
        ( x, y, z ) =
            player
    in
        y


reorderLevel : List ( String, String, Bool ) -> List ( String, String, Bool )
reorderLevel list =
    case list of
        [] ->
            []

        x :: [] ->
            [ x ]

        h :: m :: t ->
            if level m == "Above Average" && level h /= "Above Average" && starter h == False then
                (reorderLevel (m :: t)) ++ [ h ]
            else if level m == "Above Average" && level h == "Above Average" then
                h :: reorderLevel (m :: t)
            else if starter h == True then
                h :: reorderLevel (m :: t)
            else
                (reorderLevel (h :: t)) ++ [ m ]


createBlocks : Float -> Float -> List ( Float, Float )
createBlocks quantity length =
    if quantity == 0 then
        []
    else
        ( ((toFloat (floor ((length * (quantity - 1)) * 100))) / 100), ((toFloat (floor ((length * quantity) * 100))) / 100) ) :: createBlocks (quantity - 1) length


attackDefenseBlocks : List ( String, String, Bool ) -> List ( Float, Float )
attackDefenseBlocks list =
    if List.length list /= 6 then
        let
            x =
                toFloat (List.length list)
        in
            List.reverse (createBlocks x (25 / x))
    else
        List.reverse (createBlocks 4 6.25)


midfieldBlocks : List ( String, String, Bool ) -> List ( Float, Float )
midfieldBlocks list =
    if List.length list == 8 then
        List.reverse (createBlocks 4 6.25)
    else
        let
            x =
                toFloat (List.length list)
        in
            List.reverse (createBlocks x (25 / x))


attackDefenseInplay : Int -> Int -> Int -> Int -> Bool
attackDefenseInplay p t np nip =
    List.member p ([ (nip * t) % np ] ++ [ ((nip * t) + 1) % np ] ++ [ ((nip * t) + 2) % np ])


midfieldInplay : Int -> Int -> Int -> Int -> Bool
midfieldInplay p t np nip =
    List.member p ([ (nip * t) % np ] ++ [ ((nip * t) + 1) % np ] ++ [ ((nip * t) + 2) % np ] ++ [ ((nip * t) + 3) % np ])


indexedFilterMap : (Int -> a -> Maybe b) -> List a -> List b
indexedFilterMap function list =
    List.filterMap (\x -> x) (List.indexedMap function list)


attackDefenseEqualPlayerTimes : List ( String, String, Bool ) -> List ( Float, Float ) -> List ( String, List ( Float, Float ) )
attackDefenseEqualPlayerTimes playerlist timelist =
    List.indexedMap
        (\indexplayer player ->
            let
                ( x, y, z ) =
                    player
            in
                ( x
                , indexedFilterMap
                    (\indextime time ->
                        if attackDefenseInplay indexplayer indextime (List.length playerlist) 3 then
                            Just time
                        else
                            Nothing
                    )
                    timelist
                )
        )
        playerlist


midfieldEqualPlayerTimes : List ( String, String, Bool ) -> List ( Float, Float ) -> List ( String, List ( Float, Float ) )
midfieldEqualPlayerTimes playerlist timelist =
    List.indexedMap
        (\indexplayer player ->
            let
                ( x, y, z ) =
                    player
            in
                ( x
                , indexedFilterMap
                    (\indextime time ->
                        if midfieldInplay indexplayer indextime (List.length playerlist) 4 then
                            Just time
                        else
                            Nothing
                    )
                    timelist
                )
        )
        playerlist


concatenateBlocks : List ( Float, Float ) -> List ( Float, Float )
concatenateBlocks list =
    case list of
        [] ->
            []

        _ :: [] ->
            []

        [ ( w, x ), ( y, z ) ] ->
            if x == y then
                [ ( w, z ) ]
            else
                [ ( w, x ), ( y, z ) ]

        ( w, x ) :: ( y, z ) :: t ->
            if x == y then
                concatenateBlocks (( w, z ) :: t)
            else
                ( w, x ) :: concatenateBlocks (( y, z ) :: t)


attachBlocks : ( String, List ( Float, Float ) ) -> ( String, List ( Float, Float ) )
attachBlocks player =
    let
        ( x, y ) =
            player
    in
        ( x, concatenateBlocks y )


finishBlocks : List ( String, List ( Float, Float ) ) -> List ( String, List ( Float, Float ) )
finishBlocks list =
    List.map attachBlocks list


attackDefenseUnequalPlayerTimes : List ( String, String, Bool ) -> List ( Float, Float ) -> List ( String, String, List ( Float, Float ) )
attackDefenseUnequalPlayerTimes playerlist timelist =
    List.indexedMap
        (\indexplayer player ->
            let
                ( x, y, z ) =
                    player
            in
                ( x
                , y
                , indexedFilterMap
                    (\indextime time ->
                        if attackDefenseInplay indexplayer indextime (List.length playerlist) 3 then
                            Just time
                        else
                            Nothing
                    )
                    timelist
                )
        )
        playerlist


midfieldUnequalPlayerTimes : List ( String, String, Bool ) -> List ( Float, Float ) -> List ( String, String, List ( Float, Float ) )
midfieldUnequalPlayerTimes playerlist timelist =
    List.indexedMap
        (\indexplayer player ->
            let
                ( x, y, z ) =
                    player
            in
                ( x
                , y
                , indexedFilterMap
                    (\indextime time ->
                        if midfieldInplay indexplayer indextime (List.length playerlist) 4 then
                            Just time
                        else
                            Nothing
                    )
                    timelist
                )
        )
        playerlist


contains : List a -> a -> Bool
contains list item =
    case list of
        [] ->
            False

        h :: t ->
            if h == item then
                True
            else
                contains t item


remove : List a -> a -> List a
remove list item =
    case list of
        [] ->
            []

        h :: t ->
            if h == item then
                t
            else
                h :: remove t item


findBlock : List ( String, String, List ( Float, Float ) ) -> ( Float, Float ) -> Maybe String
findBlock list block =
    case list of
        [] ->
            Nothing

        h :: t ->
            let
                ( x, y, z ) =
                    h
            in
                if y == "Below Average" then
                    if contains z block then
                        Just x
                    else
                        findBlock t block
                else
                    findBlock t block


findAndRemoveBlock : List ( String, String, List ( Float, Float ) ) -> Maybe String -> ( Float, Float ) -> List ( String, String, List ( Float, Float ) )
findAndRemoveBlock list name block =
    case ( list, name ) of
        ( [], _ ) ->
            []

        ( h :: t, Nothing ) ->
            h :: t

        ( h :: t, Just p ) ->
            let
                ( x, y, z ) =
                    h
            in
                if x == p then
                    ( x, y, remove z block ) :: t
                else
                    h :: findAndRemoveBlock t name block


findOpening : List ( Float, Float ) -> List ( Float, Float ) -> ( Float, Float )
findOpening blocklist abovelist =
    case ( blocklist, abovelist ) of
        ( [], [] ) ->
            ( 0, 0 )

        ( [], _ :: _ ) ->
            ( 0, 0 )

        ( _ :: _, [] ) ->
            ( 0, 0 )

        ( w :: x, y :: z ) ->
            if w /= y then
                w
            else
                findOpening x z


findPlace : List ( Float, Float ) -> ( Float, Float ) -> List ( Float, Float )
findPlace list block =
    case ( list, block ) of
        ( [], ( _, _ ) ) ->
            []

        ( h :: [], ( x, y ) ) ->
            let
                ( a, b ) =
                    h
            in
                if a < x then
                    [ h, ( x, y ) ]
                else
                    [ ( x, y ), h ]

        ( [ ( a, b ), ( c, d ) ], ( x, y ) ) ->
            if x < a then
                [ ( x, y ), ( a, b ), ( c, d ) ]
            else if (a < x) && (x < c) then
                [ ( a, b ), ( x, y ), ( c, d ) ]
            else
                [ ( a, b ), ( c, d ), ( x, y ) ]

        ( ( a, b ) :: ( c, d ) :: t, ( x, y ) ) ->
            if x < a then
                ( x, y ) :: ( a, b ) :: ( c, d ) :: t
            else if (a < x) && (x < c) then
                ( a, b ) :: ( x, y ) :: ( c, d ) :: t
            else
                ( a, b ) :: ( c, d ) :: (findPlace t ( x, y ))


transferBlocks : List ( String, String, List ( Float, Float ) ) -> List ( Float, Float ) -> List ( String, String, List ( Float, Float ) )
transferBlocks playerlist blocklist =
    case playerlist of
        [] ->
            []

        h :: t ->
            let
                ( x, y, z ) =
                    h
            in
                if y == "Above Average" then
                    if findBlock t (findOpening blocklist z) /= Nothing then
                        ( x, y, findPlace z (findOpening blocklist z) ) :: transferBlocks (findAndRemoveBlock t (findBlock t (findOpening blocklist z)) (findOpening blocklist z)) blocklist
                    else
                        h :: transferBlocks t blocklist
                else
                    h :: transferBlocks t blocklist


removeLevel : List ( String, String, List ( Float, Float ) ) -> List ( String, List ( Float, Float ) )
removeLevel list =
    case list of
        [] ->
            []

        h :: t ->
            let
                ( x, y, z ) =
                    h
            in
                ( x, z ) :: removeLevel t


indflexbox : ( String, List ( Float, Float ) ) -> Html.Html msg
indflexbox ( a, b ) =
    Html.div [ Attrs.style [ ( "display", "flex" ), ( "flex-direction", "row" ), ( "justify-content", "flex-end" ) ] ]
        [ Html.div [ Attrs.style [ ( "color", "white" ) ] ] [ Html.text (a ++ ":  ") ]
        , Html.div [ Attrs.style [ ( "border-style", "solid" ) ] ] [ (indSchedule (coloring (scheduleBox b 0 25))) ]
        ]


scheduleBox : List ( Float, Float ) -> Float -> Float -> List ( Float, Float, Bool )
scheduleBox list start end =
    case list of
        [] ->
            []

        [ ( x, y ) ] ->
            if (x /= start && y /= end) then
                [ ( start, x, False ), ( x, y, True ), ( y, end, False ) ]
            else if (x == start && y /= end) then
                [ ( x, y, True ), ( y, end, False ) ]
            else if (x /= start && y == end) then
                [ ( start, x, False ), ( x, y, True ) ]
            else
                [ ( start, end, True ) ]

        ( x, y ) :: t ->
            if x == start then
                ( x, y, True ) :: scheduleBox t y end
            else
                ( start, x, False ) :: ( x, y, True ) :: scheduleBox t y end


coloring : List ( Float, Float, Bool ) -> List (Html.Html msg)
coloring list =
    case list of
        [] ->
            []

        ( x, y, bool ) :: t ->
            if bool == True then
                Html.div [ Attrs.style [ ( "display", "flex" ), ( "background-color", "rgb(52, 171, 177)" ), ( "height", "25px" ), ( "width", toString (abs ((x - y) * 28)) ++ "px" ), ( "justify-content", "space-between" ) ] ]
                    [ Html.div [ Attrs.style [ ( "font-family", "stencil" ) ] ] [ Html.text (toString x) ], Html.div [ Attrs.style [ ( "font-family", "stencil" ) ] ] [ Html.text " - " ], Html.div [ Attrs.style [ ( "font-family", "stencil" ) ] ] [ Html.text (toString y) ] ]
                    :: coloring t
            else
                Html.div [ Attrs.style [ ( "display", "flex" ), ( "background-color", "white" ), ( "height", "25px" ), ( "width", toString (abs ((x - y) * 28)) ++ "px" ) ] ]
                    [ Html.text "" ]
                    :: coloring t


indSchedule : List (Html.Html msg) -> Html.Html msg
indSchedule list =
    Html.div [ Attrs.style [ ( "display", "flex" ), ( "flex-direction", "row" ), ( "border-style", "solid" ) ] ] (list)


update : AppEvent -> AppState -> AppState
update event state =
    case event of
        NewDefender ->
            { state | defense = addToBack ( "", "Average", False ) state.defense }

        NewAttacker ->
            { state | attack = addToBack ( "", "Average", False ) state.attack }

        NewMidfielder ->
            { state | midfield = addToBack ( "", "Average", False ) state.midfield }

        EditNameA index text ->
            { state | attack = replaceName index text state.attack }

        EditAbilityA index text ->
            { state | attack = replaceAbility index text state.attack }

        EditStarterA index bool ->
            { state | attack = replaceStarter index bool state.attack }

        EditNameM index text ->
            { state | midfield = replaceName index text state.midfield }

        EditAbilityM index text ->
            { state | midfield = replaceAbility index text state.midfield }

        EditStarterM index bool ->
            { state | midfield = replaceStarter index bool state.midfield }

        EditNameD index text ->
            { state | defense = replaceName index text state.defense }

        EditAbilityD index text ->
            { state | defense = replaceAbility index text state.defense }

        EditStarterD index bool ->
            { state | defense = replaceStarter index bool state.defense }

        DeleteTaskA index ->
            { state | attack = delete index state.attack }

        DeleteTaskM index ->
            { state | midfield = delete index state.midfield }

        DeleteTaskD index ->
            { state | defense = delete index state.defense }


viewAttack : AppState -> Html.Html AppEvent
viewAttack state =
    Html.div [ Attrs.style [ ( "border-style", "ridge" ), ( "padding", "5px" ), ( "width", "810px" ) ] ]
        [ Html.h1 [ Attrs.style [ ( "color", "white" ), ( "font-family", "stencil" ) ] ] [ Html.text "Attack" ]
        , Html.button [ Events.onClick NewAttacker ] [ Html.text "New Player" ]
        , Html.ul []
            (List.indexedMap
                (\index player ->
                    let
                        ( name, ability, start ) =
                            player
                    in
                        Html.li []
                            [ Html.input
                                [ Events.onInput (\s -> EditNameA index s), Attrs.value name ]
                                []
                            , Html.button [ Events.onClick (DeleteTaskA index) ] [ Html.text "Delete Player" ]
                            , Html.select
                                [ Events.on
                                    "change"
                                    (Json.Decode.map (\s -> EditAbilityA index s) Events.targetValue)
                                ]
                                [ Html.option [ Attrs.value "Average" ] [ Html.text "Average" ]
                                , Html.option [ Attrs.value "Below Average" ] [ Html.text "Below Average" ]
                                , Html.option [ Attrs.value "Above Average" ] [ Html.text "Above Average" ]
                                ]
                            , Html.label
                                [ Attrs.style [ ( "color", "white" ) ] ]
                                [ Html.input [ Attrs.type_ "checkbox", Events.onCheck (\bool -> EditStarterA index bool) ] [], Html.text "Starter? Check for yes, blank for no!" ]
                            ]
                )
                state.attack
            )
        , Html.div [ Attrs.style [ ( "color", "white" ) ] ] [ viewResultAttack state ]
        ]


viewMidfield : AppState -> Html.Html AppEvent
viewMidfield state =
    Html.div [ Attrs.style [ ( "border-style", "ridge" ), ( "padding", "5px" ), ( "width", "810px" ) ] ]
        [ Html.h1 [ Attrs.style [ ( "color", "white" ), ( "font-family", "stencil" ) ] ] [ Html.text "Midfield" ]
        , Html.button [ Events.onClick NewMidfielder ] [ Html.text "New Player" ]
        , Html.ul []
            (List.indexedMap
                (\index player ->
                    let
                        ( name, ability, start ) =
                            player
                    in
                        Html.li []
                            [ Html.input
                                [ Events.onInput (\s -> EditNameM index s), Attrs.value name ]
                                []
                            , Html.button [ Events.onClick (DeleteTaskM index) ] [ Html.text "Delete Player" ]
                            , Html.select
                                [ Events.on
                                    "change"
                                    (Json.Decode.map (\s -> EditAbilityM index s) Events.targetValue)
                                ]
                                [ Html.option [ Attrs.value "Average" ] [ Html.text "Average" ]
                                , Html.option [ Attrs.value "Below Average" ] [ Html.text "Below Average" ]
                                , Html.option [ Attrs.value "Above Average" ] [ Html.text "Above Average" ]
                                ]
                            , Html.label
                                [ Attrs.style [ ( "color", "white" ) ] ]
                                [ Html.input [ Attrs.type_ "checkbox", Events.onCheck (\bool -> EditStarterM index bool) ] [], Html.text "Starter? Check for yes, blank for no!" ]
                            ]
                )
                state.midfield
            )
        , Html.div [ Attrs.style [ ( "color", "white" ) ] ] [ viewResultMidfield state ]
        ]


viewDefense : AppState -> Html.Html AppEvent
viewDefense state =
    Html.div
        [ Attrs.style
            [ ( "border-style", "ridge" )
            , ( "padding", "5px" )
            , ( "width", "810px" )
            ]
        ]
        [ Html.h1 [ Attrs.style [ ( "color", "white" ), ( "font-family", "stencil" ) ] ] [ Html.text "Defense" ]
        , Html.button [ Events.onClick NewDefender ] [ Html.text "New Player" ]
        , Html.ul []
            (List.indexedMap
                (\index player ->
                    let
                        ( name, ability, start ) =
                            player
                    in
                        Html.li []
                            [ Html.input
                                [ Events.onInput (\s -> EditNameD index s), Attrs.value name ]
                                []
                            , Html.button [ Events.onClick (DeleteTaskD index) ] [ Html.text "Delete Player" ]
                            , Html.select
                                [ Events.on
                                    "change"
                                    (Json.Decode.map (\s -> EditAbilityD index s) Events.targetValue)
                                ]
                                [ Html.option [ Attrs.value "Average" ] [ Html.text "Average" ]
                                , Html.option [ Attrs.value "Below Average" ] [ Html.text "Below Average" ]
                                , Html.option [ Attrs.value "Above Average" ] [ Html.text "Above Average" ]
                                ]
                            , Html.label
                                [ Attrs.style [ ( "color", "white" ) ] ]
                                [ Html.input [ Attrs.type_ "checkbox", Events.onCheck (\bool -> EditStarterD index bool) ] [], Html.text "Starter? Check for yes, blank for no!" ]
                            ]
                )
                state.defense
            )
        , Html.div [ Attrs.style [ ( "color", "white" ) ] ] [ viewResultDefense state ]
        ]


viewSchedule : AppState -> Html.Html msg
viewSchedule state =
    Html.div [ Attrs.style [ ( "margin", "20px" ) ] ]
        [ Html.div [ Attrs.style [ ( "border-style", "ridge" ), ( "display", "flex" ), ( "flex-direction", "row" ), ( "background", "linear-gradient(gold, #40dc40 )" ), ( "height", "35px" ) ] ]
            [ Html.p [ Attrs.style [ ( "font-size", "18px" ), ( "padding", "5px" ), ( "color", "white" ), ( "margin", "0px" ), ( "font-family", "stencil" ) ] ] [ Html.text "Position/Time" ]
            ]
        , Html.div [ Attrs.style [ ( "border-style", "ridge" ), ( "padding-right", "20px" ), ( "background", "linear-gradient(#40dc40, royalblue )" ) ] ]
            [ Html.p [ Attrs.style [ ( "font-size", "20px" ), ( "padding", "5px" ), ( "color", "white" ), ( "font-family", "stencil" ) ] ] [ Html.text "Attackers" ]
            , Html.p [] (List.map indflexbox (attackAndDefense state.attack))
            ]
        , Html.div [ Attrs.style [ ( "border-style", "ridge" ), ( "padding-right", "20px" ), ( "background", "linear-gradient(royalblue, red )" ) ] ]
            [ Html.p [ Attrs.style [ ( "font-size", "20px" ), ( "padding", "5px" ), ( "color", "white" ), ( "font-family", "stencil" ) ] ] [ Html.text "Midfielders" ]
            , Html.p [] (List.map indflexbox (midfield state.midfield))
            ]
        , Html.div [ Attrs.style [ ( "border-style", "ridge" ), ( "padding-right", "20px" ), ( "background", "linear-gradient(red, gold )" ) ] ]
            [ Html.p [ Attrs.style [ ( "font-size", "20px" ), ( "padding", "5px" ), ( "color", "white" ), ( "font-family", "stencil" ) ] ] [ Html.text "Defenders" ]
            , Html.p [] (List.map indflexbox (attackAndDefense state.defense))
            ]
        ]


viewResultAttack : AppState -> Html.Html AppEvent
viewResultAttack state =
    if (List.length (state.attack)) < 4 then
        Html.text "Enter More Players For Attack In This Order: Name of Player, Ability, Starter/Not Starter. Continue by pressing the new player button."
    else
        Html.text "Keep inputting players! If you have finished all 3 position groups, look at the schedule! If not, finish the other position groups."


viewResultMidfield : AppState -> Html.Html AppEvent
viewResultMidfield state =
    if (List.length (state.midfield)) < 3 then
        Html.text "Enter More Players For Midfield In This Order: Name of Player, Ability, Starter/Not Starter. Continue by pressing the new player button."
    else
        Html.text "Keep inputting players! If you have finished all 3 position groups, look at the schedule! If not, finish the other position groups."


viewResultDefense : AppState -> Html.Html AppEvent
viewResultDefense state =
    if (List.length (state.defense)) < 4 then
        Html.text "Enter More Players For Defense In This Order: Name of Player, Ability, Starter/Not Starter. Continue by pressing the new player button."
    else
        Html.text "Keep inputting players! If you have finished all 3 position groups, look at the schedule! If not, finish the other position groups."


view : AppState -> Html.Html AppEvent
view state =
    Html.div []
        [ Html.h1 [ Attrs.style [ ( "margin-bottom", "0" ), ( "color", "whitesmoke" ), ( "font-size", "50px" ), ( "text-align", "center" ), ( "background", "linear-gradient(#b71a1a, white)" ), ( "margin-top", "0px" ), ( "font-family", "stencil" ) ] ] [ Html.text "Gameday Lineup" ]
        , Html.div
            [ Attrs.style [ ( "display", "flex" ), ( "flex-direction", "row" ), ( "background", "linear-gradient(red, gold)" ), ( "margin", "20px" ) ] ]
            [ viewAttack state, viewMidfield state, viewDefense state ]
        , Html.div
            []
            [ viewSchedule state ]
        ]


main =
    Html.beginnerProgram
        { model = initialState, view = view, update = update }
