module TextVisualizerFinal exposing (..)

import Html
import Random exposing (..)
import Html
import Html.Attributes as Attrs
import Html.Events as Events
import Svg
import Svg.Attributes as SvgAttrs
import List


basicContainer =
    Attrs.style
        [ ( "padding", "10px" )
        , ( "margin", "10px" )
        , ( "border-style", "solid" )
        , ( "border-color", "white" )
        , ( "background-color", "#ffffff" )
        , ( "display", "flex" )
        , ( "height", "100px" )
        , ( "width", "500px" )
        , ( "justify-content", "center" )
        ]


type alias Point =
    { x : Int
    , y : Int
    }


type alias AppState =
    { textbox1 : Int
    , textbox2 : Int
    , textbox3 : Int
    , textbox4 : Int
    , textbox5 : Int
    , functionx : Fn
    , functiony : Fn
    , functionz : Fn
    , functiona : Fn
    , functionb : Fn
    }


initialState : AppState
initialState =
    { textbox1 = 0
    , textbox2 = 0
    , textbox3 = 0
    , textbox4 = 0
    , textbox5 = 0
    , functionx = Ident
    , functiony = Ident
    , functionz = Ident
    , functiona = Ident
    , functionb = Ident
    }


type AppEvent
    = Textboxchange1 String
    | Textboxchange2 String
    | Textboxchange3 String
    | Textboxchange4 String
    | Textboxchange5 String
    | ButtonPressed


type Fn
    = Sin Fn
    | Cos Fn
    | Xsquared Fn
    | Xcubed Fn
    | Ident
    | Avg Fn Fn


eval : Fn -> Float -> Float
eval f x =
    case f of
        Sin a ->
            let
                s =
                    eval a x
            in
                sin (pi * s)

        Cos a ->
            let
                s =
                    eval a x
            in
                cos (pi * s)

        Xsquared a ->
            let
                s =
                    eval a x
            in
                s ^ 2

        Xcubed a ->
            let
                s =
                    eval a x
            in
                s ^ 3

        Ident ->
            x

        Avg a b ->
            let
                s =
                    eval a x + eval b x
            in
                s / 2


type alias Model =
    { seed : Random.Seed, value : Fn }


seedGenerator : Seed -> Int -> ( Fn, Seed )
seedGenerator seed depth =
    if depth == 0 then
        ( Ident, seed )
    else
        let
            ( n, seed2 ) =
                step (Random.int 0 depth) (seed)
        in
            if n == 1 then
                let
                    ( f, seed3 ) =
                        seedGenerator seed2 (depth - 1)
                in
                    ( Sin f, seed3 )
            else if n == 2 then
                let
                    ( f, seed3 ) =
                        seedGenerator seed2 (depth - 1)
                in
                    ( Cos f, seed3 )
            else if n == 3 then
                let
                    ( f, seed3 ) =
                        seedGenerator seed2 (depth - 1)
                in
                    ( Xsquared f, seed3 )
            else if n == 4 then
                let
                    ( f, seed3 ) =
                        seedGenerator seed2 (depth - 1)
                in
                    ( Xcubed f, seed3 )
            else
                let
                    ( f, seed3 ) =
                        seedGenerator seed2 (depth - 1)
                in
                    let
                        ( g, seed4 ) =
                            seedGenerator seed3 (depth - 1)
                    in
                        ( Avg f g, seed4 )


scaleTo1 : Float -> Float
scaleTo1 pair =
    pair / 600


scaleTo600 : Float -> Float
scaleTo600 pair =
    pair * 600


stringToMaybeInt : String -> Maybe Int
stringToMaybeInt s =
    Result.toMaybe (String.toInt s)


range : Float -> Float -> Float -> List Float
range start step end =
    if start >= end then
        []
    else
        start :: range (start + step) step end


update : AppEvent -> AppState -> AppState
update event state =
    case event of
        Textboxchange1 string ->
            case stringToMaybeInt string of
                Just newValue ->
                    { state | textbox1 = newValue }

                _ ->
                    state

        Textboxchange2 string ->
            case stringToMaybeInt string of
                Just newValue ->
                    { state | textbox2 = newValue }

                _ ->
                    state

        Textboxchange3 string ->
            case stringToMaybeInt string of
                Just newValue ->
                    { state | textbox3 = newValue }

                _ ->
                    state

        Textboxchange4 string ->
            case stringToMaybeInt string of
                Just newValue ->
                    { state | textbox4 = newValue }

                _ ->
                    state

        Textboxchange5 string ->
            case stringToMaybeInt string of
                Just newValue ->
                    { state | textbox5 = newValue }

                _ ->
                    state

        ButtonPressed ->
            { state
                | functionx =
                    let
                        ( f, seed1 ) =
                            seedGenerator (initialSeed state.textbox1) 4
                    in
                        f
                , functiony =
                    let
                        ( f, seed2 ) =
                            seedGenerator (initialSeed (state.textbox2 // 2)) 4
                    in
                        f
                , functionz =
                    let
                        ( f, seed3 ) =
                            seedGenerator (initialSeed state.textbox3) 4
                    in
                        f
                , functiona =
                    let
                        ( f, seed4 ) =
                            seedGenerator (initialSeed state.textbox4) 4
                    in
                        f
                , functionb =
                    let
                        ( f, seed4 ) =
                            seedGenerator (initialSeed state.textbox5) 4
                    in
                        f
            }


view : AppState -> Html.Html AppEvent
view state =
    Html.div []
        [ Html.p [] [ Html.text "Text Visualizer" ]
        , Svg.svg [ SvgAttrs.width "600", SvgAttrs.height "600" ]
            ((List.map
                (\pt ->
                    Svg.rect
                        [ SvgAttrs.fill "black"
                        , SvgAttrs.width "700"
                        , SvgAttrs.height "700"
                        , SvgAttrs.x (toString pt.x)
                        , SvgAttrs.y (toString pt.y)
                        , SvgAttrs.fillOpacity "0.2"
                        ]
                        [ Svg.feColorMatrix
                            [ SvgAttrs.type_ "matrix"
                            , SvgAttrs.values (toString (range -1 0.01 1))
                            , SvgAttrs.in_ "SourceGraphic"
                            , SvgAttrs.x (toString pt.x)
                            , SvgAttrs.y (toString pt.y)
                            , SvgAttrs.filter "url(#f)"
                            , SvgAttrs.fill "yellow"
                            ]
                            []
                        ]
                )
                (List.map (\x -> { x = scaleTo600 (eval state.functionb (x * 2 / 3)), y = scaleTo600 (eval state.functionb x) }) (range -1 0.01 1))
             )
                ++ (List.map
                        (\pt ->
                            Svg.rect
                                [ SvgAttrs.fill "white"
                                , SvgAttrs.width "700"
                                , SvgAttrs.height "700"
                                , SvgAttrs.x (toString pt.x)
                                , SvgAttrs.y (toString pt.y)
                                , SvgAttrs.fillOpacity "0.2"
                                ]
                                [ Svg.feColorMatrix
                                    [ SvgAttrs.type_ "matrix"
                                    , SvgAttrs.values (toString (range -1 0.01 1))
                                    , SvgAttrs.in_ "SourceGraphic"
                                    , SvgAttrs.x (toString pt.x)
                                    , SvgAttrs.y (toString pt.y)
                                    , SvgAttrs.filter "url(#f)"
                                    , SvgAttrs.fill "yellow"
                                    ]
                                    []
                                ]
                        )
                        (List.map (\x -> { x = scaleTo600 (eval state.functiona x), y = scaleTo600 (eval state.functiona (x ^ 3 / 2)) }) (range -1 0.01 1))
                   )
                ++ (List.map
                        (\pt ->
                            Svg.circle
                                [ SvgAttrs.r "12"
                                , SvgAttrs.fill "red"
                                , SvgAttrs.cx (toString pt.x)
                                , SvgAttrs.cy (toString pt.y)
                                ]
                                []
                        )
                        (List.map (\x -> { x = scaleTo600 (eval state.functionx x), y = scaleTo600 (eval state.functionx (x ^ 2)) }) (range -1 0.01 1))
                   )
                ++ (List.map
                        (\pt ->
                            Svg.ellipse
                                [ SvgAttrs.fill "blue"
                                , SvgAttrs.cx (toString pt.x)
                                , SvgAttrs.cy (toString pt.y)
                                , SvgAttrs.rx "4"
                                , SvgAttrs.ry "20"
                                , SvgAttrs.fillOpacity "0.3"
                                ]
                                []
                        )
                        (List.map (\x -> { x = scaleTo600 (eval state.functiony (x / 3)), y = scaleTo600 (eval state.functiony x) }) (range -1 0.01 1))
                   )
                ++ (List.map
                        (\pt ->
                            Svg.rect
                                [ SvgAttrs.fill "green"
                                , SvgAttrs.width "40"
                                , SvgAttrs.height "8"
                                , SvgAttrs.x (toString pt.x)
                                , SvgAttrs.y (toString pt.y)
                                , SvgAttrs.fillOpacity "0.9"
                                ]
                                []
                        )
                        (List.map (\x -> { x = scaleTo600 (eval state.functionz x), y = scaleTo600 (eval state.functionz (x ^ 2 / 3)) }) (range -1 0.01 1))
                   )
            )
        , Html.div []
            [ Html.input [ Events.onInput (\s -> Textboxchange1 s) ]
                []
            , Html.input [ Events.onInput (\s -> Textboxchange2 s) ]
                []
            , Html.input [ Events.onInput (\s -> Textboxchange3 s) ]
                []
            , Html.input [ Events.onInput (\s -> Textboxchange4 s) ]
                []
            , Html.input [ Events.onInput (\s -> Textboxchange5 s) ]
                []
            , Html.br [] []
            ]
        , Html.div [ basicContainer ]
            [ Html.button [ Events.onClick ButtonPressed ] [ Html.text "DRAW" ]
            ]
        ]


main =
    Html.beginnerProgram
        { model = initialState
        , view = view
        , update = update
        }
