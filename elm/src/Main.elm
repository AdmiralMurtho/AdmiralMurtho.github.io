module Main exposing (..)

import Browser
import Browser.Dom
import Browser.Events
import Config exposing (speed)
import Html exposing (Html, code, div, h1, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Platform exposing (Task)
import SLL.InitGameStats exposing (noExtraNode)
import SVG.Arrows exposing (..)
import SVG.SLL.Animations exposing (addAnimation)
import SVG.SLL.GetAnimation exposing (getAnimation)
import SVG.SLL.RemoveAnimation exposing (removeAnimation)
import SVG.SLL.SLLNodes exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import Time



-- #Types --
{- Node ist die Liste, welche alle in der SLL enthaltenen Nodes darstellt. Folgend von einem (Maybe Node), welche die Node beschreiben, welche sich -}


type Model
    = Running Datastructure Section GameStats Float (Maybe Browser.Dom.Viewport)
    | Paused Datastructure Section GameStats (Maybe Browser.Dom.Viewport)


type Datastructure
    = SLL


type Section
    = GET String
    | ADD String
    | REMOVE String


type Msg
    = GETMsg
    | ADDMsg
    | REMOVEMsg
    | Tick
    | UpdateViewport Browser.Dom.Viewport
    | NewWindowSize Int Int
    | Play
    | Pause
    | Reset
    | UpdateIndex String


init : ( Model, Cmd Msg )
init =
    ( Paused SLL
        (GET "Testcode")
        noExtraNode
        Nothing
    , Task.perform UpdateViewport Browser.Dom.getViewport
    )



-- #Update --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Running dataStruc method data intervall viewP ->
            recalcRunningModel msg dataStruc method data intervall viewP

        Paused dataStruc method data viewP ->
            recalcRunningPaused msg dataStruc method data viewP



-- prozesses Message when model is in Running - State


recalcRunningModel : Msg -> Datastructure -> Section -> GameStats -> Float -> Maybe Browser.Dom.Viewport -> ( Model, Cmd Msg )
recalcRunningModel msg dataStruc method data intervall viewP =
    let
        getnodeList =
            data.nodeList

        getextraNode =
            data.extraNode

        getXPosfromExtra =
            case data.extraNode of
                Just (Node { position }) ->
                    case position of
                        { xPos } ->
                            xPos

                Nothing ->
                    0

                Just Empty ->
                    Debug.todo "branch 'Just Empty' not implemented"

        updatedCollection =
            { nodeList = markCorrectNode getnodeList getXPosfromExtra
            , extraNode =
                moveExtraNode getextraNode
                    (Node
                        { content = "b"
                        , position = { xPos = 260, yPos = 20 }
                        , highlighted = True
                        , arrow = NO
                        , next = Empty
                        }
                    )
            , animation = data.animation
            }

        getAnimationIndex =
            data.animation.index
    in
    case msg of
        GETMsg ->
            ( Running dataStruc (GET "Testcode für GET") data intervall viewP, Cmd.none )

        ADDMsg ->
            ( Running dataStruc (ADD "Testcode für ADD") data intervall viewP, Cmd.none )

        REMOVEMsg ->
            ( Running dataStruc (REMOVE "Testcode für REMOVE") data intervall viewP, Cmd.none )

        Tick ->
            ( Running dataStruc method (removeAnimation data getAnimationIndex) intervall viewP, Cmd.none )

        UpdateViewport newViewP ->
            ( Running dataStruc method data intervall (Just newViewP), Cmd.none )

        NewWindowSize width height ->
            ( Running dataStruc method data intervall (maybeUpdateviewPort viewP (toFloat width) (toFloat height)), Cmd.none )

        Play ->
            ( Running dataStruc method data intervall viewP, Cmd.none )

        Pause ->
            ( Paused dataStruc method data viewP, Cmd.none )

        UpdateIndex index ->
            ( Running dataStruc method data intervall viewP, Cmd.none )

        Reset ->
            init



-- prozesses Message when model is in Paused - State


recalcRunningPaused : Msg -> Datastructure -> Section -> GameStats -> Maybe Browser.Dom.Viewport -> ( Model, Cmd Msg )
recalcRunningPaused msg dataStruc method data viewP =
    case msg of
        GETMsg ->
            ( Paused dataStruc method data viewP, Cmd.none )

        ADDMsg ->
            ( Paused dataStruc method data viewP, Cmd.none )

        REMOVEMsg ->
            ( Paused dataStruc method data viewP, Cmd.none )

        Tick ->
            ( Paused dataStruc method data viewP, Cmd.none )

        UpdateViewport newViewP ->
            ( Paused dataStruc method data (Just newViewP), Cmd.none )

        NewWindowSize width height ->
            ( Paused dataStruc method data (maybeUpdateviewPort viewP (toFloat width) (toFloat height)), Cmd.none )

        Play ->
            ( Running dataStruc method data Config.speed viewP, Cmd.none )

        Pause ->
            ( Paused dataStruc method data viewP, Cmd.none )

        UpdateIndex index ->
            ( Paused dataStruc method (updateIndex data index) viewP, Cmd.none )

        Reset ->
            init


updateIndex : GameStats -> String -> GameStats
updateIndex gs index =
    let
        getIntIndex =
            case String.toInt index of
                Just i ->
                    if i > getNodeLength gs.nodeList then
                        getNodeLength gs.nodeList

                    else
                        i

                Nothing ->
                    0
    in
    { gs | nodeList = highlightNode gs.nodeList getIntIndex, animation = { state = gs.animation.state, index = getIntIndex } }


highlightNode : Node -> Int -> Node
highlightNode node index =
    case node of
        Node { content, position, arrow, next } ->
            if index == 0 then
                Node { content = content, position = position, highlighted = True, arrow = arrow, next = highlightNode next (index - 1) }

            else
                Node { content = content, position = position, highlighted = False, arrow = arrow, next = highlightNode next (index - 1) }

        Empty ->
            Empty



-- methode that updates Browser.Dom.Viewport if it already exits (is initlized when UpdateViewport - msg is received )


maybeUpdateviewPort : Maybe Browser.Dom.Viewport -> Float -> Float -> Maybe Browser.Dom.Viewport
maybeUpdateviewPort maybeViewP width height =
    let
        -- pattern to update nested record. Record is defined Browser.Dom.Viewport - else we would not have used nested records here
        updateViewPort : Browser.Dom.Viewport -> { x : Float, y : Float, width : Float, height : Float } -> Browser.Dom.Viewport
        updateViewPort viewData viewPort =
            { viewData | viewport = viewPort }
    in
    Maybe.andThen
        (\justviewPort ->
            { x = justviewPort.viewport.x
            , y = justviewPort.viewport.y
            , width = width
            , height = height
            }
                |> updateViewPort justviewPort
                |> Just
        )
        maybeViewP



-- #View --


view : Model -> Html Msg
view m =
    let
        nodes =
            case m of
                Running _ _ { nodeList } _ _ ->
                    nodeList

                Paused _ _ { nodeList } _ ->
                    nodeList

        extraNodee =
            case m of
                Running _ _ { extraNode } _ _ ->
                    case extraNode of
                        Just node ->
                            node

                        Nothing ->
                            Empty

                Paused _ _ { extraNode } _ ->
                    case extraNode of
                        Just node ->
                            node

                        Nothing ->
                            Empty

        width =
            case m of
                Running _ _ _ _ vP ->
                    case vP of
                        Just viewPort ->
                            viewPort.viewport.width

                        Nothing ->
                            0

                Paused _ _ _ vP ->
                    case vP of
                        Just viewPort ->
                            viewPort.viewport.width

                        Nothing ->
                            0

        index =
            case m of
                Running _ _ { animation } _ _ ->
                    animation.index

                Paused _ _ { animation } _ ->
                    animation.index
    in
    div []
        [ h1 [] [ Html.text "Single Linked List" ]
        , div [ Html.Attributes.style "display" "grid", Html.Attributes.style "grid-template-columns" "repeat(2, 1fr)", Html.Attributes.style "grid-gap" "10px" ]
            [ div [ Html.Attributes.style "width" (String.fromFloat (width / 3) ++ "px"), Html.Attributes.style "background" "blue", Html.Attributes.style "grid-column" "1/2" ]
                [ h1 [] [ Html.text "GET" ]
                , Html.text "Bei diesem Part wollen wir erklären, wie man ein GET auf eine Single Linked List anwendet"
                , h1 [] [ Html.text "ADD" ]
                , Html.text "Bei diesem Part wollen wir erklären, wie man ein ADD auf eine Single Linked List anwendet"
                , h1 [] [ Html.text "REMOVE" ]
                , Html.text "Bei diesem Part wollen wir erklären, wie man ein REMOVE auf eine Single Linked List anwendet"
                , Html.text (Debug.toString m)
                ]
            , div
                [ if width > 1000 then
                    Html.Attributes.style "grid-column" "2/3"

                  else
                    Html.Attributes.style "grid-column" "1/2"
                ]
                [ svg [ Svg.Attributes.width "800", Svg.Attributes.height "300" ]
                    (areaToSvg :: printAllNodes nodes ++ extraNodeToSvg extraNodee)
                ]
            , div []
                [ Html.button [ onClick Play ] [ Html.text "Play" ]
                , Html.button [ onClick Pause ] [ Html.text "Pause" ]
                , Html.button [ onClick Reset ] [ Html.text "Reset" ]
                , Html.input [ placeholder "index", value (String.fromInt index), onInput UpdateIndex ] []
                ]
            ]
        ]


areaToSvg : Svg msg
areaToSvg =
    rect [ x "0", y "0", Svg.Attributes.width "800", Svg.Attributes.height "300", Svg.Attributes.fill "lightgray" ] []



-- #Subscription --


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Running _ _ _ timeIntervall maybeWindowSize ->
            Sub.batch
                [ Time.every timeIntervall (\_ -> Tick)
                , Browser.Events.onResize (\width height -> NewWindowSize width height)
                ]

        Paused _ _ _ _ ->
            Sub.batch
                [ Browser.Events.onResize (\width height -> NewWindowSize width height)
                ]



-- #Main --


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
