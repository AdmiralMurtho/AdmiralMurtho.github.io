module SVG.SLL.SLLNodes exposing (..)

import Config exposing (boxSize, diffLeft, diffTop)
import SVG.Arrows exposing (..)
import Svg exposing (Svg, line, rect, text, text_)
import Svg.Attributes exposing (dominantBaseline, fill, height, stroke, strokeWidth, textAnchor, width, x, x1, x2, y, y1, y2)


type Node
    = Node
        { content : String
        , position : Position
        , highlighted : Bool
        , next : Node
        , arrow : Arrow
        }
    | Empty


type alias Position =
    { xPos : Int, yPos : Int }


type alias GameStats =
    { nodeList : Node
    , extraNode : Maybe Node
    , animation : Animation
    }


type alias Animation =
    { state : AnimationState, index : Int }


type AnimationState
    = GET GetStates
    | ADD AddStates
    | REMOVE RemoveStates


type AddStates
    = MoveExtra
    | SwapArrow
    | AddToNodeList
    | MoveNodesAfterIndex
    | MoveNewNode


type GetStates
    = CreateGetNode
    | MoveNode


type RemoveStates
    = CreateRemovedNode
    | RemoveNodeFromList
    | MoveRemovedNode
    | MoveLeftOverNodes



{- Die Methode printet die Nodes alle -}


printAllNodes : Node -> List (Svg msg)
printAllNodes nodes =
    let
        printAllNodesLocal node list =
            case node of
                Empty ->
                    list

                Node { next } ->
                    case next of
                        Empty ->
                            list ++ lastElementToSvg node

                        _ ->
                            printAllNodesLocal next (list ++ nodeToSvg node)
    in
    case nodes of
        Empty ->
            []

        _ ->
            startToSvg ++ printAllNodesLocal nodes []


startToSvg : List (Svg msg)
startToSvg =
    [ rect [ x "10", y "20", width "50", height "50", stroke "black", strokeWidth "1", fill "gray" ] []
    , text_ [ x "35", y "10", fill "black", textAnchor "middle", dominantBaseline "central" ] [ Svg.text "first" ]
    ]


extraNodeToSvg : Node -> List (Svg msg)
extraNodeToSvg node =
    case node of
        Empty ->
            []

        Node { content, position, highlighted } ->
            case position of
                { xPos, yPos } ->
                    [ rect
                        [ x (String.fromInt xPos)
                        , y (String.fromInt yPos)
                        , width (String.fromFloat boxSize.width)
                        , height (String.fromFloat boxSize.height)
                        , stroke "black"
                        , strokeWidth "1"
                        , fill "gray"
                        , if highlighted then
                            stroke "red"

                          else
                            stroke "black"
                        ]
                        []
                    , Svg.text_ [ x (String.fromInt (xPos + 25)), y (String.fromInt (yPos + 25)), fill "black", textAnchor "middle", dominantBaseline "central" ] [ text content ]
                    , rect
                        [ x (String.fromInt (xPos + 50))
                        , y (String.fromInt yPos)
                        , width (String.fromFloat boxSize.width)
                        , height (String.fromFloat boxSize.height)
                        , stroke "black"
                        , strokeWidth "1"
                        , fill "gray"
                        , if highlighted then
                            stroke "red"

                          else
                            stroke "black"
                        ]
                        []
                    ]



{- gibt eine Liste von Svg msg zurück, welche die Node selbst sowei den Pfeil von der vorhergehenden Node enthält -}


nodeToSvg : Node -> List (Svg msg)
nodeToSvg node =
    case node of
        Empty ->
            []

        Node { content, position, highlighted, arrow } ->
            case position of
                { xPos, yPos } ->
                    [ rect
                        [ x (String.fromInt xPos)
                        , y (String.fromInt yPos)
                        , width (String.fromFloat boxSize.width)
                        , height (String.fromFloat boxSize.height)
                        , stroke "black"
                        , strokeWidth "1"
                        , fill "gray"
                        , if highlighted then
                            stroke "red"

                          else
                            stroke "black"
                        ]
                        []
                    , Svg.text_ [ x (String.fromInt (xPos + 25)), y (String.fromInt (yPos + 25)), fill "black", textAnchor "middle", dominantBaseline "central" ] [ text content ]
                    , rect
                        [ x (String.fromInt (xPos + 50))
                        , y (String.fromInt yPos)
                        , width (String.fromFloat boxSize.width)
                        , height (String.fromFloat boxSize.height)
                        , stroke "black"
                        , strokeWidth "1"
                        , fill "gray"
                        , if highlighted then
                            stroke "red"

                          else
                            stroke "black"
                        ]
                        []
                    ]
                        -- ++ arrowLR (xPos - 75) 45 Nothing
                        ++ (case arrow of
                                Straight startX startY endX endY ->
                                    straightLine startX startY endX endY

                                _ ->
                                    []
                           )


lastElementToSvg : Node -> List (Svg msg)
lastElementToSvg node =
    case node of
        Empty ->
            []

        Node { content, position, highlighted, arrow } ->
            case position of
                { xPos, yPos } ->
                    case arrow of
                        Straight startX startY endX endY ->
                            [ rect
                                [ x (String.fromInt xPos)
                                , y (String.fromInt yPos)
                                , width (String.fromFloat boxSize.width)
                                , height (String.fromFloat boxSize.height)
                                , stroke "black"
                                , strokeWidth "1"
                                , fill "gray"
                                , if highlighted then
                                    stroke "red"

                                  else
                                    stroke "black"
                                ]
                                []
                            , Svg.text_ [ x (String.fromInt (xPos + floor (boxSize.width / 2))), y (String.fromInt (yPos + floor (boxSize.width / 2))), fill "black", textAnchor "middle", dominantBaseline "central" ] [ text content ]
                            , rect
                                [ x (String.fromInt (xPos + floor boxSize.width))
                                , y (String.fromInt yPos)
                                , width (String.fromFloat boxSize.width)
                                , height (String.fromFloat boxSize.height)
                                , stroke "black"
                                , strokeWidth "1"
                                , fill "gray"
                                , if highlighted then
                                    stroke "red"

                                  else
                                    stroke "black"
                                ]
                                []
                            , line [ x1 (String.fromInt (xPos + floor boxSize.width)), y1 (String.fromInt (yPos + floor boxSize.height)), x2 (String.fromInt (xPos + floor (boxSize.height * 2))), y2 (String.fromInt yPos), stroke "black", strokeWidth "1" ] []
                            ]
                                ++ straightLine startX startY endX endY

                        _ ->
                            []


moveExtraNode : Maybe Node -> Node -> Maybe Node
moveExtraNode extraNode indexedNode =
    case extraNode of
        Nothing ->
            Nothing

        Just node ->
            case node of
                Empty ->
                    Nothing

                Node { content, position, arrow } ->
                    if position.xPos == (getPosition indexedNode).xPos + floor (boxSize.height * 1.5) then
                        Just node

                    else
                        Just (Node { content = content, highlighted = False, position = { position | xPos = position.xPos + 1 }, arrow = arrow, next = Empty })


getPosition : Node -> Position
getPosition node =
    case node of
        Empty ->
            { xPos = 0, yPos = 0 }

        Node { position } ->
            position


markCorrectNode : Node -> Int -> Node
markCorrectNode node xPosExtra =
    case node of
        Empty ->
            Empty

        Node { content, position, next, arrow } ->
            case position.xPos of
                xPos ->
                    if xPos < xPosExtra && (xPos + floor (boxSize.height * 2)) > xPosExtra then
                        Node { content = content, highlighted = True, position = position, arrow = arrow, next = markCorrectNode next xPosExtra }

                    else
                        Node { content = content, highlighted = False, position = position, arrow = arrow, next = markCorrectNode next xPosExtra }


getNodeAtIndex : Node -> Int -> Node
getNodeAtIndex node index =
    case node of
        Empty ->
            Empty

        Node { next } ->
            if index == 1 then
                next

            else if index == 0 then
                node

            else if index < 0 then
                Node
                    { content = "first"
                    , position = { xPos = floor (-boxSize.width + diffLeft), yPos = floor diffTop }
                    , highlighted = False
                    , arrow = NO
                    , next = Empty
                    }

            else if next == Empty then
                node

            else
                getNodeAtIndex next (index - 1)


addNodeAtIndex : Node -> Node -> Int -> Node
addNodeAtIndex node newNode index =
    if index == 0 then
        case newNode of
            Node { content, position, highlighted, arrow } ->
                Node
                    { content = content
                    , position = position
                    , highlighted = highlighted
                    , arrow = arrow
                    , next = node
                    }

            Empty ->
                Empty

    else
        case node of
            Node { content, position, highlighted, arrow, next } ->
                Node
                    { content = content
                    , position = position
                    , highlighted = highlighted
                    , arrow = arrow
                    , next = addNodeAtIndex next newNode (index - 1)
                    }

            Empty ->
                Empty


changeArrow : GameStats -> Int -> GameStats
changeArrow gameStats index =
    let
        indexNode =
            getNodeAtIndex gameStats.nodeList index

        nextNode =
            case indexNode of
                Empty ->
                    Empty

                Node { next } ->
                    next
    in
    { extraNode = changeArrowPos gameStats.extraNode (getPosition indexNode).xPos (getPosition indexNode).yPos, nodeList = gameStats.nodeList, animation = gameStats.animation }


changeArrowPos : Maybe Node -> Int -> Int -> Maybe Node
changeArrowPos oldNode startXbeforeNode startYbeforeNode =
    case oldNode of
        Nothing ->
            Nothing

        Just node ->
            case node of
                Empty ->
                    Nothing

                Node { content, position, arrow } ->
                    case arrow of
                        Straight _ _ endX endY ->
                            Just
                                (Node
                                    { content = content
                                    , highlighted = True
                                    , position = position
                                    , arrow = Straight (startXbeforeNode + floor (boxSize.width * 1.5)) (startYbeforeNode + floor boxSize.width) endX endY
                                    , next = Empty
                                    }
                                )

                        _ ->
                            oldNode


moveNodeByXY : Node -> ( Int, Int ) -> Node
moveNodeByXY node ( x, y ) =
    case node of
        Node { content, position, highlighted, arrow, next } ->
            Node
                { content = content
                , position = { position | xPos = position.xPos + x, yPos = position.yPos + y }
                , highlighted = highlighted
                , arrow = arrow
                , next = next
                }

        Empty ->
            Empty


getNodeLength : Node -> Int
getNodeLength node =
    case node of
        Empty ->
            0

        Node { next } ->
            1 + getNodeLength next
