module SVG.SLL.Animations exposing (..)

import Config exposing (..)
import SVG.Arrows exposing (Arrow(..))
import SVG.SLL.SLLNodes exposing (..)


addAnimation : GameStats -> Int -> GameStats
addAnimation gs index =
    let
        getIndex =
            if index > getNodeLength gs.nodeList then
                getNodeLength gs.nodeList

            else
                index
    in
    case gs.animation.state of
        REMOVE _ ->
            gs

        GET _ ->
            gs

        ADD MoveExtra ->
            moveExtraNodeFirst gs (getNodeAtIndex gs.nodeList (getIndex - 1))

        ADD SwapArrow ->
            { gs | nodeList = changeArrowFirst gs.nodeList, animation = { state = ADD AddToNodeList, index = getIndex } }

        ADD AddToNodeList ->
            case gs.extraNode of
                Nothing ->
                    { gs | animation = { state = ADD MoveNodesAfterIndex, index = getIndex } }

                Just extraNode ->
                    { gs | nodeList = addNodeAtIndex gs.nodeList extraNode getIndex, extraNode = Nothing, animation = { state = ADD MoveNodesAfterIndex, index = getIndex } }

        ADD MoveNodesAfterIndex ->
            moveAfterIndex gs (getIndex + 1)

        ADD MoveNewNode ->
            if index >= getNodeLength gs.nodeList then
                moveNewToPos gs (getIndex - 1)

            else
                moveNewToPos gs getIndex


changeArrowFirst : Node -> Node
changeArrowFirst node =
    case node of
        Node { content, position, highlighted, next } ->
            Node
                { content = content
                , position = position
                , highlighted = highlighted
                , arrow =
                    Straight
                        (floor (boxSize.width / 2 + diffLeft))
                        (floor (boxSize.height / 2 + diffTop))
                        position.xPos
                        (position.yPos + floor (boxSize.height / 2))
                , next = changeArrowFollowing next ( position.xPos + floor (boxSize.width * 1.5), position.yPos + floor (boxSize.height * 0.5) )
                }

        Empty ->
            Empty


changeArrowFollowing : Node -> ( Int, Int ) -> Node
changeArrowFollowing node ( x, y ) =
    case node of
        Node { content, position, highlighted, next } ->
            Node
                { content = content
                , position = position
                , highlighted = highlighted
                , arrow = Straight x y position.xPos (position.yPos + floor (boxSize.height / 2))
                , next = changeArrowFollowing next ( position.xPos + floor (boxSize.width * 1.5), position.yPos + floor (boxSize.height * 0.5) )
                }

        Empty ->
            Empty


moveExtraNodeFirst : GameStats -> Node -> GameStats
moveExtraNodeFirst gs indexedNode =
    case gs.extraNode of
        Nothing ->
            gs

        Just node ->
            case node of
                Empty ->
                    gs

                Node { content, position, arrow, highlighted } ->
                    if position.xPos == (getPosition indexedNode).xPos + floor (boxSize.width * 1.5) then
                        { gs | extraNode = Just node, animation = { state = ADD SwapArrow, index = gs.animation.index } }

                    else
                        { gs
                            | extraNode =
                                Just
                                    (Node
                                        { content = content
                                        , highlighted = highlighted
                                        , position = { position | xPos = position.xPos + 1 }
                                        , arrow = arrow
                                        , next = Empty
                                        }
                                    )
                        }



{- index, welches das erste Element ist, was verschoben werden müsste. Davor muss Platz für ein neues Element sein. -}


checkIfAllMoved : Node -> Int -> Bool
checkIfAllMoved node index =
    let
        getPrevNode =
            getPosition (getNodeAtIndex node (index - 2))

        getNodePos =
            getPosition (getNodeAtIndex node index)
    in
    case node of
        Node _ ->
            if index == 0 || index == 1 then
                -- checkAfterIndex next 0 ( -40, 20 )
                getNodePos.xPos >= floor (diffLeft - boxSize.width + boxSize.width * 6)

            else if index >= getNodeLength node then
                True

            else
                getNodePos.xPos == getPrevNode.xPos + floor (boxSize.width * 6)

        Empty ->
            True



{- Der übergebene Index beschreibt das Erste Element, was verschoben werden muss -}


moveAfterIndex : GameStats -> Int -> GameStats
moveAfterIndex gs index =
    let
        move node i =
            case node of
                Node { content, position, highlighted, arrow, next } ->
                    if i <= 0 then
                        Node
                            { content = content
                            , position = { position | xPos = position.xPos + 1 }
                            , highlighted = highlighted
                            , arrow = arrow
                            , next = move next 0
                            }

                    else
                        Node
                            { content = content
                            , position = position
                            , highlighted = highlighted
                            , arrow = arrow
                            , next = move next (i - 1)
                            }

                Empty ->
                    Empty
    in
    if checkIfAllMoved gs.nodeList index then
        { gs | animation = { state = ADD MoveNewNode, index = index - 1 } }

    else
        { gs | nodeList = changeArrowFirst (move gs.nodeList index) }


moveNewToPos : GameStats -> Int -> GameStats
moveNewToPos gs index =
    let
        prevNodePos =
            getPosition (getNodeAtIndex gs.nodeList (index - 1))

        moveNodePos =
            getPosition (getNodeAtIndex gs.nodeList index)

        moveY =
            if prevNodePos.yPos < moveNodePos.yPos then
                -1

            else
                0

        moveX =
            if (prevNodePos.xPos + floor (boxSize.width * 3)) > moveNodePos.xPos then
                1

            else
                0

        move node i ( x, y ) =
            if i == 0 then
                moveNodeByXY node ( x, y )

            else
                case node of
                    Node { content, position, highlighted, arrow, next } ->
                        Node
                            { content = content
                            , position = position
                            , highlighted = highlighted
                            , arrow = arrow
                            , next = move next (i - 1) ( x, y )
                            }

                    Empty ->
                        Empty
    in
    if moveY == 0 && moveX == 0 then
        { gs | animation = { state = ADD MoveExtra, index = index } }

    else
        { gs | nodeList = changeArrowFirst (move gs.nodeList index ( moveX, moveY )) }
