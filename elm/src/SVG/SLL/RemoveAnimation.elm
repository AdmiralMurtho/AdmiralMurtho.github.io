module SVG.SLL.RemoveAnimation exposing (..)

import Config exposing (..)
import SVG.Arrows exposing (Arrow(..))
import SVG.SLL.Animations exposing (changeArrowFirst)
import SVG.SLL.GetAnimation exposing (moveNewNode)
import SVG.SLL.SLLNodes exposing (..)


removeAnimation : GameStats -> Int -> GameStats
removeAnimation gs index =
    let
        getIndex =
            if index > getNodeLength gs.nodeList then
                getNodeLength gs.nodeList

            else
                index
    in
    case gs.animation.state of
        ADD _ ->
            gs

        GET _ ->
            gs

        REMOVE CreateRemovedNode ->
            createRemovedNode gs getIndex

        REMOVE RemoveNodeFromList ->
            { gs | nodeList = changeArrowFirst (removeNodeFromList gs.nodeList getIndex), animation = { state = REMOVE MoveRemovedNode, index = getIndex } }

        REMOVE MoveRemovedNode ->
            moveNewNode gs (REMOVE MoveLeftOverNodes)

        REMOVE MoveLeftOverNodes ->
            moveAfterIndex gs getIndex


createRemovedNode : GameStats -> Int -> GameStats
createRemovedNode gs index =
    let
        createExtraNode node =
            case node of
                Node { content, position } ->
                    Node { content = content, highlighted = False, position = position, arrow = NO, next = Empty }

                Empty ->
                    Empty
    in
    { gs | extraNode = Just (createExtraNode (getNodeAtIndex gs.nodeList index)), animation = { state = REMOVE RemoveNodeFromList, index = index } }


removeNodeFromList : Node -> Int -> Node
removeNodeFromList node index =
    if index == 0 then
        case node of
            Node { next } ->
                next

            Empty ->
                Empty

    else
        case node of
            Node { content, highlighted, arrow, next, position } ->
                Node
                    { content = content
                    , highlighted = highlighted
                    , position = position
                    , arrow = arrow
                    , next = removeNodeFromList next (index - 1)
                    }

            Empty ->
                Empty


checkIfAllMoved : Node -> Int -> Bool
checkIfAllMoved node index =
    let
        getPrevNode =
            getPosition (getNodeAtIndex node (index - 1))

        getNodePos =
            getPosition (getNodeAtIndex node index)
    in
    case node of
        Node _ ->
            if index == 0 then
                getNodePos.xPos == floor (diffLeft + boxSize.width * 2)

            else
                getNodePos.xPos == getPrevNode.xPos + floor (boxSize.width * 3)

        Empty ->
            True


moveAfterIndex : GameStats -> Int -> GameStats
moveAfterIndex gs index =
    let
        move node i =
            case node of
                Node { content, position, highlighted, arrow, next } ->
                    if i <= 0 then
                        Node
                            { content = content
                            , position = { position | xPos = position.xPos - 1 }
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
        { gs | animation = { state = REMOVE MoveLeftOverNodes, index = index } }

    else
        { gs | nodeList = changeArrowFirst (move gs.nodeList index) }
