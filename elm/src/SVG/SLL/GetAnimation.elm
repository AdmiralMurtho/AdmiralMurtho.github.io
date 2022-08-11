module SVG.SLL.GetAnimation exposing (..)

import Config exposing (..)
import SVG.Arrows exposing (Arrow(..))
import SVG.SLL.SLLNodes exposing (..)


getAnimation : GameStats -> Int -> GameStats
getAnimation gs index =
    let
        getIndex =
            if index > (getNodeLength gs.nodeList - 1) then
                getNodeLength gs.nodeList - 1

            else
                index
    in
    case gs.animation.state of
        REMOVE _ ->
            gs

        ADD _ ->
            gs

        GET CreateGetNode ->
            createExtraNodeFromIndex gs getIndex

        GET MoveNode ->
            moveNewNode gs (GET MoveNode)


createExtraNodeFromIndex : GameStats -> Int -> GameStats
createExtraNodeFromIndex gs index =
    { gs | extraNode = Just (getNodeAtIndex gs.nodeList index), animation = { state = GET MoveNode, index = index } }


moveNewNode : GameStats -> AnimationState -> GameStats
moveNewNode gs animationState =
    case gs.extraNode of
        Nothing ->
            gs

        Just node ->
            case node of
                Node { content, position, highlighted, arrow } ->
                    if position.yPos < floor (boxSize.height * 2 + diffTop) then
                        { gs
                            | extraNode =
                                Just
                                    (Node
                                        { content = content
                                        , highlighted = False
                                        , position = { position | yPos = position.yPos + 1 }
                                        , arrow = arrow
                                        , next = Empty
                                        }
                                    )
                        }

                    else if position.xPos > floor -(boxSize.width * 2 + 2) then
                        { gs
                            | extraNode =
                                Just
                                    (Node
                                        { content = content
                                        , highlighted = highlighted
                                        , position = { position | xPos = position.xPos - 1 }
                                        , arrow = arrow
                                        , next = Empty
                                        }
                                    )
                        }

                    else
                        { gs | animation = { state = animationState, index = gs.animation.index } }

                Empty ->
                    gs
