module SLL.InitGameStats exposing (..)

import Config exposing (boxSize, diffLeft, diffTop)
import SVG.Arrows exposing (Arrow(..))
import SVG.SLL.SLLNodes exposing (AddStates(..), AnimationState(..), GameStats, GetStates(..), Node(..), RemoveStates(..))


noExtraNode : GameStats
noExtraNode =
    { nodeList =
        Node
            { content = "a"
            , position = { xPos = floor (diffLeft + 2 * boxSize.width), yPos = floor diffTop }
            , highlighted = True
            , arrow = Straight 35 45 110 45
            , next =
                Node
                    { content = "b"
                    , position = { xPos = floor (diffLeft + 5 * boxSize.width), yPos = floor diffTop }
                    , highlighted = False
                    , arrow = Straight 185 45 260 45
                    , next =
                        Node
                            { content = "c"
                            , position = { xPos = floor (diffLeft + 8 * boxSize.width), yPos = floor diffTop }
                            , highlighted = False
                            , arrow = Straight 335 45 410 45
                            , next =
                                Node
                                    { content = "d"
                                    , position = { xPos = floor (diffLeft + 11 * boxSize.width), yPos = floor diffTop }
                                    , highlighted = False
                                    , arrow = Straight 485 45 560 45
                                    , next = Empty
                                    }
                            }
                    }
            }
    , extraNode = Nothing
    , animation = { state = SVG.SLL.SLLNodes.REMOVE CreateRemovedNode, index = 0 }
    }


withExtraNode : GameStats
withExtraNode =
    { nodeList =
        Node
            { content = "a"
            , position = { xPos = floor (diffLeft + 2 * boxSize.width), yPos = floor diffTop }
            , highlighted = False
            , arrow = Straight 35 45 110 45
            , next =
                Node
                    { content = "b"
                    , position = { xPos = floor (diffLeft + 5 * boxSize.width), yPos = floor diffTop }
                    , highlighted = True
                    , arrow = Straight 185 45 260 45
                    , next =
                        Node
                            { content = "c"
                            , position = { xPos = floor (diffLeft + 8 * boxSize.width), yPos = floor diffTop }
                            , highlighted = False
                            , arrow = Straight 335 45 410 45
                            , next = Empty
                            }
                    }
            }
    , extraNode = Just (Node { content = "d", position = { xPos = floor diffLeft, yPos = floor (2 * boxSize.height) }, highlighted = False, arrow = NO, next = Empty })
    , animation = { state = SVG.SLL.SLLNodes.ADD MoveExtra, index = 0 }
    }
