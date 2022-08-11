module SVG.Arrows exposing (..)

import Svg exposing (Svg, line, path, polyline)
import Svg.Attributes exposing (d, fill, markerHeight, markerWidth, orient, points, rotate, stroke, strokeLinejoin, strokeWidth, transform, x1, x2, y1, y2)


type Arrow
    = Straight Int Int Int Int
    | NotStraight Int Int Int Int
    | NO


straightLine : Int -> Int -> Int -> Int -> List (Svg msg)
straightLine startX startY endX endY =
    [ line [ x1 (String.fromInt startX), x2 (String.fromInt endX), y1 (String.fromInt startY), y2 (String.fromInt endY), stroke "black", strokeWidth "1.5" ] []
    ]



{- Erst für DLL relevant -}
-- curvedArrow : Int -> Int -> Int -> Int -> List (Svg msg)
-- curvedArrow startX startY endX endY =
--     [ path [ d (positionsToCurvedPathString startX startY endX endY), fill "none", stroke "#000", strokeWidth "1.5" ] []
--     , Svg.polyline [ markerWidth "10", markerHeight "8", orient "-45", points "1 1, 9 5, 1 7", fill "none", strokeWidth "1.5", transform ("translate(" ++ String.fromInt (56 + startX) ++ "," ++ String.fromInt (-2 + startY) ++ "), rotate(-35)"), stroke "#000", strokeLinejoin "round" ] []
--     ]
-- positionsToCurvedPathString : Int -> Int -> Int -> Int -> String
-- positionsToCurvedPathString startX startY endX endY =
--     "M" ++ String.fromInt startX ++ " " ++ String.fromInt startY ++ " C " ++ String.fromInt ((endX - startX) // 4 + startX) ++ " " ++ String.fromInt ((endX - startX) // 6 + startY) ++ ", " ++ String.fromInt ((endX - startX) // 4 * 3 + startX) ++ " " ++ String.fromInt ((endX - startX) // 6 + startY) ++ ", " ++ String.fromInt endX ++ " " ++ String.fromInt endY ++ ""


arrowLRUp : Int -> Int -> List (Svg msg)
arrowLRUp startX startY =
    [ path [ d "M0 0 C 15 10, 45 10, 60 0", fill "none", stroke "#000", strokeWidth "1.5", transform ("translate(" ++ String.fromInt startX ++ "," ++ String.fromInt startY ++ ")") ] []
    , Svg.polyline [ markerWidth "10", markerHeight "8", orient "-45", points "1 1, 4 9, 7 1", fill "none", strokeWidth "1.5", transform ("translate(" ++ String.fromInt (56 + startX) ++ "," ++ String.fromInt (-2 + startY) ++ "), rotate(-35)"), stroke "#000", strokeLinejoin "round" ] []
    ]


arrowLRDown : Int -> Int -> List (Svg msg)
arrowLRDown startX startY =
    [ path [ d "M0 0 C 15 -10, 45 -10, 60 0", fill "none", stroke "#000", strokeWidth "1.5", transform ("translate(" ++ String.fromInt startX ++ "," ++ String.fromInt startY ++ ")") ] []
    , polyline [ markerWidth "10", markerHeight "8", orient "-45", points "1 1,4 9,7 1", fill "none", strokeWidth "1.5", transform ("translate(" ++ String.fromInt (60 + startX) ++ "," ++ String.fromInt (-5 + startY) ++ "), rotate(35)"), stroke "#000", strokeLinejoin "round" ] []
    ]



{- Maybe Int wird übergeben, wenn der Pfeil eine bestimmte Länge benötigt. Alternativ hat der die Länge
   um zur nächsten Box zu zeigen
-}


arrowLR : Int -> Float -> Maybe Int -> List (Svg msg)
arrowLR startX yPos length =
    case length of
        Just a ->
            [ line [ x1 (String.fromInt startX), x2 (String.fromInt (startX + a)), y1 (String.fromFloat yPos), y2 (String.fromFloat yPos), stroke "black", strokeWidth "1.5" ] []
            , polyline [ markerWidth "10", markerHeight "8", orient "-45", points "1 1, 4 9, 7 1", fill "none", strokeWidth "1.5", transform ("translate(" ++ String.fromInt (a - 2 + startX) ++ "," ++ String.fromFloat (-4.5 + yPos) ++ ")"), stroke "#000", strokeLinejoin "round" ] []
            ]

        Nothing ->
            [ line [ x1 (String.fromInt startX), x2 (String.fromInt (startX + 67)), y1 (String.fromFloat yPos), y2 (String.fromFloat yPos), stroke "black", strokeWidth "1.5" ] []
            , polyline [ markerWidth "10", markerHeight "8", orient "-45", points "1 1, 4 9, 7", fill "none", strokeWidth "1.5", transform ("translate(" ++ String.fromInt (65 + startX) ++ "," ++ String.fromFloat (-4.5 + yPos) ++ ")"), stroke "#000", strokeLinejoin "round" ] []
            ]


tryArrow : Int -> Int -> List (Svg msg)
tryArrow startX startY =
    [ path [ d "M30 0 C 10 5, 10 25, 30 30 L 40 30 C 60 35, 60 55, 40 60", fill "none", stroke "#000", strokeWidth "1.5", transform ("translate(" ++ String.fromInt startX ++ "," ++ String.fromInt startY ++ ")") ] []
    , Svg.polyline [ markerWidth "10", markerHeight "8", orient "-45", points "1 1, 9 4, 1 7", fill "none", strokeWidth "1.5", transform ("translate(" ++ String.fromInt (56 + startX) ++ "," ++ String.fromInt (-2 + startY) ++ "), rotate(-35)"), stroke "#000", strokeLinejoin "round" ] []
    ]



{-
   M30 0 C 10 5, 10 25, 30 30 L 40 30 C 60 35, 60 55, 40 60
-}
