module Config exposing (..)


type alias BoxSize =
    { width : Float, height : Float }


boxSize : BoxSize
boxSize =
    { width = 50, height = 50 }


diffTop : Float
diffTop =
    20


diffLeft : Float
diffLeft =
    10


speed : Float
speed =
    10
