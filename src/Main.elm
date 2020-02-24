module Main exposing (main)

import Playground exposing (..)


type alias Pt =
    { x : Number, y : Number }



-- Game Scaffold


type alias Mem =
    { start : Pt
    , end : Pt
    , ticks : Number
    }


init : Mem
init =
    { start = Pt -100 -100
    , end = Pt 100 100
    , ticks = 0
    }


update : Computer -> Mem -> Mem
update computer mem =
    mem


view : Computer -> Mem -> List Shape
view computer mem =
    [ words black "Welcome to Adventure" ]


main =
    game view update init
