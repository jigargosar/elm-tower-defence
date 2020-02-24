module Main exposing (main)

import Playground exposing (..)


type alias Pt =
    { x : Number, y : Number }



-- Game Scaffold


type alias Mem =
    { pos : Pt
    , speed : Number
    , deg : Number
    }


init : Mem
init =
    { pos = Pt -100 -100
    , speed = 1
    , deg = 45
    }


update : Computer -> Mem -> Mem
update computer mem =
    let
        ( dx, dy ) =
            ( mem.speed, degrees mem.deg )
                |> fromPolar

        newPos =
            Pt (mem.pos.x + dx) (mem.pos.y + dy)
    in
    { mem | pos = newPos }


view : Computer -> Mem -> List Shape
view computer mem =
    [ words black "Welcome to Adventure"
        |> moveX (computer.screen.top + 100)
    , circle blue 20
        |> move mem.pos.x mem.pos.y
    ]


main =
    game view update init
