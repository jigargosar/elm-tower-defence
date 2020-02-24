module Main exposing (main)

import Playground exposing (..)


type alias Pt =
    { x : Number, y : Number }



-- Game Scaffold


type alias Mem =
    { pos : Pt
    , speed : Number
    , deg : Number
    , st : Pt
    , end : Pt
    , curr : Pt
    }


init : Mem
init =
    let
        st =
            Pt -100 -100
    in
    { pos = Pt -100 -100
    , speed = 1
    , deg = 45
    , st = st
    , end = Pt 100 100
    , curr = st
    }


update : Computer -> Mem -> Mem
update computer mem =
    { mem
        | pos = nextPos mem
        , curr = nextCurr mem
    }


nextCurr : Mem -> Pt
nextCurr ({ curr, end } as mem) =
    let
        dx =
            end.x - curr.x

        dy =
            end.y - curr.y

        ( x, y ) =
            toPolar ( dx, dy )
                |> Tuple.mapFirst (add mem.speed)
                |> fromPolar
    in
    Pt x y


add =
    (+)


nextPos : Mem -> Pt
nextPos mem =
    let
        ( dx, dy ) =
            ( mem.speed, degrees mem.deg )
                |> fromPolar
    in
    Pt (mem.pos.x + dx) (mem.pos.y + dy)


view : Computer -> Mem -> List Shape
view computer mem =
    [ words black "Welcome to Adventure"
        |> moveX (computer.screen.top + 100)
    , circle blue 20
        |> move mem.pos.x mem.pos.y
    ]


main =
    game view update init
