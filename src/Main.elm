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



--noinspection ElmUnusedSymbol


update : Computer -> Mem -> Mem
update computer mem =
    { mem
        | pos = nextPos mem
        , curr = nextCurr mem
    }


angleFromToPt : Pt -> Pt -> Number
angleFromToPt p1 p2 =
    atan2 (p2.y - p1.y) (p2.x - p1.x)


nextCurr : Mem -> Pt
nextCurr ({ curr, st, end } as mem) =
    let
        ( dx, dy ) =
            ( mem.speed, angleFromToPt curr end )
                |> fromPolar
    in
    Pt (curr.x + dx) (curr.y + dy)


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
        |> fade 0.5
    , circle red 30
        |> move mem.curr.x mem.curr.y
        |> fade 0.5
    ]


main =
    game view update init
