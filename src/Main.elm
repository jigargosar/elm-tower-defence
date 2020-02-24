module Main exposing (main)

import Playground exposing (..)



-- Point Movement


type PtMov
    = PtMov Pt Pt Number Pt


initPtMov : Pt -> Pt -> Number -> PtMov
initPtMov st end speed =
    PtMov st end speed st


stepPtMov : PtMov -> ( Bool, PtMov )
stepPtMov ((PtMov s e speed c) as m) =
    if c == e then
        ( True, m )

    else
        let
            ( dx, dy ) =
                ( speed, angleFromToPt c e )
                    |> fromPolar

            nc =
                Pt (c.x + dx) (c.y + dy)
        in
        if ptEqw speed nc e then
            ( True, PtMov s e speed e )

        else
            ( False, PtMov s e speed nc )


ptMovToCurr : PtMov -> Pt
ptMovToCurr (PtMov _ _ _ c) =
    c


eqw tol a b =
    abs a - abs b <= tol



-- Point


type alias Pt =
    { x : Number, y : Number }


angleFromToPt : Pt -> Pt -> Number
angleFromToPt p1 p2 =
    atan2 (p2.y - p1.y) (p2.x - p1.x)


lenFromToPt : Pt -> Pt -> Number
lenFromToPt p1 p2 =
    ((p2.y - p1.y) ^ 2)
        + ((p2.x - p1.x) ^ 2)
        |> sqrt


ptEqw : Number -> Pt -> Pt -> Bool
ptEqw tol p1 p2 =
    abs (lenFromToPt p1 p2) <= tol



-- Game Scaffold


type alias Mem =
    { pos : Pt
    , speed : Number
    , deg : Number
    , st : Pt
    , end : Pt
    , curr : Pt
    , ptMov : PtMov
    }


init : Mem
init =
    let
        st =
            Pt -100 -100

        end =
            Pt 100 100

        speed =
            10
    in
    { pos = Pt -100 -100
    , speed = speed
    , deg = 45
    , st = st
    , end = end
    , curr = st
    , ptMov = initPtMov st end speed
    }



--noinspection ElmUnusedSymbol


update : Computer -> Mem -> Mem
update computer mem =
    let
        ( _, nextPtMov ) =
            stepPtMov mem.ptMov
    in
    { mem
        | pos = nextPos mem
        , curr = nextCurr mem
        , ptMov = nextPtMov
    }


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
    , rectangle black 10 10
        |> move mem.st.x mem.st.y
        |> fade 0.8
    , rectangle black 10 10
        |> move mem.end.x mem.end.y
        |> fade 0.8
    , circle green 40
        |> (let
                pt =
                    ptMovToCurr mem.ptMov
            in
            move pt.x pt.y
           )
        |> fade 0.5
    , circle red 30
        |> move mem.curr.x mem.curr.y
        |> fade 0.5
    , circle blue 20
        |> move mem.pos.x mem.pos.y
        |> fade 0.5
    ]


main =
    game view update init
