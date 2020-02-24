module Main exposing (main)

import Playground exposing (..)



-- Point Movement


type PtMov
    = PtMov Pt Number Pt


initPtMov : Pt -> Pt -> Number -> PtMov
initPtMov st end speed =
    PtMov end speed st


stepPtMov : PtMov -> ( Bool, PtMov )
stepPtMov ((PtMov e speed c) as m) =
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
            ( True, PtMov e speed e )

        else
            ( False, PtMov e speed nc )


ptMovToCurr : PtMov -> Pt
ptMovToCurr (PtMov _ _ c) =
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
    { speed : Number
    , st : Pt
    , end : Pt
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
    { speed = speed
    , st = st
    , end = end
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
        | ptMov = nextPtMov
    }


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
    ]


main =
    game view update init
