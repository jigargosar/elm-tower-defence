module Main exposing (main)

import Playground exposing (..)



-- Move Point On Path


type PtMovPath
    = PtMovPath


initPtMovPath : Pt -> List Pt -> Number -> PtMovPath
initPtMovPath st path speed =
    PtMovPath


stepPtMovPath : PtMovPath -> ( Bool, PtMovPath )
stepPtMovPath m =
    ( True, m )



-- Move Point To


type PtMovTo
    = PtMov
        -- End
        Pt
        -- dx
        Number
        -- dy
        Number
        -- Current
        Pt


initPtMov : Pt -> Pt -> Number -> PtMovTo
initPtMov st end speed =
    let
        ( dx, dy ) =
            ( speed, angleFromToPt st end )
                |> fromPolar
    in
    PtMov end dx dy st


stepPtMov : PtMovTo -> ( Bool, PtMovTo )
stepPtMov ((PtMov e dx dy c) as m) =
    if c == e then
        ( True, m )

    else
        let
            nc =
                Pt (c.x + dx) (c.y + dy)
        in
        if ptEqw dx dy nc e then
            ( True, PtMov e dx dy e )

        else
            ( False, PtMov e dx dy nc )


ptMovToCurr : PtMovTo -> Pt
ptMovToCurr (PtMov _ _ _ c) =
    c



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


ptEqw : Number -> Number -> Pt -> Pt -> Bool
ptEqw dx dy p1 p2 =
    eqw dx p1.x p2.x && eqw dy p1.y p2.y


eqw tol a b =
    abs a - abs b <= tol



-- Game Scaffold


type alias Mem =
    { speed : Number
    , st : Pt
    , end : Pt
    , ptMov : PtMovTo
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
