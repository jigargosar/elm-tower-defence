module Main exposing (main)

import Playground exposing (..)
import Random exposing (Seed)



-- Move Point On Path


type PtMovPath
    = PtMovPath PtMov (List Pt) Number


initPtMovPath : Pt -> List Pt -> Number -> PtMovPath
initPtMovPath st path speed =
    case path of
        [] ->
            PtMovPath (initPtMov st st speed) [] speed

        nxt :: rest ->
            let
                ptMov =
                    initPtMov st nxt speed
            in
            PtMovPath ptMov rest speed


stepPtMovPath : PtMovPath -> ( Bool, PtMovPath )
stepPtMovPath (PtMovPath mov path speed) =
    let
        ( done, nextMov ) =
            stepPtMov mov
    in
    if done then
        case path of
            [] ->
                ( True, PtMovPath nextMov [] speed )

            nxt :: rest ->
                let
                    ptMov =
                        initPtMov (ptMovToCurr mov) nxt speed
                in
                ( False, PtMovPath ptMov rest speed )

    else
        ( False, PtMovPath nextMov path speed )


ptMovPathToCurr : PtMovPath -> Pt
ptMovPathToCurr (PtMovPath mov _ _) =
    ptMovToCurr mov



-- Move Point To


type PtMov
    = PtMov
        -- End
        Pt
        -- dx
        Number
        -- dy
        Number
        -- Current
        Pt


initPtMov : Pt -> Pt -> Number -> PtMov
initPtMov st end speed =
    let
        ( dx, dy ) =
            ( speed, angleFromToPt st end )
                |> fromPolar
    in
    PtMov end dx dy st


stepPtMov : PtMov -> ( Bool, PtMov )
stepPtMov ((PtMov e dx dy c) as m) =
    if c == e then
        ( True, m )

    else
        let
            nc =
                Pt (c.x + dx) (c.y + dy)
        in
        if ptEqw dx dy nc e then
            ( False, PtMov e dx dy e )

        else
            ( False, PtMov e dx dy nc )


ptMovToCurr : PtMov -> Pt
ptMovToCurr (PtMov _ _ _ c) =
    c



-- Point


type alias Pt =
    { x : Number, y : Number }


angleFromToPt : Pt -> Pt -> Number
angleFromToPt p1 p2 =
    atan2 (p2.y - p1.y) (p2.x - p1.x)



--noinspection ElmUnusedSymbol


lenFromToPt : Pt -> Pt -> Number
lenFromToPt p1 p2 =
    ((p2.y - p1.y) ^ 2)
        + ((p2.x - p1.x) ^ 2)
        |> sqrt


ptEqw : Number -> Number -> Pt -> Pt -> Bool
ptEqw dx dy p1 p2 =
    eqw dx p1.x p2.x && eqw dy p1.y p2.y


eqw : number -> number -> number -> Bool
eqw tol a b =
    abs (a - b) <= abs (tol + 1)



-- MONSTER


type Monster
    = Monster PtMovPath



-- MEM


type alias Mem =
    { speed : Number
    , st : Pt
    , end : Pt
    , ptMov : PtMov
    , ptMovPath : PtMovPath
    , monsters : List PtMovPath
    , pathStart : Pt
    , path : List Pt
    , seed : Seed
    }


init : Mem
init =
    let
        st =
            Pt -100 -100

        end =
            Pt 100 100

        speed =
            5

        pathStart =
            Pt -300 300

        path =
            [ Pt -300 200, Pt -200 100, Pt 0 100, Pt 0 0, Pt -100 -200, Pt 300 -200 ]
    in
    { speed = speed
    , st = st
    , end = end
    , ptMov = initPtMov st end speed
    , ptMovPath = initPtMovPath pathStart path speed
    , monsters = []
    , pathStart = pathStart
    , path = path
    , seed = Random.initialSeed 0
    }



-- UPDATE MEM
--noinspection ElmUnusedSymbol


update : Computer -> Mem -> Mem
update computer mem =
    let
        ( _, nextPtMov ) =
            stepPtMov mem.ptMov

        ( _, nextPtMovPath ) =
            stepPtMovPath mem.ptMovPath

        newMonsters =
            if wave 0 100 1 computer.time >= 99.9 then
                [ initPtMovPath mem.pathStart mem.path mem.speed ]

            else
                []
    in
    { mem
        | ptMov = nextPtMov
        , ptMovPath = nextPtMovPath
        , monsters = newMonsters ++ updateMonsters mem
    }


updateMonsters : Mem -> List PtMovPath
updateMonsters mem =
    List.map stepPtMovPath mem.monsters
        |> List.filterMap
            (\( remove, monster ) ->
                if remove then
                    Nothing

                else
                    Just monster
            )



-- VIEW MEM


view : Computer -> Mem -> List Shape
view computer mem =
    [ words black "Welcome to Adventure"
        |> moveY computer.screen.top
        |> moveDown 50

    --, rectangle black 10 10
    --    |> move mem.st.x mem.st.y
    --    |> fade 0.8
    --, rectangle black 10 10
    --    |> move mem.end.x mem.end.y
    --    |> fade 0.8
    --, circle green 40
    --    |> (let
    --            pt =
    --                ptMovToCurr mem.ptMov
    --        in
    --        move pt.x pt.y
    --       )
    --    |> fade 0.5
    , viewPathPt mem.pathStart
    , group (List.map viewPathPt mem.path)
    , circle red 10
        |> (let
                pt =
                    ptMovPathToCurr mem.ptMovPath
            in
            move pt.x pt.y
           )
        |> fade 0.4
    , List.map viewMonster mem.monsters
        |> group
    ]


viewMonster monster =
    circle red 10
        |> (let
                pt =
                    ptMovPathToCurr monster
            in
            move pt.x pt.y
           )
        |> fade 0.4


viewPathPt pt =
    rectangle black 10 10
        |> move pt.x pt.y
        |> fade 0.8


main =
    game view update init
