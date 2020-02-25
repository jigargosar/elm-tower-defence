module Main exposing (main)

import Playground exposing (..)
import Random exposing (Generator, Seed)



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
    eqw (max 1 (abs dx)) p1.x p2.x && eqw (max 1 (abs dy)) p1.y p2.y


eqw : Float -> Float -> Float -> Bool
eqw tol a b =
    abs (b - a) <= tol



-- MONSTER


type MonsterId
    = MonsterId Int


type Monster
    = Monster MonsterId PtMovPath


posOfMonster : Monster -> Pt
posOfMonster (Monster _ mp) =
    ptMovPathToCurr mp


idOfMonster : Monster -> MonsterId
idOfMonster (Monster id _) =
    id


randomMonsterId =
    Random.int 0 Random.maxInt |> Random.map MonsterId


randomMonster : Mem -> Generator Monster
randomMonster mem =
    randomMonsterId
        |> Random.map
            (\id ->
                Monster id (initPtMovPath mem.pathStart mem.path mem.speed)
            )


randomMonsterSpawn : Mem -> Generator (List Monster)
randomMonsterSpawn mem =
    Random.int 0 500
        |> Random.andThen
            (\n ->
                if n < 10 then
                    randomMonster mem |> Random.map List.singleton

                else
                    Random.constant []
            )


stepMonsters : Mem -> List Monster
stepMonsters mem =
    let
        stepMonster (Monster id mp) =
            stepPtMovPath mp
                |> Tuple.mapSecond (Monster id)
    in
    List.map stepMonster mem.monsters
        |> List.filterMap
            (\( remove, monster ) ->
                if remove then
                    Nothing

                else
                    Just monster
            )


viewMonster : Monster -> Shape
viewMonster (Monster _ mp) =
    circle red 10
        |> (let
                pt =
                    ptMovPathToCurr mp
            in
            move pt.x pt.y
           )
        |> fade 0.4



-- TOWER


type Tower
    = Tower TowerRecord


type alias TowerRecord =
    { pos : Pt
    , delay : Number
    , elapsed : Number
    }


type Bullet
    = Bullet BulletId PtMov MonsterId


type BulletId
    = BulletId Int


randomBulletId : Generator BulletId
randomBulletId =
    Random.int 0 Random.maxInt |> Random.map BulletId


newBullet : Pt -> Pt -> MonsterId -> Generator Bullet
newBullet pos target monsterId =
    randomBulletId |> Random.map (\id -> Bullet id (initPtMov pos target 20) monsterId)


initTower : Pt -> Tower
initTower pt =
    Tower
        { pos = pt
        , delay = 30
        , elapsed = 0
        }


stepTower : List Monster -> Tower -> ( Maybe (Generator (List Bullet)), Tower )
stepTower monsters (Tower t) =
    let
        ( fire, elapsed ) =
            if t.elapsed >= t.delay then
                ( True, 0 )

            else
                ( False, t.elapsed + 1 )

        newBullets =
            if fire then
                case
                    monsters
                        |> List.reverse
                        |> List.head
                of
                    Just monster ->
                        newBullet t.pos (posOfMonster monster) (idOfMonster monster)
                            |> Random.list 1
                            |> Just

                    Nothing ->
                        Nothing

            else
                Nothing
    in
    ( newBullets
    , Tower { t | elapsed = elapsed }
    )


viewTower : Tower -> Shape
viewTower (Tower { pos }) =
    rectangle blue 30 30
        |> move pos.x pos.y
        |> fade 0.8


viewBullets : List Bullet -> Shape
viewBullets bullets =
    let
        viewBullet (Bullet _ mov _) =
            let
                { x, y } =
                    ptMovToCurr mov
            in
            circle green 5
                |> move x y
    in
    List.map viewBullet bullets
        |> group



-- MEM


type alias Mem =
    { speed : Number
    , monsters : List Monster
    , bullets : List Bullet
    , pathStart : Pt
    , path : List Pt
    , seed : Seed
    , tower : Tower
    }


init : Mem
init =
    let
        speed =
            0.5

        pathStart =
            Pt -300 300

        path =
            [ Pt -300 200, Pt -200 100, Pt 0 100, Pt 0 0, Pt -100 -200, Pt 300 -200 ]
    in
    { speed = speed
    , monsters = []
    , bullets = []
    , pathStart = pathStart
    , path = path
    , seed = Random.initialSeed 0
    , tower = initTower (Pt 50 50)
    }



-- UPDATE MEM


update : Computer -> Mem -> Mem
update computer mem =
    computeEvents mem
        |> update2 computer



--noinspection ElmUnusedSymbol


update2 : Computer -> Mem -> Mem
update2 computer mem =
    let
        ( generatedMonsters, newSeed0 ) =
            Random.step (randomMonsterSpawn mem) mem.seed

        ( maybeBulletGen, updatedTower ) =
            stepTower mem.monsters mem.tower

        ( generatedBullets, newSeed1 ) =
            maybeBulletGen
                |> Maybe.map (\gen -> Random.step gen newSeed0)
                |> Maybe.withDefault ( [], newSeed0 )
    in
    { mem
        | monsters = generatedMonsters ++ stepMonsters mem
        , seed = newSeed0
        , bullets = generatedBullets ++ stepBullets mem.bullets
        , tower = updatedTower
    }


type Event
    = DecrementMonsterHealth MonsterId


computeEvents : Mem -> Mem
computeEvents mem =
    let
        foo bullet =
            case didBulletReachMonster bullet of
                Just monsterId ->
                    [ DecrementMonsterHealth monsterId
                    ]

                Nothing ->
                    []

        events =
            List.concatMap foo mem.bullets
    in
    mem


didBulletReachMonster : Bullet -> Maybe MonsterId
didBulletReachMonster bullet =
    Nothing


stepBullets : List Bullet -> List Bullet
stepBullets bullets =
    let
        stepBullet (Bullet id mov monsterId) =
            case stepPtMov mov of
                ( True, _ ) ->
                    Nothing

                ( False, nMov ) ->
                    Just (Bullet id nMov monsterId)
    in
    List.filterMap stepBullet bullets



-- VIEW MEM


view : Computer -> Mem -> List Shape
view computer mem =
    [ words black "Welcome to Adventure"
        |> moveY computer.screen.top
        |> moveDown 50
    , viewPathPt mem.pathStart
    , group (List.map viewPathPt mem.path)
    , List.map viewMonster mem.monsters
        |> group
    , viewTower mem.tower
    , viewBullets mem.bullets
    ]


viewPathPt pt =
    rectangle black 10 10
        |> move pt.x pt.y
        |> fade 0.8


main =
    game view update init
