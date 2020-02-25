module Main exposing (main)

import List.Extra
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
    = FollowingPath MonsterRec
    | Dead MonsterRec
    | ReachedPathEnd MonsterRec


type alias MonsterRec =
    { id : MonsterId
    , movPath : PtMovPath
    , health : Number
    , maxHealth : Number
    }


decrementMonsterHealth : Monster -> Monster
decrementMonsterHealth m =
    case m of
        FollowingPath mr ->
            let
                newHealth =
                    mr.health - 1
            in
            if newHealth <= 0 then
                Dead { mr | health = 0 }

            else
                FollowingPath { mr | health = newHealth }

        Dead mr ->
            Dead mr

        ReachedPathEnd mr ->
            ReachedPathEnd mr


moveMonsterOnPath : Monster -> Monster
moveMonsterOnPath monster =
    case monster of
        FollowingPath mr ->
            case stepPtMovPath mr.movPath of
                ( True, nmp ) ->
                    ReachedPathEnd { mr | movPath = nmp }

                ( False, nmp ) ->
                    FollowingPath { mr | movPath = nmp }

        Dead _ ->
            monster

        ReachedPathEnd _ ->
            monster


posOfMonster : Monster -> Pt
posOfMonster m =
    case m of
        FollowingPath mr ->
            ptMovPathToCurr mr.movPath

        Dead mr ->
            ptMovPathToCurr mr.movPath

        ReachedPathEnd mr ->
            ptMovPathToCurr mr.movPath


idOfMonster : Monster -> MonsterId
idOfMonster m =
    case m of
        FollowingPath mr ->
            mr.id

        Dead mr ->
            mr.id

        ReachedPathEnd mr ->
            mr.id


randomMonsterId =
    Random.int 0 Random.maxInt |> Random.map MonsterId


newMonster : Mem -> Generator Monster
newMonster mem =
    randomMonsterId
        |> Random.map
            (\id ->
                let
                    maxHealth =
                        10
                in
                FollowingPath
                    { id = id
                    , movPath = initPtMovPath mem.pathStart mem.path mem.speed
                    , health = maxHealth
                    , maxHealth = maxHealth
                    }
            )


randomMonsterSpawn : Mem -> Generator (List Monster)
randomMonsterSpawn mem =
    Random.int 0 500
        |> Random.andThen
            (\n ->
                if n < 10 then
                    newMonster mem |> Random.map List.singleton

                else
                    Random.constant []
            )


viewMonster : Monster -> Shape
viewMonster m =
    circle red 10
        |> (let
                pt =
                    posOfMonster m
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
    = InFlight BulletRec
    | ReachedMonster BulletRec


idOfBullet b =
    case b of
        InFlight br ->
            br.id

        ReachedMonster br ->
            br.id


setBulletMov : PtMov -> Bullet -> Bullet
setBulletMov ptMov bullet =
    case bullet of
        InFlight r ->
            InFlight { r | mov = ptMov }

        ReachedMonster r ->
            ReachedMonster { r | mov = ptMov }


setBulletReachedMonster : Bullet -> Bullet
setBulletReachedMonster bullet =
    case bullet of
        InFlight r ->
            ReachedMonster r

        ReachedMonster _ ->
            bullet


type alias BulletRec =
    { id : BulletId
    , mov : PtMov
    , monsterId : MonsterId
    }


type BulletId
    = BulletId Int


randomBulletId : Generator BulletId
randomBulletId =
    Random.int 0 Random.maxInt |> Random.map BulletId


newBullet : Pt -> Pt -> MonsterId -> Generator Bullet
newBullet pos target monsterId =
    randomBulletId
        |> Random.map
            (\id ->
                InFlight
                    { id = id
                    , mov = initPtMov pos target 20
                    , monsterId = monsterId
                    }
            )


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
        viewBullet b =
            case b of
                InFlight br ->
                    let
                        { x, y } =
                            ptMovToCurr br.mov
                    in
                    circle green 5
                        |> move x y

                ReachedMonster br ->
                    let
                        { x, y } =
                            ptMovToCurr br.mov
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
    , houseHealth : Number
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
    , houseHealth = 10
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
    computeActionsAndUpdateMem mem
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
        , bullets = generatedBullets ++ mem.bullets
        , tower = updatedTower
    }


type Action
    = DecrementMonsterHealth MonsterId
    | RemoveBullet BulletId
    | RemoveMonster MonsterId
    | UpdateBulletMov BulletId PtMov
    | SetBulletReachedMonster BulletId
    | DecrementHouseHealth
    | NoAction
    | MoveMonsterOnPath MonsterId


computeActionsAndUpdateMem : Mem -> Mem
computeActionsAndUpdateMem mem =
    let
        events =
            computeActions mem
    in
    List.foldl updateMemWithAction mem events


updateMemWithAction : Action -> Mem -> Mem
updateMemWithAction event mem =
    case event of
        DecrementMonsterHealth monsterId ->
            { mem
                | monsters =
                    List.Extra.updateIf (idOfMonster >> eq monsterId)
                        decrementMonsterHealth
                        mem.monsters
            }

        RemoveBullet bulletId ->
            { mem | bullets = rejectWhen (idOfBullet >> eq bulletId) mem.bullets }

        RemoveMonster monsterId ->
            { mem | monsters = rejectWhen (idOfMonster >> eq monsterId) mem.monsters }

        UpdateBulletMov bulletId ptMov ->
            { mem | bullets = List.Extra.updateIf (idOfBullet >> eq bulletId) (setBulletMov ptMov) mem.bullets }

        SetBulletReachedMonster bulletId ->
            { mem | bullets = List.Extra.updateIf (idOfBullet >> eq bulletId) setBulletReachedMonster mem.bullets }

        NoAction ->
            mem

        DecrementHouseHealth ->
            { mem | houseHealth = max 0 (mem.houseHealth - 1) }

        MoveMonsterOnPath monsterId ->
            { mem
                | monsters =
                    List.Extra.updateIf (idOfMonster >> eq monsterId)
                        moveMonsterOnPath
                        mem.monsters
            }


eq =
    (==)


rejectWhen : (a -> Bool) -> List a -> List a
rejectWhen pred =
    List.filter (pred >> not)


computeActions : Mem -> List Action
computeActions mem =
    let
        eventsFromBulletState bullet =
            case bullet of
                InFlight br ->
                    let
                        ( reached, nm ) =
                            stepPtMov br.mov
                    in
                    [ UpdateBulletMov br.id nm
                    , if reached then
                        SetBulletReachedMonster br.id

                      else
                        NoAction
                    ]

                ReachedMonster br ->
                    [ DecrementMonsterHealth br.monsterId
                    , RemoveBullet (idOfBullet bullet)
                    ]

        eventsFromMonsterState monster =
            case monster of
                FollowingPath r ->
                    [ MoveMonsterOnPath r.id ]

                Dead _ ->
                    [ RemoveMonster (idOfMonster monster) ]

                ReachedPathEnd _ ->
                    [ RemoveMonster (idOfMonster monster), DecrementHouseHealth ]
    in
    List.concatMap eventsFromBulletState mem.bullets
        ++ List.concatMap eventsFromMonsterState mem.monsters


stepMonsters : Mem -> List Monster
stepMonsters mem =
    let
        stepMonster m =
            case m of
                FollowingPath mr ->
                    case stepPtMovPath mr.movPath of
                        ( True, nmp ) ->
                            ReachedPathEnd { mr | movPath = nmp }

                        ( False, nmp ) ->
                            FollowingPath { mr | movPath = nmp }

                Dead mr ->
                    Dead mr

                ReachedPathEnd mr ->
                    ReachedPathEnd mr
    in
    List.map stepMonster mem.monsters



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
