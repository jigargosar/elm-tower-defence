module Events exposing (Game, init, update, view)

-- TODO: This file needs to be renamed to reflect its current state.
-- Perhaps: Game or TDGame Or Main or TDMain or Main2
-- Not sure, so not changing it for now.

import List.Extra
import Playground exposing (..)
import Random exposing (Seed, initialSeed)
import String exposing (fromInt)



-- Config


bombTowerReloadDelay =
    50


bombTowerRange =
    150


bombSpeed =
    10


bulletFireDelay =
    40


bulletSpeed =
    10


monsterSpeed =
    1


monsterHealth =
    5



-- LOCATION


type Location
    = Location Number Number


stepLocationTowards : Location -> Number -> Location -> Maybe Location
stepLocationTowards target speed location =
    let
        (Location tx ty) =
            target

        (Location x y) =
            location

        angle =
            atan2 (ty - y) (tx - x)

        ( dx, dy ) =
            ( speed, angle ) |> fromPolar

        ( nx, ny ) =
            ( x + dx, y + dy )

        newLocation =
            Location nx ny
    in
    if locationEqWithin speed newLocation target then
        Nothing

    else
        Just newLocation


locationEqWithin : Number -> Location -> Location -> Bool
locationEqWithin tol l1 l2 =
    distanceFromToLocation l1 l2 <= tol + 0.1


distanceFromToLocation : Location -> Location -> Number
distanceFromToLocation (Location x1 y1) (Location x2 y2) =
    let
        ( x, y ) =
            ( x2 - x1, y2 - y1 )
    in
    sqrt (add (mul x x) (mul y y))



-- Tower
-- TODO: Rename to ArrowTower?


type alias Tower =
    { -- META
      delay : Number -- RELOAD TIME
    , range : Number -- SHOOTING RANGE
    , location : Location
    , viewWidth : Number

    -- STATE
    , elapsed : Number -- RELOAD PROGRESS
    }


initTower : Location -> Number -> Tower
initTower location range =
    { delay = bulletFireDelay
    , range = range
    , location = location
    , viewWidth = 30
    , elapsed = 0
    }


isLocationInRangeOfTower : Location -> Tower -> Bool
isLocationInRangeOfTower location tower =
    distanceFromToLocation location tower.location <= tower.range



-- Bullet
-- TODO: Should we rename it to Arrow?


type alias Bullet =
    { --CONFIG
      id : BulletId
    , monsterId : MonsterId
    , target : Location
    , speed : Number

    -- STATE
    , location : Location
    }


type alias BulletInit =
    { monsterId : MonsterId
    , start : Location
    , target : Location
    }


initBullet : Int -> BulletInit -> Bullet
initBullet idx { monsterId, target, start } =
    { id = BulletId idx
    , monsterId = monsterId
    , target = target
    , location = start
    , speed = bulletSpeed
    }


idOfBullet : Bullet -> BulletId
idOfBullet bullet =
    bullet.id


type BulletId
    = BulletId Int



-- BOMB TOWER


type alias BombTower =
    { -- Meta
      delay : Number
    , location : Location
    , range : Number
    , viewWidth : Number

    -- State
    , elapsed : Number
    }


initBombTower : Location -> BombTower
initBombTower location =
    { delay = bombTowerReloadDelay
    , range = bombTowerRange
    , viewWidth = 30
    , location = location
    , elapsed = 0
    }



-- NOTE: Trying to build isolated stepBombTower function,
-- i.e. minimizing its dependency on other entities in game.
-- Caution: Be ready to revert if its no longer fun.


type alias BombTowerConfig event =
    { spawnBomb : { from : Location, to : Location } -> event
    }


type alias BombTowerContext =
    { targets : List BombTowerTarget
    }


type alias BombTowerTarget =
    { location : Location
    , distanceToHouse : Number
    }


stepBombTower : BombTowerConfig event -> BombTowerContext -> BombTower -> ( BombTower, List event )
stepBombTower config ctx tower =
    if tower.elapsed >= tower.delay then
        case
            List.Extra.find
                (\aak ->
                    distanceFromToLocation tower.location aak.location <= tower.range
                )
                ctx.targets
        of
            Just aak ->
                ( { tower | elapsed = 0 }
                , [ config.spawnBomb { from = tower.location, to = aak.location }
                  ]
                )

            Nothing ->
                ( tower, [] )

    else
        ( { tower | elapsed = tower.elapsed + 1 }, [] )


viewBombTower : BombTower -> Shape
viewBombTower tower =
    let
        (Location x y) =
            tower.location
    in
    [ circle lightBrown tower.range |> fade 0.4
    , square brown tower.viewWidth
    ]
        |> group
        |> move x y



--viewBullet : Bullet -> Shape
--viewBullet bullet =
--    let
--        (Location x y) =
--            bullet.location
--    in
--    circle green 5
--        |> move x y
-- BOMB


type BombId
    = BombId Int


type alias Bomb =
    { id : BombId
    , location : Location
    , speed : Number
    , target : Location
    }


type alias BombInit =
    { from : Location, to : Location }


initBomb : Int -> BombInit -> Bomb
initBomb idx { from, to } =
    { id = BombId idx
    , location = from
    , target = to
    , speed = bombSpeed
    }


stepBomb : { remove : BombId -> event, reachedTarget : Location -> event } -> Bomb -> ( Bomb, List event )
stepBomb config bomb =
    case stepLocationTowards bomb.target bomb.speed bomb.location of
        Nothing ->
            ( bomb, [ config.remove bomb.id, config.reachedTarget bomb.target ] )

        Just newLocation ->
            ( { bomb | location = newLocation }, [] )



-- TRAVEL PATH


type Path
    = Path Number Location (List Location)


initPath : Location -> List Location -> Path
initPath start rest =
    let
        pathLen =
            List.foldl (\to ( accDistance, from ) -> ( distanceFromToLocation from to + accDistance, to ))
                ( 0, start )
                rest
                |> Tuple.first
    in
    Path pathLen start rest


pathToLocations : Path -> List Location
pathToLocations (Path _ s r) =
    s :: r


lengthOfPath : Path -> Number
lengthOfPath (Path l _ _) =
    l


startOfPath : Path -> Location
startOfPath (Path _ s _) =
    s


restOfPath : Path -> List Location
restOfPath (Path _ _ rest) =
    rest



-- PATH BUILDER


type alias PathBuilder =
    { start : Location
    , offset : Number
    , current : Location
    , restReverse : List Location
    }


initPathBuilder : Number -> Location -> PathBuilder
initPathBuilder offset start =
    { start = start
    , offset = offset
    , current = start
    , restReverse = []
    }


goDown : PathBuilder -> PathBuilder
goDown p =
    let
        (Location x y) =
            p.current

        ny =
            y - p.offset
    in
    { p | current = Location x ny }


goUp : PathBuilder -> PathBuilder
goUp p =
    let
        (Location x y) =
            p.current

        ny =
            y + p.offset
    in
    { p | current = Location x ny }


goRight : PathBuilder -> PathBuilder
goRight p =
    let
        (Location x y) =
            p.current

        nx =
            x + p.offset
    in
    { p | current = Location nx y }


goto : Location -> PathBuilder -> PathBuilder
goto to p =
    { p | current = to }


addWayPoint : PathBuilder -> PathBuilder
addWayPoint p =
    { p | restReverse = p.current :: p.restReverse }


buildPath : PathBuilder -> Path
buildPath p =
    initPath p.start (List.reverse p.restReverse)



-- TRAVEL PATH PROGRESS


type PathProgress
    = PathProgress
        { path : Path
        , speed : Number
        , location : Location
        , wayPoints : List Location
        }


initPathProgress : Path -> Number -> PathProgress
initPathProgress path speed =
    PathProgress
        { path = path
        , speed = speed
        , location = startOfPath path
        , wayPoints = restOfPath path
        }


locationOfPathProgress : PathProgress -> Location
locationOfPathProgress (PathProgress { location }) =
    location


stepPathProgress : PathProgress -> Maybe PathProgress
stepPathProgress (PathProgress p) =
    case p.wayPoints of
        [] ->
            Nothing

        wp :: rest ->
            case stepLocationTowards wp p.speed p.location of
                Nothing ->
                    Just (PathProgress { p | location = wp, wayPoints = rest })

                Just newLocation ->
                    Just (PathProgress { p | location = newLocation })


distanceToPathEnd : PathProgress -> Number
distanceToPathEnd (PathProgress p) =
    initPath p.location p.wayPoints
        |> lengthOfPath



-- Monster


type alias Monster =
    { -- CONFIG
      id : MonsterId
    , maxHealth : Number
    , speed : Number
    , dyingTicks : Number

    -- STATE
    , state : MonsterState
    }


type MonsterState
    = AliveAndKicking { health : Number, travel : PathProgress }
    | Dying { travel : PathProgress, remainingTicks : Number, overKill : Number }
    | ReachedHouse { health : Number }
    | ReadyForRemoval


initMonster : Int -> Path -> Monster
initMonster idx path =
    let
        maxHealth =
            monsterHealth

        speed =
            monsterSpeed
    in
    { id = MonsterId idx
    , maxHealth = maxHealth
    , speed = speed
    , dyingTicks = 120
    , state =
        AliveAndKicking
            { health = maxHealth
            , travel = initPathProgress path speed
            }
    }


decrementMonsterHealth : Monster -> Monster
decrementMonsterHealth monster =
    let
        func state =
            case state of
                AliveAndKicking { health, travel } ->
                    let
                        newHealth =
                            health - 1
                    in
                    (if newHealth <= 0 then
                        Dying
                            { travel = travel
                            , remainingTicks = monster.dyingTicks
                            , overKill = abs newHealth
                            }

                     else
                        AliveAndKicking { health = newHealth, travel = travel }
                    )
                        |> Just

                Dying r ->
                    Dying { r | overKill = r.overKill + 1 }
                        |> Just

                ReachedHouse _ ->
                    Nothing

                ReadyForRemoval ->
                    Nothing
    in
    case func monster.state of
        Nothing ->
            monster

        Just state ->
            { monster | state = state }


type alias AAKMonster =
    { id : MonsterId
    , location : Location
    , remainingDistance : Number
    }


aakMonsterState : Monster -> Maybe AAKMonster
aakMonsterState monster =
    case monster.state of
        AliveAndKicking { travel } ->
            Just (AAKMonster monster.id (locationOfPathProgress travel) (distanceToPathEnd travel))

        Dying { travel } ->
            Nothing

        ReachedHouse _ ->
            Nothing

        ReadyForRemoval ->
            Nothing


idOfMonster : Monster -> MonsterId
idOfMonster =
    .id


type MonsterId
    = MonsterId Int



-- LAIR


type alias Lair =
    { seed : Seed
    , delay : Number
    , elapsed : Number
    }


initLair : Lair
initLair =
    { seed = initialSeed 0
    , delay = 60
    , elapsed = 0
    }



-- HOUSE


type alias House =
    { maxHealth : Number
    , health : Number
    }


initHouse : House
initHouse =
    let
        maxHealth =
            10
    in
    { maxHealth = maxHealth
    , health = maxHealth
    }


healthOfHouse : House -> Number
healthOfHouse =
    .health


decrementHouseHealth : House -> House
decrementHouseHealth house =
    { house | health = max 0 (house.health - 1) }



-- WORLD


type alias World =
    { lair : Lair
    , path : Path
    , towers : List Tower
    , bullets : List Bullet
    , bombTowers : List BombTower
    , bombs : List Bomb
    , monsters : List Monster
    , house : House
    , nextIdx : Int
    }


hasHouseBurnedDown : World -> Bool
hasHouseBurnedDown world =
    healthOfHouse world.house == 0



-- GAME


type Game
    = Running World
    | GameOver World


applyNTimes : Int -> (c -> c) -> c -> c
applyNTimes n func val =
    List.foldl (always func) val (List.range 0 n)


init : Game
init =
    let
        path : Path
        path =
            initPathBuilder 50 (Location -250 0)
                |> applyNTimes 2 goRight
                |> addWayPoint
                |> applyNTimes 3 goDown
                |> addWayPoint
                |> applyNTimes 3 goRight
                |> addWayPoint
                |> applyNTimes 3 goUp
                |> addWayPoint
                |> applyNTimes 2 goRight
                |> addWayPoint
                |> buildPath

        towers =
            [ initTower (Location -150 -100) 200
            , initTower (Location 150 100) 150
            ]

        bombTowers =
            [ initBombTower (Location 0 0)
            , initBombTower (Location 150 -100)
            ]
    in
    Running
        { lair = initLair
        , path = path
        , towers = towers
        , bullets = []
        , bombTowers = bombTowers
        , bombs = []
        , monsters = []
        , house = initHouse
        , nextIdx = 0
        }



-- UPDATE


type Event
    = NoEvent
    | SpawnMonster
    | SpawnBullet BulletInit
    | BulletHitMonster MonsterId
    | RemoveBullet BulletId
    | RemoveMonster MonsterId
    | MonsterReachedHouse
    | BombReachedTarget Location
    | RemoveBomb BombId
    | SpawnBomb BombInit


update : Computer -> Game -> Game
update _ game =
    case game of
        Running world ->
            let
                newWorld =
                    updateWorld world
            in
            if hasHouseBurnedDown newWorld then
                GameOver newWorld

            else
                Running newWorld

        GameOver world ->
            GameOver world


updateWorld : World -> World
updateWorld world =
    let
        ( selfUpdatedLair, lairEvents ) =
            stepLair world.lair

        ( selfUpdatedHouse, houseEvents ) =
            stepHouse world.house

        akaMonstersSortedByRemainingDistance =
            world.monsters
                |> List.filterMap aakMonsterState
                |> List.sortBy .remainingDistance

        ( selfUpdatedTowers, towerEventGroups ) =
            List.map (stepTower akaMonstersSortedByRemainingDistance) world.towers
                |> List.unzip

        ( selfUpdatedBullets, bulletEventGroups ) =
            List.map stepBullet world.bullets
                |> List.unzip

        ( selfUpdatedBombTowers, bombTowerEventGroups ) =
            List.map
                (stepBombTower
                    { spawnBomb = SpawnBomb }
                    { targets =
                        akaMonstersSortedByRemainingDistance
                            |> List.map
                                (\aak ->
                                    { location = aak.location
                                    , distanceToHouse = aak.remainingDistance
                                    }
                                )
                    }
                )
                world.bombTowers
                |> List.unzip

        ( selfUpdatedBombs, bombEventGroups ) =
            List.map
                (stepBomb
                    { remove = RemoveBomb
                    , reachedTarget = BombReachedTarget
                    }
                )
                world.bombs
                |> List.unzip

        ( selfUpdatedMonsters, monsterEventGroups ) =
            List.map stepMonster world.monsters
                |> List.unzip

        acc : World
        acc =
            { lair = selfUpdatedLair
            , path = world.path
            , house = selfUpdatedHouse
            , towers = selfUpdatedTowers
            , bullets = selfUpdatedBullets
            , bombTowers = selfUpdatedBombTowers
            , bombs = selfUpdatedBombs
            , monsters = selfUpdatedMonsters
            , nextIdx = world.nextIdx
            }
    in
    acc
        |> handleEvents world lairEvents
        |> handleEvents world houseEvents
        |> handleEventGroups world towerEventGroups
        |> handleEventGroups world bulletEventGroups
        |> handleEventGroups world bombTowerEventGroups
        |> handleEventGroups world bombEventGroups
        |> handleEventGroups world monsterEventGroups


handleEventGroups : World -> List (List Event) -> World -> World
handleEventGroups world lists acc =
    List.foldl (handleEvents world) acc lists


handleEvents : World -> List Event -> World -> World
handleEvents world events acc =
    List.foldl (handleEvent world) acc events


handleEvent : World -> Event -> World -> World
handleEvent world event acc =
    case event of
        NoEvent ->
            acc

        BulletHitMonster monsterId ->
            { acc
                | monsters =
                    List.Extra.updateIf (idOfMonster >> is monsterId) decrementMonsterHealth acc.monsters
            }

        RemoveBullet bulletId ->
            { acc | bullets = List.filter (idOfBullet >> isNot bulletId) acc.bullets }

        RemoveMonster monsterId ->
            { acc | monsters = List.filter (idOfMonster >> isNot monsterId) acc.monsters }

        MonsterReachedHouse ->
            { acc | house = decrementHouseHealth acc.house }

        SpawnMonster ->
            { acc
                | monsters = initMonster acc.nextIdx world.path :: acc.monsters
                , nextIdx = acc.nextIdx + 1
            }

        SpawnBullet bulletInit ->
            { acc
                | bullets = initBullet acc.nextIdx bulletInit :: acc.bullets
                , nextIdx = acc.nextIdx + 1
            }

        BombReachedTarget location ->
            acc

        RemoveBomb bombId ->
            acc

        SpawnBomb bombInit ->
            acc


stepTower : List AAKMonster -> Tower -> ( Tower, List Event )
stepTower aakMonsters tower =
    if tower.elapsed >= tower.delay then
        case List.Extra.find (\aak -> isLocationInRangeOfTower aak.location tower) aakMonsters of
            Just aak ->
                ( { tower | elapsed = 0 }
                , [ SpawnBullet
                        { monsterId = aak.id
                        , start = tower.location
                        , target = aak.location
                        }
                  ]
                )

            Nothing ->
                ( tower, [] )

    else
        ( { tower | elapsed = tower.elapsed + 1 }, [] )


stepLair : Lair -> ( Lair, List Event )
stepLair lair =
    if lair.elapsed >= lair.delay then
        let
            randomBool =
                Random.int 0 1 |> Random.map (is 0)

            ( bool, seed ) =
                Random.step randomBool lair.seed
        in
        ( { lair | elapsed = 0, seed = seed }
        , if bool then
            [ SpawnMonster ]

          else
            []
        )

    else
        ( { lair | elapsed = lair.elapsed + 1 }, [] )


stepHouse : House -> ( House, List Event )
stepHouse house =
    ( house, [] )


stepMonster : Monster -> ( Monster, List Event )
stepMonster monster =
    let
        func state =
            case state of
                AliveAndKicking { health, travel } ->
                    case stepPathProgress travel of
                        Just nt ->
                            ( AliveAndKicking { health = health, travel = nt }, [] )

                        Nothing ->
                            ( ReachedHouse { health = health }, [ MonsterReachedHouse ] )

                Dying { travel, remainingTicks, overKill } ->
                    if remainingTicks <= 0 then
                        ( ReadyForRemoval, [ RemoveMonster monster.id ] )

                    else
                        ( Dying
                            { travel = travel
                            , remainingTicks = max 0 (remainingTicks - 1)
                            , overKill = overKill
                            }
                        , []
                        )

                ReachedHouse _ ->
                    ( ReadyForRemoval, [ RemoveMonster monster.id ] )

                ReadyForRemoval ->
                    ( ReadyForRemoval, [] )
    in
    func monster.state
        |> Tuple.mapFirst (\state -> { monster | state = state })


stepBullet : Bullet -> ( Bullet, List Event )
stepBullet bullet =
    case stepLocationTowards bullet.target bullet.speed bullet.location of
        Nothing ->
            ( bullet, [ RemoveBullet bullet.id, BulletHitMonster bullet.monsterId ] )

        Just newLocation ->
            ( { bullet | location = newLocation }, [] )



-- View


view : Computer -> Game -> List Shape
view computer game =
    case game of
        Running world ->
            [ words blue "Game Running"
                |> scale 3
                |> moveY computer.screen.top
                |> moveDown 50
            , viewWorldStats computer world |> moveDown 50
            , viewWorld computer world
            ]

        GameOver world ->
            [ [ viewWorldStats computer world |> moveDown 50
              , viewWorld computer world
              ]
                |> group
                |> fade 0.5
            , words red "GAME OVER" |> scale 3
            ]


viewWorldStats : Computer -> World -> Shape
viewWorldStats computer world =
    [ [ words black ("House Health: " ++ fromInt (round (healthOfHouse world.house)))
            |> scale 2
      , words black ("Monster Count: " ++ fromInt (List.length world.monsters))
            |> scale 2
      , words black ("Tower Count: " ++ fromInt (List.length world.towers))
            |> scale 2
      , words black ("Bullet Count: " ++ fromInt (List.length world.bullets))
            |> scale 2
      ]
        |> List.indexedMap (toFloat >> (\idx -> moveDown (idx * 50)))
        |> group
        |> moveY computer.screen.top
        |> moveDown 50
    ]
        |> group


viewWorld : Computer -> World -> Shape
viewWorld _ world =
    [ List.map viewTower world.towers |> group
    , List.map viewBombTower world.bombTowers |> group
    , viewPath world.path
    , List.map viewMonster world.monsters |> group
    , List.map viewBullet world.bullets |> group
    ]
        |> group


viewPath : Path -> Shape
viewPath path =
    let
        ep (Location x y) =
            circle black 8
                |> fade 0.8
                |> move x y
    in
    pathToLocations path
        |> List.map ep
        |> group


viewMonster : Monster -> Shape
viewMonster monster =
    let
        radius =
            20
    in
    case monster.state of
        AliveAndKicking { travel, health } ->
            let
                (Location x y) =
                    locationOfPathProgress travel
            in
            [ [ circle red radius |> fade 0.7 ] |> group
            , words white (fromInt (round health))
            ]
                |> group
                |> move x y

        Dying { travel, remainingTicks, overKill } ->
            let
                (Location x y) =
                    locationOfPathProgress travel

                dyingProgress =
                    1 - (remainingTicks / monster.dyingTicks)
            in
            [ [ circle red radius |> fade 0.7 ]
                |> group
                |> fade (1 - dyingProgress)
                |> scale (1 + dyingProgress)
            , words white (fromInt (round overKill))
            ]
                |> group
                |> move x y

        ReachedHouse _ ->
            group []

        ReadyForRemoval ->
            group []


viewTower : Tower -> Shape
viewTower tower =
    let
        (Location x y) =
            tower.location
    in
    [ circle lightBlue tower.range |> fade 0.3
    , square blue tower.viewWidth
    ]
        |> group
        |> move x y


viewBullet : Bullet -> Shape
viewBullet bullet =
    let
        (Location x y) =
            bullet.location
    in
    circle blue 5
        |> move x y


square : Color -> Number -> Shape
square c w =
    rectangle c w w



-- EXTRA


is =
    (==)


isNot =
    (/=)


add =
    (+)


mul =
    (*)
