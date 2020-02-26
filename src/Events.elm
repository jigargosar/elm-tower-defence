module Events exposing (Game, init, update, view)

import List.Extra
import Playground exposing (..)
import Random exposing (Seed, initialSeed)
import String exposing (fromInt)



-- Config


bulletFireDelay =
    10


bulletTicksToHitMonster =
    20



-- Bullet


type alias Bullet =
    { id : BulletId
    , monsterId : MonsterId
    , elapsed : Number
    , ticksToHit : Number
    }


initBullet : Int -> MonsterId -> Bullet
initBullet idx monsterId =
    Bullet (BulletId idx) monsterId 0 bulletTicksToHitMonster


idOfBullet : Bullet -> BulletId
idOfBullet bullet =
    bullet.id


type BulletId
    = BulletId Int



-- LOCATION


type Location
    = Location Number Number


distanceFromToLocation : Location -> Location -> Number
distanceFromToLocation (Location x1 y1) (Location x2 y2) =
    let
        ( x, y ) =
            ( x2 - x1, y2 - y1 )
    in
    sqrt (add (mul x x) (mul y y))



-- TRAVEL PATH


type Path
    = Path Number


initPath : Path
initPath =
    Path 500


pathToLocations : Path -> List Location
pathToLocations (Path l) =
    let
        hl =
            l / 2
    in
    [ Location -hl 0, Location hl 0 ]



-- TRAVEL PATH PROGRESS


type PathProgress
    = PathProgress { path : Path, speed : Number, progress : Number }


initPathProgress : Path -> Number -> PathProgress
initPathProgress path speed =
    PathProgress { path = path, speed = speed, progress = 0 }


pathProgressToLocation : PathProgress -> Location
pathProgressToLocation (PathProgress n) =
    let
        (Path pathLength) =
            n.path

        x =
            (n.progress - 0.5) * pathLength
    in
    Location x 0


stepPathProgress : PathProgress -> Maybe PathProgress
stepPathProgress (PathProgress n) =
    if n.progress >= 1 then
        Nothing

    else
        Just (PathProgress { n | progress = min 1 (n.progress + n.speed) })


pathProgressToNumber : PathProgress -> Number
pathProgressToNumber (PathProgress p) =
    p.progress



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
            15

        speed =
            1 / (60 * 10)
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
    { id : MonsterId, location : Location, progress : Number }


travelProgressAndLocationOfAliveAndKickingMonster : Monster -> Maybe AAKMonster
travelProgressAndLocationOfAliveAndKickingMonster monster =
    case monster.state of
        AliveAndKicking { travel } ->
            Just (AAKMonster monster.id (pathProgressToLocation travel) (pathProgressToNumber travel))

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



-- Tower


type alias Tower =
    { -- CONFIG
      delay : Number -- RELOAD TIME
    , range : Number -- SHOOTING RANGE
    , location : Location
    , w : Number

    -- STATE
    , elapsed : Number -- RELOAD PROGRESS
    }


initTower : Int -> Tower
initTower n =
    let
        w =
            30

        offset =
            5
    in
    { delay = bulletFireDelay
    , range = w * (offset + 2)
    , location = Location (toFloat (n - 1) * (w * offset)) (w * -offset)
    , w = w
    , elapsed = 0
    }


isLocationInRangeOfTower : Location -> Tower -> Bool
isLocationInRangeOfTower location tower =
    distanceFromToLocation location tower.location <= tower.range



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


init : Game
init =
    Running
        { lair = initLair
        , path = initPath
        , towers = List.range 0 1 |> List.map initTower
        , bullets = []
        , monsters = []
        , house = initHouse
        , nextIdx = 0
        }



-- UPDATE


type Event
    = NoEvent
    | SpawnMonster
    | SpawnBullet MonsterId
    | BulletHitMonster MonsterId
    | RemoveBullet BulletId
    | RemoveMonster MonsterId
    | MonsterReachedHouse


update : Computer -> Game -> Game
update computer game =
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

        healthyMonstersSortedByClosestToHouse =
            world.monsters
                |> List.filterMap
                    (\m ->
                        travelProgressAndLocationOfAliveAndKickingMonster m
                    )
                |> List.sortBy (.progress >> negate)

        ( selfUpdatedTowers, towerEventGroups ) =
            List.map (stepTower healthyMonstersSortedByClosestToHouse) world.towers
                |> List.unzip

        ( selfUpdatedBullets, bulletEventGroups ) =
            List.map stepBullet world.bullets
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
            , monsters = selfUpdatedMonsters
            , nextIdx = world.nextIdx
            }
    in
    acc
        |> handleEvents world lairEvents
        |> handleEvents world houseEvents
        |> handleEventGroups world towerEventGroups
        |> handleEventGroups world bulletEventGroups
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

        SpawnBullet monsterId ->
            { acc
                | bullets = initBullet acc.nextIdx monsterId :: acc.bullets
                , nextIdx = acc.nextIdx + 1
            }


stepTower : List AAKMonster -> Tower -> ( Tower, List Event )
stepTower aakMonsters tower =
    if tower.elapsed >= tower.delay then
        case List.Extra.find (\aak -> isLocationInRangeOfTower aak.location tower) aakMonsters of
            Just aak ->
                ( { tower | elapsed = 0 }, [ SpawnBullet aak.id ] )

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
    if bullet.elapsed >= bullet.ticksToHit then
        ( bullet, [ RemoveBullet bullet.id, BulletHitMonster bullet.monsterId ] )

    else
        ( { bullet | elapsed = bullet.elapsed + 1 }, [] )



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
    , viewPath world.path
    , List.map viewMonster world.monsters |> group
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
    case monster.state of
        AliveAndKicking { travel, health } ->
            let
                (Location x y) =
                    pathProgressToLocation travel
            in
            [ [ circle red 30 |> fade 0.7 ] |> group
            , words white (fromInt (round health))
            ]
                |> group
                |> move x y

        Dying { travel, remainingTicks, overKill } ->
            let
                (Location x y) =
                    pathProgressToLocation travel

                dyingProgress =
                    1 - (remainingTicks / monster.dyingTicks)
            in
            [ [ circle red 30 |> fade 0.7 ]
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
    [ circle green tower.range |> fade 0.2
    , square blue tower.w
    ]
        |> group
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
