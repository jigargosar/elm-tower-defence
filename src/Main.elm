module Main exposing (main)

import Bomb exposing (Bomb)
import BombId exposing (BombId)
import List.Extra
import Location as L exposing (Location)
import Playground exposing (..)
import Random exposing (Generator, Seed)
import Random.Extra
import String exposing (fromInt)



-- Config


isDebug =
    True


bombTowerReloadDelay =
    50


bombTowerRange =
    150


bombSpeed =
    3


bombAOE =
    40


towerReloadDelay =
    40


towerRange =
    200


bulletSpeed =
    10


monsterSpeed =
    1


monsterHealth =
    5


allTowersViewWidth =
    30



-- Tower
-- TODO: Rename to ArrowTower?


type TowerId
    = TowerId Int


towerIdGenerator : Generator TowerId
towerIdGenerator =
    mapRandomIdInt TowerId


type TowerType
    = ArrowShooter
    | AOEShooter


type alias Tower =
    { -- META
      id : TowerId
    , delay : Number -- RELOAD TIME
    , range : Number -- SHOOTING RANGE
    , location : Location
    , viewWidth : Number
    , towerType : TowerType

    -- STATE
    , elapsed : Number -- RELOAD PROGRESS
    }


arrowTowerGenerator : Location -> Generator Tower
arrowTowerGenerator location =
    towerIdGenerator
        |> Random.map
            (\tid ->
                { id = tid
                , delay = towerReloadDelay
                , range = towerRange
                , towerType = ArrowShooter
                , location = location
                , viewWidth = allTowersViewWidth
                , elapsed = 0
                }
            )


bombTowerGenerator : Location -> Generator Tower
bombTowerGenerator location =
    towerIdGenerator
        |> Random.map
            (\tid ->
                { id = tid
                , delay = bombTowerReloadDelay
                , range = bombTowerRange
                , towerType = AOEShooter
                , location = location
                , viewWidth = allTowersViewWidth
                , elapsed = 0
                }
            )


idOfTower : Tower -> TowerId
idOfTower =
    .id


locationOfTower : Tower -> Location
locationOfTower =
    .location


isLocationInRangeOfTower : Location -> Tower -> Bool
isLocationInRangeOfTower location tower =
    L.distanceFromTo location tower.location <= tower.range


isLocationOnTowerView : Location -> Tower -> Bool
isLocationOnTowerView location tower =
    L.isLocationInSquareAt tower.location tower.viewWidth location



-- BUTTON


type alias Button =
    { location : Location
    , width : Number
    , height : Number
    , text : String
    }


initButtons : Location -> List Button
initButtons location =
    [ Button location 100 50 "TOWER UI BUTTON" ]



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



-- TRAVEL PATH


type Path
    = Path Number Location (List Location)


initPath : Location -> List Location -> Path
initPath start rest =
    let
        pathLen =
            List.foldl (\to ( accDistance, from ) -> ( L.distanceFromTo from to + accDistance, to ))
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
    { p | current = L.shiftY -p.offset p.current }


goUp : PathBuilder -> PathBuilder
goUp p =
    { p | current = L.shiftY p.offset p.current }


goRight : PathBuilder -> PathBuilder
goRight p =
    { p | current = L.shiftX p.offset p.current }


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
            case L.stepLocationTowards wp p.speed p.location of
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
decrementMonsterHealth =
    decrementMonsterHealthBy 1


decrementMonsterHealthBy : Number -> Monster -> Monster
decrementMonsterHealthBy damage monster =
    let
        func state =
            case state of
                AliveAndKicking { health, travel } ->
                    let
                        newHealth =
                            health - damage
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


lairGenerator : Generator Lair
lairGenerator =
    Random.independentSeed
        |> Random.map
            (\seed ->
                { seed = seed
                , delay = 60
                , elapsed = 0
                }
            )



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
    , bombs : List Bomb
    , monsters : List Monster
    , house : House
    , selectedTowerId : Maybe TowerId
    , nextIdx : Int
    , seed : Seed
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
            initPathBuilder 50 (L.at -250 0)
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

        ( ig, worldSeed ) =
            Random.step initialGen (Random.initialSeed 0)
    in
    Running
        { lair = ig.lair
        , path = path
        , towers = ig.towers
        , selectedTowerId = Nothing
        , bullets = []
        , bombs = []
        , monsters = []
        , house = initHouse
        , nextIdx = 0
        , seed = worldSeed
        }


type alias InitialWorldData =
    { lair : Lair
    , towers : List Tower
    }


initialGen : Generator InitialWorldData
initialGen =
    let
        initialTowersGenerator : Generator (List Tower)
        initialTowersGenerator =
            [ arrowTowerGenerator (L.at -150 -100)
            , arrowTowerGenerator (L.at 150 100)
            , bombTowerGenerator (L.at 0 0)
            , bombTowerGenerator (L.at 150 -100)
            ]
                |> Random.Extra.combine
    in
    Random.map2 InitialWorldData
        lairGenerator
        initialTowersGenerator



-- UPDATE


type Event
    = NoEvent
    | SpawnMonster
    | SpawnBullet BulletInit
    | BulletHitMonster MonsterId
    | RemoveBullet BulletId
    | RemoveMonster MonsterId
    | MonsterReachedHouse
    | BombExploded { at : Location, aoe : Number, damage : Number }
    | RemoveBomb BombId
    | SpawnBomb { from : Location, to : Location }
    | SelectTower TowerId


update : Computer -> Game -> Game
update computer game =
    case game of
        Running world ->
            let
                newWorld =
                    updateWorld computer world
            in
            if hasHouseBurnedDown newWorld then
                GameOver newWorld

            else
                Running newWorld

        GameOver world ->
            GameOver world



-- UPDATE WORLD


updateWorld : Computer -> World -> World
updateWorld computer =
    stepWorldLair
        >> stepWorldHouse
        >> stepWorldBullets
        >> stepWorldMonsters
        >> stepWorldBombs
        >> stepWorldTowers computer
        >> stepWordClick computer


stepWordClick : Computer -> World -> World
stepWordClick computer world =
    let
        { mouse } =
            computer
    in
    if mouse.click then
        let
            clickedTower =
                List.Extra.find (isLocationOnTowerView (L.at mouse.x mouse.y)) world.towers
        in
        case ( world.selectedTowerId, clickedTower ) of
            ( Nothing, Just tower ) ->
                { world | selectedTowerId = Just (idOfTower tower) }

            ( Just _, Nothing ) ->
                { world | selectedTowerId = Nothing }

            ( Just tid, Just tower ) ->
                if tid == idOfTower tower then
                    { world | selectedTowerId = Nothing }

                else
                    { world | selectedTowerId = Just (idOfTower tower) }

            _ ->
                world

    else
        world


stepWorldLair : World -> World
stepWorldLair world =
    stepLair world.lair
        |> (\( lair, events ) -> handleEvents events { world | lair = lair })


stepWorldHouse : World -> World
stepWorldHouse world =
    stepHouse world.house
        |> (\( house, events ) -> handleEvents events { world | house = house })


stepWorldBullets : World -> World
stepWorldBullets =
    let
        func bullet world =
            stepBullet bullet
                |> (\( newBullet, events ) -> handleEvents events (setBullet newBullet world))
    in
    \world -> List.foldl func world world.bullets


setBullet : Bullet -> World -> World
setBullet bullet world =
    { world | bullets = List.Extra.setIf (idOfBullet >> is (idOfBullet bullet)) bullet world.bullets }


stepWorldTowers : Computer -> World -> World
stepWorldTowers computer world =
    let
        akaMonstersSortedByRemainingDistance =
            world.monsters
                |> List.filterMap aakMonsterState
                |> List.sortBy .remainingDistance

        stepTowerHelp tower =
            stepTower computer
                (Just (idOfTower tower) == world.selectedTowerId)
                akaMonstersSortedByRemainingDistance
                tower

        ( selfUpdatedTowers, towerEventGroups ) =
            List.map stepTowerHelp world.towers
                |> List.unzip
    in
    { world | towers = selfUpdatedTowers }
        |> handleEvents (List.concat towerEventGroups)


stepWorldBombs : World -> World
stepWorldBombs =
    let
        func bomb world =
            Bomb.stepBomb
                { remove = RemoveBomb
                , exploded = BombExploded
                }
                bomb
                |> (\( newBomb, events ) -> handleEvents events (setBomb newBomb world))
    in
    \world -> List.foldl func world world.bombs


setBomb : Bomb -> World -> World
setBomb bomb world =
    { world | bombs = List.Extra.setIf (Bomb.id >> is (Bomb.id bomb)) bomb world.bombs }


stepWorldMonsters : World -> World
stepWorldMonsters =
    let
        func monster world =
            stepMonster monster
                |> (\( newMonster, events ) -> handleEvents events (setMonster newMonster world))
    in
    \world -> List.foldl func world world.monsters


setMonster : Monster -> World -> World
setMonster monster world =
    { world | monsters = List.Extra.setIf (idOfMonster >> is (idOfMonster monster)) monster world.monsters }


handleEvents : List Event -> World -> World
handleEvents events acc =
    List.foldl handleEvent acc events


handleEvent : Event -> World -> World
handleEvent event world =
    case event of
        NoEvent ->
            world

        BulletHitMonster monsterId ->
            { world
                | monsters =
                    List.Extra.updateIf (idOfMonster >> is monsterId) decrementMonsterHealth world.monsters
            }

        RemoveBullet bulletId ->
            { world | bullets = List.filter (idOfBullet >> isNot bulletId) world.bullets }

        RemoveMonster monsterId ->
            { world | monsters = List.filter (idOfMonster >> isNot monsterId) world.monsters }

        MonsterReachedHouse ->
            { world | house = decrementHouseHealth world.house }

        SpawnMonster ->
            { world
                | monsters = initMonster world.nextIdx world.path :: world.monsters
                , nextIdx = world.nextIdx + 1
            }

        SpawnBullet bulletInit ->
            { world
                | bullets = initBullet world.nextIdx bulletInit :: world.bullets
                , nextIdx = world.nextIdx + 1
            }

        BombExploded { at, aoe, damage } ->
            let
                isLocationInAOE =
                    L.isLocationInRangeOf at aoe

                monsterIdsInBombAOE =
                    world.monsters
                        |> List.filterMap aakMonsterState
                        |> List.filter (.location >> isLocationInAOE)
                        |> List.map .id

                isMonsterInBombAOE monster =
                    List.member (idOfMonster monster) monsterIdsInBombAOE
            in
            { world
                | monsters =
                    List.Extra.updateIf isMonsterInBombAOE (decrementMonsterHealthBy damage) world.monsters
            }

        RemoveBomb bombId ->
            { world | bombs = List.filter (Bomb.id >> isNot bombId) world.bombs }

        SpawnBomb { from, to } ->
            stepWorldSeed
                (Bomb.generator
                    { location = from
                    , target = to
                    , aoe = bombAOE
                    , speed = bombSpeed
                    }
                )
                world
                |> uncurry insertNewBomb

        SelectTower towerId ->
            { world | selectedTowerId = Just towerId }


insertNewBomb : Bomb -> World -> World
insertNewBomb bomb world =
    { world | bombs = bomb :: world.bombs }


stepWorldSeed : Generator a -> World -> ( a, World )
stepWorldSeed func world =
    Random.step func world.seed
        |> Tuple.mapSecond (\seed -> { world | seed = seed })



-- WORLD ENTITY STEP FUNCTIONS


stepTower : Computer -> Bool -> List AAKMonster -> Tower -> ( Tower, List Event )
stepTower computer isSelected aakMonsters tower =
    let
        { mouse } =
            computer
    in
    if tower.elapsed >= tower.delay then
        case
            List.Extra.find
                (\aak ->
                    isLocationInRangeOfTower aak.location tower
                )
                aakMonsters
        of
            Just aak ->
                ( { tower | elapsed = 0 }
                , [ case tower.towerType of
                        ArrowShooter ->
                            SpawnBullet
                                { monsterId = aak.id
                                , start = tower.location
                                , target = aak.location
                                }

                        AOEShooter ->
                            SpawnBomb
                                { from = tower.location
                                , to = aak.location
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
    case L.stepLocationTowards bullet.target bullet.speed bullet.location of
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
    let
        viewTowerHelp tower =
            viewTower (world.selectedTowerId == Just (idOfTower tower)) tower
    in
    [ List.map viewTowerHelp world.towers |> group
    , viewPath world.path
    , List.map viewMonster world.monsters |> group
    , List.map Bomb.viewBomb world.bombs |> group
    , List.map viewBullet world.bullets |> group
    , case List.Extra.find (idOfTower >> Just >> is world.selectedTowerId) world.towers of
        Just t ->
            locationOfTower t
                |> initButtons
                |> List.map viewButton
                |> group

        Nothing ->
            noShape
    ]
        |> group


viewButton : Button -> Shape
viewButton button =
    [ rectangle lightOrange button.width button.height
    , words charcoal button.text
    ]
        |> group
        |> L.moveShape button.location


viewPath : Path -> Shape
viewPath path =
    let
        ep location =
            circle lightCharcoal 8
                |> L.moveShape location
    in
    pathToLocations path
        |> List.map ep
        |> group


viewMonster : Monster -> Shape
viewMonster monster =
    let
        monsterShape =
            --[ circle red 20 |> fade 0.9 ] |> group
            [ rectangle purple 20 30
            , circle charcoal 10 |> moveUp 22.5
            , -- legs
              rectangle charcoal 7.5 20 |> moveLeft 5 |> moveDown 25
            , rectangle charcoal 7.5 20 |> moveRight 5 |> moveDown 25
            , rectangle purple 22.5 15 |> moveDown 10
            ]
                |> group

        healthBarShape pct =
            let
                width =
                    40

                healthWidth =
                    width * pct

                barHeight =
                    10
            in
            [ rectangle red width barHeight
            , rectangle green healthWidth barHeight
                |> moveLeft ((width - healthWidth) / 2)
            ]
                |> group

        viewHealthBar health =
            healthBarShape (health / monster.maxHealth)

        placeShape : PathProgress -> Shape -> Shape
        placeShape travel =
            scale 0.7 >> L.moveShape (locationOfPathProgress travel)
    in
    case monster.state of
        AliveAndKicking { travel, health } ->
            [ monsterShape
            , viewHealthBar health |> moveUp 40
            , debugShape <|
                \_ -> words white (fromInt (round health))
            ]
                |> group
                |> placeShape travel

        Dying { travel, remainingTicks, overKill } ->
            let
                remainingProgress =
                    remainingTicks / monster.dyingTicks
            in
            [ monsterShape |> fade (remainingProgress / 2)
            , debugShape <|
                \_ -> words white (fromInt (round overKill))
            ]
                |> group
                |> placeShape travel

        ReachedHouse _ ->
            group []

        ReadyForRemoval ->
            group []


viewTower : Bool -> Tower -> Shape
viewTower isSelected tower =
    let
        ( lightC, darkC ) =
            case tower.towerType of
                ArrowShooter ->
                    ( lightBlue, blue )

                AOEShooter ->
                    ( lightBrown, brown )
    in
    [ [ circle lightC tower.range |> fade 0.3
      ]
        |> group
        |> fade
            (if isSelected then
                1

             else
                0
            )
    , square darkC tower.viewWidth
    ]
        |> group
        |> L.moveShape tower.location


viewBullet : Bullet -> Shape
viewBullet bullet =
    circle blue 5
        |> L.moveShape bullet.location


noShape : Shape
noShape =
    group []


debugShape : (() -> Shape) -> Shape
debugShape func =
    if isDebug then
        func ()

    else
        noShape



-- EXTRA


is =
    (==)


isNot =
    (/=)


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry func ( a, b ) =
    func a b


mapRandomIdInt : (Int -> b) -> Generator b
mapRandomIdInt func =
    Random.int 0 Random.maxInt
        |> Random.map func



-- MAIN


main =
    game view update init
