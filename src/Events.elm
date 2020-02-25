module Events exposing (Game, init, update, view)

import List.Extra
import Playground exposing (..)
import Random exposing (Seed, initialSeed)
import String exposing (fromInt)


bulletFireDelay =
    5


bulletTicksToHitMonster =
    20


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


type alias Monster =
    { id : MonsterId
    , maxHealth : Number
    , health : Number
    , travel : Number
    , speed : Number
    }


initMonster : Int -> Monster
initMonster idx =
    let
        maxHealth =
            15
    in
    { id = MonsterId idx
    , maxHealth = maxHealth
    , health = maxHealth
    , travel = 0
    , speed = 0.01
    }


decrementMonsterHealth : Monster -> Monster
decrementMonsterHealth monster =
    { monster | health = max 0 (monster.health - 1) }


idOfMonster : Monster -> MonsterId
idOfMonster =
    .id


comparableIdOfMonster : Monster -> Int
comparableIdOfMonster =
    idOfMonster >> (\(MonsterId int) -> int)


type MonsterId
    = MonsterId Int


type alias Tower =
    { delay : Number, elapsed : Number }


initTower : Int -> Tower
initTower _ =
    { delay = bulletFireDelay, elapsed = 0 }


type alias Lair =
    { seed : Seed, delay : Number, elapsed : Number }


initLair : Lair
initLair =
    { seed = initialSeed 0
    , delay = 30
    , elapsed = 0
    }


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


type Game
    = Running World
    | GameOver World


init : Game
init =
    Running
        { lair = initLair
        , towers = List.range 0 1 |> List.map initTower
        , bullets = []
        , monsters = []
        , house = initHouse
        , nextIdx = 0
        }


type alias World =
    { lair : Lair
    , towers : List Tower
    , bullets : List Bullet
    , monsters : List Monster
    , house : House
    , nextIdx : Int
    }


type Event
    = NoEvent
    | SpawnMonster
    | SpawnBullet MonsterId
    | BulletHitMonster MonsterId
    | RemoveBullet BulletId
    | RemoveMonster MonsterId
    | MonsterReachedHouse



-- UPDATE


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


hasHouseBurnedDown : World -> Bool
hasHouseBurnedDown world =
    healthOfHouse world.house == 0


updateWorld : World -> World
updateWorld world =
    let
        ( selfUpdatedLair, lairEvents ) =
            stepLair world.lair

        ( selfUpdatedHouse, houseEvents ) =
            stepHouse world.house

        ( selfUpdatedTowers, towerEventGroups ) =
            List.map (stepTower world.monsters) world.towers
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
                | monsters = initMonster acc.nextIdx :: acc.monsters
                , nextIdx = acc.nextIdx + 1
            }

        SpawnBullet monsterId ->
            { acc
                | bullets = initBullet acc.nextIdx monsterId :: acc.bullets
                , nextIdx = acc.nextIdx + 1
            }


stepTower : List Monster -> Tower -> ( Tower, List Event )
stepTower monsters tower =
    if tower.elapsed >= tower.delay then
        case monsters of
            fst :: _ ->
                ( { tower | elapsed = 0 }, [ SpawnBullet (idOfMonster fst) ] )

            _ ->
                ( tower, [] )

    else
        ( { tower | elapsed = tower.elapsed + 1 }, [] )


is =
    (==)


isNot =
    (/=)


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
    if monster.health <= 0 then
        ( monster, [ RemoveMonster monster.id ] )

    else if monster.travel >= 1 then
        ( monster, [ RemoveMonster monster.id, MonsterReachedHouse ] )

    else
        ( { monster | travel = monster.travel + monster.speed }, [] )


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
            , viewWorldStats computer world
                |> moveDown 50
            ]

        GameOver world ->
            [ viewWorldStats computer world
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
    , viewPath
    , List.map viewMonster world.monsters
        |> group
    ]
        |> group


pathLength =
    400


viewPath =
    let
        ep =
            circle black 5
                |> fade 0.8
    in
    [ ep |> moveLeft (pathLength / 2), ep |> moveRight (pathLength / 2) ]
        |> group


viewMonster : Monster -> Shape
viewMonster monster =
    let
        x =
            (monster.travel - 0.5) * pathLength
    in
    circle red 10
        |> fade 0.7
        |> moveRight x
