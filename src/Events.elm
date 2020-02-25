module Events exposing (Game, init, update, view)

import Playground exposing (..)
import Random exposing (Seed, initialSeed)


type Bullet
    = Bullet


type BulletId
    = BulletId


type Monster
    = Monster


type MonsterId
    = MonsterId


type Tower
    = Tower


type alias Lair =
    { seed : Seed, delay : Number, elapsed : Number }


type House
    = House


type Game
    = Running World
    | GameOver World


init : Game
init =
    Running
        { lair = Lair (initialSeed 0) 10 0
        , towers = []
        , bullets = []
        , monsters = []
        , house = House
        }


type alias World =
    { lair : Lair
    , towers : List Tower
    , bullets : List Bullet
    , monsters : List Monster
    , house : House
    }


type Event
    = NoEvent
    | SpawnMonster
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


healthOfHouse : House -> Number
healthOfHouse house =
    1


updateWorld : World -> World
updateWorld world =
    let
        ( selfUpdatedLair, lairEvents ) =
            stepLair world.lair

        ( selfUpdatedHouse, houseEvents ) =
            stepHouse world.house

        ( selfUpdatedTowers, towerEventGroups ) =
            List.map stepTower world.towers
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
            acc

        RemoveBullet bulletId ->
            acc

        RemoveMonster monsterId ->
            acc

        MonsterReachedHouse ->
            acc

        SpawnMonster ->
            acc


stepTower : Tower -> ( Tower, List Event )
stepTower tower =
    ( tower, [] )


stepLair : Lair -> ( Lair, List Event )
stepLair lair =
    if lair.elapsed >= lair.delay then
        let
            is =
                (==)

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
        ( lair, [] )


stepHouse : House -> ( House, List Event )
stepHouse house =
    ( house, [] )


stepMonster : Monster -> ( Monster, List Event )
stepMonster monster =
    ( monster, [] )


stepBullet : Bullet -> ( Bullet, List Event )
stepBullet bullet =
    ( bullet, [] )



-- View


view : Computer -> Game -> List Shape
view computer game =
    []
