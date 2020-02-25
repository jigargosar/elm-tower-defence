module Events exposing (Game, init, update, view)

import List.Extra
import Playground exposing (..)
import Random exposing (Seed, initialSeed)
import String exposing (fromFloat, fromInt)


type alias Bullet =
    { id : BulletId
    , monsterId : MonsterId
    , elapsed : Number
    , ticksToHit : Number
    }


initBullet : Int -> MonsterId -> Bullet
initBullet idx monsterId =
    Bullet (BulletId idx) monsterId 0 10


idOfBullet : Bullet -> BulletId
idOfBullet bullet =
    bullet.id


type BulletId
    = BulletId Int


type alias Monster =
    { id : MonsterId
    , health : Number
    , maxHealth : Number
    }


initMonster : Int -> Monster
initMonster idx =
    let
        maxHealth =
            10
    in
    Monster (MonsterId idx) maxHealth maxHealth


decrementMonsterHealth : Monster -> Monster
decrementMonsterHealth monster =
    { monster | health = max 0 (monster.health - 1) }


idOfMonster : Monster -> MonsterId
idOfMonster =
    .id


type MonsterId
    = MonsterId Int


type alias Tower =
    { delay : Number, elapsed : Number }


initTower : Int -> Tower
initTower _ =
    Tower 10 0


type alias Lair =
    { seed : Seed, delay : Number, elapsed : Number }


initLair : Lair
initLair =
    Lair (initialSeed 0) 100 0


type House
    = House


type Game
    = Running World
    | GameOver World


init : Game
init =
    Running
        { lair = initLair
        , towers = List.range 0 3 |> List.map initTower
        , bullets = []
        , monsters = []
        , house = House
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


isNot =
    (/=)


handleEvent : World -> Event -> World -> World
handleEvent world event acc =
    case event of
        NoEvent ->
            acc

        BulletHitMonster monsterId ->
            { acc
                | monsters =
                    -- List.filter (idOfMonster >> isNot monsterId) acc.monsters
                    List.Extra.updateIf (idOfMonster >> isNot monsterId) decrementMonsterHealth acc.monsters
            }

        RemoveBullet bulletId ->
            { acc | bullets = List.filter (idOfBullet >> isNot bulletId) acc.bullets }

        RemoveMonster monsterId ->
            { acc | monsters = List.filter (idOfMonster >> isNot monsterId) acc.monsters }

        MonsterReachedHouse ->
            acc

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
        ( { lair | elapsed = lair.elapsed + 1 }, [] )


stepHouse : House -> ( House, List Event )
stepHouse house =
    ( house, [] )


stepMonster : Monster -> ( Monster, List Event )
stepMonster monster =
    if monster.health <= 0 then
        ( monster, [ RemoveMonster monster.id ] )

    else
        ( monster, [] )


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
            [ [ words blue "Game Running" |> scale 3
              , words black ("Monster Count: " ++ fromInt (List.length world.monsters))
                    |> scale 2
                    |> moveDown 50
              , words black ("Tower Count: " ++ fromInt (List.length world.towers))
                    |> scale 2
                    |> moveDown 100
              , words black ("Bullet Count: " ++ fromInt (List.length world.bullets))
                    |> scale 2
                    |> moveDown 150
              ]
                |> group
                |> moveY computer.screen.top
                |> moveDown 50
            ]

        GameOver world ->
            [ words red "GAME OVER" |> scale 3 ]
