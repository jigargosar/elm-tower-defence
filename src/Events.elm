module Events exposing (Mem)


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


type Lair
    = Lair


type House
    = House


type alias Mem =
    { lair : Lair
    , towers : List Tower
    , bullets : List Bullet
    , monsters : List Monster
    , house : House
    }


type Event
    = NoEvent
    | BulletHitMonster MonsterId
    | RemoveBullet BulletId
    | RemoveMonster MonsterId
    | MonsterReachedHouse


update : Mem -> Mem
update mem =
    let
        ( selfUpdatedLair, lairEvents ) =
            stepLair mem.lair

        ( selfUpdatedHouse, houseEvents ) =
            stepHouse mem.house

        ( selfUpdatedTowers, towerEventGroups ) =
            List.map stepTower mem.towers
                |> List.unzip

        ( selfUpdatedBullets, bulletEventGroups ) =
            List.map stepBullet mem.bullets
                |> List.unzip

        ( selfUpdatedMonsters, monsterEventGroups ) =
            List.map stepMonster mem.monsters
                |> List.unzip

        acc : Mem
        acc =
            { lair = selfUpdatedLair
            , house = selfUpdatedHouse
            , towers = selfUpdatedTowers
            , bullets = selfUpdatedBullets
            , monsters = selfUpdatedMonsters
            }
    in
    acc
        |> handleEvents mem lairEvents
        |> handleEvents mem houseEvents
        |> handleEventGroups mem towerEventGroups
        |> handleEventGroups mem bulletEventGroups
        |> handleEventGroups mem monsterEventGroups


handleEventGroups : Mem -> List (List Event) -> Mem -> Mem
handleEventGroups mem lists acc =
    List.foldl (handleEvents mem) acc lists


handleEvents : Mem -> List Event -> Mem -> Mem
handleEvents mem events acc =
    List.foldl (handleEvent mem) acc events


handleEvent : Mem -> Event -> Mem -> Mem
handleEvent mem event acc =
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


stepTower : Tower -> ( Tower, List Event )
stepTower tower =
    ( tower, [] )


stepLair : Lair -> ( Lair, List Event )
stepLair lair =
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
