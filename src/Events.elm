module Events exposing (Mem)


type Bullet
    = Bullet


type Monster
    = Monster


type Tower
    = Tower


type Lair
    = Lair


type alias Mem =
    { lair : Lair
    , towers : List Tower
    , bullets : List Bullet
    , monsters : List Monster
    }


type Event
    = Event


update : Mem -> Mem
update mem =
    let
        ( selfUpdatedLair, lairEvents ) =
            stepLair mem.lair

        ( selfUpdatedTowers, towerEventGroups ) =
            List.map stepTower mem.towers
                |> List.unzip

        ( selfUpdatedBullets, bulletEventGroups ) =
            List.map stepBullet mem.bullets
                |> List.unzip

        ( selfUpdatedMonsters, monsterEventGroups ) =
            List.map stepMonster mem.monsters
                |> List.unzip
    in
    { mem
        | lair = selfUpdatedLair
        , towers = selfUpdatedTowers
        , bullets = selfUpdatedBullets
        , monsters = selfUpdatedMonsters
    }
        |> handleEvents lairEvents
        |> handleEventGroups towerEventGroups
        |> handleEventGroups bulletEventGroups
        |> handleEventGroups monsterEventGroups


handleEventGroups : List (List Event) -> Mem -> Mem
handleEventGroups lists mem =
    List.foldl handleEvents mem lists


handleEvents : List Event -> Mem -> Mem
handleEvents events mem =
    List.foldl handleEvent mem events


handleEvent : Event -> Mem -> Mem
handleEvent event mem =
    case event of
        Event ->
            mem


stepTower : Tower -> ( Tower, List Event )
stepTower tower =
    ( tower, [] )


stepLair : Lair -> ( Lair, List Event )
stepLair lair =
    ( lair, [] )


stepMonster : Monster -> ( Monster, List Event )
stepMonster monster =
    ( monster, [] )


stepBullet : Bullet -> ( Bullet, List Event )
stepBullet bullet =
    ( bullet, [] )
