module BombTower exposing (BombTower, Init, generator, id, stepBombTower, viewBombTower)

import BombTowerId exposing (BombTowerId)
import List.Extra
import Location as L exposing (Location)
import Playground exposing (..)
import Random exposing (Generator)


type alias BombTower =
    { -- Meta
      id : BombTowerId
    , delay : Number
    , location : Location
    , range : Number
    , viewWidth : Number

    -- State
    , elapsed : Number
    }


type alias Init =
    { location : Location
    , range : Number
    , reloadDelay : Number
    , viewWidth : Number
    }


generator : Init -> Generator BombTower
generator init =
    BombTowerId.generator
        |> Random.map (initBombTower init)


initBombTower : Init -> BombTowerId -> BombTower
initBombTower { location, range, reloadDelay, viewWidth } tid =
    { id = tid
    , delay = reloadDelay
    , range = range
    , viewWidth = viewWidth
    , location = location
    , elapsed = 0
    }


id : BombTower -> BombTowerId
id =
    .id


stepBombTower :
    { spawnBomb : { from : Location, to : Location } -> event
    , replaceTower : BombTowerId -> event
    }
    -> Mouse
    -> List Location
    -> BombTower
    -> ( BombTower, List event )
stepBombTower config mouse targetLocations tower =
    let
        isClickedOnTower =
            L.isLocationInSquareAt tower.location tower.viewWidth (L.at mouse.x mouse.y)
                && mouse.click
    in
    if isClickedOnTower then
        ( tower, [ config.replaceTower tower.id ] )

    else if tower.elapsed >= tower.delay then
        case
            List.Extra.find
                (\targetLocation ->
                    L.distanceFromTo tower.location targetLocation <= tower.range
                )
                targetLocations
        of
            Just to ->
                ( { tower | elapsed = 0 }
                , [ config.spawnBomb { from = tower.location, to = to } ]
                )

            Nothing ->
                ( tower, [] )

    else
        ( { tower | elapsed = tower.elapsed + 1 }, [] )


viewBombTower : BombTower -> Shape
viewBombTower tower =
    [ circle lightBrown tower.range |> fade 0.4
    , square brown tower.viewWidth
    ]
        |> group
        |> L.moveShape tower.location
