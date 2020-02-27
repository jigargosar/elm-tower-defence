module BombTower exposing (..)

import BombTowerId
import List.Extra
import Location as L exposing (Location)
import Playground exposing (..)
import Sequential


type alias BombTower =
    { -- Meta
      delay : Number
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


generator : Init -> Sequential.Generator BombTower
generator init =
    BombTowerId.generator
        |> Sequential.map
            (\tid ->
                initBombTower init
            )


initBombTower : Init -> BombTower
initBombTower { location, range, reloadDelay, viewWidth } =
    { delay = reloadDelay
    , range = range
    , viewWidth = viewWidth
    , location = location
    , elapsed = 0
    }



-- NOTE: Trying to build isolated stepBombTower function,
-- i.e. minimizing its dependency on other entities in game.
-- Caution: Be ready to revert if its no longer fun.


stepBombTower :
    { spawnBomb : { from : Location, to : Location } -> event }
    -> List Location
    -> BombTower
    -> ( BombTower, List event )
stepBombTower config targetLocations tower =
    if tower.elapsed >= tower.delay then
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
