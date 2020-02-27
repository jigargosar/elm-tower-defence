module Bomb exposing
    ( Bomb
    , BombId
    , idOfBomb
    , initBomb
    , stepBomb
    , viewBomb
    )

import Location as L exposing (Location)
import Playground exposing (..)


type BombId
    = BombId Int


type BombState
    = BombInFlight
    | Exploding Number
    | BombWaitingToBeRemoved


type alias Bomb =
    { id : BombId
    , aoe : Number
    , damage : Number
    , location : Location
    , speed : Number
    , target : Location
    , explosionTicks : Number
    , state : BombState
    }


type alias BombInit =
    { location : Location
    , target : Location
    , aoe : Number
    , speed : Number
    }


initBomb : Int -> BombInit -> Bomb
initBomb idx { location, target, aoe, speed } =
    { id = BombId idx
    , aoe = aoe
    , damage = 3
    , location = location
    , target = target
    , speed = speed
    , explosionTicks = 60
    , state = BombInFlight
    }


idOfBomb : Bomb -> BombId
idOfBomb bomb =
    bomb.id


stepBomb :
    { remove : BombId -> event
    , reachedTarget : { at : Location, aoe : Number, damage : Number } -> event
    }
    -> Bomb
    -> ( Bomb, List event )
stepBomb config bomb =
    case bomb.state of
        BombInFlight ->
            case L.stepLocationTowards bomb.target bomb.speed bomb.location of
                Nothing ->
                    ( { bomb | state = Exploding 0 }
                    , [ config.reachedTarget
                            { at = bomb.target
                            , aoe = bomb.aoe
                            , damage = bomb.damage
                            }
                      ]
                    )

                Just newLocation ->
                    ( { bomb | location = newLocation }, [] )

        Exploding elapsed ->
            if elapsed >= bomb.explosionTicks then
                ( { bomb | state = BombWaitingToBeRemoved }
                , [ config.remove bomb.id ]
                )

            else
                ( { bomb | state = Exploding (elapsed + 1) }, [] )

        BombWaitingToBeRemoved ->
            ( bomb, [] )


viewBomb : Bomb -> Shape
viewBomb bomb =
    let
        location =
            bomb.location
    in
    case bomb.state of
        BombInFlight ->
            circle brown 5
                |> L.moveShape location

        Exploding elapsed ->
            let
                progress =
                    elapsed / bomb.explosionTicks

                remainingProgress =
                    1 - progress
            in
            circle brown bomb.aoe
                |> fade (remainingProgress / 2)
                |> L.moveShape location

        BombWaitingToBeRemoved ->
            group []
