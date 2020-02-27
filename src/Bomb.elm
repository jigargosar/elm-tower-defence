module Bomb exposing
    ( Bomb
    , generator
    , id
    , stepBomb
    , viewBomb
    )

import BombId exposing (BombId)
import Location as L exposing (Location)
import Playground exposing (..)
import Sequential


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


generator : BombInit -> Sequential.Generator Bomb
generator bombInit =
    BombId.generator
        |> Sequential.map
            (\bid ->
                let
                    { location, target, aoe, speed } =
                        bombInit
                in
                { id = bid
                , aoe = aoe
                , damage = 3
                , location = location
                , target = target
                , speed = speed
                , explosionTicks = 60
                , state = BombInFlight
                }
            )


id : Bomb -> BombId
id =
    .id


stepBomb :
    { remove : BombId -> event
    , exploded : { at : Location, aoe : Number, damage : Number } -> event
    }
    -> Bomb
    -> ( Bomb, List event )
stepBomb config bomb =
    case bomb.state of
        BombInFlight ->
            case L.stepLocationTowards bomb.target bomb.speed bomb.location of
                Nothing ->
                    ( { bomb | state = Exploding 0 }
                    , [ config.exploded
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
