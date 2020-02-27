module BombTowerId exposing (BombTowerId, generator)

import Sequential


type BombTowerId
    = BombTowerId Int


generator : Sequential.Generator BombTowerId
generator =
    Sequential.int |> Sequential.map BombTowerId
