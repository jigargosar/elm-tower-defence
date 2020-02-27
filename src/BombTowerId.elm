module BombTowerId exposing (BombTowerId, generator)

import Random


type BombTowerId
    = BombTowerId Int


generator : Random.Generator BombTowerId
generator =
    Random.int 0 Random.maxInt |> Random.map BombTowerId
