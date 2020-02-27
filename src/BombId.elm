module BombId exposing (BombId, generator)

import Random


type BombId
    = BombId Int


generator : Random.Generator BombId
generator =
    Random.int 0 Random.maxInt |> Random.map BombId
