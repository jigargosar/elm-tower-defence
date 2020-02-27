module BombId exposing (BombId, generator)

import Sequential


type BombId
    = BombId Int


generator : Sequential.Generator BombId
generator =
    Sequential.int |> Sequential.map BombId
