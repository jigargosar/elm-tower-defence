module Main exposing (main)

import Playground exposing (..)



-- Game Scaffold


type alias Mem =
    {}


init : Mem
init =
    {}


update : Computer -> Mem -> Mem
update computer mem =
    mem


view : Computer -> Mem -> List Shape
view computer mem =
    [ words black "Welcome to Adventure" ]


main =
    game view update init
