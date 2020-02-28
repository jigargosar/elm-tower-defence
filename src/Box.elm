module Box exposing (Box, init, shiftX, shiftY)

import Location as L exposing (Location)
import Playground exposing (..)


type alias Box =
    { location : Location
    , width : Number
    , height : Number
    }


init : Number -> Number -> Box
init w h =
    Box L.origin w h


shiftX : Number -> Box -> Box
shiftX =
    L.shiftX >> mapLocation


shiftY : Number -> Box -> Box
shiftY =
    L.shiftY >> mapLocation


mapLocation : (Location -> Location) -> Box -> Box
mapLocation func box =
    { box | location = func box.location }
