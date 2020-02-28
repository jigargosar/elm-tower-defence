module Box exposing (Box, contains, horizontalLayout, init, initAt, moveShape, shape, shiftX, shiftXByWidthF, shiftY, shiftYByHeightF)

import Location as L exposing (Location)
import Playground exposing (..)


type alias Box =
    { location : Location
    , width : Number
    , height : Number
    }


init : Number -> Number -> Box
init w h =
    initAt L.origin w h


initAt : Location -> Number -> Number -> Box
initAt l w h =
    Box l w h


shiftX : Number -> Box -> Box
shiftX =
    L.shiftX >> mapLocation


shiftY : Number -> Box -> Box
shiftY =
    L.shiftY >> mapLocation


shiftYByHeightF : (Number -> Number) -> Box -> Box
shiftYByHeightF func box =
    shiftY (func box.height) box


shiftXByWidthF : (Number -> Number) -> Box -> Box
shiftXByWidthF func box =
    shiftX (func box.width) box


width : Box -> Number
width =
    .width


mapLocation : (Location -> Location) -> Box -> Box
mapLocation func box =
    { box | location = func box.location }


contains : Location -> Box -> Bool
contains location box =
    L.isLocationInRectangleAt box.location box.width box.height location


shape : Color -> Box -> Shape
shape c box =
    rectangle c box.width box.height


moveShape : Box -> Shape -> Shape
moveShape box =
    L.moveShape box.location


horizontalLayout : Number -> List Box -> List Box
horizontalLayout gap list =
    let
        listWidth =
            list |> List.map width |> List.sum

        gapsWidth =
            list |> List.length |> (+) -1 |> max 0 |> toFloat |> (*) gap

        listWidthWithGaps =
            listWidth + gapsWidth

        firstLeft =
            -listWidthWithGaps / 2

        func : Box -> ( Float, List Box ) -> ( Float, List Box )
        func box ( left, nl ) =
            let
                nb =
                    box
                        |> mapLocation (L.shiftX (left + (width box / 2)))
            in
            ( left + width box + gap, nb :: nl )

        ret =
            List.foldl func ( firstLeft, [] ) list
                |> Tuple.second
                |> List.reverse

        _ =
            Debug.log "debug" ( ( firstLeft, listWidthWithGaps ), ( gapsWidth, gap ), ret )
    in
    ret
