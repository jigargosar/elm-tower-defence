module Location exposing
    ( Location
    , at
    , distanceFromTo
    , isLocationInRangeOf
    , moveShape
    , shiftX
    , shiftY
    , stepLocationTowards
    )

import Playground exposing (..)


type Location
    = Location Number Number


at : Number -> Number -> Location
at x y =
    Location x y


stepLocationTowards : Location -> Number -> Location -> Maybe Location
stepLocationTowards target speed location =
    let
        (Location tx ty) =
            target

        (Location x y) =
            location

        angle =
            atan2 (ty - y) (tx - x)

        ( dx, dy ) =
            ( speed, angle ) |> fromPolar

        ( nx, ny ) =
            ( x + dx, y + dy )

        newLocation =
            Location nx ny
    in
    if locationEqWithin speed newLocation target then
        Nothing

    else
        Just newLocation


locationEqWithin : Number -> Location -> Location -> Bool
locationEqWithin tol l1 l2 =
    distanceFromTo l1 l2 <= tol + 0.1


distanceFromTo : Location -> Location -> Number
distanceFromTo (Location x1 y1) (Location x2 y2) =
    let
        ( x, y ) =
            ( x2 - x1, y2 - y1 )
    in
    sqrt (add (mul x x) (mul y y))


isLocationInRangeOf : Location -> Number -> Location -> Bool
isLocationInRangeOf center range location =
    distanceFromTo center location <= range


moveShape : Location -> Shape -> Shape
moveShape =
    applyLocationTo move


shiftY : Number -> Location -> Location
shiftY offset (Location x y) =
    Location x (y + offset)


shiftX : Number -> Location -> Location
shiftX offset (Location x y) =
    Location (x + offset) y


applyLocationTo : (Number -> Number -> a) -> Location -> a
applyLocationTo func (Location x y) =
    func x y


add =
    (+)


mul =
    (*)
