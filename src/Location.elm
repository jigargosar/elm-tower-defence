module Location exposing
    ( Location
    , at
    , distanceFromTo
    , isLocationInRangeOf
    , isLocationInRectangleAt
    , isLocationInSquareAt
    , moveShape
    , ofMouse
    , origin
    , shiftX
    , shiftY
    , stepLocationTowards
    )

import Playground exposing (..)


type Location
    = Location Number Number


origin : Location
origin =
    at 0 0


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


isLocationInSquareAt : Location -> Number -> Location -> Bool
isLocationInSquareAt center squareSide location =
    isLocationInRectangleAt center squareSide squareSide location


isLocationInRectangleAt : Location -> Number -> Number -> Location -> Bool
isLocationInRectangleAt (Location cx cy) w h (Location x y) =
    let
        minX =
            cx - w / 2

        minY =
            cy - h / 2

        maxX =
            cx + w / 2

        maxY =
            cy + h / 2
    in
    (x < minX || x > maxX || y < minY || y > maxY)
        |> not


ofMouse : Mouse -> Location
ofMouse { x, y } =
    at x y


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
