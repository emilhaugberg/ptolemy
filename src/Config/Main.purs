module Config.Main (
    showCoordinate,
    width,
    height,
    radius,
    circle,
    triangle,
    startingAngle,
    speed,
    coordFromAngle,
    distBetweenPoints,
    greatestDistance,
    parseInt
  ) where

import Config.Types
import Data.Foldable
import Data.Ordering (Ordering(..))
import Data.Maybe (Maybe(..), fromJust)
import Math (pi, sin, cos, sqrt, abs) as Math
import Prelude ((*), (/), (+), (-), ($), (>), (<>), show)

width :: Number
width  = 1000.0

height :: Number
height = 1000.0

radius :: Radius
radius = 300.0

circle :: Circle
circle =
  { x    : 500.0
  , y    : 500.0
  , r    : 300.0
  , start: 0.0
  , end  : Math.pi * 2.0
  }

triangle :: Triangle
triangle =
  { a: { coord: coordFromAngle angle1, color: Blue }
  , b: { coord: coordFromAngle angle2, color: Red }
  , c: { coord: coordFromAngle angle3, color: Green }
  }
  where
    angle1         = startingAngle'
    angle2         = startingAngle' * (2.0 / 3.0)
    angle3         = startingAngle' * (1.0 / 3.0)
    startingAngle' = 2.0 * Math.pi

startingAngle :: Angle
startingAngle = 0.0

speed :: Number
speed = Math.pi / 50.0

coordFromAngle :: Angle -> Coordinate
coordFromAngle angle = { x, y }
  where
    x = radius * (Math.cos angle) + 500.0
    y = radius * (Math.sin angle) + 500.0

distBetweenPoints :: Coordinate -> Coordinate -> Number
distBetweenPoints c1 c2 = Math.sqrt $ distance c1 c2
  where
    distance c1' c2' = (square (Math.abs (c1'.x - c2'.x)) + square (Math.abs (c1'.y - c2'.y)))
    square x         = x * x

greatestDistance :: Partial => Array TrianglePoint -> Coordinate -> TrianglePoint
greatestDistance xs c = fromJust $ maximumBy greatest xs
  where
    greatest x y = if (dist x.coord) > (dist y.coord) then GT else LT
    dist x       = distBetweenPoints x c

showCoordinate :: TrianglePoint -> String
showCoordinate tr = "x " <> show tr.coord.x <> "y " <> show tr.coord.y <> "color: " <> show tr.color

foreign import parseInt :: Number -> Int
