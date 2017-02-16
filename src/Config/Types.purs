module Config.Types where

import Control.Monad.Eff (Eff)
import Graphics.Canvas (CANVAS, Arc)
import Prelude (class Show, class Eq)

type CanvasEff a = forall e. Eff (canvas :: CANVAS | e) a

type Triangle = { a :: TrianglePoint, b :: TrianglePoint, c :: TrianglePoint }

type TrianglePoint = { coord :: Coordinate, color :: Color }
type Coordinate    = { x :: Number, y :: Number }
type Circle        = Arc
type Radius        = Number
type Angle         = Number

data Color = Red | Blue | Green

instance showColor :: Show Color where
  show Red   = "red"
  show Blue  = "blue"
  show Green = "green"

instance eqColor :: Eq Color where
  eq Red   Red   = true
  eq Blue  Blue  = true
  eq Green Green = true
  eq _     _     = false
