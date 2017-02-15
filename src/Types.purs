module Types where

import Control.Monad.Eff (Eff)
import Graphics.Canvas (CANVAS, Arc)

type CanvasEff a = forall e. Eff (canvas :: CANVAS | e) a

type Triangle   = { a :: Coordinate, b :: Coordinate, c :: Coordinate}
type Coordinate = { x :: Number, y :: Number }
type Circle     = Arc
type Radius     = Number
type Angle      = Number
