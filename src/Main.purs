module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Maybe (Maybe(..))
import Graphics.Canvas
import Graphics.Draw
import Math as Math
import Prelude
import Types (Angle, Coordinate, Circle, Triangle, Radius, CanvasEff)

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
  { a: coordOnCircle startingPoint
  , b: coordOnCircle (startingPoint * (2.0 / 3.0))
  , c: coordOnCircle (startingPoint * (1.0 / 3.0))
  }
  where
    startingPoint = 2.0 * Math.pi

coordOnCircle :: Angle -> Coordinate
coordOnCircle angle = { x, y }
  where
    x = radius * (Math.cos angle) + 500.0
    y = radius * (Math.sin angle) + 500.0

main :: forall e. (Partial) => CanvasEff Context2D
main = do
  Just canvas <- getCanvasElementById "canvas"
  ctx         <- getContext2D canvas
  drawCircle circle ctx
  drawTriangle triangle ctx
