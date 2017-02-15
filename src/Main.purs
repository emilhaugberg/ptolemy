module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Ref (REF, modifyRef, newRef, readRef)
import Control.Monad.Eff.Timer
import Data.Maybe (Maybe(..))
import Graphics.Canvas
import Graphics.Draw
import Math as Math
import Prelude
import Types (Angle, Coordinate, Circle, Triangle, Radius, CanvasEff)

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

startingAngle :: Angle
startingAngle = 0.0

speed :: Number
speed = Math.pi / 50.0

main :: forall e. (Partial) => Eff (ref :: REF, timer :: TIMER, canvas :: CANVAS | e) Unit
main = void do
  Just canvas <- getCanvasElementById "canvas"
  ctx         <- getContext2D canvas

  angle       <- newRef startingAngle

  setInterval 50 $ void do
    clearRect ctx {x: 0.0, y: 0.0, w: width, h: height}

    modifyRef angle ((+) speed)

    angle' <- readRef angle

    drawCircle circle ctx
    drawTriangle triangle ctx
    drawDot (coordOnCircle angle') ctx
