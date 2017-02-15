module Main where

import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Ref (REF, modifyRef, newRef, readRef)
import Control.Monad.Eff.Timer
import Data.Maybe (Maybe(..))
import Graphics.Canvas
import Graphics.Draw
import Math as Math
import Prelude
import Types (Angle, Coordinate, Circle, Triangle, Radius, CanvasEff, TrianglePoint, Color(..))

colors :: { a :: Color, b :: Color, c :: Color }
colors =
  { a: Red
  , b: Green
  , c: Blue
  }

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
  { a: { coord: coordOnCircle startingPoint, color: Blue }
  , b: { coord: coordOnCircle (startingPoint * (2.0 / 3.0)), color: Red }
  , c: { coord: coordOnCircle (startingPoint * (1.0 / 3.0)), color: Green }
  }
  where
    startingPoint = 2.0 * Math.pi

coordOnCircle :: Angle -> Coordinate
coordOnCircle angle = { x, y }
  where
    x = radius * (Math.cos angle) + 500.0
    y = radius * (Math.sin angle) + 500.0

linesToDot :: Coordinate -> TrianglePoint -> Context2D -> CanvasEff Context2D
linesToDot dot tr ctx = do
  beginPath      ctx
  moveTo         ctx tr.coord.x tr.coord.y
  lineTo         ctx dot.x dot.y
  setStrokeStyle (show tr.color) ctx
  stroke         ctx
  closePath      ctx

startingAngle :: Angle
startingAngle = 0.0

speed :: Number
speed = Math.pi / 50.0

distanceBetweenPoints :: Coordinate -> Coordinate -> Number
distanceBetweenPoints c1 c2 =
  Math.sqrt (square (Math.abs (c1.x - c2.x)) + square (Math.abs (c1.y - c2.y)))
  where
    square x = x * x

drawLength :: Coordinate -> Triangle -> Context2D -> CanvasEff Context2D
drawLength c t ctx = do
  beginPath ctx
  fillText ctx (show (parseInt (distanceBetweenPoints {x: t.a.coord.x, y: t.a.coord.y} c))) 900.0 500.0
  fillText ctx (show (parseInt (distanceBetweenPoints {x: t.b.coord.x, y: t.b.coord.y} c))) 900.0 550.0
  fillText ctx (show (parseInt (distanceBetweenPoints {x: t.c.coord.x, y: t.c.coord.y} c))) 900.0 600.0
  closePath ctx

main :: forall e. (Partial) => Eff (ref :: REF, timer :: TIMER, canvas :: CANVAS | e) Unit
main = void do
  Just canvas <- getCanvasElementById "canvas"
  ctx         <- getContext2D canvas
  angle       <- newRef startingAngle

  let triangleDots = [triangle.a, triangle.b, triangle.c]

  setInterval 50 $ void do
    clearRect ctx {x: 0.0, y: 0.0, w: width, h: height}
    modifyRef angle ((+) speed)
    angle' <- readRef angle
    drawCircle circle ctx
    drawTriangle triangle ctx
    drawDot (coordOnCircle angle') ctx
    drawLength (coordOnCircle angle') triangle ctx

    foreachE triangleDots \tp -> void $ linesToDot (coordOnCircle angle') tp ctx

foreign import parseInt :: Number -> Int
