module Main where

import Config.Main (circle, coordFromAngle, height, speed, startingAngle, triangle, width)
import Config.Types (Angle)
import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Ref (REF, modifyRef, newRef, readRef)
import Control.Monad.Eff.Timer (TIMER, setInterval)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (CANVAS, Context2D, clearRect, getCanvasElementById, getContext2D)
import Graphics.Draw (drawCircle, drawDot, drawTriangle, drawLinesToDot, drawLength)
import Prelude (Unit, bind, void, ($), (+))

draw :: forall e. Context2D -> Angle -> Eff (canvas :: CANVAS | e) Unit
draw ctx ang = void do
  drawCircle circle ctx
  drawTriangle triangle ctx
  drawDot (coordFromAngle ang) ctx
  drawLength (coordFromAngle ang) triangle ctx

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
    draw ctx angle'
    foreachE triangleDots \tp -> void $ drawLinesToDot (coordFromAngle angle') tp ctx
