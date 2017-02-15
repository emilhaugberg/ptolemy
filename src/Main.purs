module Main where

import Config.Main (circle, coordFromAngle, height, speed, startingAngle, triangle, width)
import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Ref (REF, modifyRef, newRef, readRef)
import Control.Monad.Eff.Timer (TIMER, setInterval)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (CANVAS, clearRect, getCanvasElementById, getContext2D)
import Graphics.Draw (drawCircle, drawDot, drawTriangle, drawLinesToDot, drawLength)
import Prelude (Unit, bind, void, ($), (+))

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
    drawDot (coordFromAngle angle') ctx
    drawLength (coordFromAngle angle') triangle ctx

    foreachE triangleDots \tp -> void $ drawLinesToDot (coordFromAngle angle') tp ctx
