module Graphics.Draw where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Config.Types (CanvasEff, Coordinate, Triangle, TrianglePoint, Angle)
import Config.Main (parseInt, distBetweenPoints, coordFromAngle, greatestDistance, showCoordinate)
import Data.Array (filter, (!!))
import Data.Maybe (Maybe(..), fromJust)
import Graphics.Canvas (Arc, Context2D, CANVAS, arc, beginPath, closePath, fill, fillText, lineTo, moveTo, setFillStyle, setStrokeStyle, stroke, setLineWidth)
import Prelude (Unit, bind, show, pure, unit, (*), (/=), ($), (/), (+))
import Math as Math

drawCircle :: Arc -> Context2D -> CanvasEff Context2D
drawCircle circle ctx = do
  beginPath ctx
  arc       ctx circle
  setStrokeStyle "#d1d1d1" ctx
  stroke    ctx
  closePath ctx

drawTriangle :: Triangle -> Context2D -> CanvasEff Context2D
drawTriangle tr ctx = do
  beginPath ctx
  moveTo    ctx tr.a.coord.x tr.a.coord.y
  lineTo    ctx tr.b.coord.x tr.b.coord.y
  lineTo    ctx tr.c.coord.x tr.c.coord.y
  lineTo    ctx tr.a.coord.x tr.a.coord.y
  setStrokeStyle "#d1d1d1" ctx
  stroke    ctx
  closePath ctx

drawDot :: Coordinate -> Context2D -> CanvasEff Context2D
drawDot c ctx = do
  beginPath ctx
  arc ctx arc'
  setFillStyle "black" ctx
  fill ctx
  closePath ctx
  where
    arc' = { x: c.x, y: c.y, r: 5.0, start: 0.0, end: Math.pi * 2.0 }

drawLinesToDot :: Coordinate -> TrianglePoint -> Context2D -> CanvasEff Context2D
drawLinesToDot dot tr ctx = do
  beginPath      ctx
  moveTo         ctx tr.coord.x tr.coord.y
  lineTo         ctx dot.x dot.y
  setStrokeStyle (show tr.color) ctx
  stroke         ctx
  closePath      ctx

drawLength :: Coordinate -> Triangle -> Context2D -> CanvasEff Context2D
drawLength c t ctx = do
  beginPath ctx
  fillText ctx (show (parseInt (distBetweenPoints {x: t.a.coord.x, y: t.a.coord.y} c))) 900.0 500.0
  fillText ctx (show (parseInt (distBetweenPoints {x: t.b.coord.x, y: t.b.coord.y} c))) 900.0 550.0
  fillText ctx (show (parseInt (distBetweenPoints {x: t.c.coord.x, y: t.c.coord.y} c))) 900.0 600.0
  closePath ctx

drawProgress :: forall e. (Partial) => Array TrianglePoint -> Angle -> Context2D -> Eff (canvas :: CANVAS | e) Context2D
drawProgress xs a ctx = do
  let largest = greatestDistance xs (coordFromAngle a)
  let smaller = filter (\x -> x.color /= largest.color) xs

  let smaller1 = fromJust $ smaller !! 0
  let smaller2 = fromJust $ smaller !! 1

  let smaller1'      = distBetweenPoints smaller1.coord (coordFromAngle a)
  let smaller2'      = distBetweenPoints smaller2.coord (coordFromAngle a)
  let smallerTotal   = smaller1' + smaller2'
  let lengthSmaller1 = 300.0 * (smaller1' / smallerTotal)
  let lengthSmaller2 = 300.0 * (smaller2' / smallerTotal)

  beginPath ctx
  moveTo ctx 900.0 300.0
  lineTo ctx 900.0 600.0
  setLineWidth 5.0 ctx
  setStrokeStyle (show largest.color) ctx
  stroke ctx
  closePath ctx

  beginPath ctx
  moveTo ctx 925.0 300.0
  lineTo ctx 925.0 (300.0 + lengthSmaller1)
  setLineWidth 5.0 ctx
  setStrokeStyle (show smaller1.color) ctx
  stroke ctx
  closePath ctx

  beginPath ctx
  moveTo ctx 925.0 (300.0 + lengthSmaller1)
  lineTo ctx 925.0 ((300.0 + lengthSmaller1) + lengthSmaller2)
  setLineWidth 5.0 ctx
  setStrokeStyle (show smaller2.color) ctx
  stroke ctx
  closePath ctx
