module Graphics.Draw where

import Config.Types (CanvasEff, Coordinate, Triangle, TrianglePoint)
import Config.Main (parseInt, distBetweenPoints)
import Graphics.Canvas (Arc, Context2D, arc, beginPath, closePath, fill, fillText, lineTo, moveTo, setFillStyle, setStrokeStyle, stroke)
import Prelude (bind, show, (*))
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
