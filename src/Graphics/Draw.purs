module Graphics.Draw where

import Graphics.Canvas
import Types (Coordinate, CanvasEff, Triangle, Color(..))
import Prelude
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
