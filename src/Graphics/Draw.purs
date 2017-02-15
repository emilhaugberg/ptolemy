module Graphics.Draw where

import Graphics.Canvas
import Types (Coordinate, CanvasEff, Triangle)
import Prelude
import Math as Math

drawCircle :: Arc -> Context2D -> CanvasEff Context2D
drawCircle circle ctx = do
  beginPath ctx
  arc       ctx circle
  stroke    ctx
  closePath ctx

drawTriangle :: Triangle -> Context2D -> CanvasEff Context2D
drawTriangle triangle ctx = do
  beginPath ctx
  moveTo    ctx triangle.a.x triangle.a.y
  lineTo    ctx triangle.b.x triangle.b.y
  lineTo    ctx triangle.c.x triangle.c.y
  lineTo    ctx triangle.a.x triangle.a.y
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
