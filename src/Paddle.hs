module Paddle where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color

import Types

initPaddle :: Float -> Paddle
initPaddle w = Paddle
    { pX      = w / 2
    , pY      = 32
    , pDX     = 0
    , pDDX    = 0
    , pLength = 64
    , pHeight = 8
    }

updatePaddle :: Paddle -> Paddle
updatePaddle paddle =
    let newDx = ddx + if (x > (800 - hl) && dx > 0) || (x < hl && dx < 0) then 0.8 * (-dx) else dx
    in  paddle
        { pX  = x + newDx
        , pDX = 0.9 * newDx
        }
    where x   = pX paddle
          hl  = pLength paddle / 2
          dx  = pDX paddle
          ddx = pDDX paddle

addPaddleAccel :: Float -> Paddle -> Paddle
addPaddleAccel acc paddle = paddle { pDDX = (if acc > 0 then min else max ) acc $ pDDX paddle + acc }

drawPaddle :: Paddle -> Picture
drawPaddle paddle = color (greyN 0.6) $ translate x y $ polygon corners
    where w       = pLength paddle
          h       = pHeight paddle
          x       = pX paddle - (w/2)
          y       = pY paddle - (h/2)
          corners = [(0, 0), (w, 0), (w, h), (0, h)]
