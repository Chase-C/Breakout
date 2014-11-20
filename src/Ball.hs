module Ball where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color

import Types

initBall :: Ball
initBall = Ball
    { bX      = 400
    , bY      = 300
    , bDX     = 0
    , bDY     = -256
    , bRadius = 4
    }

updateBall :: Time -> Ball -> Collision -> Ball
updateBall dt ball collision =
    let (newDx, newDy) =
            case collision of
                NoCollision ->
                    ( if (x > (800 - r) && dx > 0) || (x < r && dx < 0) then -dx else  dx
                    , if (y > (600 - r) && dy > 0) || (y < r && dy < 0) then -dy else  dy
                    )
                _ -> if cVertical collision then (-dx, dy) else (cVelMod collision + dx, -dy)
    in  ball
        { bX  = x + (dt * newDx)
        , bY  = y + (dt * newDy)
        , bDX = newDx
        , bDY = newDy
        }
    where x = bX ball
          y = bY ball
          r = bRadius ball
          dx = bDX ball
          dy = bDY ball

drawBall :: Ball -> Picture
drawBall ball = color (greyN 0.6) $ translate x y $ thickCircle r 2
    where r = bRadius ball
          x = bX ball - (r/2)
          y = bY ball - (r/2)
