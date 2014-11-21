module Collision where

import Types

checkPaddleCollision :: Paddle -> Ball -> Collision
checkPaddleCollision paddle ball
    | collision = Collision
                  { cBrick    = Nothing
                  , cVertical = False
                  , cVelMod   = dx / 2
                  }
    | otherwise = NoCollision
    where (x1, y1, dy, r)    = (bX ball, bY ball, bDY ball, bRadius ball)
          (x2, y2, dx, l, h) = (pX paddle, pY paddle, pDX paddle, pLength paddle, pHeight paddle)
          boundedLeft        = x1 > x2 - (l/2)
          boundedRight       = x1 < x2 + (l/2)
          boundedTop         = y1 > y2
          boundedBottom      = y1 < y2 + (h/2) + r
          collision          = boundedTop && boundedLeft && boundedRight && boundedBottom
                               && dy < 0

checkBricksCollision :: BrickMap -> Ball -> Collision
checkBricksCollision brickMap ball = NoCollision

collisionMerge :: Collision -> Collision -> Collision
collisionMerge col NoCollision = col
collisionMerge NoCollision col = col
collisionMerge col _           = col
