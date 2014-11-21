module Collision where

import Data.Maybe
import qualified Data.Map.Strict as M

import Types

checkPaddleCollision :: Paddle -> Ball -> Collision
checkPaddleCollision paddle ball
    | collision = Collision
                  { cBrick    = (0, 0)
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
checkBricksCollision brickMap ball
    | validBucket =
        let baseX = (bmX brickMap) + ((fromIntegral xBucket) * (bmWidth  brickMap))
            baseY = (bmY brickMap) + ((fromIntegral yBucket) * (bmHeight brickMap))
            relX2 = (bX ball) - baseX
            relY2 = (bY ball) - baseY
            m     = (bDY ball) / (bDX ball)
            b     = relY2 - (m * relX2)
            yInt  = (m * baseX) + b
        in  Collision
            { cBrick    = (xBucket + 1, yBucket + 1)
            , cVertical = (yInt > 0) && (yInt < bmHeight brickMap)
            , cVelMod   = 0
            }
    | otherwise   = NoCollision
    where relX        = (bX ball) - (bmX brickMap)
          relY        = (bY ball) - (bmY brickMap)
          xBucket     = floor $ relX / (bmWidth  brickMap + bmSep brickMap)
          yBucket     = floor $ relY / (bmHeight brickMap + bmSep brickMap)
          validBucket = (relX >= 0) && (relY >= 0)
                        && (xBucket < bmNumX brickMap) && (yBucket < bmNumY brickMap)
                        && (fromMaybe 0 (M.lookup (xBucket + 1, yBucket + 1) (bmBricks brickMap)) /= 0)

collisionMerge :: Collision -> Collision -> Collision
collisionMerge col NoCollision = col
collisionMerge NoCollision col = col
collisionMerge col _           = col
