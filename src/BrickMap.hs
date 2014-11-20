module BrickMap where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import qualified Data.Map.Strict as M

import Types

emptyBrickMap :: BrickMap
emptyBrickMap = BrickMap
    { bmBricks = M.empty
    , bmNumX   = 10
    , bmNumY   = 5
    , bmWidth  = 72
    , bmHeight = 8
    }

initBrickMap :: BrickMap
initBrickMap = fillBricks emptyBrickMap

fillBricks :: BrickMap -> BrickMap
fillBricks brickMap = brickMap
    { bmBricks = M.fromList [((x, y), 1) | x <- [1..bmNumX brickMap], y <- [1..bmNumY brickMap]]
    }

updateBricks :: Time -> BrickMap -> Collision -> BrickMap
updateBricks dt brickMap collision = brickMap

drawBricks :: BrickMap -> Picture
drawBricks brickMap =
    let w = bmWidth  brickMap
        h = bmHeight brickMap
        bricks = map (drawBrick w h) $ M.toList $ bmBricks brickMap
    in  pictures bricks

drawBrick :: Float -> Float -> ((Int, Int), Int) -> Picture
drawBrick w h ((ix, iy), _) = color (greyN 0.6) $ translate x y $ polygon corners
    where x       = ((w + 4) * fromIntegral (ix - 1)) + 22
          y       = ((h + 4) * fromIntegral (iy - 1)) + 460
          corners = [(0, 0), (w, 0), (w, h), (0, h)]
