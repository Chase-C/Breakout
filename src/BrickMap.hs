module BrickMap where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import qualified Data.Map.Strict as M

import Types

initBrickMap :: Int -> Int -> Float -> Float -> Float -> Float -> Float -> BrickMap
initBrickMap numX numY padX padY height winW winH =
    let sep = 4
        bw  = ((winW - (2 * padX)) / fromIntegral numX) - sep
        by  = winH - (padY + ((height + sep) * fromIntegral numY))
    in  BrickMap
        { bmBricks = M.fromList [((x, y), 1) | x <- [1..numX], y <- [1..numY]]
        , bmNumX   = numX
        , bmNumY   = numY
        , bmX      = padX
        , bmY      = by
        , bmWidth  = bw
        , bmHeight = height
        , bmSep    = sep
        }

updateBricks :: BrickMap -> Collision -> BrickMap
updateBricks brickMap NoCollision = brickMap
updateBricks brickMap collision   = brickMap
    { bmBricks = M.delete (cBrick collision) (bmBricks brickMap)
    }

drawBricks :: BrickMap -> Picture
drawBricks brickMap = pictures brickPics
    where validBricks = filter (\(_, v) -> v /= 0) $ M.toList $ bmBricks brickMap
          brickPics   = map (drawBrick brickMap) validBricks

drawBrick :: BrickMap -> ((Int, Int), Int) -> Picture
drawBrick brickMap ((ix, iy), _) =
    let w       = bmWidth  brickMap
        h       = bmHeight brickMap
        sep     = bmSep brickMap
        numY    = bmNumY brickMap
        x       = ((w + sep) * fromIntegral (ix - 1)) + bmX brickMap
        y       = ((h + sep) * fromIntegral (iy - 1)) + bmY brickMap
        corners = [(0, 0), (w, 0), (w, h), (0, h)]
    in  color (greyN 0.6) $ translate x y $ polygon corners
