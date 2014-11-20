module BrickMap where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import qualified Data.Map.Strict as M

import Types

initBrickMap :: Int -> Int -> Float -> Float -> Float -> Float -> Float -> Float -> BrickMap
initBrickMap numX numY padX padY height sep winW winH =
    let w = ((winW - (2 * padX)) / fromIntegral numX) - sep
        y = winH - (padY + ((height + sep) * fromIntegral numY))
    in  BrickMap
        { bmBricks = M.fromList [((x, y), 1) | x <- [1..numX], y <- [1..numY]]
        , bmNumX   = numX
        , bmNumY   = numY
        , bmX      = padX
        , bmY      = y
        , bmWidth  = w
        , bmHeight = height
        , bmSep    = sep
        }

updateBricks :: Time -> BrickMap -> Collision -> BrickMap
updateBricks dt brickMap collision = brickMap

drawBricks :: BrickMap -> Picture
drawBricks brickMap =
    let w      = bmWidth  brickMap
        h      = bmHeight brickMap
        sep    = bmSep brickMap
        numY   = bmNumY brickMap
        x      = bmX brickMap
        y      = bmY brickMap
        bricks = map (drawBrick x y w h sep) $ M.toList $ bmBricks brickMap
    in  pictures bricks

drawBrick :: Float -> Float -> Float -> Float -> Float -> ((Int, Int), Int) -> Picture
drawBrick bx by w h sep ((ix, iy), _) = color (greyN 0.6) $ translate x y $ polygon corners
    where x       = ((w + sep) * fromIntegral (ix - 1)) + bx
          y       = ((h + sep) * fromIntegral (iy - 1)) + by
          corners = [(0, 0), (w, 0), (w, h), (0, h)]
