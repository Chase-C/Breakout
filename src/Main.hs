module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Game

main :: IO ()
main = do
    let w = 800
        h = 600
    play (InWindow "Breakout" (w, h) (10, 10))
         (greyN 0.1)     -- Background color
         60              -- FPS
         (initGame w h)  -- Initial state
         render          -- Render function
         handleEvents    -- Input event function
         update          -- Update function
