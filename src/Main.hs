module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Game

main :: IO ()
main = play (InWindow "Breakout" (800, 600) (10, 10))
            (greyN 0.1)     -- Background color
            60              -- FPS
            initGame        -- Initial state
            render          -- Render function
            handleEvents    -- Input event function
            update          -- Update function
