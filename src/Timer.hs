module Timer where

import Types
import Ball

updateTimer :: Time -> Timer -> Timer
updateTimer dt timer = timer { tSeconds = tSeconds timer - dt }

tTimeout :: Timer -> Bool
tTimeout timer = tSeconds timer <= 0

ballTimer :: Timer
ballTimer = Timer
    { tSeconds = 2
    , tFunc    = func
    }
    where func game = game { gBall = (gBall game) { bDY = -5 } }
