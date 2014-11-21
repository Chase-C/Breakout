module Game where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Picture

import Types
import BrickMap
import Paddle
import Ball
import Collision

initGame :: Game
initGame = Game
    { gTime   = 0
    , gBricks = initBrickMap 11 6 25 64 12 4 800 600
    , gPaddle = initPaddle
    , gBall   = initBall
    , gScore  = 0
    , gLives  = 3
    }

render :: Game -> Picture
render game =
    let bricks = drawBricks $ gBricks game
        paddle = drawPaddle $ gPaddle game
        ball   = drawBall   $ gBall   game
        final  = pictures [bricks, paddle, ball]
    in  translate (-400) (-300) final

handleEvents :: Event -> Game -> Game
handleEvents (EventKey key state _ _) game =
    let acc = case key of
                  Char c ->
                      case c of
                          'a' -> (-1536)
                          'd' -> 1536
                          _   -> 0
                  SpecialKey sk ->
                      case sk of
                          KeyLeft  -> (-1536)
                          KeyRight -> 1536
                          _        -> 0
                  _ -> 0
        fixAcc = case state of
                     Up -> (-acc)
                     _  -> acc
    in  game { gPaddle = addPaddleAccel fixAcc $ gPaddle game }
handleEvents _ game = game

update :: Time -> Game -> Game
update dt game = game
    { gTime   = gTime game + dt
    , gBricks = bricks
    , gPaddle = paddle
    , gBall   = ball
    }
    where paddleCol = checkPaddleCollision (gPaddle game) (gBall game)
          bricksCol = checkBricksCollision (gBricks game) (gBall game)
          ball      = updateBall   dt (gBall game) $ collisionMerge paddleCol bricksCol
          bricks    = updateBricks dt (gBricks game) bricksCol
          paddle    = updatePaddle dt (gPaddle game)
