module Game where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Picture
import Data.List

import qualified Data.Map.Strict as M

import Types
import BrickMap
import Paddle
import Ball
import Collision
import Timer
import Instance

initGame :: Int -> Int -> Game
initGame w h =
    let fw = fromIntegral w
        fh = fromIntegral h
    in  Game
        { gTime   = 0
        , gDims   = (fw, fh)
        , gBricks = initBrickMap 11 5 25 64 20 fw fh
        , gPaddle = initPaddle   fw
        , gBall   = initBall     fw fh
        , gScore  = 0
        , gLives  = 3
        , gTimers = [ballTimer]
        , gInsts  = []
        }

render :: Game -> Picture
render game =
    let bricks = drawBricks $ gBricks game
        paddle = drawPaddle $ gPaddle game
        ball   = drawBall   $ gBall   game
        ui     = renderUI game
        insts  = map (`iFunc` game) $ gInsts game
        final  = pictures $ [bricks, paddle, ball, ui] ++ insts
    in  translate (-400) (-300) final

renderUI :: Game -> Picture
renderUI game =
    let (w, h) = gDims game
        score  = translate 32 (-16) $ scale 0.2 0.2 $ text $ "Score: " ++ show (gScore game)
        balls  = map (\x -> translate (fromIntegral x * 16) 0 $ circleSolid 4) [1..gLives game]
        lives  = translate (w - 96) 0 $ pictures balls
    in  translate 0 (h - 32) $ color (greyN 0.6) $ pictures [score, lives]

handleEvents :: Event -> Game -> Game
handleEvents (EventKey key state _ _) game =
    let acc = case key of
                  Char c ->
                      case c of
                          'a' -> -0.5
                          'd' -> 0.5
                          _   -> 0
                  SpecialKey sk ->
                      case sk of
                          KeyLeft  -> -0.5
                          KeyRight -> 0.5
                          _        -> 0
                  _ -> 0
        fixAcc = case state of
                     Up -> -acc
                     _  -> acc
    in  game { gPaddle = addPaddleAccel fixAcc $ gPaddle game }
handleEvents _ game = game

update :: Time -> Game -> Game
update dt oldGame
    | loseCond || winCond || gameOver =
        let (w, h) = gDims oldGame
        in  oldGame
            { gBricks = if loseCond then gBricks oldGame else initBrickMap 11 5 25 64 20 w h
            , gPaddle = initPaddle w
            , gBall   = initBall   w h
            , gScore  = if gameOver then 0 else gScore oldGame
            , gLives  = if gameOver then 3 else gLives oldGame - if loseCond then 1 else 0
            , gTimers = ballTimer : gTimers oldGame
            }
    | otherwise =
        let (due, timers) = partition tTimeout $ map (updateTimer dt) $ gTimers oldGame
            game          = foldl (flip tFunc) oldGame due
            paddleCol     = checkPaddleCollision (gPaddle game) (gBall game)
            bricksCol     = checkBricksCollision (gBricks game) (gBall game)
        in  game
            { gTime   = gTime game + dt
            , gBricks = updateBricks (gBricks game) bricksCol
            , gPaddle = updatePaddle (gPaddle game)
            , gBall   = updateBall   (gBall game) $ collisionMerge paddleCol bricksCol
            , gScore  = case bricksCol of
                            NoCollision -> gScore game
                            _           -> gScore game + 5
            , gTimers = timers
            , gInsts  = filter iTimeout $ map (updateInst dt) $ gInsts oldGame
            }
    where loseCond = (bY . gBall) oldGame < (pY . gPaddle) oldGame - 8
          winCond  = (M.size . bmBricks . gBricks) oldGame == 0
          gameOver = gLives oldGame <= 0
