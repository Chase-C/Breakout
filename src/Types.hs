module Types where

import qualified Data.Map.Strict as M

type Time = Float

data Collision = NoCollision
               | Collision
    { cBrick    :: Maybe (Int, Int)
    , cVertical :: !Bool
    , cVelMod   :: !Float
    }

data Game = Game
    { gTime     :: !Time
    , gBricks   :: !BrickMap
    , gPaddle   :: !Paddle
    , gBall     :: !Ball
    , gScore    :: !Float
    , gLives    :: !Int
    }

data BrickMap = BrickMap
    { bmBricks  :: M.Map (Int, Int) Int
    , bmNumX    :: !Int
    , bmNumY    :: !Int
    , bmX       :: !Float
    , bmY       :: !Float
    , bmWidth   :: !Float
    , bmHeight  :: !Float
    , bmSep     :: !Float
    }

data Paddle = Paddle
    { pX        :: !Float
    , pY        :: !Float
    , pDX       :: !Float
    , pDDX      :: !Float
    , pLength   :: !Float
    , pHeight   :: !Float
    }

data Ball = Ball
    { bX        :: !Float
    , bY        :: !Float
    , bDX       :: !Float
    , bDY       :: !Float
    , bRadius   :: !Float
    }
