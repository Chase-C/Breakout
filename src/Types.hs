module Types where

import Graphics.Gloss.Data.Picture
import qualified Data.Map.Strict as M

type Time = Float

data Game = Game
    { gTime     :: !Time
    , gDims     :: !(Float, Float)
    , gBricks   :: !BrickMap
    , gPaddle   :: !Paddle
    , gBall     :: !Ball
    , gScore    :: !Int
    , gLives    :: !Int
    , gTimers   :: [Timer]
    , gInsts    :: [Instance]
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

data Collision = NoCollision
               | Collision
    { cBrick    :: !(Int, Int)
    , cVertical :: !Bool
    , cVelMod   :: !Float
    }

data Timer = Timer
    { tSeconds  :: !Float
    , tFunc     :: Game -> Game
    }

data Instance = Instance
    { iSeconds  :: !Float
    , iFunc     :: Game -> Picture
    }
