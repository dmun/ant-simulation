module Ant where

import           Control.Monad                  ( forM )
import           Utils

data Ant = Ant
    { x        :: Float
    , y        :: Float
    , size     :: Float
    , angle    :: Float
    , velocity :: Float
    }

initAnts :: (Int, Int) -> (Int, Int) -> Int -> IO [Ant]
initAnts (lx, ly) (hx, hy) n = forM
    [1 .. n]
    (\_ -> do
        x        <- getRandom (rtf lx) (rtf hx)
        y        <- getRandom (rtf ly) (rtf hy)
        size     <- getRandom 2 10
        angle    <- getRandom 0 360
        velocity <- getRandom 2 10
        return $ Ant { x        = x
                     , y        = y
                     , size     = size
                     , angle    = angle
                     , velocity = velocity
                     }
    )
    where rtf = realToFrac
