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

initAnts :: Int -> IO [Ant]
initAnts n = forM
    [1 .. n]
    (\_ -> do
        angle    <- getRandom 0 360
        velocity <- getRandom 1 3
        return $ Ant { x        = 0
                     , y        = 0
                     , size     = 3
                     , angle    = angle
                     , velocity = velocity
                     }
    )

--turnAnt :: Ant -> Float -> Ant
--turnAnt ant degrees = ant { angle = (angle ant + degrees) `mod` 360 }
