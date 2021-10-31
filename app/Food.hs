module Food where

import           Control.Monad                  ( forM )
import           System.Random
import           Utils

data Food = Food
    { fx :: Float
    , fy :: Float
    , fsize :: Float
    }


initFood :: (Int, Int) -> (Int, Int) -> Int -> IO [Food]
initFood (lx, ly) (hx, hy) n = forM
    [1 .. n]
    (\_ -> do
        x        <- getRandom (rtf lx) (rtf hx)
        y        <- getRandom (rtf ly) (rtf hy)
        size     <- getRandom 2 10
        return $ Food { fx        = y
                     , fy        = y
                     , fsize     = size
                     }
    )
    where rtf = realToFrac
