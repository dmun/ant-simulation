module Utils where

import           System.Random

getRandom :: (Random a) => a -> a -> IO a
getRandom l u = do
    seed <- getStdGen
    let (n, s) = randomR (l, u) seed
    setStdGen s
    return n
