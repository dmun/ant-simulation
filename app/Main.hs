module Main where

import           Control.Monad.IO.Class
import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Simulate

import           Ant
import           Food
import           Model

window :: Display
window = InWindow "Ant Simulation" (200, 200) (10, 10)

main :: IO ()
main = do
    ants <- liftIO $ initAnts 1000
    food <- liftIO $ initFood (-400, 400) (-400, 400) 10
    --let food = initFood (-400, 400) (-400, 400) 10 getStdGen
    simulate window background 30 (initModel ants food) renderModel simulateModel
    where
        background = makeColorI 44 62 80 255

--testFood = [Food { fx = 200, fy = 400, fsize = 10 }, Food { fx = 400, fy = 200, fsize = 10 }]
