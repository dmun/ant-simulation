module Main where

import           Control.Monad.IO.Class
import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Simulate

import           Ant
import           Model

window :: Display
window = InWindow "Ant Simulation" (200, 200) (10, 10)

main :: IO ()
main = do
    ants <- liftIO $ initAnts (-50, 50) (-50, 50) 10
    simulate window (greyN 0.5) 30 (initModel ants) drawModel simulateModel
