module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Simulate

window :: Display
window = InWindow "Ant Simulation" (200, 200) (10, 10)

data Model = Model {
    size :: Float,
    ants :: [Ant]
}

data Ant = Ant {
    x :: Float,
    y :: Float
}

model :: Model
model = Model {
    size = 80
}

modelToPicture :: Model -> Picture
modelToPicture model = color black $ circleSolid $ size model

stepIteration :: ViewPort -> Float -> Model -> Model
stepIteration _ time model = model { size = size model + 1 }

main :: IO ()
main = simulate window (greyN 0.5) 15 model modelToPicture stepIteration
