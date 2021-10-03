module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Simulate

window :: Display
window = InWindow "Ant Simulation" (200, 200) (10, 10)

data Model = Model {
    size :: Float
}

model :: Model
model = Model {
    size = 80
}

modelToPicture :: Model -> Picture
modelToPicture model = Circle $ size model

stepIteration :: ViewPort -> Float -> Model -> Model
stepIteration _ time model = model { size = size model + 1 }

main :: IO ()
main = simulate window white 15 model modelToPicture stepIteration
