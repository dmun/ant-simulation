module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Simulate

window :: Display
window = InWindow "Nice Window" (200, 200) (10, 10)

modelToPicture :: Float -> Picture
modelToPicture size = circle size

--stepIteration :: ViewPort -> Float -> Model -> Model
--stepIteration _ time model = size model + 1

data Model = Model {
    size :: Float
}

model :: Model
model = Model {
    size = 80
}

main :: IO ()
--main = simulate window white model modelToPicture stepIteration
main = do
    display window white $ modelToPicture $ size model
