module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Simulate

window :: Display
window = InWindow "Ant Simulation" (200, 200) (10, 10)

data Model = Model {
    ants :: [Ant]
}

data Ant = Ant {
    x :: Float,
    y :: Float,
    size :: Float
}
ant :: Ant
ant2 = Ant {
    x = 0,
    y = 100,
    size = 20
}
ant = Ant {
    x = 0,
    y = 0,
    size = 10
}

model :: Model
model = Model {
    ants = [ant, ant2]
}

modelToPicture :: Model -> Picture
modelToPicture model = translate (x (ants model !! 0)) (y (ants model !! 0)) $ circleSolid $ size $ ants model !! 0

stepIteration :: ViewPort -> Float -> Model -> Model
stepIteration _ time model =
    Model { ants = ants' }
    where
    ants' = map (\ant -> ant {
        x = x ant + 5,
        y = y ant + 5
    }) $ ants model

main :: IO ()
main = simulate window (greyN 0.5) 30 model modelToPicture stepIteration
