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
stepIteration _ time model = simulateModel time model 0
--    ants = [
--        (ants model !! 0) {
--            --size = size (ants model !! 0) + 1,
--            x = (x (ants model !! 0)) + 1,
--            y = (y (ants model !! 0)) + (10 * (time * time))
--}]}
 

simulateModel :: Float -> Model -> Int -> Model
simulateModel time model index = 
    if length (ants model) <= index then
        moveAnt 1 1 $ ants model !! index
    else
        moveAnt 1 1 $ ants (simulateModel time model $ index + 1) !! index
        
moveAnt :: Float -> Float -> Ant -> Ant
moveAnt xO yO ant =
    ant {
        x = (x (ants model !! 0)) + xO,
        y = (y (ants model !! 0)) + yO
}

main :: IO ()
main = simulate window (greyN 0.5) 60 model modelToPicture stepIteration
