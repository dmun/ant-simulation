module Main where

import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Simulate

window :: Display
window = InWindow "Ant Simulation" (200, 200) (10, 10)

data Model = Model
    { ants :: [Ant]
    }

data Ant = Ant
    { x        :: Float
    , y        :: Float
    , size     :: Float
    , angle    :: Float
    , velocity :: Float
    }

main :: IO ()
main = simulate window (greyN 0.5) 30 model drawModel simulateModel

ant :: Ant
ant2 = Ant { x = 0, y = 100, size = 20, angle = 0, velocity = 5 }
ant = Ant { x = 0, y = 0, size = 10, angle = 90, velocity = 3 }

model :: Model
model = Model { ants = [ant, ant2] }

drawModel :: Model -> Picture
drawModel model =
    pictures
        $ map (\ant -> translate (x ant) (y ant) $ circleSolid $ size ant)
        $ ants model

simulateModel :: ViewPort -> Float -> Model -> Model
simulateModel _ time model = Model { ants = ants' }
  where
    ants' =
        map
                (\ant -> ant
                    { x = x ant + (sin $ angle ant * (pi / 180)) * velocity ant
                    , y = y ant + (cos $ angle ant * (pi / 180)) * velocity ant
                    }
                )
            $ ants model
