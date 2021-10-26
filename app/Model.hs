module Model where

import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Simulate

import           Ant

data Model = Model
    { ants :: [Ant]
    }

initModel :: [Ant] -> Model
initModel ants = Model { ants = ants }

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
