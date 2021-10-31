module Model where

import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Simulate

import           Ant
import           Food

data Model = Model
    { ants :: [Ant]
    , food :: [Food]
    }

initModel :: [Ant] -> [Food] -> Model
initModel ants food = Model { ants = ants, food = food }

renderModel :: Model -> Picture
renderModel model =
    pictures $ ant' ++ food' ++ [text $ show $ fx (food model !! 4)]
    where
        ant'  = map (\ant -> translate (x ant) (y ant) $ color black $ circleSolid $ size ant) $ ants model
        food' = map (\food -> translate (fx food) (fy food) $ color green $ circleSolid $ fsize food) $ food model

simulateModel :: ViewPort -> Float -> Model -> Model
simulateModel _ time model = Model { ants = ants', food = food' }
  where
    ants' =
        map
                (\ant -> ant
                    { x = x ant + (sin $ angle ant * (pi / 180)) * velocity ant
                    , y = y ant + (cos $ angle ant * (pi / 180)) * velocity ant
                    }
                )
            $ ants model
    food' = food model
