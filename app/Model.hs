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
    pictures $ box' ++ ant' ++ food' ++ [text $ show $ fx (food model !! 4)]
    where
        ant'  = map (\ant -> translate (x ant) (y ant) $ color black $ circleSolid $ size ant) $ ants model
        food' = map (\food -> translate (fx food) (fy food) $ color green $ circleSolid $ fsize food) $ food model
        box'  = [box 800 800]

box :: Float -> Float -> Picture
box width height = color (makeColorI 149 165 166 255) $ rectangleSolid width height

simulateModel :: ViewPort -> Float -> Model -> Model
simulateModel _ time model = Model { ants = ants', food = food' }
  where
    ants' =
        map
                (\ant -> moveAnt ant)
            $ ants model
    food' = food model

moveAnt :: Ant -> Ant
moveAnt ant =
    containAnt ant { x = x ant + xOffset * velocity ant
                    , y = y ant + yOffset * velocity ant
                    } 10 10
    where (xOffset, yOffset) = calcOffset $ angle ant

containAnt :: Ant -> Float -> Float -> Ant
containAnt ant borderLimit rotationSpeed =
    if (x ant + xOffset * (velocity ant) * borderLimit > 400 && angle ant > 0 && angle ant < 180) then ant { angle = angle ant + rotationSpeed}
    else if (x ant + xOffset * (velocity ant) * borderLimit < -400 && angle ant > 180 && angle ant < 360) then ant { angle = angle ant + rotationSpeed}
    else if (y ant + yOffset * (velocity ant) * borderLimit > 400) then ant { angle = angle ant + rotationSpeed }
    else if (y ant + yOffset * (velocity ant) * borderLimit < -400) then ant { angle = angle ant + rotationSpeed }
    else ant
    where (xOffset, yOffset) = calcOffset $ angle ant

calcOffset :: Float -> (Float, Float)
calcOffset angle = ( sin $ angle * (pi / 180)
                  , cos $ angle * (pi / 180)
                  )
