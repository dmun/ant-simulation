module Model where

import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Simulate

import           Ant
import           Food
import           Trail

data Model =
    Model
        { ants        :: [Ant]
        , food        :: [Food]
        , trail       :: [Trail]
        , elapsedTime :: Float
        }

pooptrail :: Trail
pooptrail = Trail {tx = 300, ty = 200, tsize = 9}

initModel :: [Ant] -> [Food] -> Model
initModel ants food =
    Model {ants = ants, food = food, trail = [pooptrail], elapsedTime = 0}

renderModel :: Model -> Picture
renderModel model =
    pictures $
    box' ++ ant' ++ food' ++ trail' ++ [text $ show $ elapsedTime model]
  where
    ant' =
        map
            (\ant ->
                 translate (x ant) (y ant) $
                 color black $ circleSolid $ size ant) $
        ants model
    food' =
        map
            (\food ->
                 translate (fx food) (fy food) $
                 color green $ circleSolid $ fsize food) $
        food model
    trail' =
        map
            (\trail ->
                 translate (tx trail) (ty trail) $
                 color blue $ circleSolid $ tsize trail) $
        trail model
    box' = [box 800 800]

box :: Float -> Float -> Picture
box width height =
    color (makeColorI 149 165 166 255) $ rectangleSolid width height

simulateModel :: ViewPort -> Float -> Model -> Model
simulateModel _ time model =
    Model
        { ants = ants'
        , food = food'
        , trail = trail model
        , elapsedTime = elapsedTime model + time
        }
  where
    ants' = map (\ant -> moveAnt ant) $ ants model
    food' = food model
    --trail = if time `mod` 10 == 0 then map (\ant -> model = Model { ants = ants model, food = food model, trail = (trail model) ++ Trail { tx = x ant, ty = y ant, tsize = size ant }, elapsedTime}) $ ants model

moveAnt :: Ant -> Ant
moveAnt ant =
    ant {x = x ant + xOffset * velocity ant, y = y ant + yOffset * velocity ant}
  where
    (xOffset, yOffset) = calcOffset $ angle ant

--containAnt :: Ant -> Float -> Float -> Ant
--containAnt ant borderLimit rotationSpeed =
--    if (x ant + xOffset * (velocity ant) * borderLimit > 400 && angle ant > 0 && angle ant < 180) then turnAnt ant rotationSpeed
--    else if (x ant + xOffset * (velocity ant) * borderLimit < -400 && angle ant > 180 && angle ant < 360) then ant { angle = angle ant + rotationSpeed}
--    else if (y ant + yOffset * (velocity ant) * borderLimit > 400) then ant { angle = angle ant + rotationSpeed }
--    else if (y ant + yOffset * (velocity ant) * borderLimit < -400) then ant { angle = angle ant + rotationSpeed }
--    else ant
--    where (xOffset, yOffset) = calcOffset $ angle ant
calcOffset :: Float -> (Float, Float)
calcOffset angle = (sin $ angle * (pi / 180), cos $ angle * (pi / 180))
