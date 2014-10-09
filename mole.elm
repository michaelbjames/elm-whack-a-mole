import Mouse
import Graphics.Input
import Random
import Debug

windowXSize = 500
windowYSize = 500
elephantProbability = 0.25

data Input
    = GameTick RandomValues -- Update every second and half
    | Whack

type Status = {
    score : Int,
    playsLeft : Int,
    moleOnScreen : Bool,
    windowSize : (Int, Int),
    targetPosition : (Int, Int),
    doMole : Bool
    }

type RandomValues = {
    coordinates : (Int, Int),
    probability : Float -- [0-1)
}

targetInput : Graphics.Input.Input ()
targetInput = Graphics.Input.input ()

inputs : Signal Input
inputs = 
    let
        ticker = every 1500
        randomness1 = Random.float ticker -- Signal Float
        randomness2 = Random.float ticker -- Signal Float
        randomness3 = Random.float ticker -- Signal Float
        randoms s1 s2 s3 = {
            coordinates = (ceiling (s1 * 10000) % windowXSize, ceiling (s2 * 10000) % windowYSize),
            probability = s3
            }
    in
        merges [
            lift (always Whack) <| targetInput.signal,
            lift GameTick <| lift3 randoms randomness1 randomness2 randomness3
        ]

emptyStatus = {
    score = 0,
    playsLeft = 10,
    moleOnScreen = False,
    windowSize = (windowXSize, windowYSize),
    targetPosition = (250, 250),
    doMole = True
    }

update : Input -> Status -> Status
update input status = case input of
    GameTick rvs -> {status |
        moleOnScreen <- not status.moleOnScreen,
        doMole <- rvs.probability > elephantProbability,
        targetPosition <- rvs.coordinates,
        playsLeft <- if status.playsLeft > 0 then status.playsLeft - 1 else 0
        }
    Whack -> {status |
        score <- if status.doMole then status.score + 1 else status.score - 10,
        moleOnScreen <- False
        }

gameState : Signal Status
gameState = foldp update emptyStatus inputs

main = lift draw (lift (Debug.watch "Game") gameState)

draw : Status -> Element
draw status =
    if  | status.moleOnScreen && status.playsLeft > 0 -> targetElement status
        | status.playsLeft == 0 -> asText status.score
        | otherwise -> empty


targetElement : Status -> Element
targetElement status =
    let moleImg = image 50 50 "img/mole.png"
            |> Graphics.Input.clickable targetInput.handle ()
        elephantImg = image 228 158 "img/elephant.png"
            |> Graphics.Input.clickable targetInput.handle ()
        img = if status.doMole then moleImg else elephantImg
            -- BUG! I should be able to apply clickable to this if statement
        windowX = status.windowSize |> fst
        windowY = status.windowSize |> snd
        xPosition = status.targetPosition |> fst |> absolute
        yPosition = status.targetPosition |> snd |> absolute
        position = middleAt xPosition yPosition
    in
        container windowX windowY position img
