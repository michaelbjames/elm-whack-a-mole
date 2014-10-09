import Window
import Mouse
import Graphics.Input
import Debug

data Input
    = Window (Int, Int)
    | GameTick -- Update every 300ms
    | Whack

type Status = {
    score : Int,
    playsLeft : Int,
    moleOnScreen : Bool,
    windowSize : (Int, Int),
    targetPosition : (Int, Int),
    doMole : Bool
    }

targetInput : Graphics.Input.Input ()
targetInput = Graphics.Input.input ()

inputs : Signal Input
inputs = merges [
    lift Window Window.dimensions,
    lift (always Whack) <| targetInput.signal,
    lift (always GameTick) <| every second
    ]

emptyStatus = {
    score = 0,
    playsLeft = 10,
    moleOnScreen = False,
    windowSize = (500, 500),
    targetPosition = (250, 250),
    doMole = True
    }

update : Input -> Status -> Status
update input status = case input of
    Window xy -> {status | windowSize <- xy}
    GameTick -> {status |
        moleOnScreen <- not status.moleOnScreen,
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
