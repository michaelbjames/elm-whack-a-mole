import Window
import Mouse
import Random
import Graphics.Input

data Input
    = Window (Int, Int)
    | ToD Time -- Update every 300ms
    | Random Float
    | Whack

type Status = {
    score : Int,
    ttl : Time,
    playsLeft : Int,
    moleOnScreen : Bool,
    windowSize : (Int, Int)
    }

targetInput : Graphics.Input.Input ()
targetInput = Graphics.Input.input ()

inputs : Signal Input
inputs = merges [
    lift Window Window.dimensions,
    lift (always Whack) <| targetInput.signal,
    lift ToD <| every 300,
    lift Random <| Random.float <| every 300
    ]

main = lift asText inputs
