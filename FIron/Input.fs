module Input
open Util
open Geom
open FSharpx
open Microsoft.Xna.Framework.Input

let mouseState() = Mouse.GetState()
let keyboardState() = Keyboard.GetState()
let inputState() = mouseState(), keyboardState()
//
//
//
//type State = {
//    current : MouseState * KeyboardState
//    history : (MouseState * KeyboardState) list
//}
//let initialState() = {
//    current = state()
//    history = []
//}
//let runState s = let {current = current; history = history} = s
//                 let now = state()
//                 let history = now :: history |> List.clamp 500 10
//                 {current = now; history = history}
//
//type Events = {
//    keyPressed : Event<Keys>
//    leftClicked : Event<
//}

open State
type Command = 
 | Cam of Cam.Command

let commands = 
    [Keys.W, Cam (Cam.Command.Shift [South])
     Keys.A, Cam (Cam.Command.Shift [East])
     Keys.S, Cam (Cam.Command.Shift [North])
     Keys.D, Cam (Cam.Command.Shift [West])] 

let firekeys (k:KeyboardState) = 
    for (key, v) in commands do
     if k.IsKeyDown key then 
        match v with
        | Cam c -> Cam.agent.Post c

let camMouse (m:MouseState) (w,h) = 
    let x = [if m.X <= 50 then yield East
             if m.Y <= 50 then yield South
             if m.X >= w - 50 then yield West
             if m.Y >= h - 50 then yield North] 
            |> Cam.Command.Shift
    Cam.agent.Post x

let firemouse (m:MouseState) (w,h) = 
    do camMouse m (w,h)
            
let fireinput (w,h) = 
    let m, k = inputState()
    firekeys k
    firemouse m (w,h)

//let rec inputLoop rate = async {
//    do fireinput()
//    do! Async.Sleep rate
//    do! inputLoop rate
//}

