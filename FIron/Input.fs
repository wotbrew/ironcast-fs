module Input
open Util
open Geom
open FSharpx
open Microsoft.Xna.Framework.Input

let mouseState() = Mouse.GetState()
let keyboardState() = Keyboard.GetState()
let inputState() = mouseState(), keyboardState()


open State

type Command = 
 | Cam of Cam.Msg


type InputState = MouseState * KeyboardState
type MouseButton = Left | Right

let loc (st:MouseState) = vec(float32 st.X, float32 st.Y)

module Event = 
    let source = Event<InputState>()
    let update = source.Publish
    let updateSt = update |> Event.pairwise
    let updateMouse = updateSt |> Event.map (Tup.map fst fst)
    let updateKeyboard = updateSt |> Event.map (Tup.map snd snd)

    let mouseClick = 
        updateMouse
        |> Event.map (fun (o, n) ->
            if n.LeftButton = ButtonState.Released && o.LeftButton = ButtonState.Pressed then Some (n,Left)
            elif n.RightButton = ButtonState.Released && o.RightButton = ButtonState.Pressed then Some (n,Right)
            else None)
        |> Event.choose id

    let mapClicked = 
        mouseClick
        |> Event.add(fun (s, x) ->
            match x with 
              | Left -> 
                async {
                    let! cam = Cam.getAsync()
                    let pos = Cam.world (loc s) cam / 32.0f |> Pt.ofVec
                    State.Move.movePlayer 0 pos
                } |> Async.Start
              | _ -> ())


let commands = 
    [Keys.W, Cam (Cam.Msg.Shift [South])
     Keys.A, Cam (Cam.Msg.Shift [East])
     Keys.S, Cam (Cam.Msg.Shift [North])
     Keys.D, Cam (Cam.Msg.Shift [West])] 

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
            |> Cam.Msg.Shift
    Cam.agent.Post x

let firemouse (m:MouseState) (w,h) = 
    do camMouse m (w,h)
 

          
let fireinput (w,h) = 
    let m, k = inputState()
    Event.source.Trigger(m,k)
    firekeys k
    firemouse m (w,h)


