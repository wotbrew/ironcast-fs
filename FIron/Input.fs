module Input
open Util
open Geom
open FSharpx
open Microsoft.Xna.Framework.Input

let mouseState() = Mouse.GetState()
let keyboardState() = Keyboard.GetState()
let inputState() = mouseState(), keyboardState()

open State

module Cam =
    type Command = 
        | Shift of Direction
    let toMsg = function
        | Shift d -> Cam.Msg.Shift d


type Command = 
 | Cam of Cam.Command
 | ToggleMultiSelect

let downCommands = 
    [Keys.W, Cam (Cam.Shift S)
     Keys.A, Cam (Cam.Shift E)
     Keys.S, Cam (Cam.Shift N)
     Keys.D, Cam (Cam.Shift W)
     Keys.LeftShift, ToggleMultiSelect] 

let downCommandsMap = downCommands |> Map.ofSeq
let downCommandsMapR = downCommands |> Seq.map Tup.flip |> Map.ofSeq

type InputState = MouseState * KeyboardState
type MouseButton = Left | Right

let loc (st:MouseState) = vec(float32 st.X, float32 st.Y)

let isKeyDown k (st:KeyboardState) = st.IsKeyDown k
let commandDown c (_, (m, k)) = 
    Map.find c downCommandsMapR
    |> flip isKeyDown k


module Event = 
    let source = Event<InputState>()
    let update = source.Publish
    let updateSt = update |> Event.pairwise

    //may want to struct this stuff up

    let mapInput f e = 
        e
        |> Event.map (Tup.mapsnd f)

    let updateMouse =
        updateSt 
        |> Event.map (Tup.dup >> Tup.mapsnd (Tup.map fst fst))

    let updateKeyboard = 
        updateSt 
        |> Event.map (Tup.dup >> Tup.mapsnd (Tup.map snd snd))

    let keyDown =
        updateKeyboard 
        |> mapInput (fun (_, st) -> Seq.filter (fst >> flip isKeyDown st) downCommands)
    
    let command = 
        keyDown
        |> mapInput (Seq.map snd >> Set.ofSeq)

    let mouseClick = 
        updateMouse
        |> Event.map (fun (is, (o, n)) ->
            if n.LeftButton = ButtonState.Released
               && o.LeftButton = ButtonState.Pressed then
             Some (is,(n,Left))
            elif n.RightButton = ButtonState.Released 
                 && o.RightButton = ButtonState.Pressed then
                  Some (is, (n,Right))
            else None)
        |> Event.choose id
    
    type MapClickEvent = MouseButton * State.MapState.MapState * pt
    let mapClick = 
        let mc = Event<(InputState * InputState) * MapClickEvent>()
        mouseClick
        |> Event.add (fun (is, (s,x)) ->
            //dispatch on seperate thread from here on
            async {
                let camA = Cam.getAsync()
                let msA = State.MapState.getAsync()
                let! cam = camA
                let! ms = msA
                let vp = Cam.viewport cam
                let wpos = Cam.world (loc s) cam
                if vp.Contains(int wpos.X, int wpos.Y) then
                    let pos = wpos / 32.0f |> Pt.ofVec
                    do mc.Trigger(is, (x, ms, pos))
            } |> Async.Start)
        mc.Publish
    

    let creClick = 
        mapClick
        |> mapInput (fun (x, ms, p) -> x, MapStack.getCre p ms.stack)
        |> Event.filter (snd >> snd >> Option.isSome)
        |> mapInput (Tup.mapsnd Option.get)
    
    let walkableClick =
        mapClick
        |> Event.filter (snd >> fun (x, ms, p) -> ms.stack.walk |> Grid.get1 p)

    let playerClick = 
        creClick
        |> Event.filter (snd >> snd >> Cre.isPlayer)
    
    let selectPlayer =
        playerClick.Add(fun (is, (x, p)) ->
            if commandDown ToggleMultiSelect is then
                State.Ui.select p.id
            else 
                State.Ui.selectOnly p.id)
    
    let moveSelection =
        walkableClick.Add(fun (is, (x, ms, p)) ->
            Ui.moveSelection p)
        


let firekeys (k:KeyboardState) = 
    for (key, v) in downCommands do
     if k.IsKeyDown key then 
        match v with
        | Cam c -> Cam.agent.Post (Cam.toMsg c)
        | _ -> ()

let camMouse (m:MouseState) (w,h) = 
    let x = [if m.X <= 50 then yield E
             if m.Y <= 50 then yield S
             if m.X >= w - 50 then yield W
             if m.Y >= h - 50 then yield N] 
            |> Seq.map Cam.Msg.Shift
    Seq.iter Cam.agent.Post x

let firemouse (m:MouseState) (w,h) = 
    do camMouse m (w,h)
 

          
let fireinput (w,h) = 
    let m, k = inputState()
    Event.source.Trigger(m,k)
    firekeys k
    firemouse m (w,h)


