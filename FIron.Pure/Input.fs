module Input
open FSharpx
open Util
open Geom

open Microsoft.Xna.Framework.Input

type InType = Hold | Press | Double

type InputState = {
    mouse : MouseState
    mouseOld : MouseState
    keyboard : KeyboardState
    keyboardOld : KeyboardState
}

type InputType = 
    | Mouse of Button * InType
    | Keyboard of XnaKey * InType

let initState m mo k ko = {
    mouse = m
    mouseOld = mo
    keyboard = k
    keyboardOld = ko
}

let mousePair ris = ris.mouseOld, ris.mouse
let keyboardPair ris = ris.keyboardOld, ris.keyboard
let mouse ris = ris.mouse
let keyboard ris = ris.keyboard

let refresh ms keys st = {
    mouse = ms
    mouseOld = st.mouse
    keyboard = keys
    keyboardOld = st.keyboard
}

module Keyboard =
    let keyDown k (ks:KeyboardState) =
        ks.IsKeyDown k
    let keyUp k (ks:KeyboardState) =
        ks.IsKeyUp k
    let keyPress k (ksa, ksb) = 
        keyDown k ksa && keyUp k ksb
    let allKeysDown (ks:KeyboardState) = ks.GetPressedKeys()
    let allKeysPressed (ksa, ksb) = 
        let seqa = allKeysDown ksa
        let setb = allKeysDown ksb |> Set.ofArray
        Seq.filter (flip Set.contains setb >> not) seqa

module Mouse = 
    let mousePt (ms:MouseState) = pt(ms.X, ms.Y)
    let mouseVec = mousePt >> Vec.ofPt
    let mouseMapVec gs = mouseVec >> Gs.worldVec gs
    let buttonState b (ms:MouseState) = 
        match b with
            | Left -> ms.LeftButton
            | Right -> ms.RightButton

    let mousePressed b ms = buttonState b ms = ButtonState.Pressed
    let mouseUp b = not << mousePressed b
    let leftPressed = mousePressed Left
    let leftUp = mouseUp Left
    let rightPressed = mousePressed Right
    let rightUp = mouseUp Right

    let mouseClick b (msa, msb) = 
        let a = mousePressed b msa
        let b = mouseUp b msb
        a && b

    let leftClick = mouseClick Left
    let rightClick = mouseClick Right

let mousePt = mouse >> Mouse.mousePt
let mouseVec = mouse >> Mouse.mouseVec
let mouseMapVec gs = mouse >> Mouse.mouseMapVec gs
let mouseMapPt gs = mouse >> Mouse.mouseMapVec gs >> Pt.ofVec
let buttonState b = mouse >> Mouse.buttonState b
let mousePressed b = mouse >> Mouse.mousePressed b
let mouseUp b = mouse >> Mouse.mouseUp b
let leftPressed = mouse >> Mouse.leftPressed
let leftUp = mouse >> Mouse.leftUp
let rightPressed = mouse >> Mouse.rightPressed
let rightUp = mouse >> Mouse.rightUp
let mouseClick b = mousePair >> Mouse.mouseClick b
let leftClick = mousePair >> Mouse.leftClick
let rightClick = mousePair >> Mouse.rightClick

let isKeyPressed k = keyboardPair >> Keyboard.keyPress k
let isKeyDown k = keyboard >> Keyboard.keyDown k
let allKeysDown = keyboard >> Keyboard.allKeysDown
let allKeysPressed = keyboardPair >> Keyboard.allKeysPressed

let click is = 
    if leftClick is then Some Left
    elif rightClick is then Some Right
    else None



let mouseClicks st = 
    seq {
        if leftClick st then
            yield Mouse (Left, Press)
        if rightClick st then
            yield Mouse (Right, Press)
    }

let mousePresses st = 
    seq {
        if leftPressed st then
            yield Mouse (Left, Hold)
        if rightPressed st then
            yield Mouse (Right, Hold)
    }

    
let mouseInput st =
    seq {
        yield! mouseClicks st
        yield! mousePresses st
    }

let keysHeld st = 
    let ks = allKeysDown st
    Seq.map (fun k -> Keyboard (k, Hold)) ks

let keyPresses st = 
    let ks = allKeysPressed st
    Seq.map (fun k -> Keyboard (k, Press)) ks

let keyboardInput st = 
    seq {
        yield! keysHeld st
        yield! keyPresses st
    }


type MapMouse = {
    mapPos : pt
    cre : Cre.Creature option
}

let mapMouse gs input =
    let loc = mouseMapPt gs input
    if World.inBounds loc gs.world then
        {
           mapPos = loc
           cre = Gs.getCre loc gs
        } |> Some
    else None
