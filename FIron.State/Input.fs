module Input
open Input
open Geom
open Microsoft.Xna.Framework.Input
open FSharpx

let camDirections (keys:Settings.CamKeys) st = seq {
    if isKeyDown keys.up st then
        yield S
    if isKeyDown keys.down st then
        yield N
    if isKeyDown keys.left st then
        yield E
    if isKeyDown keys.right st then
        yield W
}

let moveCam keys st =
    let directions = camDirections keys st
    let speed = if isKeyDown keys.speedMod st then 2.0f else 1.0f
    Cam.swap (Cam.shiftWithDelta directions speed) 

let select (game:Gs.Gs) cre st = 
    let keys = game.settings.keys
    let button = keys.select
    if Cre.isPlayer cre && mouseClick button st then 
        if isKeyDown keys.selectManyMod st then
            Ui.select cre.id
        else 
            Ui.selectOnly cre.id

let moveParty (mouse:MapMouse) (game:Gs.Gs) st = 
    let pos = mouse.mapPos
    let button = game.settings.keys.move
    if mouseClick button st && World.canWalk game.world pos then
       do Move.creatures pos game.ui.selected
       None
    else Some ()



let updateLasso (game:Gs.Gs) st = 
    let maprect = Cam.mapRect game.cam game.settings.video.cellSize
    let button = game.settings.keys.select
    let pressed = mousePressed button st
    let lasso = game.ui.lasso
    if pressed then
        let ui = Ui.updateFromMouse (mousePt st) game.ui
        Ui.state.Swap <| Ui.updateFromMouse (mousePt st)
        Some ()
    elif Ui.Lasso.isLassoing lasso then
        let rect = Gs.worldRect game lasso.rect
        let select = Gs.lassoedCreatures game |> Seq.map Cre.cid |> Set.ofSeq
        Ui.state.Swap <| 
            (Ui.selectOnlyMany select >> Ui.resetLasso)
        None
    else
        Ui.state.Swap Ui.resetLasso
        Some ()

let processUiTriggers (game:Gs.Gs) mapMouse = 
        let actions = actionsAt mapMouse
        Ui.state.Swap (Ui.updateActions actions mapMouse.mapPos)
        
let processActions (ui:Ui.Ui) input mapMouse = 
    let zipped = Seq.zip Input.numKeys ui.actions 
    for key, action in zipped do
        if Input.isKeyPressed key input then
            World.performAction action mapMouse.mapPos

let processInput (game:Gs.Gs) input = 
    let keys = game.settings.keys
    moveCam keys.cam input
    Option.maybe {
        let! mapMouse = mapMouse game input
        do processUiTriggers game mapMouse
        do processActions game.ui input mapMouse
        do! updateLasso game input
        do! moveParty mapMouse game input
        let! cre = mapMouse.cre
        do select game cre input
        return ()
    } |> ignore
    
type TriState = 
    | NoInput
    | OnePair of MouseState * KeyboardState
    | Complete of InputState

/// cycle the TriState to the next state.
let cycle m k =
    function
     | NoInput -> OnePair  (m, k)
     | OnePair (mo, ko) -> (initState m mo k ko) |> Complete
     | Complete ist -> ((refresh m k) ist) |> Complete

let procSideEffects game st = 
    match st with
     | NoInput -> st
     | OnePair (mo, ko) -> st
     | Complete ist -> do processInput game ist
                       st

let state = Atom(NoInput)
let update game ms keys = 
    state.Swap (fun x -> 
        let cycled = cycle ms keys x
        procSideEffects game cycled)  