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
    if mouseClick button st && World.canWalk pos game.world then
       Move.creatures pos game.ui.selected

let updateLasso (game:Gs.Gs) st = 
    let button = game.settings.keys.select
    if mousePressed button st then 
       Ui.state.Swap (Ui.mapLasso (Ui.Lasso.updateFromMouse (mousePt st)))
    else
       Ui.state.Swap (Ui.mapLasso (konst Ui.Lasso.empty))

let processInput (game:Gs.Gs) input = 
    let keys = game.settings.keys
    moveCam keys.cam input
    Option.maybe {
        let! mapMouse = mapMouse game input
        do updateLasso game input
        do moveParty mapMouse game input
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