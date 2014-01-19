module Gs
open Gs

let state = Atom<Gs option>(None)
let swap f = state.Swap (Option.map f)
let update() = state.Swap (fun _ -> Some {
        db = Db.get()
        cam = Cam.state.Value
        ui = Ui.state.Value
        settings = Settings.agent.Value
        world = World.get()
    })
let get() = state.Value.Value

let getWorld() = state.Value.Value.world
let getStack() = getWorld().stack
