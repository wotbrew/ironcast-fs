module Cam

open Microsoft.Xna.Framework

open Geom
open Cam


let shiftWithDelta directions speed = 
    let nspeed = float32 (Time.mdelta * 400.0) * speed
    shifty directions nspeed


let state = Atom(empty)
let get() = state.Value
let swap = swap state
                     