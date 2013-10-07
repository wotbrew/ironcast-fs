module State.Cam

open Microsoft.Xna.Framework

open Geom
open Util
open FSharpx
open Cam


type MapSize = int * int
type ScreenSize = int * int

type Msg = | Shift of Direction
           | Move of vec
           | Centre of vec
           | GetState of AsyncReplyChannel<Camera>
           | Resize of ScreenSize * MapSize * int


let shiftd d cam = let v = Dir.vec d * float32 (Time.mdelta * 400.0)
                   shift v cam

let agent = 
    stateAgent {
            size = 0, 0
            offsets = vec.Zero, vec.Zero
            pos = vec.Zero
        } 
        (fun st c -> 
            match c with
             | GetState r -> r.Reply(st); st
             | Shift d -> shiftd d st
             | Centre v -> centreOn v st
             | Move v -> move v st
             | Resize (s, m, cs) -> {st with offsets = offsetForMapSize s m cs
                                             size = s})
            



let getAsync() = agent.PostAndAsyncReply(GetState)
    
                     