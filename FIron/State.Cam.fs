module State.Cam

open Microsoft.Xna.Framework

open Geom
open Util
open FSharpx

/// various control settings used for the camera
type CameraCtrl = {
    /// the hotzone in pixels for mouse camera movemnt
    edge : int
    /// the number of pixels used for each movement increment
    pixels : int
    fastMult : float32
    slowMult : float32
}

type Camera = {
    /// the size of the camera viewport
    size : int * int
    /// maximum and minimum given offsets for the camera (clamp range)
    offsets : vec * vec
    /// the current world position of the camera
    pos : vec
}



/// move the camera by to world position given by v (clamped)
let move v cam = let (min, max) = cam.offsets
                 let pos = vec.Clamp(v, min, max)
                 {cam with pos = pos}
let shift v cam = move (v + cam.pos) cam
let shiftd d cam = let v = Dir.vec d * float32 (Time.mdelta * 400.0)
                   shift v cam
/// get a transform matrix for the camera
let matrix cam = let v3 = Vector3(cam.pos.X, cam.pos.Y, 0.0f)
                 Matrix.CreateTranslation(v3)
let transform v cam = vec.Transform(v, matrix cam)
let world v cam = let inv = matrix cam |> Matrix.Invert
                  vec.Transform(v, inv)
let viewport cam = let w, h = cam.size
                   vec(float32 w, float32 h)
let centre cam = viewport cam / 2.0f
let centreOn v cam = let v2 = -v + centre cam
                     move v2 cam
let offsetForMapSize (vw, vh) (mw, mh) cs =
        let v1 = vec(float32 vw, float32 vh)
        let v2 = vec(float32 (mw * cs), float32 (mh * cs))
        v1 - v2, vec.Zero

type MapSize = int * int
type ScreenSize = int * int
type Command = | Shift of Direction list
               | Move of vec
               | GetState of AsyncReplyChannel<Camera>
               | Resize of ScreenSize * MapSize * int

let agent = 
    stateAgent {
            size = 0, 0
            offsets = vec.Zero, vec.Zero
            pos = vec.Zero
        } 
        (fun st c -> 
            match c with
             | GetState r -> r.Reply(st); st
             | Shift ds -> Seq.fold (flip shiftd) st ds
             | Move v -> move v st
             | Resize (s, m, cs) -> {st with offsets = offsetForMapSize s m cs})
            


    