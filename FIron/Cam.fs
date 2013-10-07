module Cam

open Geom
open Microsoft.Xna.Framework

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
    /// the current world offset of the camera
    pos : vec
}


/// move the camera by to world position given by v (clamped)
let move v cam = let (min, max) = cam.offsets
                 let pos = vec.Clamp(v, min, max)
                 {cam with pos = pos}
let shift v cam = move (v + cam.pos) cam

/// get a transform matrix for the camera
let discMatrix cam = let v3 = Vector3(cam.pos.X, cam.pos.Y, 0.0f)
                     let n = v3 / 32.0f;
                     let n2 = Vector3(int n.X |> float32, int n.Y |> float32, int n.Z |> float32)
                     Matrix.CreateTranslation(n2*32.0f) //32's are nasty
let matrix cam =
                 let v3 = Vector3(cam.pos.X, cam.pos.Y, 0.0f)
                 Matrix.CreateTranslation(v3)

let transform v cam = vec.Transform(v, matrix cam)
let world v cam = let inv = matrix cam |> Matrix.Invert
                  vec.Transform(v, inv)
let vecsize cam = let w, h = cam.size
                  vec(float32 w, float32 h)
let centre cam = vecsize cam / 2.0f
let centreOn v cam = let v2 = -v + centre cam
                     move v2 cam
let offsetForMapSize (vw, vh) (mw, mh) cs =
        let v1 = vec(float32 vw, float32 vh)
        let v2 = vec(float32 (mw * cs), float32 (mh * cs))
        v1 - v2, vec.Zero
let viewport cam = 
        let w,h = cam.size
        let r = rect(0, 0, w, h)
        Rect.move2 (cam.pos * -1.0f) r

let empty = {
            size = 0, 0
            offsets = vec.Zero, vec.Zero
            pos = vec.Zero
        }  

/// transform a rect into world co-ordinates (by cells)
let mapRect cam cs rect =
    let world = world (Rect.locv rect) cam
    let loc = world / float32 cs
    let bounds = Rect.sizev rect / float32 cs
    Rect.ofVecs loc bounds
