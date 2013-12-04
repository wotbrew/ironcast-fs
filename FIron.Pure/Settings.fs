module Settings
open Util

type CamKeys = {
    up: XnaKey
    down: XnaKey
    left: XnaKey
    right: XnaKey
    speedMod: XnaKey
}

type Keys = {
    cam : CamKeys
    select : Button
    selectManyMod : XnaKey
    move : Button
}

type Video = {
    resolution : int * int
    fullscreen : bool
    cellSize : float32
}

type Settings = {
    video : Video
    keys : Keys
}

let defaultVideo = {
     resolution = 1024, 768
     fullscreen = false
     cellSize = 32.0f
    }

let defaultKeys = {
    cam = {
            up = XnaKey.W
            left = XnaKey.A
            right = XnaKey.D
            down = XnaKey.S
            speedMod = XnaKey.LeftShift
          }
    select = Left
    selectManyMod = XnaKey.LeftShift
    move = Left
}


let defaultSettings = {
    video = defaultVideo
    keys = defaultKeys
}
