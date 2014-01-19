module Gs
open FSharpx
open Geom

type Gs = {
    world : World.World
    ui : Ui.Ui
    db : Db.Db
    cam : Cam.Camera
    settings : Settings.Settings
}

let inline world gs = gs.world

let mapWorld f gs = {gs with world = f gs.world}
let mapStack = mapWorld << World.mapStack
let mapStackAt p f = mapStack (flip f p)

let worldVec game vec =
    let mapvec = Cam.world vec game.cam
    mapvec / game.settings.video.cellSize

let worldRect game rect = 
    Cam.mapRect game.cam game.settings.video.cellSize rect

let getCre = world >> World.getCreSafe
let getCreIn = world >> World.getCreInSafe
let getObj = world >> World.getObjSafe
let getActions = world >> World.getActionsSafe

let lassoedCreatures game =
    let cs = game.settings.video.cellSize
    let maprect = Cam.mapRect game.cam cs game.ui.lasso.rect |> Rect.shift -1 -1 |> Rect.stretch 2 2
    getCreIn game maprect