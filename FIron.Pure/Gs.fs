module Gs

type Gs = {
    world : World.World
    ui : Ui.Ui
    db : Db.Db
    cam : Cam.Camera
    settings : Settings.Settings
}

let worldVec game vec =
    let mapvec = Cam.world vec game.cam
    mapvec / game.settings.video.cellSize

let getCre pt game = World.getCre pt game.world