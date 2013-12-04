module Time

open Microsoft.Xna.Framework

type Time = {
    delta : float
    lastDelta : float
    fps : float
}
let defaultTime = {
    delta = 0.0
    lastDelta = 0.0
    fps = 0.0
}
/// mmmmmm....
let mutable mdelta = 0.0

type GT = GameTime
let delta (gt : GT) = gt.ElapsedGameTime.TotalSeconds
let smoothDelta gt ld = delta gt * 0.9 + ld * 0.1

let time gt =
     let delta = delta gt
     {delta = delta; lastDelta = delta; fps = 1.0 / delta}
let updateTime gt time =     
     let delta = smoothDelta gt time.delta
     mdelta <- delta
     {delta = delta; lastDelta = time.delta; fps = 1.0 / delta}
