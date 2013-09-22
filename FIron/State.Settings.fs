module State.Settings 
open Data
open Data.Settings
open Util
open FSharpx

let defaultSettings = {
     resolution = 1024, 768
     fullscreen = false
    }

let loadOrDefault() = loadSettings File.settings |> Option.getOrElse defaultSettings

let agent = 
    let set settings =
        let w, h = settings.resolution
        Cam.agent.Post(Cam.Resize ((w, h), (64, 64), 32))
        Xna.Gfx.changeResolution(w,h)
        Xna.Gfx.fullscreen settings.fullscreen
        Xna.Gfx.applyAll()
        Awe.resize (w,h)
        Async.Start(async {do Option.protect (fun () -> Data.Settings.saveSettings settings) () |> ignore})
        settings
    
    simpleLazyStateAgent 
        (lazy loadOrDefault())
        set
        
let get() = agent.PostAndReply(fun r -> Get r)
let set settings = agent.Post(Set settings)
