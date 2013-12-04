module Settings 

open Util
open FSharpx
open Settings
open Geom


let agent = Agent(defaultSettings)

let setVideoF vid settings = 
    newVideo.Add vid
    {settings with video = vid}

let setKeysF keys settings = 
    newKeys.Add keys
    {settings with keys = keys}

let sendF f = f >> send agent

let setVideo = sendF setVideoF
let setKeys = sendF setKeysF