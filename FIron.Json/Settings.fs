module Settings
open Util
open FSharpx
open FSharp.Data
open Geom
open Input
open File
open Settings

let parseResolution s = Option.maybe {
    let! r = String.split s 'x' |> Array.whenLength 2
    let! x = Int.parse r.[0]
    let! y = Int.parse r.[1]
    return x, y
}

type ResolutionsJson = JsonProvider< "..\FIronCS.Data\Data\\resolutions.json" >
type VideoJson = JsonProvider< "..\FIronCS.Data\Data\\video.json" >
type KeysJson = JsonProvider< "..\FIronCS.Data\Data\\keys.json" >

let parseResolutions str = 
    Option.protect ResolutionsJson.Parse str
    |> Option.bind (List.ofSeq >> Option.mapM parseResolution)
    
    
let loadResolutions = loadf parseResolutions


let parseButton = function
    | "left" | "LEFT" -> Left
    | _ -> Right

let parseKey str = Option.tryParseWith XnaKey.TryParse str : XnaKey option
/// parse a keys.json string
let parseKeys str = Option.maybe {
    let! keys = Option.protect KeysJson.Parse str
    let! up,left,down,right,speed =  Array.choose parseKey keys.Cam |> Tup.ofArr5
    return {
        cam = 
            {
                up = up
                down = down
                left = left
                right = right
                speedMod = speed
            }
        select = parseButton keys.Select
        selectManyMod = parseKey keys.SelectManyMod <??> XnaKey.LeftShift
        move = parseButton keys.Move
    }
}

/// load a keys.json from a file
let loadKeys = loadf parseKeys

/// parse a video.json string
let parseVideo str = Option.maybe {
    let! x = Option.protect VideoJson.Parse str
    let! res = parseResolution x.Resolution
    return {
        resolution = res
        fullscreen = x.Fullscreen
        cellSize = 32.0f
    }
}

/// load a video.json from a file
let loadVideo = loadf parseVideo
    
let writeResolution (w, h) = Write.String (string w + "x" + string h)

let writeVideo video =
    Write.Obj ["resolution", writeResolution video.resolution
               "fullscreen", Write.Bool video.fullscreen]

let saveVideo =
        writeVideo
        >> Write.write
        >> IO.writeStringToFile false File.video