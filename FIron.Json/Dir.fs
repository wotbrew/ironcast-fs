
module Dir

open FSharpx
open FSharp.Data

let path = IO.currentDirectory + "\\Data\\"
let rpath = IO.currentDirectory
let rdir d = IO.combinePaths rpath d |> IO.directoryInfo
let dir d = IO.combinePaths path d |> IO.directoryInfo
let tileDir = rdir "Tiles"
let raceDir = dir "races"
let themeDir = dir "themes"
let objDir = dir "objects"
