module World


open FSharp.Data
open FSharpx

open Util
open World
open File

type ThemeJson = JsonProvider< @"..\FIronCS.Data\Data\themes\castle.json" >
let parseTheme ss str = Option.maybe {
    let! x = Option.protect ThemeJson.Parse str
    let name = x.Name
    let f, w, wd = Tup.map3 List.ofArray (x.Floor, x.Wall, x.WallDecor)
    let! floor = Option.mapM (flip Map.tryFind ss) f
    let! wall = Option.mapM (flip Map.tryFind ss) w
    let! wallDecor = Option.mapM (flip Map.tryFind ss) wd
    return {
        name = name
        floor = floor
        wall = wall
        wallDecor = wallDecor
    }
}
let loadTheme ss = loadf (parseTheme ss)
let loadAllThemes ss = loadAll (loadTheme ss) Dir.themeDir  
let themeLookup themes = 
    themes
    |> Seq.map (fun r -> r.name, r)
    |> Seq.distinctBy fst
    |> Map.ofSeq
let loadThemeMap ss = loadAllThemes ss |> Option.map themeLookup