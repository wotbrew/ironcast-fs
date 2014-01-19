module World


open FSharp.Data
open FSharpx

open Util
open World
open File

let findAll ss x = Option.mapM (flip Map.tryFind ss) x

//type ThemeJson = JsonProvider< @"..\FIronCS.Data\Data\themes\castle.json" >
//
//
//let parseTheme ss str = Option.maybe {
//    let! x = Option.protect ThemeJson.Parse str
//    let name = x.Name
//    let f, w, wd = Tup.map3 List.ofArray (x.Floor, x.Wall, x.WallDecor)
//    let! floor = findAll ss f
//    let! wall = findAll ss w
//    let! wallDecor = findAll ss wd
//    return {
//        name = name
//        floor = floor
//        wall = wall
//        wallDecor = wallDecor
//    }
//}
//let loadTheme ss = loadf (parseTheme ss)
//let loadAllThemes ss = loadAll (loadTheme ss) Dir.themeDir  
//let themeLookup themes = 
//    themes
//    |> Seq.map (fun r -> r.name, r)
//    |> Seq.distinctBy fst
//    |> Map.ofSeq
//let loadThemeMap ss = loadAllThemes ss |> Option.map themeLookup