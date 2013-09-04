module Data

open Util
open FSharp.Data
open FSharpx

module Dir = 
    let path = IO.currentDirectory + "\\Data\\"
    let rpath = IO.currentDirectory
    let rdir d = IO.combinePaths rpath d |> IO.directoryInfo
    let dir d = IO.combinePaths path d |> IO.directoryInfo
    let tileDir = rdir "Tiles"
    let raceDir = dir "races"

module File = 
    let path = Dir.path
    let datafile f = IO.combinePaths path (f + ".json")
    let allRes = datafile "resolutions"
    let settings = datafile "settings"

module Write = 
    type JsonVal = | String of string
                   | Num of float
                   | Bool of bool
                   | Array of JsonVal list
                   | Obj of (string * JsonVal) list
                   | Null
    let quote a = "\"" + a + "\""
    let rec writei indent v = 
         match v with
            | String s -> quote s
            | Num f -> string f
            | Bool b -> b.ToString().ToLower()
            | Array e -> "[" + String.concat (",\n " + indent) (Seq.map (writei indent) e) + "]"
            | Obj o -> "{\n" + indent + String.concat (",\n" + indent)
                              (Seq.map (fun (a, b) -> indent + quote a + ": " + writei (indent + " ") b) o)  + "\n" + indent + "}"
            | Null -> "null"
    let write v = writei "" v
    




let loadf f file = IO.tryReadFile file
                   |> Option.bind f
let loadAll f dir = Option.protect IO.filesInDir dir
                    |> Option.bind (Seq.filter (fun fi -> fi.Extension = ".json")
                                    >> Seq.map (fun fi -> fi.FullName)
                                    >> List.ofSeq
                                    >> Option.mapM f )

module User =
    type Resolution = int * int
    let parseResolution s = Option.maybe {
        let! r = String.split s 'x' |> Array.whenLength 2
        let! x = Int.parse r.[0]
        let! y = Int.parse r.[1]
        return x, y
    }

    type ResolutionsJson = JsonProvider< "..\FIronCS\Data\\resolutions.json" >
    let parseResolutions str = 
        Option.protect ResolutionsJson.Parse str
        |> Option.bind (List.ofSeq >> Option.mapM parseResolution)
    
    
    let loadResolutions = loadf parseResolutions

    type Settings = {
        resolution : Resolution;
        fullscreen : bool
    }
    type SettingsJson = JsonProvider< "..\FIronCS\Data\settings.json" >

    /// parse a settings.json string
    let parseSettings str = Option.maybe {
        let! x = Option.protect SettingsJson.Parse str
        let! res = parseResolution x.Resolution
        return {
            resolution = res
            fullscreen = x.Fullscreen
        }
    }

    /// load a settings.json from a file
    let loadSettings = loadf parseSettings
    
    let writeResolution (w, h) = Write.String (string w + "x" + string h)
    let writeSettings 
        {
            resolution = res
            fullscreen = fs
        } =
        Write.Obj ["resolution", writeResolution res
                   "fullscreen", Write.Bool fs]
    
    let saveSettings =
         writeSettings
         >> Write.write
         >> IO.writeStringToFile false File.settings

module Cre = 
    type Race = {
        name : string
        spriteM : string * Res.sprite;
        spriteF : string * Res.sprite;
    }
    type RaceJson = JsonProvider< @"..\FIronCS\Data\races\human.json" >
    let parseRace ss str = Option.maybe {
        let! x = Option.protect RaceJson.Parse str
        let! spriteM = Map.tryFind x.SpriteMale ss
        let! spriteF = Map.tryFind x.SpriteFemale ss
        return {
            name = x.Name
            spriteM = x.SpriteMale, spriteM
            spriteF = x.SpriteFemale, spriteF
        }
    }
    let loadRace ss = loadf (parseRace ss)
    let loadAllRaces ss = loadAll (loadRace ss) Dir.raceDir
    let raceLookup races = 
        races 
        |> Seq.map (fun r -> r.name, r)
        |> Seq.distinctBy fst
        |> Map.ofSeq
    let loadRacesMap ss = loadAllRaces ss |> Option.map raceLookup
    


type Db = {
    allRes : User.Resolution list
    sprites : Map<string, Res.sprite>
    races : Map<string, Cre.Race>
}

let loadDb gd = Option.maybe {
    let! allRes = User.loadResolutions File.allRes
    let! sprites = Res.Sheet.spriteStore Dir.tileDir gd |> Choice.toOption
    let! races = Cre.loadRacesMap sprites
    return {
        allRes = allRes
        sprites = sprites
        races = races
    }
}