module Cre

open FSharp.Data
open FSharpx

open Util
open Cre
open File

type RaceJson = JsonProvider< @"..\FIronCS.Data\Data\races\human.json" >
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