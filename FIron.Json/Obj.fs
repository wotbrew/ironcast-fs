module Obj
open Obj
open FSharp.Data
open FSharpx
open FSharpx.Option
open File

type DoorJson = JsonProvider< @"..\FIronCS.Data\Data\objects\door.json" >

let parseDoor ss json =
    Option.maybe {
        let! json = Option.protect DoorJson.Parse json
        let! opened = Map.tryFind json.Open ss
        let! closed = Map.tryFind json.Closed ss
        return 
            {
                name = json.Name
                opened = opened
                closed = closed 
                isOpen = json.IsOpen
            }
    }

type ObjJson = JsonProvider< @"..\FIronCS.Data\Data\objects\obj.json" >

let dispatchObj ss rawjson (json:ObjJson.DomainTypes.Entity) = 
    match json.Type with
     | "door" -> ObjDoor <!> parseDoor ss rawjson
     | _ -> None

let parseObj ss json = 
    Option.maybe {
        let! parsed = Option.protect ObjJson.Parse json
        return! dispatchObj ss json parsed
    }

let loadObj ss = loadf (parseObj ss)
let loadAllObjects ss = loadAll (loadObj ss) Dir.objDir
let objLookup = 
    Seq.map (fun o -> objName o, o)
    >> Seq.distinctBy fst
    >> Map.ofSeq
let loadObjectMap ss = loadAllObjects ss |> Option.map objLookup
