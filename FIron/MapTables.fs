module MapTables
open Geom
open Util
open FSharpx
open FSharpx.Collections
open MapStack

type Tables = {
    cre : Map<int, Cre.Creature>
    crePos : Map<int, pt>
    players : Map<int, Cre.Creature>
}
    
let playerPositions tables =
    tables.players |> Map.toSeq |> Seq.map (Tup.flip >> Tup.mapsnd (flip Map.find tables.crePos))
let creaturePositions tables = 
    tables.cre |> Map.toSeq |> Seq.map (Tup.flip >> Tup.mapsnd (flip Map.find tables.crePos))
    

let empty = {
    cre = Map.empty
    crePos = Map.empty
    players = Map.empty
}

let mapCrePos f tables = {
    tables with 
        crePos = f tables.crePos 
}

let clearCre id = mapCrePos (Map.remove id)
let placeCre id pt = mapCrePos (Map.add id pt)
let moveCre id pt = clearCre id >> placeCre id pt

let delCre id tables = 
    {
        tables with
            cre = Map.remove id tables.cre
            crePos = Map.remove id tables.crePos
            players = Map.remove id tables.players
    }
let addPlayer cre tables = 
    {
        tables with players = Map.add (Cre.cid cre) cre tables.players
    }

let initCre stack = 
    creCells stack
    |> Seq.map (snd >> Tup.dup >> Tup.mapfst Cre.cid)
    |> Map.ofSeq

let initCrePos stack = 
    creCells stack
    |> Seq.map (Tup.flip >> Tup.mapfst Cre.cid)
    |> Map.ofSeq

let initPlayers stack =
    creCells stack
    |> Seq.filter (snd >> Cre.isPlayer)
    |> Seq.map (snd >> Tup.dup >> Tup.mapfst Cre.cid)
    |> Map.ofSeq

let init stack = {
        cre = initCre stack
        crePos = initCrePos stack
        players = initPlayers stack
    }