module MapTables
open Geom
open Util
open FSharpx
open FSharpx.Collections
open MapStack

type Tables = {
    cre : Map<int, Cre.Creature>
    crePos : Map<int, pt>
}

let empty = {
    cre = Map.empty
    crePos = Map.empty
}

let mapCrePos f tables = {
    tables with 
        crePos = f tables.crePos 
}

let clearCre id = mapCrePos (Map.remove id)
let placeCre id pt = mapCrePos (Map.add id pt)
let moveCre id pt = clearCre id >> placeCre id pt

let creCells (stack:MapStack.Stack) = Grid.someCells stack.cre

let initCre stack = 
    creCells stack
    |> Seq.map (snd >> Tup.dup >> Tup.mapfst Cre.cid)
    |> Map.ofSeq

let initCrePos stack = 
    creCells stack
    |> Seq.map (Tup.flip >> Tup.mapfst Cre.cid)
    |> Map.ofSeq

let init stack = {
        cre = initCre stack
        crePos = initCrePos stack
    }