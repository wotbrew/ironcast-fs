module Decor
open Rand

let tilesMod = 12
/// take a bool map and pick random wall tiles to decorate.
let walls g decor rate r = 
    let walls = Tile.walls g |> List.ofSeq
    let take = List.length walls / rate
    if take < 1 then
        Grid.create g.size None
    else
    let randomized = List.randomize walls r |> Seq.take take
    randomized
    |> Seq.map (fun (pt,v) -> pt, Some <| List.randn decor r)
    |> Grid.ofCells None
let walls1 a decor r = walls a decor tilesMod r