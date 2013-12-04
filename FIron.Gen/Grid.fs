module Grid
open Rand

let createTerrain rand (t:World.Theme) g = 
    g |> Grid.map (fun b ->
        if b then List.randn t.wall rand
        else List.randn t.floor rand)