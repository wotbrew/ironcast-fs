module Cre

open Util
open FSharpx
open Geom
open Res

type Creature = {
    id: int
    body: sprite list
}

let inline draw sb x y cs c cre = 
    for spr in cre.body
        do Sprite.draw sb spr x y cs c

let sightRange cre = 20

        
open Grid
let inline fastDraw sb data g =
    let cs = data.cellSize
    let inline f x y c g = 
        match Grid.get x y g with
        | Some cre -> draw sb x y cs c cre
        | None -> ()
    Grid.viewPortIter data f g

let cid cre = cre.id