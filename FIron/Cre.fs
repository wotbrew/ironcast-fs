module Cre

open Util
open FSharpx
open Geom
open Res
open Microsoft.Xna.Framework

type Creature = {
    id: int
    isPlayer: bool
    body: sprite list
}

let inline draw sb x y cs c cre = 
    for spr in cre.body
        do Sprite.draw sb spr x y cs c

let sightRange cre = 20

        
open Grid
let inline fastDraw sb data g =
    let cs = data.cellSize
    let inline f x y v g = 
        match Grid.get x y g, v with
        | Some cre, true -> draw sb x y cs Color.White cre
        | _ -> ()
    Grid.viewPortIter data f g

let cid cre = cre.id
let isPlayer cre = cre.isPlayer