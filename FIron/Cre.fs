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

       
let cid cre = cre.id
let isPlayer cre = cre.isPlayer