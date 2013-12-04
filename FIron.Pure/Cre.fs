module Cre

open Util
open FSharpx
open Geom
open Microsoft.Xna.Framework

type Race = {
    name : string
    spriteM : string * Sprite;
    spriteF : string * Sprite;
}

type Creature = {
    id: int
    isPlayer: bool
    body: Sprite list
}

    

let inline sightRange cre = 20

       
let inline cid cre = cre.id
let inline isPlayer cre = cre.isPlayer