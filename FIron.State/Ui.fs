module Ui

open Microsoft.Xna.Framework.Input

open FSharpx
open Geom
open Ui

let state = Atom(Ui.empty)

let select id = 
    state.Swap (Ui.select id)
let selectOnly id = 
    state.Swap (Ui.selectOnly id)

