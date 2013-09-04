module State.Db

open Data
open Util
open FSharpx

let loadOrFail gd = loadDb gd |> Option.orFail "Could not load database"

let agent = 
    simpleLazyStateAgent 
        (lazy loadOrFail Xna.Gfx.manager.GraphicsDevice)
        id
        
let get() = agent.PostAndReply(fun r -> Get r)
let set db = agent.Post(Set db) 

