module State.Xna

open Util
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

type Disp = | Put of (unit -> unit)
            | Get of AsyncReplyChannel<(unit -> unit) list>

let dispatchQueue =
     Agent.Start(fun box ->
        let rec loop lst =  async {
            let! x = box.Receive()
            match x with
             | Put f -> return! loop (lst @ [f])
             | Get r -> do r.Reply(lst)
                        return! loop []
            return! loop lst
        }
        loop [])

let dispatch (f:(unit -> unit)) = dispatchQueue.Post(Put f)

module Gfx = 
  let mutable manager:GraphicsDeviceManager = null 
  let device man = if manager = null then null else manager.GraphicsDevice

  let commuteMan f = dispatch (fun () -> if manager= null |> not then f manager)
  let commuteDevice f = commuteMan (device >> f)
  let changeResolution (x,y) = 
        commuteMan
         (fun m -> m.PreferredBackBufferWidth <- x
                   m.PreferredBackBufferHeight <- y)
  let fullscreen yes = 
        commuteMan (fun m -> m.IsFullScreen <- yes)
  let applyAll() = commuteMan (fun m -> m.ApplyChanges())
  
  module Foreground =
    type Item = 
          | Spr of (Res.sprite * Geom.rect * Color)
    type FG = 
        | Put of Item
        | Puts of Item list
        | Get of AsyncReplyChannel<Item list>
        | Clear
                      
    let agent = 
         Agent.Start(fun box ->
            let rec loop lst =  async {
                let! x = box.Receive()
                match x with
                 | Clear -> return! loop []
                 | Put x -> return! loop (x :: lst)
                 | Puts x -> return! loop (x @ lst)
                 | Get r -> do r.Reply(lst)
                            return! loop lst
                return! loop lst
            }
            loop [])

    let getForegroundAsync() = agent.PostAndAsyncReply(fun r -> Get r)
    let getForeground() = agent.PostAndReply(fun r -> Get r)

    let inline rsprite 
     (sb:SpriteBatch)
     (t:Res.tex, s:Geom.rect)
     (r:Geom.rect) 
     (c:Color) = sb.Draw(t, r, nullable s, c)

    let character db rect = Spr ((Map.find "humanMale" db), rect, Color.White)
    let dcharacter db rect = agent.Post(Put <| character db rect)
    
    let clear() = agent.Post(Clear)

    let renderItem sb f = 
        match f with 
        | Spr (s, r, c) -> rsprite sb s r c