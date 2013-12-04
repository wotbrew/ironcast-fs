module State.Xna
open FSharpx
open Util
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

type Disp = | Put of (unit -> unit)
            | Get of AsyncReplyChannel<(unit -> unit) list>

let dispatchQueue =
     MailboxProcessor.Start(fun box ->
        let rec loop lst =  async {
            let! x = box.Receive()
            match x with
             | Put f -> return! loop (lst @ [f])
             | Get r -> do r.Reply(lst)
                        return! loop []
            return! loop lst
        }
        loop [])

let dispatch f = 
    dispatchQueue.Post(Put f)

let dispatchRAsync f = 
    let r = ref None
    dispatchQueue.Post(Put (fun () -> r := Some (f())))
    let rec await() = async {
        do! Async.Sleep(1)
        match !r with
         | Some x -> return x
         | None -> return! await()
    }
    await()
let dispatchR f = 
    (dispatchRAsync f |> Async.StartAsTask).Result

module Game = 
  let mutable game:Game = null

module Gfx = 
  let mutable manager:GraphicsDeviceManager = null 
  let mutable blank:Texture2D = null
  let device man = if manager = null then null else manager.GraphicsDevice

  let commuteMan f = dispatch (fun () -> if manager = null |> not then f manager)
  let commuteDevice f = commuteMan (device >> f)
  
  let initBlank() = 
    commuteDevice (fun gd -> 
        let t = new Texture2D(gd, 1, 1)
        t.SetData<Color>([|Color.White|])
        blank <- t)

  let changeResolution (x,y) = 
        commuteMan
         (fun m -> m.PreferredBackBufferWidth <- x
                   m.PreferredBackBufferHeight <- y)
  let fullscreen yes = 
        commuteMan (fun m -> m.IsFullScreen <- yes)
  let applyAll() = commuteMan (fun m -> m.ApplyChanges())
   

  module Foreground =
    type Item = 
          | Spr of (Sprite * rect * Color)
    
    let agent = Agent(List.empty<Item>)
    let clear() = send agent (konst [])
    let put x = send agent (fun lst -> x :: lst)
    let puts x = send agent (fun lst -> x @ lst)
    let get() = agent.Value
                    
    let inline rsprite 
     (sb:SpriteBatch)
     (t:Tex, s:rect)
     (r:rect) 
     (c:Color) = sb.Draw(t, r, nullable s, c)

    let character db rect = Spr ((Map.find "humanMale" db), rect, Color.White)
    let dcharacter db rect = character db rect |> put
    
    let renderItem sb f = 
        match f with 
        | Spr (s, r, c) -> rsprite sb s r c


let currentResolution() = 
    let m = Gfx.device Gfx.manager
    m.Viewport.Width, m.Viewport.Height                    