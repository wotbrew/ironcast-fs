module Js.Api
open Js.Core
open State
open FSharpx
open Util

module Options =
 open Settings
 let getVideo = jsfun0 (fun () -> Settings.agent.Value.video |> jsvideo)
 let setVideo = Option.silenceIn Settings.setVideo |> jsproc1 fsvideo
 let getResolutions = jsfun0 (fun () -> Db.get().allRes |> jsallRes)
 let all = 
     ["getVideo", getVideo
      "setVideo", setVideo
      "getResolutions", getResolutions]


module Gfx = 
 open Xna
 open Gfx
 let clear = jsproc0 Foreground.clear
 //int -> pt -> ()
 let moveChar = 
    jsproc2 fsint fspt (fun oi op ->
        Option.maybe {
            let! i = oi
            let! p = op
            return World.moveCre i p
        } |> ignore)  
 let drawChar = jsproc1
                 fsrect 
                 (fun ro -> 
                   match ro with 
                    | Some r -> Foreground.dcharacter
                                 (Db.get().sprites)
                                 r
                    | _ -> ())

 let screenPos = jsfun1 fspt (fun op ->
        Option.maybe {
            let! pt = op 
            let vec = Geom.Vec.ofPt pt
            let trans = Cam.transform vec Cam.state.Value |> Geom.Pt.ofVec
            return jspair (trans.X, trans.Y)
        } <??> JSVal.Null)
 let all = 
    ["clear", clear
     "drawChar", drawChar
     "moveChar", moveChar
     "screenPos", screenPos]

module Interact = 
    open Interact
    let at = jsfun1 fspt (fun pt ->
                Option.maybe {
                    let! ptx = pt
                    let actions = World.getActionsSafe (World.get()) ptx |> List.map jsstr
                    return jsarr actions
                } <??> jsarr [])
    //let hash = ["at", at]
    let all = ["interactAt", at]
        

let iron = 
     Options.all @
     Gfx.all @
     Interact.all
