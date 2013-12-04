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


module Drawing = 
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
 let all = 
    ["clear", clear
     "drawChar", drawChar
     "moveChar", moveChar]

let iron = 
     Options.all @
     Drawing.all
