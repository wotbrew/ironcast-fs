module Js.Api
open Js.Core
open State
open FSharpx
open Util

module Options =
 open User
 let getSettings = jsfun0 (fun () -> Settings.get() |> jssettings)
 let setSettings = Option.silenceIn Settings.set |> jsproc1 fssettings
 let getResolutions = jsfun0 (fun () -> Db.get().allRes |> jsallRes)
 let all = 
     ["getSettings", getSettings
      "setSettings", setSettings
      "getResolutions", getResolutions]


module Drawing = 
 open Xna
 open Gfx
 let clear = jsproc0 Foreground.clear
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
     "drawChar", drawChar]

let iron = 
     Options.all @
     Drawing.all

module Awe = 
    open Awesomium.Core
    let init (awe:WebView) = 
      do awe.LoadHTML("<html><head><title>Loading...</title></head><body></body></html>") |> ignore
      while not awe.IsDocumentReady do WebCore.Update()
      let o : JSObject = JSValue.op_Implicit(awe.CreateGlobalJavascriptObject("iron"))
      mutObj iron o