module Gs
open Util
open Awesomium.Core
open AwesomiumXNA
open FSharpx
open Microsoft.Xna.Framework;

module Pages = 
    let urify s = (IO.currentDirectory + s).ToUri()
    let main = urify @"/Gui/main.html"
    let newGame = urify @"/Gui/newgame.html"

//module HtmlMut = 
    //let mutable current : System.Uri = null

let parseRes s = let x = String.split s 'x'
                 (int x.[0], int x.[1])

//module Js = 
//    let loadFunctions (awe:WebView) xs = 
//      do awe.LoadHTML("<html><head><title>Loading...</title></head><body></body></html>") |> ignore
//      while not awe.IsDocumentReady do WebCore.Update()
//      let o : JSObject = JSValue.op_Implicit(awe.CreateGlobalJavascriptObject("iron"))
//      for n,callback in xs
//        do o.Bind(n, true, (fun x y -> callback x y))
////                 
//    let setFullscreen (gdm:GraphicsDeviceManager)
//                      (awe:AwesomiumComponent)
//                      o (e:JavascriptMethodEventArgs) =
//        let fs = e.Arguments.[0].ToString() = "true"
//        (fun () ->
//            if fs = gdm.IsFullScreen |> not then
//                gdm.IsFullScreen <- fs
//                gdm.ApplyChanges()) |> State.Xna.dispatch
//
//    let setRes (gdm:GraphicsDeviceManager)
//               (awe:AwesomiumComponent)
//               o (e:JavascriptMethodEventArgs) =
//        let w, h = e.Arguments.[0].ToString() |> parseRes
//        Cam.agent.Post(Cam.Resize ((w, h),(64,64),32))
//        (fun () -> 
//            gdm.PreferredBackBufferWidth <- w
//            gdm.PreferredBackBufferHeight <- h
//            gdm.ApplyChanges()
//            awe.Area <- Geom.rect(0, 0, w, h)) |> State.Xna.dispatch
//
//    let drawChar db o (e:JavascriptMethodEventArgs) = 
//         let r = e.Arguments.[0].ToString() |> Geom.Rect.parse
//         match r with 
//          | Some r -> Gui.Draw.dcharacter db r
//          | _ -> ()
//
//    let testing (db:Data.Db)
//                (gdm:GraphicsDeviceManager) 
//                (awe:AwesomiumComponent) = 
//        let functions = ["setRes", setRes gdm awe
//                         "getAllRes", (fun o e -> e.Result <- db.allRes |> Js.User.jsallRes |> Js.toJsValue)
//                         "getSettings", (fun o e ->  e.Result <- db.settings |> Js.User.jssettings |> Js.toJsValue)
//                         "setFullscreen", setFullscreen gdm awe
//                         "clear", (fun o e -> Gui.Draw.clear())
//                         "drawChar", drawChar db.sprites]
//        do loadFunctions (awe.WebView) functions
        

module Html =
    let empty = "<html><head><title></title></head><body style='background-color:transparent'></body></html>"
//    let refreshCurrent (awe:WebView) = 
//     if HtmlMut.current = awe.Source |> not then
//         if HtmlMut.current = null then
//             awe.LoadHTML(empty) |> ignore
//         else awe.Source <- HtmlMut.current
        

type Init = {
    awe : WebView
}

type Mode = | MainMenu
            | InGame

type Command = Initialize of Init

