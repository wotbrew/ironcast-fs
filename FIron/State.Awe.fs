module State.Awe

open Util
open FSharpx
open Geom
open AwesomiumXNA
open Awesomium.Core
open Js.Core

open Res

type Awe = AwesomiumComponent
type AweCore = WebView

let init game (w,h) = 
    let awe = new AwesomiumComponent(game, rect(0,0,w,h)) 
    let wv = awe.WebView
    do wv.ParentWindow <- game.Window.Handle
       wv.ConsoleMessage.Add(fun c -> printf "%s" c.Message)
       game.Components.Add(awe)
    awe

type Msg = 
     | Resize of (int * int)
     | SetGlobal of string * (string * JSVal) list
     | SetSource of System.Uri
     | GetTex of AsyncReplyChannel<tex option>

// awesomium functions should be completely protocted by agent
// as not thread safe
let agent = 
    let empty = "<html><head><title>Loading...</title></head><body></body></html>"
    let resize (awe:Awe) (w ,h) = awe.Area <- rect(0, 0, w, h)
    let setGlobal (awe:AweCore) name glob = 
        do awe.LoadHTML(empty) |> ignore
        while not awe.IsDocumentReady do WebCore.Update()
        let o : JSObject = JSValue.op_Implicit(awe.CreateGlobalJavascriptObject(name))
        mutObj glob o


    Agent.Start(fun box ->
        let init = lazy ((fun () -> init Xna.Game.game (Xna.currentResolution()))
                         |> Xna.dispatchR)
        let rec loop (a:Lazy<Awe>) = async {
            let! r = box.Receive()
            let awe = Lazy.force a
            let wv = awe.WebView
            match r with
             | Resize sz -> (fun () -> resize awe sz) |> Xna.dispatch
             | SetGlobal (n, o) -> (fun () -> setGlobal wv n o |> ignore) |> Xna.dispatch
             | SetSource s -> (fun () -> wv.Source <- s) |> Xna.dispatch
             | GetTex r -> r.Reply(Option.ofNil awe.WebViewTexture) //should be safish?!
            return! loop a
        }
        loop init)

let resize sz = agent.Post(Resize sz)
let tex() = agent.PostAndReply(fun r -> GetTex r)
let setSource s = agent.Post(SetSource s)
let setGlobal n o = agent.Post(SetGlobal (n, o))