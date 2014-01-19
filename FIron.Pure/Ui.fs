module Ui

open Geom
open Util
open Microsoft.Xna.Framework.Input
open FSharpx
open FSharpx.Collections

module Js = 
    type Call = string
    module Interact =
        let call func args = sprintf "window.ui.interact.%s(" func + String.concat "," args + ")"
        let show (pt:pt) = call "show" [string pt.X; string pt.Y]

module Lasso = 
    type Lasso = {
        last : Lasso option
        rect : rect
        released : bool
    }

    let empty = {
        last = None
        rect = rect(0,0,0,0)
        released = false
    }

    let rect l = l.rect
    let last l = l.last

    let lastLassoRect s = Option.map rect s.last <??> Rect.def
    let isLassoing s = 
        let r = rect s
        Rect.w r > 10 && Rect.h r > 10

    let wasLassoing = last >> Option.option false isLassoing
    let isInitialState l = l.last.IsNone && (not <| isLassoing l)
    let lassoReleased l = l.released

    let updateFromMouse pt l = 
        if isInitialState l then 
            {l with rect = Pt.toRect pt 0 0; last = Some l }
        else
        let loc = Rect.loc l.rect
        let bounds = Pt.sub pt loc
        let r = Rect.ofPts loc bounds
        {
            last = Some l
            rect = r
            released = false
        }

    let release lasso = {lasso with released = true}
        

type Ui = {
    selected : Set<int>
    lasso : Lasso.Lasso
    actions: string list
    jscalls: Js.Call list
}
let empty = {
    selected = Set.empty
    lasso = Lasso.empty
    actions = []
    jscalls = []
}


let inline mapSelected f st = {st with selected = f st.selected}
let inline mapLasso f st = {st with lasso = f st.lasso}
let inline mapCalls f st = {st with jscalls = f st.jscalls}
let inline mapActions f st = {st with actions = f st.actions}
let inline selected ui = ui.selected

let updateFromMouse pt = mapLasso (Lasso.updateFromMouse pt)

let selectNone = mapSelected (konst Set.empty)
let select id = mapSelected (Set.add id)
let selectOnly id = mapSelected (konst (Set.singleton id))
let selectOnlyMany ids = mapSelected (konst ids)
let selectMany ids = mapSelected (Seq.fold2 Set.add ids)

let resetLasso = mapLasso (konst Lasso.empty)


let updateActions actions mappt ui = 
    let areEqual = ui.actions = actions
    if not areEqual then
        { 
          ui with 
            jscalls = Js.Interact.show mappt :: ui.jscalls
            actions = actions
        }
    else
        {ui with actions = actions}

    