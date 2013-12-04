module Ui

open Geom
open Util
open Microsoft.Xna.Framework.Input
open FSharpx
open FSharpx.Collections

module Lasso = 
    type Lasso = {
        last : Lasso option
        rect : rect
    }

    let empty = {
        last = None
        rect = rect(0,0,0,0)
    }

    let rect l = l.rect
    let last l = l.last

    let lastLassoRect s = Option.map rect s.last <??> Rect.def
    let isLassoing s = 
        let r = rect s
        Rect.w r > 10 && Rect.h r > 10

    let wasLassoing = last >> Option.option false isLassoing
    let isInitialState l = l.last.IsNone
    let lassoReleased l = wasLassoing l && not <| isLassoing l

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
        }

type Ui = {
    selected : Set<int>
    lasso : Lasso.Lasso
}
let empty = {
    selected = Set.empty
    lasso = Lasso.empty
}


let inline mapSelected f st = {st with selected = f st.selected}
let inline mapLasso f st = {st with lasso = f st.lasso}

let inline selected ui = ui.selected
let selectNone = mapSelected (konst Set.empty)
let select id = mapSelected (Set.add id)
let selectOnly id = mapSelected (konst (Set.singleton id))
let selectMany ids = mapSelected (Seq.fold2 Set.add ids)
