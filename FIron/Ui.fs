module Ui

open Geom
open Util
open FSharpx

module Lasso = 
    type Lasso = {
        last : Lasso option
        rect : rect
    }

    let rect l = l.rect
    let last l = l.last

    let lastLassoRect s = Option.map rect s.last <??> Rect.def
    let isLassoing s = 
        let r = rect s
        Rect.w r > 10 && Rect.h r > 10

    let wasLassoing = last >> Option.option false isLassoing
    let lassoReleased l = wasLassoing l && not <| isLassoing l

    let updateFromMouse ms l = 
        let loc = Rect.loc l.rect
        let bounds = Pt.sub ms loc
        let r = Rect.ofPts loc bounds
        {
            last = Some l
            rect = r
        }
