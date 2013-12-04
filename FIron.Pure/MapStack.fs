module MapStack

open Microsoft.Xna.Framework

open Util
open FSharpx
open FSharpx.Collections

open Grid
open Geom
    

type Stack = {
    light: Grid<bool>
    terr: Grid<Sprite>
    dung: Grid<bool>
    walk: Grid<bool>
    decor: Grid<Sprite option> 
    expl: Grid<bool>
    cre: Grid<Cre.Creature option>
    vis: Grid<bool>
}

let empty = {
    light = Grid.empty
    terr = Grid.empty
    dung = Grid.empty
    walk = Grid.empty
    decor = Grid.empty
    expl = Grid.empty
    cre = Grid.empty
    vis = Grid.empty
}

let creCells stack = Grid.someCells stack.cre


let mapVis f stack = 
    {
        stack with vis = f stack.vis
    } : Stack
let withVis v stack = mapVis (konst v) stack

let mapCre f stack = {
        stack with cre = f stack.cre
    }
let mapWalk f stack = {
        stack with walk = f stack.walk
    }

    
let clearCre p stack = 
    mapCre (Grid.update1 p None) stack 
    |> mapWalk (fun x -> if get1 p stack.dung then Grid.update1 p true x else x)
let placeCre cre p = mapCre (Grid.update1 p (Some cre)) >> mapWalk (Grid.update1 p false)
let moveCre cre a b stack = 
    if get1 b stack.walk && get1 b stack.cre |> Option.isNone then 
        clearCre a stack |> placeCre cre b |> Some
    else None

/// get the creature at the given point
let getCre p ms = ms.cre |> Grid.get1 p
let getCreSafe p ms = ms.cre |> Grid.getSafe1 p |> Option.concat

/// find the creatures in the given rect
let creIn rect ms =
    Rect.pts rect
    |> Seq.map (flip getCre ms)
    |> Seq.choose id

/// is the given x y co-ords in the bounds of the mapstack     
let inBounds ms x y = Grid.inBounds x y ms.cre
/// is the given x y co-ords in the bounds of the mapstack
let inBounds1 ms pt = Grid.inBounds1 pt ms.cre

let clearVisibility stack = withVis (Grid.create stack.terr.size false) stack
let updateVisibility prs stack = 
     let lm = stack.light
     let pts = 
        [for cre, pt in prs do
             let range = Cre.sightRange cre
             yield async {
                return FIronCS.Geom.RaysWhileSkip(pt, (fun p -> Grid.inBounds1 p lm && (Grid.get1 p lm |> not)), range, 1, 0.3f)
             }]
        |> Async.Parallel |> Async.map Seq.concat |> Async.StartAsTask
     mapVis (Grid.updateAll (pts.Result |> Seq.map (Tup.pairWith true))) stack
        

        