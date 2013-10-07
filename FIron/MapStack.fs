module MapStack

open Microsoft.Xna.Framework

open Util
open FSharpx
open FSharpx.Collections

open Grid
open Res
open Geom
    

type Stack = {
    light: Grid<bool>
    terr: Grid<sprite>
    dung: Grid<bool>
    walk: Grid<bool>
    decor: Grid<sprite option> 
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

let generate seed theme = 
    let dungeon = Builder.genDungeon [Builder.defRoomSpec] seed
                  |> Grid.ofArray2D
    let invdungeon = dungeon |> Grid.map not
    let wallsAndFloors = 
        Grid.innerWallsAndFloors dungeon
        |> Seq.map (Tup.mapsnd (konst true))
    {
        light = dungeon
        terr = Grid.Terr.create seed theme dungeon
        decor = Builder.Decor.walls1 dungeon theme.wallDecor seed
        walk = invdungeon
        dung = invdungeon
        expl = Grid.ofCells1 false dungeon.size wallsAndFloors 
        vis = Grid.create dungeon.size false
        cre = Grid.create dungeon.size None
    }
    
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
    else 
    #if DEBUG
    do printfn "Could not move creature %i to %i,%i" cre.id b.X b.Y 
    #endif
    None

/// get the creature at the given point
let getCre p ms = ms.cre |> Grid.get1 p

/// find the creatures in the given rect
let creIn rect ms =
    Rect.pts rect
    |> Seq.map (flip getCre ms)
    |> Seq.choose id
     



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
        

        