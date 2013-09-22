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
    walk: Grid<bool>
    decor: Grid<sprite option> 
    expl: Grid<bool>
    vis: Grid<bool>
    cre: Grid<Cre.Creature option>
}

let empty = {
    light = Grid.empty
    terr = Grid.empty
    walk = Grid.empty
    decor = Grid.empty
    expl = Grid.empty
    cre = Grid.empty
    vis = Grid.empty
}


let generate seed theme = 
    let dungeon = Builder.genDungeon [Builder.defRoomSpec] seed
                  |> Grid.ofArray2D
    let wallsAndFloors = 
        Grid.innerWallsAndFloors dungeon
        |> Seq.map (Tup.mapsnd (konst true))
    {
        light = dungeon
        terr = Grid.Terr.create seed theme dungeon
        decor = Builder.Decor.walls1 dungeon theme.wallDecor seed
        walk = dungeon |> Grid.map not
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

    
let clearCre p = mapCre (Grid.update1 p None)
let placeCre cre p = mapCre (Grid.update1 p (Some cre))
let moveCre cre a b = clearCre a >> placeCre cre b


let draw sb vp cs stack =
     let {terr = terr
          walk = walk
          decor = decor
          expl = expl
          vis = vis
          cre = cre} = stack
     let gdraw = {
        expl = expl
        vis = vis
        viewport = vp
        cellSize = cs
     }
     Grid.fastDraw sb gdraw terr
     Grid.fastDraw1 sb gdraw decor
     Cre.fastDraw sb gdraw cre


let clearVisibility stack = withVis (Grid.create stack.terr.size false) stack
let updateVisibility cre pt stack = 
     let range = Cre.sightRange cre
     let lm = stack.light
     let pts = FIronCS.Geom.RaysWhileSkip(pt, (fun p -> Grid.inBounds1 p lm && (Grid.get1 p lm |> not)), range, 1, 0.3f)
     mapVis (Grid.updateAll (pts |> Seq.map (Tup.pairWith true))) stack
        

        