module Grid

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Util
open Geom
open FSharpx
open FSharpx.Collections
open Data
open Rand

type Grid<'a when 'a : equality> = {
    vector : Vector<'a>
    size: int * int
}
let empty = {
    vector = Vector.empty
    size = 0, 0
}
type Cell<'a> = pt * 'a

let inline size g = g.size
let inline vector g = g.vector

let inline indmap x y g = (x + (y * snd g.size))

let create (w,h) v = {
    vector = Vector.init (w * h) (konst v)
    size = w,h
}

let inline get x y g = Vector.nth (indmap x y g) g.vector
let inline get1 (pt:pt) g = get pt.X pt.Y g
let inline cell x y g = (pt(x,y), get x y g)
let inline cell1 (pt:pt) g = (pt, get1 pt g)    

let cells g = 
    let w,h = g.size
    seq {
        for x in 0 .. w-1 do
        for y in 0 .. h-1 do
            yield pt(x,y), get x y g
    }

let filterCells f g = cells g |> Seq.filter (fun (a, b) -> f b)
let someCells g = cells g |> Seq.filter (snd >> Option.isSome) |> Seq.map (Tup.mapsnd Option.getOrDefault)

let inline inBounds x y g =
    let w,h = g.size
    x >= 0 && x < w && y >= 0 && y < h            

let inline inBounds1 (pt:pt) g = inBounds pt.X pt.Y g

let inline mapVector f g = {vector = f g.vector; size = g.size}
let inline update x y v g = mapVector (Vector.update (indmap x y g) v) g
let inline update1 (pt:pt) v g = update pt.X pt.Y v g
let inline updateCell (pt, v) g = update1 pt v g

let updateAll cells g = Seq.fold (fun st cell -> updateCell cell st) g cells 

let ofCells1 def sz cells = 
    let g = create sz def
    updateAll cells g

let ofCells def cells = 
    let sz = Seq.map fst cells |> Pt.max |> Pt.add2 1 |> Pt.toPair
    ofCells1 def sz cells

let map f v = mapVector (Vector.map f) v

let inline sizeRect g = 
    let w, h = g.size
    rect(0,0,w,h)

let ofArray2D arr = 
    let sz = Array2D.dim arr
    {
        vector = Vector.ofSeq (Array2D.flatten arr)
        size = sz
    }

    
let floors g = 
    cells g
    |> Seq.filter (snd >> not)
let walls g = 
    cells g
    |> Seq.filter snd

let inline isWall g p = get1 p g : bool
let inline notWall g p = isWall g p |> not

let innerWalls g =
    let inBounds = flip inBounds1 g
    walls g
    |> Seq.filter (fun (p, v) ->
        Pt.adj p |> Seq.filter inBounds |> Seq.exists (notWall g))

let innerWallsAndFloors g = floors g |> Seq.append <| innerWalls g

//data required to draw a grid
type DrawData = {
    viewport:rect
    expl:Grid<bool>
    vis:Grid<bool>
    cellSize:int
}



 

module Terr = 
    open Dungeon
    let create rand t g = 
        g |> map (fun b ->
            if b then List.randn t.wall rand
            else List.randn t.floor rand)

let viewPortIter data f g =
    let vp = data.viewport
    let dm = data.expl
    let cs = data.cellSize  
    let mw, mh = g.size
    let sx = max 0 ((vp.X / cs) - 1);
    let sy = max 0 ((vp.Y / cs) - 1);
    let w = (vp.Width / cs) + 1;
    let h = (vp.Height / cs) + 1;
    let right = min (mw-1) (sx + w)
    let bottom = min (mh-1) (sy + h)
    for x = sx to right do
        for y = sy to bottom do
            let c = if get x y data.vis then Color.White else Color.Gray
            if get x y dm then f x y c g

/// draw quickly a grid of sprites
let fastDraw sb data g =
    let cs = data.cellSize
    let inline f x y c g =
         let spr = get x y g
         Res.Sprite.draw sb spr x y 32 c
    viewPortIter data f g

/// draw quickly a grid of sprite options
let fastDraw1 sb data g =
    let cs = data.cellSize
    let inline f x y c g =
       match get x y g with
       | Some spr -> Res.Sprite.draw sb spr x y 32 c
       | None -> ()
    viewPortIter data f g