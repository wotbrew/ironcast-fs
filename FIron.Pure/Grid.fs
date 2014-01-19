module Grid

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Util
open FIronCS
open Geom
open FSharpx
open FSharpx.Collections

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

/// gets the value at x y
let inline get g x y = Vector.nth (indmap x y g) g.vector
/// gets the value at point pt
let inline get1 g (pt:pt) = get g pt.X pt.Y
let inline get2 g (vec:vec) = get1 g (Pt.ofVec vec)

let inline inBounds g x y =
    let w,h = g.size
    x >= 0 && x < w && y >= 0 && y < h            

let inline inBounds1 g (pt:pt) = inBounds g pt.X pt.Y

let whereInBounds1 g = Seq.filter (inBounds1 g)

let adj g = Dir.adj >> whereInBounds1 g
let adjc g = Dir.adjc >> whereInBounds1 g

let getSafe g x y = 
    if inBounds g x y then Some (get g x y) else None
let getSafe1 g (p:pt) = getSafe g p.X p.Y               

/// gets the values in the given rect 
let getIn g rect = 
    Rect.mapPts (get1 g) rect
let getInSafe g rect = 
    Rect.mapPts (getSafe1 g) rect

/// gets the cell at x y
let inline cell g x y = (pt(x,y), get g x y)
/// gets the cell at point pt
let inline cell1 g (pt:pt) = (pt, get1 g pt)    
/// gets the cells in the given rect
let cellsIn g rect = 
    Rect.mapPts (cell1 g) rect

/// gets all the cells in a grid
let cells g = 
    let w,h = g.size
    seq {
        for x in 0 .. w-1 do
        for y in 0 .. h-1 do
            yield pt(x,y), get g x y
    }

let filterCells f g = cells g |> Seq.filter (fun (a, b) -> f b)
let someCells g = cells g |> Seq.filter (snd >> Option.isSome) |> Seq.map (Tup.mapsnd Option.getOrDefault)



let inline mapVector f g = {vector = f g.vector; size = g.size}
let inline update x y v g = mapVector (Vector.update (indmap x y g) v) g
let inline update1 (pt:pt) v = update pt.X pt.Y v
let inline updateWith (pt:pt) f g = update pt.X pt.Y (f (get g pt.X pt.Y)) g
let inline updateCell (pt, v) = update1 pt v

let updateAll g cells = Seq.fold (flip updateCell) g cells
let updateAllPts g pts value = updateAll g (Seq.map (Tup.pairWith value) pts)

let ofCells1 def sz cells = 
    let g = create sz def
    updateAll g cells

let ofCells def cells = 
    let sz = Seq.map fst cells |> Pt.max |> Pt.add2 1 |> Pt.toPair
    ofCells1 def sz cells

///get the cells of grid g, where the cell value is equal to v    
let cellsEqualTo v = cells >> Seq.filter (snd >> ((=) v))

let map f g = mapVector (Vector.map f) g
let iteri f g = 
    let w, h = g.size
    for y = 0 to h - 1 do
        for x = 0 to w - 1 do
            do f x y (get g x y)
    
let rows g = 
    let w, h = g.size
    seq {
    for y = 0 to h - 1 do 
        yield [for x in 0 .. w - 1 do yield get g x y]
    }

        
let mapi f g = 
    let w, h = g.size
    let vector =
        seq {
            for y = 0 to h - 1 do
                for x = 0 to w - 1 do
                    yield f x y (get g x y)
        } |> Vector.ofSeq
    mapVector (konst vector) g

let gridAnd g1 g2 = 
    mapi (fun x y v -> v && getSafe g2 x y <??> false) g1
           

let inline sizeRect g = 
    let w, h = g.size
    rect(0,0,w,h)
let borderPts g = sizeRect g |> Rect.edges
let pts g = sizeRect g |> Rect.pts

let ofArray2D arr = 
    let sz = Array2D.dim arr
    {
        vector = Vector.ofSeq (Array2D.flatten arr)
        size = sz
    }

let toArray2D grd = 
    let w,h = grd.size
    Array2D.init w h (get grd)




