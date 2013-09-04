module Geom

open Microsoft.Xna.Framework;
open FSharpx
open Util

type pt = Point
type vec = Vector2
type rect = Rectangle

type Rectangle with
    member r.BottomRight = pt(r.X + r.Width, r.Y + r.Height)

type Direction = | North
                 | NorthEast
                 | East
                 | SouthEast
                 | South
                 | SouthWest
                 | West
                 | NorthWest

module Dir = 
    /// Transform a direction to a unit point
    let inline pt d =
        match d with
            | North -> pt(0, -1)
            | NorthEast -> pt(1, -1)
            | East -> pt(1, 0)
            | SouthEast -> pt(1, 1)
            | South -> pt(0, 1)
            | SouthWest -> pt(-1, 1)
            | West -> pt(-1, 0)
            | NorthWest -> pt(-1, -1)
    /// Transform a direction to a unit vector
    let inline vec d = let p = pt d
                       in vec(float32 p.X, float32 p.Y)
    /// A set of all the directions
    let all = [North; NorthEast; East; SouthEast; South; SouthWest; West; NorthWest] |> set
    /// A set if all cardinal directions
    let cardinal = [North; East; South; West;] |> set
    let cardinalp = Seq.map pt all |> Array.ofSeq
    let allp = Seq.map pt all |> Array.ofSeq
    /// A set of all diagonal directions
    let diag = Set.difference all cardinal
    let diagp = Seq.map pt diag |> Array.ofSeq

module Pt =
    let inline map fx fy (a:pt) = pt(fx a.X, fy a.Y)
    let inline map2 f x y (a:pt) = pt(f a.X x, f a.Y y)
    let inline ofVec (pt:pt) = vec(float32 pt.X, float32 pt.Y)
    let inline of2 x y = pt(x, y)
    let inline ofPair (x, y) = pt(x, y)
    let inline add1 x y a = map2 (+) x y a
    let inline add a (b:pt) = add1 b.X b.Y a
    let inline add2 n a = add1 n n a
    let inline sub1 x y a = map2 (-) x y a
    let inline sub a (b:pt) = sub1 b.X b.Y a
    let inline sub2 a n = sub1 n n a
    let inline mult1 x y a = map2 (-) x y a
    let inline mult a (b:pt) = mult1 b.X b.Y a
    let inline mult2 n a = mult1 n n a
    let inline multf x y (a:pt) = pt(float32 a.X * x |> int, float32 a.X * y |> int)
    let inline multf2 n a = multf n n a
    let inline div1 x y a = map2 (/) x y a
    let inline div a (b:pt) = div1 b.X b.Y a
    let inline div2 n a = div1 n n a
    let inline divf x y (a:pt) = pt(float32 a.X / x |> int, float32 a.Y / y |> int)
    let inline divf2 n a = divf n n a
    let inline adj a = Seq.map (add a) Dir.allp
    let inline adjc a = Seq.map (add a) Dir.cardinalp
module Rect =
    open FSharpx.Option
    let inline move x y (r:rect) = rect(x, y, r.Width, r.Height)
    let inline move1 (pt:pt) r = move pt.X pt.Y r
    let inline origin r = move 0 0 r
    let inline shift x y (r:rect) = rect(r.X + x, r.Y + y, r.Width, r.Height)
    let inline stretch w h (r:rect) = rect(r.X, r.Y, r.Width + w, r.Height + h)
    let inline intersects (a:rect) (b:rect) = a.Intersects b
    let inline intersects1 a b = intersects b a
    let inline contains (a:rect) (b:rect) = a.Contains(b)
    let inline contains1 a b = contains b a
    let inline ofPts (loc:pt) (wh:pt) = rect(loc.X, loc.Y, wh.X, wh.Y)
    let inline ofTup (x, y, w, h) = rect(x, y, w, h)
    let inline map f (r:rect) = seq {for x = r.X to r.Right - 1 do
                                       for y = r.Y to r.Bottom - 1 do
                                         yield f x y} 
    let inline pts r = map Pt.of2 r 
    let inline iter f (r:rect) = for x = r.X to r.Right - 1 do
                                    for y = r.Y to r.Bottom - 1 do
                                        f x y
    let inline edges (r:rect) = seq { 
        for x = r.X to r.Right - 1 do
         yield pt(x, r.Y)
         yield pt(x, r.Bottom-1)
        for y = r.Y to r.Height - 1 do
         yield pt(r.X, y)
         yield pt(r.Right-1, y)
    }
                  
    let parse s = maybe {
        let! split =  String.split s ' ' 
                      |> List.ofArray  
                      |> List.whenLength 4
                      >>= Option.mapM Int.parse
        return rect(split.[0], split.[1], split.[2], split.[3])
    }
   


