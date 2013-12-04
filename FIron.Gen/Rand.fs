module Rand
open FSharpx
open Util
open Geom

type rand = System.Random

let between min max (r:rand) = r.Next(min, max)

let randFloat (r:rand) = r.NextDouble()
let randPt (min:pt) (max:pt) r = let x = between min.X max.X r
                                 let y = between min.Y max.Y r
                                 pt(x, y)

let randPtIn (re:rect) r = randPt re.Location re.BottomRight r
let randPt1 minx miny maxx maxy r = pt(between minx maxx r, between miny maxy r)
let randRect (min:rect) (max:rect) r = let l = randPt min.Location max.Location r
                                       let sz = randPt min.BottomRight max.BottomRight r
                                       Rect.ofPts l sz
let randRect1 minw minh mw mh r = rect(0, 0, between minw mw r, between minh mh r)
let randRect2 (minw, mw) (minh, mh) r = randRect1 minw minh mw mh r

module List =
    let randn xs r = let i = between 0 (List.length xs) r
                     xs.[i]
    let randomize xs r = List.sortBy (fun _ -> randFloat r) xs