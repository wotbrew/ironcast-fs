module Builder
open Geom
open Util
open Rand
open FSharpx

type RoomSpec = {seed:rand; maxIter:int; count:int; width:int*int; height:int*int; gridSize:int*int}
let defRoomSpec = {seed = rand(); maxIter = 10000; count = 38; width = (4, 14); height = (4, 14); gridSize = (64, 64)} 

let maxGridSize rs = let gs = Seq.map (fun r -> r.gridSize) rs
                     (Seq.map fst gs |> Seq.max, Seq.map snd gs |> Seq.max)

let inAny r rms = Seq.exists (Rect.shift -1 -1 >> Rect.stretch 2 2 >> Rect.intersects1 r) rms

let genRooms (r:RoomSpec) = let (w, h) = r.gridSize
                            let limit = rect(1, 1, w-2, h-2)
                            let rec genrooms c mi rms = 
                                if mi = 0 then rms
                                else if c = 0 then rms
                                else 
                                     let loc = randPtIn limit r.seed
                                     let nr = randRect2 r.width r.height r.seed |> Rect.move1 loc
                                     if (not <| inAny nr rms) && Rect.contains limit nr then
                                        genrooms (c-1) (mi-1) (nr :: rms)
                                     else genrooms (c-1) (mi-1) rms
                            genrooms r.count r.maxIter []

let rpath 
    (pf:FIronCS.PathFinderFast) 
    r1 r2 seed = 
        let st = randPtIn r1 seed
        let e = randPtIn r2 seed
        let path = pf.FindPath(st, e)
        if not (path = null) then seq path else Seq.empty

let genDungeon rs seed = 
                    let (gw, gh) = maxGridSize rs
                    let grid = Array2D.create<bool> gw gh true
                    let gr = rect(0, 0, gw, gh)
                    Rect.edges gr |> Seq.iter(fun p -> grid.[p.X, p.Y] <- false)
                    let pf = new FIronCS.PathFinderFast(grid)
                    pf.Diagonals <- false
                    let rooms = List.collect genRooms rs
                    let paths = seq {
                        for r, r2 in Seq.product rooms rooms do
                            for pn in rpath pf r r2 seed do
                             yield pt(pn.X, pn.Y)
                    }
                    do Seq.collect Rect.pts rooms
                       |> Seq.append paths
                       |> Seq.iter (fun pt -> grid.[pt.X, pt.Y] <- false)
                    //Seq.iter (Rect.iter (fun x y -> grid.[x, y] <- false)) rooms
                    Rect.edges gr |> Seq.iter(fun p -> grid.[p.X, p.Y] <- true)
                    grid

let drawGrid g = Array2D.map (fun x -> if x then "#" else "_") g 
                 |> Array.ofArray2D |> Seq.map (String.concat "") |> String.concat "\n"                  
                           

module Decor =
    let tilesMod = 12
    /// take a bool map and pick random wall tiles to decorate.
    let walls g decor rate r = 
        let walls = Grid.walls g |> List.ofSeq
        let take = List.length walls / rate
        if take < 1 then
            Grid.create g.size None
        else
        let randomized = List.randomize walls r |> Seq.take take
        randomized
        |> Seq.map (fun (pt,v) -> pt, Some <| List.randn decor r)
        |> Grid.ofCells None
    let walls1 a decor r = walls a decor tilesMod r

