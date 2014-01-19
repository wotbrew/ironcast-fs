module Walks
open Rand


let randomRectPath 
    (pf:FIronCS.PathFinderFast) 
    r1 r2 seed = 
        let st = randPtIn r1 seed
        let e = randPtIn r2 seed
        let path = pf.FindPath(st, e)
        if not (path = null) then seq path else Seq.empty

let findPaths walkgrid rooms seed = 
    let pf = new FIronCS.PathFinderFast(walkgrid)
    pf.Diagonals <- false
    seq {
        for r, r2 in Seq.product rooms rooms do
            yield [for pn in randomRectPath pf r r2 seed do yield pt(pn.X, pn.Y)]
    }
