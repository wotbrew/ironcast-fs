module Doors
open Rand
open Geom
open FIronCS
open FSharpx

//
//       
//let isDoorCandidate g pt = Tile.isCorridor g pt && Grid.adj g pt |> Seq.filter (Tile.isFloor g) |> Seq.length >= 4
//let doorCandidates g = Grid.pts g |> Seq.filter (isDoorCandidate g)
//let doorCandidatesSet g =
//    let candidates = doorCandidates g |> List.ofSeq
//    let rec candify set = function 
//        | [] -> set
//        | x :: xs -> let adjc = Grid.adjc g x |> Seq.map (Pt.toPair)
//                     if Seq.exists (flip Set.contains set) adjc then
//                        candify set xs
//                     else 
//                        candify (Set.add (Pt.toPair x) set) xs
//    candify Set.empty candidates |> Seq.map Pt.ofPair
//
//let doorGrid g theme = 
//    let candidates = doorCandidates g |> Seq.map (fun pt -> pt, Some <| Use.defaultDoor theme) |> List.ofSeq
//    Grid.ofCells1 None (Grid.size g) candidates
