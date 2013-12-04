module Move

open FSharpx
open Geom

type Path = pt list
type CreId = {
    id : int
    isPlayer : bool
}
let isPlayer cid = cid.isPlayer
type PathState = {
    paths : Map<CreId, Path>
}
let empty = {
    paths = Map.empty
}

let printPath path = 
    match path with 
     | [] -> printfn "(empty)"
     | _ -> 
        for pt in path do
         printfn "%O" pt 

type World = World.World
module Core = 
    let addPath i pth st = {st with paths = Map.add i pth st.paths}
    let pathFind (ms:World) i (pt:pt) = 
        Option.maybe {
            let! pos = ms.tables.crePos |> Map.tryFind i
            let path = Path.findPath pos pt
            return path
        }
    let moveTo i (p:pt) world = 
        let isPlayer = World.isPlayer i world
        let creid = {
            id = i
            isPlayer = isPlayer
        }
        let path = pathFind world i p
        match path with 
            | Some pth -> Some (creid, pth)
            | _ -> None

type Msg =  
    | MoveTo of int * pt
    | ClearPaths
    | TickPaths

module Box =
    let post x (box:Actor<'a>) = box.Post(x)

    let tickPaths st = 
        let paths = Seq.filter (snd >> List.notEmpty) (st.paths |> Map.toSeq) |> List.ofSeq
        let all = 
            seq {
                for i, pth in paths do
                    yield async {
                        let! didMove = World.moveCreAsync i.id pth.Head
                        return i, didMove, if didMove then Seq.skip 1 pth |> List.ofSeq else pth |> List.ofSeq
                    }
            } 
            |> Async.Parallel |> Async.StartAsTask
        let anyPlayers = all.Result |> Seq.exists (fun (i, dm, pth) -> i.isPlayer && dm)
        if anyPlayers then
          do World.refreshVis()
        all.Result |> Seq.map (fun (i, dm, pth) -> i,pth) |> Map.ofSeq

let actor = Actor.Start(fun box ->
    let rec loop st = async {
        let! msg = box.Receive()
        match msg with
         | MoveTo (i, p) ->
            let! world = World.getAsync()
            let path = Core.moveTo i p world
            return! loop (path |> Option.map (fun (i, p) -> Core.addPath i p st) <??> st)
         | ClearPaths -> return! loop {st with paths = Map.empty}
         | TickPaths ->
            let newPaths = Box.tickPaths st
            return! loop {st with paths = newPaths}
    }
    loop empty)

let tick() = actor.Post(TickPaths)
let clear() = actor.Post(ClearPaths)
let creature pt i = actor.Post(MoveTo (i, pt))
let creatures pt = Seq.iter (creature pt)