module State.Move

open Util
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

type MapState = State.MapState.MapState
module Core = 
    let pathFind (ms:MapState) i (pt:pt) = 
        #if DEBUG_MOVE
        printfn "Attempting move creature %i -> %O" i pt
        #endif
        Option.maybe {
            let! pos = ms.tables.crePos |> Map.tryFind i
            let path = State.Path.findPath pos pt
            #if DEBUG_MOVE
            printfn "Found creature %i, currently at %O" i pt
            printfn "Pathfind yielded: "
            printPath path
            #endif
            return path
        }

type Msg =  
    | AddPath of CreId * Path
    | MoveTo of int * pt
    | ClearPaths
    | TickPaths

module Box =
    let post x (box:Agent<'a>) = box.Post(x)

    let moveTo i (p:pt) box = 
            async {
                let! st = State.MapState.getAsync()
                let isPlayer = st.tables.players |> Map.containsKey i
                let creid = {
                    id = i
                    isPlayer = isPlayer
                }
                let path = Core.pathFind st i p
                match path with 
                 | Some pth -> post (AddPath(creid,pth)) box
                 | _ -> ()
            } |> Async.Start

    let tickPaths st = 
        let paths = Seq.filter (snd >> List.notEmpty) (st.paths |> Map.toSeq) |> List.ofSeq
        let all = 
            seq {
                for i, pth in paths do
                    yield async {
                        let! didMove = State.MapState.moveCreAsync i.id pth.Head
                        return i, didMove, if didMove then Seq.skip 1 pth |> List.ofSeq else pth |> List.ofSeq
                    }
            } 
            |> Async.Parallel |> Async.StartAsTask
        let anyPlayers = all.Result |> Seq.exists (fun (i, dm, pth) -> i.isPlayer && dm)
        if anyPlayers then
          do State.MapState.refreshVis()
        all.Result |> Seq.map (fun (i, dm, pth) -> i,pth) |> Map.ofSeq

let agent = Agent.Start(fun box ->
    let rec loop st = async {
        let! msg = box.Receive()
        match msg with
         | AddPath (i, p) -> return! loop {st with paths = Map.add i p st.paths}
         | MoveTo (i, p) ->
            do Box.moveTo i p box
            return! loop st
         | ClearPaths -> return! loop {st with paths = Map.empty}
         | TickPaths ->
            let newPaths = Box.tickPaths st
            return! loop {st with paths = newPaths}
    }
    loop empty)

let tick() = agent.Post(TickPaths)
let movePlayer i p = agent.Post(MoveTo (i, p))