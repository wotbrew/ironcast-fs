module State.Path
open Util
open FSharpx
open Geom
type Path = pt list

type Msg = 
    | FindPath of pt * pt * AsyncReplyChannel<Path>
    | NewWalk of Grid.Grid<bool>
    

module Core = 
    let shouldFindPath walk (a:pt) (b:pt) =
         let inBounds = Array2D.inBounds a.X a.Y walk && Array2D.inBounds b.X b.Y walk
         inBounds && walk.[b.X, b.Y]
    let pathfind (pather:FIronCS.PathFinderFast) a b = pather.FindPath(a,b)
    let findPath pather walk a b =
        if shouldFindPath walk a b then
            pathfind pather b a //switch to avoid list reversal
            |> Option.ofNil
            |> Option.map (Seq.skip 1 >> Seq.map (fun nd -> pt(nd.X, nd.Y)) >> List.ofSeq)
        else None

        

let agent = Agent.Start(fun box ->
    let pather = ref null : FIronCS.PathFinderFast ref
    let walk = ref null : bool[,] ref
    let rec loop() = async {
        let! msg = box.Receive()
        match msg with
         | FindPath (a,b,r) ->
            let path = Core.findPath !pather !walk a b
                       |> Option.getOrElse []
            r.Reply(path)
            return! loop()
         | NewWalk g ->
            walk := Grid.toArray2D g
            pather := FIronCS.PathFinderFast(!walk)
            return! loop()
    }
    loop())

let findPathAsync a b = agent.PostAndAsyncReply(fun r -> FindPath (a, b, r))
let findPath a b = agent.PostAndReply(fun r -> FindPath (a, b, r))
let newWalk g = agent.Post(NewWalk g)