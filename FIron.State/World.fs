module World

open Geom
open FSharpx
open World


let initState stack = {
    stack = stack
    tables = MapTables.init stack
}

let mapStack f state = {
    state with stack = f state.stack
}
let mapTables f state = {
    state with tables = f state.tables
}

module Core = 
    let moveCre id p state = 
        if Grid.inBounds1 p state.stack.cre then
            Option.maybe {
                let! pos = Map.tryFind id state.tables.crePos
                let! cre = Map.tryFind id state.tables.cre
                let! stack = MapStack.moveCre cre pos p state.stack
                let tables = MapTables.moveCre id p state.tables
                Path.newWalk stack.walk
                return cre, {
                    stack = stack
                    tables = tables
                }   
            }
        else None


type RefreshEvent = 
     | Vis

let refresh event state = 
    let stack = state.stack
    let tables = state.tables
    match event with
     | Vis -> let cleared = MapStack.clearVisibility stack
              let playerPositions = tables.players |> Map.toSeq |> Seq.map (Tup.flip >> Tup.mapsnd (flip Map.find tables.crePos))
              let ns = MapStack.updateVisibility playerPositions cleared
              mapStack (konst ns) state

type Msg = 
     | Get of AsyncReplyChannel<World>
     | Init of MapStack.Stack
     | Refresh of RefreshEvent
     | MoveCre of int * pt * AsyncReplyChannel<bool>



let actor = Actor.Start(fun box ->
    let initial = empty
    let rec loop st = async {
        let! msg = box.Receive()
        match msg with 
         | Get r -> r.Reply st; return! loop st
         | Init s -> return! loop (initState s)
         | Refresh r -> let ns = refresh r st
                        return! loop ns
         | MoveCre (i, p, r) -> 
            let nso = Core.moveCre i p st |> Option.map snd
            r.Reply(Option.isSome nso)
            return! loop (nso <??> st)
    }
    loop initial)

let getAsync() = actor.PostAndAsyncReply(Get)
let get() = actor.PostAndReply(Get)
let init st = actor.Post(Init st)
let refreshVis() = actor.Post(Refresh Vis)
let moveCreAsync i p = actor.PostAndAsyncReply(fun r -> MoveCre (i, p, r))
let moveCre i p = actor.PostAndReply(fun r -> MoveCre (i, p, r))
