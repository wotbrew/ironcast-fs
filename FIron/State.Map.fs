module State.Map

open Geom
open Util
open FSharpx
 
type MapState = {
    stack : MapStack.Stack
    tables: MapTables.Tables
}

let empty = {
    stack = MapStack.empty
    tables = MapTables.empty
}

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
                let stack = MapStack.moveCre cre pos p state.stack
                let tables = MapTables.moveCre id p state.tables
                return cre, {
                    stack = stack
                    tables = tables
                }   
            }
        else None
    let moveParty p state = 
        Option.maybe {
            let! cre, st = moveCre 0 p state
            return mapStack (MapStack.clearVisibility >> MapStack.updateVisibility cre p) st
        }

type RefreshEvent = 
     | Vis

let refresh event stack = 
    match event with
     | Vis -> MapStack.clearVisibility stack

type Msg = 
     | Get of AsyncReplyChannel<MapState>
     | Init of MapStack.Stack
     | Refresh of RefreshEvent
     | MoveCre of int * pt
     | MoveParty of pt

let agent = Agent.Start(fun box ->
    let initial = empty
    let rec loop st = async {
        let! msg = box.Receive()
        match msg with 
         | Get r -> r.Reply st; return! loop st
         | Init s -> return! loop (initState s)
         | Refresh r -> let ns = mapStack (refresh r) st
                        return! loop ns
         | MoveCre (i, p) -> 
            let ns = Core.moveCre i p st |> Option.map snd |> Option.getOrElse st
            return! loop ns
         | MoveParty p ->
            let ns = Core.moveParty p st |> Option.getOrElse st
            return! loop ns
    }
    loop initial)

let getAsync() = agent.PostAndAsyncReply(Get)
let get() = agent.PostAndReply(Get)
let init st = agent.Post(Init st)
let refreshVis = agent.Post(Refresh Vis)
let moveCre i p = agent.Post(MoveCre (i, p))
let moveParty p = agent.Post(MoveParty p)
