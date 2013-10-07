module State.Ui

open Util
open FSharpx
open Geom

type State = {
    selected : Set<int>
}

let empty = {
    selected = Set.empty
}

module Core =
    let select i st = {st with selected = Set.add i st.selected}
    let selectOnly i st = {st with selected = Set.singleton i}
    let moveSelection st p = 
        for s in st.selected
            do Move.movePlayer s p

type Msg = 
    | Select of int
    | SelectOnly of int
    | Get of AsyncReplyChannel<State>
    | MoveSelection of pt


let agent = Agent.Start(fun box ->
    let rec loop st = async {
        let! msg = box.Receive()
        match msg with
         | Select i -> return! loop (Core.select i st)
         | SelectOnly i -> return! loop (Core.selectOnly i st)
         | MoveSelection p -> 
            do Core.moveSelection st p
            return! loop st
         | Get r ->
            r.Reply(st)
            return! loop st
    }
    loop empty)

let select i = agent.Post(Select i)
let selectOnly i = agent.Post(SelectOnly i)

let get() = agent.PostAndReply(Get)
let getAsync() = agent.PostAndAsyncReply(Get)

let moveSelection p = agent.Post(MoveSelection p)