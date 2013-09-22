module State.Event
open Util
open State.MapState
open FSharpx
open FSharpx.Collections

type MasterEvent = 
    | MoveTick

let fireEvent = function 
     | MoveTick -> State.Move.tick()

let runningEvents = 
    [
        0.2, MoveTick
    ]


let cycle mi mx x = if x > mx then mi
                    elif x < mi then mi
                    else x

type Push = Push of float * AsyncReplyChannel<unit>
let eventLoop = Agent.Start(fun box ->
    let rec loop ticks = async {
        let! Push (delta, r) = box.Receive()
        let ticks = 
            [for ctr, v, event in ticks do
                if ctr + delta > v then
                    do fireEvent event
                    yield 0.0, v, event
                else yield ctr + delta, v, event]
        r.Reply()
        return! loop ticks
    }
    loop (runningEvents |> List.map (fun (b,c) -> 0.0, b, c)))

let pushAsync delta = eventLoop.PostAndAsyncReply(fun r -> Push(delta, r))