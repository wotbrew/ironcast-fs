module Util
open FSharpx


let inline nullable x = Nullable.create x

module Int = 
    let parse = Option.tryParseWith System.Int32.TryParse
module Float = 
    let parse = Option.tryParseWith System.Double.TryParse

module Tup = 
    let pairWith b a = (a, b)
    let mapall4 f (a,b,c,d) = (f a, f b, f c, f d)
    let inline mapsnd f (a,b) = a, f b
    let inline mapfst f (a,b) = f a, b
    let ofList4 = 
        function
        | [a;b;c;d] -> Some (a,b,c,d)
        | _ -> None
    
module Seq = 
    let notEmpty s = not <| Seq.isEmpty s
    let whenAny xs = Option.ofBoolAndValue (notEmpty xs, xs)
    let product a b = Seq.collect (fun x -> Seq.map (tuple2 x) b) a
module List =
    let tail = function
                | [] -> []
                | x :: xs -> xs
    let clamp n tk xs = if List.length xs > n then
                            Seq.take tk xs |> List.ofSeq
                        else xs
    let whenLength n xs = 
            if List.length xs = n then Some xs
            else None
                        
module Option =
    let ofNil a = if a = null then None else Some a 
    let inline protect f x = 
        try
            Some <| f x
        with e -> None
    let orFail str = Option.getOrElseF (fun () -> failwith str)
    let silenceIn f = fun x -> match x with | Some a -> f a | None -> ()
    let orElseF f = 
        function 
        | None -> f()
        | x -> x
module Choice =
    let mapMLazy f xs = 
        let rec loop acc ele = 
            match ele with 
            | [] -> acc
            | x :: xs -> let c = f x
                         match c with
                         | Choice.Choice2Of2 b -> Choice2Of2 b
                         | Choice.Choice1Of2 a -> loop (Choice.returnM (a :: Choice.get acc)) xs
        loop (Choice.returnM []) xs
        |> Choice.map List.rev

module String = 
    let isNullOrWhitespace s = System.String.IsNullOrWhiteSpace s
    let isNullOrEmpty s = System.String.IsNullOrEmpty s
    let split (s:string) (c:char) = s.Split(c)
    let lines s = split s '\n'
    let unlines xs = String.concat "\n" xs
    let trim (s:string) = s.Trim()
    let startsWith a (s:string) = s.StartsWith(a)

module StringSeq = 
    let filterWhitespace s = Seq.filter (not << String.isNullOrWhitespace) s
    let trim s = Seq.map String.trim s

module Array = 
    let whenLength n xs = 
            if Array.length xs = n then Some xs
            else None
    let ofArray2D a = let l1 = Array2D.length1 a
                      let l2 = Array2D.length2 a
                      let r = Array.zeroCreate l2
                      for y = 0 to l2 - 1 do 
                        r.[y] <- Array.init l1 (fun x -> a.[x, y])
                      r
module Array2D = 
    let flatten a = let l1 = Array2D.length1 a
                    let l2 = Array2D.length2 a
                    let r = Array.zeroCreate (l1 * l2)
                    for x = 0 to l1 - 1 do
                        for y = 0 to l2 - 1 do
                        r.[y * l2 + x] <- a.[x, y]
                    r

module IO = 
    let tryReadFile f = Option.protect IO.readFileAsString f

type Agent<'T> = MailboxProcessor<'T>
let stateAgent init f =
    Agent.Start(fun box ->
        let state = init
        let rec loop st = async {
            let! c = box.Receive()
            let nst = f st c
            return! loop st
        }
        loop state)
        
let lazyStateAgent init f = 
    Agent.Start(fun box ->
        let state = init
        let rec loop st = async {
            let! c = box.Receive()
            let nst = f (Lazy.force st) c
            return! loop st
        }
        loop state)

type SimpleState<'a> = | Get of (AsyncReplyChannel<'a>)
                       | Set of 'a

let simpleStateAgent init set = 
    stateAgent init 
     (fun st c -> 
        match c with 
         | Get r -> r.Reply(st); st
         | Set a -> set a)

let simpleLazyStateAgent init set = 
    lazyStateAgent init 
     (fun st c -> 
        match c with 
         | Get r -> r.Reply(st); st
         | Set a -> set a)