[<AutoOpen>]
module Util
open FSharpx
open FSharpx.Collections
open Microsoft.Xna.Framework.Input
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

type Button = Left | Right
type XnaKey = Keys

type pt = Point
type vec = Vector2
type rect = Rectangle
type Tex = Texture2D
type Sprite = Tex * rect
type Sheet = string * ((string * rect) list)

let blockingQueue<'a> = 
    let queue = new System.Collections.Concurrent.ConcurrentQueue<'a>()
    new System.Collections.Concurrent.BlockingCollection<'a>(queue)

let inline nullable x = Nullable.create x
let inline delay f x = fun () -> f x
let inline (<??>) a b = Option.getOrElse b a

let inline alt a b x = a x || b x
let inline (<||>) a b = alt a b

let inline refswap ref f = 
    let v = !ref
    ref := f v
    
type Atom<'a>(value:'a) = 
    let r = ref value
    member x.Swap(f) = 
        System.Threading.Monitor.Enter(r)
        refswap r f
        System.Threading.Monitor.Exit(r)
    member x.Value = !r

let swap (a:Atom<'a>) f = a.Swap f
let inline atomSwap a f = swap a f

type Agent<'a>(value: 'a) = 
    let r = ref value
    let mail = MailboxProcessor.Start(fun box -> 
        let rec loop() = async {
            let! f = box.Receive()
            r := f !r
            return! loop()
        }
        loop()
    )
    member x.Dispatch(f) = mail.Post(f)
    member x.Value = !r

let send (a:Agent<'a>) f = a.Dispatch f


module Int = 
    let parse = Option.tryParseWith System.Int32.TryParse
module Float = 
    let parse = Option.tryParseWith System.Double.TryParse

module Tup = 
    let inline map fa fb (a,b) = fa a, fb b
    let inline  dup a = a,a
    let inline  flip (a,b) = (b,a)
    let inline  pairWith b a = (a, b)
    let mapall4 f (a,b,c,d) = (f a, f b, f c, f d)
    let inline mapsnd f (a,b) = a, f b
    let inline mapfst f (a,b) = f a, b
    let inline map3 f (a,b,c) = f a, f b, f c
    let ofList4 = 
        function
        | [a;b;c;d] -> Some (a,b,c,d)
        | _ -> None
    let ofArr4 =
        function
        | [|a;b;c;d|] -> Some (a,b,c,d)
        | _ -> None
    let ofArr5 =
        function
        | [|a;b;c;d;e|] -> Some (a,b,c,d,e)
        | _ -> None
module Tup3 =
    let inline third (a,b,c) = c   
    let inline pair (a,b,c) = a,b
    let inline mapthird f (a,b,c) = a, b, f c
    let inline map fa fb fc (a,b,c) = fa a, fb b, fc c
module Seq = 
    let atLeast n f xs = (Seq.sumBy (fun x -> if f x then 1 else 0) xs > n)   
    let notEmpty s = not <| Seq.isEmpty s
    let whenAny xs = Option.ofBoolAndValue (notEmpty xs, xs)
    let product a b = Seq.collect (fun x -> Seq.map (tuple2 x) b) a
    let fold2 f seq st = Seq.fold (flip f) st seq
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
    let inline notEmpty xs = not <| List.isEmpty xs
    let inline cons x xs = x :: xs
                        
module Option =
    open FSharpx.Option
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
    let tuple3 (a,b,c) = maybe {
        let! sa = a
        let! sb = b
        let! sc = c
        return sa, sb, sc
    }

module Map = 
    let findAll m seq = Seq.choose (flip Map.tryFind m) seq

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
    let ofArray (arr:char[]) = new System.String(arr)

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
    let dim a = Array2D.length1 a, Array2D.length2 a
    let cells a = let l1, l2 = dim a
                  seq {
                    for x = 0 to l1 - 1 do
                    for y = 0 to l2 - 1 do 
                    yield x,y,a.[x,y]
                  }
    let flatten a = let l1, l2 = dim a
                    seq {
                        for x = 0 to l1 - 1 do
                         for y = 0 to l2 - 1 do 
                          yield a.[x,y]
                    }
    let flatten1 a = let l1, l2 = dim a
                     let r = Array.zeroCreate (l1 * l2)
                     for x = 0 to l1 - 1 do
                        for y = 0 to l2 - 1 do
                        r.[y * l2 + x] <- a.[x, y]
                     r
    let inline inBounds x y a = 
        x < Array2D.length1 a
        && y < Array2D.length2 a
        && x >= 0
        && y >= 0
    let inline fill v arr = arr |> Array2D.iteri (fun x y _ -> arr.[x,y] <- v)

module IO = 
    let tryReadFile f = Option.protect IO.readFileAsString f

type Actor<'T> = MailboxProcessor<'T>
let stateAgent init f =
    Actor.Start(fun box ->
        let state = init
        let rec loop st = async {
            let! c = box.Receive()
            let nst = f st c
            return! loop nst
        }
        loop state)
        
let lazyStateAgent init f = 
    Actor.Start(fun box ->
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

module Vector = 
   let ofArray2D arr = 
       Array2D.flatten arr
       |> Vector.ofSeq



let printAll all = 
    for e in all do
       printfn "%O" e