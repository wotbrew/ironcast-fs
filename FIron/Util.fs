module Util
open FSharpx
open FSharpx.Collections


let inline nullable x = Nullable.create x
let inline delay f x = fun () -> f x
let inline (<??>) a b = Option.getOrElse b a
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
module Tup3 =
    let inline third (a,b,c) = c   
    let inline pair (a,b,c) = a,b
    let inline mapthird f (a,b,c) = a, b, f c
    let inline map fa fb fc (a,b,c) = fa a, fb b, fc c
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
    let inline notEmpty xs = not <| List.isEmpty xs
                        
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

type Agent<'T> = MailboxProcessor<'T>
let stateAgent init f =
    Agent.Start(fun box ->
        let state = init
        let rec loop st = async {
            let! c = box.Receive()
            let nst = f st c
            return! loop nst
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

module Vector = 
   let ofArray2D arr = 
       Array2D.flatten arr
       |> Vector.ofSeq

module IntervalTree =     
    module A = Array
    module L = List

    type interval = { lbound : float ;
                      rbound : float }

    type interval_tree =
        Empty
      | Node of
       (* x_mid   left_list       right_list      left_tree       right_tree *)
          float * interval list * interval list * interval_tree * interval_tree

    (* -------------------- utility functions -------------------- *)

    let new_interval l r =
      assert (l <= r);
      { lbound = l ;
        rbound = r }

    let leftmost_bound_first i1 i2 =
      compare i1.lbound i2.lbound

    let rightmost_bound_first i1 i2 =
      compare i2.rbound i1.rbound

    let is_before interval x_mid =
      interval.rbound < x_mid

    let contains interval x_mid =
      (interval.lbound <= x_mid) && (x_mid <= interval.rbound)

    let bounds_array_of_intervals intervals =
      let n   = L.length intervals  in
      let res = A.create (2 * n) 0. in
      let i   = ref 0               in
      L.iter
        (fun interval ->
           res.[!i] <- interval.lbound; incr i;
           res.[!i] <- interval.rbound; incr i)
        intervals;
      res

    let median1 xs =
      A.sortInPlace xs;
      let n = A.length xs in
      if n % 2 = 1 then
        xs.[n/2]
      else
        (xs.[n/2] + xs.[n/(2 - 1)] / 2.0)
    let median intervals =
      let bounds = bounds_array_of_intervals intervals
      median1 bounds

    let partition intervals x_mid =
      let left_intervals, maybe_right_intervals =
        L.partition
          (fun interval -> is_before interval x_mid)
          intervals in
      let mid_intervals, right_intervals =
        L.partition
          (fun interval -> contains interval x_mid)
          maybe_right_intervals in
       left_intervals, mid_intervals, right_intervals

    (* -------------------- construction -------------------- *)

    (* interval tree of a list of intervals WARNING: NOT TAIL RECURSIVE *)
    let rec interval_tree intervals =
      match intervals with
          [] -> Empty
        | _  ->
            let x_mid            = median intervals                 in
            let left, mid, right = partition intervals x_mid        in
            let left_list        = L.sortWith leftmost_bound_first mid in
            let right_list       = L.sortWith rightmost_bound_first mid in
            Node (x_mid,
                  left_list, right_list,
                  interval_tree left, interval_tree right)

    (* -------------------- query -------------------- *)

    (* fold_left f on l while p is true *)
    let rec fold_while f p acc l =
      match l with
          []      -> acc
        | x :: xs ->
            if p x then
              fold_while f p (f x :: acc) xs
            else
              acc

    let filter_left_list l qx acc =
      fold_while
        (fun x -> x)
        (fun interval -> interval.lbound <= qx)
        acc l

    let filter_right_list l qx acc =
      fold_while
        (fun x -> x)
        (fun interval -> interval.rbound >= qx)
        acc l

    (* find all intervals that contain qx *)
    let query initial_tree qx =
      let rec query_priv acc tree = 
        match tree with
            | Empty -> acc
            | Node (x_mid, left_list, right_list, left_tree, right_tree) ->
                if qx < x_mid then
                  let new_acc = filter_left_list  left_list  qx acc in
                  query_priv new_acc left_tree
                else
                  let new_acc = filter_right_list right_list qx acc in
                  query_priv new_acc right_tree
      in
      query_priv [] initial_tree

    let ofPairs pairs =
      interval_tree
        (Seq.fold
           (fun acc (a, b) -> (new_interval a b) :: acc)
           [] pairs)