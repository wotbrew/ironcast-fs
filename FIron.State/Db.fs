module Db
open Db
open Util
open FSharpx

let state = Atom (None : Db option)
let get() = state.Value |> Option.orFail "no database found"
let set = konst >> state.Swap
