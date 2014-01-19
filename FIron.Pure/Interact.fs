module Interact

open FSharpx
open Obj

let doorActions door = 
    if door.isOpen then ["Close"] else ["Open"]
let doorPerform door = function
    | "Open" -> openDoor door
    | "Close" -> closeDoor door
    | _ -> door
    
let objActions = function
    | ObjDoor door -> doorActions door

let objPerform action = function
    | ObjDoor door -> doorPerform door action |> ObjDoor