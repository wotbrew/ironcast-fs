module Obj

open FSharpx
open FSharpx.Option

type Door = {
    name: string
    opened: Sprite
    closed: Sprite
    isOpen: bool
}

let doorSprite door = 
    if door.isOpen then door.opened else door.closed
let openDoor door = {door with isOpen = true}
let closeDoor door = {door with isOpen = false}
type Obj = 
     | ObjDoor of Door

let asDoor = function
    | ObjDoor door -> Some door
    | _ -> None

let objType = function
    | ObjDoor _ -> "door"
let objName = function
    | ObjDoor door -> door.name

let inline objSolid obj =
    match obj with
        | ObjDoor door -> not door.isOpen

let walkable = not << objSolid

let inline objSolidAt grid pt = 
    Grid.get1 grid pt |> Option.map objSolid <??> false

let walkableAt grid = not << objSolidAt grid
let walkmap = Grid.map (Option.option true walkable)

let mergeAttrDoor sprites map door =
    let find = flip Map.tryFind map 
    let findspr = flip Map.tryFind sprites
    let opened = find "opened" >>= findspr <??> door.opened
    let closed = find "closed" >>= findspr <??> door.closed
    let name = find "name" <??> door.name
    let isOpen = find "is-open" >>= tryParseWith bool.TryParse <??> door.isOpen
    {
        opened = opened
        closed = closed
        isOpen = isOpen
        name = name
    }

let mergeAttr sprites map = function
    | ObjDoor door -> mergeAttrDoor sprites map door |> ObjDoor

