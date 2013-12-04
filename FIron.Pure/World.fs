module World
open FSharpx

type Theme = {
    name : string
    floor : Sprite list
    wall : Sprite list
    wallDecor : Sprite list
}

type World = {
    stack : MapStack.Stack
    tables: MapTables.Tables
}

let empty = {
    stack = MapStack.empty
    tables = MapTables.empty
}

let inline stack w = w.stack
let inline tables w = w.tables

let getCre p = stack >> MapStack.getCre p
let getCreSafe p = stack >> MapStack.getCreSafe p

let isPlayer id world = Map.containsKey id world.tables.players

let canWalk p world =
    let walk = Grid.getSafe1 p world.stack.walk <??> false
    let cre = getCreSafe p world |> Option.map (konst true) <??> false
    walk && not cre

let inBounds p world = Grid.inBounds1 p world.stack.cre