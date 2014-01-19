module World
open FSharpx

//type Theme = {
//    name : string
//    floor : Sprite list
//    wall : Sprite list
//    wallDecor : Sprite list
//}

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

let mapStack f world = {world with stack = f world.stack}
let mapStackAt p f = mapStack (flip f p)
let mapTables f world = {world with tables = f world.tables}

let getCre = stack >> MapStack.getCre
let getCreSafe = stack >> MapStack.getCreSafe
let getCreIn = stack >> MapStack.getCreIn
let getCreInSafe = stack >> MapStack.getCreInSafe

let getObj = stack >> MapStack.getObj
let getObjSafe = stack >> MapStack.getObjSafe

let getActions = stack >> MapStack.getActions
let getActionsSafe = stack >> MapStack.getActionsSafe

let performAction action p world = {world with stack = MapStack.performAction action p world.stack}

let isPlayer id world = Map.containsKey id world.tables.players

let canWalk world p =
    let walk = Grid.getSafe1 world.stack.walk p <??> false
    let cre = getCreSafe world p |> Option.map (konst true) <??> false
    walk && not cre

let inBounds world = Grid.inBounds1 world.stack.cre
