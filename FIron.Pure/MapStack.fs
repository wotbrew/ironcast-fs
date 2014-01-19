module MapStack

open Microsoft.Xna.Framework

open Util
open FSharpx
open FSharpx.Collections

open Grid
open Geom

type MapInitData = {
    dungeon: Grid.Grid<Tile.Tile>
    terrain: Grid.Grid<Sprite>
    decor: Grid.Grid<Sprite option>
    obj: Grid.Grid<Obj.Obj option>
}

type Stack = {
    light: Grid<bool>
    terr: Grid<Sprite>
    dung: Grid<Tile.Tile>
    walk: Grid<bool>
    decor: Grid<Sprite option> 
    expl: Grid<bool>
    obj: Grid.Grid<Obj.Obj option>
    cre: Grid<Cre.Creature option>
    vis: Grid<bool>
}
let inline cre stack = stack.cre
let inline obj stack = stack.obj
let inline walk stack = stack.walk

let empty = {
    light = Grid.empty
    terr = Grid.empty
    dung = Grid.empty
    walk = Grid.empty
    decor = Grid.empty
    expl = Grid.empty
    obj = Grid.empty
    cre = Grid.empty
    vis = Grid.empty
}

let creCells stack = Grid.someCells stack.cre


let mapVis f stack = 
    {
        stack with vis = f stack.vis
    } : Stack
let withVis v stack = mapVis (konst v) stack

let mapCre f stack = {
        stack with cre = f stack.cre
    }
let mapObj f (stack:Stack) = {
        stack with obj = f stack.obj
    }
let mapWalk f stack = {
        stack with walk = f stack.walk
    }

let also b a arg1 arg2 = 
    let x = b arg1 arg2
    let y = a x arg2
    y
//let alsoAt p2 b a arg1 arg2 = 
//    let x = b arg1 arg2
//    let y = a x p2
//    y

let setValueWith trans v st p = trans (Grid.update1 p v) st
let mapValueWith trans f st p = trans (Grid.updateWith p f) st

let getWalk = walk >> get1
let setWalkAt = setValueWith mapWalk 

let hasM f st = f st >> Option.isSome
let noM f st = f st >> Option.isNone

/// get the creature at the given point
let getCre = cre >> get1
let hasCre = hasM getCre
let noCre = noM getCre
let getCreSafe ms = getSafe1 ms.cre >> Option.concat
/// find the creatures in the given rect
let getCreIn ms = getIn ms.cre >> Seq.choose id
let getCreInSafe ms = getInSafe ms.cre >> Seq.choose id >> Seq.choose id 

let setCreAt = setValueWith mapCre
let setCreAtM = setCreAt << Some
let mapCreAt = mapValueWith mapCre
let mapCreAtM = mapCreAt << Option.map

let clearCre = setCreAt None |> also <| setWalkAt true
let placeCre cre = setCreAtM cre |> also <| setWalkAt false

let moveCre cre a b stack = 
    if getWalk stack b && noCre stack b then 
        let cleared = clearCre stack a
        let placed = placeCre cre cleared b
        Some placed
    else None
    
let getObj = obj >> get1
let hasObj = hasM getObj
let getDoor ms = getObj ms >> Option.bind Obj.asDoor
let hasDoor = hasM getDoor
        
let getObjSafe ms = getSafe1 ms.obj >> Option.concat

let private liftActionF o = Option.map Interact.objActions o <??> []
let getActions ms = getObj ms >> liftActionF
let getActionsSafe ms = getObjSafe ms >> liftActionF

let private liftPerformActionF a o = Option.map (Interact.objPerform a) o
let performAction action p = mapObj (Grid.updateWith p (liftPerformActionF action))

let mapObjAt = mapValueWith mapObj
let mapObjAtM = mapObjAt << Option.map
let mapObjAtAs asf f = mapObjAtM (fun o -> match asf o with Some x -> f x | _ -> o)
let mapDoorAt f = mapObjAtAs Obj.asDoor (f >> Obj.ObjDoor)


let openDoorAt = 
    mapDoorAt Obj.openDoor 
    |> also <| 
    setWalkAt true

let closeDoorAt = 
    mapDoorAt Obj.closeDoor
    |> also <| 
    setWalkAt false


/// is the given x y co-ords in the bounds of the mapstack     
let inBounds ms = Grid.inBounds ms.cre
/// is the given x y co-ords in the bounds of the mapstack
let inBounds1 ms = Grid.inBounds1 ms.cre 

let clearVisibility stack = withVis (Grid.create stack.terr.size false) stack
let updateVisibility prs stack = 
     let lm = stack.light
     let pts = 
        [for cre, pt in prs do
             let range = Cre.sightRange cre
             yield async {
                return FIronCS.Geom.RaysWhileSkip(pt, (fun p -> Grid.inBounds1 lm p && (Grid.get1 lm p |> not)), range, 1, 0.3f)
             }]
        |> Async.Parallel |> Async.map Seq.concat |> Async.StartAsTask
     mapVis (fun g -> Grid.updateAll g (pts.Result |> Seq.map (Tup.pairWith true))) stack
        

        