module Tile
open Geom
open FSharpx

type Tile = Earth | Wall | Floor


let floors = Grid.cellsEqualTo Floor
let walls = Grid.cellsEqualTo Wall
let inline isEarth g p = Grid.get1 g p = Earth
let inline isEarthSafe g p = Grid.getSafe1 g p |> Option.option false ((=) Earth)
let inline isWall g p = Grid.get1 g p = Wall
let inline isWallSafe g p = Grid.getSafe1 g p |> Option.option false ((=) Wall)
let inline notWall g p = not <| isWall g p
let inline notWallSafe g p = not <| isWallSafe g p
let inline isFloor g p = Grid.get1 g p = Floor
let inline isFloorSafe g p = Grid.getSafe1 g p |> Option.option false ((=) Floor)
let walkable = function Earth | Wall -> false | Floor -> true
let walkableAt g = Grid.get1 g >> walkable
let walkmap = Grid.map walkable
let walkarr = walkmap >> Grid.toArray2D
let wallsAndFloors g = floors g |> Seq.append <| walls g

///we are surrounded by at least 2 pts opposite one another
let hasOpposite pt pts =  
    Seq.exists (fun x -> 
        let px = Pt.add pt (Pt.opposite pt x) 
        Seq.exists ((=) px) pts) pts
        
let wallsOf g = Seq.filter (isWall g)
let floorOf g = Seq.filter (notWall g)

let isCorridor g p =
    if isWall g p then false
    else
    let adj = Dir.adjc p |> Seq.filter (Grid.inBounds1 g)
    let walls = wallsOf g adj
    let floor = floorOf g adj
    hasOpposite p walls && hasOpposite p floor

let isRoom g p = 
    let adj = Dir.adj p
    not (isWall g p || isCorridor g p) && (Seq.atLeast 2 (notWall g) adj)

let roomFrom g p =
    Pt.flood (isRoom g) p
