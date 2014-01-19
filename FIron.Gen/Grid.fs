module Grid
open Rand
open Geom

///place a single wall at point
let placeWall dungeon pt = Grid.update1 pt Tile.Wall dungeon
///place many walls at the given points
let placeWalls dungeon pts =  Grid.updateAllPts dungeon pts Tile.Wall

///dig a hole in the dungeon earth (replace with floor)
let dig dungeon pt = Grid.update1 pt Tile.Floor dungeon 

///dig a rectangle in the dungeon (replace with floor)
let digRect dungeon rect = Grid.updateAllPts dungeon (Rect.pts rect) Tile.Floor

///dig a room (floor, surrounded by walls)
let digRoom dungeon room = 
    let dug = digRect dungeon room
    let expanded = Rect.expand room 1
    let inner = expanded |> Rect.edges
    placeWalls dug inner

let digCorridorSquare dungeon pt = 
    let dug = dig dungeon pt
    let adj = Grid.adj dug pt
    let walls = Seq.filter (Tile.isEarth dug) adj
    placeWalls dug walls
   

//initialize an empty dungeon grid
let emptyDungeon size = Grid.create size Tile.Earth

//place a border of walls around the dungeon
let placeBorder dungeon = Grid.borderPts dungeon |> placeWalls dungeon

let initialWalkArray size = Grid.create size Tile.Floor |> placeBorder |> Tile.walkarr



let createDungeon (level:Spec.LevelSpec) seed = 
    let sz = level.size
    let rooms = Spec.genRooms level seed
    let empty = emptyDungeon sz |> placeBorder
    let paths = Walks.findPaths (initialWalkArray sz) rooms seed |> Seq.concat |> Seq.distinct
    //fill 'er in
    let roomed = Seq.fold digRoom empty rooms 
    let corridored = Seq.fold digCorridorSquare roomed paths
    corridored

let printTile = function 
    | Tile.Earth -> "'"
    | Tile.Wall -> "#"
    | Tile.Floor -> "_"

let sprintDungeon dungeon = 
    Grid.rows dungeon
    |> Seq.map (List.map printTile >> String.concat "")
    |> String.concat "\n"

let printDungeon dungeon = sprintDungeon dungeon |> printf "%s"
//
//let terrainForTile rand (theme:World.Theme) = function
//    | Tile.Wall | Tile.Earth -> List.randn theme.wall rand
//    | Tile.Floor -> List.randn theme.floor rand
//
//let createTerrain rand theme = Grid.map (terrainForTile rand theme)