module MapStack
open FSharpx
open MapStack

let ofTileMap init = 
    let {
            dungeon = dungeon
            terrain = terrain
            decor = decor
            obj = obj
        } = init
    Grid.printDungeon dungeon
    let tilewalk = Tile.walkmap dungeon 
    let objwalk = Obj.walkmap obj
    let walk = Grid.gridAnd tilewalk objwalk  
    let wallsAndFloors = 
        Tile.wallsAndFloors dungeon
        |> Seq.map (Tup.mapsnd (konst true))
    {
        light = walk |> Grid.map not
        terr = terrain
        decor = decor
        walk = walk
        dung = dungeon
        expl = Grid.ofCells1 false dungeon.size wallsAndFloors 
        vis = Grid.create dungeon.size false
        cre = Grid.create dungeon.size None
        obj = obj
    }
//
//let generate seed theme = 
//    let dungeon = Grid.createDungeon Spec.defaultLevelSpec seed
//    ofTileMap dungeon seed theme
