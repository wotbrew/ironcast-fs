module MapStack
open FSharpx
open MapStack

let generate seed theme = 
    let dungeon = Builder.genDungeon [Builder.defRoomSpec] seed
                  |> Grid.ofArray2D
    let invdungeon = dungeon |> Grid.map not
    let wallsAndFloors = 
        Grid.innerWallsAndFloors dungeon
        |> Seq.map (Tup.mapsnd (konst true))
    {
        light = dungeon
        terr = Grid.createTerrain seed theme dungeon
        decor = Builder.Decor.walls1 dungeon theme.wallDecor seed
        walk = invdungeon
        dung = invdungeon
        expl = Grid.ofCells1 false dungeon.size wallsAndFloors 
        vis = Grid.create dungeon.size false
        cre = Grid.create dungeon.size None
    }