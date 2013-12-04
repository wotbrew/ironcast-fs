module Init
open Microsoft.Xna.Framework
open FSharpx
open FSharpx.Collections
open Geom
open FIronCS
open Awesomium.Core

module Pages = 
    let urify s = (IO.currentDirectory + s).ToUri()
    let main = urify @"/Gui/main.html"
    let newGame = urify @"/Gui/newgame.html"

let tileDir = (IO.combinePaths IO.currentDirectory "Tiles")
let cacheDir = (IO.combinePaths IO.currentDirectory "Gui\\img\\sprites\\")

let seed = Rand.rand()

let initXna gdm = 
    do printfn "%s" "Initializing xna"
    State.Xna.Gfx.manager <- gdm
    State.Xna.Gfx.initBlank()
    do printfn "%s" "Done"

let loadDb gd : Db.Db option = Option.maybe {
    do printfn "%s" "Loading resolutions"
    let! allRes = Settings.loadResolutions File.allRes
    do printfn "%s" "Done"
    do printfn "%s" "Loading resources"
    let! sprites = Res.Sheet.spriteStore Dir.tileDir gd |> Choice.toOption
    do printfn "%s" "Done"
    do printfn "%s" "Loading races"
    let! races = Cre.loadRacesMap sprites
    do printfn "%s" "Done"
    do printfn "%s" "Loading themes"
    let! themes = World.loadThemeMap sprites
    do printfn "%s" "Done"
    return {
        allRes = allRes
        sprites = sprites
        races = races
        themes = themes
    }
}

let initDb gd = 
    let dbo = loadDb gd
    if dbo.IsNone then 
        do printfn "%s" "Could not load db"
        exit 0
    Db.set dbo

let initCam screen cellSize = 
    do printfn "%s" "Initializing camera"
    let mapSize = Builder.defRoomSpec.gridSize
    Cam.swap (Cam.resize screen mapSize cellSize)
    do printfn "%s" "Done"

let initImgCache sprites = 
    do printfn "%s" "Creating image cache"
    Res.ImgCache.saveToCache cacheDir sprites
    do printfn "%s" "Done"

let initAwe() = 
    do printfn "%s" "Initializing awesomium"
    State.Awe.setGlobal "iron" Js.Api.iron
    State.Awe.setSource Pages.main
    do printfn "%s" "Done"

let placeTestCreatures (db:Db.Db) (map:MapStack.Stack) = 
    let visible = map.walk 
                  |> Grid.cells 
                  |> Seq.filter snd
                  |> Seq.map fst 
                  |> List.ofSeq 
                  |> flip Rand.List.randomize seed
    let vis6 = Seq.take 6 visible |> List.ofSeq
    let creature i = {id = i; isPlayer = true; body = db.races.["Human"].spriteM |> snd |> List.singleton} : Cre.Creature
    Seq.fold
            (fun map (pt, i) -> MapStack.placeCre (creature i) pt map)
            map
            (Seq.zip vis6 [0 .. 5])

let initWorld (db:Db.Db) = 
    do printfn "%s" "Initializing world"
    let castle = db.themes.["castle"]
    let map = MapStack.generate seed castle
             |> placeTestCreatures db 
    World.init map
    Path.newWalk map.walk
    World.refreshVis()
    do printfn "%s" "Done"
    
let centreCam cellSize =
    do printfn "%s" "Centering camera"
    let crei = World.get().tables.crePos.Item 0
    Cam.swap (Cam.centreOn ((Vec.ofPt crei) * cellSize)) |> ignore
    do printfn "%s" "Done"

let init gdm screen = 
    initXna gdm
    initDb gdm.GraphicsDevice

    let db = Db.get()
    let settings = Settings.agent.Value

    //initImgCache db.sprites
    initCam screen settings.video.cellSize
    initAwe()
    initWorld db

    centreCam settings.video.cellSize

