module Program
// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open Microsoft.Xna.Framework;
open Microsoft.Xna.Framework.Graphics
open Geom
open Builder
open FSharpx
open FSharpx.Collections
open FIronCS
open Awesomium.Core
open Util
let nil<'a> = Unchecked.defaultof<'a>

module Pages = 
    let urify s = (IO.currentDirectory + s).ToUri()
    let main = urify @"/Gui/main.html"
    let newGame = urify @"/Gui/newgame.html"

let tileDir = (IO.combinePaths IO.currentDirectory "Tiles")
let cacheDir = (IO.combinePaths IO.currentDirectory "Gui\\img\\sprites\\")

type MyGame() as x = 
    inherit Game()

    do State.Xna.Game.game <- x

    let gdm = new GraphicsDeviceManager(x)
    let settings = State.Settings.get()

    let w, h = settings.resolution

    do gdm.PreferredBackBufferWidth <- w
       gdm.PreferredBackBufferHeight <- h
       gdm.IsFullScreen <- settings.fullscreen
    
    let sb = lazy(new SpriteBatch(x.GraphicsDevice))
    let time = ref State.Time.defaultTime
    let sampler = new SamplerState()
    do sampler.AddressU <- TextureAddressMode.Clamp
    do sampler.AddressV <- TextureAddressMode.Clamp

    do x.IsMouseVisible <- true
       x.IsFixedTimeStep <- false
       gdm.SynchronizeWithVerticalRetrace <- false
       x.Content.RootDirectory <- "Content"
    
    let seed = Rand.rand()
    let db = lazy (State.Db.get())
    let mutable drawData = None : Render.DrawData option
    override x.Initialize() = do
        let gd = gdm.GraphicsDevice
        State.Xna.Gfx.manager <- gdm
        State.Xna.Gfx.initBlank()

        let db = db.Value
        let ss = db.sprites
        Res.ImgCache.saveToCache cacheDir ss
        State.Cam.agent.Post(State.Cam.Resize ((x.Window.ClientBounds.Width, x.Window.ClientBounds.Height), Builder.defRoomSpec.gridSize, 32))           
        
        State.Awe.setGlobal "iron" Js.Api.iron
        State.Awe.setSource Pages.main
        let castle = db.themes.["castle"]
        let map = MapStack.generate seed castle

        let visible = map.walk |> Grid.cells |> Seq.filter (fun (pt, v) -> v) |> Seq.map fst |> List.ofSeq |> flip Rand.List.randomize seed
        let vis6 = Seq.take 6 visible |> List.ofSeq
        Seq.fold (fun map (pt, i) ->  MapStack.placeCre ({id = i; isPlayer = true; body = db.races.["Human"].spriteM |> snd |> List.singleton} : Cre.Creature) pt map) map (Seq.zip vis6 [0 .. 5])
        |> State.MapState.init
        //MapStack.placeCre ({id = 0; isPlayer = true; body = db.races.["Human"].spriteM |> snd |> List.singleton } : Cre.Creature) visible map
        //|> State.MapState.init

        let map = State.MapState.get()

        let printMap map =
            let vec = Grid.vector map
            let w,h = map.size
            for y = 0 to h - 1 do
                printf "\n"
                for x = 0 to w - 1 do
                 let c = if Grid.get x y map then '_' else '#'
                 printf "%c" c

        printfn "walk"
        printMap map.stack.walk
        printfn "\nexpl"
        printMap map.stack.expl

        let newWalk = map.stack.walk |> Grid.mapi (fun x y v -> v && Grid.get x y map.stack.expl)
        
        printfn "\nnewwalk"
        printMap newWalk

        State.Path.newWalk newWalk
        State.MapState.refreshVis()
        State.Cam.agent.Post(State.Cam.Centre (Vec.ofPt vis6.Head * 32.0f))

        drawData <- Some { db = db; gd = x.GraphicsDevice; sb = sb.Value; sampler = sampler }

        base.Initialize()

    override x.Draw(gt) = do
        match drawData with
         | Some dd -> Render.draw dd
         | _ -> ()

        base.Draw(gt)

    override x.Update(gt) = 
        
        time := State.Time.updateTime gt !time
        let delta = time.Value.delta    
        let update = State.Event.pushAsync delta |> Async.StartAsTask
        x.Window.Title <- sprintf "FPS: %f" time.Value.fps
        let gd = gdm.GraphicsDevice
        Input.fireinput (gd.Viewport.Width, gd.Viewport.Height)


        let dispatch = State.Xna.dispatchQueue.PostAndReply(fun r -> State.Xna.Get r)
        for d in dispatch do d()

        update.Wait()
        base.Update(gt)


[<EntryPoint>]
let main argv = 
    MyGame().Run()
    1
    

    