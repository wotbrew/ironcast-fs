module Program
// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open Microsoft.Xna.Framework;
open Microsoft.Xna.Framework.Graphics
open Builder
open Geom
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

    override x.Initialize() = do
        let gd = gdm.GraphicsDevice
        State.Xna.Gfx.manager <- gdm
        let db = State.Db.get()
        let ss = db.sprites
        Res.ImgCache.saveToCache cacheDir ss

        State.Cam.agent.Post(State.Cam.Resize ((x.Window.ClientBounds.Width, x.Window.ClientBounds.Height), Builder.defRoomSpec.gridSize, 32))           
        
        State.Awe.setGlobal "iron" Js.Api.iron
        State.Awe.setSource Pages.main
        let castle = db.themes.["castle"]
        let map = MapStack.generate seed castle

        let visible = map.walk |> Grid.cells |> Seq.filter (fun (pt, v) -> v) |> List.ofSeq |> flip Rand.List.randn seed |> fst

        MapStack.placeCre ({id = 0; body = db.races.["Human"].spriteM |> snd |> List.singleton } : Cre.Creature) visible map
        |> State.Map.init

        State.Cam.agent.Post(State.Cam.Centre (Vec.ofPt visible * 32.0f))
        base.Initialize()
    override x.Draw(gt) = do
        let stack = State.Map.getAsync() |> Async.StartAsTask
        let cam = State.Cam.getAsync() |> Async.StartAsTask
        let postGui = State.Xna.Gfx.Foreground.getForegroundAsync() |> Async.StartAsTask

        x.GraphicsDevice.SamplerStates.[0] <- sampler
        x.GraphicsDevice.Clear(Color.Black)
        
        let sb = sb.Value
        let matrix = State.Cam.matrix cam.Result
        let vp = State.Cam.viewport cam.Result

        let map = stack.Result

        do sb.Begin(SpriteSortMode.Deferred, 
                    BlendState.AlphaBlend,
                    SamplerState.PointClamp,
                    DepthStencilState.None,
                    RasterizerState.CullNone,
                    null,
                    matrix)
        MapStack.draw sb vp 32 map.stack

        do sb.End()
        
        let gd = gdm.GraphicsDevice
        sb.Begin()

        let aweTex = State.Awe.tex()
        match State.Awe.tex() with
         | Some t -> sb.Draw(t, gd.Viewport.Bounds, Color.White)
         | None -> ()
         
        for f in postGui.Result do State.Xna.Gfx.Foreground.renderItem sb f 

        sb.End()

        base.Draw(gt)

    override x.Update(gt) = 
        
        time := State.Time.updateTime gt !time
        x.Window.Title <- sprintf "FPS: %f" time.Value.fps
        let gd = gdm.GraphicsDevice
        Input.fireinput (gd.Viewport.Width, gd.Viewport.Height)


        let dispatch = State.Xna.dispatchQueue.PostAndReply(fun r -> State.Xna.Get r)
        for d in dispatch do d()
        base.Update(gt)


[<EntryPoint>]
let main argv = 
    let build = Builder.genRooms defRoomSpec
    MyGame().Run()
    1
    

    