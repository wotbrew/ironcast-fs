// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open Microsoft.Xna.Framework;
open Microsoft.Xna.Framework.Graphics
open Builder
open Geom
open FSharpx
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
    let desktop = new SampleDesktop()
    let settings = State.Settings.get()

    let w, h = settings.resolution

    do desktop.Name <- "desktop"
       gdm.PreferredBackBufferWidth <- w
       gdm.PreferredBackBufferHeight <- h
       gdm.IsFullScreen <- settings.fullscreen
    
    let sb = lazy(new SpriteBatch(x.GraphicsDevice))
    let dungeon = ref None
    let time = ref State.Time.defaultTime
    let sampler = new SamplerState()
    do sampler.AddressU <- TextureAddressMode.Clamp
    do sampler.AddressV <- TextureAddressMode.Clamp

    do x.IsMouseVisible <- true
       x.IsFixedTimeStep <- false
       gdm.SynchronizeWithVerticalRetrace <- false
       x.Content.RootDirectory <- "Content"
       
    override x.Initialize() = do
        let gd = gdm.GraphicsDevice
        State.Xna.Gfx.manager <- gdm
        let ss = State.Db.get().sprites
        Res.ImgCache.saveToCache cacheDir ss

        dungeon := Some (Builder.genDungeon [Builder.defRoomSpec] (Rand.rand())    
                         |> Array2D.map (fun x -> if x then ss.["castleWall1"] else ss.["castleFloor1"] ))
        State.Cam.agent.Post(State.Cam.Resize ((x.Window.ClientBounds.Width, x.Window.ClientBounds.Height), Builder.defRoomSpec.gridSize, 32))           

        State.Awe.setGlobal "iron" Js.Api.iron
        State.Awe.setSource Pages.main


        base.Initialize()
    override x.Draw(gt) = do
        let postGui = State.Xna.Gfx.Foreground.getForegroundAsync() |> Async.StartAsTask

        x.GraphicsDevice.SamplerStates.[0] <- sampler
        x.GraphicsDevice.Clear(Color.Black)

        let sb = sb.Value
        let matrix = State.Cam.Get.matrix()
        do sb.Begin(SpriteSortMode.Deferred, 
                    BlendState.AlphaBlend,
                    SamplerState.LinearClamp,
                    DepthStencilState.None,
                    RasterizerState.CullNone,
                    null,
                    matrix)
        do Option.iter (Array2D.iteri (fun x y (t, r) -> sb.Draw(t, Rect.move (x*32) (y*32) r, Nullable.create r, Color.White))) !dungeon 
        
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
    

    