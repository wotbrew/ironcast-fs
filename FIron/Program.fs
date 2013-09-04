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

let tileDir = (IO.combinePaths IO.currentDirectory "Tiles")
let cacheDir = (IO.combinePaths IO.currentDirectory "Gui\\img\\sprites\\")

type MyGame() as x = 
    inherit Game()
    let gdm = new GraphicsDeviceManager(x)
    let desktop = new SampleDesktop()

    let w, h = 1024, 768

    do desktop.Name <- "desktop"
       gdm.PreferredBackBufferWidth <- w
       gdm.PreferredBackBufferHeight <- h
    
    let sb = lazy(new SpriteBatch(x.GraphicsDevice))
    let awe = lazy(new AwesomiumXNA.AwesomiumComponent(x, rect(0, 0, w, h)))
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
        let db = Data.loadDb gd |> Option.orFail "Could not load db"
        
        let ss = db.sprites
        Res.ImgCache.saveToCache cacheDir ss

        dungeon := Some (Builder.genDungeon [Builder.defRoomSpec] (Rand.rand())    
                         |> Array2D.map (fun x -> if x then ss.["castleWall1"] else ss.["castleFloor1"] ))
        State.Cam.agent.Post(State.Cam.Resize ((x.Window.ClientBounds.Width, x.Window.ClientBounds.Height), Builder.defRoomSpec.gridSize, 32))           
        
        awe.Value.WebView.ParentWindow <- x.Window.Handle;
        awe.Value.WebView.ConsoleMessage.Add(fun c -> printf "%s" c.Message)
        Js.Api.Awe.init awe.Value.WebView |> ignore
        awe.Value.WebView.Source <- Gs.Pages.main
        //Gs.HtmlMut.current <- Gs.Pages.main
        x.Components.Add(awe.Value)
        

        base.Initialize()
    override x.Draw(gt) = do
        let postGui = State.Xna.Gfx.Foreground.getForegroundAsync() |> Async.StartAsTask

        x.GraphicsDevice.SamplerStates.[0] <- sampler
        x.GraphicsDevice.Clear(Color.Black)

        let sb = sb.Value
        let matrix = State.Cam.agent.PostAndReply(State.Cam.GetState) |> State.Cam.matrix
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
        if not <| (awe.Value.WebViewTexture = null) then
          sb.Draw(awe.Value.WebViewTexture, gd.Viewport.Bounds, Color.White);
          for f in postGui.Result do State.Xna.Gfx.Foreground.renderItem sb f 
        sb.End()

        base.Draw(gt)

    override x.Update(gt) = 
        
        time := State.Time.updateTime gt !time
        x.Window.Title <- sprintf "FPS: %f" time.Value.fps
        let gd = gdm.GraphicsDevice
        Input.fireinput (gd.Viewport.Width, gd.Viewport.Height)

        //let awe = awe.Value
        //Gs.Html.refreshCurrent awe.WebView

        let dispatch = State.Xna.dispatchQueue.PostAndReply(fun r -> State.Xna.Get r)
        for d in dispatch do d()
        base.Update(gt)


[<EntryPoint>]
let main argv = 
    let build = Builder.genRooms defRoomSpec
    MyGame().Run()
    1
    

    