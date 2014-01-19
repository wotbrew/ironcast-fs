module Program
// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open Microsoft.Xna.Framework;
open Microsoft.Xna.Framework.Input;
open Microsoft.Xna.Framework.Graphics
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
    let settings = Settings.agent.Value
    let video = settings.video
    let w, h = video.resolution

    do gdm.PreferredBackBufferWidth <- w
       gdm.PreferredBackBufferHeight <- h
       gdm.IsFullScreen <- video.fullscreen
    
    let sb = lazy(new SpriteBatch(x.GraphicsDevice))
    let time = ref Time.defaultTime
    let sampler = new SamplerState()
    do sampler.AddressU <- TextureAddressMode.Clamp
    do sampler.AddressV <- TextureAddressMode.Clamp

    do x.IsMouseVisible <- true
       x.IsFixedTimeStep <- false
       gdm.SynchronizeWithVerticalRetrace <- false
       x.Content.RootDirectory <- "Content"
    
    let seed = Rand.rand()
    let db = lazy (Db.get())
    let mutable drawData = None : Render.DrawData option
    override x.Initialize() = do
        Init.init gdm (x.Window.ClientBounds.Width, x.Window.ClientBounds.Height)
        drawData <- Some { db = Lazy.force db; gd = x.GraphicsDevice; sb = sb.Value; sampler = sampler }

        base.Initialize()

    override x.Draw(gt) = do
        match drawData with
         | Some dd -> Render.draw dd
         | _ -> ()

        base.Draw(gt)

    override x.Update(gt) = 
        Gs.update()
        let game = Gs.get()
        if game.ui.jscalls.Length > 0 then
            State.Awe.actor.Post (State.Awe.Call game.ui.jscalls)
            Ui.state.Swap (Ui.mapCalls (konst []))

        time := Time.updateTime gt !time
        let delta = time.Value.delta    
        let update = Event.pushAsync delta |> Async.StartAsTask
        x.Window.Title <- sprintf "FPS: %f" time.Value.fps
        let gd = gdm.GraphicsDevice
        //Input.fireinput (gd.Viewport.Width, gd.Viewport.Height)
        Input.update game (Mouse.GetState()) (Keyboard.GetState())
        
        let dispatch = State.Xna.dispatchQueue.PostAndReply(fun r -> State.Xna.Get r)
        for d in dispatch do d()

        update.Wait()
        base.Update(gt)


[<EntryPoint>]
let main argv = 
    MyGame().Run()
    1
    

    