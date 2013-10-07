module Render

open Microsoft.Xna.Framework;
open Microsoft.Xna.Framework.Graphics
open Geom
open FSharpx
open FSharpx.Collections
open FIronCS
open Awesomium.Core

module Grid = 
    open Grid

    //data required to draw a grid
    type DrawData = {
        viewport:rect
        expl:Grid<bool>
        vis:Grid<bool>
        cellSize:int
    }

    let viewPortIter data f g =
        let vp = data.viewport
        let dm = data.expl
        let cs = data.cellSize  
        let mw, mh = g.size
        let sx = max 0 ((vp.X / cs) - 1);
        let sy = max 0 ((vp.Y / cs) - 1);
        let w = (vp.Width / cs) + 1;
        let h = (vp.Height / cs) + 1;
        let right = min (mw-1) (sx + w)
        let bottom = min (mh-1) (sy + h)
        for x = sx to right do
            for y = sy to bottom do
                let isVisible = get x y data.vis 
                if get x y dm then f x y isVisible g

    /// draw quickly a grid of sprites
    let fastDraw sb data g =
        let cs = data.cellSize
        let inline f x y c g =
                let spr = get x y g
                Res.Sprite.draw sb spr x y 32 (shadeForVisbility c)
        viewPortIter data f g

    /// draw quickly a grid of sprite options
    let fastDraw1 sb data g =
        let cs = data.cellSize
        let inline f x y c g =
            match get x y g with
            | Some spr -> Res.Sprite.draw sb spr x y 32 (shadeForVisbility c) 
            | None -> ()
        viewPortIter data f g 


module Cre = 
    open Cre
    open Grid
    let inline fastDraw sb data g =
        let cs = data.cellSize
        let inline f x y v g = 
            match Grid.get x y g, v with
            | Some cre, true -> draw sb x y cs Color.White cre
            | _ -> ()
        Grid.viewPortIter data f g
    

type DrawData = {  
    db : Data.Db
    gd : GraphicsDevice
    sb : SpriteBatch
    sampler : SamplerState
}
    
let draw dd = 
    let {
            db = db
            gd = gd
            sb = sb
            sampler = sampler
        } = dd
    let stack = State.MapState.getAsync() |> Async.StartAsTask
    let cam = State.Cam.getAsync() |> Async.StartAsTask
    let postGui = State.Xna.Gfx.Foreground.getForegroundAsync() |> Async.StartAsTask
    let uiState = State.Ui.getAsync() |> Async.StartAsTask

    gd.SamplerStates.[0] <- sampler
    gd.Clear(Color.Black)
        
    let matrix = Cam.matrix cam.Result
    let vp = Cam.viewport cam.Result

    let map = stack.Result

    do sb.Begin(SpriteSortMode.Deferred, 
                BlendState.AlphaBlend,
                SamplerState.PointClamp,
                DepthStencilState.None,
                RasterizerState.CullNone,
                null,
                matrix)

        
    let gdraw = 
            { 
            expl = map.stack.expl
            vis = map.stack.vis
            viewport = vp
            cellSize = 32
            } : Grid.DrawData

    Grid.fastDraw sb gdraw map.stack.terr
    Grid.fastDraw1 sb gdraw map.stack.decor

    let selected = uiState.Result.selected |> Seq.choose (flip Map.tryFind map.tables.crePos)
    let selection = db.sprites.["selection"]
    for i in selected do
        Res.Sprite.draw sb selection i.X i.Y 32 Color.LightGreen
            
    Cre.fastDraw sb gdraw map.stack.cre

    do sb.End()
       
    sb.Begin()

    let aweTex = State.Awe.tex()
    match State.Awe.tex() with
        | Some t -> sb.Draw(t, gd.Viewport.Bounds, Color.White)
        | None -> ()
         
    for f in postGui.Result do State.Xna.Gfx.Foreground.renderItem sb f 

    sb.End()