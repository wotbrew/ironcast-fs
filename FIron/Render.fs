module Render

open Util
open Microsoft.Xna.Framework;
open Microsoft.Xna.Framework.Graphics
open Geom
open FSharpx
open FSharpx.Collections
open FIronCS
open Awesomium.Core

let inline drawBlank (sb:SpriteBatch) (dest:rect) color =
    sb.Draw(State.Xna.Gfx.blank, dest, color)
let inline drawBox sb (dest:rect) color thickness =
    let sides = 
        [rect(dest.X, dest.Y, dest.Width, thickness)
         rect(dest.Right - thickness, dest.Y, thickness, dest.Height)
         rect(dest.X, dest.Bottom - thickness, dest.Width, thickness)
         rect(dest.X, dest.Y, thickness, dest.Height)]
    for r in sides do drawBlank sb r color

module Grid = 
    open Grid

    let inline shadeForVisbility visible = 
        if visible then Color.White
        else Color.Gray

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
                let isVisible = get data.vis x y 
                if get dm x y then f x y isVisible g

    /// draw quickly a grid of sprites
    let fastDraw sb data g =
        let cs = data.cellSize
        let inline f x y c g =
                let spr = get g x y
                Res.Sprite.draw sb spr x y data.cellSize (shadeForVisbility c)
        viewPortIter data f g

    /// draw quickly a grid of sprite options
    let fastDraw1 sb data g =
        let cs = data.cellSize
        let inline f x y c g =
            match get g x y with
            | Some spr -> Res.Sprite.draw sb spr x y data.cellSize (shadeForVisbility c) 
            | None -> ()
        viewPortIter data f g 

module Obj = 
    open Grid
    open Obj
    open Res
    let inline draw sb x y cs c door = 
        Sprite.draw sb (Obj.doorSprite door) x y cs c
    let inline fastDraw sb data g = 
        let cs = data.cellSize
        let inline f x y v g = 
            match Grid.get g x y, v with 
             | Some (ObjDoor door), c -> draw sb x y cs (shadeForVisbility c) door
             | _ -> ()
        Grid.viewPortIter data f g
module Cre = 
    open Grid
    open Cre
    open Res

    let inline draw sb x y cs c cre = 
        for spr in cre.body
            do Sprite.draw sb spr x y cs c

    let inline fastDraw sb data g =
        let cs = data.cellSize
        let inline f x y v g = 
            match Grid.get g x y, v with
            | Some cre, true -> draw sb x y cs Color.White cre
            | _ -> ()
        Grid.viewPortIter data f g
    
module Ui = 
    open Ui
    open Res
    let drawSelection sb sprite cellSize selected = 
        let selection = sprite
        for i:pt in selected do
            Res.Sprite.draw sb selection i.X i.Y cellSize Color.LightGreen

    let drawLasso sb lasso = 
        if Lasso.isLassoing lasso then
            drawBox sb lasso.rect Color.Green 2

type DrawData = {  
    db : Db.Db
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
    let stack = World.getAsync() |> Async.StartAsTask
    let cam = Cam.get()
    let postGui = State.Xna.Gfx.Foreground.get()
    let uiState = Ui.state.Value

    gd.SamplerStates.[0] <- sampler
    gd.Clear(Color.Black)
        
    let matrix = Cam.matrix cam
    let vp = Cam.viewport cam

    let map = stack.Result

    do sb.Begin(SpriteSortMode.Deferred, 
                BlendState.AlphaBlend,
                SamplerState.PointClamp,
                DepthStencilState.None,
                RasterizerState.CullNone,
                null,
                matrix)

    let settings = Settings.agent.Value
    let video = settings.video    
    let cellSize = int video.cellSize
    let gdraw = 
            { 
            expl = map.stack.expl
            vis = map.stack.vis
            viewport = vp
            cellSize = cellSize
            } : Grid.DrawData

    Grid.fastDraw sb gdraw map.stack.terr
    Grid.fastDraw1 sb gdraw map.stack.decor

    let selected = 
        uiState.selected
        |> Seq.choose (flip Map.tryFind map.tables.crePos)

    let selection = db.sprites.["selection"]

    Ui.drawSelection sb selection cellSize selected
    Obj.fastDraw sb gdraw map.stack.obj
    Cre.fastDraw sb gdraw map.stack.cre

    do sb.End()
       
    sb.Begin()
    Ui.drawLasso sb uiState.lasso

    let aweTex = State.Awe.tex()
    match State.Awe.tex() with
        | Some t -> sb.Draw(t, gd.Viewport.Bounds, Color.White)
        | None -> ()
         
    for f in postGui do State.Xna.Gfx.Foreground.renderItem sb f 

    sb.End()