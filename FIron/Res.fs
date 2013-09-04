module Res

open Microsoft.Xna.Framework.Graphics

open Geom
open FSharpx
open FSharpx.Option
open Util

type tex = Texture2D
type sprite = tex * rect
type sheet = string * (string * rect list)

/// Concerning Textures
module Tex = 
 /// Load a texture from file on disk
 let load (gd:GraphicsDevice) file = 
    use fs = new System.IO.FileStream(file, System.IO.FileMode.Open)
    tex.FromStream(gd, fs)
 

/// Concerning spritesheets
module Sheet =
 type SpriteLine = FSharpx.Regex< 
                    @"^(?<name>[\d\w:\(\)\[\]_\-]+)?[ ]+?(?<x>\d+)?[ ]+?(?<y>\d+)[ ]+?(?<width>\d+)[ ]+?(?<height>\d+)$" >
 /// Read a single .spr file sprite line
 let readSprite s = let m = SpriteLine().Match(s)
                    if m.Success then
                        let r = Tup.mapall4 int (m.x.Value, m.y.Value, m.width.Value, m.height.Value)
                        Some (m.name.Value, Rect.ofTup r)
                    else None

 let isComment = String.startsWith "#"
 let filterComments = Seq.filter (not << isComment)
 /// Read a sprite from its textual representation
 let readSheet s = 
        maybe {
            let! lines = String.lines s
                        |> StringSeq.trim
                        |> StringSeq.filterWhitespace
                        |> filterComments
                        |> List.ofSeq |> Seq.whenAny
            let file = lines.Head.Trim()
            let! sprites = Option.mapM readSprite (List.tail lines)
            return file + ".png", sprites
        }
 let readSheet1 s = readSheet s 
                    |> Choice.ofOption (sprintf "Error reading sheet '%s'" s)
 let extension = ".spr"

 /// Read all .spr files in directory, and load the sprites.
 /// Result will be a map of sprite name against the sprite. (or an error message)
 let spriteStore dir gd = 
        Choice.choose {
            let! files = Choice.protect IO.filesInDir dir
                            |> Choice.mapSecond string
            let! sheets = files
                          |> Seq.filter (fun f -> f.Extension = extension)
                          |> Seq.map ((fun f -> f.FullName) >> IO.readFileAsString)
                          |> List.ofSeq
                          |> Choice.mapMLazy readSheet1
            let! textures = sheets
                            |> Choice.mapMLazy (fun (n, spr) -> (Choice.protect (Tex.load gd) n)
                                                                |> Choice.map (Tup.pairWith spr))
                            |> Choice.mapSecond string
            let sprites = textures
                          |> Seq.collect (fun (t, s) -> Seq.map (flip Tup.pairWith t) s)
                          |> Seq.distinctBy (fun (_, (n,r)) -> n)
                          |> Seq.map (fun (t, (n,r)) -> (n, (t, r)))

            return Map.ofSeq sprites
        }   

/// module for generating the .png sprite cache
module ImgCache =
    open System.Drawing
    let createCache map = failwith "ud"
    let createImg (t:tex) (r:rect) = 
        let img = new Bitmap(r.Width, r.Height)
        let data = Array.create (r.Width * r.Height) Microsoft.Xna.Framework.Color.Transparent
        t.GetData(0, nullable r, data, 0, (r.Width * r.Height))
        Rect.origin r
        |> Rect.iter (fun x y -> let c = data.[x + (r.Height * y)]
                                 img.SetPixel(x, y, Color.FromArgb(int c.A,int c.R,int c.G,int c.B)))
        img
    let ofSpriteStore ss = 
        seq { 
            for name, (text, rect) in Map.toSeq ss do
                yield name, createImg text rect
        } 
    let saveToCache directory ss = 
        let sprites = ofSpriteStore ss
        for name, img in sprites do
            let path = (IO.combinePaths directory (name + ".png"))
            img.Save(path, Imaging.ImageFormat.Png)
         
            