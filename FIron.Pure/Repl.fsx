#r @"C:\Users\Dan\Documents\Visual Studio 2012\Projects\FIron\packages\FSharpx.Core.1.8.41\lib\40\FSharpx.Core.dll"
#r @"C:\Program Files (x86)\Microsoft XNA\XNA Game Studio\v4.0\References\Windows\x86\Microsoft.Xna.Framework.dll"
#r @"C:\Program Files (x86)\Microsoft XNA\XNA Game Studio\v4.0\References\Windows\x86\Microsoft.Xna.Framework.Game.dll"
#r @"C:\Program Files (x86)\Microsoft XNA\XNA Game Studio\v4.0\References\Windows\x86\Microsoft.Xna.Framework.Graphics.dll"
#r @"C:\Users\Dan\Documents\Visual Studio 2012\Projects\FIron\FIronCS\bin\x86\Debug\FIronCS.dll"

#load "Util.fs"
#load "Geom.fs"

open Util
open Geom

let a = pt(0,0)
let b = pt(1,0)

let recta = rect(0, 0, 10, 10)

Rect.edges recta |> List.ofSeq