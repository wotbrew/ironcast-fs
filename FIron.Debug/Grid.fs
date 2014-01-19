module Grid
open Grid
open FSharpx

let sprintGridRow y cellf grid =
    let w, _ = size grid
    [|
        for x = 0 to w - 1
            do yield cellf (get grid x y)
    |] |> String.ofArray

let printGrid cellf grid = 
    let w, h = size grid
    for y = 0 to h - 1 do 
     printfn "%s" (sprintGridRow y cellf grid)

let sprintGridRowWithPoint (pt:pt) y cellf grid =
    let w, _ = size grid
    [|
        for x = 0 to w - 1 do
            if pt.X = x && pt.Y = y then 
                yield 'X'
            else
                yield cellf (get grid x y)
    |] |> String.ofArray

let printGridWithPoint cellf point grid = 
    let w, h = size grid
    for y = 0 to h - 1 do 
     printfn "%s" (sprintGridRowWithPoint point y cellf grid)

let printCreGrid g = printGrid (Option.map (konst 'C') >> Option.getOrElse '-') g

let printCreGridWithPoint pt g = 
    printGridWithPoint (Option.map (konst 'C') >> Option.getOrElse '-') pt g