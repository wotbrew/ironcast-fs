module File

open FSharpx
open Util

let path = Dir.path
let datafile f = IO.combinePaths path (f + ".json")
let allRes = datafile "resolutions"
let video = datafile "video"
let keys = datafile "keys"


let loadf f file = IO.tryReadFile file
                   |> Option.bind f

let loadAll f dir = Option.protect IO.filesInDir dir
                    |> Option.bind (Seq.filter (fun fi -> fi.Extension = ".json")
                                    >> Seq.map (fun fi -> fi.FullName)
                                    >> List.ofSeq
                                    >> Option.mapM f )