module Js.Core

open Awesomium
open Awesomium.Core
open FSharpx

open Util

type JSVal = 
    | String of string
    | Num of float
    | Bool of bool
    | Array of JSVal list
    | Obj of (string * JSVal) list
    | Null
    | Func of (JSVal list -> JSVal)
    | Proc of (JSVal list -> unit)


let rec toFsVal (v:JSValue) = 
    if v.IsString then
        String (v.ToString())
    elif v.IsArray then
        let arr = (JSValue.op_Explicit(v):JSValue[])
        Array (arr |> Array.map toFsVal |> Array.toList)
    elif v.IsBoolean then
        let b = JSValue.op_Implicit(v):bool
        Bool b
    elif v.IsDouble || v.IsNumber || v.IsInteger then
        let f = (JSValue.op_Explicit(v):float)
        Num f
    elif v.IsObject then
        let o = (JSValue.op_Implicit(v):JSObject)
        readObj o
    else Null
and readObj (o:JSObject) =
    [for p in o.GetPropertyNames() do
        yield p, toFsVal (o.Property p)] |> Obj
   

let rec fsstr = 
    function 
        | String s -> Some s
        | Num n -> Some <| string n
        | Bool b-> Some <| string b
        | _ -> None
let rec fsstr1 a = 
    match fsstr a with
        | Some s -> s
        | None -> 
            match a with
            | Array x -> List.map fsstr x |> string
            | Obj prs -> List.map (Tup.mapsnd fsstr) prs |> string
            | _ -> "" 
        

let fsfloat = 
    function
        | Num n -> Some n
        | e -> fsstr e |> Option.bind Float.parse
let fsint = fsfloat >> Option.map int

let fsbool = 
    function 
        | Bool b -> Some b
        | Null -> Some false
        | _ -> None
let fsbool1 b = 
    match fsbool b with
        | Some b -> b
        | _ -> true
         
let fslistStr = 
    function
        | Array x -> List.map fsstr x |> Some
        | _ -> None
let fslistFloat =
    function
        | Array x -> Option.mapM fsfloat x
        | _ -> None

let fsmap = 
    function
        | Obj o -> Map.ofList o |> Some
        | _ -> None

let fspair fa fb = 
     function
        | Array [a; b] -> Option.maybe {
                let! x = fa a;
                let! y = fa b;
                return x, y
            }
        | _ -> None

let fstup4 fa fb fc fd =
     function 
        | Array [a; b; c; d;] -> Option.maybe {
               let! q = fa a
               let! w = fb b
               let! e = fc c
               let! r = fd d
               return q,w,e,r
           }
        | _ -> None

let fstupx4 f = 
      function
       | Array x -> List.whenLength 4 x 
                    |> Option.bind (Option.mapM f)
                    |> Option.bind Tup.ofList4
       | _ -> None

let fsrect a = 
      fstupx4 fsint a 
      |> Option.map Geom.Rect.ofTup
      |> Option.orElseF (fun () -> fsstr1 a |> Geom.Rect.parse)


let fspick map prop f = Map.tryFind prop map |> Option.bind f


let jsstr s = String s
let jsint i = Num (float i)
let jsfloat f = Num f
let jsarr x = Array x
let jsobj o = Obj o
let jsbool b = Bool b

let jslookup f map = 
    Map.toSeq map
    |> Seq.map (Tup.mapsnd f)
    |> List.ofSeq
    |> jsobj

let jspair (a, b) = jsarr [jsint a; jsint b;]

let jsfun0 f = Func (fun args -> f())
let jsfun1 fs f = 
    Func (fun args ->
            match args with 
             | a :: _ -> (fs a) |> f
             | _ -> failwith "Invalid arguments")
let jsfun2 fsa fsb f =
    Func (fun args ->
            match args with 
             | a :: b :: _ -> f (fsa a) (fsb b)
             | _ -> failwith "Invalid arguments")

let jsfun3 fsa fsb fsc f =
    Func (fun args ->
            match args with 
             | a :: b :: c :: _ -> f (fsa a) (fsb b) (fsc c)
             | _ -> failwith "Invalid arguments")
let jsproc0 f = Proc (fun args -> f())
let jsproc1 fs f = 
    Proc (fun args ->
               match args with 
                | a :: _ -> fs a |> f
                | _ -> failwith "Invalid arguments")
let jsproc2 fsa fsb f = 
    Proc (fun args ->
               match args with 
                | a :: b :: _ -> f (fsa a) (fsb b)
                | _ -> failwith "Invalid arguments")
let jsproc3 fsa fsb fsc f = 
    Proc (fun args ->
               match args with 
                | a :: b :: c :: _ -> f (fsa a) (fsb b) (fsc c)
                | _ -> failwith "Invalid arguments")


let rec toJsValue v = 
    match v with
     | Bool b -> JSValue(b)
     | String s -> JSValue(s)
     | Num f -> JSValue(f)
     | Array x -> JSValue(Seq.map toJsValue x |> Array.ofSeq)
     | Obj pairs -> createObj pairs
     | Null -> JSValue()
     | _ -> failwith "raw value binding not supported for functions"
and mutObj pairs (o:JSObject) =
    for x, y in pairs do
       match y with 
        | Func a -> o.Bind(x, true, 
                            (fun o (e:JavascriptMethodEventArgs) ->
                                let args = Seq.map toFsVal e.Arguments |> List.ofSeq
                                e.Result <- a args |> toJsValue))
        | Proc a -> o.Bind(x, false, 
                            (fun o (e:JavascriptMethodEventArgs) ->
                                let args = Seq.map toFsVal e.Arguments |> List.ofSeq
                                a args))
        | _ -> o.Property x <- toJsValue y
    o |> JSValue.op_Implicit
and createObj pairs = 
    let o = new JSObject()
    mutObj pairs o

module User =
    open Data.User
    let jsresolution = jspair
    let jssettings s = 
        let res = jsresolution s.resolution
        let fs = jsbool s.fullscreen
        ["resolution", res
         "fullscreen", fs] |> jsobj
    let jsallRes arr = List.map jsresolution arr |> jsarr

    let fsresolution v =
          fspair fsint fsint v
          |> Option.orElseF (fun () -> fsstr v 
                                       |> Option.bind parseResolution)

    let fssettings s = Option.maybe {
        let! map = fsmap s
        let prop b f = fspick map b f
        let! resolution = prop "resolution" fsresolution
        let! fullscreen = prop "fullscreen" fsbool
        return {
            resolution = resolution
            fullscreen = fullscreen
        }
    }

module Cre = 
    open Data.Cre

    let jsrace r = 
        let name = r.name |> jsstr
        let spriteM = fst r.spriteM |> jsstr 
        let spriteF = fst r.spriteF |> jsstr
        ["name", name
         "spriteMale", spriteM
         "spriteFemale", spriteF] |> jsobj

    let jsraceLookup = jslookup jsrace

    



