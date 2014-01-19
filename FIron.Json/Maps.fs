module Maps


open FSharp.Data.Json
open FSharp.Data.Json.Extensions
open FSharpx
open FSharpx.Collections


let asString (js:JsonValue) = js.AsString()
let asInt (js:JsonValue) = js.AsInteger()
let asArray (js:JsonValue) = js.AsArray()
let asMap  mapv = function
     | JsonValue.Object map -> 
        Map.map (fun k v -> mapv v) map
     | _ -> failwith "json value is not an object"

type TileLayer = {
    name: string
    data: Grid.Grid<int>
}

let parseTile = function
    | "floor" | "Floor" -> Tile.Floor
    | "wall" | "Wall" -> Tile.Wall
    | _ -> Tile.Earth

let parseLayerData = function
    | JsonValue.Array arr -> 
        let typed = Seq.map asInt arr
        typed
    | _ -> failwith "could not parse layer"

//reverse indmap    
let rindmap i (w, h) tw = 
    let height = int <| (i * tw) / w
    let width = int <| (i * tw) - (height * w)
    pt(width, height * tw)

type Object = {
    name: string
    ``type``: string
    rect: rect
    properties: Map<string, string>
}

let parseObject = function 
    | JsonValue.Object map ->
        let find = flip Map.find map
        let name = find "name" |> asString
        let ``type`` = find "type" |> asString
        let x, y = find "x" |> asInt, find "y" |> asInt
        let w, h = find "width" |> asInt, find "height" |> asInt
        let props = find "properties" |> asMap asString
        {
            name = name
            ``type`` = ``type``
            rect = rect(x, y, w, h)
            properties = props
        }
    | _ -> failwith "could not parse object"

type ObjectLayer = {
    name: string
    size: int * int
    objects: Object list
}

let parseObjectLayer = function
    | JsonValue.Object map ->
        let find = flip Map.find map  
        let name = find "name" |> asString
        let size = find "width" |> asInt, find "height" |> asInt
        let objects = find "objects" |> asArray |> Seq.map parseObject |> List.ofSeq
        {
            name = name
            size = size
            objects = objects
        }
    | _ -> failwith "could not parse object layer"

let parseLayer = function
    | JsonValue.Object map ->
        let find = flip Map.find map  
        let name = find "name" |> asString
        let size = find "width" |> asInt, find "height" |> asInt
        let data = find "data" |> parseLayerData |> Seq.toList
        let cells = Seq.mapi (fun i v -> rindmap i size 1, v) data
        let grid = Grid.ofCells1 0 size cells
        {
            name = name
            data = grid
        }
    | _ -> failwith "could not parse layer"

let tileLayerTypeIs t json = json?``type`` |> asString = t
let tileLayerNameIs n json = json?name |> asString = n

let isTileLayer = tileLayerTypeIs "tilelayer"
let isBaseLayer = tileLayerNameIs "base"
let isDecorLayer = tileLayerNameIs "decor"
let isObjLayer = tileLayerTypeIs "objectgroup"

type Tileset = {
    image: string
    name: string
    size: int * int
    gid: int
    tilesize: int * int
    tiles: Map<int, string>
}

let parseTilePair terr (k:string, v) = 
    int k, v?terrain |> asArray |> Seq.head |> asInt |> flip Map.find terr

let parseTiles terr = function 
    | JsonValue.Object map -> Map.toSeq map |> Seq.map (parseTilePair terr) |> Map.ofSeq
    | _ -> failwith "could not parse tiles"

let parseTerrains = function 
    | JsonValue.Array arr -> 
         arr
         |> Seq.mapi (fun i terr -> i, terr.GetProperty "name" |> asString)
         |> Map.ofSeq
    | _ -> failwith "could not parse terrains"

let parseTileset = function
        | JsonValue.Object map ->
            let find = flip Map.find map  
            let tryfind = flip Map.tryFind map
            let terrain = tryfind "terrains" |> Option.map parseTerrains <??> Map.empty
            let tiles = tryfind "tiles" |> Option.map (parseTiles terrain) <??> Map.empty
            let tilesize = find "tilewidth" |> asInt, find "tileheight" |> asInt
            let size = find "imagewidth" |> asInt, find "imageheight" |> asInt
            {
                image = find "image" |> asString |> Strings.replace @"../" "" 
                name = find "name" |> asString
                size = size
                gid = find "firstgid" |> asInt
                tiles = tiles
                tilesize = tilesize
            }
        | _ -> failwith "could not parse tileset"

let parseTilesets = function
    | JsonValue.Array arr -> Seq.map parseTileset arr
    | _ -> failwith "could not parse tilesets"
    
type TiledMap = {
    baseLayer: TileLayer
    decorLayer: TileLayer
    objectLayer: ObjectLayer
    size: int * int
    tilesets: Tileset list
}

let allLayers = Map.find "layers" >> asArray
let allTileLayers = Seq.filter isTileLayer >> List.ofSeq
let allObjLayers = Seq.filter isObjLayer >> List.ofSeq
let baseLayer = Seq.find isBaseLayer >> parseLayer
let decorLayer = Seq.find isDecorLayer >> parseLayer
let objectLayer = Seq.head >> parseObjectLayer

let parseMap = function
    | JsonValue.Object map ->
        let find = flip Map.find map
        let allLayers = allLayers map
        let tileLayers = allTileLayers allLayers
        let objLayers = allObjLayers allLayers
        let tilesets = find "tilesets" |> parseTilesets
        let size = find "width" |> asInt, find "height" |> asInt
        {
            baseLayer = tileLayers |> baseLayer
            decorLayer = tileLayers |> decorLayer
            objectLayer = objLayers |> objectLayer
            size = size
            tilesets = tilesets |> List.ofSeq
        }
    | _ -> failwith "could not parse map"



let readMap = IO.readFileAsString >> JsonValue.Parse >> parseMap

let tileGrid tiledMap = 
    let grid = tiledMap.baseLayer.data
    let terrainMap = tiledMap.tilesets
                     |> Seq.collect (fun ts -> 
                            ts.tiles 
                            |> Map.toSeq 
                            |> Seq.map (Tup.mapfst ((+) ts.gid))) 
                     |> Map.ofSeq
    Grid.map (flip Map.tryFind terrainMap >> Option.getOrElse "wall" >> parseTile) grid

    
let findSprite pt sprites = 
    Seq.tryFind (fun (_, rect:rect) -> rect.Location = pt) sprites

let sheetFileName  = Strings.split '\\' >> Seq.last >> Strings.split '/' >> Seq.last

let findSprites (sheets:Map<string, Sheet>) tileset = 
    let name = tileset.name
    let w, h = tileset.size
    let tw, _ = tileset.tilesize
    let file = sheetFileName tileset.image
    let sheetM = Map.toSeq sheets |> Seq.map (Tup.mapfst sheetFileName) |> Map.ofSeq 
    let _, sprites = Map.find file sheetM
    let result = Seq.init ((w / tw * h / tw)) (fun i -> tileset.gid + i, (findSprite (rindmap i tileset.size tw) sprites))
                 |> Seq.choose (fun (id, sprite) -> Option.map (Tup.pairWith id >> Tup.flip >> Tup.mapsnd fst) sprite)
    result

let spriteGrid layer spriteMap = 
    let grid = layer.data
    Grid.map (flip Map.tryFind spriteMap) grid

let terrainGrid spriteMap tileMap = 
    spriteGrid tileMap.baseLayer spriteMap

let decorGrid spriteMap tileMap = 
    spriteGrid tileMap.decorLayer spriteMap

let spriteMap sheets tileMap = 
    Seq.collect (findSprites sheets) tileMap.tilesets |> Map.ofSeq

let resolveObject o (db:Db.Db) =
    Map.tryFind o.``type`` db.objects
    |> Option.map (Obj.mergeAttr db.sprites o.properties)

let objectGrid db tileMap =
    let layer = tileMap.objectLayer
    let ts = (fst tileMap.tilesets.Head.tilesize)
    let cells = layer.objects |> Seq.collect (fun o -> 
        Geom.Rect.div2 ts o.rect
        |> Geom.Rect.pts 
        |> Seq.map (fun pt -> pt, resolveObject o db))
    Grid.ofCells1 None layer.size cells

let mapInitData blank (db:Db.Db) tileMap = 
    let sheets = db.sheets |> Seq.map (fun (n, spr) -> n, (n, spr)) |> Map.ofSeq
    let spriteMap = spriteMap sheets tileMap
    let sprites = db.sprites
    let dungeon = tileGrid tileMap
    let terrain = terrainGrid spriteMap tileMap
                  |> Grid.map (Option.bind (flip Map.tryFind sprites) >> Option.getOrElse blank)
    let decor = decorGrid spriteMap tileMap
                  |> Grid.map (Option.bind (flip Map.tryFind sprites))
    let objs = objectGrid db tileMap

    {
        dungeon = dungeon
        terrain = terrain
        decor = decor
        obj = objs
    } : MapStack.MapInitData
