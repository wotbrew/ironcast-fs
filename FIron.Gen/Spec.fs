module Spec
open Rand
open Geom

type RoomSpec = {
    width: int * int
    height: int * int
}
type LevelSpec = {
    rooms: (RoomSpec * float) list
    roomCount: int
    size: int * int
}
let levelLimit {size = w,h} = rect(1,1,w-2, h-2)

let randRoomSpecs level = 
    List.weightedRandomize level.rooms
    >> Seq.take level.roomCount 
    >> List.ofSeq

let defaultRoomSpec = {
    width = 4, 14
    height = 4, 14
}
let defaultLevelSpec = {
    rooms = [defaultRoomSpec, 1.0]
    roomCount = 8
    size = 64, 64
}

let genRoom r rspec = randRect2 rspec.width rspec.height r

let placeRoom r lspec room =
    let np = randPtIn (levelLimit lspec) r
    Rect.move1 np room

let roomAllowed existing limit room = 
    not <| Rect.inAny (Rect.expand room 1) existing && Rect.contains limit room

let genRooms level seed = 
    let limit = levelLimit level
    let specs = randRoomSpecs level seed
    let rec genify mi rms specs = 
            match mi, specs with
                | 0, _ -> rms
                | _, spec :: xs -> 
                      let room = genRoom seed spec |> placeRoom seed level
                      if roomAllowed rms limit room then genify mi (room :: rms) xs
                      else genify (mi - 1) rms specs
                | _, [] -> rms
    genify 10000 [] specs

            
