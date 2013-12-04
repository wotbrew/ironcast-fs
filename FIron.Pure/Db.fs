module Db
open Util

type Db = {
    allRes : (int * int) list
    sprites : Map<string, Sprite>
    races : Map<string, Cre.Race>
    themes : Map<string, World.Theme>
}