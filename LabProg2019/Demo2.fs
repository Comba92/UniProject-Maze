(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Demo1.fs: sample usage of engine
* (C) 2019 Alvise Spano' @ Universita' Ca' Foscari di Venezia
*)

module LabProg2019.Demo2

open System
open Engine
open Gfx
    

[< NoEquality; NoComparison >]
type state = {
    player : sprite
}

let W = 60
let H = 30

let main () =       
    let engine = new engine (W, H)

    let my_update (key : ConsoleKeyInfo) (screen : wronly_raster) (inf : info) (st : state) =
        // move player
        let dx, dy =
            match key.KeyChar with 
            | 'w' -> 0., -1.
            | 's' -> 0., 1.
            | 'a' -> -1., 0.
            | 'd' -> 1., 0.
            | _   -> 0., 0.
        // TODO: check bounds
        st.player.move_by (dx, dy)
        st, key.KeyChar = 'q'


    // create simple backgroud and player
    ignore <| engine.create_and_register_sprite (image.rectangle (W, H, pixel.filled Color.Yellow, pixel.filled Color.Blue), 0, 0, 0)
    let player = engine.create_and_register_sprite (image.circle (2, pixel.filled Color.White, pixel.filled Color.Gray), W / 2, H / 2, 1)

    // initialize state
    let st0 = { 
        player = player
        }
    // start engine
    engine.loop_on_key my_update st0

