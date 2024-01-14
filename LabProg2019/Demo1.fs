(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Demo1.fs: sample usage of engine
* (C) 2019 Alvise Spano' @ Universita' Ca' Foscari di Venezia
*)

module LabProg2019.Demo1

open System
open Engine
open Gfx
    
let player_speed_step = 0.1

[< NoEquality; NoComparison >]
type state = {
    player_speed : float
    sprites : (sprite * float * float)[]
    player : sprite
}

let main () =       
    let engine = new engine (80, 50)
    let rec rnd_px () =
        let col = rnd_color ()
        if col = Color.Black then rnd_px ()
        else pixel.filled col

    let my_update (keyo : ConsoleKeyInfo option) (screen : wronly_raster) (inf : info) (st : state) =
        let speed =
            match keyo with
            | None -> st.player_speed
            | Some key ->
                // move player
                let dx, dy =
                    let u = st.player_speed
                    match key.KeyChar with 
                    | 'w' -> 0., -u
                    | 's' -> 0., u
                    | 'a' -> -u, 0.
                    | 'd' -> u, 0.
                    | _   -> 0., 0.
                st.player.move_by (dx, dy)                
                // modify speed
                match key.KeyChar with
                | '+' -> st.player_speed + player_speed_step
                | '-' -> st.player_speed - player_speed_step
                | _   -> st.player_speed
        let speed = max player_speed_step speed
        // move other sprites
        let sprites = [|
            for spr, dx, dy in st.sprites do
                spr.move_by (dx, dy)
                let dx = if int spr.x + spr.width >= screen.width || int spr.x <= 0 then -dx else dx
                let dy = if int spr.y + spr.height >= screen.height || int spr.y <= 0 then -dy else dy
                yield spr, dx, dy
            |]
        // print message
        screen.draw_text (sprintf "Use + and - keys to increase/decrease player sprite speed\nCurrent speed: %g" st.player_speed, 0, screen.height - 2, Color.White, Color.DarkMagenta)
        // print engine info
        screen.draw_text (sprintf "frame count: %d  timer: %g secs" inf.frame_cnt inf.timer, 0, screen.height - 3, Color.Blue, Color.DarkGray)
        // calculate next state
        { sprites = sprites     // bind the new sprite array that we recalculate every frame
          player_speed = speed  // bind the new speed that we calculate every frame 
          player = st.player    // this does not change
          }, match keyo with None -> false | Some k -> k.KeyChar = 'q'


    // create sprites    
    let sprites = [|
        let rnd_dim () = rnd_int 1 10
        let gradient f =
            let d = rnd_dim ()
            let img = new image (d, d)
            for i = img.width / 2 downto 1 do
                f img i (rnd_px ())
            img 
        for i = 1 to 10 do
            let img =
                let fpx = if rnd_bool () then Some (rnd_px ()) else None
                match rnd_int 0 3 with
                | 0 -> image.rectangle (rnd_dim (), rnd_dim (), rnd_px (), ?filled_px = fpx)
                | 1 -> image.circle (rnd_dim (), rnd_px (), ?filled_px = fpx)
                | _ -> gradient <| fun img i px -> img.draw_rectangle (i, i, img.width - i * 2, img.height - i * 2, px)
                // | _ -> gradient <| fun img i px -> let c = img.width / 2 in img.draw_unfilled_circle (c, c, i - 1, px); img.flood_fill (c, c, px)

            let spr = engine.create_and_register_sprite (img, rnd_int 3 (engine.screen_width - 3), rnd_int 3 (engine.screen_height - 3), i)
            let d () = rnd_float -2. 2.
            yield spr, d (), d ()
        |]
    // create player sprite
    let player = let d = rnd_int 2 9 in engine.create_and_register_sprite (d, d, engine.screen_width / 3, engine.screen_height / 3, 100)
    for i = 0 to player.width / 2 do
        player.draw_rectangle (i, i, player.width - i * 2, player.height - i * 2, rnd_px ())
    player.draw_text ("P1", 1, 1, rnd_color ())
    // define initial state
    let st0 = { 
        player_speed = 1.
        sprites = sprites
        player = player
        }
    // start engine loop
    engine.loop my_update st0

