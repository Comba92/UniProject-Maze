(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Maze.fs: maze
* (C) 2019 Alvise Spano' @ Universita' Ca' Foscari di Venezia
*)

module LabProg2019.Maze


open External
open Gfx
open System
open Engine
open System.IO // Name spaces can be opened just as modules
      
[< NoEquality; NoComparison >]
type state = {
    player : sprite
}

let W = 60
let H = 30

type Cell() = 
    member val isWall = true with get, set

type Maze(W, H) =
    let maze = Array2D.init W H (fun _ _ -> new Cell ())

    let isLegal (w,h) =   
        if h >= H || h < 0 || w >= W || w < 0 then false
        else true

    let getRandom ls =      
        let index = rnd_int 0 ((List.length ls)-1)
        List.item index ls

    let getNextIndex (w,h) = 
        let rec checkLegals ls = 
            match ls with 
            | [] -> []
            | ((x,y), midPosition)::tail -> 
                if isLegal (x,y) then 
                    if maze.[x,y].isWall = false then checkLegals tail
                    else ((x,y), midPosition)::(checkLegals tail)
                else checkLegals tail

        let neighbors = [((w-2,h), (w-1,h)); ((w+2,h), (w+1,h));
                         ((w,h-2), (w,h-1)); ((w,h+2), (w,h+1))]

        let newNeighbors = checkLegals neighbors         
        if newNeighbors = [] then ((-1,-1), (-1,-1))    
        else getRandom newNeighbors                     

    let update (w,h) =  
        maze.[w,h].isWall <- false            
        let nextCell = getNextIndex (w,h)
        if nextCell = ((-1,-1), (-1,-1)) then (-1,-1)
        else 
            let (nextW,nextH), (midW,midH) = nextCell
            maze.[midW, midH].isWall <- false  
            (nextW, nextH)

    member this.make_path () = 
        let rec generate stack = 
            if stack = [] then ()
            else 
                let nextCell = update (List.head stack) 
                if nextCell = (-1,-1) then
                    generate (List.tail stack)
                else generate (nextCell::stack)

        in generate [(0,0)]
        maze

    member this.get = this.make_path()

let main () =       
    let engine = new engine (W, H)
    let maze = (Maze (W, H)).get


    let my_update (key : ConsoleKeyInfo) (screen : wronly_raster) (st : state) =
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

    let wall = image.rectangle (1,1, pixel.filled Color.Gray)

    let mazeWalls = [|
        for i in 0..(W-1) do
            for j in 0..(H-1) do 
                if maze.[i,j].isWall = true then engine.create_and_register_sprite (wall, i+1, j+1, 1)
    |]

    // create simple backgroud and player
    //let screen = engine.create_and_register_sprite (image.rectangle (W, H, pixel.filled Color.Gray), 0, 0, 0)
    let player = engine.create_and_register_sprite (image.rectangle (1, 1, pixel.filled Color.Red), 0, 0, 2)

    //screen.draw_text ("Testing", 0, (H-1), Color.Cyan)

    // initialize state
    let st0 = { 
        player = player
        }
    // start engine
    engine.loop_on_key my_update st0
