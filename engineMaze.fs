(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Maze.fs: maze
* (C) 2019 Alvise Spano' @ Universita' Ca' Foscari di Venezia
*)

module LabProg2019.Maze

open Gfx
open System
open Engine
      
[< NoEquality; NoComparison >]
type state = {
    player : sprite
}

// Per i bordi corretti, le grandezze devono essere DISPARI
let W = 21
let H = 21

type Cell() = 
    member val isWall = true with get, set
    member val isPath = false with get, set
    member val isVisited = false with get, set

type Maze(W, H) =
    let maze = Array2D.init W H (fun _ _ -> new Cell ())
    let startingCell = (0,0)
    let exitCell = (W-2, H-2)  // -2 perché W e H sono pari, exitCell deve essere pari...

    let isLegal (w,h) =   
        if h >= H || h < 0 || w >= W || w < 0 then false
        else true

    let getRandom ls =      
        let index = rnd_int 0 ((List.length ls)-1)
        List.item index ls

    let rec checkLegals ls = 
        match ls with 
        | [] -> []
        | ((x,y), midPosition)::tail -> 
            if isLegal (x,y) then 
                    if maze.[x,y].isWall = false then checkLegals tail 
                    else ((x,y), midPosition)::(checkLegals tail)
            else checkLegals tail

    let rec checkLegals2 ls = 
        match ls with
        | [] -> []
        | (x,y):: t -> 
            if isLegal (x,y) then 
                if maze.[x,y].isWall then checkLegals2 t
                elif maze.[x,y].isVisited then checkLegals2 t
                else (x,y)::(checkLegals2 t)
            else checkLegals2 t

    let getAllNeighbors (h,w) = 
        [((h-2,w), (h-1,w)); ((h+2,w), (h+1,w));
        ((h,w-2), (h,w-1)); ((h,w+2), (h,w+1))]

    let getLegalNeighbors (h,w) =
        checkLegals (getAllNeighbors (h,w))

    let getLegalNeighbors2 (h,w) =
        checkLegals2 (List.map (fun x -> snd x ) (getAllNeighbors (h,w)))

    let getNextIndex (h,w) =  
        let legalNeighbors = getLegalNeighbors (h,w)
        if legalNeighbors = [] then ((-1,-1), (-1,-1))    
        else getRandom legalNeighbors                         

    let update (w,h) =  
        maze.[w,h].isWall <- false
        if (w,h) = exitCell then (-1,-1) // funziona solo con exitCell pari....
        else 
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

        generate [startingCell]

    member this.find_path () = 
        let rec iterate ls p stack =
            match ls with 
            | [] -> ()
            | h :: t -> let x,y = h 
                        maze.[x,y].isPath <- true
                        maze.[x,y].isVisited <- true
                        p ((x,y)::stack)
        
        let rec find stack = 
            if (List.head stack) = exitCell then ()
            else 
                let x,y = List.head stack
                
                
                let neighbors = (getLegalNeighbors2 (List.head stack))
                if neighbors = [] then 
                    maze.[x,y].isPath <- false
                    find (List.tail stack)
                else 
                    iterate neighbors find stack    // I FOR PUZZANO COSI HO FATTO STA PORCHERIA RICORSIVA!
        in find [startingCell]


    member this.get = maze
    member this.exit = exitCell

let win() = exit(0)

let userGame () =       
    let engine = new engine (W, H)
    let mazeObj = new Maze ((W-1), (H-1)) 
    mazeObj.make_path()
    let maze = mazeObj.get // Mi salvo la struttura del labirinto

    let exitx, exity = let a, b = mazeObj.exit  // Prendo coordinate exit, aggiungo +1 per visualizzazione
                       (a+1, b+1)
    
    engine.show_fps <- false

    
    let my_update (key : ConsoleKeyInfo) (screen : wronly_raster) (st : state) =
        // move player
        let dx, dy, prevx, prevy =
            match key.KeyChar with 
            | 'w' -> 0., -1., 0., 1.
            | 's' -> 0., 1., 0., -1.
            | 'a' -> -1., 0., 1., 0.
            | 'd' -> 1., 0., -1., 0.
            | _   -> 0., 0., 0., 0.

        // Muoviamo il player; se è legale tieni la posizione, altrimenti torna indietro.
        st.player.move_by (dx, dy)
        let px, py = (int(st.player.x))-1, (int (st.player.y))-1    // L'indicizzazione dell'oggetto maze è uguale a quella dell'engine-1

        // Controllo collisione coi bordi
        if px+1 = 0 || px+1 = W || py+1 = 0 || py+1 = H then st.player.move_by (prevx, prevy)
        // Controllo collisione coi muri
        elif maze.[px, py].isWall = true then st.player.move_by (prevx, prevy)
        // Controllo collisione exit
        if (int(st.player.x), int(st.player.y)) = (exitx, exity) then win()
        st, key.KeyChar = 'q'


    let wall = image.rectangle (1,1, pixel.filled Color.Gray)

    // Stampa delle mura
    let mazeWalls = [|
        for i in 0..(W-2) do
            for j in 0..(H-2) do 
                if maze.[i,j].isWall = true then engine.create_and_register_sprite (wall, i+1, j+1, 1)
    |]

    let screen = engine.create_and_register_sprite (image.rectangle (W, H, pixel.filled Color.Gray), 0, 0, 0)

    // create simple backgroud and player
    let player = engine.create_and_register_sprite (image.rectangle (1, 1, pixel.filled Color.Red), 1, 1, 2)
    let exit = engine.create_and_register_sprite (image.rectangle (1,1, pixel.filled Color.Yellow), exitx, exity, 3)

    // initialize state
    let st0 = { 
        player = player
        }

    // start engine
    engine.loop_on_key my_update st0

let cpuGame () = 
    let engine = new engine (W, H)
    let mazeObj = new Maze ((W-1), (H-1)) 
    mazeObj.make_path()
    mazeObj.find_path()
    let maze = mazeObj.get // Mi salvo la struttura del labirinto

    let exitx, exity = let a, b = mazeObj.exit  // Prendo coordinate exit, aggiungo +1 per visualizzazione
                       (a+1, b+1)
    
    engine.show_fps <- false
    
    let my_update (key : ConsoleKeyInfo) (screen : wronly_raster) (st : state) =
        st, key.KeyChar = 'q'


    let wall = image.rectangle (1,1, pixel.filled Color.Gray)
    let path = image.rectangle (1,1, pixel.filled Color.Red)

    // Stampa delle mura
    let mazeWalls = [|
        for i in 0..(W-2) do
            for j in 0..(H-2) do 
                if maze.[i,j].isWall then engine.create_and_register_sprite (wall, i+1, j+1, 1)
    |]

    let mazePath = [|
        for i in 0..(W-2) do 
            for j in 0..(H-2) do
                if maze.[i,j].isPath then engine.create_and_register_sprite (path, i+1, j+1, 1)
    |]

    let screen = engine.create_and_register_sprite (image.rectangle (W, H, pixel.filled Color.Gray), 0, 0, 0)

    // create simple backgroud and player
    let start = engine.create_and_register_sprite (path, 1, 1, 1)
    let exit = engine.create_and_register_sprite (image.rectangle (1,1, pixel.filled Color.Yellow), exitx, exity, 3)

    // initialize state
    let st0 = { 
        player = start
        }
    // start engine
    engine.loop_on_key my_update st0


let main () =
    let mainW, mainH = 60, 20
    let engine = new engine (mainW, mainH)
    engine.show_fps <- false

    let my_update (key : ConsoleKeyInfo) (screen : wronly_raster) (st : state) =
        let dx, dy =
            match key.KeyChar with
            //| 'w' -> 0., -20.
            //| 's' -> 0., 20.
            | 'a' -> -20., 0.
            | 'd' -> 20., 0.
            | ' ' -> 1., 1.
            | 'e' -> 2.,2.
            | _   -> 0., 0.
        if (dx, dy) = (1.,1.) then userGame()
        elif (dx, dy) = (2.,2.) then cpuGame()
        else 
            st.player.move_by (dx, dy)
        st, key.KeyChar = 'q'

    let screen = engine.create_and_register_sprite (image.rectangle (mainW, mainH, pixel.filled Color.Black), 0, 0, 0)
    let box1 = engine.create_and_register_sprite (image.rectangle (15, 6, pixel.filled Color.Gray, pixel.filled Color.Gray), 10, 5, 1)
    let box2 = engine.create_and_register_sprite (image.rectangle (15, 6, pixel.filled Color.Gray, pixel.filled Color.Gray), 30, 5, 1)
    let selection = engine.create_and_register_sprite (image.rectangle (17, 8, pixel.filled Color.Red), 9, 4, 2)

    screen.draw_text ("Testing... Press Space for userGame, E for CPUGame", 0, 0, Color.Blue)
    box1.draw_text ("Find the Exit", 1, 3, Color.White)
    box2.draw_text (" Auto Finder!", 1, 3, Color.White)

    let st0 = {
        player = selection
        }
    engine.loop_on_key my_update st0
