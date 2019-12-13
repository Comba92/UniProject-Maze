module mazeGeneration.Main
open System
open System.Threading

type Cell = ToVisit | Wall | Empty | Exit
type Direction = Up | Left | Down | Right
let rnd = new System.Random()

let H = 10 * 2
let W = 10 * 2
let mutable maze = [| for _ in 1..H do [| for _ in 1..W do ToVisit |] |]

let printer = 
    for row in maze do 
        for cell in row do 
            match cell with
                | ToVisit -> printf "\ "
                | Wall -> printf "X "
                | Empty -> printf "  "
                | Exit -> printf "E "
        printf "\n"

let debug s1 s2 = 
    printfn "[DEBUG]: @%s - %s" s1 s2
    System.Console.ReadKey()
    0

let get_cell (h,w) = // Maze Getter 
    maze.[h].[w]

let set_cell (h,w) value = // Maze Setter
    maze.[h].[w] <- value

let check_bounds (h,w) = // Checks if a coordinate is legal
    debug "check_bounds" "Checking Bounds..."
    if h < 0 || h > H || w < 0 || w > H then not (get_cell (h,w) = Wall) 
    else true

let get_near direction (h,w) =  // Returns the next coord. given the direction and the current coord. 
    let nextCoord = 
        match direction with
            | Up -> (h-1, w)
            | Down -> (h+1, w)
            | Left -> (h, w-1)
            | Right -> (h, w+1)
    if check_bounds nextCoord then nextCoord
    else (-1,-1)
    
let rec get_rnd_cell coord = 
    let get_rnd_dir = 
        let random = rnd.Next(4)
        let direction = 
            match random with
                | 0 -> Up
                | 1 -> Left
                | 2 -> Down
                | 3 -> Right
        direction 
    let direction = get_rnd_dir
    let nextCoord = get_near direction coord 
    if nextCoord = (-1,-1) then get_rnd_cell coord
    else (nextCoord, direction)


let check_neighbors coord cellType =    // Checks how many cells of type cellType are near coord.
    let mutable count = 0      
    if get_cell (get_near Up coord) <> cellType then count <- count+1
    if get_cell (get_near Left coord) <> cellType then count <- count+1
    if get_cell (get_near Down coord) <> cellType then count <- count+1
    if get_cell (get_near Right coord) <> cellType then count <- count+1

    count



let get_opposite_wall direction = 
    match direction with
        | Up -> Left
        | Down -> Right
        | Left -> Down
        | Right -> Up

let nearby_wall (h,w) =
    if check_neighbors (h,w) Wall = 0  // If there aren't any Walls near, must add one
        then false
    else true

// Caso Base -> Visitate tutte le celle (ritornato alla coordinata di partenza)
// Caso Ind. 1 -> Non ci sono più celle vicine da visitare; torno indietro
// Caso Ind. 2 -> Ci sono celle vicine da visitare; ne visito una
let make_path (h,w) = 
    let rec aux stack debugCycle =
        printer
        let h, w = List.head stack
        set_cell (h,w) Empty
        
        debug "make_path, aux" "Cell set. Continuing..."
        let nextCoord, nextDir = get_rnd_cell (h,w)
        let nextH, nextW = nextCoord

        debug "make_path, aux" "Random coordinate got. Continuing..."

       
        if nearby_wall (h,w) then ()
        else
            debug "make_path, aux" "Setting Walls..."
            let wallDir = get_opposite_wall nextDir      // Returns opposite direction for setting the wall
            debug "DANGER" "ENTERING DANGER ZONE"
            set_cell (get_near wallDir (h,w)) Wall       // Sets the Wal
        

        debug "make_path, aux" "Wall set. Continuing..."

        printfn "ITER %d /
        stack = %A - Coordinate = (%d, %d) - NextCoordinate = (%d, %d)" debugCycle stack h w nextH nextW
        if stack = [] then ()
        elif check_neighbors (nextH, nextW) ToVisit = 0 then aux (List.tail stack) (debugCycle + 1)
        else aux ((nextH, nextW) :: stack) (debugCycle + 1)

    debug  "make_path" "Starting. "
    aux [(h,w)] 1


[<EntryPoint>]
let main argv =
    make_path (5,5)
    0