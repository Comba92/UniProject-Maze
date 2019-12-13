open System
open System.Threading

type Cell = ToVisit | Wall | Empty | Exit | Error
type Direction = Up | Left | Down | Right | None
let rnd = new System.Random()

let H = 10
let W = 10 
let mutable maze = [| for _ in 1..H do [| for _ in 1..W do ToVisit |] |]


let debug s1 s2 = 
    printfn "[DEBUG]: @%s - %s" s1 s2
    System.Console.ReadKey()
    0

let get_cell (h,w) = // Maze Getter 
    if (h,w) = (-1,-1) then Error
    else maze.[h].[w]

let set_cell (h,w) value = // Maze Setter
    maze.[h].[w] <- value

let get_near direction (h,w) = 
    let newCoord =        
        match direction with
            | Up -> (h-1, w)
            | Down -> (h+1, w)
            | Left -> (h, w-1)
            | Right -> (h, w+1)
    let newH, newW = newCoord
    if newH < 0 || newH > H || newW < 0 || newW > W then (-1,-1)
    elif get_cell (newH, newW) = Wall || get_cell (newH, newW) = Empty then (-1,-1)
    else newCoord 

let check_neighbors coord cellType =    // Checks how many cells of type cellType are near coord.
    debug "CHECK_NEIGHBORS" "WARNING"
    let mutable count = 0      
    printfn "%A" count
    if get_cell (get_near Up coord) = cellType then printfn "%A" count
                                                    count <- count+1    
    if get_cell (get_near Left coord) = cellType then printfn "%A" count
                                                      count <- count+1
    if get_cell (get_near Down coord) = cellType then printfn "%A"  
                                                      count<- count+1
    if get_cell (get_near Right coord) = cellType then printfn "%A" count   
                                                       count <- count+1
    printfn "COUNT = %A" count

    count

let intToDir direction =        
    let nextCoord =
        match direction with
        | 0 -> Up
        | 1 -> Down
        | 2 -> Left
        | 3 -> Right
    nextCoord

let get_rnd_dir coord =
    let get_rnd_int() =        
        let randInt = rnd.Next(4)
        intToDir randInt
    let mutable randDir = get_rnd_int()
    debug "get_rnd_dir" "Random direction got."
    printfn "%A" randDir
    let mutable newCoord = (get_near randDir coord)
    let mutable whileError = true
    while whileError do
        debug "in while" "Checking if coordinate is legal."
        if newCoord = (-1,-1) then 
            debug "NOT LEGAL" "getting another ranodom..."
            printfn "%A" (check_neighbors newCoord Empty)
            if check_neighbors newCoord Empty = 4 then 
                debug "check_neighbors ""No more cells to visit"
                newCoord <- coord
                randDir <- None 
                whileError <- false
            else 
                randDir <- get_rnd_int()
                printfn "%A" randDir
                newCoord <- (get_near randDir coord)
       (* elif check_neighbors newCoord Empty = 4 then 
            debug "check_neighbors = 4" "No more near cells to visit."
            *)
        else whileError <- false
    (newCoord, randDir)
        

let get_opposite_wall direction = 
    match direction with
        | Up -> Left
        | Down -> Right
        | Left -> Down
        | Right -> Up

let check_wall (h,w) =
    if check_neighbors (h,w) Wall = 0  // If there aren't any Walls near, must add one
        then false
    else true

let mazePrint() = 
    System.Console.Clear()
    for row in maze do 
        for cell in row do
            match cell with
                | Empty -> printf "0 "
                | Wall -> printf "X "
                | ToVisit -> printf "\ "
        printf "\n"
    printf "\n"

// Caso Base -> Visitate tutte le celle (ritornato alla coordinata di partenza)
// Caso Ind. 1 -> Non ci sono più celle vicine da visitare; torno indietro
// Caso Ind. 2 -> Ci sono celle vicine da visitare; ne visito una
let make_path (h,w) = 
    let rec aux stack debugCycle = 
        let h, w = List.head stack
        set_cell (h,w) Empty
        
        debug "make_path, aux" "Cell set. Continuing..."
        let nextCoord, direction = get_rnd_dir (h,w)
        let nextH, nextW = nextCoord

        debug "make_path, aux" "Random coordinate got. Continuing..."

        
        (*if check_wall (h,w) then ()
        else
            debug "make_path, aux" "Setting Walls..."
            let wallDir = get_opposite_wall direction     // Returns opposite direction for setting the wall
            set_cell (get_near wallDir (h,w)) Wall       // Sets the Wal
        *)

        debug "make_path, aux" "Wall set. Continuing..."

        mazePrint()
        printfn "ITER %d /
        stack = %A - Coordinate = (%d, %d) - NextCoordinate = (%d, %d), Direction (%A)" debugCycle stack h w nextH nextW direction
        
        if stack = [] then ()
        elif 
            check_neighbors (nextH, nextW) ToVisit = 0 then 
                debug "No more near cells."  "Popping the stack."
                aux (List.tail stack) (debugCycle + 1)
        else aux ((nextH, nextW) :: stack) (debugCycle + 1)

    debug  "make_path" "Starting. "
    aux [(h,w)] 1


[<EntryPoint>]
let main argv =
    make_path (0,0)
    0