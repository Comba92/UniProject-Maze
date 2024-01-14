module LabProg2019.Main
open System
open System.Threading
open Maze

type Cell() = 
    member val isWall = true with get, set
    member val isPath = false with get, set
    member val isVisited = false with get, set

type Maze(H,W) =
    let maze = Array2D.init H W (fun _ _ -> new Cell ())
    let oddHeight = if H % 2 = 1 then true else false
    let oddWidth = if W % 2 = 1 then true else false

    let startingCell = (0,0)
    let exitCell = let h = if oddHeight then H-1 else H-2
                   let w = if oddWidth then W-1 else W-2
                   (h,w)

    let isLegal (h,w) =
        if h >= H || h < 0 || w >= W || w < 0 then false
        else true

    let getRandom ls =      
        let rnd = new Random()
        let index = rnd.Next (List.length ls)
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

    let update (h,w) =  
        maze.[h,w].isWall <- false
        if (h,w) = exitCell then (-1,-1)
        else 
        let nextCell = getNextIndex (h,w)
        if nextCell = ((-1,-1), (-1,-1)) then (-1,-1)
        else 
            let (nextH,nextW), (midH,midW) = nextCell
            maze.[midH, midW].isWall <- false 
            (nextH, nextW)


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
                        p ((x,y)::stack)
        
        let rec find stack = 
            if (List.head stack) = exitCell then ()
            else 
                let x,y = List.head stack
                maze.[x,y].isVisited <- true
                
                let neighbors = (getLegalNeighbors2 (List.head stack))
                if neighbors = [] then 
                    maze.[x,y].isPath <- false
                    find (List.tail stack)
                else 
                    iterate neighbors find stack    // I FOR PUZZANO COSI HO FATTO STA PORCHERIA RICORSIVA!
        in find [startingCell]
                  
    member this.printer() =
        let drawBorders() = 
            for _ in 0..(W-1) do
                printf "██" 
            if oddWidth = true then printf "████\n"
            else printf "\n"

        drawBorders() // Bordo alto

        for i in 0..(H-1) do 
            printf "██"   // Bordo sinistro
            for j in 0..(W-1) do 
                if maze.[i,j].isWall then printf "██"
                elif (i,j) = exitCell then printf "EE"
                elif maze.[i,j].isPath then printf "XX" 
                else printf "  "
            if oddWidth = true then printf "██\n"    // Bordo destro solo se Width è dispari
            else printf "\n"

        if oddHeight then drawBorders() // Bordo basso

    member this.rawPrint() = 
        for i in 0..(H-1) do 
            printf " "
            for j in 0..(W-1) do 
                if maze.[i,j].isWall = true then printf "██"
                elif (i,j) = exitCell then printf "EE"
                else printf "  "
            printf "\n"


[<EntryPoint>]
let main argv = 
    mainGame()
    0
