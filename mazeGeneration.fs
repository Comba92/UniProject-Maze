open System

type Cell() = 
    member val isWall = true with get, set

type Maze(H,W) =

    let maze = Array2D.init H W (fun _ _ -> new Cell ())
    let oddWidth = if W % 2 = 1 then true else false    // Flag per la stampa corretta
    let oddHeight = if H % 2 = 1 then true else false 

    let isLegal (h,w) =     // Controlla se una coordinata esce dall'array
        if h >= H || h < 0 || w >= W || w < 0 then false
        else true

// Ottiene un elemento random da una lista (serve per scegliere uno a caso tra le celle vicine visitabili disponibili)
    let getRandom ls =      
        let rnd = new Random()
        let index = rnd.Next (List.length ls)
        List.item index ls


// Ritorna le coordinate della prossima cella scelta a caso; deve ritornare due coordinate dato che la prossima cella è 
// più avanti di due blocchi, e abbiamo bisogno anche della coordinata della cella tra quella d'origine e quella di 
// arrivo. Se non ci sono celle vicine visitabili, ritorna il caso speciale ((-1,-1),(-1,-1))
    let getNextIndex (h,w) = 
        // checkLegals controlla una ad una le vicine e costruisce una lista di celle vicine visitabili
        let rec checkLegals ls = 
            match ls with 
            | [] -> []
            | ((x,y), midPosition)::tail -> 
                if isLegal (x,y) then 
                    if maze.[x,y].isWall = false then checkLegals tail
                    else ((x,y), midPosition)::(checkLegals tail)
                else checkLegals tail

        let neighbors = [((h-2,w), (h-1,w)); ((h+2,w), (h+1,w));
                         ((h,w-2), (h,w-1)); ((h,w+2), (h,w+1))]    // Lista con tutte le celle vicine

        let newNeighbors = checkLegals neighbors        // Qui controllo quali sono le celle vicine visitabili 
        if newNeighbors = [] then ((-1,-1), (-1,-1))    // Non ci sono celle vicine visitabili
        else getRandom newNeighbors                     // Se ci sono celle vicine visitabili, sceglie una random
            

// Aggiorna le celle adeguatamente e ritorna la posizione della cella in cui ci siamo spostati.
    let update (h,w) =  
        maze.[h,w].isWall <- false             // Aggiorna la cella corrente
        let nextCell = getNextIndex (h,w)
        if nextCell = ((-1,-1), (-1,-1)) then (-1,-1)
        else 
            let (nextH,nextW), (midH,midW) = nextCell
            maze.[midH, midW].isWall <- false  // Aggiorna cella in mezzo
            (nextH, nextW)


// Genera il percorso ricorsivamente. Stack è una lista che salva tutte le posizioni visitate in modo da poter tornare 
// indietro in caso non ci siano più celle vicine visitabili
    member this.make_path () = 
        let rec generate stack = 
            if stack = [] then ()
            else 
                let nextCell = update (List.head stack) 
                if nextCell = (-1,-1) then
                    generate (List.tail stack)
                else generate (nextCell::stack)

        in generate [(0,0)]
        this.printer()
                  
                  
    member this.printer() =
        let drawBorders() = 
            for _ in 0..(W-1) do
                printf "* *"
            if oddWidth = true then printf "* *\n"
            else printf "*\n"

        drawBorders() // Bordo alto

        for i in 0..(H-1) do 
            printf "* "     // Bordo sinistro
            for j in 0..(W-1) do 
                if maze.[i,j].isWall = true then printf "* "
                else printf "  "
            if oddWidth = true then printf "*\n"    // Bordo destro solo se Width è dispari
            else printf "\n"

        if oddHeight then drawBorders() // Bordo basso


[<EntryPoint>]
let main argv = 
    let maze = new Maze(30,30)
    maze.make_path()
    0
