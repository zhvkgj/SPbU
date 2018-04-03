namespace Tasks

//task1
module Task1 =
    let avgSum ls =
        let rec sumSin (ls: float list) (sum: float) (count: float) =
            match ls with
            | h::t -> sumSin t (sum + sin h) (count + 1.0)
                  
            | [] -> if (count = 0.0) then 0.0
                    else sum / count
    
        sumSin ls 0.0 0.0

//task2
module Task2 =
    type Tree<'a> =
    | Node of 'a * Tree<'a> * Tree<'a>
    | Empty 

    let checkMin (x: int) (y: int) =
        if (x < y) then x else y

    type ContinuationStep<'a> = 
        | Finished
        | Step of 'a * (unit -> ContinuationStep<'a>)

    let rec mapping tree contFunc =
        match tree with 
        | Empty -> contFunc()
        | Node(x, left, right) -> Step(x, (fun () -> mapping left (fun () -> mapping right contFunc)))

  