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
    let buildSquare n =
        let rec buildingSq n acc =
            let edge = String.replicate n "*" + "\n"
            let side = "*" + String.replicate (n - 2) " " + "*\n"
            if (n < 1) then failwith "Wrong input!"
            else
                match acc with
                | 1 -> printf "%s" edge
                | _ when acc = n -> printf "%s" edge
                                    buildingSq n (acc - 1)
                | _ -> printf "%s" side
                       buildingSq n (acc - 1)
        buildingSq n n  
    buildSquare(4)

//task3
module Task3 =
    open System
    open System.Collections.Generic
    
    type MyStack<'T>() =
        let ls = new List<'T>()

        member this.Push value =
            ls.Add(value)

        member this.IsEmpty() =
            ls.Count = 0

        member this.Pop () =
            if (this.IsEmpty()) then
                failwith "Stack is empty!"
            let topIndex = ls.Count - 1
            let value = ls.Item(topIndex)
            ls.RemoveAt(topIndex)
            value
