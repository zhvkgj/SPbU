namespace Tasks

open System
open System.Collections.Generic

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

module Task3 =
     type MyPriorityQueue<'c>() = 
        let myList = new List<'a * 'b>()
        let rec findMin (ls: List<'a * 'b>) maxPr index =
            if (ls.Count = 0) then index
            elif (snd (ls.Item (0)) > maxPr) then 
                let tempList = ls
                tempList.RemoveAt(0)
                findMin tempList (snd (ls.Item (0))) (index + 1)
            else 
                let tempList = ls
                tempList.RemoveAt(0)
                findMin tempList maxPr (index + 1)
        let rec findMax (ls: List<'a * 'b>) maxPr index =
            if (ls.Count = 0) then index
            elif (snd (ls.Item (0)) < maxPr) then 
                let tempList = ls
                tempList.RemoveAt(0)
                findMax tempList (snd (ls.Item (0))) (index + 1)
            else 
                let tempList = ls
                tempList.RemoveAt(0)
                findMax tempList maxPr (index + 1)
        member this.push value prVal = 
            myList.Add(value, prVal)
        member this.FindMin() = findMin myList 0 0 
        member this.FindMax() = findMax myList 10000 0
        member this.DeleteMin() =
            if (this.IsEmpty()) then
                failwith "Queue is empty" 
            let value = myList.Item (this.FindMin())
            myList.RemoveAt (this.FindMin())
            value
        member this.DeleteMax() =
            if (this.IsEmpty()) then
                failwith "Queue is empty" 
            let value = myList.Item (this.FindMax())
            myList.RemoveAt (this.FindMax())
            value
        member this.IsEmpty() =
            myList.Count = 0
