//3.1
 let firstEvenSearch myList =
   myList
   |> List.map (fun x -> (x + 1) % 2)
   |> List.sum 

let secondEvenSearch myList =
    myList
    |> List.filter (fun x -> x % 2 = 0)
    |> List.length

let thirdEvenSearch myList =
    List.fold (fun acc x -> if (x % 2 = 0) then (acc + 1) else acc) 0 myList

//3.2
