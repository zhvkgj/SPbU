namespace hw3

//3.1
module Task1 =
    let firstEvenSearch myList =
        myList
        |> List.map (fun x -> if (x % 2 = 0) then 1 else 0)
        |> List.sum 

    let secondEvenSearch myList =
        myList
        |> List.filter (fun x -> x % 2 = 0)
        |> List.length

    let thirdEvenSearch myList =
        List.fold (fun acc x -> if (x % 2 = 0) then (acc + 1) else acc) 0 myList

//3.2
module Task2 =
    type Tree<'a> =
    | Node of 'a * Tree<'a> * Tree<'a>
    | Tip of 'a 

    let rec treeMap tree mapping =
        match tree with 
        | Node(x , left, right) ->  Node (mapping x, treeMap left mapping, treeMap right mapping)                           
        | Tip x -> Tip(mapping x)

//3.3
module Task3 =
    type ArithmeticExpression =
        | Number of int
        | Addition of  ArithmeticExpression * ArithmeticExpression     
        | Subtraction of ArithmeticExpression * ArithmeticExpression    
        | Multiplication of ArithmeticExpression * ArithmeticExpression 
        | Division of ArithmeticExpression * ArithmeticExpression       

    let rec eval (e: ArithmeticExpression) =
        match e with
        | Number(e1) -> e1
        | Addition(e1, e2) -> eval e1 + eval e2
        | Subtraction(e1, e2) -> eval e1 - eval e2
        | Multiplication(e1, e2) -> eval e1 * eval e2
        | Division(e1, e2) -> match (eval e2) with 
                              | _ when eval e2 <> 0 -> eval e1 / eval e2
                              | _ -> failwith "Divisor cannot be zero!"

//3.4
module Task4 =
    let checkIsPrime num =
        let maxNum = sqrt (float num)
        let rec checkTR (curNum : int) = 
            match curNum with
            | _ when curNum > (int maxNum) && maxNum <> 1.0 -> true
            | _ when (num % curNum <> 0) -> checkTR (curNum + 1)
            | _ -> false
       
        checkTR 2

    let buildInfPrimeSeq = 
        Seq.initInfinite (fun index -> if index = 0 then 2 else 2 * index + 1)
        |> Seq.filter (fun x -> checkIsPrime x)