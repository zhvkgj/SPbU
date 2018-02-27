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
type Tree<'a> =
    | Tree of 'a * Tree<'a> * Tree<'a>
    | Tip of 'a


//3.3
type ArithmeticExpression =
    | Number of int
    | Addition of  ArithmeticExpression * ArithmeticExpression      //+
    | Subtraction of ArithmeticExpression * ArithmeticExpression    //-
    | Multiplication of ArithmeticExpression * ArithmeticExpression //*
    | Division of ArithmeticExpression * ArithmeticExpression       // /

let rec eval (e: ArithmeticExpression) =
    match e with
    | Number(e1) -> e1
    | Addition(e1, e2) -> eval e1 + eval e2
    | Subtraction(e1, e2) -> eval e1 - eval e2
    | Multiplication(e1, e2) -> eval e1 * eval e2
    | Division(e1, e2) -> eval e1 / eval e2

printfn "%d" <| eval (Addition(Number(1), Number(2)))
//3.4
let checkIsPrime num =
    let maxNum = sqrt (float num)
    let rec checkTR (curNum : int) = 
        match curNum with
        | _ when curNum > (int maxNum) && maxNum <> 1.0 -> true
        | _ when (num % curNum <> 0) -> checkTR (curNum + 1)
        | _ -> false
       
    checkTR 2

let buildInfPrimeSeq = 
    Seq.initInfinite (fun index -> index + 1)
    |> Seq.filter (fun x -> checkIsPrime x)

printfn "%A" buildInfPrimeSeq
    

