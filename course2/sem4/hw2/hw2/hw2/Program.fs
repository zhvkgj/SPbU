//2.1
let mul number =
    let rec recursiveMul number curNum =
        if (number / 10 = 0) then curNum
        else curNum * recursiveMul (number / 10) ((number / 10) % 10) 
    recursiveMul number (number % 10)
      
printfn "%d" <| mul 656

//2.2
let searchNum list1 num =
    let rec recursiveSearch list1 num curIndex =
        match list1 with
        | [] -> None
        | _ -> if (List.head list1 = num) then Some (curIndex)
               else recursiveSearch (List.tail list1) num (curIndex + 1)
    recursiveSearch list1 num 0

printfn "%O" <| searchNum [1..3] 3

//2.3
let checkStr str =
    let lengthStr = String.length str
    [0..lengthStr / 2]
    |> List.forall (fun index -> str.[index] = str.[lengthStr - index - 1]) 

printfn "%b" <| checkStr "0123210"

//2.4
let rec split ls left right =
    match ls with
    | [] -> (left, right)
    | [a] -> (a::left, right)
    | a::b::tail -> split tail (a::left) (b::right)

let rec merge left right =
    match (left, right) with
    | ([], myList) -> myList
    | (myList, []) -> myList
    | (headLeft::tailLeft, headRight::tailRight) -> 
        match headLeft <= headRight with
        | true -> headLeft :: merge tailLeft (headRight::tailRight)
        | false -> headRight :: merge (headLeft::tailLeft) tailRight

let rec sortPart listPart =
    match listPart with
    | [_] | [] -> listPart
    | _ -> let (left, right) = split listPart [] []
           merge (sortPart left) (sortPart right) 

printfn "%A" <| sortPart [1234; 10; 150; -12334; 0; 1245; 11]