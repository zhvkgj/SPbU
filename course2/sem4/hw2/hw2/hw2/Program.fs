//2.1
let comp number =
    let rec tempFunc number curNum =
        if (number / 10 = 0) then curNum
        else curNum * tempFunc (number / 10) ((number / 10) % 10) 
    tempFunc number (number % 10)
      
printfn "%d" (comp 656)

//2.2
let searchNum list1 num =
    let rec tempFunc list1 num curIndex =
        match list1 with
        | [] -> None
        | _ -> if (List.head list1 = num) then Some (curIndex)
               else tempFunc (List.tail list1) num (curIndex + 1)
    tempFunc list1 num 0

printfn "%O" (searchNum [1..3] 3) 

//2.3
let checkStr str =
    let lengthStr = String.length str
    [0..lengthStr / 2]
    |> List.forall (fun index -> str.[index] = str.[lengthStr - index - 1]) 

printfn "%b" (checkStr "0123210")

//2.4

