let fibonacciNum n =
    let rec fibonacci n i prevFirstValue prevSecondValue =
        match n with
        | _ when n > 0 ->
            if i = n then 
                prevSecondValue
            else 
                fibonacci n (i + 1) (prevFirstValue + prevSecondValue) prevFirstValue
        | _ -> -1
    fibonacci n 1 1 0
    
printfn "%d" <| fibonacciNum 5