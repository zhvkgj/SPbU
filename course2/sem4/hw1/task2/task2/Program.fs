let fibonacciNum N =
    let rec fibonacci N i prevFirstValue prevSecondValue =
        if i = N then 
            prevSecondValue
        else 
            fibonacci N (i + 1) (prevFirstValue + prevSecondValue) prevFirstValue
    fibonacci N 1 1 0
    
printfn "%d" <| fibonacciNum 5