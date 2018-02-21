let rec pow2 n = 
    if n = 1 then 2
    else 2 * pow2 (n-1)

let listBuilder n m =
    let value = pow2 n
    let rec listTemp temp m =
        match m with
        |_ when m < 0 -> []
        |_ when m >= 0 -> temp :: listTemp (temp * 2) (m - 1)
    listTemp value m
    
