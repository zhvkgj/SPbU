namespace hw4

module Task3 =
    type LambdaTerm =
        | Var of char
        | App of LambdaTerm * LambdaTerm
        | LambdaAbstr of LambdaTerm * char

    let getFV (lambdaTerm: LambdaTerm) =
        let rec accFV lambdaTerm acc =
            match lambdaTerm with
            | Var x ->  x::acc
            | App(S, T) -> let lsFst = accFV S acc
                           let lsSnd = accFV T acc
                           List.append lsFst lsSnd

            | LambdaAbstr(S, x) -> 
                accFV S acc |> List.filter ((<>) x)
        accFV lambdaTerm []

    let containsFV (T: LambdaTerm) (S: LambdaTerm) x var =
        getFV T
        |> List.contains x &&
        getFV S
        |> List.contains var

    let rec subst (lambdaTermCur: LambdaTerm) (lambdaTermSubst: LambdaTerm) var =
        match lambdaTermCur with
        | Var x -> if (x = var) then lambdaTermSubst else lambdaTermCur
        | App(S, T) -> 
            App(subst S lambdaTermSubst var, subst T lambdaTermSubst var)
        
        | LambdaAbstr(S, x) -> 
            if (not(containsFV lambdaTermSubst S x var))
            then LambdaAbstr(subst S lambdaTermSubst var, x)
            else
                let z = 
                    ['a'..'z'] 
                    |> List.filter 
                        (fun x ->
                            not(List.contains x ((getFV lambdaTermSubst) @ (getFV S))))
                    |> List.head
                LambdaAbstr( subst (subst S (Var z) x) lambdaTermSubst var, z)
            
    let rec eval (lambdaTerm: LambdaTerm) =
        match lambdaTerm with
        | LambdaAbstr(S, x) -> LambdaAbstr(eval S, x)
        | App (S, T) -> let tempCheck = eval S
                        match tempCheck with 
                        | LambdaAbstr(M, x) -> eval (subst M T x)
                        | _ -> App(tempCheck, eval T)
        | Var x -> Var x
        
