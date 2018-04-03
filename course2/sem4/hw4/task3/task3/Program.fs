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

    let rec subst (lambdaTermFst: LambdaTerm) (lambdaTermSnd: LambdaTerm) var =
        match lambdaTermFst with
        | Var x -> if (x = var) then lambdaTermSnd else lambdaTermFst
        | App(S, T) -> 
            App(subst S lambdaTermSnd var, subst T lambdaTermSnd var)
        
        | LambdaAbstr(S, x) -> 
            if (not(getFV lambdaTermSnd
                    |> List.contains x && 
                    getFV S 
                    |> List.contains var))
            then LambdaAbstr(subst S lambdaTermSnd var, x)
            else
                let z = 
                    ['a'..'z'] 
                    |> List.filter 
                        (fun x ->
                            not(List.contains x ((getFV lambdaTermSnd) @ (getFV S))))
                    |> List.head
                LambdaAbstr( subst (subst S (Var z) x) lambdaTermSnd var, z)
            
    let rec eval (lambdaTerm: LambdaTerm) =
        match lambdaTerm with
        | LambdaAbstr(S, x) -> LambdaAbstr(eval S, x)
        | App (S, T) -> match eval S with 
        ///вот это очень важно^^^^иначе потеряется рекурсивный вызов
                        | LambdaAbstr(M, x) -> eval (subst M T x)
                        | _ -> App(eval S, eval T)
        | Var x -> Var x
        
