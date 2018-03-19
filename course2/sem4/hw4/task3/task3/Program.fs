namespace hw4

type lambdaExp =
  | Var of char
  | LambdaAbstr of char * lambdaExp
  | App of lambdaExp * lambdaExp

module Task3 =
 (* Declaring a function which extracts the list 
    of free variables from a lambda term *)
  let searchFV term =
    let rec searchingFV term acc =
      match term with
      | Var(var) -> var::acc
      | LambdaAbstr(var, body) -> List.filter 
                                   (fun cur -> cur <> var) 
                                    (searchingFV body acc)
      | App(L, R) -> let leftPart = searchingFV L acc
                     let rightPart = searchingFV R acc
                     List.append leftPart rightPart
    searchingFV term []

  (* Declaring a function for substituting 
     all free occurrences of a variable 
     x in a term t with the term t *)
  let rec subst S x T =
    match S with
    | Var(var) -> match var with
                  | _ when var = x -> T
                  | _ -> S
    | LambdaAbstr(var, body) -> let FreeVar = List.append 
                                               (searchFV body) 
                                                (searchFV T)
                                match var with
                                | _ when var = x -> S
                                | _ -> let newVar = ['a'..'z']
                                                    |> List.filter (fun curChar -> not (List.contains curChar FreeVar))
                                                    |> List.head 
                                       LambdaAbstr(newVar, subst (subst body var (Var(newVar))) x T)
    | App(L, R) -> App(subst L x T, subst R x T)

  // Declaring a function for beta-reduction
  let rec betaRdct t =
    match t with
    | Var(x) -> Var(x)
    | LambdaAbstr(var, body) -> LambdaAbstr(var, betaRdct body)
    | App(LambdaAbstr(var, body), T) -> betaRdct(subst body var T)
    | App(L, R) -> App(betaRdct L, betaRdct R)
    
    



    



