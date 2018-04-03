namespace hw5

module Task1 =
  let checkBrackets (str: string) =
    let myList = Seq.toList str
    let listForCheck = [('(', ')'); ('{', '}'); ('[', ']')]
    let rec check curList acc =
        match curList with
        | hd::tl -> match hd with
                    | '(' | '{' | '[' -> check tl (hd::acc)
                    | ')' | '}' | ']' -> if (not(List.isEmpty acc) && List.head acc = (listForCheck |> List.find ((=) hd << snd) |> fst))
                                            then check tl (List.tail acc)
                                         else false
                        
                    | _ -> check tl acc

        | [] -> List.isEmpty acc 
                   
    check myList []

// func x l = List.map (fun y -> y * x) l
module Task2 =
   
   let listMul'0 count ls = 
        List.map (fun y -> y * count) ls

   let listMul'1 count : int list -> int list = 
        List.map (fun y -> y * count)
   
   let listMul'2: int -> int list -> int list = 
       (fun count -> List.map (fun y -> y * count))

   let listMul'3: int -> int list -> int list = 
       (fun count -> List.map (fun y -> (*) y count))
   
   let listMul'4: int -> int list -> int list = 
       (fun count -> List.map ((*) count))
   
   //просто listMul'5 почему-то некорректно работает в тестах
   let resultFunc () =
        let listMul'5: int -> int list -> int list =
            List.map << (*)
        listMul'5
