namespace hw5

module Task1 =
  let checkBrackets (str: string) =
    let myList = Seq.toList str
    let rec checking curList acc =
        match curList with
        | head::tail -> match head with
                        | _ when head = '(' || head = '{' || head = '[' -> checking tail (head::acc)
                        | _ when head = ')' -> if (not(List.isEmpty acc) && List.head acc = '(') then checking tail (List.tail acc)
                                               else false
                        | _ when head = '}' -> if (not(List.isEmpty acc) && List.head acc = '{') then checking tail (List.tail acc)
                                               else false
                        | _ when head = ']' -> if (not(List.isEmpty acc) && List.head acc = '[') then checking tail (List.tail acc)
                                               else false
                        | _ -> checking tail acc
        | [] -> if (List.isEmpty acc) 
                    then true
                else false
    checking myList []

// func x l = List.map (fun y -> y * x) l
module Task2 =
   
   let listMulUltraPointFreeStyle0 count ls = 
        List.map (fun y -> y * count) ls

   let listMulUltraPointFreeStyle1 count : int list -> int list = 
        List.map (fun y -> y * count)
   
   let listMulUltraPointFreeStyle2 : int -> int list -> int list = 
       (fun count -> List.map (fun y -> y * count))

   let listMulUltraPointFreeStyle3 : int -> int list -> int list = 
       (fun count -> List.map (fun y -> (*) y count))
   
   let listMulUltraPointFreeStyle4 : int -> int list -> int list = 
       (fun count -> List.map ((*) count))

   let listMulUltraPointFreeStyle5 : int -> int list -> int list =
       List.map << (*)