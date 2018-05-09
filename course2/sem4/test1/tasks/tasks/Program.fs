namespace Tasks

//task1
module Task1 =
    let avgSum ls =
        let rec sumSin (ls: float list) (sum: float) (count: float) =
            match ls with
            | h::t -> sumSin t (sum + sin h) (count + 1.0)
                  
            | [] -> if (count = 0.0) then 0.0
                    else sum / count
    
        sumSin ls 0.0 0.0

//task2
module Task2 =
    let buildSquare n =
        let rec buildingSq n acc =
            let edge = String.replicate n "*" + "\n"
            let side = "*" + String.replicate (n - 2) " " + "*\n"
            if (n < 1) then failwith "Wrong input!"
            else
                match acc with
                | 1 -> printf "%s" edge
                | _ when acc = n -> printf "%s" edge
                                    buildingSq n (acc - 1)
                | _ -> printf "%s" side
                       buildingSq n (acc - 1)
        buildingSq n n  
    buildSquare(4)

//task3
module Task3 =
    open System
    open System.Collections.Generic
    
    let maxSize = 255

    type Item(key: string, value: string) =
        let mutable key = key
        let mutable value = value
        member i.Key
            with get() = key
            and set k = key <- k
        member i.Value
            with get() = value
            and set v = value <- v
        
    type HashTable(items: seq<int * List<Item>>) =
        let elems = new Dictionary<_, _>(maxSize)
        do items |> Seq.iter(fun (k, v) -> elems.Add (k, v))
        let getHash (value: string) =
            let lengthStr = value.Length
            match lengthStr with
            | 0 -> failwith "Value is empty!"
            | _ when lengthStr > maxSize -> failwith "Value length > max value!"
            | _ -> lengthStr

        member h.Insert key value =
            let item = new Item(key, value)
            let hash = getHash(item.Key)
            let mutable hashTableItem = new List<Item>()
            if (elems.ContainsKey(hash)) then 
                hashTableItem <- elems.[hash]
                elems.[hash].Add(item)
            else
                hashTableItem.Add(item)
                elems.Add(hash, hashTableItem)
        member h.Delete key =
            let hash = getHash(key)
            if (elems.ContainsKey(hash)) then
                let hashTableItem = elems.[hash]
                let item = hashTableItem.Find (fun i -> i.Key = key)
                hashTableItem.Remove(item)
            else failwith "Value not found!"
        member h.Search key =
            let hash = getHash(key)
            if (elems.ContainsKey(hash)) then 
                let hashTableItem = elems.[hash]
                let item = hashTableItem.Find (fun i -> i.Key = key)
                item
            else failwith "Element not found!"
                    