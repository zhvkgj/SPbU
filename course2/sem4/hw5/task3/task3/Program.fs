﻿namespace hw5

//1.exit
//2.add contact (name, call number)
//3.search call number by name
//4.search name by call number
//5.print all pow
//6.save data into file
//7.get data from file
module Task3 =

   let dataToStringArray ls =
        let rec toString ls acc =
            match ls with
            | head :: tail -> toString tail <| (fst head + " " + snd head) :: acc
            | [] -> acc

        toString ls []

   let stringsToData (lsStr: list<string>) = 
        let rec toData (lsStr: list<string>) (acc: list<string * string>) =
            match lsStr with
            | head :: tail -> let spl = head.Split(' ') 
                              toData tail <| (spl.[0], spl.[1]) :: acc
            | [] -> acc

        toData lsStr []

   let readFromFile fileName =
        if System.IO.File.Exists fileName then 
            let tempLs = List.ofArray <| System.IO.File.ReadAllLines fileName
            stringsToData tempLs
        else []

   let searchByName (name: string) (ls: list<string * string>) =
        let tempLs = List.filter ((=) name << fst) ls
        match tempLs with
        | [x] -> "Call number: " + snd x
        | _   -> "Contact doesn't exist!"
   
   let searchByCallNumber (num: string) (ls: list<string * string>) =
        let tempLs = List.filter ((=) num << snd) ls
        match tempLs with
        | [x] -> "Name: " + fst x
        | _   -> "Contact doesn't exist!"       
   
   let phoneBook () =
        printfn "1. Exit"
        printfn "2. Add contact (name, call number)"
        printfn "3. Search call number by name"
        printfn "4. Search name by call number"
        printfn "5. Print all data"
        printfn "6. Save data into file"
        printfn "7. Read data from file"

        let rec phoneBookHandler acc =  
                printfn "Enter a command: "
                let funcNum = System.Console.ReadLine()

                match funcNum with
                | "1" -> printfn "Exit..."

                | "2" -> printfn "Enter the name: "
                         let name = System.Console.ReadLine()
                         printfn "Enter the call number: "
                         let callNumber = System.Console.ReadLine()
                         
                         phoneBookHandler <| (name, callNumber)::acc

                | "3" -> printfn "Enter the name: "
                         let name = System.Console.ReadLine()
                         printfn "%s" <| searchByName name acc

                         phoneBookHandler acc
                         
                | "4" -> printfn "Enter the call number: "
                         let callNumber = System.Console.ReadLine()
                         printfn "%s" <| searchByCallNumber callNumber acc

                         phoneBookHandler acc

                | "5" -> List.map (printfn "%s") <| dataToStringArray acc |> ignore
                         
                         phoneBookHandler acc

                | "6" -> printfn "Enter the absolute name of file, for example D:\Admin\Desktop\test.txt"
                         let fileName = System.Console.ReadLine()
                         System.IO.File.WriteAllLines(fileName, dataToStringArray acc)
                         
                         phoneBookHandler acc
               

                | "7" -> printfn "Enter the name of file: "
                         let fileName = System.Console.ReadLine()
                         
                         phoneBookHandler <| readFromFile fileName
                          
                | _ -> printfn "Unknown command!"
                       
                       phoneBookHandler acc
        
        phoneBookHandler []
   phoneBook()