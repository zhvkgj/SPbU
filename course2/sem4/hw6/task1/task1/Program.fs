namespace hw6

module Task1 =
    open System

    [<Measure>] type percent

    let random = new System.Random()

    let (|Linux|_|) osName =
        match osName with
        | "linux" -> Some 35<percent>
        | _ -> None
    let (|Windows|_|) osName = 
        match osName with 
        | "windows" -> Some 70<percent>
        | _ -> None
    let (|MacOS|_|) osName =
        match osName with
        | "macOS" -> Some 25<percent>
        | _ -> None

    type Computer(os: string, isInfected: bool, randNum: Random) =
        let mutable infected = isInfected
        let mutable possibilityOfInfection = 0<percent>
        do match os with
           | Windows x | Linux x | MacOS x -> 
                possibilityOfInfection <- x
           | _ -> failwith "Unknown OS!"  

        member this.Infected 
            with get() = infected
        member this.TryToInfect = 
            let rNum = randNum.Next(100)
            infected <- (possibilityOfInfection >= rNum * 1<percent>)
             
    type Network(computers: list<Computer>, matrix: list<list<bool>>) =
        let mutable computers = computers
        let mutable infectedComp = List.empty
        let updateInfComp index =
            if (computers.[index].Infected) then 
                infectedComp <- index :: infectedComp           
        do for i in 0..computers.Length - 1 do
            updateInfComp i   
        
        member this.MakeStep = 
            for i in infectedComp do
                for j in 0..matrix.[i].Length - 1 do
                    if (matrix.[i].[j] && not(List.contains j infectedComp))
                        then computers.[j].TryToInfect
                             updateInfComp j
        
        member this.GetState =
            for i in infectedComp do
                printfn "Computer with a number %i is infected..." i

        member this.CheckWork =
            this.MakeStep
            printfn "Current condition: "
            this.GetState


    let computers = 
        [Computer("linux", false, random); 
         Computer("windows", true, random); 
         Computer("macOS", false, random)]
    
    let matrix = 
        [[false; true; true];
         [true; false; true;]
         [true; true; false]]

    let network = new Network(computers, matrix)
    for i in 0..3 do
        network.CheckWork

//module Task2 = 
    


(*open System.Collections.Generic

type SparseVector(items : seq<int * float>) =
    let elems = new SortedDictionary<_, _>()
    do items |> Seq.iter(fun(k, v) -> elems.Add(k, v))

    member t.Item
        with get(idx) =
            if elems.ContainsKey(idx) then elems.[idx]
            else 0.0

let v = SparseVector [(3, 547.0)]
printfn "%f" v.[3]*)


 
    
