namespace Tasks

module Task1 =
    open System

    type OS = 
        { Name: string;
          PossibilityOfInfection: float }

    type Computer(oSystem: OS, isInfected: bool, randNum: Random) =
        let mutable infected = isInfected 
        member this.Infected 
            with get() = infected
        member this.tryToInfect = 
            infected <- (oSystem.PossibilityOfInfection >= randNum.NextDouble())

    type Network(computers: list<Computer>, matrix: list<list<bool>>) =
        let mutable computers = computers
        let mutable infectedComp = []
        member this.updateInfComp = 
            
        member this.makeStep =
            
        


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


 
    
