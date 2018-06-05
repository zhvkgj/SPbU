namespace Hw6

module ComputersNetwork =

    /// <summary>
    /// Units of measure for probability of infection
    /// </summary>
    [<Measure>] type percent

    /// <summary>
    /// Partial-case active patterns for matching possibility of infection
    /// </summary>
    /// <param name="osName">OS name</param>
    let (|Linux|_|) osName =
        match osName with
        | "linux" -> Some 35<percent>
        | _ -> None

    /// <summary>
    /// Partial-case active patterns for matching possibility of infection
    /// </summary>
    /// <param name="osName">OS name</param>
    let (|Windows|_|) osName = 
        match osName with 
        | "windows" -> Some 70<percent>
        | _ -> None

    /// <summary>
    /// Partial-case active patterns for matching possibility of infection
    /// </summary>
    /// <param name="osName">OS name</param>
    let (|MacOS|_|) osName =
        match osName with
        | "macOS" -> Some 25<percent>
        | _ -> None

    /// <summary>
    /// Abstract class for custom class of random numbers
    /// </summary>
    [<AbstractClass>]
    type IRandomNum() =
        abstract member Next : unit -> int

    /// <summary>
    /// Class with custom method of making random numbers
    /// </summary>
    type CustomRandNum(n) =
        inherit IRandomNum()
        override this.Next() =
            System.Random().Next(n)
    
    /// <summary>
    /// Abstraction of computer
    /// </summary>
    /// <param name="os">OS name on computer</param>
    /// <param name="isInfected">Initial data of infection</param>
    /// <param name="randNum">Obj for randon number</param>
    type Computer(os: string, isInfected: bool, randNum: CustomRandNum) =

        /// <summary>
        /// Condition of infection
        /// </summary>
        let mutable infected = isInfected

        /// <summary>
        /// Probability of infection in percent
        /// </summary>
        let mutable possibilityOfInfection = 0<percent>

        // Set a possibility of infection by OS name
        do match os with
           | Windows x | Linux x | MacOS x -> 
                possibilityOfInfection <- x
           | _ -> failwith "Unknown OS!"

        /// <summary>
        /// Get condition of infection
        /// </summary>
        member this.Infected 
            with get() = infected

        /// <summary>
        /// Get possibility of infection
        /// </summary>
        member this.PossibilityOfInfection
            with get() = possibilityOfInfection

        /// <summary>
        /// Try to infect current computer
        /// </summary>
        member this.TryToInfect() = 
            let rNum = randNum.Next()
            infected <- (possibilityOfInfection >= rNum * 1<percent>)

    /// <summary>
    /// Abstraction of computers network
    /// </summary>
    /// <param name="computers">List of computers on the network</param>
    /// <param name="matrix">Adjacency matrix of computers communication</param>       
    type Network(computers: list<Computer>, matrix: list<list<bool>>) =

        /// <summary>
        /// List of computers on the current network
        /// </summary>
        let mutable computers = computers

        /// <summary>
        /// List of infected computers
        /// </summary>
        let mutable infectedComp = List.empty

        /// <summary>
        /// Checking current computer for infection
        /// and add to infected computers if it is infected
        /// </summary>
        /// <param name="index">Index of the computer being scanned</param>
        let updateInfComp index =
            if (computers.[index].Infected) then 
                infectedComp <- index :: infectedComp

        // Init default infected computer
        do for i in 0..computers.Length - 1 do
            updateInfComp i   
        
        /// <summary>
        /// Make a step on the network
        /// </summary>
        member this.MakeStep() =
            // run through the infected computers
            for i in infectedComp do
                for j in 0..matrix.[i].Length - 1 do
                    // find and try to infect adjacents computers
                    // which are not infected yet
                    if (matrix.[i].[j] && not(List.contains j infectedComp))
                        then computers.[j].TryToInfect()
            for i in 0..computers.Length - 1 do       
                updateInfComp i

        /// <summary>
        /// Print a state of the network
        /// </summary>
        member this.PrintState() =
            for i in infectedComp do
                printfn "Computer with a number %i is infected..." i

        /// <summary>
        /// Get a state of the network
        /// </summary>
        member this.GetState() =
            let mutable infoStr = "Infected computers: "
            for i in 0..computers.Length - 1 do
                if (computers.[i].Infected) then 
                    infoStr <- infoStr + i.ToString() + " "
            infoStr

        /// <summary>
        /// Make a step and print a state of the network
        /// </summary>
        member this.StepWithPrint() =
            this.MakeStep()
            printfn "Current condition: "
            this.PrintState()

        /// <summary>
        /// Make a step and get a state of the network
        /// </summary>
        member this.StepWithGet() =
            this.MakeStep()
            this.GetState()
