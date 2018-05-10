namespace Tests

open Hw6
open NUnit.Framework
open FsUnit
//open BinTree

module Task1Test =

    open ComputersNetwork
    
    let mutable net = Network([], [])

    let mutable matrix = []

    let mutable computers = []

    [<Test>]
    let ``Test factory is OK``() =
        let random = CustomRandNum(100)
        let comp = Computer("linux", false, random)
        comp.Infected |> should equal false
        comp.PossibilityOfInfection |> should equal 35<percent> 

    [<SetUp>]
    let ``Init tests before execution``() =
        let random = CustomRandNum(2)

        matrix <- [[false; true; true];
                   [true; false; true];
                   [true; true; false]]

        computers <- [Computer("linux", false, random); 
                      Computer("windows", true, random); 
                      Computer("macOS", false, random)]

        net <- Network(computers, matrix)

    [<Test>]
    let ``First test making step and getting condition of network``() =
        net.MakeStep() 
        net.GetState() |> should equal "Infected computers: 0 1 2 "

    [<Test>]
    let ``Second test making step and getting condition of network ``() =
        net.StepWithGet() |> should equal "Infected computers: 0 1 2 "
