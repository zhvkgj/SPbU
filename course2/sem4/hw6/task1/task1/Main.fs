namespace Hw6

module Main =
    open Hw6
    open ComputersNetwork
    //open BinTree

    [<EntryPoint>]
    let main args =
        printfn "%A" args
        let random = CustomRandNum(100)
        let computers = 
            [Computer("linux", false, random); 
             Computer("windows", true, random); 
             Computer("macOS", false, random)]
    
        let matrix = 
            [[false; true; true];
             [true; false; true];
             [true; true; false]]

        let network = Network(computers, matrix)
        network.PrintState()
        for _ in 0..3 do
            printfn "%s" <| network.StepWithGet()
        0 //return an integer exit code
