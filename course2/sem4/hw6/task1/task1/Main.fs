namespace Hw6

module Main =
    open Hw6
    open ComputersNetwork
    open BinTree

    [<EntryPoint>]
    let main args =
        printfn "%A" args
        let myTree = new MyBinaryTree<int>()
        for i in -10..10 do myTree.Add(i)
        printfn "Count of elements in my tree: %d" myTree.Count
        printfn "My tree: %A" <| myTree.ToList()
        printfn "3 in my tree? - %A" <| myTree.Contains(-3)
        printfn "5 was removed? - %A" <| myTree.Remove(5)
        printfn "My tree: %A" <| myTree.ToList()
        printfn "-11 was removed? - %A" <| myTree.Remove(-11)
        printfn "My tree: %A" <| myTree.ToList()
        for node in myTree do
            printf "%A" [node]
        printfn "Count of elements in my tree: %d" myTree.Count
        for node in myTree do
            if (node % 2 = 0) then myTree.Remove(node) |> ignore
            printfn "Count: %d" <| myTree.Count
        printfn "My tree: %A" <| myTree.ToList()
        let curr = fst <| myTree.FindWithParent(7)
        printfn "Current with value 7: %s" <| curr.ToString()

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
