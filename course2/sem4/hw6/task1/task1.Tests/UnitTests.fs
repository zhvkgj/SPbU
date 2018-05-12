namespace Tests

open Hw6
open NUnit.Framework
open FsUnit

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

module Task2Test =

    open BinTree

    [<Test>]
    let ``First test add elements to binaryTree``() =
        let myTree = MyBinaryTree<int>()
        for i in -1..1 do myTree.Add(i)
        myTree.ToList() |> should equal [-1; 0; 1]

    [<Test>]
    let ``Second test add elements to binaryTree``() =
        let myTree = MyBinaryTree<int>()
        for i in -10..10 do myTree.Add(i)
        myTree.ToList() |> 
            should equal [-10; -9; -8; -7; -6; -5; -4; -3; -2; -1; 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
        myTree.Count |> should equal 21

    [<Test>]
    let ``First test remove elements from binary tree``() =
        let myTree = MyBinaryTree<int>()
        for i in -10..10 do myTree.Add(i)
        for i in myTree do
            if (i % 2 = 0) then myTree.Remove(i) |> ignore
        myTree.ToList() |> should equal [-9; -7; -5; -3; -1; 1; 3; 5; 7; 9]
        myTree.Count |> should equal 10

    [<Test>]
    let ``Second test remove elements from binary tree``() =
        let myTree = MyBinaryTree<int>()
        for i in -2..2 do myTree.Add(i)
        myTree.Count |> should equal 5
        myTree.Remove(2) |> should equal true
        myTree.Count |> should equal 4
        myTree.Remove(3) |> should equal false
        myTree.Count |> should equal 4
        myTree.ToList() |> should equal [-2; -1; 0; 1]
        myTree.Count |> should equal 4

    [<Test>]
    let ``FindWithParent test for getting element with parent``() =
        let myTree = MyBinaryTree<float>()
        myTree.Add(8.0)
        myTree.Add(4.0)
        myTree.Add(2.0)
        myTree.Add(3.0)
        myTree.Add(10.0)
        myTree.Add(6.0)
        myTree.Add(7.0)
        myTree.Count |> should equal 7
        myTree.ToList() |> should equal [2.0; 3.0; 4.0; 6.0; 7.0; 8.0; 10.0]
        let curr = fst <| myTree.FindWithParent(2.0) 
        curr.ToString() |> should equal "BinaryTreeNode (2.0,Null,BinaryTreeNode (3.0,Null,Null))" 
        myTree.Remove(5.0) |> should equal false
        myTree.Remove(4.0) |> should equal true
        myTree.ToList() |> should equal [2.0; 3.0; 6.0; 7.0; 8.0; 10.0]
