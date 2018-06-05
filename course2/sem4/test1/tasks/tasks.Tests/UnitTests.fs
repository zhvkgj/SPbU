module Tests

open NUnit.Framework
open FsUnit
open Tasks
open Task1
open Task3
open System

//task1 tests
[<Test>]
let ``Simple test1`` () =
    let expected = Math.PI / 6.0
    let actual = avgSum [Math.PI / 6.0; Math.PI / 6.0; Math.PI / 6.0]
    Assert.AreEqual(expected, actual)

[<Test>]
let ``Simple test2`` () =
    let expected = Math.PI / 2.0
    let actual = avgSum [Math.PI / 2.0; Math.PI / 2.0; Math.PI / 2.0]
    Assert.AreEqual(expected, actual)

[<Test>]
let ``Simple test3`` () =
    let expected = Math.PI / 3.0
    let actual = avgSum [Math.PI / 3.0; Math.PI / 3.0; Math.PI / 3.0]
    Assert.AreEqual(expected, actual)

//task2 tests
//Check F# Interactive)

//task3 tests
[<Test>]
let ``First test``() =
    let stack = new MyStack<int>()
    for i in 0..10
        do stack.Push(i)
    let expected = 10
    let actual = stack.Pop()
    Assert.AreEqual(expected, actual)

[<Test>]
let ``Second test``() =
    let stack = new MyStack<int>()
    stack.IsEmpty() |> should equal true

[<Test>]
let ``Third test``() =
    let stack = new MyStack<int>()
    (fun () -> stack.Pop() |> ignore) |> should throw typeof<System.Exception>