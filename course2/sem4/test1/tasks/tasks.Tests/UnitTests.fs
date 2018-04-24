module Tests

open NUnit.Framework
open Tasks
open Task1
open Task2
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
Check F# Interactive)

//task3 tests