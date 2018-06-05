module Tests

open NUnit.Framework
open hw7
open Task1
open Task2

//task1 testing
[<Test>]
let ``Test1 for task1`` () =
    let expected = 0.048
    let actual =
        rounding 3 {
            let! a = 2.0 / 12.0
            let! b = 3.5
            return a / b
        }
    Assert.AreEqual(actual, expected)

[<Test>]
let ``Test2 for task1`` () =
    let expected = 490.83579
    let actual =
        rounding 5 {
            let! a = 12.16568 * 22.005 + 223.13
            return a
        }
    Assert.AreEqual(actual, expected)

[<Test>]
let ``Test3 for task1`` () =
    let expected = 7.26
    let actual = 
        rounding 2 {
            let! a = 191.0
            let! b = 26.3
            let! z = a / b
            return z
        }
    Assert.AreEqual(actual, expected)
//task2 testing
[<Test>]
let ``Test1 for task2`` () =
    let expected = Some 3
    let actual = 
        calculate () {
            let! x = "1"
            let! y = "2"
            let z = x + y
            return z
        }
    Assert.AreEqual(actual, expected)

[<Test>]
let ``Test2 for task2`` () =
    let expected = None
    let actual = 
        calculate () {
            let! x = "1"
            let! y = "Ъ"
            let z = x + y
            return z
        }
    Assert.AreEqual(actual, expected)

[<Test>]
let ``Test3 for task2`` () =
    let expected = Some(-6419754)
    let actual = 
        calculate () {
            let! x = "1234567"
            let! y = "7654321"
            let! z = "0001"
            let n = (x - y) * z
            return n
        }
    Assert.AreEqual(actual, expected)
