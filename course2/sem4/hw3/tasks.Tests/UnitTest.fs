module Tests

open NUnit.Framework
open hw3
open Task1
open Task2
open Task3
open Task4

//3.1 testing
[<Test>]
let ``firstEvenSearch [] should equal 0`` () = 
    let expected = 0
    let actual = firstEvenSearch []
    Assert.AreEqual(expected, actual)

[<Test>]
let ``firstEvenSearch [-10; -5; -4; 0; 12; 15; 19; 100002] should equal 5`` () =
    let expected = 5
    let actual = firstEvenSearch [-10; -5; -4; 0; 12; 15; 19; 100002]
    Assert.AreEqual(expected, actual)

[<Test>]
let ``firstEvenSearch [1..1000000] should unequal 500000`` () =
    let expected = 500000
    let actual = firstEvenSearch [1..1000000]
    Assert.AreEqual(expected, actual)

[<Test>]
let ``secondEvenSearch [] should equal 0`` () = 
    let expected = 0
    let actual = secondEvenSearch []
    Assert.AreEqual(expected, actual)

[<Test>]
let ``secondEvenSearch [-10; -5; -4; 0; 12; 15; 19; 100002] should equal 5`` () =
    let expected = 5
    let actual = secondEvenSearch [-10; -5; -4; 0; 12; 15; 19; 100002]
    Assert.AreEqual(expected, actual)

[<Test>]
let ``secondEvenSearch [1..1000000] should unequal 500000`` () =
    let expected = 500000
    let actual = secondEvenSearch [1..1000000]
    Assert.AreEqual(expected, actual)

[<Test>]
let ``thirdEvenSearch [] should equal 0`` () = 
    let expected = 0
    let actual = thirdEvenSearch []
    Assert.AreEqual(expected, actual)

[<Test>]
let ``thirdEvenSearch [-10; -5; -4; 0; 12; 15; 19; 100002] should equal 5`` () =
    let expected = 5
    let actual = thirdEvenSearch [-10; -5; -4; 0; 12; 15; 19; 100002]
    Assert.AreEqual(expected, actual)

[<Test>]
let ``thirdEvenSearch [1..1000000] should unequal 500000`` () =
    let expected = 500000
    let actual = thirdEvenSearch [1..1000000]
    Assert.AreEqual(expected, actual)

//3.2 testing
[<Test>]
let ``increase by one each element of the tree`` () =
    let expected = Node(1, Node(2, Tip(3), Tip(4)), Tip(5))
    let actual = treeFuncTR (Node(0, Node(1, Tip(2), Tip(3)), Tip(4))) (fun x -> (x + 1))
    Assert.AreEqual(expected, actual)

[<Test>]
let ``squar each element of the tree`` () =
    let expected = Node(0, Node(1, Tip(4), Tip(9)), Tip(16))
    let actual = treeFuncTR (Node(0, Node(1, Tip(2), Tip(3)), Tip(4))) (fun x -> (x * x))
    Assert.AreEqual(expected, actual)

//3.3 testing
[<Test>]
let ``calculation ((10 + (2 * 5)) / 4)`` () =
    let expected = 5
    let actual = eval (Division(Addition(Number(10), Multiplication(Number(2), Number(5))), Number(4)))
    Assert.AreEqual(expected, actual)

//3.4 testing
[<Test>]
let ``let find the fifth prime number`` () =
    let expected = 11
    let actual = Seq.item 4 buildInfPrimeSeq
    Assert.AreEqual(expected, actual)

[<Test>]
let ``let find the ninth prime number`` () =
    let expected = 23
    let actual = Seq.item 8 buildInfPrimeSeq
    Assert.AreEqual(expected, actual)

[<Test>]
let ``let find the 46th prime number`` () =
    let expected = 199
    let actual = Seq.item 45 buildInfPrimeSeq
    Assert.AreEqual(expected, actual)