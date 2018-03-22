module Tests

open NUnit.Framework
open FsUnit
open hw5
open Task1
open Task2

//task1 testing
[<Test>]
let ``Simple testing test1`` () =
    checkBrackets "()" |> should equal true

[<Test>]
let ``Simple testing test2`` () =
    checkBrackets "" |> should equal true

[<Test>]
let ``Simple testing test3`` () =
    checkBrackets "(({}))" |> should equal true

[<Test>]
let ``First testing previous homework test3`` () =
    checkBrackets "searchFV (App (LambdaAbstr ('x', LambdaAbstr ('y', Var ('x'))), LambdaAbstr ('x', App (Var ('z'), Var ('x')))))" |> should equal true

[<Test>]
let ``Second testing previous homework test3`` () =
    checkBrackets "betaRdct(App(LambdaAbstr('x', App(LambdaAbstr('y', Var('x')), Var('u'))), Var('y')))" |> should equal true

[<Test>]
let ``Advanced testing test 4`` () =
    checkBrackets "((((Andsjfdnsj[{}]{fskfmmflkd}ksmFmMFKFKF(mkkKKKM[]f))))))" |> should equal false

//task2 testing
[<Test>]
let ``Testing listMulUltraPointFreeStyle0`` () =
    listMulUltraPointFreeStyle0 10 [1; 2; 3] |> should equal [10; 20; 30]

[<Test>]
let ``Testing listMulUltraPointFreeStyle1`` () =
    listMulUltraPointFreeStyle1 10 [1; 2; 3] |> should equal [10; 20; 30]

[<Test>]
let ``Testing listMulUltraPointFreeStyle2`` () =
    listMulUltraPointFreeStyle2 10 [1; 2; 3] |> should equal [10; 20; 30]

[<Test>]
let ``Testing listMulUltraPointFreeStyle3`` () =
    listMulUltraPointFreeStyle3 10 [1; 2; 3] |> should equal [10; 20; 30]

[<Test>]
let ``Testing listMulUltraPointFreeStyle4`` () =
    listMulUltraPointFreeStyle4 10 [1; 2; 3] |> should equal [10; 20; 30]

[<Test>]
let ``Testing listMulUltraPointFreeStyle5`` () =
    listMulUltraPointFreeStyle5 5 [1; 2; 3] |> should equal [10; 20; 30]
