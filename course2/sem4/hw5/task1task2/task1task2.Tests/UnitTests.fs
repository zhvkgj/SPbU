module Tests

open NUnit.Framework
open FsUnit
open FsCheck.NUnit
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
[<Property>]
let ``Test for listMul'1`` num ls =
     listMul'1 num ls = listMul'0 num ls

[<Property>]
let ``Test for listMul'2`` num ls =
    listMul'2 num ls = listMul'0 num ls

[<Property>]
let ``Test for listMul'3`` num ls =
    listMul'3 num ls = listMul'0 num ls

[<Property>]
let ``Test for listMul'4`` num ls =
    listMul'4 num ls = listMul'0 num ls

[<Property>]
let ``Test for resultFunc`` num ls =
    resultFunc () num ls = listMul'0 num ls
