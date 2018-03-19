module Tests

open NUnit.Framework
open hw4
open Task3

//4.3 testing
[<Test>]
let ``First simple search for free variable`` () =
  let expected = ['x']
  let actual = searchFV(Var('x'))
  Assert.AreEqual(expected, actual)

[<Test>]
let ``Second simple search for free variable`` () =
  let expected = ['y']
  let actual = searchFV(Var('x'))
  Assert.AreNotEqual(expected, actual)

[<Test>]
let ``Third simple search for free variable`` () = 
    let expected = []
    let actual = searchFV (LambdaAbstr ('x', Var ('x')))
    Assert.AreEqual(expected, actual)

[<Test>]
let ``Advanced search test`` () = 
    let expected = ['z']
    let actual = searchFV (App (LambdaAbstr ('x', LambdaAbstr ('y', Var ('x'))), LambdaAbstr ('x', App (Var ('z'), Var ('x')))))
    Assert.AreEqual(expected, actual)

[<Test>]
let ``First test for substitution`` () = 
    let expected = (Var ('y'))
    let actual = subst (Var ('y')) ('x') (Var ('z'))
    Assert.AreEqual(expected, actual)

[<Test>]
let ``Second test for substitution `` () = 
    let S = LambdaAbstr ('y', Var ('x'))
    let T = LambdaAbstr ('s', Var ('y'))
    let actual = subst S 'x' T
    let expected = LambdaAbstr ('a', LambdaAbstr ('s', Var ('y')))
    Assert.AreEqual(expected, actual)

[<Test>]
let ``Test for term "(λx y.x) y u" which should return 'y'`` () =
  let expected = Var('y')
  let actual = betaRdct(App(LambdaAbstr('x', App(LambdaAbstr('y', Var('x')), Var('u'))), Var('y')))
  Assert.AreEqual(expected, actual)

[<Test>]
let ``(Id)(Id) should return Id`` () =
  let expected = LambdaAbstr('x', Var('x'))
  let actual = betaRdct(App(LambdaAbstr('x', Var('x')), LambdaAbstr('x', Var('x'))))
  Assert.AreEqual(expected, actual)

[<Test>]
let ``Beta-reduction with SKK should return I`` () =
  let expected = LambdaAbstr('b', Var('b'))
  let actual = betaRdct(betaRdct(App(LambdaAbstr('x', App(LambdaAbstr('y', LambdaAbstr('z', App(App(Var('x'), Var('z')), App(Var('y'), Var('z'))))), LambdaAbstr('x', LambdaAbstr('y', Var('x'))))), LambdaAbstr('x', LambdaAbstr('y', Var('x'))))))
  Assert.AreEqual(expected, actual)