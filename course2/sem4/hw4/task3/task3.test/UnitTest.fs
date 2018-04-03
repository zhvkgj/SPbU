module Tests

open NUnit.Framework
open hw4
open Task3

//4.3 testing
[<Test>]
let ``First simple search for free variable`` () =
  let expected = ['x']
  let actual = getFV(Var('x'))
  Assert.AreEqual(expected, actual)

[<Test>]
let ``Second simple search for free variable`` () =
  let expected = ['y']
  let actual = getFV(Var('x'))
  Assert.AreNotEqual(expected, actual)

[<Test>]
let ``Third simple search for free variable`` () = 
    let (expected: char list) = []
    let actual = getFV (LambdaAbstr (Var ('x'), 'x'))
    Assert.AreEqual(expected, actual)

[<Test>]
let ``Advanced search test`` () = 
    let expected = ['z']
    let actual = getFV (App (LambdaAbstr (LambdaAbstr (Var ('x'), 'y'), 'x'), LambdaAbstr (App (Var ('z'), Var ('x')), 'x')))
    Assert.AreEqual(expected, actual)

[<Test>]
let ``First test for substitution`` () = 
    let expected = (Var ('y'))
    let actual = subst (Var ('y')) (Var ('z')) ('x')
    Assert.AreEqual(expected, actual)

[<Test>]
let ``Second test for substitution `` () = 
    let S = LambdaAbstr (Var ('x'), 'y')
    let T = LambdaAbstr (Var ('y'), 's')
    let actual = subst S T 'x'
    let expected = LambdaAbstr (LambdaAbstr (Var ('y'), 's'), 'a')
    Assert.AreEqual(expected, actual)

[<Test>]
let ``Test for term "(λx y.x) y u" which should return 'y'`` () =
  let expected = Var('y')
  let actual = eval(App(LambdaAbstr(App(LambdaAbstr(Var('x'), 'y'), Var('u')), 'x'), Var('y')))
  Assert.AreEqual(expected, actual)

[<Test>]
let ``(Id)(Id) should return Id`` () =
  let expected = LambdaAbstr(Var('x'), 'x')
  let actual = eval(App(LambdaAbstr(Var('x'), 'x'), LambdaAbstr(Var('x'), 'x')))
  Assert.AreEqual(expected, actual)

[<Test>]
let ``Beta-reduction with SKK should return I`` () =
  let expected = LambdaAbstr(Var('z'), 'z')
  let actual = eval(App(LambdaAbstr(App(LambdaAbstr(LambdaAbstr(App(App(Var('x'), Var('z')), App(Var('y'), Var('z'))), 'z'), 'y'), LambdaAbstr(LambdaAbstr(Var('x'), 'y'), 'x')), 'x'), LambdaAbstr(LambdaAbstr(Var('x'), 'y'), 'x')))
  Assert.AreEqual(expected, actual)
