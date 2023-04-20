namespace WhileAbstractInterpreter.Test

open Program
open SignDomain
open AbstractState
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestSignDomain () =

    [<TestMethod>]
    member _.TestBasicIf () =
        let input = """
            x := 40;
            if x >= 40 then {
               x := 1;
            } else {
               x := 0;
            }
        """
        let program = evaluate input
        let sign_domain = SignDomain()
        let abstract_state = AbstractState<_>(sign_domain)
        let result, program_points = abstract_state.eval program

        let resultExpected = Map.empty.Add("x", Positive)

        Assert.AreEqual(resultExpected, result)
        
    [<TestMethod>]
    member _.TestIfRandom () =
        let input = """
            x := random;
            if x >= 40 then {
               skip;
            } else {
               skip;
            }
        """
        let program = evaluate input
        let sign_domain = SignDomain()
        let abstract_state = AbstractState<_>(sign_domain)
        let result, program_points = abstract_state.eval program

        let true_branch = Map.empty.Add ("x", Positive)
        let false_branch = Map.empty.Add ("x", Top)

        Assert.AreEqual(true_branch, program_points.[2])
        Assert.AreEqual(false_branch, program_points.[4])


    [<TestMethod>]
    member _.TestIfRange () =
        let input = """
            x := [-40; 100];
            if x >= 40 then {
               skip;
            } else {
               skip;
            }
        """
        let program = evaluate input
        let sign_domain = SignDomain()
        let abstract_state = AbstractState<_>(sign_domain)
        let result, program_points = abstract_state.eval program

        let true_branch = Map.empty.Add ("x", Positive)
        let false_branch = Map.empty.Add ("x", Top)

        Assert.AreEqual(true_branch, program_points.[2])
        Assert.AreEqual(false_branch, program_points.[4])

    [<TestMethod>]
    member _.TestIfAnd () =
        let input = """
            x := [-40; 100];
            if x >= 40 && x < 80 then {
               skip;
            } else {
               skip;
            }
        """
        let program = evaluate input
        let interval_domain = SignDomain()
        let abstract_state = AbstractState<_>(interval_domain)
        let result, program_points = abstract_state.eval program

        let true_branch = Map.empty.Add ("x", Positive)
        let false_branch = Map.empty.Add ("x", Top)

        Assert.AreEqual(true_branch, program_points.[2])
        Assert.AreEqual(false_branch, program_points.[4])

    [<TestMethod>]
    member _.TestWhileIncrement () =
        let input = """
            x := 0;
            while x < 40 do {
                x := x + 1
            }
        """
        let program = evaluate input
        let interval_domain = SignDomain()
        let abstract_state = AbstractState<_>(interval_domain)
        let result, program_points = abstract_state.eval program

        let pre_body = Map.empty.Add("x", Positive)
        let post_body = Map.empty.Add("x", Positive)
        let resultExpected = Map.empty.Add("x", Positive)

        Assert.AreEqual(pre_body, program_points.[2])
        Assert.AreEqual(post_body, program_points.[3])
        Assert.AreEqual(resultExpected, result)

    [<TestMethod>]
    member _.TestWhileDecrement () =
        let input = """
            x := 40;
            while  x != 0 do {
                x := x - 1
            }
        """
        let program = evaluate input
        let interval_domain = SignDomain()
        let abstract_state = AbstractState<_>(interval_domain)
        let result, program_points = abstract_state.eval program

        let pre_body = Map.empty.Add("x", Top)
        let post_body = Map.empty.Add("x", Top)
        let resultExpected = Map.empty.Add("x", Top)

        Assert.AreEqual(pre_body, program_points.[2])
        Assert.AreEqual(post_body, program_points.[3])
        Assert.AreEqual(resultExpected, result)

    [<TestMethod>]
    member _.TestWhileAnd () =
        let input = """
            x := 0;
            y := 0;
            while x < 10 && y < 5 do {
                x := x + 1;
                y := y + 1;
            }
        """
        let program = evaluate input
        let interval_domain = SignDomain()
        let abstract_state = AbstractState<_>(interval_domain)
        let result, program_points = abstract_state.eval program

        let pre_body =
            Map.empty
            |> Map.add "x" Positive
            |> Map.add "y" Positive
        let post_body =
            Map.empty
            |> Map.add "x" Positive
            |> Map.add "y" Positive
        let resultExpected =
            Map.empty
            |> Map.add "x" Positive
            |> Map.add "y" Positive

        Assert.AreEqual(pre_body, program_points.[3])
        Assert.AreEqual(post_body, program_points.[5])
        Assert.AreEqual(resultExpected, result)

    [<TestMethod>]
    member _.TestFactorial () =
        let input = """
            n := 5;
            tmp := n - 1;
            factorial := n;
            while tmp > 1 do {
                factorial := factorial * tmp;
                tmp := tmp - 1;
            }
        """
        let program = evaluate input
        let interval_domain = SignDomain()
        let abstract_state = AbstractState<_>(interval_domain)
        let result, program_points = abstract_state.eval program

        let pre_body =
            Map.empty
            |> Map.add "factorial" Positive
            |> Map.add "n" Positive
            |> Map.add "tmp" Positive
        let post_body =
            Map.empty
            |> Map.add "factorial" Positive
            |> Map.add "n" Positive
            |> Map.add "tmp" Top
        let resultExpected =
            Map.empty
            |> Map.add "factorial" Positive
            |> Map.add "n" Positive
            |> Map.add "tmp" Top

        Assert.AreEqual(pre_body, program_points.[4])
        Assert.AreEqual(post_body, program_points.[6])
        Assert.AreEqual(resultExpected, result)

    [<TestMethod>]
    member _.TestFibonacci () =
        let input = """
            n := 5;
            pred1 := 1;
            pred2 := 1;
            i := 2;
            fib := 1;
            if n > 2 then {
                while i < n do {
                   fib := pred1 + pred2;
                   pred2 := pred1;
                   pred1 := fib;
                   i := i + 1;
                }
            } else {
               skip;
            }
        """
        let program = evaluate input
        let interval_domain = SignDomain()
        let abstract_state = AbstractState<_>(interval_domain)
        let result, program_points = abstract_state.eval program

        let pre_while_body =
            Map.empty
            |> Map.add "fib" Positive
            |> Map.add "i" Positive
            |> Map.add "n" Positive
            |> Map.add "pred1" Positive
            |> Map.add "pred2" Positive
        let post_while_body =
            Map.empty
            |> Map.add "fib" Positive
            |> Map.add "i" Positive
            |> Map.add "n" Positive
            |> Map.add "pred1" Positive
            |> Map.add "pred2" Positive
        let resultExpected =
            Map.empty
            |> Map.add "fib" Positive
            |> Map.add "i" Positive
            |> Map.add "n" Positive
            |> Map.add "pred1" Positive
            |> Map.add "pred2" Positive
        Assert.AreEqual(pre_while_body, program_points.[7])
        Assert.AreEqual(post_while_body, program_points.[11])
        Assert.AreEqual(resultExpected, result)
