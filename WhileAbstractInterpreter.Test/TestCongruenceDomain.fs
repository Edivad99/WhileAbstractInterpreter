namespace WhileAbstractInterpreter.Test

open Program
open CongruenceDomain
open AbstractState
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestCongruenceDomain () =

    [<TestMethod>]
    member _.TestBasicIf () =
        let input = """
            x := 40;
            if x >= 0 then {
               x := 1;
            } else {
               x := 0;
            }
        """
        let program = evaluate input
        let interval_domain = CongruenceDomain()
        let abstract_state = AbstractState<_>(interval_domain)
        let result, program_points = abstract_state.eval program

        let resultExpected = Map.empty.Add("x", (Value(0, 1)))

        Assert.AreEqual(resultExpected, result)

    [<TestMethod>]
    member _.TestWhile () =
        let input = """
            x := 0;
            y := 2;

            while x < 40 do {
                x := x + 2;
                if x < 5 then {
                    y := y + 18;
                };
                if x > 8 then {
                    y := y - 30;
                }
            }
        """
        let program = evaluate input
        let interval_domain = CongruenceDomain()
        let abstract_state = AbstractState<_>(interval_domain)
        let result, program_points = abstract_state.eval program

        let pre_body =
            Map.empty
            |> Map.add "x" (Value(2, 0))
            |> Map.add "y" (Value(6, 2))
        let post_body =
            Map.empty
            |> Map.add "x" (Value(2, 0))
            |> Map.add "y" (Value(6, 2))
        let resultExpected =
            Map.empty
            |> Map.add "x" (Value(2, 0))
            |> Map.add "y" (Value(6, 2))

        Assert.AreEqual(pre_body, program_points.[3])
        Assert.AreEqual(post_body, program_points.[10])
        Assert.AreEqual(resultExpected, result)
