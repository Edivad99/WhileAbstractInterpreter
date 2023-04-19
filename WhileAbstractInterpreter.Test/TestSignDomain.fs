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