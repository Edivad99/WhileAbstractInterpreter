module Program

open System
open System.Diagnostics
open System.IO
open System.Reflection
open FSharp.Text.Lexing
open IntervalDomain
open SignDomain
open CongruenceDomain
open AbstractState

let evaluate input =
    let lexbuf = LexBuffer<char>.FromString input
    Parser.prog Lexer.tokenize lexbuf

let read_file filename =
    Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
    |> fun x -> Path.Combine (x, "Examples", filename)
    |> File.ReadAllText


[<EntryPoint>]
let main args =
    let input = """
x := 0;
y := random;
if y = 0 then {
    x := x + 1;
    if x > 40 then {
        x := 0;
    }
}
    """
    let domain = IntervalDomain()
    let input = read_file "simple_loop.wl"
    //let input = read_file "interval.wl"
    //let input = read_file "random.wl"

    //let domain = SignDomain()
    //let input = read_file "simple_loop_sign.wl"

    //let domain = CongruenceDomain()
    //let input = read_file "congruence_loop.wl"

    //let domain = IntervalDomain()
    //let input = read_file "widening_delay.wl"

    let program = evaluate input
    //Console.WriteLine(program)

    let abstract_state = AbstractState<_>(domain) //delay:3

    let start = Stopwatch.GetTimestamp()
    let result, program_points = abstract_state.eval program
    let delta = Stopwatch.GetElapsedTime start

    //Console.WriteLine($"Result: {result}")
    Report.generate_report input program_points delta
    //Console.ReadKey() |> ignore
    0
