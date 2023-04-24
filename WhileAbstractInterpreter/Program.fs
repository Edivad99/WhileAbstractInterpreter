module Program

open System
open System.Diagnostics
open System.IO
open System.Reflection
open FSharp.Text.Lexing
open IntervalDomain
open SignDomain
open Domain
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
x := [-40; 40];
if x >= -10 && x < 10 then {
    skip;
} else {
    skip;
}
    """
    //let input = read_file "if.wl"

    let program = evaluate input
    Console.WriteLine(program)

    //let domain = IntervalDomain()
    let domain = SignDomain()

    let abstract_state = AbstractState<_>(domain)

    let start = Stopwatch.GetTimestamp()
    let result, program_points = abstract_state.eval program
    let delta = Stopwatch.GetElapsedTime start

    //Console.WriteLine($"Result: {result}")
    Report.generate_report input program_points delta
    //Console.ReadKey() |> ignore
    0
