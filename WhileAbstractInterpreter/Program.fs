module Program

open System
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
        x := -1;
        if 0 > x then {
            x := 10
        } else {
            x := -10
        }
    """
    //let input = read_file "if.wl"

    let program = evaluate input
    Console.WriteLine(program)

    let interval_domain = IntervalDomain()
    let sign_domain = SignDomain()

    let abstract_state = AbstractState<_>(sign_domain)
    let result, program_points = abstract_state.eval program

    Console.WriteLine($"Result: {result}")
    Report.generate_report input program_points
    //Console.ReadKey() |> ignore
    0
