module Program

open System
open System.IO
open System.Reflection
open FSharp.Text.Lexing
open IntervalDomain
open Domain
open AbstractState

let evaluate input =
    let lexbuf = LexBuffer<char>.FromString input
    Parser.prog Lexer.tokenize lexbuf

let evaluate_file filename =
    let executableLocation = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
    let fileLocation = Path.Combine(executableLocation, "Examples", filename)

    use rd = new StreamReader(fileLocation)
    let lexbuf = LexBuffer<char>.FromTextReader rd
    Parser.prog Lexer.tokenize lexbuf


[<EntryPoint>]
let main args =
    let input = """
        x := 110;
        y := 0;
    """

    let program = evaluate input 
    //let program = evaluate_file "test.wl"
    Console.WriteLine(program)

    let interval_domain = IntervalDomain()
    let abstract_state = AbstractState<_>(interval_domain)
    let init_state = interval_domain.get_init_state program
    let (result, program_points) = abstract_state.eval (program, init_state, [init_state])

    Console.WriteLine($"Result: {result}")
    Report.generate_report input program_points
    //Console.ReadKey() |> ignore
    0
