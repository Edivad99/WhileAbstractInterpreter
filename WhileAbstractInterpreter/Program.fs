module Program

open System
open System.IO
open System.Reflection
open FSharp.Text.Lexing
open IntervalDomain
open Domain
open AbstractDomain

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
        // Ciao
        x := 110;
        y := 0;
        z := 16;
        //while x > y do {
        //    x:= x - 10;
        //    y:= y + 1
        //}
    """

    let program = evaluate input 
    //let program = evaluate_file "test.wl"
    Console.WriteLine(program)

    let init_state = IntervalDomain.get_init_state program
    let (result, program_points) = eval (program, init_state, [init_state])

    Console.WriteLine($"Result: {result}")

    Report.generate_report input program_points

    //List.iter (fun env -> Console.WriteLine(env)) program_points

    //let a = (b :> Domain.Domain<Interval>).Less Bottom Bottom
    
    //let me = test :> Domain<_>

    //let a = IntervalDomain.IntervalDomain. Bottom Bottom
    //Console.WriteLine (IntervalDomain.eval program)
    //Console.ReadKey() |> ignore
    0
