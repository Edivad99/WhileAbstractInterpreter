module Program

open System
open System.IO
open System.Reflection
open FSharp.Text.Lexing
open IntervalDomain

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
        // Example from page 109 of the slides.
        x := -1;
        while x != 0 do {
          x := x + 1
        };
        y := 2
    """

    let program = evaluate input
    //let program = evaluate_file "test.wl"
    Console.WriteLine(program)
    Console.WriteLine (IntervalDomain.eval program)
    Console.ReadKey() |> ignore
    0
