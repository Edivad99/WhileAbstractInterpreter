module Program

open System
open FSharp.Text.Lexing

let evaluate input =
    let lexbuf = LexBuffer<char>.FromString input
    Parser.prog Lexer.tokenize lexbuf


[<EntryPoint>]
let main args =
    let input = """
    // Example from page 109 of the slides.
    x := -1;
    while x != 0 do {
      x := x + 1
    }
    """

    Console.WriteLine(input)
    let result = evaluate input
    Console.WriteLine(result)
    Console.ReadKey() |> ignore
    0
