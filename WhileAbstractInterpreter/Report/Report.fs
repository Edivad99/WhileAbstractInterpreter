module Report

open HandlebarsDotNet
open System.IO
open System.Diagnostics
open System.Reflection

let private source =
    """<!DOCTYPE html>
    <html lang="it">
    <head>
        <meta charset="UTF-8">
        <title>While Static Analyzer</title>
        <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.0-alpha3/dist/css/bootstrap.min.css" rel="stylesheet">
    </head>
    <body style="margin-left: 20px;">
        <h3 class="display-3">While Static Analyzer</h3>
        <strong class="text-body-secondary">Davide Albiero, Damiano Mason</strong>
        <h5 style="margin-top: 50px;" class="text-primary">Input code:</h5>
        <pre>{{{code}}}</pre>
    </body>
    </html>"""

let private template = Handlebars.Compile(source)

let rec private interleave (program_points: string list, code_lines: string list) =
    match program_points, code_lines with
    | ([], code_lines) -> printfn "warning: interleave"; code_lines //NON DOVREBBE MAI SUCCEDERE
    | (program_points, []) -> program_points
    | (program_point::program_points, code_line::code_lines) ->
        // Ignoriamo i commenti
        if (code_line.StartsWith("//")) then
            code_line :: interleave(program_point::program_points, code_lines)
        else
            program_point :: code_line :: interleave (program_points, code_lines)


let private format_code (code: string, program_points) =
    let formatted_code = code.Split '\n'
                        |> Array.map (fun x -> x.TrimStart())
                        |> Array.filter (fun x -> x.Length > 0)
                        //|> Array.filter (fun x -> not (x.StartsWith("//")))
                        |> List.ofArray

    let formatted_program_points =
        program_points
        |> List.map (fun x -> x.ToString().Replace("map", "//"))
        |> List.map (fun x -> $"""<span style="color: green">{x}</span>""")

    interleave (formatted_program_points, formatted_code)
    |> String.concat("\n")

let generate_report code program_points =
    let data = {|code = format_code(code, program_points);|}
    let result = template.Invoke(data)

    let executableLocation = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
    let file_path = Path.Combine(executableLocation, "report.html")
    File.WriteAllText(file_path, result)

    let procStartInfo = ProcessStartInfo(UseShellExecute = true, FileName = file_path)
    let p = new Process(StartInfo = procStartInfo)
    p.Start() |> ignore
