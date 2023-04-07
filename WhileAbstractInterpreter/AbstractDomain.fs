module AbstractDomain

open Ast
open IntervalDomain


let eval_expr expr state =
    match expr with
    | Constant value -> Range(Num value, Num value)
    | _ -> failwithf "Not yet implemented"

// Resitutisce una mappa con i risultati finali per ogni variabile,
// Inoltre restituisce i vari program points
let rec eval (program: Stm, state: Map<string, Interval>, state_points: Map<string, Interval> list) =
    if state.IsEmpty then
        (Map.empty, state_points @ [Map.empty])
    else
        match program with
        | Skip -> (state, state_points @ [state])

        // Se l'expr è valutata come bottom, lo propaghiamo
        | VarDec (var_name, expr) ->
            let value = eval_expr expr state
            let state = match value with
                        | Range _ -> state.Add(var_name, value)
                        | Bottom -> Map.empty
            in (state, state_points @ [state])

        // Esegui il primo sotto-programma e poi il secondo sullo stato risultato
        // dall'esecuzione del primo
        | Seq (p1, p2) ->
            let (s1, state_points) = eval(p1, state, state_points)
            eval (p2, s1, state_points)

        | _ -> failwithf "Not yet implemented"
