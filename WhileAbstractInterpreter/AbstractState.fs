module AbstractState

open Ast
open Domain


type AbstractState<'T when 'T: comparison>(domain : Domain<'T>) =
    member _.Domain = domain

    member this.eval (program: Stm, state: Map<string, 'T>, state_points: Map<string, 'T> list) =
        if state.IsEmpty then
            (Map.empty, state_points @ [Map.empty])
        else
            match program with
            | Skip -> (state, state_points @ [state])

            // Se l'expr è valutata come bottom, lo propaghiamo
            | VarDec (var_name, expr) ->
                let state = this.Domain.eval_var_dec var_name expr state
                in (state, state_points @ [state])

            // Esegui il primo sotto-programma e poi il secondo sullo stato risultato
            // dall'esecuzione del primo
            | Seq (p1, p2) ->
                let (s1, state_points) = this.eval(p1, state, state_points)
                this.eval (p2, s1, state_points)

            | _ -> failwithf "Not yet implemented"

