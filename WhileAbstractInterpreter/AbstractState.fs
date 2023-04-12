module AbstractState

open Ast
open Domain


type AbstractState<'T when 'T: comparison>(domain : Domain<'T>) =
    member _.Domain = domain


    member private this.point_wise_union (s1: Map<string, 'T>) (s2: Map<string, 'T>) =
        let resolve_conflicts acc key value =
            match Map.tryFind key acc with
            | Some v -> Map.add key (this.Domain.union v value) acc
            | None -> Map.add key value acc

        Map.fold resolve_conflicts s1 s2


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

            | IfThenElse (cond, true_branch, false_branch) ->
                let state_cond = this.Domain.eval_abstr_cond cond state

                let s1, true_branch_points =
                    if Map.isEmpty state_cond then (Map.empty, [Map.empty])
                    else this.eval (true_branch, state_cond, List.empty)

                let state_else_cond = this.Domain.eval_abstr_cond (UnOp("!", cond)) state

                let s2, false_branch_points =
                    if Map.isEmpty state_else_cond then (Map.empty, [Map.empty])
                    else this.eval (false_branch, state_else_cond, List.empty)

                // Fai il point wise union
                let next = this.point_wise_union s1 s2

                (next, state_points
                    @ [state_cond] @ true_branch_points
                    @ [state_else_cond] @ false_branch_points
                    @ [next])

            | _ -> failwithf "Not yet implemented"

