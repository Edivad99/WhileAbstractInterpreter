module AbstractState

open Ast
open Domain


type AbstractState<'T when 'T: comparison>(domain : Domain<'T>) =
    member _.Domain = domain
    member _.WideningDelay = 0


    member private _.resolve_conflicts f acc key value =
        match Map.tryFind key acc with
        | Some v -> Map.add key (f v value) acc
        | None -> Map.add key value acc

    member private this.point_wise_union (s1: Map<string, 'T>) (s2: Map<string, 'T>) =
        Map.fold (fun acc key value -> this.resolve_conflicts this.Domain.union acc key value) s1 s2

    member private this.point_wise_widening (s1: Map<string, 'T>) (s2: Map<string, 'T>) =
        Map.fold (fun acc key value -> this.resolve_conflicts this.Domain.widening acc key value) s1 s2

    member private this.point_wise_narrowing (s1: Map<string, 'T>) (s2: Map<string, 'T>) =
        Map.fold (fun acc key value -> this.resolve_conflicts this.Domain.narrowing acc key value) s1 s2

    member private _.check_fixpoint (s1: Map<string, 'T>) (s2: Map<string, 'T>) =
        s1 = s2

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

            | While (cond, expr) ->
                let mutable prev_state = this.Domain.eval_abstr_cond cond state

                let mutable before_body = []
                let mutable after_body = []
                let mutable fixpoint = false
                let mutable iteration = 1

                while not fixpoint do
                    let s2 = this.Domain.eval_abstr_cond cond prev_state
                    // Salva i program points
                    before_body <- [s2]
                    // Valuta il corpo del while con gli stati che soddisfano la condizione
                    let while_state, body_points = this.eval(expr, s2, List.empty)
                    after_body <- body_points
                    // Fai l'unione point wise tra gli stati originali e gli ultimi
                    let union = this.point_wise_union state while_state
                    // Applica il widening per accelerare la divergenza
                    let curr_state =
                        if iteration > this.WideningDelay then
                            this.point_wise_widening prev_state union
                        else
                            union

                    // Controlla se abbiamo l'invariante
                    fixpoint <- this.check_fixpoint prev_state curr_state
                    // Aggiorna il prev_state
                    prev_state <- curr_state
                    iteration <- iteration + 1

                let state_after_while = this.Domain.eval_abstr_cond (UnOp("!", cond)) prev_state

                let state_after_while_narr = this.point_wise_narrowing state_after_while (List.last after_body)

                (state_after_while_narr, state_points
                    @ before_body
                    @ after_body
                    @ [state_after_while_narr])
