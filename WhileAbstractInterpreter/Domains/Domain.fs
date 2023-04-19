﻿module Domain
open Ast

[<AbstractClass>]
type Domain<'T when 'T: comparison>() =

    // Definisce lo stato iniziale delle variabili
    abstract default_var_state: 'T
    abstract eval_var_dec: string -> Expr -> Map<string, 'T> -> Map<string, 'T>
    abstract eval_abstr_cond: Expr -> Map<string, 'T> -> Map<string, 'T>

    // Union: Slide 62/100 parte 2
    abstract union: 'T -> 'T -> 'T
    abstract intersect: 'T -> 'T -> 'T

    abstract widening: 'T -> 'T -> 'T
    abstract narrowing: 'T -> 'T -> 'T

    member private _.resolve_conflicts f acc key value =
        match Map.tryFind key acc with
        | Some v -> Map.add key (f v value) acc
        | None -> Map.add key value acc

    member this.point_wise_union (s1: Map<string, 'T>) (s2: Map<string, 'T>) =
        Map.fold (fun acc key value -> this.resolve_conflicts this.union acc key value) s1 s2

    member this.point_wise_widening (s1: Map<string, 'T>) (s2: Map<string, 'T>) =
        Map.fold (fun acc key value -> this.resolve_conflicts this.widening acc key value) s1 s2

    member this.point_wise_narrowing (s1: Map<string, 'T>) (s2: Map<string, 'T>) =
        Map.fold (fun acc key value -> this.resolve_conflicts this.narrowing acc key value) s1 s2

    member this.point_wise_intersection (s1: Map<string, 'T>) (s2: Map<string, 'T>) =
        Map (
            seq {
                for KeyValue(k, vs1) in s1 do
                    match Map.tryFind k s2 with
                    | Some vs2 -> yield k, this.intersect vs1 vs2
                    | None -> ()
            }
        )

    member this.get_init_state program =
        let rec find_variable program =
            match program with
            | VarDec (name, _) -> Set.singleton name
            | Skip -> Set.empty
            | IfThenElse (_, true_branch, false_branch) ->
                Set.union (find_variable true_branch) (find_variable false_branch)
            | While (_, block) -> find_variable block
            | Seq (stm_1, stm_2) ->
                Set.union (find_variable stm_1) (find_variable stm_2)

        find_variable program
        |> Set.map (fun x -> (x, this.default_var_state))
        |> Map.ofSeq
