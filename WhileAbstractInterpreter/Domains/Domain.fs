module Domain
open Ast

[<AbstractClass>]
type Domain<'T when 'T: comparison>() =

    // Definisce lo stato iniziale delle variabili
    abstract default_var_state: 'T
    abstract eval_var_dec: string -> Expr -> Map<string, 'T> -> Map<string, 'T>
    abstract eval_abstr_cond: Expr -> Map<string, 'T> -> Map<string, 'T>

    // Union: Slide 62/100 parte 2
    abstract union: 'T -> 'T -> 'T

    abstract widening: 'T -> 'T -> 'T
    abstract narrowing: 'T -> 'T -> 'T

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
