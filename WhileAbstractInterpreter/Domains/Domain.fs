﻿module Domain
open Ast

[<AbstractClass>]
type Domain<'T when 'T: comparison>() =

    // Definisce lo stato iniziale delle variabili
    abstract default_var_state: 'T
    abstract eval_var_dec: string -> Expr -> Map<string, 'T> -> Map<string, 'T>

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





//abstract Less : 'T -> bool
//abstract LessEqual : 'T -> bool
//abstract Equal : 'T -> bool
//abstract GreaterEqual : 'T -> bool
//abstract Greater : 'T -> bool
//abstract NotEqual: 'T -> bool

//abstract Sum : 'T -> 'T
//abstract Minus : 'T -> 'T
//abstract Multiply : 'T -> 'T
//abstract Division : 'T -> 'T
//abstract Modulo : 'T -> 'T
//abstract Negate : unit -> 'T