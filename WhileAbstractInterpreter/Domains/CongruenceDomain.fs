module CongruenceDomain

open Ast
open Domain


let rec private gcd a b =
    if b = 0 then a
    else gcd b (a % b)

let private lcm a b =
    match gcd a b with
    | 0 -> 0
    | x -> a * b / x

type Congruence =
    | Value of int * int
    | Bottom

    with
        static member ( ~- ) x =
            match x with
            | Value (a, b) -> Value(a, -b)
            | Bottom -> Bottom

        static member ( + ) (x, y) =
            match x, y with
            | Value (a, b), Value (a', b') ->
                match gcd a a' with
                | 0 -> Value (0, b + b')
                | a -> Value (a, (((b + b') % a) + a) % a)
            | _ -> Bottom

        static member ( - ) (x, y) =
            match x, y with
            | Value (a, b), Value (a', b') ->
                match gcd a a' with
                | 0 -> Value (0, b - b')
                | a -> Value (a, (((b - b') % a) + a) % a)
            | _ -> Bottom

        static member ( * ) (x, y) =
            match x, y with
            | Value (a, b), Value (a', b') ->
                let aa' = a * a'
                let ab' = a * b'
                let a'b = a' * b
                let bb' = b * b'
                Value (gcd aa' ab' |> gcd a'b, bb')
            | _ -> Bottom

        static member ( / ) (x, y) =
            match x, y with
            | Value _, Value (0, 0) -> Bottom
            | Value (a, b), Value (a', b') ->
                if a' = 0 && b' <> 0 && a % b' = 0 && b % b' = 0 then Value(a / (abs b'), b / b')
                else Value (1, 0)
            | _ -> Bottom

        override this.ToString() =
            match this with
            | Value (a, b) -> $"{a}\u2124 + {b}" // $"{b} mod {a}"
            | Bottom -> "\u22A5"

type CongruenceDomain() =
    inherit Domain<Congruence>()

    override _.default_var_state = Value(1, 0)

    override _.union x y =
        match x, y with
        | Value (a, b), Value (a', b') ->
            let bb' = abs (b - b')
            Value (gcd a a' |> gcd bb', b)
        | _ -> Bottom

    override this.widening x y =
        // No actual widening is used in the congruence domain
        y

    override _.narrowing x y =
        match x, y with
        | Value (a, _), Value (_, _) when a = 1 -> y
        | _ -> x

    member private this.bezout a b =
        match b with
        | 0 -> (a, 1, 0)
        | _ ->
            let (d, x, y) = this.bezout b (a % b)
            (d, y, x - (a / b) * y)

    override this.intersect x y =
        match x, y with
        | Value (a, b), Value (a', b') when (gcd a a') % abs (b - b') = 0 ->
            let b'', _, _ = this.bezout b (lcm a a')
            Value (lcm a a', b'')
        | _ -> Bottom

    member private this.eval_expr expr state =
        match expr with
        | Constant value -> Value(0, value)
        | Random -> Value(0, System.Random().Next())
        | Variable var_name ->
            match Map.tryFind var_name state with
            | Some v -> v
            | None -> Bottom
        | UnOp ("-", expr) ->
            -this.eval_expr expr state
        | BinOp (l, ("+" | "-" | "*" | "/" as op), r) ->
            let left_val = this.eval_expr l state
            let right_val = this.eval_expr r state
            match op with
            | "+" -> left_val + right_val
            | "-" -> left_val - right_val
            | "*" -> left_val * right_val
            | "/" -> left_val / right_val
            | _ -> failwithf "Not implemented yet"
        | Range (a, b) -> if a = b then Value (0, a) else Value(1, 0)
        | _ -> failwithf "Not implemented yet"

    override this.eval_var_dec var_name expr state =
        let value = this.eval_expr expr state
        match value with
        | Value _ -> state.Add(var_name, value)
        | Bottom -> Map.empty

    override this.eval_abstr_cond expr state =
        match expr with
        | BinOp (l, "<=", r) ->
            match l, r with
            | Constant a, Constant b -> if a <= b then state else Map.empty
            | Variable _, Constant 0 ->
                let left_val = this.eval_expr l state
                match left_val with
                | Value (a, b) -> if b > 0 then Map.empty else state
                | _ -> state
            | Constant 0, Variable _ ->
                let right_val = this.eval_expr r state
                match right_val with
                | Value (a, b) -> if b >= 0 then state else Map.empty
                | _ -> state
            | _ -> state
        | BinOp(l, ">", r) ->
            match l, r with
            | Constant a, Constant b -> if a > b then state else Map.empty
            | Variable _, Constant 0 ->
                let left_val = this.eval_expr l state
                match left_val with
                | Value (a, b) -> if b > 0 then state else Map.empty
                | _ -> state
            | Constant 0, Variable _ ->
                let right_val = this.eval_expr r state
                match right_val with
                | Value (a, b) -> if b > 0 then Map.empty else state
                | _ -> state
            | _ -> state

        | BinOp(l, "=", r) ->
            match l, r with
            | Constant a, Constant b -> if a = b then state else Map.empty
            | _ -> state

        | BinOp(l, "!=", r) ->
            match l, r with
            | Constant a, Constant b -> if a <> b then state else Map.empty
            | _ -> state

        | _ -> this.eval_generic_abstr_cond expr state
