module CongruenceDomain

open Ast
open Domain

type Congruence =
    | Value of int * int
    | Bottom

    with
        static member ( ~- ) x =
            match x with
            | Value (a, b) -> Value(a, -b)
            | Bottom -> Bottom

        static member gcd a b =
            if b = 0 then a
            else Congruence.gcd b (a % b)

        static member lcm a b =
            match Congruence.gcd a b with
            | 0 -> 0
            | x -> a * b / x

        static member ( + ) (x, y) =
            match x, y with
            | Value (a, b), Value (a', b') -> Value (Congruence.gcd a a', b + b')
            | _ -> Bottom

        static member ( - ) (x, y) =
            match x, y with
            | Value (a, b), Value (a', b') -> Value (Congruence.gcd a a', b - b')
            | _ -> Bottom

        static member ( * ) (x, y) =
            match x, y with
            | Value (a, b), Value (a', b') ->
                let aa' = a * a'
                let ab' = a * b'
                let a'b = a' * b
                let bb' = b * b'
                Value (Congruence.gcd (Congruence.gcd aa' ab') a'b, bb')
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
            | Value (a, b) -> $"{a}\u2124 + {b}"
            | Bottom -> "\u22A5"

type CongruenceDomain() =
    inherit Domain<Congruence>()

    override _.default_var_state = Value(1, 0)

    override _.union x y =
        match x, y with
        | Value (a, b), Value (a', b') ->
            let bb' = abs (b - b')
            Value (Congruence.gcd (Congruence.gcd a a') bb', b)
        | _ -> Bottom

    override _.widening x y =
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
        | Value (a, b), Value (a', b') when (Congruence.gcd a a') % abs (b - b') = 0 ->
            let b'', _, _ = this.bezout b (Congruence.lcm a a')
            Value (Congruence.lcm a a', b'')
        | _ -> Bottom

    member private this.eval_expr expr state =
        match expr with
        | Constant value -> Value(value, 0)
        | Random -> Value(System.Random().Next(), 0)
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
        | _ -> failwithf "Not implemented yet"

    override this.eval_var_dec var_name expr state =
        let value = this.eval_expr expr state
        match value with
        | Value _ -> state.Add(var_name, value)
        | Bottom -> Map.empty

    override this.eval_abstr_cond expr state = failwith "todo"
