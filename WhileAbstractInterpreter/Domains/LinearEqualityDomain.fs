module LinearEqualityDomain

open System
open MathNet.Numerics.LinearAlgebra
open Ast
open Domain

type LinearEquality =
    | Bottom
    | Constraint of int

    with
        static member ( ~- ) x =
            match x with
            | Constraint n -> Constraint -n
            | Bottom -> Bottom

        static member ( + ) (x, y) =
            match x, y with
            | Constraint a, Constraint b -> Constraint (a + b)
            | _ -> Bottom

        static member ( - ) (x, y) =
            match x, y with
            | Constraint a, Constraint b -> Constraint (a - b)
            | _ -> Bottom

        static member ( * ) (x, y) =
            match x, y with
            | Constraint a, Constraint b -> Constraint (a * b)
            | _ -> Bottom

        static member ( / ) (x, y) =
            match x, y with
            | Constraint a, Constraint b -> Constraint (a / b)
            | _ -> Bottom

        override this.ToString() =
            match this with
            | Constraint n -> n.ToString()
            | Bottom -> "\u22A5"

let private V = Vector<double>.Build;
let private M = Matrix<double>.Build;


type LinearEqualityDomain() =
    inherit Domain<LinearEquality>()

    override this.default_var_state index count =
        //let init = V.Dense(count, fun i -> if i = index then 1.0 else 0.0)
        Constraint 0

    override this.point_wise_union s1 s2 =
        let coeff = M.DenseIdentity(s1.Count)
        let s1_value =
            s1
            |> Map.toArray
            |> Array.map (fun (_, value) -> value)
            |> Array.map (fun x -> match x with
                                   | Constraint x -> Convert.ToDouble x
                                   | Bottom -> Bottom)
            |> V.DenseOfArray
        s2

    override this.union x y =
        if x = y then x
        else failwithf "todo"

    override _.widening x y =
        // No actual widening is used in the linear equality domain
        y

    override this.narrowing x y = failwithf "todo"

    override this.intersect x y = failwithf "todo"

    member this.eval_expr expr (state: Map<string, LinearEquality>) =
        match expr with
        | Constant value -> Constraint value
        | Random -> Constraint (System.Random().Next())
        | Variable var_name ->
            match state.TryFind var_name with
            | Some v -> v
            | None -> Bottom
        | UnOp ("-", expr) -> -this.eval_expr expr state
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
        | Constraint _ -> state.Add(var_name, value)
        | Bottom -> Map.empty

    override this.eval_abstr_cond expr state =
        match expr with
        | BinOp (l, "<=", r) ->
            let left_val = this.eval_expr l state
            let right_val = this.eval_expr r state
            match left_val, right_val with
            | Constraint a, Constraint b ->
                if a <= b then state
                else Map.empty
            | _ -> state
        | BinOp (l, ">", r) ->
            let left_val = this.eval_expr l state
            let right_val = this.eval_expr r state
            match left_val, right_val with
            | Constraint a, Constraint b ->
                if a > b then state
                else Map.empty
            | _ -> state

        | BinOp (l, "=", r) ->
            let left_val = this.eval_expr l state
            let right_val = this.eval_expr r state

            match left_val, right_val with
            | Constraint a, Constraint b ->
                if a = b then state
                else Map.empty
            | _ -> state

        | BinOp (l, "!=", r) ->
            let left_val = this.eval_expr l state
            let right_val = this.eval_expr r state

            match left_val, right_val with
            | Constraint a, Constraint b ->
                if a <> b then state
                else Map.empty
            | _ -> state

        | _ -> this.eval_generic_abstr_cond expr state
