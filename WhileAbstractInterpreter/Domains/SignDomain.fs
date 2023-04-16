module SignDomain

open Ast
open Domain

type Sign =
    | Top
    | Positive
    | Zero
    | Negative
    | Bottom
    with
        static member ( ~- ) x =
            match x with
            | Top -> Top
            | Positive -> Negative
            | Zero -> Zero
            | Negative -> Positive
            | Bottom -> Bottom

        static member ( + ) (x, y) =
            match x, y with
            | Zero, Zero -> Zero

            | Negative, Negative
            | Negative, Zero
            | Zero, Negative -> Negative

            | Positive, Positive -> Positive
            | Positive, Zero -> Positive
            | Zero, Positive -> Positive

            | Negative, Positive
            | Positive, Negative
            | Top, _
            | _, Top -> Top

            | _ -> Bottom

        static member ( - ) (x, y) =
            match x, y with
            | Top, _
            | _, Top -> Top

            | Negative, Negative -> Top
            | Negative, Zero -> Negative
            | Negative, Positive -> Negative

            | Zero, Negative -> Positive
            | Zero, Zero -> Zero
            | Zero, Positive -> Negative

            | Positive, Negative -> Positive
            | Positive, Zero -> Positive
            | Positive, Positive -> Top

            | _, Bottom
            | Bottom, _ -> Bottom


        static member ( * ) (x, y) =
            match x, y with
            | _, Zero
            | Zero, _ -> Zero

            | Negative, Negative
            | Positive, Positive -> Positive

            | Negative, Positive
            | Positive, Negative -> Negative

            | Top, _
            | _, Top -> Top

            | _ -> Bottom

        static member ( / ) (x, y) =
            match x, y with
            | _, Zero
            | Bottom, _
            | _, Bottom -> Bottom

            | Zero, _ -> Zero

            | Top, _
            | _, Top -> Top

            | Negative, Negative -> Positive
            | Negative, Positive -> Negative
            | Positive, Positive -> Positive
            | Positive, Negative -> Negative

        override this.ToString() =
            match this with
            | Top -> "\u22A4"
            | Positive -> "+"
            | Zero -> "0"
            | Negative -> "-"
            | Bottom -> "\u22A5"

type SignDomain() =
    inherit Domain<Sign>()

    override _.default_var_state = Top

    override _.union x y =
        match x, y with
        | _, Bottom
        | Bottom, _ -> Bottom

        | Negative, Negative -> Negative
        | Zero, Zero -> Zero
        | Positive, Positive -> Positive

        | _ -> Top

    override _.widening x y = failwithf "Not implemented yet"

    override _.narrowing x y = failwithf "Not implemented yet"


    member this.eval_expr expr (state: Map<string, Sign>) =
        match expr with
        | Constant value ->
            if value = 0 then Zero
            elif value > 0 then Positive
            else Negative
        | Variable var_name ->
            match state.TryFind var_name with
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
        | Range (a, b) ->
            let left_val = this.eval_expr a state
            let right_val = this.eval_expr b state
            this.union left_val right_val
        | _ -> failwithf "Not implemented yet"

    override this.eval_var_dec var_name expr state =
        let value = this.eval_expr expr state
        match value with
        | Bottom -> Map.empty
        | _ -> state.Add(var_name, value)

    override this.eval_abstr_cond expr state =
        failwithf "Not implemented yet"
