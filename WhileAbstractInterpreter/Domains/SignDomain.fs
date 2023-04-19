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

        | Positive, Zero
        | Zero, Positive -> Positive

        | Negative, Zero
        | Zero, Negative -> Negative

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
        match expr with
        | Boolean true -> state
        | Boolean false -> Map.empty

        | BinOp (l, "<", r) -> this.eval_abstr_cond (BinOp (r, ">", l)) state
        | BinOp (l, ">=", r) -> this.eval_abstr_cond (BinOp (r, "<=", l)) state

        | BinOp (l, "<=", r) ->
            match l, r with
            | Constant a, Constant b -> if a <= b then state else Map.empty
            | Variable var_name, Constant c ->
                let left_val = this.eval_expr l state 
                match left_val with
                | Positive
                | Zero ->
                    if c = 0 then state.Add(var_name, Zero)
                    elif c < 0 then Map.empty
                    else state
                | Negative
                | Top when c <= 0 -> state.Add(var_name, Negative)
                | Negative when c > 0 -> state
                | Top when c > 0 -> state.Add(var_name, Top)
                | _ -> state // Bottom
            | Constant c, Variable var_name ->
                let right_val = this.eval_expr r state 
                match right_val with
                | Positive -> state
                | Zero -> if c <= 0 then state else Map.empty
                | Negative -> if c <= 0 then state else Map.empty
                | Top -> state.Add(var_name, Positive)
                | _ -> state
            | Variable left_var_name, Variable right_var_name -> 
                let left_val = this.eval_expr l state
                let right_val = this.eval_expr r state

                match left_val with
                | Positive, _ -> 
                    match right_val with
                    | Zero -> state.Add(left_var_name, Zero)
                    | Negative ->
                        state.Add(left_var_name, Zero)
                             .Add(right_var_name, Zero)
                    | Top -> state.Add(right_var_name, Positive)
                    | _ -> state
                | Zero -> 
                    match right_val with
                    | Negative -> state.Add(right_var_name, Zero)
                    | Top -> state.Add(right_var_name, Positive)
                    | _ -> state
                | Top -> 
                    match right_val with
                    | Zero
                    | Negative -> state.Add(left_var_name, Negative)
                    | _ -> state
                //| Negative -> state
                | _ -> state
            | _ -> state

        | BinOp(l, ">", r) -> 
            match l,r with
            | Constant a, Constant b ->
                if a > b then state
                else Map.empty
            | Variable var_name, Constant c ->
                let left_val = this.eval_expr l state
                match left_val with
                | Positive -> state
                | Zero                    
                | Negative when c >= 0 -> Map.empty
                | Zero
                | Negative when c < 0 -> state
                | Top ->
                    if c >= 0 then state.Add(var_name, Positive)
                    else state.Add(var_name, Top)
                | _ -> state
            | Constant c, Variable var_name ->
                let right_val = this.eval_expr r state
                match right_val with
                | Positive ->
                    if c <> 0 then Map.empty
                    else state
                | Zero ->
                    if c >= 0 then Map.empty
                    else state
                | Top ->
                    if c >= 0 then state.Add(var_name, Negative)
                    else state
                //| Negative -> state
                | _ -> state
            | Variable left_var_name, Variable right_var_name ->
                let left_val = this.eval_expr l state
                let right_val = this.eval_expr r state

                match left_val, right_val with
                (*| Positive, _ 
                | _, Negative
                | Zero, Top 
                | Top, Negative
                | Top, Top -> state*)
                | Zero, Positive
                | Zero, Zero 
                | Negative, Positive
                | Negative, Zero -> Map.empty
                | Negative, Top -> state.Add(right_var_name, Negative)
                | Top, Positive
                | Top, Zero -> state.Add(left_var_name, Positive)
                | _ -> state
        | BinOp(l, "=", r) -> 
            match l, r with
            | Constant a, Constant b -> if a = b then state else Map.empty
            | Variable var_name, Constant c
            | Constant c, Variable var_name ->
                let new_val = this.eval_expr (Variable var_name) state
                match new_val with
                | Positive ->
                    if c = 0 then state.Add(var_name, Zero)
                    elif c > 0 then state
                    else Map.empty
                | Zero -> if c = 0 then state else Map.empty
                | Negative -> 
                    if c = 0 then state.Add(var_name, Zero)
                    elif c < 0 then state
                    else Map.empty
                | Top -> 
                    if c = 0 then state.Add(var_name, Zero)
                    elif c > 0 then state.Add(var_name, Positive)
                    else state.Add(var_name, Negative)
                | _ -> state
            | Variable left_var_name, Variable right_var_name -> 
                let left_val = this.eval_expr l state
                let right_val = this.eval_expr r state

                match left_val, right_val with
                | Positive, Positive
                | Zero, Zero
                | Negative, Negative
                | Top, Top -> state
                | Zero, _ -> state.Add(left_var_name, Zero)
                | _, Zero -> state.Add(right_var_name, Zero)
                | Positive, Negative
                | Negative, Positive -> 
                    state.Add(left_var_name, Zero)
                         .Add(right_var_name, Zero)
                | Positive, Top -> state.Add(right_var_name, Positive)
                | Negative, Top -> state.Add(right_var_name, Negative)
                | Top, Positive -> state.Add(left_var_name, Positive)
                | Top, Negative -> state.Add(left_var_name, Negative)
                | _ -> state
        | BinOp(l, "!=", r) -> 
            match l, r with
            | Constant a, Constant b -> if a <> b then state else Map.empty
            | Variable var_name, Constant c
            | Constant c, Variable var_name -> 
                let new_val = this.eval_expr (Variable var_name) state
                match new_val with
                | Positive when c > 0 -> state.Add(var_name, Top)
                | Negative when c < 0 -> state.Add(var_name, Top)
                | Zero when c = 0 -> Map.empty
                | Top -> 
                    if c = 0 then state
                    elif c < 0 then state.Add(var_name, Positive)
                    else state.Add(var_name, Negative)
                | _ -> state
            | Variable left_var_name, Variable right_var_name ->
                let left_val = this.eval_expr l state
                let right_val = this.eval_expr r state

                match left_val, right_val with
                | Positive, Positive
                | Zero, Zero
                | Negative, Negative
                | Top, Top -> Map.empty
                | Positive, Top -> state.Add(right_var_name, Negative)
                | Negative, Top -> state.Add(right_var_name, Positive)
                | Top, Positive -> state.Add(left_var_name, Negative)
                | Top, Negative -> state.Add(left_var_name, Positive)
                | _ -> state
            | _ -> state