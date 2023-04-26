module IntervalDomain

open Ast
open Domain

type Number =
    | MinusInf
    | Num of int
    | PlusInf
    with
        static member ( + ) (x, y) =
            match x, y with
            | Num l, Num r -> Num (l + r)

            | Num _, PlusInf
            | PlusInf, Num _-> PlusInf

            | Num _, MinusInf
            | MinusInf, Num _-> MinusInf

            | PlusInf, PlusInf -> PlusInf
            | MinusInf, MinusInf -> MinusInf
            | _ -> failwith "sum operands not supported"

        static member ( - ) (x, y) =
            match x, y with
            | Num l, Num r -> Num (l - r)

            | Num _, PlusInf -> MinusInf
            | Num _, MinusInf -> PlusInf

            | MinusInf, Num _ -> MinusInf
            | MinusInf, PlusInf -> MinusInf

            | PlusInf, Num _ -> PlusInf
            | PlusInf, MinusInf -> PlusInf
            | _ -> failwith "minus operands not supported"

        static member ( * ) (x, y) =
            match x, y with
            | Num l, Num r -> Num (l * r)

            | _, Num z
            | Num z, _ when z = 0 -> Num 0 // 0 x _ = 0

            | Num n, PlusInf
            | PlusInf, Num n -> if n < 0 then MinusInf else PlusInf

            | Num n, MinusInf
            | MinusInf, Num n -> if n < 0 then PlusInf else MinusInf

            | MinusInf, MinusInf
            | PlusInf, PlusInf -> PlusInf
            | PlusInf, MinusInf
            | MinusInf, PlusInf -> MinusInf

        static member ( / ) (x, y) =
            match x, y with
            | Num num, Num den ->
                match num, den with
                | 0, _ -> Num 0
                | _, 0 -> if num < 0 then MinusInf else PlusInf
                | _ -> Num (num / den)
            | Num _, PlusInf
            | Num _, MinusInf -> Num 0  // n / (+-Inf) = 0
            | PlusInf, Num n -> if n >= 0 then PlusInf else MinusInf
            | MinusInf, Num n -> if n >= 0 then MinusInf else PlusInf
            | _ -> Num 0                // (+-Inf) / (+-Inf) = 0

        override this.ToString() =
            match this with
            | PlusInf -> "+\u221E"
            | Num n -> n.ToString()
            | MinusInf -> "-\u221E"

type Interval =
    | Range of Number * Number
    | Bottom

    with
        static member ( ~- ) x =
            match x with
            | Range (n1, n2) ->
                match n1, n2 with
                | Num a, Num b -> Range (Num -b, Num -a)        // [a,b] = [-b,-a]
                | MinusInf, Num a -> Range (Num -a, PlusInf)    // [-Inf, a] = [-a, +Inf]
                | Num a, PlusInf -> Range (MinusInf, Num -a)    // [a, +Inf] = [-Inf, -a]
                | _ -> Range (MinusInf, PlusInf)                // [+-Inf, +-Inf] = [-Inf, +Inf] Always sound
            | Bottom -> Bottom

        static member ( + ) (x, y) =
            match x, y with
            | Range (a, b), Range (c, d) -> Range (a + c, b + d)
            | _ -> Bottom

        static member ( - ) (x, y) =
            match x, y with
            | Range (a, b), Range (c, d) -> Range (a - d, b - c)
            | _ -> Bottom

        static member ( * ) (x, y) =
            match x, y with
            | Range (a, b), Range (c, d) ->
                let ac = a * c
                let ad = a * d
                let bc = b * c
                let bd = b * d

                let min = List.min [ac; ad; bc; bd]
                let max = List.max [ac; ad; bc; bd]
                Range (min, max)
            | _ -> Bottom

        static member union x y =
            match x, y with
            | Range (a, b), Range (c, d) -> Range (min a c, max b d)
            | Bottom, _ -> y
            | _, Bottom -> x

        static member ( / ) (x, y) =
            match x, y with
            | Range (a, b), Range (c, d) ->
                if c = Num 0 && d = Num 0 then Bottom // if [c,d] = [0,0]
                elif Num 0 <= c then
                    let ac = a / c
                    let ad = a / d
                    let bc = b / c
                    let bd = b / d

                    let min = List.min [ac; ad; bc; bd]
                    let max = List.max [ac; ad; bc; bd]
                    Range (min, max)
                elif d <= Num 0 then
                    let t1 = -x
                    let t2 = -y
                    t1 / t2 // [-b, -a] / [-d, -c] if d <= 0
                else
                    let t1 = x / (Range (c, Num 0))
                    let t2 = x / (Range (Num 0, d))
                    Interval.union t1 t2 // ([a, b] / [c, 0]) U ([a, b] / [0, d])
            | _ -> Bottom

        override this.ToString() =
            match this with
            | Range (l, r) -> $"[{l.ToString()}, {r.ToString()}]"
            | Bottom -> "\u22A5"

type IntervalDomain() =
    inherit Domain<Interval>()

    override _.default_var_state _ _= Range(MinusInf, PlusInf)

    override _.union x y = Interval.union x y

    override _.widening x y =
        match x, y with
        | Range (a, b), Range(c, d) ->
            let lower =
                        if a <= c then a
                        elif Num 0 <= c && c < a then Num 0
                        else MinusInf
            let higher =
                        if b >= d then b
                        elif Num 0 >= d && d > b then Num 0
                        else PlusInf
            Range (lower, higher)
        | Bottom, _ -> y
        | _, Bottom -> x

    override _.narrowing x y =
        match x, y with
        | Range (a, b), Range (c, d) ->
            let lower = if a = MinusInf then c else a
            let higher = if b = PlusInf then d else b
            Range(lower, higher)
        | Bottom, _ -> y
        | _, Bottom -> x

    override _.intersect x y =
        match x, y with
        | Range(a, b), Range(c, d) ->
            let lower = max a c
            let higher = min b d
            if lower <= higher then Range(lower, higher) else Bottom
        | _ -> Bottom

    member private this.eval_expr expr (state: Map<string, Interval>) =
        match expr with
        | Constant value -> Range(Num value, Num value)
        | Random -> Range(MinusInf, PlusInf)
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
        | Expr.Range (a, b) ->
            let left_val = this.eval_expr a state
            let right_val = this.eval_expr b state
            match left_val, right_val with
            | Range(a, b), Range (c, d) when a = b && c = d -> Range(a, c)
            | _ -> failwithf "Unsupported values"
        | _ -> failwithf "Not implemented yet"

    override this.eval_var_dec var_name expr state =
        let value = this.eval_expr expr state
        match value with
        | Range _ -> state.Add(var_name, value)
        | Bottom -> Map.empty

    override this.eval_abstr_cond expr state =
        match expr with
        | BinOp (l, "<=", r) ->
            match l, r with
            | Constant a, Constant b ->
                if a <= b then state
                else Map.empty

            | Variable var_name, Constant c ->
                let c = Num c
                let left_val = this.eval_expr l state
                match left_val with
                | Range (a, b) ->
                    if c < a then Map.empty
                    else
                        state.Add(var_name, Range(a, min b c))
                | _ -> state

            | Constant c, Variable var_name ->
                let c = Num c
                let right_val = this.eval_expr r state
                match right_val with
                | Range (a, b) ->
                    if b < c then Map.empty
                    else
                        state.Add(var_name, Range(max a c, b))
                | _ -> state

            | Variable left_var_name, Variable right_var_name ->
                let left_val = this.eval_expr l state
                let right_val = this.eval_expr r state
                match left_val, right_val with
                | Range (a, b), Range (c, d) ->
                    if a > d then Map.empty
                    else
                        let state = state.Add(left_var_name, Range(a, min b d))
                        state.Add(right_var_name, Range(max c a, d))
                | _ -> state
            | _ -> state

        | BinOp (l, ">", r) ->
            match l, r with
            | Constant a, Constant b ->
                if a > b then state
                else Map.empty
            | Variable var_name, Constant c ->
                let c = Num c
                let left_val = this.eval_expr l state
                match left_val with
                | Range (a, b) ->
                    if c >= b then Map.empty
                    else
                        state.Add(var_name, Range(max a (c + Num 1), b))
                | _ -> state
            | Constant c, Variable var_name ->
                let c = Num c
                let right_val = this.eval_expr r state
                match right_val with
                | Range (a, b) ->
                    if c <= a then Map.empty
                    else
                        state.Add(var_name, Range(a, min (c - Num 1) b))
                | _ -> state
            | Variable left_var_name, Variable right_var_name ->
                let left_val = this.eval_expr l state
                let right_val = this.eval_expr r state
                match left_val, right_val with
                | Range (a, b), Range (c, d) ->
                    if b <= c then Map.empty
                    else
                        let state = state.Add(left_var_name, Range(max a (c + Num 1) , b))
                        // Non ne sono sicuro ma dovrebbe essere così
                        state.Add(right_var_name, Range(c, min (b - Num 1) d))
                | _ -> state
            | _ -> state

        | BinOp (l, "=", r) ->
            let left_val = this.eval_expr l state
            let right_val = this.eval_expr r state

            match left_val, right_val with
            | Range (a, b), Range (c, d) ->
                if b < c then Map.empty
                elif d < a then Map.empty
                else
                    let new_value = this.intersect left_val right_val
                    match l, r with
                    | Variable left_var_name, Variable right_var_name ->
                        state.Add(left_var_name, new_value).Add(right_var_name, new_value)
                    | Variable left_var_name, _ -> state.Add(left_var_name, new_value)
                    | _, Variable right_var_name -> state.Add(right_var_name, new_value)
                    | _ -> state
            | _ -> state

        | BinOp (l, "!=", r) ->
            match l, r with
            | Constant a, Constant b -> if a <> b then state else Map.empty
            | Variable var_name, Constant c ->
                let c = Num c
                let left_val = this.eval_expr l state
                match left_val with
                | Range (a, b) ->
                    if c < a || c > b then state
                    elif a < c && c < b then Map.empty
                    elif a = c && c < b then state.Add(var_name, Range(a + Num 1, b))
                    elif b = c && a < b then state.Add(var_name, Range(a, b - Num 1))
                    else state
                | _ -> state

            | Constant _, Variable _ -> this.eval_abstr_cond(BinOp(r, "!=", l)) state

            | Variable left_var_name, Variable right_var_name ->
                let left_val = this.eval_expr l state
                let right_val = this.eval_expr r state
                match left_val, right_val with
                | Range (a, b), Range (c, d) ->
                    if c < a && b < d then Map.empty
                    elif a < c && d < b then Map.empty
                    elif a > d || b < c then state
                    elif a > c then this.eval_abstr_cond (BinOp (r, "!=", l)) state
                    else
                        let state = state.Add(left_var_name, Range(a, min (c - Num 1) b))
                        let lower_bound = min (max c (b + Num 1)) d
                        state.Add(right_var_name, Range(lower_bound, d))
                | _ -> state
            | _ -> state

        | _ -> this.eval_generic_abstr_cond expr state
