module IntervalDomain

open Ast
open Domain

type Number =
    | PlusInf
    | Num of int
    | MinusInf
    with override this.ToString() =
            match this with
            | PlusInf -> "+\u221E"
            | Num n -> n.ToString()
            | MinusInf -> "-\u221E"

type Interval =
    | Range of Number * Number
    | Bottom
    with override this.ToString() =
            match this with
            | Range (l, r) -> $"[{l.ToString()}, {r.ToString()}]"
            | Bottom -> "\u22A5"

type IntervalDomain() =
    inherit Domain<Interval>()

    override _.default_var_state = Range(MinusInf, PlusInf)

    member _.eval_expr expr state =
        match expr with
        | Constant value -> Range(Num value, Num value)
        | _ -> failwithf "Not yet implemented"

    override this.eval_var_dec var_name expr state =
        let value = this.eval_expr expr state
        match value with
        | Range _ -> state.Add(var_name, value)
        | Bottom -> Map.empty


