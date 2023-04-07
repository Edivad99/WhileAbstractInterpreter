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

type IntervalDomain =

    (*member _.Interval = interval

    interface Domain<Interval> with
        member this.Equal other : bool =
            match (this.Interval, other) with
            | Bottom, Bottom -> true
            | Range(a, b), Range (c, d) -> a = c && b = d
            | _ -> false

        static member LessEqual t1 t2 = true*)

    static member get_init_state program =
        let rec set_init_state program =
            match program with
            | VarDec (name, _) -> Set.singleton name
            | Skip -> Set.empty
            | IfThenElse (_, true_branch, false_branch) ->
                Set.union (set_init_state true_branch) (set_init_state false_branch)
            | While (_, block) -> set_init_state block
            | Seq (stm_1, stm_2) ->
                Set.union (set_init_state stm_1) (set_init_state stm_2)

        set_init_state program
        |> Set.map (fun x -> (x, Range(MinusInf, PlusInf)))
        |> Map.ofSeq

    static member eval program =
        IntervalDomain.get_init_state program