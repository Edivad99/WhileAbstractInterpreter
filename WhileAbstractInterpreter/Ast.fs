module Ast

type Expr =
    | Constant of int
    | Random
    | Variable of string
    | Boolean of bool
    | UnOp of string * Expr
    | BinOp of Expr * string * Expr
    | Range of int * int

type Stm =
    | VarDec of string * Expr
    | Skip
    | IfThenElse of Expr * Stm * Stm option
    | While of Expr * Stm
    | Seq of Stm * Stm
