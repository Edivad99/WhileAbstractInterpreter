module Ast

type Expr =
    | Constant of int
    | Random
    | Variable of string
    | Boolean of bool
    | UnOp of string * Expr
    | BinOp of Expr * string * Expr
    | Range of Expr * Expr

type Stm =
    | VarDec of string * Expr
    | Skip
    | IfThenElse of Expr * Stm * Stm
    | While of Expr * Stm
    | Seq of Stm * Stm
