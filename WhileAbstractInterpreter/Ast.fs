module Ast

type Expr =
    | Num of int
    | Variable of string
    | Boolean of bool
    | UnOp of string * Expr
    | BinOp of Expr * string * Expr

type Stm =
    | VarDec of string * Expr
    | Skip
    | IfThenElse of Expr * Stm * Stm
    | While of Expr * Stm
//    | Read of string //Capire come implementare la lettura e se serve
//    | Print of Expr
    | Seq of Stm * Stm
