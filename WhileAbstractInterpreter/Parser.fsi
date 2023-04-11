// Signature file for parser generated by fsyacc
module Parser
type token = 
  | EOF
  | LPAREN
  | RPAREN
  | LCURLPAREN
  | RCURLPAREN
  | ASN
  | SEMICOLON
  | SKIP
  | IF
  | THEN
  | ELSE
  | WHILE
  | DO
  | PLUS
  | MINUS
  | MULT
  | DIV
  | TRUE
  | FALSE
  | NOT
  | AND
  | OR
  | LT
  | LEQ
  | EQ
  | GEQ
  | GT
  | Var of (string)
  | Number of (int)
type tokenId = 
    | TOKEN_EOF
    | TOKEN_LPAREN
    | TOKEN_RPAREN
    | TOKEN_LCURLPAREN
    | TOKEN_RCURLPAREN
    | TOKEN_ASN
    | TOKEN_SEMICOLON
    | TOKEN_SKIP
    | TOKEN_IF
    | TOKEN_THEN
    | TOKEN_ELSE
    | TOKEN_WHILE
    | TOKEN_DO
    | TOKEN_PLUS
    | TOKEN_MINUS
    | TOKEN_MULT
    | TOKEN_DIV
    | TOKEN_TRUE
    | TOKEN_FALSE
    | TOKEN_NOT
    | TOKEN_AND
    | TOKEN_OR
    | TOKEN_LT
    | TOKEN_LEQ
    | TOKEN_EQ
    | TOKEN_GEQ
    | TOKEN_GT
    | TOKEN_Var
    | TOKEN_Number
    | TOKEN_end_of_input
    | TOKEN_error
type nonTerminalId = 
    | NONTERM__startprog
    | NONTERM_prog
    | NONTERM_seqStatement
    | NONTERM_statement
    | NONTERM_arithmExpr
    | NONTERM_boolExpr
/// This function maps tokens to integer indexes
val tagOfToken: token -> int

/// This function maps integer indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val prog : (FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> FSharp.Text.Lexing.LexBuffer<'cty> -> (Ast.Stm) 
