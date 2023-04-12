// Implementation file for parser generated by fsyacc
module Parser
#nowarn "64";; // turn off warnings that type variables used in production annotations are instantiated to concrete type
open FSharp.Text.Lexing
open FSharp.Text.Parsing.ParseHelpers
# 1 "Parser.fsy"

open Ast

# 10 "Parser.fs"
// This type is the type of tokens accepted by the parser
type token = 
  | EOF
  | LPAREN
  | RPAREN
  | LCURLPAREN
  | RCURLPAREN
  | LSQRPAREN
  | RSQRPAREN
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
// This type is used to give symbolic names to token indexes, useful for error messages
type tokenId = 
    | TOKEN_EOF
    | TOKEN_LPAREN
    | TOKEN_RPAREN
    | TOKEN_LCURLPAREN
    | TOKEN_RCURLPAREN
    | TOKEN_LSQRPAREN
    | TOKEN_RSQRPAREN
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
// This type is used to give symbolic names to token indexes, useful for error messages
type nonTerminalId = 
    | NONTERM__startprog
    | NONTERM_prog
    | NONTERM_seqStatement
    | NONTERM_statement
    | NONTERM_arithmExpr
    | NONTERM_boolExpr

// This function maps tokens to integer indexes
let tagOfToken (t:token) = 
  match t with
  | EOF  -> 0 
  | LPAREN  -> 1 
  | RPAREN  -> 2 
  | LCURLPAREN  -> 3 
  | RCURLPAREN  -> 4 
  | LSQRPAREN  -> 5 
  | RSQRPAREN  -> 6 
  | ASN  -> 7 
  | SEMICOLON  -> 8 
  | SKIP  -> 9 
  | IF  -> 10 
  | THEN  -> 11 
  | ELSE  -> 12 
  | WHILE  -> 13 
  | DO  -> 14 
  | PLUS  -> 15 
  | MINUS  -> 16 
  | MULT  -> 17 
  | DIV  -> 18 
  | TRUE  -> 19 
  | FALSE  -> 20 
  | NOT  -> 21 
  | AND  -> 22 
  | OR  -> 23 
  | LT  -> 24 
  | LEQ  -> 25 
  | EQ  -> 26 
  | GEQ  -> 27 
  | GT  -> 28 
  | Var _ -> 29 
  | Number _ -> 30 

// This function maps integer indexes to symbolic token ids
let tokenTagToTokenId (tokenIdx:int) = 
  match tokenIdx with
  | 0 -> TOKEN_EOF 
  | 1 -> TOKEN_LPAREN 
  | 2 -> TOKEN_RPAREN 
  | 3 -> TOKEN_LCURLPAREN 
  | 4 -> TOKEN_RCURLPAREN 
  | 5 -> TOKEN_LSQRPAREN 
  | 6 -> TOKEN_RSQRPAREN 
  | 7 -> TOKEN_ASN 
  | 8 -> TOKEN_SEMICOLON 
  | 9 -> TOKEN_SKIP 
  | 10 -> TOKEN_IF 
  | 11 -> TOKEN_THEN 
  | 12 -> TOKEN_ELSE 
  | 13 -> TOKEN_WHILE 
  | 14 -> TOKEN_DO 
  | 15 -> TOKEN_PLUS 
  | 16 -> TOKEN_MINUS 
  | 17 -> TOKEN_MULT 
  | 18 -> TOKEN_DIV 
  | 19 -> TOKEN_TRUE 
  | 20 -> TOKEN_FALSE 
  | 21 -> TOKEN_NOT 
  | 22 -> TOKEN_AND 
  | 23 -> TOKEN_OR 
  | 24 -> TOKEN_LT 
  | 25 -> TOKEN_LEQ 
  | 26 -> TOKEN_EQ 
  | 27 -> TOKEN_GEQ 
  | 28 -> TOKEN_GT 
  | 29 -> TOKEN_Var 
  | 30 -> TOKEN_Number 
  | 33 -> TOKEN_end_of_input
  | 31 -> TOKEN_error
  | _ -> failwith "tokenTagToTokenId: bad token"

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
let prodIdxToNonTerminal (prodIdx:int) = 
  match prodIdx with
    | 0 -> NONTERM__startprog 
    | 1 -> NONTERM_prog 
    | 2 -> NONTERM_seqStatement 
    | 3 -> NONTERM_seqStatement 
    | 4 -> NONTERM_seqStatement 
    | 5 -> NONTERM_statement 
    | 6 -> NONTERM_statement 
    | 7 -> NONTERM_statement 
    | 8 -> NONTERM_statement 
    | 9 -> NONTERM_statement 
    | 10 -> NONTERM_arithmExpr 
    | 11 -> NONTERM_arithmExpr 
    | 12 -> NONTERM_arithmExpr 
    | 13 -> NONTERM_arithmExpr 
    | 14 -> NONTERM_arithmExpr 
    | 15 -> NONTERM_arithmExpr 
    | 16 -> NONTERM_arithmExpr 
    | 17 -> NONTERM_arithmExpr 
    | 18 -> NONTERM_arithmExpr 
    | 19 -> NONTERM_boolExpr 
    | 20 -> NONTERM_boolExpr 
    | 21 -> NONTERM_boolExpr 
    | 22 -> NONTERM_boolExpr 
    | 23 -> NONTERM_boolExpr 
    | 24 -> NONTERM_boolExpr 
    | 25 -> NONTERM_boolExpr 
    | 26 -> NONTERM_boolExpr 
    | 27 -> NONTERM_boolExpr 
    | 28 -> NONTERM_boolExpr 
    | 29 -> NONTERM_boolExpr 
    | _ -> failwith "prodIdxToNonTerminal: bad production index"

let _fsyacc_endOfInputTag = 33 
let _fsyacc_tagOfErrorTerminal = 31

// This function gets the name of a token as a string
let token_to_string (t:token) = 
  match t with 
  | EOF  -> "EOF" 
  | LPAREN  -> "LPAREN" 
  | RPAREN  -> "RPAREN" 
  | LCURLPAREN  -> "LCURLPAREN" 
  | RCURLPAREN  -> "RCURLPAREN" 
  | LSQRPAREN  -> "LSQRPAREN" 
  | RSQRPAREN  -> "RSQRPAREN" 
  | ASN  -> "ASN" 
  | SEMICOLON  -> "SEMICOLON" 
  | SKIP  -> "SKIP" 
  | IF  -> "IF" 
  | THEN  -> "THEN" 
  | ELSE  -> "ELSE" 
  | WHILE  -> "WHILE" 
  | DO  -> "DO" 
  | PLUS  -> "PLUS" 
  | MINUS  -> "MINUS" 
  | MULT  -> "MULT" 
  | DIV  -> "DIV" 
  | TRUE  -> "TRUE" 
  | FALSE  -> "FALSE" 
  | NOT  -> "NOT" 
  | AND  -> "AND" 
  | OR  -> "OR" 
  | LT  -> "LT" 
  | LEQ  -> "LEQ" 
  | EQ  -> "EQ" 
  | GEQ  -> "GEQ" 
  | GT  -> "GT" 
  | Var _ -> "Var" 
  | Number _ -> "Number" 

// This function gets the data carried by a token as an object
let _fsyacc_dataOfToken (t:token) = 
  match t with 
  | EOF  -> (null : System.Object) 
  | LPAREN  -> (null : System.Object) 
  | RPAREN  -> (null : System.Object) 
  | LCURLPAREN  -> (null : System.Object) 
  | RCURLPAREN  -> (null : System.Object) 
  | LSQRPAREN  -> (null : System.Object) 
  | RSQRPAREN  -> (null : System.Object) 
  | ASN  -> (null : System.Object) 
  | SEMICOLON  -> (null : System.Object) 
  | SKIP  -> (null : System.Object) 
  | IF  -> (null : System.Object) 
  | THEN  -> (null : System.Object) 
  | ELSE  -> (null : System.Object) 
  | WHILE  -> (null : System.Object) 
  | DO  -> (null : System.Object) 
  | PLUS  -> (null : System.Object) 
  | MINUS  -> (null : System.Object) 
  | MULT  -> (null : System.Object) 
  | DIV  -> (null : System.Object) 
  | TRUE  -> (null : System.Object) 
  | FALSE  -> (null : System.Object) 
  | NOT  -> (null : System.Object) 
  | AND  -> (null : System.Object) 
  | OR  -> (null : System.Object) 
  | LT  -> (null : System.Object) 
  | LEQ  -> (null : System.Object) 
  | EQ  -> (null : System.Object) 
  | GEQ  -> (null : System.Object) 
  | GT  -> (null : System.Object) 
  | Var _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | Number _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
let _fsyacc_gotos = [| 0us;65535us;1us;65535us;0us;1us;3us;65535us;0us;2us;7us;5us;22us;6us;6us;65535us;0us;4us;7us;4us;14us;15us;16us;17us;20us;21us;22us;4us;18us;65535us;9us;10us;12us;34us;18us;34us;25us;26us;40us;28us;41us;29us;42us;30us;43us;31us;44us;32us;45us;33us;54us;34us;59us;34us;60us;34us;61us;35us;62us;36us;63us;37us;64us;38us;65us;39us;6us;65535us;12us;13us;18us;19us;45us;58us;54us;55us;59us;56us;60us;57us;|]
let _fsyacc_sparseGotoTableRowOffsets = [|0us;1us;3us;7us;14us;33us;|]
let _fsyacc_stateToProdIdxsTableElements = [| 1us;0us;1us;0us;3us;1us;3us;4us;1us;1us;1us;2us;3us;3us;4us;4us;3us;3us;4us;9us;2us;3us;4us;1us;5us;1us;5us;5us;5us;13us;14us;15us;16us;1us;6us;1us;7us;3us;7us;22us;23us;1us;7us;1us;7us;1us;7us;1us;7us;1us;8us;3us;8us;22us;23us;1us;8us;1us;8us;1us;9us;1us;9us;1us;10us;1us;11us;5us;11us;13us;14us;15us;16us;1us;12us;5us;13us;13us;14us;15us;16us;5us;13us;14us;14us;15us;16us;5us;13us;14us;15us;15us;16us;5us;13us;14us;15us;16us;16us;5us;13us;14us;15us;16us;17us;10us;13us;14us;15us;16us;17us;24us;25us;26us;27us;28us;9us;13us;14us;15us;16us;24us;25us;26us;27us;28us;5us;13us;14us;15us;16us;24us;5us;13us;14us;15us;16us;25us;5us;13us;14us;15us;16us;26us;5us;13us;14us;15us;16us;27us;5us;13us;14us;15us;16us;28us;1us;13us;1us;14us;1us;15us;1us;16us;1us;17us;2us;17us;29us;1us;17us;1us;18us;1us;18us;1us;18us;1us;18us;1us;18us;1us;19us;1us;20us;1us;21us;3us;21us;22us;23us;3us;22us;22us;23us;3us;22us;23us;23us;3us;22us;23us;29us;1us;22us;1us;23us;1us;24us;1us;25us;1us;26us;1us;27us;1us;28us;1us;29us;|]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us;2us;4us;8us;10us;12us;16us;20us;23us;25us;27us;33us;35us;37us;41us;43us;45us;47us;49us;51us;55us;57us;59us;61us;63us;65us;67us;73us;75us;81us;87us;93us;99us;105us;116us;126us;132us;138us;144us;150us;156us;158us;160us;162us;164us;166us;169us;171us;173us;175us;177us;179us;181us;183us;185us;187us;191us;195us;199us;203us;205us;207us;209us;211us;213us;215us;217us;|]
let _fsyacc_action_rows = 67
let _fsyacc_actionTableElements = [|5us;32768us;3us;22us;9us;11us;10us;12us;13us;18us;29us;8us;0us;49152us;2us;32768us;0us;3us;8us;7us;0us;16385us;0us;16386us;0us;16388us;2us;32768us;4us;23us;8us;7us;5us;16387us;3us;22us;9us;11us;10us;12us;13us;18us;29us;8us;1us;32768us;7us;9us;5us;32768us;1us;44us;5us;47us;16us;25us;29us;27us;30us;24us;4us;16389us;15us;40us;16us;41us;17us;42us;18us;43us;0us;16390us;8us;32768us;1us;45us;5us;47us;16us;25us;19us;52us;20us;53us;21us;54us;29us;27us;30us;24us;3us;32768us;11us;14us;22us;59us;23us;60us;5us;32768us;3us;22us;9us;11us;10us;12us;13us;18us;29us;8us;1us;32768us;12us;16us;5us;32768us;3us;22us;9us;11us;10us;12us;13us;18us;29us;8us;0us;16391us;8us;32768us;1us;45us;5us;47us;16us;25us;19us;52us;20us;53us;21us;54us;29us;27us;30us;24us;3us;32768us;14us;20us;22us;59us;23us;60us;5us;32768us;3us;22us;9us;11us;10us;12us;13us;18us;29us;8us;0us;16392us;5us;32768us;3us;22us;9us;11us;10us;12us;13us;18us;29us;8us;0us;16393us;0us;16394us;5us;32768us;1us;44us;5us;47us;16us;25us;29us;27us;30us;24us;2us;16395us;17us;42us;18us;43us;0us;16396us;2us;16397us;17us;42us;18us;43us;2us;16398us;17us;42us;18us;43us;0us;16399us;0us;16400us;5us;32768us;2us;46us;15us;40us;16us;41us;17us;42us;18us;43us;10us;32768us;2us;46us;15us;40us;16us;41us;17us;42us;18us;43us;24us;61us;25us;62us;26us;63us;27us;64us;28us;65us;9us;32768us;15us;40us;16us;41us;17us;42us;18us;43us;24us;61us;25us;62us;26us;63us;27us;64us;28us;65us;4us;16408us;15us;40us;16us;41us;17us;42us;18us;43us;4us;16409us;15us;40us;16us;41us;17us;42us;18us;43us;4us;16410us;15us;40us;16us;41us;17us;42us;18us;43us;4us;16411us;15us;40us;16us;41us;17us;42us;18us;43us;4us;16412us;15us;40us;16us;41us;17us;42us;18us;43us;5us;32768us;1us;44us;5us;47us;16us;25us;29us;27us;30us;24us;5us;32768us;1us;44us;5us;47us;16us;25us;29us;27us;30us;24us;5us;32768us;1us;44us;5us;47us;16us;25us;29us;27us;30us;24us;5us;32768us;1us;44us;5us;47us;16us;25us;29us;27us;30us;24us;5us;32768us;1us;44us;5us;47us;16us;25us;29us;27us;30us;24us;8us;32768us;1us;45us;5us;47us;16us;25us;19us;52us;20us;53us;21us;54us;29us;27us;30us;24us;0us;16401us;1us;32768us;30us;48us;1us;32768us;8us;49us;1us;32768us;30us;50us;1us;32768us;6us;51us;0us;16402us;0us;16403us;0us;16404us;8us;32768us;1us;45us;5us;47us;16us;25us;19us;52us;20us;53us;21us;54us;29us;27us;30us;24us;2us;16405us;22us;59us;23us;60us;0us;16406us;1us;16407us;22us;59us;3us;32768us;2us;66us;22us;59us;23us;60us;8us;32768us;1us;45us;5us;47us;16us;25us;19us;52us;20us;53us;21us;54us;29us;27us;30us;24us;8us;32768us;1us;45us;5us;47us;16us;25us;19us;52us;20us;53us;21us;54us;29us;27us;30us;24us;5us;32768us;1us;44us;5us;47us;16us;25us;29us;27us;30us;24us;5us;32768us;1us;44us;5us;47us;16us;25us;29us;27us;30us;24us;5us;32768us;1us;44us;5us;47us;16us;25us;29us;27us;30us;24us;5us;32768us;1us;44us;5us;47us;16us;25us;29us;27us;30us;24us;5us;32768us;1us;44us;5us;47us;16us;25us;29us;27us;30us;24us;0us;16413us;|]
let _fsyacc_actionTableRowOffsets = [|0us;6us;7us;10us;11us;12us;13us;16us;22us;24us;30us;35us;36us;45us;49us;55us;57us;63us;64us;73us;77us;83us;84us;90us;91us;92us;98us;101us;102us;105us;108us;109us;110us;116us;127us;137us;142us;147us;152us;157us;162us;168us;174us;180us;186us;192us;201us;202us;204us;206us;208us;210us;211us;212us;213us;222us;225us;226us;228us;232us;241us;250us;256us;262us;268us;274us;280us;|]
let _fsyacc_reductionSymbolCounts = [|1us;2us;1us;2us;3us;3us;1us;6us;4us;3us;1us;2us;1us;3us;3us;3us;3us;3us;5us;1us;1us;2us;3us;3us;3us;3us;3us;3us;3us;3us;|]
let _fsyacc_productionToNonTerminalTable = [|0us;1us;2us;2us;2us;3us;3us;3us;3us;3us;4us;4us;4us;4us;4us;4us;4us;4us;4us;5us;5us;5us;5us;5us;5us;5us;5us;5us;5us;5us;|]
let _fsyacc_immediateActions = [|65535us;49152us;65535us;16385us;16386us;65535us;65535us;65535us;65535us;65535us;65535us;16390us;65535us;65535us;65535us;65535us;65535us;16391us;65535us;65535us;65535us;16392us;65535us;16393us;16394us;65535us;65535us;16396us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;16401us;65535us;65535us;65535us;65535us;16402us;16403us;16404us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;16413us;|]
let _fsyacc_reductions = lazy [|
# 279 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Ast.Stm in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : 'gentype__startprog));
# 288 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_seqStatement in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 27 "Parser.fsy"
                                                                           _1 
                   )
# 27 "Parser.fsy"
                 : Ast.Stm));
# 299 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_statement in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 30 "Parser.fsy"
                                                                           _1 
                   )
# 30 "Parser.fsy"
                 : 'gentype_seqStatement));
# 310 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_seqStatement in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 31 "Parser.fsy"
                                                                           _1 
                   )
# 31 "Parser.fsy"
                 : 'gentype_seqStatement));
# 321 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_seqStatement in
            let _3 = parseState.GetInput(3) :?> 'gentype_seqStatement in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 32 "Parser.fsy"
                                                                           Seq (_1, _3) 
                   )
# 32 "Parser.fsy"
                 : 'gentype_seqStatement));
# 333 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> string in
            let _3 = parseState.GetInput(3) :?> 'gentype_arithmExpr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 35 "Parser.fsy"
                                                                           VarDec (_1, _3) 
                   )
# 35 "Parser.fsy"
                 : 'gentype_statement));
# 345 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 36 "Parser.fsy"
                                                                           Skip 
                   )
# 36 "Parser.fsy"
                 : 'gentype_statement));
# 355 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = parseState.GetInput(2) :?> 'gentype_boolExpr in
            let _4 = parseState.GetInput(4) :?> 'gentype_statement in
            let _6 = parseState.GetInput(6) :?> 'gentype_statement in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 37 "Parser.fsy"
                                                                           IfThenElse (_2, _4, _6) 
                   )
# 37 "Parser.fsy"
                 : 'gentype_statement));
# 368 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = parseState.GetInput(2) :?> 'gentype_boolExpr in
            let _4 = parseState.GetInput(4) :?> 'gentype_statement in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 38 "Parser.fsy"
                                                                           While (_2, _4) 
                   )
# 38 "Parser.fsy"
                 : 'gentype_statement));
# 380 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = parseState.GetInput(2) :?> 'gentype_seqStatement in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 39 "Parser.fsy"
                                                                           _2 
                   )
# 39 "Parser.fsy"
                 : 'gentype_statement));
# 391 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> int in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 42 "Parser.fsy"
                                                                           Constant (_1) 
                   )
# 42 "Parser.fsy"
                 : 'gentype_arithmExpr));
# 402 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = parseState.GetInput(2) :?> 'gentype_arithmExpr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 43 "Parser.fsy"
                                                                           UnOp ("-", _2) 
                   )
# 43 "Parser.fsy"
                 : 'gentype_arithmExpr));
# 413 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> string in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 44 "Parser.fsy"
                                                                           Variable (_1) 
                   )
# 44 "Parser.fsy"
                 : 'gentype_arithmExpr));
# 424 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_arithmExpr in
            let _3 = parseState.GetInput(3) :?> 'gentype_arithmExpr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 45 "Parser.fsy"
                                                                           BinOp (_1, "+", _3) 
                   )
# 45 "Parser.fsy"
                 : 'gentype_arithmExpr));
# 436 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_arithmExpr in
            let _3 = parseState.GetInput(3) :?> 'gentype_arithmExpr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 46 "Parser.fsy"
                                                                           BinOp (_1, "-", _3) 
                   )
# 46 "Parser.fsy"
                 : 'gentype_arithmExpr));
# 448 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_arithmExpr in
            let _3 = parseState.GetInput(3) :?> 'gentype_arithmExpr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 47 "Parser.fsy"
                                                                           BinOp (_1, "*", _3) 
                   )
# 47 "Parser.fsy"
                 : 'gentype_arithmExpr));
# 460 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_arithmExpr in
            let _3 = parseState.GetInput(3) :?> 'gentype_arithmExpr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 48 "Parser.fsy"
                                                                           BinOp (_1, "/", _3) 
                   )
# 48 "Parser.fsy"
                 : 'gentype_arithmExpr));
# 472 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = parseState.GetInput(2) :?> 'gentype_arithmExpr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 49 "Parser.fsy"
                                                                           _2 
                   )
# 49 "Parser.fsy"
                 : 'gentype_arithmExpr));
# 483 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = parseState.GetInput(2) :?> int in
            let _4 = parseState.GetInput(4) :?> int in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 50 "Parser.fsy"
                                                                           Range (_2, _4) 
                   )
# 50 "Parser.fsy"
                 : 'gentype_arithmExpr));
# 495 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 53 "Parser.fsy"
                                                                           Boolean (true) 
                   )
# 53 "Parser.fsy"
                 : 'gentype_boolExpr));
# 505 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 54 "Parser.fsy"
                                                                           Boolean (false) 
                   )
# 54 "Parser.fsy"
                 : 'gentype_boolExpr));
# 515 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = parseState.GetInput(2) :?> 'gentype_boolExpr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 55 "Parser.fsy"
                                                                           UnOp ("!", _2) 
                   )
# 55 "Parser.fsy"
                 : 'gentype_boolExpr));
# 526 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_boolExpr in
            let _3 = parseState.GetInput(3) :?> 'gentype_boolExpr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 56 "Parser.fsy"
                                                                           BinOp (_1, "&&", _3) 
                   )
# 56 "Parser.fsy"
                 : 'gentype_boolExpr));
# 538 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_boolExpr in
            let _3 = parseState.GetInput(3) :?> 'gentype_boolExpr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 57 "Parser.fsy"
                                                                           BinOp (_1, "||", _3) 
                   )
# 57 "Parser.fsy"
                 : 'gentype_boolExpr));
# 550 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_arithmExpr in
            let _3 = parseState.GetInput(3) :?> 'gentype_arithmExpr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 58 "Parser.fsy"
                                                                           BinOp (_1, "<", _3) 
                   )
# 58 "Parser.fsy"
                 : 'gentype_boolExpr));
# 562 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_arithmExpr in
            let _3 = parseState.GetInput(3) :?> 'gentype_arithmExpr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 59 "Parser.fsy"
                                                                           BinOp (_1, "<=", _3) 
                   )
# 59 "Parser.fsy"
                 : 'gentype_boolExpr));
# 574 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_arithmExpr in
            let _3 = parseState.GetInput(3) :?> 'gentype_arithmExpr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 60 "Parser.fsy"
                                                                           BinOp (_1, "=", _3) 
                   )
# 60 "Parser.fsy"
                 : 'gentype_boolExpr));
# 586 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_arithmExpr in
            let _3 = parseState.GetInput(3) :?> 'gentype_arithmExpr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 61 "Parser.fsy"
                                                                           BinOp (_1, ">=", _3) 
                   )
# 61 "Parser.fsy"
                 : 'gentype_boolExpr));
# 598 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_arithmExpr in
            let _3 = parseState.GetInput(3) :?> 'gentype_arithmExpr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 62 "Parser.fsy"
                                                                           BinOp (_1, ">", _3) 
                   )
# 62 "Parser.fsy"
                 : 'gentype_boolExpr));
# 610 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = parseState.GetInput(2) :?> 'gentype_boolExpr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 63 "Parser.fsy"
                                                                           _2 
                   )
# 63 "Parser.fsy"
                 : 'gentype_boolExpr));
|]
# 622 "Parser.fs"
let tables : FSharp.Text.Parsing.Tables<_> = 
  { reductions = _fsyacc_reductions.Value;
    endOfInputTag = _fsyacc_endOfInputTag;
    tagOfToken = tagOfToken;
    dataOfToken = _fsyacc_dataOfToken; 
    actionTableElements = _fsyacc_actionTableElements;
    actionTableRowOffsets = _fsyacc_actionTableRowOffsets;
    stateToProdIdxsTableElements = _fsyacc_stateToProdIdxsTableElements;
    stateToProdIdxsTableRowOffsets = _fsyacc_stateToProdIdxsTableRowOffsets;
    reductionSymbolCounts = _fsyacc_reductionSymbolCounts;
    immediateActions = _fsyacc_immediateActions;
    gotos = _fsyacc_gotos;
    sparseGotoTableRowOffsets = _fsyacc_sparseGotoTableRowOffsets;
    tagOfErrorTerminal = _fsyacc_tagOfErrorTerminal;
    parseError = (fun (ctxt:FSharp.Text.Parsing.ParseErrorContext<_>) -> 
                              match parse_error_rich with 
                              | Some f -> f ctxt
                              | None -> parse_error ctxt.Message);
    numTerminals = 34;
    productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
let engine lexer lexbuf startState = tables.Interpret(lexer, lexbuf, startState)
let prog lexer lexbuf : Ast.Stm =
    engine lexer lexbuf 0 :?> _
