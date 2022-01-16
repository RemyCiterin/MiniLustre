type token =
  | NODE
  | TRUE
  | FALSE
  | LET
  | TEL
  | IF
  | WITH
  | THEN
  | ELSE
  | RETURNS
  | VAR
  | CONST
  | PRE
  | T_INT
  | T_BOOL
  | T_FLOAT
  | IDENT of (string)
  | INT of (int)
  | FLOAT of (float)
  | L_PAR
  | R_PAR
  | EQ
  | NEQ
  | LT
  | GT
  | GE
  | LE
  | AND
  | OR
  | XOR
  | NOT
  | ADD
  | MIN
  | DIV
  | MOD
  | TIMES
  | V
  | PV
  | PP
  | AS
  | EOF
  | INIT

val file :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.node list
