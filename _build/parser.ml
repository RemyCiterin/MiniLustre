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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Ast 
open Lexing 
open Parsing 
open Utils

let loc () = symbol_start_pos (), symbol_end_pos ()

let make_expr e = { exprP_expr= e; exprP_position= loc () }

# 58 "parser.ml"
let yytransl_const = [|
  257 (* NODE *);
  258 (* TRUE *);
  259 (* FALSE *);
  260 (* LET *);
  261 (* TEL *);
  262 (* IF *);
  263 (* WITH *);
  264 (* THEN *);
  265 (* ELSE *);
  266 (* RETURNS *);
  267 (* VAR *);
  268 (* CONST *);
  269 (* PRE *);
  270 (* T_INT *);
  271 (* T_BOOL *);
  272 (* T_FLOAT *);
  276 (* L_PAR *);
  277 (* R_PAR *);
  278 (* EQ *);
  279 (* NEQ *);
  280 (* LT *);
  281 (* GT *);
  282 (* GE *);
  283 (* LE *);
  284 (* AND *);
  285 (* OR *);
  286 (* XOR *);
  287 (* NOT *);
  288 (* ADD *);
  289 (* MIN *);
  290 (* DIV *);
  291 (* MOD *);
  292 (* TIMES *);
  293 (* V *);
  294 (* PV *);
  295 (* PP *);
  296 (* AS *);
    0 (* EOF *);
  297 (* INIT *);
    0|]

let yytransl_block = [|
  273 (* IDENT *);
  274 (* INT *);
  275 (* FLOAT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\004\000\004\000\006\000\
\006\000\006\000\007\000\007\000\007\000\007\000\007\000\007\000\
\005\000\005\000\005\000\009\000\009\000\009\000\010\000\010\000\
\010\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
\008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
\008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
\008\000\008\000\008\000\008\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\014\000\012\000\000\000\001\000\003\000\
\005\000\003\000\001\000\001\000\001\000\002\000\002\000\002\000\
\000\000\005\000\010\000\000\000\001\000\003\000\000\000\001\000\
\003\000\003\000\001\000\001\000\001\000\001\000\001\000\004\000\
\006\000\006\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\004\000\002\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\053\000\000\000\000\000\000\000\001\000\
\003\000\000\000\000\000\000\000\007\000\000\000\000\000\000\000\
\010\000\000\000\011\000\012\000\013\000\000\000\000\000\014\000\
\015\000\016\000\000\000\000\000\009\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\005\000\000\000\030\000\031\000\000\000\000\000\000\000\000\000\
\028\000\029\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\052\000\051\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\022\000\000\000\004\000\000\000\000\000\000\000\000\000\000\000\
\026\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\048\000\018\000\
\000\000\000\000\000\000\000\000\050\000\000\000\032\000\000\000\
\000\000\000\000\025\000\000\000\000\000\000\000\000\000\000\000\
\019\000"

let yydgoto = "\002\000\
\004\000\005\000\006\000\012\000\036\000\013\000\022\000\087\000\
\040\000\088\000"

let yysindex = "\006\000\
\009\255\000\000\254\254\000\000\016\000\009\255\012\255\000\000\
\000\000\018\255\015\255\046\255\000\000\018\255\079\255\026\255\
\000\000\013\255\000\000\000\000\000\000\054\255\077\255\000\000\
\000\000\000\000\018\255\018\255\000\000\080\255\001\255\245\254\
\018\255\083\255\100\255\119\255\121\255\207\000\089\255\106\255\
\000\000\245\254\000\000\000\000\207\000\207\000\128\255\134\255\
\000\000\000\000\207\000\207\000\207\000\011\001\100\255\092\255\
\150\255\192\255\215\255\207\000\207\000\226\000\000\000\000\000\
\207\000\207\000\207\000\207\000\207\000\207\000\207\000\207\000\
\207\000\207\000\207\000\207\000\207\000\207\000\245\254\207\000\
\000\000\139\255\000\000\207\000\207\000\247\000\031\001\136\255\
\000\000\027\255\027\255\027\255\027\255\027\255\027\255\086\001\
\071\001\101\001\030\255\030\255\125\255\125\255\000\000\000\000\
\051\001\138\255\149\000\171\000\000\000\207\000\000\000\207\000\
\207\000\207\000\000\000\141\255\051\001\051\001\144\255\245\254\
\000\000"

let yyrindex = "\000\000\
\163\000\000\000\000\000\000\000\000\000\163\000\000\000\000\000\
\000\000\153\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\255\254\000\000\000\000\
\000\000\000\000\000\000\153\255\000\000\000\000\000\000\174\255\
\179\255\000\000\164\255\000\000\000\000\000\000\165\255\000\000\
\000\000\174\255\000\000\000\000\000\000\000\000\000\000\047\255\
\000\000\000\000\000\000\000\000\000\000\000\000\164\255\000\000\
\000\000\000\000\000\000\000\000\166\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\174\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\167\255\000\000\
\000\000\244\255\011\000\034\000\057\000\080\000\103\000\114\000\
\128\000\249\254\143\255\169\255\081\255\112\255\000\000\000\000\
\091\255\000\000\000\000\000\000\000\000\166\255\000\000\166\255\
\000\000\000\000\000\000\000\000\122\255\208\000\000\000\174\255\
\000\000"

let yygindex = "\000\000\
\000\000\183\000\000\000\236\255\214\255\246\255\000\000\229\255\
\148\000\242\255"

let yytablesize = 649
let yytable = "\057\000\
\036\000\036\000\008\000\017\000\032\000\034\000\001\000\030\000\
\035\000\003\000\054\000\033\000\037\000\036\000\007\000\008\000\
\029\000\058\000\059\000\008\000\036\000\036\000\036\000\062\000\
\063\000\064\000\024\000\025\000\026\000\036\000\036\000\010\000\
\086\000\036\000\011\000\023\000\104\000\090\000\091\000\092\000\
\093\000\094\000\095\000\096\000\097\000\098\000\099\000\100\000\
\101\000\102\000\103\000\014\000\105\000\015\000\027\000\027\000\
\107\000\108\000\074\000\075\000\076\000\077\000\078\000\076\000\
\077\000\078\000\016\000\027\000\027\000\027\000\027\000\027\000\
\027\000\027\000\027\000\027\000\027\000\121\000\027\000\027\000\
\027\000\027\000\027\000\027\000\027\000\117\000\118\000\027\000\
\046\000\046\000\018\000\027\000\019\000\020\000\021\000\115\000\
\028\000\116\000\049\000\049\000\031\000\046\000\046\000\046\000\
\046\000\046\000\046\000\046\000\046\000\046\000\046\000\049\000\
\046\000\046\000\046\000\046\000\039\000\046\000\046\000\047\000\
\047\000\046\000\038\000\041\000\042\000\055\000\056\000\049\000\
\049\000\033\000\033\000\082\000\047\000\047\000\047\000\047\000\
\047\000\047\000\047\000\047\000\047\000\047\000\033\000\047\000\
\047\000\047\000\047\000\060\000\047\000\047\000\044\000\044\000\
\047\000\061\000\083\000\106\000\111\000\112\000\033\000\033\000\
\078\000\119\000\002\000\044\000\044\000\044\000\044\000\044\000\
\044\000\044\000\044\000\044\000\044\000\006\000\044\000\044\000\
\045\000\045\000\017\000\044\000\044\000\120\000\006\000\044\000\
\020\000\021\000\023\000\024\000\009\000\045\000\045\000\045\000\
\045\000\045\000\045\000\045\000\045\000\045\000\045\000\084\000\
\045\000\045\000\081\000\000\000\000\000\045\000\045\000\000\000\
\000\000\045\000\000\000\000\000\000\000\065\000\066\000\067\000\
\068\000\069\000\070\000\071\000\072\000\073\000\085\000\074\000\
\075\000\076\000\077\000\078\000\000\000\000\000\000\000\000\000\
\080\000\000\000\000\000\000\000\065\000\066\000\067\000\068\000\
\069\000\070\000\071\000\072\000\073\000\000\000\074\000\075\000\
\076\000\077\000\078\000\039\000\039\000\000\000\000\000\080\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\039\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
\039\000\039\000\038\000\038\000\000\000\000\000\000\000\000\000\
\039\000\039\000\000\000\000\000\039\000\000\000\000\000\038\000\
\038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
\038\000\040\000\040\000\000\000\000\000\000\000\000\000\038\000\
\038\000\000\000\000\000\038\000\000\000\000\000\040\000\040\000\
\040\000\040\000\040\000\040\000\040\000\040\000\040\000\040\000\
\041\000\041\000\000\000\000\000\000\000\000\000\040\000\040\000\
\000\000\000\000\040\000\000\000\000\000\041\000\041\000\041\000\
\041\000\041\000\041\000\041\000\041\000\041\000\041\000\043\000\
\043\000\000\000\000\000\000\000\000\000\041\000\041\000\000\000\
\000\000\041\000\000\000\000\000\043\000\043\000\043\000\043\000\
\043\000\043\000\043\000\043\000\043\000\043\000\042\000\042\000\
\000\000\000\000\000\000\000\000\043\000\043\000\000\000\000\000\
\043\000\035\000\035\000\042\000\042\000\042\000\042\000\042\000\
\042\000\042\000\042\000\042\000\042\000\000\000\035\000\037\000\
\037\000\000\000\000\000\042\000\042\000\035\000\035\000\042\000\
\000\000\000\000\000\000\000\000\037\000\000\000\035\000\035\000\
\000\000\000\000\035\000\000\000\037\000\113\000\000\000\000\000\
\000\000\000\000\000\000\000\000\037\000\037\000\000\000\000\000\
\037\000\000\000\065\000\066\000\067\000\068\000\069\000\070\000\
\071\000\072\000\073\000\114\000\074\000\075\000\076\000\077\000\
\078\000\000\000\000\000\000\000\000\000\080\000\000\000\000\000\
\065\000\066\000\067\000\068\000\069\000\070\000\071\000\072\000\
\073\000\000\000\074\000\075\000\076\000\077\000\078\000\000\000\
\043\000\044\000\000\000\080\000\045\000\046\000\000\000\034\000\
\034\000\000\000\000\000\047\000\000\000\000\000\000\000\048\000\
\049\000\050\000\051\000\000\000\034\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\052\000\000\000\053\000\
\000\000\000\000\000\000\000\000\034\000\034\000\089\000\065\000\
\066\000\067\000\068\000\069\000\070\000\071\000\072\000\073\000\
\000\000\074\000\075\000\076\000\077\000\078\000\000\000\000\000\
\000\000\000\000\080\000\109\000\065\000\066\000\067\000\068\000\
\069\000\070\000\071\000\072\000\073\000\000\000\074\000\075\000\
\076\000\077\000\078\000\000\000\000\000\000\000\000\000\080\000\
\065\000\066\000\067\000\068\000\069\000\070\000\071\000\072\000\
\073\000\000\000\074\000\075\000\076\000\077\000\078\000\000\000\
\079\000\000\000\000\000\080\000\065\000\066\000\067\000\068\000\
\069\000\070\000\071\000\072\000\073\000\000\000\074\000\075\000\
\076\000\077\000\078\000\110\000\000\000\000\000\000\000\080\000\
\065\000\066\000\067\000\068\000\069\000\070\000\071\000\072\000\
\073\000\000\000\074\000\075\000\076\000\077\000\078\000\000\000\
\000\000\000\000\000\000\080\000\065\000\066\000\067\000\068\000\
\069\000\070\000\071\000\000\000\073\000\000\000\074\000\075\000\
\076\000\077\000\078\000\065\000\066\000\067\000\068\000\069\000\
\070\000\000\000\000\000\073\000\000\000\074\000\075\000\076\000\
\077\000\078\000\065\000\066\000\067\000\068\000\069\000\070\000\
\000\000\000\000\000\000\000\000\074\000\075\000\076\000\077\000\
\078\000"

let yycheck = "\042\000\
\008\001\009\001\004\001\014\000\004\001\017\001\001\000\028\000\
\020\001\001\001\038\000\011\001\033\000\021\001\017\001\000\000\
\027\000\045\000\046\000\021\001\028\001\029\001\030\001\051\000\
\052\000\053\000\014\001\015\001\016\001\037\001\038\001\020\001\
\060\000\041\001\017\001\010\001\079\000\065\000\066\000\067\000\
\068\000\069\000\070\000\071\000\072\000\073\000\074\000\075\000\
\076\000\077\000\078\000\037\001\080\000\039\001\008\001\009\001\
\084\000\085\000\032\001\033\001\034\001\035\001\036\001\034\001\
\035\001\036\001\021\001\021\001\022\001\023\001\024\001\025\001\
\026\001\027\001\028\001\029\001\030\001\120\000\032\001\033\001\
\034\001\035\001\036\001\037\001\038\001\113\000\114\000\041\001\
\008\001\009\001\012\001\038\001\014\001\015\001\016\001\110\000\
\020\001\112\000\008\001\009\001\021\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\029\001\030\001\021\001\
\032\001\033\001\034\001\035\001\017\001\037\001\038\001\008\001\
\009\001\041\001\040\001\005\001\004\001\037\001\021\001\037\001\
\038\001\008\001\009\001\040\001\021\001\022\001\023\001\024\001\
\025\001\026\001\027\001\028\001\029\001\030\001\021\001\032\001\
\033\001\034\001\035\001\020\001\037\001\038\001\008\001\009\001\
\041\001\020\001\005\001\017\001\021\001\020\001\037\001\038\001\
\036\001\021\001\000\000\021\001\022\001\023\001\024\001\025\001\
\026\001\027\001\028\001\029\001\030\001\021\001\032\001\033\001\
\008\001\009\001\005\001\037\001\038\001\038\001\004\001\041\001\
\021\001\021\001\021\001\021\001\006\000\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\029\001\030\001\008\001\
\032\001\033\001\055\000\255\255\255\255\037\001\038\001\255\255\
\255\255\041\001\255\255\255\255\255\255\022\001\023\001\024\001\
\025\001\026\001\027\001\028\001\029\001\030\001\008\001\032\001\
\033\001\034\001\035\001\036\001\255\255\255\255\255\255\255\255\
\041\001\255\255\255\255\255\255\022\001\023\001\024\001\025\001\
\026\001\027\001\028\001\029\001\030\001\255\255\032\001\033\001\
\034\001\035\001\036\001\008\001\009\001\255\255\255\255\041\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\021\001\022\001\023\001\024\001\025\001\026\001\027\001\028\001\
\029\001\030\001\008\001\009\001\255\255\255\255\255\255\255\255\
\037\001\038\001\255\255\255\255\041\001\255\255\255\255\021\001\
\022\001\023\001\024\001\025\001\026\001\027\001\028\001\029\001\
\030\001\008\001\009\001\255\255\255\255\255\255\255\255\037\001\
\038\001\255\255\255\255\041\001\255\255\255\255\021\001\022\001\
\023\001\024\001\025\001\026\001\027\001\028\001\029\001\030\001\
\008\001\009\001\255\255\255\255\255\255\255\255\037\001\038\001\
\255\255\255\255\041\001\255\255\255\255\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\029\001\030\001\008\001\
\009\001\255\255\255\255\255\255\255\255\037\001\038\001\255\255\
\255\255\041\001\255\255\255\255\021\001\022\001\023\001\024\001\
\025\001\026\001\027\001\028\001\029\001\030\001\008\001\009\001\
\255\255\255\255\255\255\255\255\037\001\038\001\255\255\255\255\
\041\001\008\001\009\001\021\001\022\001\023\001\024\001\025\001\
\026\001\027\001\028\001\029\001\030\001\255\255\021\001\008\001\
\009\001\255\255\255\255\037\001\038\001\028\001\029\001\041\001\
\255\255\255\255\255\255\255\255\021\001\255\255\037\001\038\001\
\255\255\255\255\041\001\255\255\029\001\009\001\255\255\255\255\
\255\255\255\255\255\255\255\255\037\001\038\001\255\255\255\255\
\041\001\255\255\022\001\023\001\024\001\025\001\026\001\027\001\
\028\001\029\001\030\001\009\001\032\001\033\001\034\001\035\001\
\036\001\255\255\255\255\255\255\255\255\041\001\255\255\255\255\
\022\001\023\001\024\001\025\001\026\001\027\001\028\001\029\001\
\030\001\255\255\032\001\033\001\034\001\035\001\036\001\255\255\
\002\001\003\001\255\255\041\001\006\001\007\001\255\255\008\001\
\009\001\255\255\255\255\013\001\255\255\255\255\255\255\017\001\
\018\001\019\001\020\001\255\255\021\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\031\001\255\255\033\001\
\255\255\255\255\255\255\255\255\037\001\038\001\021\001\022\001\
\023\001\024\001\025\001\026\001\027\001\028\001\029\001\030\001\
\255\255\032\001\033\001\034\001\035\001\036\001\255\255\255\255\
\255\255\255\255\041\001\021\001\022\001\023\001\024\001\025\001\
\026\001\027\001\028\001\029\001\030\001\255\255\032\001\033\001\
\034\001\035\001\036\001\255\255\255\255\255\255\255\255\041\001\
\022\001\023\001\024\001\025\001\026\001\027\001\028\001\029\001\
\030\001\255\255\032\001\033\001\034\001\035\001\036\001\255\255\
\038\001\255\255\255\255\041\001\022\001\023\001\024\001\025\001\
\026\001\027\001\028\001\029\001\030\001\255\255\032\001\033\001\
\034\001\035\001\036\001\037\001\255\255\255\255\255\255\041\001\
\022\001\023\001\024\001\025\001\026\001\027\001\028\001\029\001\
\030\001\255\255\032\001\033\001\034\001\035\001\036\001\255\255\
\255\255\255\255\255\255\041\001\022\001\023\001\024\001\025\001\
\026\001\027\001\028\001\255\255\030\001\255\255\032\001\033\001\
\034\001\035\001\036\001\022\001\023\001\024\001\025\001\026\001\
\027\001\255\255\255\255\030\001\255\255\032\001\033\001\034\001\
\035\001\036\001\022\001\023\001\024\001\025\001\026\001\027\001\
\255\255\255\255\255\255\255\255\032\001\033\001\034\001\035\001\
\036\001"

let yynames_const = "\
  NODE\000\
  TRUE\000\
  FALSE\000\
  LET\000\
  TEL\000\
  IF\000\
  WITH\000\
  THEN\000\
  ELSE\000\
  RETURNS\000\
  VAR\000\
  CONST\000\
  PRE\000\
  T_INT\000\
  T_BOOL\000\
  T_FLOAT\000\
  L_PAR\000\
  R_PAR\000\
  EQ\000\
  NEQ\000\
  LT\000\
  GT\000\
  GE\000\
  LE\000\
  AND\000\
  OR\000\
  XOR\000\
  NOT\000\
  ADD\000\
  MIN\000\
  DIV\000\
  MOD\000\
  TIMES\000\
  V\000\
  PV\000\
  PP\000\
  AS\000\
  EOF\000\
  INIT\000\
  "

let yynames_block = "\
  IDENT\000\
  INT\000\
  FLOAT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'node_list) in
    Obj.repr(
# 86 "parser.mly"
                    (_1)
# 411 "parser.ml"
               : Ast.node list))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "parser.mly"
                  ([])
# 417 "parser.ml"
               : 'node_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'node) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'node_list) in
    Obj.repr(
# 91 "parser.mly"
                     (_1 :: _2)
# 425 "parser.ml"
               : 'node_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 12 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 10 : 'param_star) in
    let _8 = (Parsing.peek_val __caml_parser_env 6 : 'param_star) in
    let _11 = (Parsing.peek_val __caml_parser_env 3 : 'param_star) in
    let _13 = (Parsing.peek_val __caml_parser_env 1 : 'assigns) in
    Obj.repr(
# 95 "parser.mly"
                                                                                                       (
        let definitions = Hashtbl.create 42 in

        let add_def p def n = 
            if Hashtbl.mem definitions n then 
                raise (Error(p, MultipleDef(n, _2)));
            Hashtbl.add definitions n def 
        in

        List.iter (fun def -> match def with 
        | Dexpr(p, name, _) -> add_def p def name
        | Dcall(p, idents, _, _) -> List.iter (add_def p def) idents
        ) _13;

        let variables = Hashtbl.create 42 in 

        List.iter (fun (name, ty) -> 
            if Hashtbl.mem definitions name then 
                raise (Error(loc (), MultipleDef(name, _2))); 
            
            if Hashtbl.mem variables name then 
                raise (Error(loc (), MultipleDef(name, _2))); 
            Hashtbl.add variables name ty; 
        ) _4;

        List.iter (fun (name, ty) -> 
            if not (Hashtbl.mem definitions name) then 
                raise (Error(loc (), Undefined_variable(name, _2))); 

            if is_const ty then 
                raise (Error(loc (), NonConst ty));
            
            if Hashtbl.mem variables name then
                raise (Error(loc (), MultipleDef(name, _2))); 
            Hashtbl.add variables name ty; 
        ) (_8 @ _11);

        {node_name= _2; node_position= loc (); node_inputs= _4; node_outputs= _8; node_variables= _11; node_definitions= definitions}
    )
# 474 "parser.ml"
               : 'node))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 10 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 8 : 'param_star) in
    let _8 = (Parsing.peek_val __caml_parser_env 4 : 'param_star) in
    let _11 = (Parsing.peek_val __caml_parser_env 1 : 'assigns) in
    Obj.repr(
# 135 "parser.mly"
                                                                                        (
        let definitions = Hashtbl.create 42 in

       let add_def p def n = 
            if Hashtbl.mem definitions n then 
                raise (Error(p, MultipleDef(n, _2)));
            Hashtbl.add definitions n def 
        in

        List.iter (fun def -> match def with 
        | Dexpr(p, name, _) -> add_def p def name
        | Dcall(p, idents, _, _) -> List.iter (add_def p def) idents
        ) _11;

        let variables = Hashtbl.create 42 in 

        List.iter (fun (name, ty) -> 
            if Hashtbl.mem definitions name then 
                raise (Error(loc (), MultipleDef(name, _2))); 
            
            if Hashtbl.mem variables name then 
                raise (Error(loc (), MultipleDef(name, _2))); 
            Hashtbl.add variables name ty; 
        ) _4;

        List.iter (fun (name, ty) -> 
            if not (Hashtbl.mem definitions name) then 
                raise (Error(loc (), Undefined_variable(name, _2))); 

            if is_const ty then 
                raise (Error(loc (), NonConst ty));
            
            if Hashtbl.mem variables name then
                raise (Error(loc (), MultipleDef(name, _2))); 
            Hashtbl.add variables name ty; 
        ) _8;

        {node_name= _2; node_position= loc (); node_inputs= _4; node_outputs= _8; node_variables= []; node_definitions= definitions}
    )
# 522 "parser.ml"
               : 'node))
; (fun __caml_parser_env ->
    Obj.repr(
# 177 "parser.mly"
                  ([])
# 528 "parser.ml"
               : 'param_star))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'param_plus) in
    Obj.repr(
# 178 "parser.mly"
                  (_1)
# 535 "parser.ml"
               : 'param_star))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 182 "parser.mly"
                                ([(_1, _3)])
# 543 "parser.ml"
               : 'param_plus))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'param_plus) in
    Obj.repr(
# 183 "parser.mly"
                                ((_1 , _3) :: _5)
# 552 "parser.ml"
               : 'param_plus))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'param_plus) in
    Obj.repr(
# 184 "parser.mly"
                                (let (s, t) = List.hd _3 in (_1, t) :: _3)
# 560 "parser.ml"
               : 'param_plus))
; (fun __caml_parser_env ->
    Obj.repr(
# 188 "parser.mly"
              (Tint)
# 566 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    Obj.repr(
# 189 "parser.mly"
              (Tbool)
# 572 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    Obj.repr(
# 190 "parser.mly"
              (Tfloat)
# 578 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    Obj.repr(
# 191 "parser.mly"
                  (Tconst_int)
# 584 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    Obj.repr(
# 192 "parser.mly"
                   (Tconst_bool)
# 590 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    Obj.repr(
# 193 "parser.mly"
                    (Tconst_float)
# 596 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    Obj.repr(
# 196 "parser.mly"
                                                      ([])
# 602 "parser.ml"
               : 'assigns))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'assigns) in
    Obj.repr(
# 197 "parser.mly"
                                                      (Dexpr(loc (), _1, _3) :: _5)
# 611 "parser.ml"
               : 'assigns))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 8 : 'tupleIdent) in
    let _5 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 3 : 'args) in
    let _10 = (Parsing.peek_val __caml_parser_env 0 : 'assigns) in
    Obj.repr(
# 198 "parser.mly"
                                                                  (Dcall(loc (), _2, _5, _7) :: _10)
# 621 "parser.ml"
               : 'assigns))
; (fun __caml_parser_env ->
    Obj.repr(
# 202 "parser.mly"
                         ([])
# 627 "parser.ml"
               : 'tupleIdent))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 203 "parser.mly"
                         ([_1])
# 634 "parser.ml"
               : 'tupleIdent))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'tupleIdent) in
    Obj.repr(
# 204 "parser.mly"
                         (_1 :: _3)
# 642 "parser.ml"
               : 'tupleIdent))
; (fun __caml_parser_env ->
    Obj.repr(
# 208 "parser.mly"
                  ([])
# 648 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 209 "parser.mly"
           ([_1])
# 655 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 210 "parser.mly"
                  (_1 :: _3)
# 663 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 213 "parser.mly"
                       (_2)
# 670 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 214 "parser.mly"
            (make_expr (Eident _1))
# 677 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 215 "parser.mly"
            (make_expr (Econst (Cint _1)))
# 684 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 216 "parser.mly"
            (make_expr (Econst (Cfloat _1)))
# 691 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 217 "parser.mly"
            (make_expr (Econst (Cbool true)))
# 697 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 218 "parser.mly"
            (make_expr (Econst (Cbool false)))
# 703 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args) in
    Obj.repr(
# 220 "parser.mly"
                             (make_expr (Ecall (_1, _3)))
# 711 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 222 "parser.mly"
                                    (make_expr (Eif   (_2, _4, _6)))
# 720 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 223 "parser.mly"
                                    (make_expr (Ewith (_2, _4, _6)))
# 729 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 225 "parser.mly"
                    (make_expr (Ebinop (_1, Band, _3)))
# 737 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 226 "parser.mly"
                    (make_expr (Ebinop (_1, Bxor, _3)))
# 745 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 227 "parser.mly"
                    (make_expr (Ebinop (_1, Bor,  _3)))
# 753 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 229 "parser.mly"
                    (make_expr (Ebinop (_1, Bneq, _3)))
# 761 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 230 "parser.mly"
                    (make_expr (Ebinop (_1, Beq,  _3)))
# 769 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 231 "parser.mly"
                    (make_expr (Ebinop (_1, Blt,  _3)))
# 777 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 232 "parser.mly"
                    (make_expr (Ebinop (_1, Bgt,  _3)))
# 785 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 233 "parser.mly"
                    (make_expr (Ebinop (_1, Ble,  _3)))
# 793 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 234 "parser.mly"
                    (make_expr (Ebinop (_1, Bge,  _3)))
# 801 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 236 "parser.mly"
                    (make_expr (Ebinop (_1, Badd, _3)))
# 809 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 237 "parser.mly"
                    (make_expr (Ebinop (_1, Bmin, _3)))
# 817 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 238 "parser.mly"
                    (make_expr (Ebinop (_1, Bdiv, _3)))
# 825 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 239 "parser.mly"
                    (make_expr (Ebinop (_1, Bmod, _3)))
# 833 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 240 "parser.mly"
                      (make_expr (Ebinop (_1, Btimes, _3)))
# 841 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 242 "parser.mly"
                     (make_expr (Einit (_1, _3)))
# 849 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 243 "parser.mly"
                           (make_expr (Epre _3))
# 856 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 245 "parser.mly"
                          (make_expr (Eunop (Uneg, _2)))
# 863 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 246 "parser.mly"
                          (make_expr (Eunop (Unot, _2)))
# 870 "parser.ml"
               : 'expr))
(* Entry file *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let file (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.node list)
