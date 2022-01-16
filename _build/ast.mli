type position = Lexing.position * Lexing.position 

type ty = 
    | Tint 
    | Tbool 
    | Tfloat 
    | Tconst_int 
    | Tconst_bool 
    | Tconst_float 

and const = 
    | Cint   of int
    | Cbool  of bool
    | Cfloat of float

and binop = 
    | Band | Bor  | Bxor                      (* boolean binop *)
    | Beq  | Bneq | Blt  | Bgt  | Ble  | Bge  (* boolean and integer binop *)
    | Badd | Bmin | Bdiv | Bmod | Btimes      (* integer binop *)

and unop = 
    | Unot | Uneg

and expr = 
    | Econst   of const
    | Epre     of exprP 
    | Eident   of string
    | Eunop    of unop   * exprP
    | Einit    of exprP  * exprP
    | Ecall    of string * (exprP list)
    | Ebinop   of exprP  * binop * exprP
    | Eif      of exprP  * exprP * exprP
    | Ewith    of exprP  * exprP * exprP


and exprP = {exprP_expr: expr; exprP_position: position}

and definition = 
    | Dcall of position * string list * string * exprP list
    | Dexpr of position * string * exprP 

and node = {
    node_name: string;
    node_position: position;
    node_inputs: (string * ty) list;
    node_outputs: (string * ty) list;
    node_variables: (string * ty) list;
    node_definitions: (string, definition) Hashtbl.t;
}
    
