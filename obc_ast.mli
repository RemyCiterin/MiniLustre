(** Description of obc file *)

open Ast 

type expr = 
    | Econst  of const
    | Eident  of string
    | Epre    of string
    | Eunop   of unop * string
    | Einit   of string * string
    | Eif     of string * string * string
    | Ebinop  of string * binop  * string

and definition = 
    | Dexpr of position * ty      * string * expr (** define a variable with a expression *)
    | Dcall of position * ty list * (string list) * string * (string list) (** Only extern functions like sin cos... *)


and obc = {
    obc_name: string; 
    obc_position: position;
    obc_inputs: (string * ty) list;
    obc_outputs: (string * ty) list;
    obc_variables: (string * ty) list;
    obc_definitions: definition list;
}