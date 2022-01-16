open Ast
open Obc_ast 
open Utils 

let lustre4ty (name, ty) = match ty with 
    | Tconst_float -> "const " ^ name ^ " : real"
    | Tfloat       ->            name ^ " : real"
    | Tconst_bool  -> "const " ^ name ^ " : bool"
    | Tbool        ->            name ^ " : bool"
    | Tconst_int   -> "const " ^ name ^ " : int" 
    | Tint         ->            name ^ " : int"

let lustre4string kind2 node = 
    let rec string_of_args = function 
    | (x, t) :: h :: q -> lustre4ty (x, t) ^ "; " ^ string_of_args (h :: q)
    | (x, t) :: []     -> lustre4ty (x, t)
    | []               -> ""
    in 

    let rec string_of_args' = function 
    | x :: h :: q -> x ^ "; " ^ string_of_args' (h :: q)
    | x :: []     -> x
    | []          -> ""
    in 

    let kin2message = if kind2 then "\t--%MAIN;\n\t--%PROPERTY "^ (let (n, _) = List.hd node.obc_outputs in n) ^";\n" else "" in 

    let string_of_def = function 
    | Obc_ast.Dexpr(_, _, name, Ebinop(n, Btimes, m)) -> "\t" ^ name ^ " = " ^ n ^ " * " ^ m ^ ";\n"
    | Obc_ast.Dexpr(_, _, name, Ebinop(n, Badd  , m)) -> "\t" ^ name ^ " = " ^ n ^ " + " ^ m ^ ";\n"
    | Obc_ast.Dexpr(_, _, name, Ebinop(n, Bmin  , m)) -> "\t" ^ name ^ " = " ^ n ^ " - " ^ m ^ ";\n"
    | Obc_ast.Dexpr(_, _, name, Ebinop(n, Bmod  , m)) -> "\t" ^ name ^ " = " ^ n ^ " mod " ^ m ^ ";\n"
    | Obc_ast.Dexpr(_, _, name, Ebinop(n, Beq   , m)) -> "\t" ^ name ^ " = " ^ n ^ " = " ^ m ^ ";\n"
    | Obc_ast.Dexpr(_, _, name, Ebinop(n, Bneq  , m)) -> "\t" ^ name ^ " = " ^ n ^ " <> " ^ m ^ ";\n"
    | Obc_ast.Dexpr(_, _, name, Ebinop(n, Blt   , m)) -> "\t" ^ name ^ " = " ^ n ^ " < " ^ m ^ ";\n"
    | Obc_ast.Dexpr(_, _, name, Ebinop(n, Bgt   , m)) -> "\t" ^ name ^ " = " ^ n ^ " > " ^ m ^ ";\n"
    | Obc_ast.Dexpr(_, _, name, Ebinop(n, Ble   , m)) -> "\t" ^ name ^ " = " ^ n ^ " <= " ^ m ^ ";\n"
    | Obc_ast.Dexpr(_, _, name, Ebinop(n, Bge   , m)) -> "\t" ^ name ^ " = " ^ n ^ " >= " ^ m ^ ";\n"
    | Obc_ast.Dexpr(_, _, name, Ebinop(n, Band  , m)) -> "\t" ^ name ^ " = " ^ n ^ " and " ^ m ^ ";\n"
    | Obc_ast.Dexpr(_, _, name, Ebinop(n, Bor   , m)) -> "\t" ^ name ^ " = " ^ n ^ " or " ^ m ^ ";\n"
    | Obc_ast.Dexpr(_, _, name, Ebinop(n, Bxor  , m)) -> "\t" ^ name ^ " = " ^ n ^ " xor " ^ m ^ ";\n"
    | Obc_ast.Dexpr(_, Tint, name, Ebinop(n, Bdiv  , m)) -> "\t" ^ name ^ " = " ^ n ^ " div " ^ m ^ ";\n"
    | Obc_ast.Dexpr(_, Tfloat, name, Ebinop(n, Bdiv  , m)) -> "\t" ^ name ^ " = " ^ n ^ " / " ^ m ^ ";\n"
    | Obc_ast.Dexpr(_, _, name, Ebinop(n, Bdiv  , m)) -> failwith ""


    | Obc_ast.Dexpr(_, _, name, Eunop(Unot, b)) -> "\t" ^ name ^ " = not " ^ b ^ ";\n"
    | Obc_ast.Dexpr(_, _, name, Eunop(Uneg, b)) -> "\t" ^ name ^ " = -" ^ b ^ ";\n"

    | Obc_ast.Dexpr(_, _, name, Epre i) -> "\t" ^ name ^ " = pre (" ^ i ^ ");\n"

    | Obc_ast.Dexpr(_, _, name, Einit(i, j)) -> "\t" ^ name ^ " = " ^ i ^ " -> " ^ j ^ ";\n"

    | Obc_ast.Dexpr(_, _, name, Eident(i)) -> "\t" ^ name ^ " = " ^ i ^ ";\n"

    | Obc_ast.Dexpr(_, _, name, Econst(i)) -> "\t" ^ name ^ " = " ^ string_of_const i ^ ";\n"

    | Obc_ast.Dexpr(_, _, name, Eif  (i, j, k)) -> "\t" ^ name ^ " = if "   ^ i ^ " then " ^ j ^ " else "^ k ^ ";\n"

    | Obc_ast.Dcall(_, _, idents, name, args)   -> "\t(" ^ string_of_args' idents ^ ") = "^name^"("^string_of_args' args ^ ");\n"
    in

    "node "^node.obc_name^"("^string_of_args node.obc_inputs ^ ") returns (" ^ string_of_args node.obc_outputs ^ ")\n" ^
    begin
        if List.length node.obc_variables > 0 then 
            "var " ^ string_of_args node.obc_variables ^ ";\n"
        else
            ""
    end 
    ^ "let\n" ^ List.fold_right (fun d s -> string_of_def d ^ s) node.obc_definitions kin2message ^ "tel\n"
