open Ast 
open Obc_ast 


(** Number of variables introduce by the algorithm at each time *)
let number_intro = ref 0 

(** Introduce a new variable *)
let intro () = 
    let name = "var_" ^ string_of_int !number_intro in 
    number_intro := !number_intro + 1; 
    name 

(** Convert a type (ty) to a string *)
let string_of_ty = function 
    | Tconst_float -> "const float"
    | Tconst_bool  -> "const bool"
    | Tconst_int   -> "const int"
    | Tfloat       -> "float"
    | Tbool        -> "bool"
    | Tint         -> "int"

(** is_const t is true if t is a constant type (like Tconst_int for example) *)
let is_const = function 
    | Tconst_float -> true 
    | Tconst_bool  -> true 
    | Tconst_int   -> true 
    | Tfloat -> false 
    | Tbool  -> false
    | Tint   -> false

(** transform a constant type into a non constant type *)
let from_const = function 
    | Tconst_float -> Tfloat 
    | Tconst_bool  -> Tbool 
    | Tconst_int   -> Tint 
    | x            -> x

(** compatible t1 t2 is true if t1 and t2 are the same type (one can be constant and the other not) *)
let compatible t1 t2 = match (t1, t2) with 
    | (Tconst_float, Tconst_float) -> true
    | (Tfloat, Tconst_float)       -> true
    | (Tconst_float, Tfloat)       -> true
    | (Tfloat, Tfloat)             -> true 

    | (Tconst_bool, Tconst_bool)   -> true
    | (Tbool, Tconst_bool)         -> true
    | (Tconst_bool, Tbool)         -> true
    | (Tbool, Tbool)               -> true 

    | (Tconst_int, Tconst_int)     -> true
    | (Tint, Tconst_int)           -> true
    | (Tconst_int, Tint)           -> true
    | (Tint, Tint)                 -> true 

    | _ -> false

(** Deduce a compatible type with t1 and t2 as constant as the less constant of the two *)
let deduce_ty t1 t2 = 
    if compatible t1 t2 then (
        if not (is_const t1) then t1 else t2
    ) else failwith "les types ne sont pas compatibles"

(** The different types of errors called by the compiler *)
type error = 
    | NotInteger
    | Typage of ty * ty 
    | Arguments of string * int * int
    | TupleLength of string * int * int  
    | Causality 
    | Undefined_variable of string * string 
    | Undefined_node of string 
    | MultipleDef of string * string 
    | NonConst of ty
    | DoubleNode of string 
    | ConstWith 
    | ConstInput

(** Zip two lists to one list *)
let rec zip l1 l2 = match (l1, l2) with 
    | (x :: xs, y :: ys) -> (x, y) :: zip xs ys 
    | ([], _)            -> []
    | (_, [])            -> [] 

(** an error is a position and a type of error (defined above) *)
exception Error of position * error 

(** Transform an error to a string (the error message) *)
let string_of_error = function 
    | Typage (t1, t2) -> "couldn't match expected type ‘"^string_of_ty t1^"’ with actual type ‘"^string_of_ty t2^"’"
    | Arguments(s, i, j) -> 
        begin 
            if i > j then 
                "not enough arguments to unpack: expected `"^string_of_int i^"` arguments, currently `"^string_of_int j^"` arguments are given"
            else
                "too many arguments to unpack: expected `"^string_of_int i^"` arguments, currently `"^string_of_int j^"` arguments are given"
        end 
    | TupleLength(s, i, j) -> 
        begin 
            if i > j then 
                "not enough variables to unpack: expected `"^string_of_int i^"` arguments, currently `"^string_of_int j^"` arguments are given"
            else
                "too many variables to unpack: expected `"^string_of_int i^"` arguments, currently `"^string_of_int j^"` arguments are given"
        end 
    | Causality -> "causality error"
    | NotInteger -> "couldn't match expected type ‘float’ or ‘int’ with actual type ‘bool’"
    | Undefined_variable (var, node) -> "undefined variable `" ^ var ^ "` in the node `" ^ node ^"`"
    | Undefined_node node -> "undefined node `" ^ node ^ "`"
    | MultipleDef (name, node) -> "the variable `"^name^"` is defined several times in the node `"^node^"`"
    | NonConst t -> "const type are illegal in variable definition : " ^ string_of_ty t
    | DoubleNode s -> "the node `"^s^"` have sereval definition"
    | ConstWith -> "non const with ... then ... else ..."
    | ConstInput -> "const input in the matin node"

(** I am cpoied the `iter` library because I could not import it *)
type 'a iterator = ('a -> unit) -> unit 

(** Cons opperation : constant time *)
let (-+) x xs k = k x; xs k

(** Append operation : constant time *)
let (+-) xs x k = xs k; k x

(** Concat operation : constant time *)
let (++) x y k = x k; y k 

(** empty iterator *)
let empty _ = ()

(** singleton x operator *)
let singleton x k = k x 

(** lienar time *)
let rec from_list = function
    | t :: q -> t -+ from_list q 
    | []     -> empty 

(** linear time *)
let to_list seq = 
    let liste = ref [] in 
    seq (fun x -> liste := x :: !liste);
    List.rev !liste 

let string_of_const = function 
    | Cint i -> string_of_int i 
    | Cbool b -> string_of_bool b 
    | Cfloat f -> string_of_float f 

let string_of_obc node = 
    let rec string_of_args = function 
    | (x, t) :: h :: q -> x ^ " : " ^ string_of_ty t ^ "; " ^ string_of_args (h :: q)
    | (x, t) :: []     -> x ^ " : " ^ string_of_ty t
    | []               -> ""
    in 

    let rec string_of_args' = function 
    | x :: h :: q -> x ^ "; " ^ string_of_args' (h :: q)
    | x :: []     -> x
    | []          -> ""
    in 

    let string_of_def = function 
    | Obc_ast.Dexpr(_, _, name, Ebinop(n, Btimes, m)) -> "\t" ^ name ^ " = " ^ n ^ " * " ^ m ^ ";\n"
    | Obc_ast.Dexpr(_, _, name, Ebinop(n, Badd  , m)) -> "\t" ^ name ^ " = " ^ n ^ " + " ^ m ^ ";\n"
    | Obc_ast.Dexpr(_, _, name, Ebinop(n, Bmin  , m)) -> "\t" ^ name ^ " = " ^ n ^ " - " ^ m ^ ";\n"
    | Obc_ast.Dexpr(_, _, name, Ebinop(n, Bdiv  , m)) -> "\t" ^ name ^ " = " ^ n ^ " / " ^ m ^ ";\n"
    | Obc_ast.Dexpr(_, _, name, Ebinop(n, Bmod  , m)) -> "\t" ^ name ^ " = " ^ n ^ " % " ^ m ^ ";\n"
    | Obc_ast.Dexpr(_, _, name, Ebinop(n, Beq   , m)) -> "\t" ^ name ^ " = " ^ n ^ " == " ^ m ^ ";\n"
    | Obc_ast.Dexpr(_, _, name, Ebinop(n, Bneq  , m)) -> "\t" ^ name ^ " = " ^ n ^ " != " ^ m ^ ";\n"
    | Obc_ast.Dexpr(_, _, name, Ebinop(n, Blt   , m)) -> "\t" ^ name ^ " = " ^ n ^ " < " ^ m ^ ";\n"
    | Obc_ast.Dexpr(_, _, name, Ebinop(n, Bgt   , m)) -> "\t" ^ name ^ " = " ^ n ^ " > " ^ m ^ ";\n"
    | Obc_ast.Dexpr(_, _, name, Ebinop(n, Ble   , m)) -> "\t" ^ name ^ " = " ^ n ^ " <= " ^ m ^ ";\n"
    | Obc_ast.Dexpr(_, _, name, Ebinop(n, Bge   , m)) -> "\t" ^ name ^ " = " ^ n ^ " >= " ^ m ^ ";\n"
    | Obc_ast.Dexpr(_, _, name, Ebinop(n, Band  , m)) -> "\t" ^ name ^ " = " ^ n ^ " && " ^ m ^ ";\n"
    | Obc_ast.Dexpr(_, _, name, Ebinop(n, Bor   , m)) -> "\t" ^ name ^ " = " ^ n ^ " || " ^ m ^ ";\n"
    | Obc_ast.Dexpr(_, _, name, Ebinop(n, Bxor  , m)) -> "\t" ^ name ^ " = " ^ n ^ " ^ " ^ m ^ ";\n"

    | Obc_ast.Dexpr(_, _, name, Eunop(Unot, b)) -> "\t" ^ name ^ " = !" ^ b ^ ";\n"
    | Obc_ast.Dexpr(_, _, name, Eunop(Uneg, b)) -> "\t" ^ name ^ " = -" ^ b ^ ";\n"

    | Obc_ast.Dexpr(_, _, name, Epre i) -> "\t" ^ name ^ " = pre " ^ i ^ ";\n"

    | Obc_ast.Dexpr(_, _, name, Einit(i, j)) -> "\t" ^ name ^ " = " ^ i ^ " -> " ^ j ^ ";\n"

    | Obc_ast.Dexpr(_, _, name, Eident(i)) -> "\t" ^ name ^ " = " ^ i ^ ";\n"

    | Obc_ast.Dexpr(_, _, name, Econst(i)) -> "\t" ^ name ^ " = " ^ string_of_const i ^ ";\n"

    | Obc_ast.Dexpr(_, _, name, Eif  (i, j, k)) -> "\t" ^ name ^ " = if "   ^ i ^ " then " ^ j ^ " else "^ k ^ ";\n"

    | Obc_ast.Dcall(_, _, idents, name, args)   -> "\t(" ^ string_of_args' idents ^ ") = "^name^"("^string_of_args' args ^ ");\n"
    in

    "node "^node.obc_name^"("^string_of_args node.obc_inputs ^ ") returns (" ^ string_of_args node.obc_outputs ^ ")\n" ^
    begin
        if List.length node.obc_variables > 0 then 
            "var " ^ string_of_args node.obc_variables ^ "\n"
        else
            ""
    end 
    ^ "let\n" ^ List.fold_right (fun d s -> string_of_def d ^ s) node.obc_definitions "tel\n"

(** Define the state of each expression to make a topological sort *)
type definition_state = 
    | Find    (** The expression is safe for causality check *)
    | View    (** The expression is being observed *)
    | NotView (** The expression has not yet been observed *)


let topo_sort node = 
    let definitions = Hashtbl.create 42 in 

    List.iter (fun def -> match def with 
    | Dexpr(_, _, name, _)      -> Hashtbl.add definitions name def
    | Dcall(_, _, idents, _, _) -> List.iter (fun i -> Hashtbl.add definitions i def) idents
    ) node.obc_definitions;

    let table = Hashtbl.create 42 in 

    let def_iter = ref empty in 

    let rec aux def = 
        try match Hashtbl.find table def with
            | View -> 
                begin
                    let pos = match def with 
                    | Dexpr(p, _, _, _) -> p 
                    | Dcall(p, _, _, _, _) -> p 
                    in


                    raise (Error(pos, Causality))
                end
            | Find -> ()
            | NotView -> 
                begin 
                    Hashtbl.replace table def View;
                    let () = match def with 
                    | Dexpr(_, _, name, Econst(_))       -> def_iter := !def_iter +- def
                    | Dexpr(_, _, name, Epre(s))         -> (def_iter := def -+ !def_iter)
                    | Dexpr(_, _, name, Eident(n))       -> (def_of n; def_iter := !def_iter +- def)
                    | Dexpr(_, _, name, Eunop(_, n))     -> (def_of n; def_iter := !def_iter +- def)
                    | Dexpr(_, _, name, Ebinop(i, _, j)) -> (def_of i; def_of j; def_iter := !def_iter +- def)
                    | Dexpr(_, _, name, Einit(i, j))     -> (def_of i; def_of j; def_iter := !def_iter +- def)
                    | Dexpr(_, _, name, Eif(i, j, k))    -> (def_of i; def_of j; def_of k; def_iter := !def_iter +- def)
                    | Dcall(_, _, idents, _, args)       -> (List.iter def_of args; def_iter := !def_iter +- def)
                    in Hashtbl.replace table def Find; 
                end
        with Not_found -> 
            Hashtbl.replace table def NotView; aux def 
    
    and def_of name = 
        try let d = Hashtbl.find definitions name in aux d 
        with Not_found -> () (* name is an input *)
    in 

    List.iter aux node.obc_definitions; 

    {node with obc_definitions= to_list !def_iter}

(** Simplify an obc node (delete Eident) *)
let simplify node = 
    let definitions = Hashtbl.create 42 in

    let used = Hashtbl.create 42 in 
    List.iter (fun (n, _) -> Hashtbl.replace used n ()) node.obc_outputs;

    let rec aux = function 
    | Dexpr(pos, ty, name, Eident(s)) :: q -> 
        begin 
            let expr = 
                try Hashtbl.find definitions s
                with Not_found -> Eident(s)
            in 
            let def = Dexpr(pos, ty, name, expr) in 
            Hashtbl.add definitions name expr; 
            def :: aux q
        end
    | Dexpr(pos, ty, name, expr) :: q -> 
        begin 
            let () = 
                match expr with 
                | Epre(s) -> Hashtbl.replace used s () 
                | _ -> ()
            in Hashtbl.add definitions name expr; 
            Dexpr(pos, ty, name, expr) :: aux q
        end
    | def :: q -> def :: aux q
    | [] -> []
    in 

    let new_def = aux node.obc_definitions in 

    let rec delete_unused = function 
    | Dexpr(pos, ty, name, expr) :: q when Hashtbl.mem used name -> 
        begin 
            let () = 
            match expr with 
            | Econst _        -> ()
            | Eident i        -> Hashtbl.replace used i ()
            | Eunop(_, i)     -> Hashtbl.replace used i ()
            | Epre(i)         -> Hashtbl.replace used i ()
            | Ebinop(i, _, j) -> begin Hashtbl.replace used i (); Hashtbl.replace used j () end
            | Einit(i, j)     -> begin Hashtbl.replace used i (); Hashtbl.replace used j () end
            | Eif(i, j, k)    -> begin Hashtbl.replace used i (); Hashtbl.replace used j (); Hashtbl.replace used k () end
            in
            Dexpr(pos, ty, name, expr) :: delete_unused q
        end
    | Dcall(pos, ty, idents, name, args) :: q when List.exists (fun name -> Hashtbl.mem used name) idents -> 
        begin 
            List.iter (fun name -> Hashtbl.replace used name ()) args; 
            Dcall(pos, ty, idents, name, args) :: delete_unused q 
        end
    | _ :: q-> delete_unused q
    | [] -> []
    in

    let new_def' = List.rev (delete_unused (List.rev new_def)) in 

    {node with obc_definitions = new_def'; obc_variables= List.filter (fun (n, _) -> Hashtbl.mem used n) node.obc_variables}