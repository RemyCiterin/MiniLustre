(** Transform a lustre file to an obc file (with non-recursive expression) *)

open Ast 
open Obc_ast 
open Utils

(** Number of nodes seen during the compilation *)
let context_number = ref 0

type state_and_expr = definition_state * Obc_ast.definition option

type compiler_state = {
    evaluated: (string, const) Hashtbl.t;                                             (** table of the currently evaluated constants *)
    nodes: (string, Ast.node) Hashtbl.t;                                              (** table of the nodes of the current program *)
    var_of_name: (string, string * (string * int)) Hashtbl.t;                         (** transform a name of obc variable into the original variable and their context *)
    name_of_var: (string * (string * int), string) Hashtbl.t;                         (** transform a variable and their context into a name with intro () *)
    mutable definitions: Obc_ast.definition iterator;                                 (** definitions of the result node *)
    mutable context: string * int;                                                    (** =(name, i) if the actual node of name name is the i-th node seen diring the compilation *)
}

(** Search the definition of a given variable in the current node  *)
let search_definition state position variable = 

    let aux_ty (name, ty) x = if name = variable then Some ty else x in 

    let (name, _) = state.context in 
    
    let node = 
        try 
            Hashtbl.find state.nodes name
        with Not_found -> 
            raise (Error (position, Undefined_node name));
    in

    let variables = node.node_variables @ node.node_inputs @ node.node_outputs in 
    let ty        = match List.fold_right aux_ty  variables None with 
    | None -> raise (Error (position, Undefined_variable(variable, name)));
    | Some(t) -> t 
    in

    try 
        let def = Hashtbl.find node.node_definitions variable in 
        (Some def, ty)
    with Not_found -> 
        (None, ty)

(** Give the name of a variable in the context state.context *)
let get_name state variable = 
    try 
        Hashtbl.find state.name_of_var (variable, state.context)

    with Not_found -> 
        let name = intro () in 
        Hashtbl.add state.name_of_var (variable, state.context) name; 
        Hashtbl.add state.var_of_name name (variable, state.context);
        name 

(** transform an obc defintion of expression into a tuple *)
let match_definition = function 
    | Obc_ast.Dexpr(position, ty, name, expr) -> (position, ty, name, expr)
    | _ -> failwith "match_definition"

(** return the definition and add it's to the current definitions iterator *)
let return_expr state position ty expr = 
    let name = intro () in 
    let def  = Dexpr(position, ty, name, expr) in 
    state.definitions <- state.definitions +- def;
    let () = if is_const ty then match expr with
        | Econst c -> Hashtbl.add state.evaluated name c
        | _ -> failwith "return_expr"
    in def 


(** return the definition of the assiciated variable *)
let rec expr_to_obc state {exprP_expr= expr; exprP_position=position} = match expr with 
    | Ast.Ebinop(e1, b, e2) -> 
        begin 
            let (pos1, t1, name1, expr1) = match_definition (expr_to_obc state e1) in
            let (pos2, t2, name2, expr2) = match_definition (expr_to_obc state e2) in 

            match b with 
            | Band | Bor | Bxor -> 
                begin 
                    if not (compatible t1 Tbool) then
                        raise (Error (pos1, Typage(Tbool, t1)));
                        
                    if not (compatible t2 Tbool) then 
                        raise (Error (pos2, Typage(Tbool, t2)));

                    if is_const t1 && is_const t2 then match (expr1, expr2) with 
                        | (Econst(Cbool b1), Econst(Cbool b2)) -> 
                            begin 
                                match b with 
                                | Band -> return_expr state position Tconst_bool (Econst (Cbool (b1 && b2)))
                                | Bor  -> return_expr state position Tconst_bool (Econst (Cbool (b1 || b2)))
                                | Bxor -> return_expr state position Tconst_bool (Econst (Cbool (b1 <> b2)))
                                | _    -> failwith "ne doit pas arrivé"
                            end 
                        | _ -> failwith "bool binop"
                    else return_expr state position Tbool (Ebinop (name1, b, name2)) 
                            
                end
            
            | Beq | Bneq | Ble | Bge | Blt | Bgt -> 
                begin 
                    if not (compatible t1 t2) then 
                        raise (Error(pos2, Typage(t1, t2)));

                    if is_const t1 && is_const t2 then match (expr1, b, expr2) with 
                        | (Econst i, Beq, Econst j)  -> return_expr state position Tconst_bool (Econst (Cbool (i = j)))
                        | (Econst i, Bneq, Econst j) -> return_expr state position Tconst_bool (Econst (Cbool (i <> j)))
                        | (Econst i, Blt,  Econst j) -> return_expr state position Tconst_bool (Econst (Cbool (i < j)))
                        | (Econst i, Bgt,  Econst j) -> return_expr state position Tconst_bool (Econst (Cbool (i > j)))
                        | (Econst i, Ble,  Econst j) -> return_expr state position Tconst_bool (Econst (Cbool (i <= j)))
                        | (Econst i, Bge,  Econst j) -> return_expr state position Tconst_bool (Econst (Cbool (i >= j)))
                        | _ -> failwith "compare binop" 
                    else return_expr state position Tbool (Ebinop(name1, b, name2)) 

                end
            | Bmod -> 
                begin 
                    if not (compatible t1 Tint) then 
                        raise (Error (pos1, Typage(Tint, t1)));

                    if not (compatible t2 Tint) then 
                        raise (Error (pos2, Typage(Tint, t2)));

                    if is_const t1 && is_const t2 then match (expr1, expr2) with 
                        | (Econst (Cint i), Econst (Cint j)) -> return_expr state position Tconst_int (Econst (Cint (i mod j)))
                        | _ -> failwith "mod" 
                    else return_expr state position (deduce_ty t1 t2) (Ebinop (name1, Bmod, name2)) 
                end 
            | Btimes | Bdiv | Badd | Bmin -> 
                begin 
                    if compatible t1 Tbool then 
                        raise (Error(pos1, Typage(Tfloat, t1)));

                    if compatible t2 Tbool then 
                        raise (Error(pos2, Typage(Tfloat, t2)));

                    let op_int i j = match b with 
                    | Btimes -> i * j | Badd -> i + j | Bdiv -> i / j | Bmin -> i - j | _ -> failwith "ne doit pas arrivé"
                    in 

                    let op_float i j = match b with 
                    | Btimes -> i *. j | Badd -> i +. j | Bdiv -> i /. j | Bmin -> i -. j | _ -> failwith "ne doit pas arrivé"
                    in 

                    if is_const t1 && is_const t2 then match (expr1, expr2) with
                    | (Econst (Cint   i), Econst (Cint   j)) -> return_expr state position Tconst_int   (Econst (Cint (op_int i j)))
                    | (Econst (Cfloat i), Econst (Cfloat j)) -> return_expr state position Tconst_float (Econst (Cfloat (op_float i j)))
                    | (Econst (Cfloat i), Econst (Cint   j)) -> return_expr state position Tconst_float (Econst (Cfloat (op_float i (float_of_int j))))
                    | (Econst (Cint   i), Econst (Cfloat j)) -> return_expr state position Tconst_float (Econst (Cfloat (op_float (float_of_int i) j)))

                    | _ -> failwith "integer binop"
                    else
                        begin 
                            match (from_const t1, from_const t2) with
                            | (Tint, Tint) -> return_expr state position Tint   (Ebinop (name1, b, name2)) 
                            | _            -> return_expr state position Tfloat (Ebinop (name1, b, name2)) 

                        end 

                end 

        end
    | Ast.Eunop(Unot, e) -> 
        begin 
            let (pos, ty, name, expr) = match_definition (expr_to_obc state e) in 

            if not (compatible ty Tbool) then 
                raise (Error (pos, Typage(Tbool, ty)));

            if is_const ty then match expr with
                | Econst (Cbool b) -> return_expr state position Tconst_bool (Econst (Cbool (not b)))
                | _ -> failwith "not" 
            else return_expr state position Tbool (Eunop(Unot, name))
        end
    | Ast.Eunop(Uneg, e) -> 
        begin 
            let (pos, ty, name, expr) = match_definition (expr_to_obc state e) in 

            if not (compatible ty Tint) && not (compatible ty Tfloat) then 
                raise (Error (pos, NotInteger));

            if is_const ty then match expr with
                | Econst (Cfloat f) -> return_expr state position Tconst_float (Econst (Cfloat (-. f)))
                | Econst (Cint i)   -> return_expr state position Tconst_int   (Econst (Cint   (-  i)))
                | _ -> failwith "neg" 
            else return_expr state position ty (Eunop(Uneg, name))
        end
    | Ast.Econst(Cint i) -> 
        return_expr state position Tconst_int (Econst(Cint i))
    | Ast.Econst(Cbool i) -> 
        return_expr state position Tconst_bool (Econst (Cbool i))
    | Ast.Econst(Cfloat i) -> 
        return_expr state position Tconst_float (Econst (Cfloat i))
    
    | Ast.Epre(e) -> 
        begin
            let (pos, ty, name, expr) = match_definition (expr_to_obc state e) in 
            let def = Dexpr(position, from_const ty, intro (), Epre name) in 
            state.definitions <- def -+ state.definitions;
            def
        end
    
    | Ast.Einit(e1, e2) -> 
        begin 
            let (pos1, t1, name1, expr1) = match_definition (expr_to_obc state e1) in
            let (pos2, t2, name2, expr2) = match_definition (expr_to_obc state e2) in 

            if not (compatible t1 t2) then
                raise (Error(pos2, Typage(t1, t2)));

            return_expr state position (from_const t1) (Einit(name1, name2))
        end 
    
    | Ast.Eif(e1, e2, e3) -> 
        begin 
            let (pos1, t1, name1, expr1) = match_definition (expr_to_obc state e1) in
            let (pos2, t2, name2, expr2) = match_definition (expr_to_obc state e2) in 
            let (pos3, t3, name3, expr3) = match_definition (expr_to_obc state e3) in

            if not (compatible t1 Tbool) then 
                raise (Error (pos1, Typage(Tbool, t1)));

            if not (compatible t2 t3) then 
                raise (Error (pos3, Typage(t2, t3)));

            return_expr state position (from_const (deduce_ty t2 t3)) (Eif(name1, name2, name3))

        end 
    
    | Ast.Ewith(e1, e2, e3) ->
        begin 
            let (pos1, t1, name1, expr1) = match_definition (expr_to_obc state e1) in

            if not (compatible t1 Tbool) then
                raise (Error (pos1, Typage(Tbool, t1)));

            if is_const t1 then match expr1 with 
                | Econst (Cbool b) -> if b then expr_to_obc state e2 else expr_to_obc state e3
                | _ -> raise (Error(pos1, ConstWith));
            else 
                begin
                    raise (Error(pos1, ConstWith));
                end
        end
    
    | Eident i -> 
        begin
            let name = get_name state i in
            let (_, ty) = search_definition state position i in 

            try (* i is a constant value *)
                let value = Hashtbl.find state.evaluated name in
                return_expr state position ty (Econst value)
            with Not_found -> (* i is a non constant value *)
                return_expr state position ty (Eident name) 
        end 
    
    | Ecall(node_name, args) -> 
        begin 
            let node = 
                try 
                    Hashtbl.find state.nodes node_name
                with Not_found -> 
                    raise (Error (position, Undefined_node node_name));
            in

            let todo inputs outputs generate_node = 

                if List.length inputs <> List.length args then 
                    raise (Error(position, Arguments(node_name, List.length inputs, List.length args)));

                let (output, t_out) = match outputs with 
                | (output, t_out) :: [] -> (output, t_out)
                | _ -> raise (Error(position, TupleLength(node_name, 1, List.length outputs)));
                in

                let args' = List.map (expr_to_obc state) args in

                List.iter (fun (expr, (name, ty)) -> 
                    let (pos, t, n, e) = match_definition expr in 

                    let () = 
                        try 
                            let c = Hashtbl.find state.evaluated n in
                            Hashtbl.replace state.evaluated name c
                        with Not_found -> 
                            ()
                    in

                    if not (compatible t ty) then 
                        raise (Error(pos, Typage(ty, t)));

                    let def = Dexpr(pos, ty, name, Eident n) in
                    state.definitions <- state.definitions +- def;
                    
                ) (zip args' inputs);

                generate_node (); 

                Some (return_expr state position t_out (Eident (output)))
            in 
            match node_obc state node todo with 
            | None -> failwith ""
            | Some (def) -> def
        end 



(** Make the obc definitions for a given lustre definition *)
and definition_obc' state definition = match definition with 
    | Ast.Dexpr(position, i, expr) -> 
        begin 
            let name = get_name state i in 
            let (_, t1) = search_definition state position i in
            
            let (pos, t2, name', _) = match_definition (expr_to_obc state expr) in 

            if not (compatible t1 t2) then 
                raise (Error(pos, Typage(t1, t2)));

            let def = Dexpr(position, t2, name, Eident name') in 
            state.definitions <- state.definitions +- def; 

        end 
    | Ast.Dcall(position, idents, node_name, args) -> 
        begin 
            let node = 
                try 
                    Hashtbl.find state.nodes node_name
                with Not_found -> 
                    raise (Error (position, Undefined_node node_name));
            in

            let my_node = 
                let (name, _) = state.context in 
                try 
                    Hashtbl.find state.nodes name
                with Not_found -> 
                    raise (Error (position, Undefined_node name));
            in

            
            let todo inputs outputs generate_node = 
                if List.length inputs <> List.length args then 
                    raise (Error(position, Arguments(node_name, List.length inputs, List.length args)));

                if List.length outputs <> List.length idents then 
                    raise (Error(position, TupleLength(node_name, List.length idents, List.length outputs)));

                let args' = List.map (expr_to_obc state) args in

                List.iter (fun (expr, (name, ty)) -> 
                    let (pos, t, n, e) = match_definition expr in 

                    if not (compatible t ty) then 
                        raise (Error(pos, Typage(ty, t)));

                    let () = 
                        try 
                            let c = Hashtbl.find state.evaluated n in
                            Hashtbl.replace state.evaluated name c;
                        with Not_found -> 
                            ()
                    in

                    let def = Dexpr(pos, ty, name, Eident n) in
                    state.definitions <- state.definitions +- def;
                    
                ) (zip args' inputs);

                generate_node ();

                List.iter (fun (name, (out_name, out_ty)) -> 
                    let (_, ty) = List.find (fun (n, _) -> n = name) (my_node.node_variables@my_node.node_inputs@my_node.node_outputs) in 
                    if not (compatible out_ty ty) then
                        raise (Error(position, Typage(out_ty, ty)));

                    let def = Dexpr(position, out_ty, get_name state name, Eident out_name) in
                    state.definitions <- state.definitions +- def;
                ) (zip idents outputs);

                None
            in 
            let _ = node_obc state node todo in ()
        end

and definition_obc =
    let hashtbl = Hashtbl.create 42 in

    let rec aux state def =
        if Hashtbl.mem hashtbl (def, state.context) then () else
            (Hashtbl.replace hashtbl (def, state.context) (); definition_obc' state def)
    in aux

(** check a node and return the list of it's inputs/outputs *)
and node_obc state node todo = 
    let context  = state.context in
    let context' = (node.node_name, !context_number) in 
    context_number := !context_number + 1; 

    state.context <- context';
    let inputs  = List.map (fun (name, ty) -> (get_name state name, ty)) node.node_inputs  in 
    let outputs = List.map (fun (name, ty) -> (get_name state name, ty)) node.node_outputs in 
    state.context <- context;

    let generate_node () = 
        state.context <- context';
        Hashtbl.iter (fun _ def -> definition_obc state def) node.node_definitions;
        state.context <- context;
    in 

    todo inputs outputs generate_node
    

(** transform a lustre node into an obc node *)
let transform_nodes nodes name =

    let state = {
        evaluated= Hashtbl.create 42; 
        nodes= 
            begin 
                let h = Hashtbl.create 42 in 
                List.iter (fun n -> 
                    if Hashtbl.mem h n.node_name then 
                        raise (Error ((List.hd nodes).node_position, DoubleNode n.node_name));
                    Hashtbl.replace h n.node_name n
                ) nodes; 
                h
            end; 
        var_of_name= Hashtbl.create 42; 
        name_of_var= Hashtbl.create 42; 
        (*definitions_state= Hashtbl.create 42;*)
        context= (name, 0);
        definitions= empty; 
    } in 

    

    let node = 
        try 
            Hashtbl.find state.nodes name 
        with Not_found -> 
            raise (Error ((List.hd nodes).node_position, Undefined_node name)) 
    in 

    if List.exists (fun (_, ty) -> is_const ty) node.node_inputs then 
        raise (Error(node.node_position, ConstInput));


    let _ = node_obc state node (fun _ _ gen -> gen (); None) in 

    (*state.context <- (name, 0);*)

    let inputs  = List.map (fun (n, t) -> (get_name state n, t)) node.node_inputs  in 
    let outputs = List.map (fun (n, t) -> (get_name state n, t)) node.node_outputs in 
    let definitions = to_list state.definitions in 

    let variables = List.fold_right (fun def l -> match def with  
    | Dcall(_, ty, name, _, _) -> List.fold_right List.cons (zip name (List.map from_const ty)) l
    | Dexpr(_, ty, name, _)    -> (name, from_const ty) :: l
    ) definitions [] in

    let is_same_name (n, _) (m, _) = n = m in
    let variables' = List.filter (fun x -> not (List.exists (is_same_name x) outputs)) variables in
    
    {
        obc_name= name; 
        obc_inputs= inputs; 
        obc_outputs= outputs; 
        obc_variables= variables';
        obc_position= node.node_position;
        obc_definitions= definitions;  
    }