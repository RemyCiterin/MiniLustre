open Obc_ast

open BDD

(** model checker using BDD *)

(** Represent a program in terme of 'a (bdd or Obc_ast.definition) *)
type ('a, 'b) program = {
    inputs : 'b list; 
    register : 'b list;
    formula : ('b, 'a) Hashtbl.t;
}

(** transform a Obc_ast.obc into a Obc_ast.definition program *)
let rec to_program (obc : Obc_ast.definition list) : (Obc_ast.definition, string) program = match obc with 
    | t :: q -> begin 
        let prog = to_program q in 
        
        let reg = match t with 
        | Dexpr(pos, ty, ident, Epre(arg)) -> arg :: prog.register
        | _ -> prog.register 
        in
        
        let () = match t with 
        | Dcall(pos, ty, idents, name, args) -> List.iter (fun n -> Hashtbl.add prog.formula n t) idents 
        | Dexpr(pos, ty, ident, _) -> Hashtbl.add prog.formula ident t 
        in
        
        {
            inputs   = [];
            register = reg;
            formula  = prog.formula;
        }

    end 
    | [] -> {
        register = []; formula = Hashtbl.create 42; inputs = []
    }


(** Initial value of memoization table for BDD *)
let init_table : global_table = 
    let map0 = MapAVL.add compareNode BinTree.Leaf Zero {tag=Prims.of_int 0; node=Zero} in 
    let map1 = MapAVL.add compareNode map0         One  {tag=Prims.of_int 1; node=One}  in
    {
        size=Prims.of_int 2; 
        map =map1 
    }

let zero = {tag=Prims.of_int 0; node=Zero}
let one  = {tag=Prims.of_int 1; node=One}

let rec bdd_to_string (b : BDD.bdd) : string = match b.node with 
    | Zero -> "F"
    | One  -> "T"
    | Node (l, v, h) -> 
        "(" ^ bdd_to_string l ^ " <- " ^ Z.to_string v ^ " -> " ^ bdd_to_string h ^ ")"


(** transform a Obc_ast.definition program to an object of the type global_table * bdd program *)
let rec program_to_bdd (program : (Obc_ast.definition, string) program) : (string, int) Hashtbl.t * global_table * (bdd, Prims.int) program * (bdd, Prims.int) program =

    let str_to_int_table = Hashtbl.create 42 in 
    let var_num = ref 0 in 

    let table   = ref init_table in

    let prog_1 = program_to_bdd_with str_to_int_table var_num table true  program in 
    let prog_2 = program_to_bdd_with str_to_int_table var_num table false program in 

    Hashtbl.iter (fun name num -> 
        print_string (name ^ " : " ^ string_of_int num ^ "\n")
    ) str_to_int_table;
    

    (str_to_int_table, !table, prog_1, prog_2)


and program_to_bdd_with str_to_int_table var_num table is_init program = 
    let formula = Hashtbl.create 42 in
    let inputs  = Hashtbl.create 42 in 

    let str_to_int : string -> Prims.int = 

        let aux (name: string) : int =
            try 
                Hashtbl.find str_to_int_table name
            with Not_found -> 
                let _ = 
                    Hashtbl.replace str_to_int_table name !var_num;
                    var_num := !var_num+1
                in !var_num - 1
        in

        fun i -> Prims.of_int (aux i)
    in
    

    let register0 = List.filter (fun x-> 
        match Hashtbl.find program.formula x with
        | Dcall(pos, ty, idents, name, args) -> begin 
            let (_, t) = (List.find (fun (i, t) -> i = x) (List.combine idents ty)) in 
            Utils.compatible t Ast.Tbool 
        end 
        | Dexpr(_, ty, _, _) -> Utils.compatible ty Ast.Tbool
    ) program.register in 

    let register = List.rev (List.sort compare (List.map str_to_int register0)) in 

    let add_node (name : string) (node : BDD.node) : unit = 
        let (b, t) = BDD.makeNode !table node in table := t;
        Hashtbl.replace formula (str_to_int name) b
    in


    let mem_table = Hashtbl.create 42 in 
    let rec aux (def : definition) : unit =
        let _ = if not (Hashtbl.mem mem_table def) then begin

            match def with 
            | Dexpr(pos, ty, ident, Epre(arg)) when Utils.compatible ty Ast.Tbool -> begin 
                add_node ident (Node (zero, (str_to_int arg), one))
            end
            | Dcall(pos, ty, idents, name, args) -> begin
                let filter_map f l = List.map (function Some(x) -> x) 
                    (List.filter (function None -> false | Some _ -> true) (List.map f l)) 
                in

                let bool_idents = filter_map (fun (t, i) -> 
                    if Utils.compatible t Ast.Tbool then Some(i) else None 
                ) (List.combine ty idents) in 
                List.iter (fun n -> 
                    let i = str_to_int n in 
                    Hashtbl.replace inputs i ();
                    add_node n (Node (zero, i, one))
                ) bool_idents
            end 
            | Dexpr(pos, ty, ident, Eident(arg)) when Utils.compatible ty Ast.Tbool -> begin 
                try 
                    let d = Hashtbl.find program.formula arg in 
                    aux d; add_node ident (Hashtbl.find formula (str_to_int arg)).node
                with Not_found -> 
                    add_node ident (Node(zero, str_to_int arg, one));
                    add_node arg   (Node(zero, str_to_int arg, one))
            end 
            | Dexpr(pos, ty, ident, Ebinop(arg1, binop, arg2)) when Utils.compatible ty Ast.Tbool -> begin 
                let _ = try
                    let d1 = Hashtbl.find program.formula arg1 in aux d1
                    with Not_found -> add_node arg1 (Node(zero, str_to_int arg1, one))
                in

                let _ = try
                    let d2 = Hashtbl.find program.formula arg2 in aux d2
                    with Not_found -> add_node arg2 (Node(zero, str_to_int arg2, one))
                in
                
                let f x y = match binop with 
                    | Blt  -> y && not x 
                    | Bgt  -> x && not y
                    | Ble  -> y || not x
                    | Bge  -> x || not y
                    | Beq  -> x =  y
                    | Bneq -> x <> y
                    | Bor  -> x || y 
                    | Bxor -> x <> y 
                    | Band -> x && y 
                    | _ -> failwith "incomplet : juste avec des bool"
                in

                let (b, t) = BDD.apply !table f (Hashtbl.find formula (str_to_int arg1)) (Hashtbl.find formula (str_to_int arg2)) in 
                table := t;  Hashtbl.replace formula (str_to_int ident) b
            end 
            | Dexpr(pos, ty, ident, Eunop(Ast.Unot, arg)) -> begin
                let _ = try
                    let d = Hashtbl.find program.formula arg in aux d
                    with Not_found -> add_node arg (Node(zero, str_to_int arg, one))
                in

                let (b, t) = BDD.notBDD !table (Hashtbl.find formula (str_to_int arg)) in 
                table := t; Hashtbl.replace formula (str_to_int ident) b
            end
            | Dexpr(pos, ty, ident, Einit(arg1, arg2)) when Utils.compatible ty Tbool -> 
                if is_init then begin
                    let _ = try
                        let d1 = Hashtbl.find program.formula arg1 in aux d1
                        with Not_found -> add_node arg1 (Node(zero, str_to_int arg1, one))
                    in

                    add_node ident (Hashtbl.find formula (str_to_int arg1)).node

                end else begin  
                    let _ = try
                        let d2 = Hashtbl.find program.formula arg2 in aux d2
                        with Not_found -> add_node arg2 (Node(zero, str_to_int arg2, one))
                    in

                    add_node ident (Hashtbl.find formula (str_to_int arg2)).node
                end 
            | Dexpr(pos, ty, ident, Econst(Ast.Cbool false)) -> begin 
                add_node ident Zero
            end
            | Dexpr(pos, ty, ident, Econst(Ast.Cbool true)) -> begin 
                add_node ident One
            end
            | Dexpr(pos, ty, ident, Eif(arg1, arg2, arg3)) when Utils.compatible ty Ast.Tbool -> begin 
                let _ = try
                    let d = Hashtbl.find program.formula arg1 in aux d
                    with Not_found -> add_node arg1 (Node(zero, str_to_int arg1, one))
                in

                let _ = try
                    let d = Hashtbl.find program.formula arg2 in aux d
                    with Not_found -> add_node arg2 (Node(zero, str_to_int arg2, one))
                in

                let _ = try
                    let d = Hashtbl.find program.formula arg3 in aux d
                    with Not_found -> add_node arg3 (Node(zero, str_to_int arg3, one))
                in

                let (b1, t1) = BDD.apply !table (fun x y -> y || not x) (Hashtbl.find formula (str_to_int arg1)) (Hashtbl.find formula (str_to_int arg2)) in
                let (b2, t2) = BDD.apply t1 (fun x y -> y || x)         (Hashtbl.find formula (str_to_int arg1)) (Hashtbl.find formula (str_to_int arg3)) in
                let (b3, t3) = BDD.apply t2 (fun x y -> x && y) b1 b2 in 
                table := t3; Hashtbl.replace formula (str_to_int ident) b3
            end 

            | Dexpr(_, _, _, _) -> ()
            
        end in 
        Hashtbl.replace mem_table def ()
    
    in

    let () = Hashtbl.iter (fun _ def -> aux def) program.formula in

    Hashtbl.iter (fun i b ->
        print_string (Z.to_string i ^ " = " ^ bdd_to_string b ^ "\n")
    ) formula; print_string "\n";

    {register = register; formula = formula; inputs = List.of_seq (Hashtbl.to_seq_keys inputs)}




let rec image (table':BDD.global_table) (set: BDD.bdd list) (program : (BDD.bdd, Prims.int) program) : (BDD.bdd * BDD.global_table) = 
    match program.register with
    | t :: q -> begin 
        let b' = Hashtbl.find program.formula t in

        let (b, table) = List.fold_left (
            fun (x, t) y -> BDD.simplify t x y
        ) (b', table') (List.rev set) in
        
        match b.node with 
        | Zero -> begin
            let (b1, table1) = image table set {inputs = program.inputs; register = q; formula = program.formula} in 
            BDD.makeNode table1 (Node(b1, t, zero))
        end 
        | One -> begin
            let (b1, table1) = image table set {inputs = program.inputs; register = q; formula = program.formula} in 
            BDD.makeNode table1 (Node(zero, t, b1))
        end 
        | _ -> begin 
            let (not_b, table1) = BDD.notBDD table b in 
            let (b1, table2) = image table1 (not_b :: set) {inputs = program.inputs; register = q; formula = program.formula} in 
            let (b2, table3) = image table2 (b     :: set) {inputs = program.inputs; register = q; formula = program.formula} in 
            BDD.makeNode table3 (Node(b1, t, b2))
        end 
    end 
    | [] -> (one, table')

let verify (table0 : BDD.global_table) (program1 : (BDD.bdd, Prims.int) program) (program2 : (BDD.bdd, Prims.int) program) (var:Prims.int) : bool = 

    let property_init = Hashtbl.find program1.formula var in
    let property_rest = Hashtbl.find program2.formula var in

    let (state', table1) = image table0 [] program1 in 
    let state = ref state' in let table = ref table1 in 

    if property_init.node <> One then false else begin 
        
        let rec aux () = 
            let (err, t) = BDD.apply !table (fun x y -> x && not y) !state property_rest in 
            table := t;

            print_string (bdd_to_string !state ^ "\n");
            
            if err.node <> BDD.Zero then false else begin 
            
                let (state', table') = image !table [!state] program2 in 
                table := table'; if !state.tag = state'.tag then true else (state := state'; aux ())

            end
        in aux ()
    end

    