

open Ast 
open Obc_ast 

open Utils 

open SAT
(** assume that all variables are bool *)


let generate_name iter name =
    string_of_int iter ^ "#" ^ name

let rec pre_name = function 
    | Dexpr(_, _, name, Epre(s))::q -> (name, s) :: pre_name q
    | _ :: q -> pre_name q 
    | [] -> []



let rec generate_transition table iter = function 
    | Dexpr(_, _, name, expr) :: q -> 
        begin 
            let search n = 
                try Hashtbl.find table (generate_name iter n) 
                with Not_found -> (* input *)
                    let var = leaf () in 
                    Hashtbl.add table (generate_name iter n) var; 
                    var 
            in

            generate_transition table iter q &&& (search name <==> match expr with 
            | Epre(s) -> 
                (try Hashtbl.find table (generate_name (iter-1) s) with Not_found -> leaf ())
            | Eident(i) -> 
                search i
            | Eunop(Unot, i) -> 
                notF (search i)
            | Ebinop(i, Band, j) -> 
                search i &&& search j
            | Ebinop(i, Bor, j) -> 
                search i ||| search j 
            | Ebinop(i, Bxor, j) -> 
                (search i &&& notF (search j)) ||| (search j &&& notF (search i))
            | Ebinop(i, Beq, j) -> 
                search i <==> search j 
            | Ebinop(i, Bneq, j) -> 
                (search i &&& notF (search j)) ||| (search j &&& notF (search i))
            | Ebinop(i, Blt, j) -> 
                search j &&& notF (search i) 
            | Ebinop(i, Bgt, j) -> 
                search i &&& notF (search j) 
            | Ebinop(i, Ble, j) -> 
                search i ==> search j
            | Ebinop(i, Bge, j) -> 
                search j ==> search i 
            | Econst(Cbool(true)) -> one 
            | Econst(Cbool(false)) -> zero 
            | Einit(i, j) -> if iter = 1 then search i else search j 
            | Eif(i, t, e) -> 
                (search t ||| notF (search i)) &&& (search e ||| search i)
            | _ -> failwith ""
            )

        end
    | _ :: _ -> failwith ""
    | [] -> one

let rec generate_transition' table iter = function 
    | Dexpr(_, _, name, expr) :: q ->
        begin 
            let search n = 
                try Hashtbl.find table (generate_name iter n) 
                with Not_found -> (* input *)
                    let var = leaf () in 
                    Hashtbl.add table (generate_name iter n) var; 
                    var 
            in

            generate_transition table iter q &&& (search name <==> match expr with 
            | Epre(s) -> 
                (try Hashtbl.find table (generate_name (iter-1) s) with Not_found -> leaf ())
            | Eident(i) -> 
                search i
            | Eunop(Unot, i) -> 
                notF (search i)
            | Ebinop(i, Band, j) -> 
                search i &&& search j
            | Ebinop(i, Bor, j) -> 
                search i ||| search j 
            | Ebinop(i, Bxor, j) -> 
                (search i &&& notF (search j)) ||| (search j &&& notF (search i))
            | Ebinop(i, Beq, j) -> 
                search i <==> search j 
            | Ebinop(i, Bneq, j) -> 
                (search i &&& notF (search j)) ||| (search j &&& notF (search i))
            | Ebinop(i, Blt, j) -> 
                search j &&& notF (search i) 
            | Ebinop(i, Bgt, j) -> 
                search i &&& notF (search j) 
            | Ebinop(i, Ble, j) -> 
                search i ==> search j
            | Ebinop(i, Bge, j) -> 
                search j ==> search i 
            | Econst(Cbool(true)) -> one 
            | Econst(Cbool(false)) -> zero 
            | Einit(i, j) -> if iter = 1 then search i else search j 
            | Eif(i, t, e) -> 
                (search t ||| notF (search i)) &&& (search e ||| search i)
            | _ -> failwith ""
            )

        end
    | _ :: _ -> failwith ""
    | [] -> one



let bounded node k = 
    let table = Hashtbl.create 42 in 

    let (output, _) = List.hd node.obc_outputs in
    let cnf = ref (generate_transition table 1 node.obc_definitions) in (* because Einit *)
    let condition = ref (Hashtbl.find table (generate_name 1 output)) in (* because Einit *)
    
    for i=2 to k do
        cnf := !cnf &&& generate_transition table i node.obc_definitions;
        condition := !condition &&& Hashtbl.find table (generate_name i output);
    done;

    let new_cnf = generate_cnf (!cnf &&& notF !condition) in

    let str = "p cnf " ^ string_of_int !variables_number ^ " " ^ string_of_int (List.length new_cnf) ^ "\n" ^
    List.fold_right (
        fun l s -> List.fold_right (
            fun l s ->match l with | Not i -> string_of_int (-i-1) ^ " " ^ s | Var i -> string_of_int (i+1) ^ " " ^ s
        ) l "0\n" ^ s
    ) new_cnf "" in 


    let file = Unix.openfile "test.cnf" [Unix.O_RDWR; Unix.O_CREAT; Unix.O_TRUNC] 0o777 in 
    let _ = Unix.write_substring file str 0 (String.length str) in 

    Format.printf "done  "; Format.print_flush ()
