module Int = struct 
    let compare x y = compare x y
    type t = int 
end 

module SET = Set.Make(Int)
module MAP = Map.Make(Int)

type literal = | Not of int | Var of int

type tseitin = {
    tseitin_name: int; 
    tseitin_names: SET.t; 
    tseitin_intro: literal list list; 
}

let variables_number = ref 0 
let global_table = Hashtbl.create 42

let intro () =
    let number = !variables_number in 
    variables_number := !variables_number + 1;
    number 

let make name names intro = 
    let tseitin = {tseitin_name= name; tseitin_names= names; tseitin_intro= intro} in 
    Hashtbl.add global_table name tseitin; 
    tseitin  

let (&&&) {tseitin_name=a} {tseitin_name=b} = 
    let name = intro () in 

    make name (SET.of_list [a; b]) [[Not a; Not b; Var name]; [Var a; Not name]; [Var b; Not name]] 

let (|||) {tseitin_name=a} {tseitin_name=b} = 
    let name = intro () in 

    make name (SET.of_list [a; b]) [[Var a; Var b; Not name]; [Not a; Var name]; [Not b; Var name]] 

let leaf () = make (intro ()) SET.empty []

let notF {tseitin_name= a} = let name = intro () in 
    make name (SET.singleton a) [[Var a; Var name]; [Not a; Not name]]

let one  = let name = intro () in make name SET.empty [[Var name]]
let zero = let name = intro () in make name SET.empty [[Not name]]

let (==>) a b = b ||| notF a

let (<==>) a b = (a ==> b) &&& (b ==> a)

let generate_cnf (formula : tseitin) : literal list list =
    
    let variables = ref SET.empty in

    let rec aux formula = 
        if not (SET.mem formula.tseitin_name !variables) then 
            begin
                variables := SET.add formula.tseitin_name !variables;
                SET.iter (fun n -> aux (Hashtbl.find global_table n)) formula.tseitin_names
            end
    in aux formula; 
    let clauses = ref [] in 
    SET.iter (fun name -> 
        let formula = Hashtbl.find global_table name in 
        List.iter (fun c -> clauses:= c::!clauses) formula.tseitin_intro;
    ) !variables; [Var formula.tseitin_name] :: !clauses
        
    
    