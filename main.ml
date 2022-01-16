

open Format 

open Lexing 

open Lustre2obc

open Utils 

open Obc2lustre4

open Check 

let file = "test.mls"

let get_loc lb = (lexeme_start_p lb, lexeme_end_p lb)

let print_loc (start_pos, end_pos) = 
	let l_start = start_pos.pos_lnum in
    let l_end   = end_pos.pos_lnum   in 
	let c_start = start_pos.pos_cnum - start_pos.pos_bol + 1 in
	let c_end   = end_pos.pos_cnum   - start_pos.pos_bol + 1 in
    if l_start = l_end then 
	    Format.printf "File \"%s\", line %d, characters %d-%d:\n" file l_start c_start c_end
    else
        Format.printf "File \"%s\", line %d-%d:\n" file l_start l_end

let () = 
	let c = open_in file in
	let lb = Lexing.from_channel c in

	try 
		let f = Parser.file Lexer.token lb in
    	close_in c;

        let f' = simplify (topo_sort (transform_nodes f "main")) in

        let p = Check.to_program f'.obc_definitions in
        let str_to_int, table, program1, program2 = Check.program_to_bdd p in
        
        let (b1, t1) = Check.image table [] program1 in
        let (b2, t2) = Check.image t1  [b1] program2 in 

        print_string ("\n" ^ Check.bdd_to_string b1 ^ "\n");
        print_string ("\n" ^ Check.bdd_to_string b2 ^ "\n");

        let l4string = lustre4string true f' in
        print_string ("\n" ^ l4string ^ "\n");

        let var_name, _ = List.hd f'.obc_outputs in 
        let var = Hashtbl.find str_to_int var_name in 

        print_bool (Check.verify t2 program1 program2 (Prims.of_int var));
        print_string "\n";

        assert false; 

        let l4string = lustre4string true f' in

        try 
            Unix.mkdir "TEST" 0o777; 
        with 
            | Unix.Unix_error(Unix.EEXIST, _,_ ) -> (); 
        
        Unix.chdir "TEST";
        printf "%s\n" (Unix.getcwd ());

        let l4file = Unix.openfile "test.lustre" [Unix.O_RDWR; Unix.O_CREAT; Unix.O_TRUNC] 0o777 in 
        let _ = Unix.write_substring l4file l4string 0 (String.length l4string) in

        


        let status = Sys.command "lesar test.lustre main -poly" in
        let _ = Sys.command "lustre test.lustre main" in
        let _ = Sys.command "kind2 test.lustre" in
        printf "status = %d\n" status;

        print_string "the end\n";
	

	with 
		| Lexer.Lexical_error s -> 
			begin 
				let pos = get_loc lb in 
				print_loc pos; 
				print_string (s^"\n"); 
			end 

		| Parsing.Parse_error -> 
			begin 
				let pos = get_loc lb in 
				print_loc pos; 
				Format.printf "Syntax error\n";
			end 
        | Utils.Error(pos, error) -> 
            begin 
                print_loc pos; 
                print_string (string_of_error error);
                print_string "\n";
            end
