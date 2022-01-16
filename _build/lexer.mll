{
(* ce fichier est inpirer du TP minilustre *)
open Parser 
open Lexing

exception Lexical_error of string

let fromString = 
    let h = Hashtbl.create 42 in 

    List.iter (fun (ident, tok) -> Hashtbl.add h ident tok) [
        "node",     NODE; 
        "true",     TRUE; 
        "false",    FALSE; 
        "let",      LET;  
        "tel",      TEL;  
        "if",       IF; 
        "with",     WITH; 
        "then",     THEN; 
        "else",     ELSE; 
        "returns",  RETURNS; 
        "var",      VAR; 
        "const",    CONST; 
        "pre",      PRE;
        "int",      T_INT; 
        "bool",     T_BOOL; 
        "float",    T_FLOAT;  

    ]; 
    let aux ident = 
        try Hashtbl.find h ident 
        with Not_found -> IDENT ident 
    in aux

let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- {pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }

}

let digit    = ['0'-'9']

let alpha    = ['a'-'z' 'A'-'Z']

let ident    = alpha (alpha | digit | '_' | '#')*

let exponent = ('e' | 'E') ('+' | '-')? digit+

let float    = digit exponent | digit+ '.' digit* exponent? | digit* '.' digit+ exponent? 

rule token = parse 
    | (' ' | '\t')  {token lexbuf}

    | '\n'          {newline lexbuf; token lexbuf}

    | digit+ as i   {INT (int_of_string i)}

    | float  as f   {FLOAT (float_of_string f)}

    | ident as s    {fromString s}

    | "->"          {INIT}

    | '('           {L_PAR}
    | ')'           {R_PAR}

    | "=="          {EQ}
    | "!="          {NEQ}
    | "<="          {LE}
    | ">="          {GE}
    | "<"           {LT}
    | ">"           {GT}

    | "&&"          {AND}
    | "^"           {XOR}
    | "!"           {NOT}
    | "||"          {OR}

    | "+"           {ADD}
    | "-"           {MIN}
    | "/"           {DIV}
    | "%"           {MOD}
    | "*"           {TIMES}

    | ','           {V}
    | ';'           {PV}
    | ':'           {PP}
    | '='           {AS}


    | "(*"          {comment lexbuf; token lexbuf}

    | eof           {EOF}

    | _             {raise (Lexical_error "unknow caracter")}

and comment = parse
  | "*)" { () }
  | '\n' { newline lexbuf; comment lexbuf }
  | _    { comment lexbuf }
  | eof  { raise (Lexical_error "unterminated comment") }