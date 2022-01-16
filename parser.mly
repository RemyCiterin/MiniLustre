%{
open Ast 
open Lexing 
open Parsing 
open Utils

let loc () = symbol_start_pos (), symbol_end_pos ()

let make_expr e = { exprP_expr= e; exprP_position= loc () }

%}

%token NODE 
%token TRUE 
%token FALSE 
%token LET 
%token TEL 
%token IF 
%token WITH 
%token THEN
%token ELSE 
%token RETURNS
%token VAR 
%token CONST
%token PRE
%token T_INT 
%token T_BOOL
%token T_FLOAT 

%token <string> IDENT 

%token <int> INT 
%token <float> FLOAT 


%token L_PAR
%token R_PAR

%token EQ 
%token NEQ 
%token LT 
%token GT 
%token GE
%token LE 

%token AND 
%token OR 
%token XOR 
%token NOT 

%token ADD 
%token MIN 
%token DIV 
%token MOD 
%token TIMES

%token V 
%token PV 
%token PP 
%token AS 

%token EOF 

%token INIT 

%nonassoc THEN 
%nonassoc ELSE
%right INIT
%left OR 
%left AND 
%left XOR
%left EQ NEQ LT GT LE GE 
%left ADD MIN 
%left DIV MOD 
%left TIMES
%nonassoc umin
%nonassoc NOT 


%start file
%type <Ast.node list> file

%%

file: 
    | node_list EOF {$1}
;

node_list: 
    | /* empty */ {[]}
    | node node_list {$1 :: $2}
;

node: 
    | NODE IDENT L_PAR param_star R_PAR RETURNS  L_PAR param_star R_PAR VAR param_star LET assigns TEL {
        let definitions = Hashtbl.create 42 in

        let add_def p def n = 
            if Hashtbl.mem definitions n then 
                raise (Error(p, MultipleDef(n, $2)));
            Hashtbl.add definitions n def 
        in

        List.iter (fun def -> match def with 
        | Dexpr(p, name, _) -> add_def p def name
        | Dcall(p, idents, _, _) -> List.iter (add_def p def) idents
        ) $13;

        let variables = Hashtbl.create 42 in 

        List.iter (fun (name, ty) -> 
            if Hashtbl.mem definitions name then 
                raise (Error(loc (), MultipleDef(name, $2))); 
            
            if Hashtbl.mem variables name then 
                raise (Error(loc (), MultipleDef(name, $2))); 
            Hashtbl.add variables name ty; 
        ) $4;

        List.iter (fun (name, ty) -> 
            if not (Hashtbl.mem definitions name) then 
                raise (Error(loc (), Undefined_variable(name, $2))); 

            if is_const ty then 
                raise (Error(loc (), NonConst ty));
            
            if Hashtbl.mem variables name then
                raise (Error(loc (), MultipleDef(name, $2))); 
            Hashtbl.add variables name ty; 
        ) ($8 @ $11);

        {node_name= $2; node_position= loc (); node_inputs= $4; node_outputs= $8; node_variables= $11; node_definitions= definitions}
    }

    | NODE IDENT L_PAR param_star R_PAR RETURNS  L_PAR param_star R_PAR LET assigns TEL {
        let definitions = Hashtbl.create 42 in

       let add_def p def n = 
            if Hashtbl.mem definitions n then 
                raise (Error(p, MultipleDef(n, $2)));
            Hashtbl.add definitions n def 
        in

        List.iter (fun def -> match def with 
        | Dexpr(p, name, _) -> add_def p def name
        | Dcall(p, idents, _, _) -> List.iter (add_def p def) idents
        ) $11;

        let variables = Hashtbl.create 42 in 

        List.iter (fun (name, ty) -> 
            if Hashtbl.mem definitions name then 
                raise (Error(loc (), MultipleDef(name, $2))); 
            
            if Hashtbl.mem variables name then 
                raise (Error(loc (), MultipleDef(name, $2))); 
            Hashtbl.add variables name ty; 
        ) $4;

        List.iter (fun (name, ty) -> 
            if not (Hashtbl.mem definitions name) then 
                raise (Error(loc (), Undefined_variable(name, $2))); 

            if is_const ty then 
                raise (Error(loc (), NonConst ty));
            
            if Hashtbl.mem variables name then
                raise (Error(loc (), MultipleDef(name, $2))); 
            Hashtbl.add variables name ty; 
        ) $8;

        {node_name= $2; node_position= loc (); node_inputs= $4; node_outputs= $8; node_variables= []; node_definitions= definitions}
    }
;

param_star: 
    | /* empty */ {[]}
    | param_plus  {$1}
; 

param_plus: 
    | IDENT PP ty               {[($1, $3)]}
    | IDENT PP ty PV param_plus {($1 , $3) :: $5} /* shift/reduce fonclict :( */
    | IDENT V param_plus        {let (s, t) = List.hd $3 in ($1, t) :: $3}
; 

ty: 
    | T_INT   {Tint}
    | T_BOOL  {Tbool}
    | T_FLOAT {Tfloat}
    | CONST T_INT {Tconst_int}
    | CONST T_BOOL {Tconst_bool}
    | CONST T_FLOAT {Tconst_float}

assigns: 
    | /* empty */                                     {[]}
    | IDENT      AS expr PV assigns                   {Dexpr(loc (), $1, $3) :: $5}
    | L_PAR tupleIdent R_PAR AS IDENT L_PAR args R_PAR PV assigns {Dcall(loc (), $2, $5, $7) :: $10} 
; 

tupleIdent: 
    | /* empty */        {[]}
    | IDENT              {[$1]}
    | IDENT V tupleIdent {$1 :: $3}
; 

args: 
    | /* empty */ {[]}
    | expr {[$1]}
    | expr V args {$1 :: $3}

expr: 
    | L_PAR expr R_PAR {$2}
    | IDENT {make_expr (Eident $1)}
    | INT   {make_expr (Econst (Cint $1))}
    | FLOAT {make_expr (Econst (Cfloat $1))}
    | TRUE  {make_expr (Econst (Cbool true))}
    | FALSE {make_expr (Econst (Cbool false))}

    | IDENT L_PAR args R_PAR {make_expr (Ecall ($1, $3))}

    | IF   expr THEN expr ELSE expr {make_expr (Eif   ($2, $4, $6))}
    | WITH expr THEN expr ELSE expr {make_expr (Ewith ($2, $4, $6))}

    | expr AND expr {make_expr (Ebinop ($1, Band, $3))}
    | expr XOR expr {make_expr (Ebinop ($1, Bxor, $3))}
    | expr OR  expr {make_expr (Ebinop ($1, Bor,  $3))}

    | expr NEQ expr {make_expr (Ebinop ($1, Bneq, $3))}
    | expr EQ  expr {make_expr (Ebinop ($1, Beq,  $3))}
    | expr LT  expr {make_expr (Ebinop ($1, Blt,  $3))}
    | expr GT  expr {make_expr (Ebinop ($1, Bgt,  $3))}
    | expr LE  expr {make_expr (Ebinop ($1, Ble,  $3))}
    | expr GE  expr {make_expr (Ebinop ($1, Bge,  $3))}

    | expr ADD expr {make_expr (Ebinop ($1, Badd, $3))}
    | expr MIN expr {make_expr (Ebinop ($1, Bmin, $3))}
    | expr DIV expr {make_expr (Ebinop ($1, Bdiv, $3))}
    | expr MOD expr {make_expr (Ebinop ($1, Bmod, $3))}
    | expr TIMES expr {make_expr (Ebinop ($1, Btimes, $3))}

    | expr INIT expr {make_expr (Einit ($1, $3))}
    | PRE L_PAR expr R_PAR {make_expr (Epre $3)}

    | MIN expr %prec umin {make_expr (Eunop (Uneg, $2))}
    | NOT expr            {make_expr (Eunop (Unot, $2))}


;
