(* This file uses some advanced parsing techniques
   to parse juxtaposed applications [e1 e2 e3] the
	 same way as OCaml does. *)
(** [make_apply e [e1; e2; ...]] makes the application  
    [e e1 e2 ...]).  Requires: the list argument is non-empty. *)
(* let rec make_apply e = function
  | [] -> failwith "precondition violated"
  | [e'] -> App (e, e')
	| h :: ((_ :: _) as t) -> make_apply (App (e, h)) t *)


%{
open Ast
let rec make_apply e = function
  | [] -> failwith "precondition violated"
  | [e'] -> App (e, e')
	| h :: ((_ :: _) as t) -> make_apply (App (e, h)) t
%}

%token <int> INT
%token <string> ID

// NEW
// %token RANDOM
// %token SAMPLE
// %token FROM

%token TRUE
%token FALSE
%token LEQ
%token TIMES  
%token PLUS
%token MINUS
%token LPAREN
%token RPAREN
%token LET
%token EQUALS
%token IN
%token IF
%token THEN
%token ELSE
%token FUN
%token FIX
// %token LETFIX
%token ARROW
%token EOF

// %nonassoc IN
// %nonassoc ELSE
// %nonassoc ARROW
// %nonassoc ARROW
%nonassoc LEQ
%left PLUS
%left TIMES

%start <Ast.expr> prog

%%

prog:
	| e = expr EOF { e }
	
expr:
	| e1 = expr PLUS e2 = expr { Binop (Add, e1, e2) }
	| e1 = expr TIMES e2 = expr { Binop (Mult, e1, e2) }
	| e1 = expr MINUS e2 = expr { Binop (Minus, e1, e2) }
	| e1 = expr LEQ e2 = expr { Binop (Leq, e1, e2) }
	| e = atom { e }
	| e = atom es = atom+ { make_apply e es }

atom:
	| i = INT { Int i }
	| x = ID { Var x }
	| TRUE { Bool true }
	| FALSE { Bool false }
	| LET x = ID EQUALS e1 = expr IN e2 = expr { Let (x, e1, e2) }
	| IF e1 = expr THEN e2 = expr ELSE e3 = expr { If (e1, e2, e3) }
	| FUN x = ID ARROW e = expr { Fun (x, e) }
	| LET FIX name = ID x = ID EQUALS e1 = expr IN e2 = expr { Let (name, Rec (name, x, e1), e2) }
	| LPAREN e=expr RPAREN { e } 
	;
	
