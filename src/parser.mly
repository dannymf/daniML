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
%token <float> FLOAT
%token <string> ID

// PROBABILISTIC
%token RANDOM
%token SAMPLE
%token FROM
%token PROB
%token UNPROB

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
%token LETFIX
%token ARROW
%token TARROW

// TYPES
%token COLON
%token INT_TYPE
%token BOOL_TYPE
%token FLOAT_TYPE

%token EOF

%nonassoc LEQ
%left PLUS MINUS
%left TIMES

%start <Ast.expr> prog

%%

prog:
	| e = expr EOF { e }
	
expr:
	| e1 = expr PLUS e2 = expr { Binop (Add, e1, e2) }
	| e1 = expr MINUS e2 = expr { Binop (Minus, e1, e2) }
	| e1 = expr TIMES e2 = expr { Binop (Mult, e1, e2) }
	| e1 = expr LEQ e2 = expr { Binop (Leq, e1, e2) }
	| e = atom { e }
	| e = atom es = atom+ { make_apply e es }

atom:
	| i = INT { Int i }
	| f = FLOAT { Float f }
	| x = ID { Var x }
	| TRUE { Bool true }
	| FALSE { Bool false }
	| LET x = ID EQUALS e1 = expr IN e2 = expr { Let (x, e1, e2) }
	| LET x = ID EQUALS e = expr { Decl (x, e) }
	| SAMPLE x = ID FROM e1 = expr IN e2 = expr { Sample (x, e1, e2) }
	| IF e1 = expr THEN e2 = expr ELSE e3 = expr { If (e1, e2, e3) }
	| FUN x = ID COLON t = typ ARROW e = expr { Fun (x, t, e) }
	| LETFIX name = ID COLON t1 = typ EQUALS FUN x = ID COLON t2 = typ ARROW e1 = expr IN e2 = expr { Let (name, Rec (name, t1, x, t2, e1), e2) }
	| RANDOM { Random } 
	| PROB e = expr { Prob e }
	| UNPROB e = expr { Sample ("_", e, Var "_") }
	| LPAREN e = expr RPAREN { e } 

typ:
	| INT_TYPE { TInt }
	| BOOL_TYPE { TBool }
	| FLOAT_TYPE { TFloat }
	| PROB t = typ { TProb t }
	| t1 = typ TARROW t2 = typ { TArrow (t1, t2) }
	;
