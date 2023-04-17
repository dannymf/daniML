(** The type of binary operators. *)
type bop = 
  | Add
  | Mult
  | Leq

type tname = string
type typ = TString | TInt | TArrow of typ * typ | TData of tname
type var = string
type constructor = string * typ list

(** [Env] is module to help with environments, which 
    are maps that have strings as keys. *)
module Env = Map.Make(String)


(** The type of the abstract syntax tree (AST). *)
type expr =
  | Var of var
  | Int of int
  | Bool of bool
  (* | Rec of var * var * expr *)
  | Fun of var * expr
  | Rec of var * var * expr
  (* | AnonClosure of var * expr * env *)
  | NamedClosure of var * var * expr * env
  | App of expr * expr
  | Binop of bop * expr * expr
  | Let of var * expr * expr
  | If of expr * expr * expr
  (* | Cons of constructor
  | Match of expr * branch list *)

and env = expr Env.t

and branch = constructor * expr

type decl =
  | DLet of var * expr
  | DData of tname * constructor list

type prog = decl list
(* type prog = decl list * expr *)
(* execute expression, bind values to environment associated w corresponding var
   if dataD, record fact that user created new var *)
    (* construct mapping from constructors to types and back *)

  (* Program is list of decl in OCaml
     type x = .. | .. | .. *)