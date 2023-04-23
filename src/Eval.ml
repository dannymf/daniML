open Ast
open Errors
open Typing
(* open Prob *)

module Env = Ast.Env
type env = Ast.env

(** [parse s] parses [s] into an AST. *)
let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(** [typ] represents the type of an expression. *)
(* type typ =
  | TInt
  | TBool
  | TFloat
  | TProb
  | TClosure *)

(** [eval_big e] is the [e ==> v] relation. *)
let rec eval_big (env : env) (e : expr) : expr = match e with
  | Int _ | Bool _ | Float _ | Unit | Prob _ -> e
  | Closure _ -> e
  | Binop (bop, e1, e2) -> eval_bop env bop e1 e2
  | Let (x, e1, e2) -> eval_let env x e1 e2
  | Sample (x, e1, e2) -> eval_sample env x e1 e2
  | LetRec _ -> failwith "TODO"
  | If (e1, e2, e3) -> eval_if env e1 e2 e3
  | Fun (x, typ, e1) -> eval_fun env x typ e1
  | Rec (name, x, e1, typ) -> eval_rec env name x e1 typ
  | App (e1, e2) -> eval_app env e1 e2
  | AppProb (e1, e2) -> eval_appprob env e1 e2
  | Var x -> eval_var env x


and eval_var env x = 
  try Env.find x env with Not_found -> runtime_error Errors.unbound_var_err

and eval_bop env bop e1 e2 = match bop, eval_big env e1, eval_big env e2 with
  | Add, Int a, Int b -> Int (a + b)
  | Add, Float a, Float b -> Float (a +. b)
  | Mult, Int a, Int b -> Int (a * b)
  | Mult, Float a, Float b -> Float (a *. b)
  | Minus, Int a, Int b -> Int (a - b)
  | Minus, Float a, Float b -> Float (a -. b)
  | Leq, Int a, Int b -> Bool (a <= b)
  | Leq, Float a, Float b -> Bool (a <= b)
  | _ -> runtime_error Errors.bop_err

(** [eval_if e1 e2 e3] is the [e] such that [if e1 then e2 else e3 ==> e]. *)
and eval_if env e1 e2 e3 = match eval_big env e1 with
  | Bool true -> eval_big env e2
  | Bool false -> eval_big env e3
  | _ -> runtime_error Errors.if_guard_err

and eval_fun env x typ e = 
  Closure ("_", x, e, typ, env)

and eval_rec env name x e typ = 
  Closure (name, x, e, typ, env)

and eval_app env e1 e2 =
  match eval_big env e1 with
  | Closure (name, x, e, _, defenv) as rec_closure -> 
    begin
      let v2 = eval_big env e2 in
      let env' = Env.add x v2 defenv in
      let env'' = Env.add name rec_closure env' in
      eval_big env'' e
    end
  | _ -> runtime_error Errors.app_err

and eval_let env x e1 e2 =
  match eval_big env e1 with
  | e -> eval_big (Env.add x e env) e2

 and eval_sample =
  failwith "TODO"
 (* eval_sample env x e1 e2 *)
  (* match eval_big env e1 with
  |  -> eval_big (Env.add x e env) (App (e2, app e1)) *)
 
  and eval_appprob =
  (* eval_appprob env e1 e2 *)
  failwith "TODO"

(** [interp_big s] interprets [s] by parsing, type-checking,
    and evaluating it with the big-step model. *)
let interp_big (s : string) : expr =
  s |> parse |> typecheck |> (eval_big Env.empty)
