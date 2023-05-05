open Ast
open Errors
open Typing
(* open Prob *)
open Random

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

  (** [subst e v x] is [e] with [v] substituted for [x], that
    is, [e{v/x}]. *)
  let rec subst e v x = match e with
  | Var y -> if x = y then v else e
  | Int _ | Float _ | Bool _ | Decl _
  | Fun _ | Rec _ | AppProb _
  | Random | Closure _ -> e
  | Prob e -> Prob (subst e v x)
  | App (e1, e2) -> App (subst e1 v x, subst e2 v x)
  | Binop (bop, e1, e2) -> Binop (bop, subst e1 v x, subst e2 v x)
  | Let (y, e1, e2) ->
    let e1' = subst e1 v x in
    if x = y
    then Let (y, e1', e2)
    else Let (y, e1', subst e2 v x)
  | Sample (y, e1, e2) -> 
    let e1' = subst e1 v x in
    if x = y
    then Sample (y, e1', e2)
    else Sample (y, e1', subst e2 v x)
  | If (e1, e2, e3) -> 
    If (subst e1 v x, subst e2 v x, subst e3 v x)  

  
(** [eval_big e] is the [e ==> v] relation. *)
let rec eval_big (env : env) (e : expr) : expr = match e with
  | Int _ | Bool _ | Float _ | Prob _ -> e
  | Random -> Float (RandomGen.random ())
  | Closure _ -> e
  | Binop (bop, e1, e2) -> eval_bop env bop e1 e2
  (* | Op (op, es) ->  
     (type of es is list) *) 
  | Let (x, e1, e2) -> eval_let env x e1 e2
  | Sample (x, e1, e2) -> eval_sample env x e1 e2
  | If (e1, e2, e3) -> eval_if env e1 e2 e3
  | Fun (x, typ, e1) -> eval_fun env x typ e1
  | Rec (name, t1, x, t2, e1) -> eval_rec env name t1 x t2 e1
  | App (e1, e2) -> eval_app env e1 e2
  | AppProb e -> eval_appprob env e
  | Var x -> eval_var env x
  | Decl _ -> e

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
  Closure (None, x, e, typ, env)

and eval_rec env name t1 x e t2 =
  Closure (Some (name, t1), x, t2, e, env)

(* and eval_rec env name x e typ = 
  Closure (name, x, e, typ, env) *)

and eval_app env e1 e2 =
  let v2 = eval_big env e2 in
  match eval_big env e1 with
  | Closure (name, x, e, _, defenv) as rec_closure -> 
    begin
      match e with
      | Prob exp -> 
        Prob (subst exp v2 x) 
      | _ -> begin
        let env' = Env.add x v2 defenv in
        (match name with
        | None -> eval_big env' e
        | Some (name, _) -> 
        let env'' = Env.add name rec_closure env' in
        eval_big env'' e)
    end
  end
  | _ -> runtime_error Errors.app_err

and eval_let env x e1 e2 =
  match eval_big env e1 with
  | e -> eval_big (Env.add x e env) e2

 and eval_sample env x e1 e2 =
 (* eval_sample env x e1 e2 *)
  let app_e = eval_appprob env e1 in
    let env' = Env.add x app_e env in
    eval_big env' e2
 
  and eval_appprob env e =
    match eval_big env e with
    | Prob e' -> eval_big env e'
    | _ -> runtime_error Errors.app_err


let eval_big_env (env : env) (e : expr) : expr * env =
  match e with
  | Decl (x, e) -> begin
    let e1' = eval_big env e in
    e1', Env.add x e1' env
  end
  | _ -> eval_big env e, env

(** [interp_big s] interprets [s] by parsing, type-checking,
    and evaluating it with the big-step model. *)
let interp_big (s : string) : expr =
  (* s |> parse |> (eval_big Env.empty) *)
  s |> parse |> typecheck |> (eval_big Env.empty)

let interp_big_env (s : string) (env: env) : expr * env =
  (* s |> parse |> eval_big_env env  *)
  s |> parse |> typecheck |> eval_big_env env
