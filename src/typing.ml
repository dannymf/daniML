open Ast
open Errors
open Context

let rec string_of_typ typ =
  match typ with
  | TInt -> "int"
  | TBool -> "bool"
  | TUnit -> "unit"
  | TFloat -> "float"
  | TProb t -> "prob " ^ string_of_typ t
  | TArrow (t1, t2) -> "(" ^ string_of_typ t1 ^ " -> " ^ string_of_typ t2 ^ ")"

(** [typeof ctx e] is the type of [e] in context [ctx]. 
    Raises: [Failure] if [e] is not well typed in [ctx]. *)
let rec typeof ctx = function
  | Int _ -> TInt
  | Bool _ -> TBool
  | Float _ -> TFloat
  | Prob e -> TProb (typeof ctx e)
  | Random -> TFloat
  | Decl (_, e) -> typeof ctx e
  | Closure (_, _, _, typ, _) -> typ
  | Var x -> ContextMap.lookup ctx x
  | Let (x, e1, e2) -> typeof_let ctx x e1 e2
  | Sample (x, e1, e2) -> typeof_sample ctx x e1 e2
  | AppProb e -> typeof_appprob ctx e
  | Binop (bop, e1, e2) -> typeof_bop ctx bop e1 e2
  | If (e1, e2, e3) -> typeof_if ctx e1 e2 e3
  | Fun (x, typ, e) -> typeof_fun ctx x typ e
  | Rec (name, t1, x, e, t2) -> typeof_rec ctx name t1 x e t2
  | App (e1, e2) -> typeof_app ctx e1 e2

(** Helper function for [typeof]. *)
and typeof_bop ctx bop e1 e2 =
  let t1, t2 = typeof ctx e1, typeof ctx e2 in
  match bop, t1, t2 with
  | Add, TInt, TInt 
  | Mult, TInt, TInt 
  | Minus, TInt, TInt -> TInt
  | Leq, TInt, TInt -> TBool
  | Leq, TFloat, TFloat -> TBool 
  | _ -> type_error Errors.bop_err

(** Helper function for [typeof]. *)
and typeof_let ctx x e1 e2 = 
  let t' = typeof ctx e1 in
  let ctx' = ContextMap.extend ctx x t' in
  typeof ctx' e2

and typeof_sample ctx x e1 e2 = 
  match typeof ctx e1 with
  | TProb t ->
    let ctx' = ContextMap.extend ctx x t in
    typeof ctx' e2
  | _ -> 
    print_endline (string_of_typ (typeof ctx e1));
    type_error Errors.bind_sample_err

(** Helper function for [typeof]. *)
and typeof_if ctx e1 e2 e3 =
  match typeof ctx e1 with
  | TBool ->
    let t2 = typeof ctx e2 in
    if t2 = typeof ctx e3 then t2
    else type_error Errors.if_branch_err
  | _ -> type_error Errors.if_guard_err

and typeof_app ctx e1 e2 =
  match typeof ctx e1, typeof ctx e2 with
  | TArrow (typ1, typ2), typ3 when typ1 = typ3 ->
    typ2
  | TArrow _, _ -> type_error Errors.app_err_scd
  | _ -> type_error Errors.app_err


and typeof_fun ctx x typ e =
  let ctx' = ContextMap.extend ctx x typ in
  let t1 = typeof ctx' e in
  TArrow (typ, t1)

(* TECHNICALLY WRONG *)
(* and typeof_rec ctx name x e typ = *)
and typeof_rec ctx name t1 x t2 e =
  let ctx' = ContextMap.extend ctx name t1 in
  let ctx'' = ContextMap.extend ctx' x t2 in
  ignore (typeof ctx'' e); t1

and typeof_appprob ctx e =
    match typeof ctx e with
    | TProb e' -> e'
    | _ -> type_error Errors.proba_err
    
(** [typecheck e] checks whether [e] is well typed in
    the empty context. Raises: [Failure] if not. *)
let typecheck e =
  ignore (typeof ContextMap.empty e); e

let typecheck_env env e =
  ignore (typeof env e); e